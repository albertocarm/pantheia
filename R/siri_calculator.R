#' Launch SIRI Prognostic Calculator (Shiny App)
#'
#' Launches a local Shiny application that predicts Progression-Free Survival (PFS),
#' Overall Survival (OS), and Response Probability based on the SIRI biomarker and
#' other clinical variables.
#'
#' @return A Shiny app object.
#' @import shiny
#' @import ggplot2
#' @importFrom splines ns
#' @importFrom stats model.frame model.matrix delete.response median formula na.pass
#' @importFrom scales percent
#' @export
siri_calculator <- function() {
  
  m_pfs  <- mod_pfs
  m_os   <- mod_os
  m_resp <- mod_resp
  
  get_lp <- function(model, newdata) {
    trms <- stats::delete.response(model$terms)
    environment(trms) <- environment()
    
    if("knots_logsiri" %in% all.vars(stats::formula(model))) {
      knots_logsiri <- model$knots 
    }
    
    mf <- stats::model.frame(trms, newdata, xlev = model$xlevels, na.action = stats::na.pass)
    X <- stats::model.matrix(trms, mf)
    
    beta <- model$coef
    beta[is.na(beta)] <- 0
    
    lp <- as.vector(X %*% beta)
    
    var_mat <- if(!is.null(model$vcov)) model$vcov else model$var
    se_lp <- 0
    
    if (!is.null(var_mat)) {
      var_mat[is.na(var_mat)] <- 0
      
      if(ncol(X) == ncol(var_mat)) {
        se_lp <- sqrt(diag(X %*% var_mat %*% t(X)))
      } else {
        common_names <- intersect(colnames(X), colnames(var_mat))
        if(length(common_names) > 0) {
          X_sub <- X[, common_names, drop = FALSE]
          var_sub <- var_mat[common_names, common_names, drop = FALSE]
          se_lp <- sqrt(diag(X_sub %*% var_sub %*% t(X_sub)))
        }
      }
    }
    
    return(list(lp = lp, se_lp = se_lp))
  }
  
  predict_survival <- function(model, newdata) {
    res <- get_lp(model, newdata)
    lp <- res$lp
    se_lp <- res$se_lp
    
    if (!is.null(model$dist) && model$dist == "weibull") {
      scale <- model$scale
      log_log_2 <- log(log(2)) 
      median_val <- exp(lp + scale * log_log_2)
      
      ic_low <- exp((lp - 1.96 * se_lp) + scale * log_log_2)
      ic_up  <- exp((lp + 1.96 * se_lp) + scale * log_log_2)
      
      return(list(val = median_val, lower = ic_low, upper = ic_up, lp = lp, scale = scale, se_lp = se_lp))
    } else {
      return(list(val = exp(lp)))
    }
  }
  
  predict_response <- function(model, newdata) {
    res <- get_lp(model, newdata)
    lp <- res$lp
    prob <- 1 / (1 + exp(-lp))
    return(list(val = prob))
  }
  
  if (!is.null(m_pfs$xlevels)) {
    levels_list <- m_pfs$xlevels
    regimen_opts <- levels_list$regimen_cat[levels_list$regimen_cat != "Other"]
  } else {
    levels_list <- list(
      diam_low = c("Low", "High"),
      regimen_cat = c("FOLFIRINOX", "Gem-Abraxane", "Gemcitabine mono", "Other"),
      ecog_cat_3 = c("0", "1", "2+"),
      CACS = c("No", "Yes")
    )
    regimen_opts <- c("FOLFIRINOX", "Gem-Abraxane", "Gemcitabine mono")
  }
  
  ui <- fluidPage(
    tags$head(tags$style(HTML("
      body { background: linear-gradient(135deg, #e0e4e8 0%, #f3f5f8 100%); font-family: 'Segoe UI', system-ui, sans-serif; color: #2c3e50; }
      .well { background: #ffffff; border: 1px solid #d1d9e6; box-shadow: 0 10px 20px rgba(0,0,0,0.08); border-radius: 12px; }
      .btn-primary { background: linear-gradient(to bottom, #3a6186, #26425f); border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.2); font-weight: 600; text-transform: uppercase; letter-spacing: 1px; transition: all 0.3s ease; }
      .btn-primary:hover { background: linear-gradient(to bottom, #4b7aa8, #2c4e70); box-shadow: 0 6px 10px rgba(0,0,0,0.3); transform: translateY(-1px); }
      h2, h3, h4, .main-title { color: #2c3e50; }
      .result-card { background: linear-gradient(145deg, #ffffff, #f0f0f0); padding: 20px; border-radius: 15px; text-align: center; box-shadow: 5px 5px 10px #bebebe, -5px -5px 10px #ffffff; margin-bottom: 20px; }
      .result-value { font-size: 32px; font-weight: 800; color: #26425f; }
      .result-label { font-size: 14px; text-transform: uppercase; color: #7f8c8d; letter-spacing: 1px; }
      .plot-container { background: linear-gradient(145deg, #ffffff, #f8f9fa); border-radius: 15px; padding: 15px; box-shadow: 5px 5px 10px #bebebe, -5px -5px 10px #ffffff; margin-top: 20px; }
      .plot-title { font-weight: 700; text-align: center; margin-bottom: 10px; font-size: 16px; }
      .btn-calc { font-weight: bold; font-size: 16px; padding: 12px; }
      .info-icon { display: inline-block; width: 16px; height: 16px; background: linear-gradient(to bottom, #3a6186, #26425f); color: white; border-radius: 50%; text-align: center; font-size: 11px; font-weight: bold; line-height: 16px; cursor: help; margin-left: 5px; vertical-align: middle; }
      .footer-section { margin-top: 30px; padding-top: 20px; border-top: 1px solid #bdc3c7; text-align: center; }
    "))),
    
    div(class = "main-title", style = "font-size: 24px; text-align: center; font-weight: 800;", 
        "Pancreatic Cancer Prognostic Calculator (Pantheia Group)"),
    div(class = "subtitle", style = "text-align: center; font-style: italic; color: #5d6d7e; margin-bottom: 20px;", 
        "Illustrating the prognostic impact of SIRI on therapeutic effects"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Clinical Data", style="border-bottom: 2px solid #3a6186; padding-bottom: 10px;"),
        div(
          tags$label("SIRI (absolute value):", tags$span(class = "info-icon", title = "Systemic Inflammation Response Index.", "?")),
          numericInput("siri", label = NULL, value = 2.1, min = 0.01, step = 0.1)
        ),
        selectInput("diam", "Sum of all baseline tumor diameters:", choices = c("Low (<=5 cm)" = "Low", "High (>5 cm)" = "High"), selected = "High"),
        selectInput("regimen", "Regimen:", choices = regimen_opts, selected = "FOLFIRINOX"),
        selectInput("ecog", "ECOG PS:", choices = levels_list$ecog_cat_3, selected = "1"),
        div(
          tags$label("CACS:", tags$span(class = "info-icon", title = "Cancer Anorexia-Cachexia Syndrome.", "?")),
          selectInput("cacs", label = NULL, choices = levels_list$CACS, selected = "Yes")
        ),
        br(),
        actionButton("btn_calc", "CALCULATE NOW", class = "btn-primary btn-block btn-calc")
      ),
      
      mainPanel(
        conditionalPanel(
          condition = "input.btn_calc > 0",
          h3("Model Estimates"),
          hr(),
          fluidRow(
            column(4, div(class = "result-card", div(class = "result-label", "Median PFS"), div(class = "result-value", textOutput("pfs_val")), div(class = "result-ci", textOutput("pfs_ci")))),
            column(4, div(class = "result-card", div(class = "result-label", "Median OS"), div(class = "result-value", textOutput("os_val")), div(class = "result-ci", textOutput("os_ci")))),
            column(4, div(class = "result-card", div(class = "result-label", "Response Prob."), div(class = "result-value", style="color:#27ae60", textOutput("resp_val")), div(class = "result-ci", "Clinical Probability")))
          ),
          hr(),
          h4("Predicted Survival Curves", style="border-bottom: 2px solid #3a6186; padding-bottom: 10px;"),
          fluidRow(
            column(6, div(class = "plot-container", div(class = "plot-title", "Progression-Free Survival (PFS)"), plotOutput("plot_pfs", height = "320px"))),
            column(6, div(class = "plot-container", div(class = "plot-title", "Overall Survival (OS)"), plotOutput("plot_os", height = "320px")))
          ),
          hr(),
          div(class = "footer-section",
              div(style="font-style: italic; color: #5d6d7e;", "Developed by Pantheia/SEOM project researchers"),
              div(style="font-style: italic; color: #95a5a6; font-size: 12px;", "Disclaimer: This is an experimental tool designed solely to illustrate the prognostic impact of SIRI on all endpoints.")
          )
        )
      )
    )
  )
  
  server <- function(input, output) {
    vals <- reactiveValues(pfs = NULL, os = NULL, resp = NULL)
    
    observeEvent(input$btn_calc, {
      
      lvls <- if(!is.null(m_pfs$xlevels)) m_pfs$xlevels else levels_list
      
      df <- data.frame(
        logsiri = log(input$siri),
        diam_low    = factor(input$diam, levels = lvls$diam_low),
        regimen_cat = factor(input$regimen, levels = lvls$regimen_cat),
        ecog_cat_3  = factor(input$ecog, levels = lvls$ecog_cat_3),
        CACS        = factor(input$cacs, levels = lvls$CACS)
      )
      
      vals$pfs  <- predict_survival(m_pfs, df)
      vals$os   <- predict_survival(m_os, df)
      vals$resp <- predict_response(m_resp, df)
    })
    
    output$pfs_val  <- renderText({ if(is.null(vals$pfs)) "-" else paste(round(vals$pfs$val, 2), "months") })
    output$pfs_ci   <- renderText({ if(is.null(vals$pfs)) "" else paste0("CI: ", round(vals$pfs$lower, 1), " - ", round(vals$pfs$upper, 1)) })
    output$os_val   <- renderText({ if(is.null(vals$os)) "-" else paste(round(vals$os$val, 2), "months") })
    output$os_ci    <- renderText({ if(is.null(vals$os)) "" else paste0("CI: ", round(vals$os$lower, 1), " - ", round(vals$os$upper, 1)) })
    output$resp_val <- renderText({ if(is.null(vals$resp)) "-" else paste0(round(vals$resp$val * 100, 1), "%") })
    
    plot_curve <- function(obj, color, max_time) {
      req(obj)
      lp <- obj$lp; scale <- obj$scale; se_lp <- obj$se_lp
      times <- seq(0, max_time, by = 0.1)
      
      surv_est <- exp(-(times / exp(lp))^(1/scale))
      
      surv_lo  <- exp(-(times / exp(lp - 1.96 * se_lp))^(1/scale))
      surv_hi  <- exp(-(times / exp(lp + 1.96 * se_lp))^(1/scale))
      
      df_plot <- data.frame(time = times, surv = surv_est, lower = surv_lo, upper = surv_hi)
      med_txt <- paste0("Median: ", round(obj$val, 1), " m")
      
      ggplot(df_plot, aes(x = time)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = color, alpha = 0.2) +
        geom_line(aes(y = surv), color = color, linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
        annotate("text", x = max_time * 0.75, y = 0.85, label = med_txt, fontface = "bold") +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        scale_x_continuous(name = "Time (months)") +
        theme_minimal()
    }
    
    output$plot_pfs <- renderPlot({ plot_curve(vals$pfs, "#2E86C1", 24) }, bg="transparent")
    output$plot_os  <- renderPlot({ plot_curve(vals$os, "#C0392B", 36) }, bg="transparent")
  }
  
  shiny::shinyApp(ui, server)
}