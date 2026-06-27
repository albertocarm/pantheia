# PANTHEIA-SIRI overall-survival calculator (3-level tumour-burden model).
# Weibull accelerated failure time model; coefficients pooled across multiple
# imputations with Rubin's rules. Tumour burden is entered as one of three
# levels: >5 cm, <=5 cm, or non-measurable disease.
# Run with: shiny::runApp("inst/siri_calculator_3level")
# Research tool; not intended for clinical decision-making.

library(shiny)
library(splines)
library(ggplot2)

# Model is loaded relative to the app directory.
model <- readRDS("FINAL_OS_3LEVEL.rds")
coefs <- model$coef; coefs[is.na(coefs)] <- 0
scale <- model$scale
knots <- model$knots          # variable name must match the fitted column names
xlev  <- model$xlevels

rhs <- ~ diam3 + ns(logsiri, knots = knots[2], Boundary.knots = knots[c(1, 3)]) +
  regimen_cat + ecog_cat_3 + CACS + regimen_cat:logsiri

# Linear predictor for one covariate combination.
linear_predictor <- function(siri, regimen, ecog, cacs, diam3) {
  nd <- data.frame(
    diam3       = factor(diam3, levels = xlev$diam3),
    logsiri     = log(siri),
    regimen_cat = factor(regimen, levels = xlev$regimen_cat),
    ecog_cat_3  = factor(ecog, levels = xlev$ecog_cat_3),
    CACS        = factor(cacs, levels = xlev$CACS))
  X  <- model.matrix(rhs, data = nd, xlev = xlev)
  cf <- coefs[colnames(X)]; cf[is.na(cf)] <- 0
  as.numeric(X %*% cf)
}
surv_prob   <- function(t, lp) exp(-(t / exp(lp))^(1 / scale))
median_surv <- function(lp) exp(lp) * (log(2))^scale

diam_choices <- c("> 5 cm"               = "GT5",
                  "<= 5 cm"              = "LE5",
                  "Non-measurable disease" = "NonMeasurable")

ui <- fluidPage(
  titlePanel("PANTHEIA-SIRI overall-survival calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("siri", "SIRI", value = 2.0, min = 0.05, max = 30, step = 0.1),
      selectInput("regimen", "First-line regimen",
                  choices = xlev$regimen_cat, selected = "Gem-Abraxane"),
      selectInput("ecog", "ECOG performance status",
                  choices = c("0", "1", "2+"), selected = "1"),
      selectInput("cacs", "Cancer anorexia-cachexia syndrome",
                  choices = c("No", "Yes"), selected = "No"),
      selectInput("diam3", "Tumour burden (sum of RECIST target lesions)",
                  choices = diam_choices, selected = "GT5"),
      tags$hr(),
      tags$small("Research tool; not for clinical use.")
    ),
    mainPanel(
      fluidRow(
        column(4, wellPanel(tags$b("Median OS"), textOutput("med"))),
        column(4, wellPanel(tags$b("6-month OS"), textOutput("s6"))),
        column(4, wellPanel(tags$b("12-month OS"), textOutput("s12")))
      ),
      plotOutput("curve", height = "380px")
    )
  )
)

server <- function(input, output, session) {
  lp <- reactive(linear_predictor(input$siri, input$regimen, input$ecog, input$cacs, input$diam3))
  output$med <- renderText(sprintf("%.1f months", median_surv(lp())))
  output$s6  <- renderText(sprintf("%.0f%%", 100 * surv_prob(6,  lp())))
  output$s12 <- renderText(sprintf("%.0f%%", 100 * surv_prob(12, lp())))
  output$curve <- renderPlot({
    tt <- seq(0, 36, by = 0.25)
    df <- data.frame(t = tt, s = 100 * surv_prob(tt, lp()))
    ggplot(df, aes(t, s)) +
      geom_line(colour = "#C0392B", linewidth = 1.2) +
      geom_vline(xintercept = c(6, 12), linetype = "dashed", colour = "grey60") +
      geom_vline(xintercept = median_surv(lp()), linetype = "dotted", colour = "#1F618D") +
      coord_cartesian(xlim = c(0, 36), ylim = c(0, 100)) +
      labs(x = "Months from metastasis", y = "Predicted overall survival (%)") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
