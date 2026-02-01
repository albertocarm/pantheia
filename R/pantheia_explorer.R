#' @title PANTHEIA Data Explorer
#'
#' @description
#' Launch an interactive Shiny application for comprehensive exploration of the
#' PANTHEIA registry data. The app provides univariate and bivariate analyses,
#' correlation matrices, and data export capabilities.
#'
#' @param data A data frame to explore. Defaults to \code{pantheia_data} from
#' the pantheia package.
#' @param launch.browser Logical. If \code{TRUE} (default), the app opens in
#' a browser.
#'
#' @return Launches a Shiny application. Returns invisibly.
#'
#' @details
#' The explorer includes five main tabs:
#' \describe{
#'   \item{Overview}{Summary statistics, variable types, and missing data patterns}
#'   \item{Univariate}{Distribution plots and descriptive statistics for individual variables}
#'   \item{Bivariate}{Cross-tabulations and association analyses between pairs of variables}
#'   \item{Correlations}{Correlation matrix for numeric variables with multiple methods}
#'   \item{Data}{Full searchable and exportable data table}
#' }
#'
#' @examples
#' \dontrun{
#' # Launch with default PANTHEIA data
#' pantheia_explorer()
#'
#' # Launch with custom data
#' pantheia_explorer(data = my_custom_data)
#' }
#'
#' @importFrom shiny shinyApp reactive observe req renderText renderUI
#' @importFrom shiny conditionalPanel radioButtons checkboxInput sliderInput selectInput
#' @importFrom shiny updateSelectInput tags hr helpText textOutput uiOutput
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_item bs_theme
#' @importFrom bslib font_google layout_columns layout_sidebar sidebar card card_header card_body
#' @importFrom ggplot2 ggplot aes geom_col geom_histogram geom_density geom_boxplot
#' @importFrom ggplot2 geom_violin geom_point geom_segment geom_tile geom_text geom_smooth geom_jitter
#' @importFrom ggplot2 geom_hex geom_density_2d_filled geom_rug geom_vline stat_summary
#' @importFrom ggplot2 coord_flip coord_polar scale_fill_manual scale_fill_gradient scale_fill_gradient2
#' @importFrom ggplot2 scale_fill_viridis_d scale_color_manual labs theme_minimal theme theme_void
#' @importFrom ggplot2 element_text element_blank element_line margin expand_limits annotate
#' @importFrom dplyr filter mutate arrange select group_by ungroup summarise count n
#' @importFrom dplyr desc
#' @importFrom tidyr pivot_longer
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom plotly ggplotly renderPlotly plotlyOutput config layout
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_to_title
#' @importFrom stats cor cor.test kruskal.test chisq.test complete.cases
#' @importFrom stats median quantile sd IQR hclust as.dist na.omit
#'
#' @export
pantheia_explorer <- function(data = NULL, launch.browser = TRUE) {


  # ---------------------------------------------------------------------------

  # Data Validation
 
# ---------------------------------------------------------------------------

  if (is.null(data)) {
    if (!requireNamespace("pantheia", quietly = TRUE)) {
      stop("Package 'pantheia' is required. Please install it.", call. = FALSE
)
    }
    data("pantheia_data", package = "pantheia", envir = environment())
    df <- pantheia_data
  } else {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame.", call. = FALSE)
    }
    df <- data
  }

  # ---------------------------------------------------------------------------
  # Helper Functions
  # ---------------------------------------------------------------------------

 
 # Classify variable types
  classify_vars <- function(data) {
    sapply(data, function(x) {
      if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
        "date"
      } else if (is.factor(x) || is.character(x) || is.logical(x)) {
        "categorical"
      } else if (is.numeric(x)) {
        n_unique <- length(unique(stats::na.omit(x)))
        if (n_unique <= 10) "categorical" else "numeric"
      } else {
        "other"
      }
    })
  }

  # Generate summary statistics for numeric variables
  numeric_summary <- function(x) {
    n_total <- length(x)
    n_miss <- sum(is.na(x))
    x <- stats::na.omit(x)
    data.frame(
      Statistic = c("N (valid)", "Missing", "Mean", "SD", "Median",
                    "Q1 (25%)", "Q3 (75%)", "Min", "Max", "IQR"),
      Value = c(
        length(x),
        n_miss,
        round(mean(x), 3),
        round(stats::sd(x), 3),
        round(stats::median(x), 3),
        round(stats::quantile(x, 0.25), 3),
        round(stats::quantile(x, 0.75), 3),
        round(min(x), 3),
        round(max(x), 3),
        round(stats::IQR(x), 3)
      )
    )
  }

  # Generate summary for categorical variables
  categorical_summary <- function(x) {
    tbl <- table(x, useNA = "ifany")
    result <- data.frame(
      Category = names(tbl),
      Count = as.integer(tbl),
      Percentage = round(100 * as.integer(tbl) / sum(tbl), 1)
    )
    result[order(-result$Count), ]
  }

  # Custom theme for plots
  theme_pantheia <- function() {
    ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = 16, color = "#2c3e50"),
        plot.subtitle = ggplot2::element_text(color = "#7f8c8d", size = 12),
        axis.title = ggplot2::element_text(face = "bold", color = "#34495e"),
        axis.text = ggplot2::element_text(color = "#7f8c8d"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#ecf0f1"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
  }

  # Color palette
  colors_pantheia <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6",
                       "#1abc9c", "#e67e22", "#34495e", "#16a085", "#c0392b",
                       "#27ae60", "#2980b9", "#8e44ad", "#d35400", "#c0392b")

  # ---------------------------------------------------------------------------
  # UI
  # ---------------------------------------------------------------------------

  ui <- bslib::page_navbar(
    title = shiny::tags$span(
      shiny::tags$i(class = "fas fa-database", style = "margin-right: 10px;"),
      "PANTHEIA Data Explorer"
    ),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#3498db",
      secondary = "#95a5a6",
      success = "#2ecc71",
      info = "#1abc9c",
      warning = "#f39c12",
      danger = "#e74c3c",
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Inter")
    ),
    fillable = TRUE,

    header = shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      shiny::tags$style(shiny::HTML("
        .card { border: none; box-shadow: 0 2px 10px rgba(0,0,0,0.08); border-radius: 12px; }
        .card-header {
          background: linear-gradient(135deg, #3498db, #2980b9);
          color: white;
          font-weight: 600;
          border-radius: 12px 12px 0 0 !important;
        }
        .nav-link.active { font-weight: 600; }
        .stat-card { text-align: center; padding: 25px; background: white; }
        .stat-value { font-size: 2.8rem; font-weight: 700; color: #3498db; }
        .stat-label { color: #7f8c8d; font-size: 0.85rem; text-transform: uppercase; letter-spacing: 1px; }
        .plot-container { min-height: 400px; }
        .sidebar { background: #f8f9fa; }
        hr { border-color: #ecf0f1; }
        .form-label { font-weight: 600; color: #34495e; }
      "))
    ),

    # -------------------------------------------------------------------------
    # Tab 1: Overview
    # -------------------------------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(
        shiny::tags$i(class = "fas fa-chart-pie", style = "margin-right: 8px;"),
        "Overview"
      ),

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,

        bslib::card(
          class = "stat-card",
          shiny::div(class = "stat-value", shiny::textOutput("n_obs", inline = TRUE)),
          shiny::div(class = "stat-label", "Observations")
        ),
        bslib::card(
          class = "stat-card",
          shiny::div(class = "stat-value", shiny::textOutput("n_vars", inline = TRUE)),
          shiny::div(class = "stat-label", "Variables")
        ),
        bslib::card(
          class = "stat-card",
          shiny::div(class = "stat-value", shiny::textOutput("n_numeric", inline = TRUE)),
          shiny::div(class = "stat-label", "Numeric")
        ),
        bslib::card(
          class = "stat-card",
          shiny::div(class = "stat-value", shiny::textOutput("n_categorical", inline = TRUE)),
          shiny::div(class = "stat-label", "Categorical")
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_header(
            shiny::tags$i(class = "fas fa-list", style = "margin-right: 8px;"),
            "Variable Types"
          ),
          bslib::card_body(plotly::plotlyOutput("var_types_plot", height = "320px"))
        ),
        bslib::card(
          bslib::card_header(
            shiny::tags$i(class = "fas fa-exclamation-triangle", style = "margin-right: 8px;"),
            "Missing Data Pattern"
          ),
          bslib::card_body(plotly::plotlyOutput("missing_plot", height = "320px"))
        )
      ),

      bslib::card(
        bslib::card_header(
          shiny::tags$i(class = "fas fa-table", style = "margin-right: 8px;"),
          "Data Preview (First 100 Rows)"
        ),
        bslib::card_body(DT::DTOutput("data_preview"))
      )
    ),

    # -------------------------------------------------------------------------
    # Tab 2: Univariate Analysis
    # -------------------------------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(
        shiny::tags$i(class = "fas fa-chart-bar", style = "margin-right: 8px;"),
        "Univariate"
      ),

      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          title = "Variable Selection",
          shiny::selectInput(
            "uni_var",
            label = shiny::tags$span(
              shiny::tags$i(class = "fas fa-columns", style = "margin-right: 5px;"),
              "Select Variable"
            ),
            choices = NULL,
            width = "100%"
          ),
          shiny::hr(),
          shiny::uiOutput("uni_options")
        ),

        bslib::layout_columns(
          col_widths = c(8, 4),
          bslib::card(
            bslib::card_header(
              shiny::tags$i(class = "fas fa-chart-area", style = "margin-right: 8px;"),
              "Distribution"
            ),
            bslib::card_body(
              class = "plot-container",
              plotly::plotlyOutput("uni_plot", height = "480px")
            )
          ),
          bslib::card(
            bslib::card_header(
              shiny::tags$i(class = "fas fa-calculator", style = "margin-right: 8px;"),
              "Summary Statistics"
            ),
            bslib::card_body(DT::DTOutput("uni_summary"))
          )
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Tab 3: Bivariate Analysis
    # -------------------------------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(
        shiny::tags$i(class = "fas fa-object-group", style = "margin-right: 8px;"),
        "Bivariate (2\u00d72)"
      ),

      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          title = "Cross Variables",
          shiny::selectInput(
            "bi_var_x",
            label = shiny::tags$span(
              shiny::tags$i(class = "fas fa-arrow-right", style = "margin-right: 5px;"),
              "X Variable"
            ),
            choices = NULL,
            width = "100%"
          ),
          shiny::selectInput(
            "bi_var_y",
            label = shiny::tags$span(
              shiny::tags$i(class = "fas fa-arrow-up", style = "margin-right: 5px;"),
              "Y Variable"
            ),
            choices = NULL,
            width = "100%"
          ),
          shiny::hr(),
          shiny::uiOutput("bi_options")
        ),

        bslib::layout_columns(
          col_widths = c(8, 4),
          bslib::card(
            bslib::card_header(shiny::textOutput("bi_plot_title")),
            bslib::card_body(
              class = "plot-container",
              plotly::plotlyOutput("bi_plot", height = "520px")
            )
          ),
          bslib::card(
            bslib::card_header(
              shiny::tags$i(class = "fas fa-info-circle", style = "margin-right: 8px;"),
              "Association Analysis"
            ),
            bslib::card_body(
              shiny::uiOutput("bi_stats"),
              shiny::hr(),
              shiny::tags$h6("Cross-tabulation", class = "text-muted"),
              DT::DTOutput("bi_table")
            )
          )
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Tab 4: Correlation Matrix
    # -------------------------------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(
        shiny::tags$i(class = "fas fa-project-diagram", style = "margin-right: 8px;"),
        "Correlations"
      ),

      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          title = "Options",
          shiny::selectInput(
            "cor_method",
            label = "Correlation Method",
            choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
            selected = "spearman"
          ),
          shiny::checkboxInput("cor_show_values", "Show Correlation Values", TRUE),
          shiny::sliderInput("cor_text_size", "Text Size", min = 2, max = 5, value = 3, step = 0.5),
          shiny::hr(),
          shiny::helpText("Only numeric variables with >10 unique values are included.")
        ),

        bslib::card(
          bslib::card_header(
            shiny::tags$i(class = "fas fa-th", style = "margin-right: 8px;"),
            "Correlation Matrix (Numeric Variables)"
          ),
          bslib::card_body(plotly::plotlyOutput("cor_plot", height = "650px"))
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Tab 5: Full Data Table
    # -------------------------------------------------------------------------
    bslib::nav_panel(
      title = shiny::tags$span(
        shiny::tags$i(class = "fas fa-database", style = "margin-right: 8px;"),
        "Data"
      ),
      bslib::card(
        bslib::card_header(
          shiny::tags$i(class = "fas fa-search", style = "margin-right: 8px;"),
          "Full Dataset (Searchable & Exportable)"
        ),
        bslib::card_body(DT::DTOutput("full_data"))
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::tags$span(
        class = "navbar-text text-muted small",
        shiny::tags$i(class = "fas fa-code", style = "margin-right: 5px;"),
        "Built with Shiny + bslib"
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Server
  # ---------------------------------------------------------------------------

  server <- function(input, output, session) {

    # Reactive: Variable classification
    var_types <- shiny::reactive({
      classify_vars(df)
    })

    numeric_vars <- shiny::reactive({
      names(var_types()[var_types() == "numeric"])
    })

    categorical_vars <- shiny::reactive({
      names(var_types()[var_types() == "categorical"])
    })

    # Initialize inputs
    shiny::observe({
      all_vars <- names(df)

      shiny::updateSelectInput(session, "uni_var", choices = all_vars, selected = all_vars[1])
      shiny::updateSelectInput(session, "bi_var_x", choices = all_vars, selected = all_vars[1])
      shiny::updateSelectInput(
        session, "bi_var_y",
        choices = all_vars,
        selected = if (length(all_vars) > 1) all_vars[2] else all_vars[1]
      )
    })

    # -------------------------------------------------------------------------
    # Overview Tab
    # -------------------------------------------------------------------------

    output$n_obs <- shiny::renderText({ format(nrow(df), big.mark = ",") })
    output$n_vars <- shiny::renderText({ ncol(df) })
    output$n_numeric <- shiny::renderText({ sum(var_types() == "numeric") })
    output$n_categorical <- shiny::renderText({ sum(var_types() == "categorical") })

    output$var_types_plot <- plotly::renderPlotly({
      type_counts <- data.frame(
        Type = c("Numeric", "Categorical", "Date", "Other"),
        Count = c(
          sum(var_types() == "numeric"),
          sum(var_types() == "categorical"),
          sum(var_types() == "date"),
          sum(var_types() == "other")
        )
      )
      type_counts <- type_counts[type_counts$Count > 0, ]
      type_counts$Type <- forcats::fct_reorder(type_counts$Type, type_counts$Count)

      p <- ggplot2::ggplot(type_counts, ggplot2::aes(x = .data$Type, y = .data$Count, fill = .data$Type)) +
        ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = .data$Count), hjust = -0.3, fontface = "bold", size = 4) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(
          "Numeric" = "#3498db", "Categorical" = "#2ecc71",
          "Date" = "#f39c12", "Other" = "#95a5a6"
        )) +
        ggplot2::labs(x = NULL, y = "Number of Variables", title = "Variable Type Distribution") +
        theme_pantheia() +
        ggplot2::expand_limits(y = max(type_counts$Count) * 1.2)

      plotly::ggplotly(p, tooltip = c("y")) |>
        plotly::config(displayModeBar = FALSE) |>
        plotly::layout(margin = list(l = 10, r = 30, t = 50, b = 50))
    })

    output$missing_plot <- plotly::renderPlotly({
      missing_pct <- sapply(df, function(x) 100 * sum(is.na(x)) / length(x))
      missing_df <- data.frame(
        Variable = names(missing_pct),
        Missing = missing_pct
      )
      missing_df <- missing_df[missing_df$Missing > 0, ]
      missing_df <- missing_df[order(-missing_df$Missing), ]
      missing_df <- utils::head(missing_df, 12)

      if (nrow(missing_df) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate(
            "text", x = 0.5, y = 0.5,
            label = "\u2713 No missing data!",
            size = 7, color = "#2ecc71", fontface = "bold"
          ) +
          ggplot2::theme_void()
      } else {
        missing_df$Variable <- forcats::fct_reorder(missing_df$Variable, missing_df$Missing)

        p <- ggplot2::ggplot(missing_df, ggplot2::aes(x = .data$Variable, y = .data$Missing)) +
          ggplot2::geom_col(fill = "#e74c3c", width = 0.7, alpha = 0.85) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(round(.data$Missing, 1), "%")),
            hjust = -0.15, size = 3.5, fontface = "bold"
          ) +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = "Missing (%)", title = "Variables with Missing Data") +
          theme_pantheia() +
          ggplot2::expand_limits(y = max(missing_df$Missing) * 1.2)
      }

      plotly::ggplotly(p, tooltip = c("y")) |>
        plotly::config(displayModeBar = FALSE) |>
        plotly::layout(margin = list(l = 10, r = 30, t = 50, b = 50))
    })

    output$data_preview <- DT::renderDT({
      DT::datatable(
        utils::head(df, 100),
        options = list(
          scrollX = TRUE,
          pageLength = 8,
          dom = "frtip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        class = "cell-border stripe hover compact",
        rownames = FALSE
      )
    })

    # -------------------------------------------------------------------------
    # Univariate Tab
    # -------------------------------------------------------------------------

    uni_var_type <- shiny::reactive({
      shiny::req(input$uni_var)
      var_types()[input$uni_var]
    })

    output$uni_options <- shiny::renderUI({
      shiny::req(input$uni_var)

      if (uni_var_type() == "numeric") {
        shiny::tagList(
          shiny::radioButtons(
            "uni_plot_type",
            label = "Plot Type",
            choices = c("Histogram" = "hist", "Density" = "density",
                        "Box Plot" = "box", "Violin" = "violin"),
            selected = "hist"
          ),
          shiny::conditionalPanel(
            condition = "input.uni_plot_type == 'hist'",
            shiny::sliderInput("uni_bins", "Number of Bins", min = 10, max = 80, value = 30)
          ),
          shiny::checkboxInput("uni_show_rug", "Show Data Points (rug)", FALSE)
        )
      } else {
        shiny::tagList(
          shiny::radioButtons(
            "uni_plot_type_cat",
            label = "Plot Type",
            choices = c("Bar Chart" = "bar", "Horizontal Bars" = "hbar", "Lollipop" = "lollipop"),
            selected = "hbar"
          ),
          shiny::checkboxInput("uni_sort", "Sort by Count", TRUE),
          shiny::sliderInput("uni_top_n", "Show Top N Categories", min = 5, max = 30, value = 15)
        )
      }
    })

    output$uni_plot <- plotly::renderPlotly({
      shiny::req(input$uni_var)

      var_data <- df[[input$uni_var]]
      var_name <- input$uni_var

      if (uni_var_type() == "numeric") {
        plot_type <- if (is.null(input$uni_plot_type)) "hist" else input$uni_plot_type
        show_rug <- if (is.null(input$uni_show_rug)) FALSE else input$uni_show_rug

        p <- if (plot_type == "hist") {
          bins <- if (is.null(input$uni_bins)) 30 else input$uni_bins
          med_val <- stats::median(var_data, na.rm = TRUE)

          base_p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_name]])) +
            ggplot2::geom_histogram(bins = bins, fill = "#3498db", color = "white", alpha = 0.85) +
            ggplot2::geom_vline(xintercept = med_val, color = "#e74c3c", linetype = "dashed", linewidth = 1.2) +
            ggplot2::annotate(
              "text", x = med_val, y = Inf,
              label = paste("Median:", round(med_val, 2)),
              hjust = -0.1, vjust = 2, color = "#e74c3c", fontface = "bold", size = 3.5
            ) +
            ggplot2::labs(
              title = paste("Distribution of", var_name),
              subtitle = "Red dashed line = Median",
              x = var_name, y = "Frequency"
            ) +
            theme_pantheia()

          if (show_rug) base_p <- base_p + ggplot2::geom_rug(alpha = 0.3, color = "#34495e")
          base_p

        } else if (plot_type == "density") {
          base_p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_name]])) +
            ggplot2::geom_density(fill = "#3498db", alpha = 0.6, color = "#2980b9", linewidth = 1) +
            ggplot2::labs(title = paste("Density Plot of", var_name), x = var_name, y = "Density") +
            theme_pantheia()

          if (show_rug) base_p <- base_p + ggplot2::geom_rug(alpha = 0.3, color = "#34495e")
          base_p

        } else if (plot_type == "box") {
          ggplot2::ggplot(df, ggplot2::aes(y = .data[[var_name]])) +
            ggplot2::geom_boxplot(
              fill = "#3498db", alpha = 0.75,
              outlier.color = "#e74c3c", outlier.size = 2, width = 0.5
            ) +
            ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#f39c12") +
            ggplot2::coord_flip() +
            ggplot2::labs(
              title = paste("Box Plot of", var_name),
              subtitle = "Orange diamond = Mean",
              x = NULL, y = var_name
            ) +
            theme_pantheia()

        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = "", y = .data[[var_name]])) +
            ggplot2::geom_violin(fill = "#3498db", alpha = 0.65, color = "#2980b9") +
            ggplot2::geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
            ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#f39c12") +
            ggplot2::labs(
              title = paste("Violin Plot of", var_name),
              subtitle = "Orange diamond = Mean",
              x = NULL, y = var_name
            ) +
            theme_pantheia()
        }

      } else {
        plot_type <- if (is.null(input$uni_plot_type_cat)) "hbar" else input$uni_plot_type_cat
        sort_by_count <- if (is.null(input$uni_sort)) TRUE else input$uni_sort
        top_n <- if (is.null(input$uni_top_n)) 15 else input$uni_top_n

        count_data <- as.data.frame(table(df[[var_name]], useNA = "ifany"))
        names(count_data) <- c(var_name, "Count")
        count_data <- count_data[order(-count_data$Count), ]
        count_data <- utils::head(count_data, top_n)
        count_data$Pct <- round(100 * count_data$Count / sum(count_data$Count), 1)

        if (sort_by_count) {
          count_data[[var_name]] <- forcats::fct_reorder(count_data[[var_name]], count_data$Count)
        }

        p <- if (plot_type == "bar") {
          ggplot2::ggplot(count_data, ggplot2::aes(
            x = .data[[var_name]], y = .data$Count, fill = .data[[var_name]]
          )) +
            ggplot2::geom_col(show.legend = FALSE, alpha = 0.9) +
            ggplot2::geom_text(ggplot2::aes(label = .data$Count), vjust = -0.3, fontface = "bold", size = 3.5) +
            ggplot2::scale_fill_manual(values = rep(colors_pantheia, length.out = nrow(count_data))) +
            ggplot2::labs(title = paste("Distribution of", var_name), x = var_name, y = "Count") +
            theme_pantheia() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::expand_limits(y = max(count_data$Count) * 1.1)

        } else if (plot_type == "hbar") {
          ggplot2::ggplot(count_data, ggplot2::aes(
            x = .data[[var_name]], y = .data$Count, fill = .data[[var_name]]
          )) +
            ggplot2::geom_col(show.legend = FALSE, alpha = 0.9, width = 0.75) +
            ggplot2::geom_text(
              ggplot2::aes(label = paste0(.data$Count, " (", .data$Pct, "%)")),
              hjust = -0.05, size = 3.2, fontface = "bold"
            ) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_manual(values = rep(colors_pantheia, length.out = nrow(count_data))) +
            ggplot2::labs(title = paste("Distribution of", var_name), x = NULL, y = "Count") +
            theme_pantheia() +
            ggplot2::expand_limits(y = max(count_data$Count) * 1.25)

        } else {
          ggplot2::ggplot(count_data, ggplot2::aes(x = .data[[var_name]], y = .data$Count)) +
            ggplot2::geom_segment(
              ggplot2::aes(xend = .data[[var_name]], yend = 0),
              color = "#bdc3c7", linewidth = 1.2
            ) +
            ggplot2::geom_point(size = 5, color = "#3498db") +
            ggplot2::geom_text(ggplot2::aes(label = .data$Count), hjust = -0.5, fontface = "bold", size = 3.5) +
            ggplot2::coord_flip() +
            ggplot2::labs(title = paste("Distribution of", var_name), x = NULL, y = "Count") +
            theme_pantheia() +
            ggplot2::expand_limits(y = max(count_data$Count) * 1.15)
        }
      }

      plotly::ggplotly(p) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE) |>
        plotly::layout(margin = list(l = 10, r = 30, t = 80, b = 60))
    })

    output$uni_summary <- DT::renderDT({
      shiny::req(input$uni_var)

      var_data <- df[[input$uni_var]]

      if (uni_var_type() == "numeric") {
        summary_df <- numeric_summary(var_data)
      } else {
        summary_df <- categorical_summary(var_data)
      }

      DT::datatable(
        summary_df,
        options = list(dom = "t", pageLength = 25, scrollY = "400px"),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # -------------------------------------------------------------------------
    # Bivariate Tab
    # -------------------------------------------------------------------------

    bi_var_types <- shiny::reactive({
      shiny::req(input$bi_var_x, input$bi_var_y)
      c(x = var_types()[input$bi_var_x], y = var_types()[input$bi_var_y])
    })

    bi_combination <- shiny::reactive({
      types <- bi_var_types()
      paste(types["x"], types["y"], sep = "_")
    })

    output$bi_plot_title <- shiny::renderText({
      shiny::req(input$bi_var_x, input$bi_var_y)
      paste(input$bi_var_x, "vs", input$bi_var_y)
    })

    output$bi_options <- shiny::renderUI({
      shiny::req(input$bi_var_x, input$bi_var_y)
      combo <- bi_combination()

      if (combo == "numeric_numeric") {
        shiny::tagList(
          shiny::radioButtons(
            "bi_plot_type_nn", "Plot Type",
            choices = c("Scatter Plot" = "scatter", "Hex Bins" = "hex", "2D Density" = "density2d"),
            selected = "scatter"
          ),
          shiny::checkboxInput("bi_add_smooth", "Add Linear Trend", TRUE),
          shiny::checkboxInput("bi_add_loess", "Add LOESS Smooth", FALSE)
        )
      } else if (combo %in% c("categorical_numeric", "numeric_categorical")) {
        shiny::tagList(
          shiny::radioButtons(
            "bi_plot_type_cn", "Plot Type",
            choices = c("Box Plot" = "box", "Violin Plot" = "violin", "Jitter Plot" = "jitter"),
            selected = "box"
          ),
          shiny::checkboxInput("bi_show_points", "Overlay Data Points", FALSE)
        )
      } else {
        shiny::tagList(
          shiny::radioButtons(
            "bi_plot_type_cc", "Plot Type",
            choices = c("Stacked Bar" = "stacked", "Grouped Bar" = "grouped",
                        "Proportion" = "fill", "Heatmap" = "heatmap"),
            selected = "grouped"
          )
        )
      }
    })

    output$bi_plot <- plotly::renderPlotly({
      shiny::req(input$bi_var_x, input$bi_var_y)

      combo <- bi_combination()
      var_x <- input$bi_var_x
      var_y <- input$bi_var_y

      p <- if (combo == "numeric_numeric") {
        plot_type <- if (is.null(input$bi_plot_type_nn)) "scatter" else input$bi_plot_type_nn
        add_smooth <- if (is.null(input$bi_add_smooth)) TRUE else input$bi_add_smooth
        add_loess <- if (is.null(input$bi_add_loess)) FALSE else input$bi_add_loess

        base_plot <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_x]], y = .data[[var_y]]))

        if (plot_type == "scatter") {
          base_plot <- base_plot + ggplot2::geom_point(alpha = 0.5, color = "#3498db", size = 2)
        } else if (plot_type == "hex") {
          base_plot <- base_plot +
            ggplot2::geom_hex(bins = 25) +
            ggplot2::scale_fill_gradient(low = "#ecf0f1", high = "#3498db", name = "Count")
        } else {
          base_plot <- base_plot +
            ggplot2::geom_density_2d_filled(alpha = 0.85) +
            ggplot2::scale_fill_viridis_d(option = "plasma")
        }

        if (add_smooth && plot_type == "scatter") {
          base_plot <- base_plot +
            ggplot2::geom_smooth(method = "lm", color = "#e74c3c", se = TRUE, alpha = 0.15, linewidth = 1.2)
        }

        if (add_loess && plot_type == "scatter") {
          base_plot <- base_plot +
            ggplot2::geom_smooth(method = "loess", color = "#2ecc71", se = FALSE, linetype = "dashed", linewidth = 1)
        }

        base_plot +
          ggplot2::labs(title = paste(var_x, "vs", var_y), x = var_x, y = var_y) +
          theme_pantheia()

      } else if (combo %in% c("categorical_numeric", "numeric_categorical")) {
        plot_type <- if (is.null(input$bi_plot_type_cn)) "box" else input$bi_plot_type_cn
        show_points <- if (is.null(input$bi_show_points)) FALSE else input$bi_show_points

        if (combo == "numeric_categorical") {
          temp <- var_x
          var_x <- var_y
          var_y <- temp
        }

        base_plot <- ggplot2::ggplot(df, ggplot2::aes(
          x = .data[[var_x]], y = .data[[var_y]], fill = .data[[var_x]]
        ))

        if (plot_type == "box") {
          base_plot <- base_plot +
            ggplot2::geom_boxplot(show.legend = FALSE, alpha = 0.8, outlier.alpha = 0.5)
          if (show_points) {
            base_plot <- base_plot +
              ggplot2::geom_jitter(width = 0.2, alpha = 0.3, size = 1, show.legend = FALSE)
          }
        } else if (plot_type == "violin") {
          base_plot <- base_plot +
            ggplot2::geom_violin(show.legend = FALSE, alpha = 0.75) +
            ggplot2::geom_boxplot(width = 0.12, fill = "white", show.legend = FALSE, alpha = 0.9)
          if (show_points) {
            base_plot <- base_plot +
              ggplot2::geom_jitter(width = 0.15, alpha = 0.3, size = 1, show.legend = FALSE)
          }
        } else {
          base_plot <- ggplot2::ggplot(df, ggplot2::aes(
            x = .data[[var_x]], y = .data[[var_y]], color = .data[[var_x]]
          )) +
            ggplot2::geom_jitter(width = 0.25, alpha = 0.6, size = 2, show.legend = FALSE) +
            ggplot2::stat_summary(fun = stats::median, geom = "crossbar", width = 0.5, color = "black", linewidth = 0.8)
        }

        base_plot +
          ggplot2::scale_fill_manual(values = colors_pantheia) +
          ggplot2::scale_color_manual(values = colors_pantheia) +
          ggplot2::labs(title = paste(var_y, "by", var_x), x = var_x, y = var_y) +
          theme_pantheia() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      } else {
        plot_type <- if (is.null(input$bi_plot_type_cc)) "grouped" else input$bi_plot_type_cc

        cross_data <- as.data.frame(table(df[[var_x]], df[[var_y]]))
        names(cross_data) <- c(var_x, var_y, "n")
        cross_data <- dplyr::group_by(cross_data, .data[[var_x]])
        cross_data <- dplyr::mutate(cross_data, pct = .data$n / sum(.data$n) * 100)
        cross_data <- dplyr::ungroup(cross_data)

        if (plot_type == "heatmap") {
          ggplot2::ggplot(cross_data, ggplot2::aes(
            x = .data[[var_x]], y = .data[[var_y]], fill = .data$n
          )) +
            ggplot2::geom_tile(color = "white", linewidth = 0.5) +
            ggplot2::geom_text(ggplot2::aes(label = .data$n), color = "white", fontface = "bold", size = 4) +
            ggplot2::scale_fill_gradient(low = "#85c1e9", high = "#1a5276", name = "Count") +
            ggplot2::labs(title = paste("Crosstab:", var_x, "\u00d7", var_y), x = var_x, y = var_y) +
            theme_pantheia() +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              panel.grid = ggplot2::element_blank()
            )
        } else {
          position <- switch(plot_type,
                             "stacked" = "stack",
                             "grouped" = "dodge",
                             "fill" = "fill")

          y_label <- if (plot_type == "fill") "Proportion" else "Count"
          y_var <- if (plot_type == "fill") cross_data$pct / 100 else cross_data$n

          ggplot2::ggplot(cross_data, ggplot2::aes(
            x = .data[[var_x]],
            y = if (plot_type == "fill") .data$pct / 100 else .data$n,
            fill = .data[[var_y]]
          )) +
            ggplot2::geom_col(position = position, alpha = 0.9, width = 0.75) +
            ggplot2::scale_fill_manual(values = colors_pantheia) +
            ggplot2::labs(title = paste(var_x, "by", var_y), x = var_x, y = y_label, fill = var_y) +
            theme_pantheia() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        }
      }

      plotly::ggplotly(p) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE) |>
        plotly::layout(margin = list(l = 10, r = 30, t = 80, b = 80))
    })

    output$bi_stats <- shiny::renderUI({
      shiny::req(input$bi_var_x, input$bi_var_y)

      combo <- bi_combination()
      var_x <- input$bi_var_x
      var_y <- input$bi_var_y

      if (combo == "numeric_numeric") {
        complete_cases <- stats::complete.cases(df[[var_x]], df[[var_y]])
        n_complete <- sum(complete_cases)

        cor_spearman <- stats::cor(df[[var_x]], df[[var_y]], use = "pairwise.complete.obs", method = "spearman")
        cor_pearson <- stats::cor(df[[var_x]], df[[var_y]], use = "pairwise.complete.obs", method = "pearson")

        cor_test <- tryCatch(
          stats::cor.test(df[[var_x]], df[[var_y]], method = "pearson"),
          error = function(e) NULL
        )

        shiny::tags$div(
          shiny::tags$h5(
            shiny::tags$i(class = "fas fa-chart-line text-primary", style = "margin-right: 8px;"),
            "Correlation Analysis"
          ),
          shiny::tags$table(
            class = "table table-sm",
            shiny::tags$tr(shiny::tags$td(shiny::tags$strong("N (complete)")), shiny::tags$td(n_complete)),
            shiny::tags$tr(shiny::tags$td(shiny::tags$strong("Pearson r")), shiny::tags$td(round(cor_pearson, 4))),
            shiny::tags$tr(shiny::tags$td(shiny::tags$strong("Spearman \u03c1")), shiny::tags$td(round(cor_spearman, 4))),
            if (!is.null(cor_test)) {
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$strong("p-value")),
                shiny::tags$td(format.pval(cor_test$p.value, digits = 4))
              )
            }
          )
        )

      } else if (combo %in% c("categorical_numeric", "numeric_categorical")) {
        if (combo == "numeric_categorical") {
          temp <- var_x
          var_x <- var_y
          var_y <- temp
        }

        test_result <- tryCatch(
          stats::kruskal.test(df[[var_y]] ~ df[[var_x]]),
          error = function(e) NULL
        )

        shiny::tags$div(
          shiny::tags$h5(
            shiny::tags$i(class = "fas fa-balance-scale text-primary", style = "margin-right: 8px;"),
            "Kruskal-Wallis Test"
          ),
          if (!is.null(test_result)) {
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$strong("Chi-squared")),
                shiny::tags$td(round(test_result$statistic, 3))
              ),
              shiny::tags$tr(shiny::tags$td(shiny::tags$strong("df")), shiny::tags$td(test_result$parameter)),
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$strong("p-value")),
                shiny::tags$td(format.pval(test_result$p.value, digits = 4))
              )
            )
          } else {
            shiny::tags$p(class = "text-muted", "Test not available")
          }
        )

      } else {
        tbl <- table(df[[var_x]], df[[var_y]])
        test_result <- tryCatch(stats::chisq.test(tbl), error = function(e) NULL)

        cramers_v <- tryCatch({
          chi2 <- test_result$statistic
          n <- sum(tbl)
          k <- min(nrow(tbl), ncol(tbl))
          sqrt(chi2 / (n * (k - 1)))
        }, error = function(e) NA)

        shiny::tags$div(
          shiny::tags$h5(
            shiny::tags$i(class = "fas fa-th text-primary", style = "margin-right: 8px;"),
            "Chi-squared Test"
          ),
          if (!is.null(test_result)) {
            shiny::tags$table(
              class = "table table-sm",
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$strong("X\u00b2")),
                shiny::tags$td(round(test_result$statistic, 3))
              ),
              shiny::tags$tr(shiny::tags$td(shiny::tags$strong("df")), shiny::tags$td(test_result$parameter)),
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$strong("p-value")),
                shiny::tags$td(format.pval(test_result$p.value, digits = 4))
              ),
              if (!is.na(cramers_v)) {
                shiny::tags$tr(
                  shiny::tags$td(shiny::tags$strong("Cram\u00e9r's V")),
                  shiny::tags$td(round(cramers_v, 4))
                )
              }
            )
          } else {
            shiny::tags$p(class = "text-muted", "Test not available")
          }
        )
      }
    })

    output$bi_table <- DT::renderDT({
      shiny::req(input$bi_var_x, input$bi_var_y)

      combo <- bi_combination()

      if (combo == "categorical_categorical") {
        cross_tab <- table(df[[input$bi_var_x]], df[[input$bi_var_y]])
        cross_df <- as.data.frame.matrix(cross_tab)
        cross_df <- cbind(Category = rownames(cross_df), cross_df)
        rownames(cross_df) <- NULL

        DT::datatable(
          cross_df,
          options = list(dom = "t", scrollX = TRUE, pageLength = 25, scrollY = "200px"),
          rownames = FALSE,
          class = "compact stripe hover"
        )
      } else if (combo %in% c("categorical_numeric", "numeric_categorical")) {
        var_cat <- if (combo == "categorical_numeric") input$bi_var_x else input$bi_var_y
        var_num <- if (combo == "categorical_numeric") input$bi_var_y else input$bi_var_x

        summary_df <- do.call(rbind, lapply(split(df[[var_num]], df[[var_cat]]), function(x) {
          data.frame(
            N = sum(!is.na(x)),
            Mean = round(mean(x, na.rm = TRUE), 2),
            SD = round(stats::sd(x, na.rm = TRUE), 2),
            Median = round(stats::median(x, na.rm = TRUE), 2)
          )
        }))
        summary_df <- cbind(Category = rownames(summary_df), summary_df)
        rownames(summary_df) <- NULL
        summary_df <- summary_df[order(-summary_df$N), ]

        DT::datatable(
          summary_df,
          options = list(dom = "t", scrollX = TRUE, pageLength = 25, scrollY = "200px"),
          rownames = FALSE,
          class = "compact stripe hover"
        )
      } else {
        NULL
      }
    })

    # -------------------------------------------------------------------------
    # Correlation Tab
    # -------------------------------------------------------------------------

    output$cor_plot <- plotly::renderPlotly({
      shiny::req(length(numeric_vars()) >= 2)

      num_data <- df[, numeric_vars(), drop = FALSE]
      cor_matrix <- stats::cor(num_data, use = "pairwise.complete.obs", method = input$cor_method)

      cor_df <- as.data.frame(cor_matrix)
      cor_df$Var1 <- rownames(cor_df)
      cor_long <- tidyr::pivot_longer(cor_df, cols = -"Var1", names_to = "Var2", values_to = "Correlation")

      hc <- stats::hclust(stats::as.dist(1 - abs(cor_matrix)))
      var_order <- rownames(cor_matrix)[hc$order]
      cor_long$Var1 <- factor(cor_long$Var1, levels = var_order)
      cor_long$Var2 <- factor(cor_long$Var2, levels = rev(var_order))

      p <- ggplot2::ggplot(cor_long, ggplot2::aes(
        x = .data$Var1, y = .data$Var2, fill = .data$Correlation
      )) +
        ggplot2::geom_tile(color = "white", linewidth = 0.3) +
        ggplot2::scale_fill_gradient2(
          low = "#e74c3c", mid = "white", high = "#3498db",
          midpoint = 0, limits = c(-1, 1),
          name = paste0(stringr::str_to_title(input$cor_method), "\nCorrelation")
        ) +
        theme_pantheia() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = ggplot2::element_text(size = 9),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          title = paste(stringr::str_to_title(input$cor_method), "Correlation Matrix"),
          subtitle = paste(length(numeric_vars()), "numeric variables")
        )

      if (input$cor_show_values) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = round(.data$Correlation, 2)),
          size = input$cor_text_size, color = "black"
        )
      }

      plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE) |>
        plotly::layout(margin = list(l = 100, r = 50, t = 80, b = 120))
    })

    # -------------------------------------------------------------------------
    # Data Tab
    # -------------------------------------------------------------------------

    output$full_data <- DT::renderDT({
      DT::datatable(
        df,
        filter = "top",
        extensions = c("Buttons", "Scroller"),
        options = list(
          scrollX = TRUE,
          scrollY = "550px",
          scroller = TRUE,
          pageLength = 50,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = "Copy"),
            list(extend = "csv", text = "CSV"),
            list(extend = "excel", text = "Excel")
          ),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        class = "cell-border stripe hover compact",
        rownames = FALSE
      )
    })
  }

  # ---------------------------------------------------------------------------
  # Run Application
  # ---------------------------------------------------------------------------
  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser))
}
