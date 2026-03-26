library(shiny)
library(shinydashboard)
library(personalized)
library(xgboost)
library(ggplot2)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

format_label <- function(x) {
  gsub(" +", " ", trimws(gsub("_", " ", x)))
}

friendly_labels <- c(
  ndi_w0 = "Baseline NDI (0-100)",
  vas_neck_w0 = "Baseline neck pain VAS (0-100)",
  vas_arm_w0 = "Baseline arm pain VAS (0-100)",
  hnp_number_c7t1 = "HNP at C7-T1",
  work = "Physical workload",
  hnp_number_c45 = "HNP at C4-C5",
  educ = "Education level",
  hnp_loc_central = "Central HNP location",
  married = "Married",
  opioid = "Opioid use",
  hnp_loc_form = "Foraminal HNP location",
  armpain_freq = "Arm pain frequency",
  sick_leave = "Sick leave",
  neckpain_freq = "Neck pain frequency",
  alcohol = "Alcohol use",
  daytime = "Employment status",
  hnp_number_c67 = "HNP at C6-C7",
  nsaid = "NSAID use",
  loss_of_strength = "Loss of strength",
  tingl_freq = "Tingling frequency",
  hnp_loc_ml = "Mediolateral HNP location",
  root_compress = "Root compression",
  hnp_size = "HNP size",
  smoking = "Smoking",
  children = "Children",
  myel_compress_lvl = "Myelopathy compression level"
)

input_groups <- list(
  "Symptoms" = c(
    "ndi_w0",
    "vas_neck_w0",
    "vas_arm_w0",
    "armpain_freq",
    "neckpain_freq",
    "loss_of_strength",
    "tingl_freq"
  ),
  "Imaging and Clinical" = c(
    "hnp_number_c45",
    "hnp_number_c67",
    "hnp_number_c7t1",
    "hnp_loc_central",
    "hnp_loc_form",
    "hnp_loc_ml",
    "root_compress",
    "hnp_size",
    "myel_compress_lvl"
  ),
  "Background" = c(
    "work",
    "educ",
    "married",
    "children",
    "sick_leave",
    "alcohol",
    "smoking",
    "opioid",
    "nsaid",
    "daytime"
  )
)

model_bundle <- readRDS("~/Documents/Casino/personalised_model.RDS")

input_data <- model_bundle$collin_varz$result$df
input_data$vas_neck_w0 <- model_bundle$df$vas_neck_w0
input_data <- input_data[, unique(names(input_data)), drop = FALSE]

input_names <- names(input_data)

default_values <- setNames(
  lapply(input_names, function(var_name) {
    values <- input_data[[var_name]]
    if (is.factor(values)) {
      as.character(mode_value(values))
    } else {
      round(stats::median(values, na.rm = TRUE), 1)
    }
  }),
  input_names
)

model_obj <- model_bundle$subgrp.model1
validation_obj <- model_bundle$validation1
outcome_name <- "NDI @ 1 year"

interaction_plot <- function(validation_obj, ylab_text) {
  temp <- plot(validation_obj, type = "interaction")$data

  temp$Recommended <- factor(
    temp$Recommended,
    levels = c(0, 1),
    labels = c("Conservative", "Surgery")
  )
  temp$Received <- factor(
    temp$Received,
    levels = c(0, 1),
    labels = c("Conservative", "Surgery")
  )

  ggplot(temp, aes(x = Recommended, y = Value, color = Received, group = Received)) +
    geom_errorbar(
      aes(
        ymin = Value - 1.96 * SE,
        ymax = Value + 1.96 * SE
      ),
      width = 0.08,
      linewidth = 0.6,
      alpha = 0.7
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c("#1f78b4", "#e67e22")) +
    labs(
      x = "Recommended treatment",
      y = ylab_text,
      color = "Actual treatment received"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
}

build_input_control <- function(var_name) {
  values <- input_data[[var_name]]
  label <- friendly_labels[[var_name]] %||% format_label(var_name)

  if (is.factor(values)) {
    return(
      selectInput(
        inputId = var_name,
        label = label,
        choices = levels(values),
        selected = default_values[[var_name]]
      )
    )
  }

  numericInput(
    inputId = var_name,
    label = label,
    value = default_values[[var_name]],
    min = floor(min(values, na.rm = TRUE)),
    max = ceiling(max(values, na.rm = TRUE)),
    step = 0.1
  )
}

build_patient_profile <- function(input_values) {
  patient <- input_data[1, , drop = FALSE]

  for (var_name in input_names) {
    if (is.factor(input_data[[var_name]])) {
      patient[[var_name]] <- factor(
        input_values[[var_name]],
        levels = levels(input_data[[var_name]])
      )
    } else {
      patient[[var_name]] <- as.numeric(input_values[[var_name]])
    }
  }

  patient
}

build_design_matrix <- function(patient_data, var_names) {
  x_new <- model.matrix(~ 0 + ., data = patient_data)
  missing_cols <- setdiff(var_names, colnames(x_new))

  if (length(missing_cols) > 0) {
    missing_matrix <- matrix(
      0,
      nrow = nrow(x_new),
      ncol = length(missing_cols),
      dimnames = list(NULL, missing_cols)
    )
    x_new <- cbind(x_new, missing_matrix)
  }

  x_new[, var_names, drop = FALSE]
}

validation_plot <- interaction_plot(validation_obj, outcome_name)

server <- function(input, output, session) {
  observeEvent(input$reset_defaults, {
    for (var_name in input_names) {
      if (is.factor(input_data[[var_name]])) {
        updateSelectInput(
          session = session,
          inputId = var_name,
          selected = default_values[[var_name]]
        )
      } else {
        updateNumericInput(
          session = session,
          inputId = var_name,
          value = default_values[[var_name]]
        )
      }
    }
  })

  patient_profile <- reactive({
    numeric_inputs_ok <- all(vapply(
      input_names[!vapply(input_data, is.factor, logical(1))],
      function(var_name) {
        !is.null(input[[var_name]]) && is.finite(input[[var_name]])
      },
      logical(1)
    ))

    req(numeric_inputs_ok)

    input_values <- setNames(vector("list", length(input_names)), input_names)

    for (var_name in input_names) {
      input_values[[var_name]] <- input[[var_name]]
    }

    build_patient_profile(input_values)
  })

  recommendation <- reactive({
    patient_data <- patient_profile()
    design_matrix <- build_design_matrix(patient_data, model_obj$var.names)
    trt_group <- as.numeric(predict(model_obj, design_matrix, type = "trt.group"))
    benefit_score <- as.numeric(predict(model_obj, design_matrix, type = "benefit.score"))
    treatment <- if (trt_group == 1) "Surgery" else "Conservative"

    data.frame(
      outcome = outcome_name,
      recommendation = treatment,
      benefit_score = benefit_score,
      note = if (treatment == "Surgery") {
        "This model favors surgery."
      } else {
        "This model favors conservative care."
      },
      stringsAsFactors = FALSE
    )
  })

  output$summary_cards <- renderUI({
    recs <- recommendation()
    treatment <- recs$recommendation[1]
    card_class <- if (treatment == "Surgery") {
      "summary-card majority-surgery"
    } else {
      "summary-card majority-conservative"
    }

    fluidRow(
      column(
        width = 12,
        div(
          class = card_class,
          div(class = "summary-label", outcome_name),
          div(class = "summary-value", treatment),
          div(class = "summary-meta", sprintf("Benefit score %.2f", recs$benefit_score[1]))
        )
      )
    )
  })

  output$outcome_cards <- renderUI({
    recs <- recommendation()
    row <- recs[1, , drop = FALSE]

    fluidRow(
      column(
        width = 12,
        div(
          class = paste(
            "outcome-card",
            if (row$recommendation == "Surgery") "outcome-surgery" else "outcome-conservative"
          ),
          div(class = "outcome-kicker", row$outcome),
          div(class = "recommendation-row",
            div(class = "recommendation-label", row$recommendation),
            div(class = "score-pill", sprintf("Score %.2f", row$benefit_score))
          ),
          p(class = "recommendation-note", row$note)
        )
      )
    )
  })

  output$recommendation_table <- renderTable({
    recs <- recommendation()
    recs$benefit_score <- sprintf("%.2f", recs$benefit_score)
    recs
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$score_plot <- renderPlot({
    recs <- recommendation()
    recs$outcome <- factor(recs$outcome, levels = outcome_name)

    ggplot(recs, aes(x = benefit_score, y = outcome, fill = recommendation)) +
      geom_vline(xintercept = 0, color = "#6c7a89", linewidth = 0.6, linetype = "dashed") +
      geom_col(width = 0.65) +
      scale_fill_manual(values = c("Conservative" = "#1f78b4", "Surgery" = "#e67e22")) +
      labs(
        x = "Benefit score",
        y = NULL,
        fill = NULL,
        caption = "Negative scores favor surgery; positive scores favor conservative."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  }, res = 110)

  output$validation_plot_1 <- renderPlot({
    validation_plot
  }, res = 110)
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Casino Dashboard", titleWidth = 320),
  dashboardSidebar(
    width = 380,
    sidebarMenu(
      id = "tabs",
      menuItem("Recommendations", tabName = "recommendations", icon = icon("user-md")),
      menuItem("Validation", tabName = "validation", icon = icon("chart-line"))
    ),
    tags$div(
      class = "sidebar-scroll",
      tags$h4("Patient profile"),
      tags$p(
        "Enter baseline variables. The dashboard updates automatically for the fitted NDI @ 1 year model."
      ),
      tags$p(
        class = "sidebar-note",
        "Defaults are training-set medians or most common categories."
      ),
      lapply(names(input_groups), function(group_name) {
        vars <- input_groups[[group_name]]
        tags$div(
          class = "input-group",
          tags$h5(group_name),
          lapply(vars, build_input_control)
        )
      }),
      div(
        style = "margin-top: 10px;",
        actionButton("reset_defaults", "Reset defaults")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        :root {
          --bg: #eef2f1;
          --ink: #182126;
          --muted: #61717a;
          --line: rgba(24, 33, 38, 0.08);
          --conservative: #1f6f78;
          --surgery: #c96a2a;
          --accent: #23343b;
        }
        .content-wrapper, .right-side {
          background:
            radial-gradient(circle at top right, rgba(201,106,42,0.10), transparent 26%),
            radial-gradient(circle at top left, rgba(31,111,120,0.10), transparent 24%),
            var(--bg);
          color: var(--ink);
        }
        .main-header .logo {
          font-weight: 700;
          letter-spacing: 0.04em;
          background: #182126;
          font-size: 18px;
        }
        .main-header .navbar {
          background: #23343b;
        }
        .main-sidebar {
          background: #203039;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a {
          background: rgba(255,255,255,0.08);
          border-left-color: #d8c7a1;
        }
        .sidebar-scroll {
          padding: 16px;
        }
        .sidebar-scroll h4 {
          color: #fffaf1;
          font-weight: 700;
          margin-top: 6px;
          margin-bottom: 8px;
        }
        .sidebar-scroll p,
        .sidebar-scroll label,
        .sidebar-scroll h5 {
          color: #e8edf0;
        }
        .sidebar .form-control {
          border-radius: 12px;
          border: 0;
          min-height: 42px;
          box-shadow: none;
        }
        .sidebar .btn-default {
          border-radius: 12px;
          border: 0;
          background: #d8c7a1;
          color: #1f2a30;
          font-weight: 700;
          margin-top: 8px;
        }
        .input-group {
          border-top: 1px solid rgba(255,255,255,0.12);
          margin-top: 18px;
          padding-top: 18px;
          display: block;
        }
        .input-group h5 {
          font-weight: 700;
          margin-bottom: 12px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          font-size: 11px;
        }
        .sidebar-note {
          font-size: 12px;
          opacity: 0.8;
        }
        .hero-card,
        .summary-card,
        .outcome-card,
        .panel-card {
          border-radius: 22px;
          border: 1px solid var(--line);
          background: rgba(255,255,255,0.82);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .hero-card {
          padding: 24px 26px;
          margin-bottom: 20px;
          background: linear-gradient(135deg, rgba(24,33,38,0.96), rgba(35,52,59,0.96));
          color: #f7f4ee;
          border: 0;
        }
        .hero-eyebrow {
          text-transform: uppercase;
          letter-spacing: 0.12em;
          font-size: 11px;
          opacity: 0.72;
          margin-bottom: 8px;
        }
        .hero-title {
          font-size: 30px;
          line-height: 1.1;
          font-weight: 700;
          margin: 0 0 10px 0;
        }
        .hero-copy {
          max-width: 760px;
          color: rgba(247,244,238,0.82);
          margin: 0;
          font-size: 15px;
        }
        .summary-card {
          padding: 20px 22px;
          min-height: 150px;
          margin-bottom: 18px;
        }
        .majority-conservative {
          background: linear-gradient(180deg, rgba(31,111,120,0.18), #ffffff);
        }
        .majority-surgery {
          background: linear-gradient(180deg, rgba(201,106,42,0.18), #ffffff);
        }
        .summary-label {
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: var(--muted);
          margin-bottom: 16px;
        }
        .summary-value {
          font-size: 34px;
          font-weight: 700;
          line-height: 1;
          color: var(--ink);
          margin-bottom: 12px;
        }
        .summary-meta {
          color: var(--muted);
          font-size: 13px;
        }
        .outcome-card {
          padding: 20px 22px;
          margin-bottom: 18px;
          min-height: 164px;
          position: relative;
          overflow: hidden;
        }
        .outcome-card:before {
          content: '';
          position: absolute;
          left: 0;
          top: 0;
          bottom: 0;
          width: 6px;
        }
        .outcome-conservative:before {
          background: var(--conservative);
        }
        .outcome-surgery:before {
          background: var(--surgery);
        }
        .outcome-kicker {
          color: var(--muted);
          text-transform: uppercase;
          letter-spacing: 0.08em;
          font-size: 11px;
          margin-bottom: 14px;
        }
        .recommendation-row {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
          gap: 12px;
          margin-bottom: 10px;
        }
        .recommendation-label {
          font-size: 28px;
          font-weight: 700;
          margin: 0;
          line-height: 1.1;
        }
        .score-pill {
          border-radius: 999px;
          padding: 7px 12px;
          background: #f4f1eb;
          color: var(--accent);
          font-size: 12px;
          font-weight: 700;
          white-space: nowrap;
        }
        .recommendation-note {
          margin-bottom: 0;
          color: var(--muted);
          font-size: 14px;
          line-height: 1.5;
        }
        .box {
          border-top: 0;
        }
        .panel-card.box {
          background: rgba(255,255,255,0.84);
          border-radius: 22px;
          border: 1px solid var(--line);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .panel-card .box-header {
          padding: 18px 20px 0 20px;
        }
        .panel-card .box-title {
          font-weight: 700;
          color: var(--ink);
        }
        .panel-card .box-body {
          padding: 16px 20px 20px 20px;
        }
        .table {
          background: transparent;
        }
        @media (max-width: 991px) {
          .hero-title {
            font-size: 24px;
          }
          .recommendation-row {
            flex-direction: column;
            align-items: flex-start;
          }
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "recommendations",
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-card",
              div(class = "hero-eyebrow", "Personalised treatment support"),
              h2(class = "hero-title", "Recommendation from one patient profile"),
              p(
                class = "hero-copy",
                "Use the sidebar to enter baseline variables. The dashboard estimates which treatment is favored by the fitted NDI @ 1 year model and shows the model benefit score."
              )
            )
          )
        ),
        uiOutput("summary_cards"),
        uiOutput("outcome_cards"),
        fluidRow(
          box(
            class = "panel-card",
            width = 6,
            title = "Recommendation table",
            tableOutput("recommendation_table")
          ),
          box(
            class = "panel-card",
            width = 6,
            title = "Benefit score",
            plotOutput("score_plot", height = 320)
          )
        ),
        fluidRow(
          box(
            class = "panel-card",
            width = 12,
            title = "Interpretation note",
            p(
              "This app shows the recommendation from the fitted NDI @ 1 year model."
            )
          )
        )
      ),
      tabItem(
        tabName = "validation",
        fluidRow(
          box(
            class = "panel-card",
            width = 12,
            title = "Validation interaction plot",
            plotOutput(
              outputId = "validation_plot_1",
              height = 420
            )
          )
        )
      )
    )
  )
)

shinyApp(ui, server)


