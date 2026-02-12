# =============================================================================
# gbifgaps — Gap Analysis Dashboard
# =============================================================================
# Interactive dashboard for identifying and prioritising biodiversity data gaps
# across spatial, temporal, and taxonomic dimensions.
#
# To run: shiny::runApp("shiny_app/gap_app")
# =============================================================================

library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(scales)
library(glue)
library(lubridate)

# =============================================================================
# LOAD DATA
# =============================================================================

data_path <- "data/shiny_data.rds"
if (!file.exists(data_path)) stop("shiny_data.rds not found in data/. Run scripts/11_prepare_gap_app_data.R")

message("Loading shiny data from: ", normalizePath(data_path))
app_data <- readRDS(data_path)

# Safe accessor
safe_get <- function(name) if (name %in% names(app_data)) app_data[[name]] else NULL

# Extract datasets
dashboard       <- safe_get("dashboard")
grid_10km       <- safe_get("grid_10km")
spatial_gaps    <- safe_get("spatial_gaps_10km")
cell_recency    <- safe_get("cell_recency_10km")
cell_summary    <- safe_get("cell_summary_10km")
time_summary    <- safe_get("time_summary_10km")
order_5yr       <- safe_get("order_5yr")
order_change    <- safe_get("order_change")
order_summary   <- safe_get("order_summary")
top_orders      <- safe_get("top_orders")
tax_by_threat   <- safe_get("tax_by_threat")
tax_by_rank     <- safe_get("tax_by_rank")
tax_by_order    <- safe_get("tax_by_order")
tax_by_family   <- safe_get("tax_by_family")
priority_zero   <- safe_get("priority_zero_cells")
priority_stale  <- safe_get("priority_stale_cells")
comparison_grids <- safe_get("comparison_grids")
metadata        <- safe_get("metadata")
spatial_overview <- safe_get("spatial_overview")

# Derived
basis_types   <- if (!is.null(spatial_gaps)) sort(unique(spatial_gaps$basisofrecord)) else "all"
basis_types_no_all <- basis_types[basis_types != "all"]
order_choices <- if (!is.null(top_orders)) top_orders$order else character(0)
current_year  <- year(Sys.Date())
country_name  <- tryCatch(yaml::read_yaml("../../config.yml")$country$name, error = function(e) "")

# Plotly theme helper — light background, warm palette
plotly_layout <- function(p, ...) {
  args <- list(...)
  
  # Default axis settings
  default_xaxis <- list(gridcolor = "#e8e7e1", zerolinecolor = "#e0dfda")
  default_yaxis <- list(gridcolor = "#e8e7e1", zerolinecolor = "#e0dfda")
  
  # Merge caller's axis settings ON TOP of defaults (caller wins)
  if (!is.null(args$xaxis)) {
    args$xaxis <- modifyList(default_xaxis, args$xaxis)
  } else {
    args$xaxis <- default_xaxis
  }
  if (!is.null(args$yaxis)) {
    args$yaxis <- modifyList(default_yaxis, args$yaxis)
  } else {
    args$yaxis <- default_yaxis
  }
  
  do.call(layout, c(list(
    p = p,
    paper_bgcolor = "transparent",
    plot_bgcolor  = "#fafaf7",
    font = list(color = "#2d2d2d", family = "Outfit")
  ), args))
}

# Palette
pal <- list(
  sage  = "#6b8f71", sage2 = "#8ab090",
  slate = "#5c7a99", slate2 = "#7d9ab5",
  sand  = "#c4a882", sand2  = "#d4c0a0",
  coral = "#c47a6c", coral2 = "#d9a090",
  plum  = "#8b6d8f",
  text  = "#2d2d2d", muted = "#6b6b6b"
)


# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Header
  div(class = "main-header",
    div(
      div(class = "main-title",
        if (nchar(country_name) > 0) paste0("\U0001f4ca ", country_name, " — Data Gap Analysis")
        else "\U0001f4ca Biodiversity Data Gap Analysis"),
      div(class = "main-subtitle", "Identify and prioritise biodiversity data gaps")
    ),
    div(class = "header-stats",
      div(style = "display:flex; align-items:center; gap:1.5rem;",
        if (!is.null(metadata)) tagList(
          span("Prepared: ", span(class = "header-stat-value",
            format(metadata$created_at, "%d %b %Y"))),
          span("Datasets: ", span(class = "header-stat-value", metadata$n_datasets))
        ),
        div(style = "display:flex; align-items:center; gap:0.4rem;",
          span(style = "font-size:0.8rem; color:#6b6b6b;", "Record type:"),
          selectInput("basis_filter", NULL,
            choices = basis_types,
            selected = "all",
            width = "180px"))
      )
    )
  ),

  # Main content
  div(style = "padding: 0 1rem;",
    tabsetPanel(
      id = "main_tabs", type = "pills",

      # =====================================================================
      # OVERVIEW TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("dashboard"), "Overview"),
        value = "overview",
        div(style = "padding: 1.25rem 0;",

          # Top-level stats
          div(class = "stat-grid",
            div(class = "stat-box",
              div(class = "stat-value sage", textOutput("ov_total_occ", inline = TRUE)),
              div(class = "stat-label", "Total Occurrences")),
            div(class = "stat-box",
              div(class = "stat-value slate", textOutput("ov_species", inline = TRUE)),
              div(class = "stat-label", "Species in GBIF")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("ov_year_range", inline = TRUE)),
              div(class = "stat-label", "Year Range")),
            div(class = "stat-box",
              div(class = "stat-value plum", textOutput("ov_last_update", inline = TRUE)),
              div(class = "stat-label", "Data Prepared"))
          ),

          # Four gap summary panels with visual indicators
          fluidRow(
            column(6,
              div(class = "card",
                div(class = "card-title", icon("map"), "Spatial Gaps"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$sage, ";"),
                    textOutput("ov_spatial_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_spatial_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_spatial_detail", inline = TRUE))
              )
            ),
            column(6,
              div(class = "card",
                div(class = "card-title", icon("clock"), "Temporal Gaps"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$slate, ";"),
                    textOutput("ov_temporal_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_temporal_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_temporal_detail", inline = TRUE))
              )
            )
          ),
          fluidRow(
            column(6,
              div(class = "card",
                div(class = "card-title", icon("leaf"), "Taxonomic Gaps"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$sand, ";"),
                    textOutput("ov_tax_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_tax_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_tax_detail", inline = TRUE))
              )
            ),
            column(6,
              div(class = "card",
                div(class = "card-title", icon("exclamation-triangle"), "Threatened Species"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$coral, ";"),
                    textOutput("ov_threat_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_threat_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_threat_detail", inline = TRUE))
              )
            )
          ),

          # Coverage overview charts
          fluidRow(
            column(7, div(class = "card",
              div(class = "card-title", icon("chart-bar"), "Coverage Overview"),
              plotlyOutput("overview_coverage", height = "260px"))),
            column(5, div(class = "card",
              div(class = "card-title", icon("calendar-alt"), "Temporal Span"),
              plotlyOutput("overview_temporal_span", height = "260px")))
          )
        )
      ),

      # =====================================================================
      # SPATIAL TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("map"), "Spatial"),
        value = "spatial",
        div(style = "padding: 1.25rem 0;",
          fluidRow(
            column(8, div(class = "card",
              div(class = "card-title", icon("globe-europe"), "Geographic Coverage"),
              leafletOutput("spatial_map", height = "520px"))),
            column(4,
              div(class = "card",
                div(class = "card-title", icon("sliders-h"), "Display"),
                radioButtons("map_var", NULL,
                  choices = c("Occurrences (log)" = "occ",
                              "Staleness (months)" = "stale",
                              "Species richness" = "richness"),
                  selected = "occ")),
              div(class = "card",
                div(class = "card-title", icon("info-circle"), "Statistics"),
                tableOutput("spatial_stats")))
          ),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("th"), "Grid Comparison"),
              plotlyOutput("spatial_grid", height = "260px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-area"), "Occurrence Distribution"),
              plotlyOutput("spatial_hist", height = "260px")))
          )
        )
      ),

      # =====================================================================
      # TEMPORAL TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("clock"), "Temporal"),
        value = "temporal",
        div(style = "padding: 1.25rem 0;",
          div(class = "filter-section",
            fluidRow(
              column(6,
                div(class = "filter-label", "Year Range"),
                sliderInput("year_range", NULL, min = 1900, max = current_year,
                  value = c(1970, current_year), step = 1, sep = "")),
              column(6,
                div(class = "filter-label", "Orders to Display"),
                pickerInput("order_select", NULL, choices = order_choices,
                  selected = head(order_choices, 5), multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)))
            )),
          fluidRow(
            column(8, div(class = "card",
              div(class = "card-title", icon("chart-line"), "Historical Trend"),
              plotlyOutput("temporal_trend", height = "300px"))),
            column(4, div(class = "card",
              div(class = "card-title", icon("calendar-alt"), "Seasonal Pattern"),
              plotlyOutput("temporal_season", height = "300px")))
          ),
          fluidRow(
            column(12, div(class = "card",
              div(class = "card-title", icon("th"), "Year \u00d7 Month Heatmap"),
              plotlyOutput("temporal_heatmap", height = "350px")))
          )
        )
      ),

      # =====================================================================
      # BASIS OF RECORD TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("layer-group"), "Basis of Record"),
        value = "basis_tab",
        div(style = "padding: 1.25rem 0;",
          div(class = "stat-grid", uiOutput("basis_stat_boxes")),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-pie"), "Occurrences by Basis of Record"),
              plotlyOutput("basis_pie", height = "340px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-line"), "Temporal Trend by Basis"),
              plotlyOutput("basis_timeline", height = "340px")))
          ),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("map"), "Spatial Coverage by Basis"),
              plotlyOutput("basis_spatial_bar", height = "340px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("dna"), "Species Coverage by Basis"),
              plotlyOutput("basis_species_bar", height = "340px")))
          ),
          div(class = "card",
            div(class = "card-title", icon("map"), "Spatial Distribution per Basis of Record"),
            selectInput("basis_map_select", NULL,
              choices = basis_types_no_all, width = "250px"),
            leafletOutput("basis_map", height = "450px"))
        )
      ),

      # =====================================================================
      # TAXONOMIC TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("leaf"), "Taxonomic"),
        value = "taxonomic",
        div(style = "padding: 1.25rem 0;",
          div(class = "filter-section",
            fluidRow(
              column(4,
                div(class = "filter-label", "Number of Groups"),
                sliderInput("tax_n_groups", NULL, min = 5, max = 30,
                  value = 20, step = 5)),
              column(4,
                div(class = "filter-label", "Minimum Species in Backbone"),
                sliderInput("tax_min_taxa", NULL, min = 1, max = 50,
                  value = 10, step = 5)),
              column(4,
                div(class = "filter-label", "Sort By"),
                radioButtons("tax_sort", NULL, inline = TRUE,
                  choices = c("Total species" = "n_taxa",
                              "Coverage %" = "pct_coverage"),
                  selected = "n_taxa"))
            )),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("layer-group"), "Coverage by Order"),
              plotlyOutput("tax_order", height = "450px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("folder-tree"), "Coverage by Family"),
              plotlyOutput("tax_family", height = "450px")))
          ),
          fluidRow(
            column(12, div(class = "card",
              div(class = "card-title", icon("exchange-alt"), "Recent vs Historical Sampling Intensity"),
              div(class = "info-note",
                strong("Baseline: "), "Historical = all records before 2000. ",
                "Recent = records from 2000 onwards. ",
                "Bars show the change in total occurrences between the two periods for the top orders."),
              plotlyOutput("tax_change", height = "320px")))
          )
        )
      ),

      # =====================================================================
      # THREATENED TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("exclamation-triangle"), "Threatened"),
        value = "threatened",
        div(style = "padding: 1.25rem 0;",
          div(class = "stat-grid",
            div(class = "stat-box",
              div(class = "stat-value coral", textOutput("stat_cr", inline = TRUE)),
              div(class = "stat-label", "CR Missing")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("stat_en", inline = TRUE)),
              div(class = "stat-label", "EN Missing")),
            div(class = "stat-box",
              div(class = "stat-value", style = "color:#b8a060;", textOutput("stat_vu", inline = TRUE)),
              div(class = "stat-label", "VU Missing")),
            div(class = "stat-box",
              div(class = "stat-value sage", textOutput("stat_nt", inline = TRUE)),
              div(class = "stat-label", "NT Missing"))
          ),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("shield-alt"), "Coverage by Threat Status"),
              plotlyOutput("threat_coverage", height = "300px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("times-circle"), "Missing Taxa by Status"),
              plotlyOutput("threat_missing", height = "300px")))
          ),
          div(class = "card",
            div(class = "card-title", icon("list"), "Missing CR & EN Species"),
            div(class = "info-note",
              "Species listed in the national taxonomy backbone with CR or EN threat status ",
              "that have no matching GBIF occurrence records. ",
              "Use the column filters below to narrow by order or family."),
            DTOutput("threat_table"))
        )
      ),

      # =====================================================================
      # PRIORITIES TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("bullseye"), "Priorities"),
        value = "priorities",
        div(style = "padding: 1.25rem 0;",
          div(class = "stat-grid", style = "grid-template-columns: repeat(3, 1fr);",
            div(class = "stat-box",
              div(class = "stat-value coral", textOutput("stat_zero", inline = TRUE)),
              div(class = "stat-label", "Zero Coverage Cells")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("stat_stale", inline = TRUE)),
              div(class = "stat-label", "Stale Cells (>5 yrs)")),
            div(class = "stat-box",
              div(class = "stat-value plum", textOutput("stat_taxa", inline = TRUE)),
              div(class = "stat-label", "Priority Taxa (CR/EN)"))
          ),
          div(class = "card",
            div(class = "card-title", icon("tasks"), "Recommended Actions"),
            uiOutput("action_table")),
          div(class = "card",
            div(style = "display:flex; align-items:center; justify-content:space-between;",
              div(
                div(class = "card-title", icon("download"), "Export Action Plan"),
                div(style = "font-size:0.85rem; color:#6b6b6b;",
                  "Download all priority items as a single CSV: zero-coverage cells, stale cells, and missing threatened species.")),
              div(style = "padding-left:1rem;",
                downloadButton("download_action_plan", "Download CSV",
                  class = "btn-download", style = "white-space:nowrap;"))
            )),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("map-marker-alt"), "Zero Coverage Cells"),
              leafletOutput("zero_map", height = "400px"),
              div(style = "margin-top:0.75rem;"),
              DTOutput("zero_table"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("hourglass-half"), "Stale Cells"),
              leafletOutput("stale_map", height = "400px"),
              div(style = "margin-top:0.75rem;"),
              DTOutput("stale_table")))
          )
        )
      ),

      # =====================================================================
      # EXPLORER TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("database"), "Explorer"),
        value = "explorer",
        div(style = "padding: 1.25rem 0;",
          div(class = "card",
            div(class = "card-title", icon("search"), "Data Explorer"),
            fluidRow(
              column(6,
                selectInput("explorer_ds", "Select Dataset:",
                  choices = c(
                    "Spatial Gaps (10km)"  = "spatial_gaps_10km",
                    "Cell Recency (10km)"  = "cell_recency_10km",
                    "Coverage by Order"    = "tax_by_order",
                    "Coverage by Family"   = "tax_by_family",
                    "Coverage by Rank"     = "tax_by_rank",
                    "Order Summary"        = "order_summary",
                    "Priority Zero Cells"  = "priority_zero_cells",
                    "Priority Stale Cells" = "priority_stale_cells",
                    "Dashboard Summary"    = "dashboard_long"))),
              column(6, div(style = "padding-top: 25px;",
                downloadButton("explorer_download", "Download CSV",
                  class = "btn-download")))
            ),
            DTOutput("explorer_table"))
        )
      )
    ),

    # Footer
    div(class = "metadata-footer",
      HTML(paste0(
        "Data prepared: ",
        if (!is.null(metadata)) format(metadata$created_at, "%Y-%m-%d %H:%M") else "Unknown",
        " \u00b7 Datasets: ", if (!is.null(metadata)) metadata$n_datasets else "?",
        " \u00b7 gbifgaps"
      ))
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---- Reactive filtered data ----
  basis_selected <- reactive({
    b <- input$basis_filter
    if (is.null(b) || b == "") "all" else b
  })

  time_filtered <- reactive({
    req(time_summary)
    time_summary |> filter(
      basisofrecord == basis_selected(),
      year >= input$year_range[1],
      year <= input$year_range[2])
  })

  order_filtered <- reactive({
    req(order_5yr, input$order_select)
    order_5yr |> filter(order %in% input$order_select,
                        period_start >= input$year_range[1])
  })

  # ===================================================================
  # OVERVIEW
  # ===================================================================

  output$ov_total_occ <- renderText({
    if (!is.null(dashboard)) comma(dashboard$total_occurrences[1]) else "?"
  })
  output$ov_species <- renderText({
    if (!is.null(dashboard)) comma(dashboard$taxa_in_gbif[1]) else "?"
  })
  output$ov_year_range <- renderText({
    if (!is.null(dashboard))
      paste0(dashboard$year_min[1], "\u2013", dashboard$year_max[1])
    else "?"
  })
  output$ov_last_update <- renderText({
    if (!is.null(dashboard)) as.character(dashboard$analysis_date[1]) else "?"
  })

  # Spatial gap panel
  output$ov_spatial_pct <- renderText({
    if (!is.null(dashboard)) paste0(dashboard$cells_10km_pct_coverage[1], "% coverage")
    else "?"
  })
  output$ov_spatial_detail <- renderText({
    if (!is.null(dashboard))
      paste0(comma(dashboard$cells_10km_with_data[1]), " of ",
             comma(dashboard$cells_10km_total[1]),
             " 10km grid cells have occurrence data. ",
             comma(dashboard$cells_10km_zero[1]), " cells have zero records.")
    else ""
  })

  # Temporal gap panel
  output$ov_temporal_pct <- renderText({
    if (!is.null(dashboard)) paste0(dashboard$pct_stale_5y_10km[1], "% stale (>5yr)")
    else "?"
  })
  output$ov_temporal_detail <- renderText({
    if (!is.null(dashboard))
      paste0("Data spans ", dashboard$year_span[1], " years (",
             dashboard$year_min[1], "\u2013", dashboard$year_max[1], "). ",
             "Median cell staleness: ", dashboard$median_staleness_months_10km[1], " months. ",
             round(dashboard$pct_stale_1y_10km[1], 1), "% of cells not sampled in the past year.")
    else ""
  })

  # Taxonomic gap panel
  output$ov_tax_pct <- renderText({
    if (!is.null(dashboard)) paste0(dashboard$taxa_pct_coverage[1], "% coverage")
    else "?"
  })
  output$ov_tax_detail <- renderText({
    if (!is.null(dashboard))
      paste0(comma(dashboard$taxa_in_gbif[1]), " of ",
             comma(dashboard$taxa_in_reference[1]),
             " backbone species found in GBIF. ",
             comma(dashboard$taxa_missing[1]), " species have no occurrence records.")
    else ""
  })

  # Threatened panel
  output$ov_threat_pct <- renderText({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat) && nrow(tax_by_threat) > 0) {
      n_miss <- sum(tax_by_threat$n_missing[tax_by_threat$threatStatus %in% c("CR", "EN", "VU", "NT")], na.rm = TRUE)
      paste0(comma(n_miss), " missing")
    } else "No threat data"
  })
  output$ov_threat_detail <- renderText({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat) && nrow(tax_by_threat) > 0) {
      df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT"))
      paste0(comma(sum(df$n_ref_total)), " threatened species in backbone, ",
             comma(sum(df$n_in_gbif)), " found in GBIF (",
             round(100 * sum(df$n_in_gbif) / sum(df$n_ref_total), 1), "% coverage).")
    } else "Threat status data not available in the current backbone. Enable a red list in config.yml to see threatened species analysis."
  })

  # Overview coverage chart
  output$overview_coverage <- renderPlotly({
    req(dashboard)
    df <- tibble(
      dim = c("Spatial (10km)", "Spatial (50km)", "Taxonomic"),
      cov = c(as.numeric(dashboard$cells_10km_pct_coverage[1]),
              as.numeric(dashboard$cells_50km_pct_coverage[1]),
              as.numeric(dashboard$taxa_pct_coverage[1]))
    ) |> filter(!is.na(cov)) |>
      mutate(gap = 100 - cov) |>
      pivot_longer(c(cov, gap), names_to = "type", values_to = "pct") |>
      mutate(type = factor(type, c("gap", "cov"), c("Gap", "Covered")))

    plot_ly(df, x = ~pct, y = ~dim, color = ~type, type = "bar", orientation = "h",
      colors = c("Gap" = pal$coral, "Covered" = pal$sage),
      hovertemplate = "%{x:.1f}%<extra>%{fullData.name}</extra>") |>
      plotly_layout(
        barmode = "stack",
        xaxis = list(title = "", ticksuffix = "%"),
        yaxis = list(title = ""),
        legend = list(orientation = "h", y = -0.2))
  })

  # Progress bar helper
  make_progress_bar <- function(pct, color, bg_color) {
    pct <- min(max(pct, 0), 100)
    tags$div(
      style = paste0("background:", bg_color, "; border-radius:6px; height:10px; width:100%; overflow:hidden;"),
      tags$div(style = paste0(
        "background:", color, "; height:100%; width:", pct, "%; border-radius:6px; transition:width 0.5s ease;"
      ))
    )
  }

  # Spatial progress bar
  output$ov_spatial_bar <- renderUI({
    pct <- if (!is.null(dashboard)) as.numeric(dashboard$cells_10km_pct_coverage[1]) else 0
    make_progress_bar(pct, pal$sage, "#e8ede9")
  })

  # Temporal progress bar (inverse: % not stale = freshness)
  output$ov_temporal_bar <- renderUI({
    pct <- if (!is.null(dashboard)) 100 - as.numeric(dashboard$pct_stale_5y_10km[1]) else 0
    make_progress_bar(pct, pal$slate, "#e2e8ee")
  })

  # Taxonomic progress bar
  output$ov_tax_bar <- renderUI({
    pct <- if (!is.null(dashboard)) as.numeric(dashboard$taxa_pct_coverage[1]) else 0
    make_progress_bar(pct, pal$sand, "#ede8df")
  })

  # Threatened progress bar
  output$ov_threat_bar <- renderUI({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat) && nrow(tax_by_threat) > 0) {
      df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT"))
      if (sum(df$n_ref_total) > 0) {
        pct <- round(100 * sum(df$n_in_gbif) / sum(df$n_ref_total), 1)
      } else { pct <- 0 }
      make_progress_bar(pct, pal$coral, "#ede3e0")
    } else {
      tags$div(
        style = "background:#eee; border-radius:6px; height:10px; width:100%;",
        tags$div(style = "background:#ccc; height:100%; width:0%; border-radius:6px;")
      )
    }
  })

  # Temporal span chart — decade-level occurrence volume
  output$overview_temporal_span <- renderPlotly({
    req(time_summary)
    df <- time_summary |>
      filter(basisofrecord == "all", year >= 1900) |>
      mutate(decade = floor(year / 10) * 10) |>
      group_by(decade) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      mutate(decade_label = paste0(decade, "s"))

    plot_ly(df, x = ~decade, y = ~occ, type = "bar",
      marker = list(color = pal$slate,
                    line = list(color = pal$slate2, width = 0.5)),
      hovertemplate = "%{x}s: %{y:,.0f} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "", dtick = 20),
        yaxis = list(title = "Occurrences per decade"))
  })

  # ===================================================================
  # SPATIAL
  # ===================================================================

  # Base map (rendered once)
  output$spatial_map <- renderLeaflet({
    req(grid_10km)
    leaflet(grid_10km) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 16, lat = 63, zoom = 5)
  })

  # Reactive map update when map_var or basis changes
  observe({
    req(grid_10km, spatial_gaps, input$map_var)

    sf_base <- spatial_gaps |> filter(basisofrecord == basis_selected())

    if (input$map_var == "stale" && !is.null(cell_recency)) {
      rec <- cell_recency |> filter(basisofrecord == basis_selected()) |>
        select(eeacellcode, staleness_months)
      map_sf <- grid_10km |> left_join(rec, by = "eeacellcode")
      vals   <- map_sf$staleness_months
      pal_fn <- colorNumeric(c(pal$sage, pal$sand, pal$coral), domain = vals, na.color = "#ddd")
      legend_title <- "Months since last obs"
      popup_fn <- ~paste0("Cell: ", eeacellcode, "<br>Staleness: ",
                          ifelse(is.na(staleness_months), "No data", paste(staleness_months, "months")))

    } else if (input$map_var == "richness") {
      map_sf <- grid_10km |> left_join(
        sf_base |> select(eeacellcode, n_species), by = "eeacellcode")
      vals   <- log10(pmax(map_sf$n_species, 1, na.rm = TRUE))
      pal_fn <- colorNumeric(c("#f6f5f1", pal$slate, "#2c4a6b"), domain = vals, na.color = "#ddd")
      legend_title <- "log10(species)"
      popup_fn <- ~paste0("Cell: ", eeacellcode, "<br>Species: ", comma(n_species))

    } else {
      map_sf <- grid_10km |> left_join(
        sf_base |> select(eeacellcode, occurrences, log_occ), by = "eeacellcode") |>
        mutate(log_occ = ifelse(is.na(log_occ), 0, log_occ))
      vals   <- map_sf$log_occ
      pal_fn <- colorNumeric(c("#f6f5f1", pal$sage, "#2c5e3a"), domain = vals, na.color = "#ddd")
      legend_title <- "log10(occurrences)"
      popup_fn <- ~paste0("Cell: ", eeacellcode, "<br>Occ: ", comma(occurrences))
    }

    leafletProxy("spatial_map", data = map_sf) |>
      clearShapes() |> clearControls() |>
      addPolygons(
        fillColor   = ~pal_fn(vals),
        fillOpacity = 0.7,
        weight      = 0.3,
        color       = "#999",
        popup       = popup_fn) |>
      addLegend("bottomright", pal = pal_fn, values = vals, title = legend_title)
  })

  output$spatial_stats <- renderTable({
    req(spatial_gaps)
    sf <- spatial_gaps |> filter(basisofrecord == basis_selected())
    tibble(
      Metric = c("Total 10km Cells", "Cells with Data", "Coverage",
                  "Total Occurrences", "Median per Cell"),
      Value = c(comma(nrow(sf)),
                comma(sum(sf$has_data, na.rm = TRUE)),
                paste0(round(100 * mean(sf$has_data, na.rm = TRUE), 1), "%"),
                comma(sum(sf$occurrences, na.rm = TRUE)),
                comma(median(sf$occurrences[sf$occurrences > 0], na.rm = TRUE)))
    )
  }, width = "100%")

  output$spatial_grid <- renderPlotly({
    req(comparison_grids)
    df <- comparison_grids |>
      mutate(empty = n_cells_total - n_cells_with_data)

    plot_ly(df, x = ~grid, y = ~n_cells_with_data, type = "bar",
      name = "With Data", marker = list(color = pal$sage)) |>
      add_trace(y = ~empty, name = "Empty", marker = list(color = pal$coral)) |>
      plotly_layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "Grid cells"))
  })

  output$spatial_hist <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps |> filter(basisofrecord == basis_selected(), occurrences > 0)
    plot_ly(df, x = ~log_occ, type = "histogram",
      marker = list(color = pal$sage, line = list(color = pal$sage2, width = 0.5))) |>
      plotly_layout(
        xaxis = list(title = "log10(Occurrences + 1)"),
        yaxis = list(title = "Number of cells"))
  })

  # ===================================================================
  # TEMPORAL
  # ===================================================================

  output$temporal_trend <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(year) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = paste0(pal$sage, "33"),
      line = list(color = pal$sage, width = 2)) |>
      plotly_layout(
        xaxis = list(title = "", range = c(input$year_range[1], input$year_range[2])),
        yaxis = list(title = "Occurrences"))
  })

  output$temporal_season <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(month) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")

    month_cols <- colorRampPalette(c(pal$slate, pal$sage, pal$sand, pal$coral))(12)

    plot_ly(df, x = ~month, y = ~occ, type = "bar",
      marker = list(color = month_cols)) |>
      plotly_layout(
        xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12),
        yaxis = list(title = ""))
  })

  output$temporal_heatmap <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(year, month) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      mutate(log_occ = log10(occ + 1))

    # Warm sage-to-sand colorscale
    heatmap_cs <- list(
      list(0, "#f6f5f1"), list(0.25, "#c8dbc6"),
      list(0.5, pal$sage), list(0.75, pal$sand),
      list(1, pal$coral))

    plot_ly(df, x = ~month, y = ~year, z = ~log_occ, type = "heatmap",
      colorscale = heatmap_cs, hovertemplate = "Year: %{y}<br>Month: %{x}<br>log10(occ): %{z:.1f}<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12),
        yaxis = list(title = ""))
  })

  # ===================================================================
  # BASIS OF RECORD
  # ===================================================================

  # Basis palette
  basis_colors <- c(
    "HUMAN_OBSERVATION"     = "#6b8f71",
    "PRESERVED_SPECIMEN"    = "#5c7a99",
    "MACHINE_OBSERVATION"   = "#c4a882",
    "OBSERVATION"           = "#8b6d8f",
    "MATERIAL_SAMPLE"       = "#c47a6c",
    "OCCURRENCE"            = "#7daa90",
    "LITERATURE"            = "#a89060",
    "LIVING_SPECIMEN"       = "#8fa4b8",
    "FOSSIL_SPECIMEN"       = "#b8967a",
    "humanObservation"      = "#6b8f71",
    "preservedSpecimen"     = "#5c7a99",
    "machineObservation"    = "#c4a882",
    "observation"           = "#8b6d8f",
    "materialSample"        = "#c47a6c",
    "occurrence"            = "#7daa90",
    "literature"            = "#a89060",
    "livingSpecimen"        = "#8fa4b8",
    "fossilSpecimen"        = "#b8967a"
  )
  get_basis_color <- function(b) ifelse(b %in% names(basis_colors), basis_colors[b], "#999999")

  # Stat boxes
  output$basis_stat_boxes <- renderUI({
    req(spatial_gaps)
    sg <- spatial_gaps |> filter(basisofrecord != "all")
    basis_summary <- sg |>
      group_by(basisofrecord) |>
      summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE),
                n_cells = n(),
                n_species = sum(n_species, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_occ)) |>
      slice_head(n = 4)

    color_classes <- c("sage", "slate", "sand", "coral")

    tagList(lapply(seq_len(nrow(basis_summary)), function(i) {
      b <- basis_summary[i, ]
      div(class = "stat-box",
        div(class = paste("stat-value", color_classes[i]), comma(b$total_occ)),
        div(class = "stat-label", str_replace_all(b$basisofrecord, "_", " "))
      )
    }))
  })

  # Pie chart
  output$basis_pie <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps |>
      filter(basisofrecord != "all") |>
      group_by(basisofrecord) |>
      summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_occ))

    cols <- sapply(df$basisofrecord, get_basis_color)
    labels <- str_replace_all(df$basisofrecord, "_", " ")

    plot_ly(df, labels = ~labels, values = ~total_occ, type = "pie",
      marker = list(colors = cols, line = list(color = "#fff", width = 1.5)),
      textinfo = "label+percent", textposition = "auto",
      textfont = list(size = 11),
      hovertemplate = "%{label}<br>%{value:,.0f} occurrences<br>%{percent}<extra></extra>") |>
      plotly_layout(showlegend = FALSE)
  })

  # Timeline by basis
  output$basis_timeline <- renderPlotly({
    req(time_summary)
    df <- time_summary |>
      filter(basisofrecord != "all", !is.na(year), year >= 1970) |>
      group_by(basisofrecord, year) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")

    basis_order <- df |> group_by(basisofrecord) |>
      summarise(tot = sum(occ)) |> arrange(desc(tot)) |> pull(basisofrecord)

    p <- plot_ly()
    for (b in basis_order) {
      bd <- df |> filter(basisofrecord == b)
      p <- p |> add_trace(data = bd, x = ~year, y = ~occ,
        type = "scatter", mode = "lines",
        name = str_replace_all(b, "_", " "),
        line = list(color = get_basis_color(b), width = 2),
        stackgroup = "one",
        hovertemplate = paste0(str_replace_all(b, "_", " "),
          "<br>%{x}: %{y:,.0f}<extra></extra>"))
    }
    p |> plotly_layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Occurrences"),
      legend = list(orientation = "h", y = -0.15))
  })

  # Spatial coverage bar
  output$basis_spatial_bar <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps |>
      filter(basisofrecord != "all") |>
      group_by(basisofrecord) |>
      summarise(
        cells_with_data = sum(occurrences > 0, na.rm = TRUE),
        .groups = "drop") |>
      arrange(desc(cells_with_data))

    total_cells <- if (!is.null(grid_10km)) nrow(grid_10km) else max(df$cells_with_data)
    df$pct <- round(100 * df$cells_with_data / total_cells, 1)
    labels <- str_replace_all(df$basisofrecord, "_", " ")
    cols <- sapply(df$basisofrecord, get_basis_color)

    plot_ly(df, y = ~reorder(labels, cells_with_data), x = ~pct,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      text = ~paste0(pct, "% (", comma(cells_with_data), " cells)"),
      textposition = "auto", textfont = list(size = 10, color = "#fff"),
      hovertemplate = "%{y}<br>%{x:.1f}% of cells<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "% of grid cells covered", range = c(0, 105)),
        yaxis = list(title = ""))
  })

  # Species coverage bar
  output$basis_species_bar <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps |>
      filter(basisofrecord != "all") |>
      group_by(basisofrecord) |>
      summarise(
        total_species = sum(n_species, na.rm = TRUE),
        .groups = "drop") |>
      arrange(desc(total_species))

    labels <- str_replace_all(df$basisofrecord, "_", " ")
    cols <- sapply(df$basisofrecord, get_basis_color)

    plot_ly(df, y = ~reorder(labels, total_species), x = ~total_species,
      type = "bar", orientation = "h",
      marker = list(color = cols),
      text = ~comma(total_species),
      textposition = "auto", textfont = list(size = 10, color = "#fff"),
      hovertemplate = "%{y}<br>%{x:,.0f} species detections<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Species detections (sum across cells)"),
        yaxis = list(title = ""))
  })

  # Spatial map for selected basis
  output$basis_map <- renderLeaflet({
    req(grid_10km, spatial_gaps, input$basis_map_select)
    sel <- input$basis_map_select

    sg <- spatial_gaps |> filter(basisofrecord == sel)
    map_sf <- grid_10km |> left_join(sg |> select(eeacellcode, occurrences, n_species), by = "eeacellcode")

    vals <- log10(pmax(map_sf$occurrences, 1, na.rm = TRUE))
    base_col <- get_basis_color(sel)
    pal_fn <- colorNumeric(c("#f6f5f1", base_col), domain = vals, na.color = "#eee")

    leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal_fn(vals), fillOpacity = 0.65,
        weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Occurrences:</strong> ", comma(occurrences),
                        "<br><strong>Species:</strong> ", comma(n_species))) |>
      addLegend("bottomright", pal = pal_fn, values = vals,
        title = paste0("log\u2081\u2080(Occ) \u2014 ", str_replace_all(sel, "_", " ")))
  })

  # ===================================================================
  # TAXONOMIC
  # ===================================================================

  output$tax_order <- renderPlotly({
    req(tax_by_order)
    n <- input$tax_n_groups %||% 20
    min_t <- input$tax_min_taxa %||% 10
    sort_col <- input$tax_sort %||% "n_taxa"

    df <- tax_by_order |>
      filter(!is.na(order), n_taxa >= min_t) |>
      arrange(desc(.data[[sort_col]])) |>
      slice_head(n = n) |>
      mutate(
        miss = n_taxa - n_in_gbif,
        label = paste0(round(pct_coverage, 1), "%"))

    plot_ly(df, y = ~reorder(order, n_taxa), x = ~n_in_gbif, type = "bar",
      name = "In GBIF", marker = list(color = pal$sage),
      text = ~label, textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      orientation = "h") |>
      add_trace(x = ~miss, name = "Missing from GBIF",
        marker = list(color = pal$sand2),
        text = "", textposition = "none") |>
      plotly_layout(
        barmode = "stack",
        xaxis = list(title = "Species count"),
        yaxis = list(title = "Order", categoryorder = "total ascending"),
        legend = list(orientation = "h", y = -0.15))
  })

  output$tax_family <- renderPlotly({
    req(tax_by_family)
    n <- input$tax_n_groups %||% 20
    min_t <- input$tax_min_taxa %||% 10
    sort_col <- input$tax_sort %||% "n_taxa"

    df <- tax_by_family |>
      filter(!is.na(family), n_taxa >= min_t) |>
      arrange(desc(.data[[sort_col]])) |>
      slice_head(n = n) |>
      mutate(label = paste0(round(pct_coverage, 1), "%"))

    plot_ly(df, y = ~reorder(family, pct_coverage), x = ~pct_coverage, type = "bar",
      marker = list(color = pal$slate),
      text = ~label, textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      orientation = "h") |>
      plotly_layout(
        xaxis = list(title = "Coverage (%)", range = c(0, 105)),
        yaxis = list(title = "Family", categoryorder = "total ascending"))
  })

  output$tax_change <- renderPlotly({
    # Use order_change if available (has pct_change between historical and recent periods)
    if (!is.null(order_change) && nrow(order_change) > 0) {
      df <- order_change |>
        arrange(desc(abs(pct_change))) |>
        slice_head(n = 15) |>
        mutate(bar_col = ifelse(pct_change >= 0, pal$sage, pal$coral))

      plot_ly(df, y = ~reorder(order, pct_change), x = ~pct_change, type = "bar",
        marker = list(color = ~bar_col), orientation = "h",
        hovertemplate = "%{y}: %{x:+.1f}%<extra></extra>") |>
        plotly_layout(
          xaxis = list(title = "Change (%)",
                       zeroline = TRUE, zerolinecolor = "#ccc", zerolinewidth = 1),
          yaxis = list(title = "Order"))

    } else if (!is.null(order_5yr)) {
      # Fallback: derive from order_5yr
      df <- order_5yr |>
        mutate(era = ifelse(period_start >= 2000, "Recent", "Historical")) |>
        group_by(order, era) |>
        summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
        pivot_wider(names_from = era, values_from = occ, values_fill = 0) |>
        filter(Historical > 0) |>
        mutate(
          pct_change = round(100 * (Recent - Historical) / Historical, 1),
          bar_col = ifelse(pct_change >= 0, pal$sage, pal$coral)) |>
        arrange(desc(abs(pct_change))) |>
        slice_head(n = 15)

      plot_ly(df, y = ~reorder(order, pct_change), x = ~pct_change, type = "bar",
        marker = list(color = ~bar_col), orientation = "h",
        hovertemplate = "%{y}: %{x:+.1f}%<extra></extra>") |>
        plotly_layout(
          xaxis = list(title = "Change (%)",
                       zeroline = TRUE, zerolinecolor = "#ccc", zerolinewidth = 1),
          yaxis = list(title = "Order"))

    } else {
      plotly_empty() |> plotly_layout()
    }
  })

  # ===================================================================
  # THREATENED
  # ===================================================================

  output$stat_cr <- renderText({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat))
      comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "CR"], na.rm = TRUE))
    else "0"
  })
  output$stat_en <- renderText({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat))
      comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "EN"], na.rm = TRUE))
    else "0"
  })
  output$stat_vu <- renderText({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat))
      comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "VU"], na.rm = TRUE))
    else "0"
  })
  output$stat_nt <- renderText({
    if (!is.null(tax_by_threat) && "threatStatus" %in% names(tax_by_threat))
      comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "NT"], na.rm = TRUE))
    else "0"
  })

  output$threat_coverage <- renderPlotly({
    req(tax_by_threat, nrow(tax_by_threat) > 0, "threatStatus" %in% names(tax_by_threat))
    df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT", "LC"))
    if (nrow(df) == 0) return(plotly_empty())

    threat_cols <- c(CR = pal$coral, EN = pal$sand, VU = "#b8a060", NT = pal$sage, LC = pal$sage2)

    plot_ly(df, x = ~threatStatus, y = ~pct_coverage, type = "bar",
      marker = list(color = threat_cols[df$threatStatus]),
      text = ~paste0(round(pct_coverage, 1), "%"), textposition = "auto") |>
      plotly_layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Coverage %", range = c(0, 110)))
  })

  output$threat_missing <- renderPlotly({
    req(tax_by_threat, nrow(tax_by_threat) > 0, "threatStatus" %in% names(tax_by_threat))
    df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT"))
    if (nrow(df) == 0) return(plotly_empty())

    threat_cols <- c(CR = pal$coral, EN = pal$sand, VU = "#b8a060", NT = pal$sage)

    plot_ly(df, x = ~threatStatus, y = ~n_missing, type = "bar",
      marker = list(color = threat_cols[df$threatStatus]),
      text = ~comma(n_missing), textposition = "auto") |>
      plotly_layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Missing taxa"))
  })

  output$threat_table <- renderDT({
    # Use priority_taxa_missing (individual missing threatened species)
    priority_taxa <- safe_get("priority_taxa_missing")
    if (!is.null(priority_taxa) && nrow(priority_taxa) > 0) {
      df <- priority_taxa |>
        filter(threatStatus %in% c("CR", "EN")) |>
        select(any_of(c("scientificName", "threatStatus", "kingdom", "phylum",
                         "class", "order", "family"))) |>
        arrange(factor(threatStatus, levels = c("CR", "EN")), order, family)
    } else {
      df <- tibble(Message = "No missing CR/EN species found.")
    }

    # Force categorical columns to use dropdown filters
    col_names <- names(df)
    filter_types <- lapply(col_names, function(cn) {
      if (cn %in% c("threatStatus", "kingdom", "phylum", "class", "order", "family")) "factor"
      else "character"
    })
    # Convert dropdown columns to factor so DT renders them as selects
    for (cn in c("threatStatus", "kingdom", "phylum", "class", "order", "family")) {
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    }

    datatable(df, options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap4", filter = "top")
  })

  # ===================================================================
  # PRIORITIES
  # ===================================================================

  output$stat_zero <- renderText({
    if (!is.null(priority_zero) && nrow(priority_zero) > 0) {
      comma(nrow(priority_zero))
    } else if (!is.null(dashboard)) {
      comma(dashboard$n_zero_coverage_cells[1])
    } else "0"
  })
  output$stat_stale <- renderText({
    if (!is.null(priority_stale)) comma(nrow(priority_stale)) else "0"
  })
  output$stat_taxa <- renderText({
    if (!is.null(dashboard)) comma(dashboard$n_priority_taxa[1]) else "0"
  })

  output$action_table <- renderUI({
    n_zero  <- if (!is.null(priority_zero) && nrow(priority_zero) > 0) nrow(priority_zero) else {
      # Fallback: derive from dashboard
      if (!is.null(dashboard)) dashboard$n_zero_coverage_cells[1] else 0
    }
    n_stale <- if (!is.null(priority_stale)) nrow(priority_stale) else 0
    n_taxa  <- if (!is.null(dashboard)) dashboard$n_priority_taxa[1] else 0

    HTML(paste0('
      <table style="width:100%; border-collapse:separate; border-spacing:0 0.6rem;">
        <tr>
          <td style="width:100px; vertical-align:middle;">
            <span class="priority-badge priority-high" style="font-size:0.85rem; padding:0.4rem 1rem;">HIGH</span></td>
          <td style="vertical-align:middle; font-size:1.15rem; padding:0.75rem 0.5rem;">
            Survey zero-coverage grid cells</td>
          <td style="text-align:right; vertical-align:middle; padding:0.75rem 0.5rem;
                     color:', pal$sage, '; font-family:IBM Plex Mono,monospace; font-size:1.3rem; font-weight:500;">
            ', comma(n_zero), ' cells</td>
        </tr>
        <tr>
          <td style="vertical-align:middle;">
            <span class="priority-badge priority-high" style="font-size:0.85rem; padding:0.4rem 1rem;">HIGH</span></td>
          <td style="vertical-align:middle; font-size:1.15rem; padding:0.75rem 0.5rem;">
            Monitor CR and EN species lacking GBIF records</td>
          <td style="text-align:right; vertical-align:middle; padding:0.75rem 0.5rem;
                     color:', pal$sage, '; font-family:IBM Plex Mono,monospace; font-size:1.3rem; font-weight:500;">
            ', comma(n_taxa), ' species</td>
        </tr>
        <tr>
          <td style="vertical-align:middle;">
            <span class="priority-badge priority-medium" style="font-size:0.85rem; padding:0.4rem 1rem;">MED</span></td>
          <td style="vertical-align:middle; font-size:1.15rem; padding:0.75rem 0.5rem;">
            Re-survey cells with &gt;5 years since last observation</td>
          <td style="text-align:right; vertical-align:middle; padding:0.75rem 0.5rem;
                     color:', pal$sage, '; font-family:IBM Plex Mono,monospace; font-size:1.3rem; font-weight:500;">
            ', comma(n_stale), ' cells</td>
        </tr>
        <tr>
          <td style="vertical-align:middle;">
            <span class="priority-badge priority-low" style="font-size:0.85rem; padding:0.4rem 1rem;">LOW</span></td>
          <td style="vertical-align:middle; font-size:1.15rem; padding:0.75rem 0.5rem;">
            Expand citizen science programs to low-coverage regions</td>
          <td style="text-align:right; vertical-align:middle; padding:0.75rem 0.5rem;
                     color:', pal$muted, '; font-size:1.1rem;">
            Ongoing</td>
        </tr>
      </table>'))
  })

  # Zero coverage map
  output$zero_map <- renderLeaflet({
    req(grid_10km)

    if (is.null(priority_zero) || nrow(priority_zero) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = 16, lat = 63, zoom = 5)
      )
    }

    zero_codes <- priority_zero$eeacellcode
    zero_sf <- grid_10km |> filter(eeacellcode %in% zero_codes)

    if (nrow(zero_sf) == 0) {
      # Codes might not match — show empty map
      return(
        leaflet(grid_10km) |>
          addProviderTiles(providers$CartoDB.Positron)
      )
    }

    leaflet(zero_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor   = pal$coral,
        fillOpacity = 0.7,
        weight      = 0.5,
        color       = pal$coral,
        popup       = ~paste0("Cell: ", eeacellcode, "<br>Status: Zero coverage"))
  })

  output$zero_table <- renderDT({
    req(priority_zero)
    datatable(priority_zero |> slice_head(n = 100),
      options = list(pageLength = 6, scrollX = TRUE), style = "bootstrap4")
  })

  # Stale cells map — color-coded by staleness
  output$stale_map <- renderLeaflet({
    req(grid_10km)

    if (is.null(priority_stale) || nrow(priority_stale) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = 16, lat = 63, zoom = 5)
      )
    }

    stale_join <- priority_stale |>
      mutate(years_stale = staleness_months / 12) |>
      select(eeacellcode, staleness_months, years_stale, total_occurrences)

    stale_sf <- grid_10km |>
      inner_join(stale_join, by = "eeacellcode")

    if (nrow(stale_sf) == 0) {
      return(
        leaflet(grid_10km) |>
          addProviderTiles(providers$CartoDB.Positron)
      )
    }

    yrs <- stale_sf$years_stale

    pal_stale <- colorNumeric(
      palette = c(pal$sand, pal$coral, "#8b2020"),
      domain = yrs,
      na.color = "#ccc")

    leaflet(stale_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor   = ~pal_stale(yrs),
        fillOpacity = 0.7,
        weight      = 0.3,
        color       = "#999",
        popup       = ~paste0(
          "Cell: ", eeacellcode,
          "<br>Last sampled: ", round(yrs, 1), " years ago",
          "<br>Total occurrences: ", comma(total_occurrences))) |>
      addLegend("bottomright", pal = pal_stale, values = yrs,
        title = "Years since sampled")
  })

  output$stale_table <- renderDT({
    req(priority_stale)
    datatable(
      priority_stale |>
        mutate(years_stale = round(staleness_months / 12, 1)) |>
        select(any_of(c("eeacellcode", "years_stale", "total_occurrences",
                         "last_ym", "priority_level"))) |>
        arrange(desc(years_stale)) |>
        slice_head(n = 100),
      options = list(pageLength = 6, scrollX = TRUE), style = "bootstrap4")
  })

  # Export combined action plan
  output$download_action_plan <- downloadHandler(
    filename = function() {
      paste0("action_plan_", Sys.Date(), ".csv")
    },
    content = function(file) {
      parts <- list()

      # Zero coverage cells
      if (!is.null(priority_zero) && nrow(priority_zero) > 0) {
        parts[[1]] <- priority_zero |>
          mutate(priority_type = "zero_coverage") |>
          select(priority_type, any_of(c("eeacellcode", "grid", "priority_reason", "priority_level")))
      }

      # Stale cells
      if (!is.null(priority_stale) && nrow(priority_stale) > 0) {
        parts[[length(parts) + 1]] <- priority_stale |>
          mutate(priority_type = "stale_cell",
                 years_stale = round(staleness_months / 12, 1)) |>
          select(priority_type, any_of(c("eeacellcode", "grid", "years_stale",
                   "staleness_months", "total_occurrences", "last_ym",
                   "priority_reason", "priority_level")))
      }

      # Missing threatened species
      priority_taxa <- safe_get("priority_taxa_missing")
      if (!is.null(priority_taxa) && nrow(priority_taxa) > 0) {
        parts[[length(parts) + 1]] <- priority_taxa |>
          filter(threatStatus %in% c("CR", "EN")) |>
          mutate(priority_type = "missing_threatened") |>
          select(priority_type, any_of(c("scientificName", "threatStatus",
                   "kingdom", "phylum", "class", "order", "family")))
      }

      # Combine with bind_rows (fills missing columns with NA)
      combined <- bind_rows(parts)
      readr::write_csv(combined, file)
    }
  )

  # ===================================================================
  # EXPLORER
  # ===================================================================

  explorer_data <- reactive({
    req(input$explorer_ds)
    df <- safe_get(input$explorer_ds)
    if (is.null(df)) return(tibble(Message = "Dataset not available"))
    # Remove geometry columns for display
    if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
    df |> slice_head(n = 1000)
  })

  output$explorer_table <- renderDT({
    datatable(explorer_data(),
      options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap4", filter = "top")
  }, server = TRUE)

  output$explorer_download <- downloadHandler(
    filename = function() {
      paste0(input$explorer_ds, "_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Get filtered row indices from DT
      filtered_rows <- input$explorer_table_rows_all
      df <- explorer_data()
      if (!is.null(filtered_rows)) {
        df <- df[filtered_rows, , drop = FALSE]
      }
      readr::write_csv(df, file)
    }
  )
}

shinyApp(ui, server)
