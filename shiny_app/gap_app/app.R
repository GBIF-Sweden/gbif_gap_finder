# =============================================================================
# gbifgaps — Gap Analysis Dashboard
# =============================================================================
# Interactive dashboard for identifying and prioritising biodiversity data gaps
# across spatial, temporal, and taxonomic dimensions.
#
# To run: shiny::runApp("shiny_app/gap_analysis")
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

data_paths <- c("data/shiny_data.rds", "../data/shiny_data.rds", "shiny_data.rds")
data_path <- NULL
for (p in data_paths) {
  if (file.exists(p)) { data_path <- p; break }
}
if (is.null(data_path)) stop("shiny_data.rds not found. Run scripts/11_prepare_shiny_data.R")

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

# Derived
basis_types   <- if (!is.null(spatial_gaps)) unique(spatial_gaps$basisofrecord) else "all"
order_choices <- if (!is.null(top_orders)) top_orders$order else character(0)
current_year  <- year(Sys.Date())
country_name  <- tryCatch(yaml::read_yaml("../../config.yml")$country$name, error = function(e) "")

# Plotly theme helper — light background, warm palette
plotly_layout <- function(p, ...) {
  p |> layout(
    paper_bgcolor = "transparent",
    plot_bgcolor  = "#fafaf7",
    font = list(color = "#2d2d2d", family = "Outfit"),
    xaxis = list(gridcolor = "#e8e7e1", zerolinecolor = "#e0dfda"),
    yaxis = list(gridcolor = "#e8e7e1", zerolinecolor = "#e0dfda"),
    ...
  )
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
      if (!is.null(metadata)) tagList(
        span("Prepared: ", span(class = "header-stat-value",
          format(metadata$created_at, "%d %b %Y"))),
        span("Datasets: ", span(class = "header-stat-value", metadata$n_datasets))
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

          # Four gap summary panels
          fluidRow(
            column(6,
              div(class = "gap-panel spatial",
                div(class = "gap-panel-title", icon("map"), "Spatial Gaps"),
                div(class = "gap-metric", style = paste0("color:", pal$sage, ";"),
                  textOutput("ov_spatial_pct", inline = TRUE)),
                div(class = "gap-detail",
                  textOutput("ov_spatial_detail", inline = TRUE))
              )
            ),
            column(6,
              div(class = "gap-panel temporal",
                div(class = "gap-panel-title", icon("clock"), "Temporal Gaps"),
                div(class = "gap-metric", style = paste0("color:", pal$slate, ";"),
                  textOutput("ov_temporal_pct", inline = TRUE)),
                div(class = "gap-detail",
                  textOutput("ov_temporal_detail", inline = TRUE))
              )
            )
          ),
          fluidRow(
            column(6,
              div(class = "gap-panel taxonomic",
                div(class = "gap-panel-title", icon("leaf"), "Taxonomic Gaps"),
                div(class = "gap-metric", style = paste0("color:", pal$sand, ";"),
                  textOutput("ov_tax_pct", inline = TRUE)),
                div(class = "gap-detail",
                  textOutput("ov_tax_detail", inline = TRUE))
              )
            ),
            column(6,
              div(class = "gap-panel threatened",
                div(class = "gap-panel-title", icon("exclamation-triangle"), "Threatened Species"),
                div(class = "gap-metric", style = paste0("color:", pal$coral, ";"),
                  textOutput("ov_threat_pct", inline = TRUE)),
                div(class = "gap-detail",
                  textOutput("ov_threat_detail", inline = TRUE))
              )
            )
          ),

          # Coverage overview chart
          div(class = "card",
            div(class = "card-title", icon("chart-bar"), "Coverage Overview"),
            plotlyOutput("overview_coverage", height = "260px"))
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
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("map-marker-alt"), "Zero Coverage Cells"),
              leafletOutput("zero_map", height = "350px"),
              div(style = "margin-top:0.75rem;"),
              DTOutput("zero_table"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("hourglass-half"), "Stale Cells"),
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
  time_filtered <- reactive({
    req(time_summary)
    time_summary |> filter(
      basisofrecord == "all",
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
    if (!is.null(dashboard) && dashboard$threatened_in_reference[1] > 0)
      paste0(comma(dashboard$threatened_missing[1]), " missing")
    else "No threat data"
  })
  output$ov_threat_detail <- renderText({
    if (!is.null(dashboard) && dashboard$threatened_in_reference[1] > 0)
      paste0(comma(dashboard$threatened_in_reference[1]),
             " threatened species in backbone, ",
             comma(dashboard$threatened_in_gbif[1]), " found in GBIF.")
    else "Threat status data not available in the current backbone. Enable a red list in config.yml to see threatened species analysis."
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

  # Reactive map update when map_var changes
  observe({
    req(grid_10km, spatial_gaps, input$map_var)

    sf_base <- spatial_gaps |> filter(basisofrecord == "all")

    if (input$map_var == "stale" && !is.null(cell_recency)) {
      rec <- cell_recency |> filter(basisofrecord == "all") |>
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
    sf <- spatial_gaps |> filter(basisofrecord == "all")
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
    df <- spatial_gaps |> filter(basisofrecord == "all", occurrences > 0)
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
      summarise(occ = sum(occurrences, na.rm = TRUE), .groups = "drop")
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
      summarise(occ = sum(occurrences, na.rm = TRUE), .groups = "drop")

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
      summarise(occ = sum(occurrences, na.rm = TRUE), .groups = "drop") |>
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
      add_trace(x = ~miss, name = "Missing",
        marker = list(color = pal$sand2),
        text = "", textposition = "none") |>
      plotly_layout(
        barmode = "stack",
        xaxis = list(title = "Number of species"),
        yaxis = list(title = ""),
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
        xaxis = list(title = "Coverage %", range = c(0, 105)),
        yaxis = list(title = ""))
  })

  output$tax_change <- renderPlotly({
    req(order_summary)
    # Derive recent vs historical from order_summary
    df <- order_summary |>
      filter(grid == "grid10km") |>
      mutate(
        period = ifelse(first_year < 2000 & last_year >= 2000, "both",
                 ifelse(last_year < 2000, "historical", "recent"))) |>
      filter(period == "both") |>
      # We don't have year-level split in order_summary, so show top orders by total
      arrange(desc(total_occurrences)) |>
      slice_head(n = 15) |>
      mutate(
        bar_col = ifelse(total_occurrences >= median(total_occurrences), pal$sage, pal$sand))

    plot_ly(df, y = ~reorder(order, total_occurrences), x = ~total_occurrences, type = "bar",
      marker = list(color = ~bar_col), orientation = "h",
      hovertemplate = "%{y}: %{x:,.0f} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Total occurrences"),
        yaxis = list(title = ""))
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
    # tax_by_order has n_threatened — show orders with missing threatened species
    req(tax_by_order)
    df <- tax_by_order |>
      filter(n_threatened > 0) |>
      mutate(threatened_missing = n_threatened - n_threatened_in_gbif) |>
      filter(threatened_missing > 0) |>
      select(order, n_threatened, n_threatened_in_gbif, threatened_missing, pct_coverage) |>
      arrange(desc(threatened_missing))

    if (nrow(df) == 0) {
      df <- tibble(Message = "No threatened species data available. Enable a red list in config.yml.")
    }

    datatable(df, options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap4", filter = "top")
  })

  # ===================================================================
  # PRIORITIES
  # ===================================================================

  output$stat_zero <- renderText({
    if (!is.null(priority_zero)) comma(nrow(priority_zero)) else "0"
  })
  output$stat_stale <- renderText({
    if (!is.null(priority_stale)) comma(nrow(priority_stale)) else "0"
  })
  output$stat_taxa <- renderText({
    if (!is.null(dashboard)) comma(dashboard$n_priority_taxa[1]) else "0"
  })

  output$action_table <- renderUI({
    n_zero  <- if (!is.null(priority_zero)) nrow(priority_zero) else 0
    n_stale <- if (!is.null(priority_stale)) nrow(priority_stale) else 0
    n_taxa  <- if (!is.null(dashboard)) dashboard$n_priority_taxa[1] else 0

    HTML(paste0('
      <table class="action-table">
        <tr><td><span class="priority-badge priority-high">HIGH</span></td>
            <td>Survey zero-coverage grid cells</td>
            <td style="text-align:right;color:', pal$sage, ';">', comma(n_zero), ' cells</td></tr>
        <tr><td><span class="priority-badge priority-high">HIGH</span></td>
            <td>Monitor CR and EN species lacking GBIF records</td>
            <td style="text-align:right;color:', pal$sage, ';">', comma(n_taxa), ' species</td></tr>
        <tr><td><span class="priority-badge priority-medium">MEDIUM</span></td>
            <td>Re-survey cells with >5 years since last observation</td>
            <td style="text-align:right;color:', pal$sage, ';">', comma(n_stale), ' cells</td></tr>
        <tr><td><span class="priority-badge priority-low">LOW</span></td>
            <td>Expand citizen science programs to low-coverage regions</td>
            <td style="text-align:right;color:', pal$muted, ';">Ongoing</td></tr>
      </table>'))
  })

  # Zero coverage map
  output$zero_map <- renderLeaflet({
    req(grid_10km, priority_zero)

    zero_codes <- priority_zero$eeacellcode
    zero_sf <- grid_10km |> filter(eeacellcode %in% zero_codes)

    leaflet(zero_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor   = pal$coral,
        fillOpacity = 0.6,
        weight      = 0.5,
        color       = pal$coral,
        popup       = ~paste0("Cell: ", eeacellcode))
  })

  output$zero_table <- renderDT({
    req(priority_zero)
    datatable(priority_zero |> slice_head(n = 100),
      options = list(pageLength = 8, scrollX = TRUE), style = "bootstrap4")
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
      options = list(pageLength = 8, scrollX = TRUE), style = "bootstrap4")
  })

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
  })

  output$explorer_download <- downloadHandler(
    filename = function() {
      paste0(input$explorer_ds, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(explorer_data(), file)
    }
  )
}

shinyApp(ui, server)
