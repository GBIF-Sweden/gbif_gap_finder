# shiny_app/app.R
# ==============================================================================
# GBIF Sweden Gap Analysis - Interactive Dashboard
# ==============================================================================
# 
# This Shiny app provides interactive exploration of biodiversity data gaps
# across spatial, temporal, and taxonomic dimensions.
#
# Data source: Output from scripts/11_prepare_shiny_data.R
# 
# To run locally:
#   shiny::runApp("shiny_app")
#
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(scales)
library(viridis)
library(glue)
library(lubridate)

# =============================================================================
# Load Data
# =============================================================================

# Try multiple paths for flexibility
data_paths <- c(
  "data/shiny_data.rds",
  "../data_proc/shiny_data.rds",
  "shiny_data.rds",
  "../shiny_app/data/shiny_data.rds"
)

data_path <- NULL
for (p in data_paths) {
  if (file.exists(p)) {
    data_path <- p
    break
  }
}

if (is.null(data_path)) {
  stop("Could not find shiny_data.rds. Run scripts/11_prepare_shiny_data.R first.")
}

# Load the data bundle
app_data <- readRDS(data_path)

# Extract commonly used datasets with safe access
safe_get <- function(name) {
  if (name %in% names(app_data)) app_data[[name]] else NULL
}

dashboard <- safe_get("dashboard")
grid_10km <- safe_get("grid_10km")
spatial_gaps <- safe_get("spatial_gaps_10km")
cell_recency <- safe_get("cell_recency_10km")
time_summary <- safe_get("time_summary_10km")
order_5yr <- safe_get("order_5yr")
top_orders <- safe_get("top_orders")
tax_by_threat <- safe_get("tax_by_threat")
tax_by_kingdom <- safe_get("tax_by_kingdom")
tax_by_order <- safe_get("tax_by_order")
tax_by_family <- safe_get("tax_by_family")
priority_taxa <- safe_get("priority_taxa_missing")
if (is.null(priority_taxa)) priority_taxa <- safe_get("priority_taxa_all")
priority_zero <- safe_get("priority_zero_cells")
priority_stale <- safe_get("priority_stale_cells")
order_change <- safe_get("order_change")
comparison_grids <- safe_get("comparison_grids")

# Get available basis of record types
basis_types <- if (!is.null(spatial_gaps) && "basisofrecord" %in% names(spatial_gaps)) {
  unique(spatial_gaps$basisofrecord)
} else {
  c("all")
}

# Get available orders
order_choices <- if (!is.null(top_orders)) top_orders$order else c()

# Current year
current_year <- year(Sys.Date())

# Color palettes
threat_colors <- c("CR" = "#D32F2F", "EN" = "#E64A19", "VU" = "#F57C00", 
                   "NT" = "#FBC02D", "LC" = "#8BC34A", "DD" = "#78909C")
gap_colors <- c("In GBIF" = "#2A9D8F", "Missing" = "#E76F51")

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "GBIF Sweden Gap Analysis", titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Spatial Gaps", tabName = "spatial", icon = icon("map")),
      menuItem("Temporal Gaps", tabName = "temporal", icon = icon("clock")),
      menuItem("Taxonomic Gaps", tabName = "taxonomic", icon = icon("leaf")),
      menuItem("Threatened Species", tabName = "threatened", icon = icon("exclamation-circle")),
      menuItem("Priority Actions", tabName = "priorities", icon = icon("exclamation-triangle")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    ),
    hr(),
    h4("Filters", style = "padding-left: 15px; color: #fff;"),
    pickerInput("basis_filter", "Basis of Record:", choices = basis_types, selected = "all"),
    conditionalPanel(
      condition = "input.tabs == 'temporal'",
      sliderInput("year_range", "Year Range:", min = 1900, max = current_year, 
                  value = c(1970, current_year), step = 1, sep = "")
    ),
    hr(),
    div(style = "padding: 15px; color: #aaa; font-size: 11px;",
        HTML(paste0("Data: ", if (!is.null(app_data$metadata)) 
          format(app_data$metadata$created_at, "%Y-%m-%d") else "?",
          "<br>Threat data: ", if (!is.null(app_data$metadata)) 
            ifelse(app_data$metadata$has_threat_status, "✓", "✗") else "?")))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box, .info-box, .small-box { border-radius: 5px; }
    "))),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_spatial", width = 3),
          valueBoxOutput("vb_temporal", width = 3),
          valueBoxOutput("vb_taxonomic", width = 3),
          valueBoxOutput("vb_threatened", width = 3)
        ),
        fluidRow(
          box(title = "Coverage Summary", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("overview_coverage_plot", height = "350px")),
          box(title = "Kingdom Coverage", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("overview_kingdom_plot", height = "350px"))
        ),
        fluidRow(
          box(title = "Quick Stats", status = "info", solidHeader = TRUE, width = 12,
              tableOutput("overview_stats_table"))
        )
      ),
      
      # Spatial Tab
      tabItem(tabName = "spatial",
        fluidRow(
          box(title = "Geographic Coverage", status = "primary", solidHeader = TRUE, width = 8,
              leafletOutput("spatial_map", height = "600px")),
          box(title = "Map Options", status = "info", width = 4,
              radioButtons("map_variable", "Display:", 
                           choices = c("Occurrences" = "log_occ", "Staleness" = "staleness", 
                                       "Zero Coverage" = "zero"), selected = "log_occ"),
              hr(), h4("Statistics"), tableOutput("spatial_stats_table"))
        ),
        fluidRow(
          box(title = "Grid Comparison", status = "warning", solidHeader = TRUE, width = 6,
              plotlyOutput("spatial_grid_plot", height = "300px")),
          box(title = "Distribution", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("spatial_hist_plot", height = "300px"))
        )
      ),
      
      # Temporal Tab
      tabItem(tabName = "temporal",
        fluidRow(
          box(title = "Historical Trend", status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("temporal_trend_plot", height = "350px")),
          box(title = "Orders", status = "info", width = 4,
              pickerInput("order_select", "Select Orders:", choices = order_choices,
                          selected = head(order_choices, 5), multiple = TRUE,
                          options = list(`actions-box` = TRUE)))
        ),
        fluidRow(
          box(title = "Seasonal Pattern", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("temporal_seasonal_plot", height = "300px")),
          box(title = "Order Trends", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("order_mobilization_plot", height = "300px"))
        ),
        fluidRow(
          box(title = "Year × Month Heatmap", status = "warning", solidHeader = TRUE, width = 12,
              plotlyOutput("temporal_heatmap", height = "400px"))
        )
      ),
      
      # Taxonomic Tab
      tabItem(tabName = "taxonomic",
        fluidRow(
          box(title = "Coverage by Order", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("tax_order_plot", height = "450px")),
          box(title = "Coverage by Family", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("tax_family_plot", height = "450px"))
        ),
        fluidRow(
          box(title = "Recent vs Historical Change", status = "info", solidHeader = TRUE, width = 12,
              plotlyOutput("tax_change_plot", height = "350px"))
        )
      ),
      
      # Threatened Species Tab
      tabItem(tabName = "threatened",
        fluidRow(
          valueBoxOutput("vb_cr", width = 3), valueBoxOutput("vb_en", width = 3),
          valueBoxOutput("vb_vu", width = 3), valueBoxOutput("vb_nt", width = 3)
        ),
        fluidRow(
          box(title = "Coverage by Threat Status", status = "danger", solidHeader = TRUE, width = 6,
              plotlyOutput("threat_coverage_plot", height = "350px")),
          box(title = "Missing Taxa", status = "warning", solidHeader = TRUE, width = 6,
              plotlyOutput("threat_missing_plot", height = "350px"))
        ),
        fluidRow(
          box(title = "Missing CR & EN Species", status = "danger", solidHeader = TRUE, width = 12,
              DTOutput("threat_table"))
        )
      ),
      
      # Priorities Tab
      tabItem(tabName = "priorities",
        fluidRow(
          infoBoxOutput("ib_zero", width = 4),
          infoBoxOutput("ib_stale", width = 4),
          infoBoxOutput("ib_taxa", width = 4)
        ),
        fluidRow(
          box(title = "Zero Coverage Cells", status = "danger", solidHeader = TRUE, width = 6,
              DTOutput("zero_table")),
          box(title = "Stale Cells (>5 Years)", status = "warning", solidHeader = TRUE, width = 6,
              DTOutput("stale_table"))
        ),
        fluidRow(
          box(title = "Recommended Actions", status = "info", solidHeader = TRUE, width = 12,
              tableOutput("actions_table"))
        )
      ),
      
      # Explorer Tab
      tabItem(tabName = "explorer",
        fluidRow(
          box(title = "Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("explorer_ds", "Dataset:",
                          choices = c("Spatial Gaps" = "spatial_gaps_10km",
                                      "Taxonomic Match" = "taxonomic_match_summary",
                                      "Threat Coverage" = "tax_by_threat",
                                      "Priority Taxa" = "priority_taxa_missing")),
              DTOutput("explorer_table"))
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive filtered data
  spatial_filtered <- reactive({
    req(spatial_gaps)
    spatial_gaps |> filter(basisofrecord == input$basis_filter)
  })
  
  time_filtered <- reactive({
    req(time_summary)
    time_summary |> filter(basisofrecord == input$basis_filter,
                           year >= input$year_range[1], year <= input$year_range[2])
  })
  
  order_filtered <- reactive({
    req(order_5yr, input$order_select)
    order_5yr |> filter(order %in% input$order_select,
                        period_start >= input$year_range[1])
  })
  
  # Overview outputs
  output$vb_spatial <- renderValueBox({
    val <- if (!is.null(dashboard)) dashboard$cells_10km_pct_coverage[1] else "?"
    valueBox(paste0(val, "%"), "Spatial (10km)", icon = icon("map"),
             color = if (is.numeric(val) && val >= 75) "green" else "yellow")
  })
  
  output$vb_temporal <- renderValueBox({
    val <- if (!is.null(dashboard)) dashboard$year_range[1] else "?"
    valueBox(paste0(val, " yrs"), "Temporal Range", icon = icon("clock"), color = "blue")
  })
  
  output$vb_taxonomic <- renderValueBox({
    val <- if (!is.null(dashboard)) dashboard$taxa_pct_coverage[1] else "?"
    valueBox(paste0(val, "%"), "Taxonomic", icon = icon("leaf"),
             color = if (is.numeric(val) && val >= 50) "green" else "yellow")
  })
  
  output$vb_threatened <- renderValueBox({
    n <- if (!is.null(priority_taxa)) sum(priority_taxa$threatStatus %in% c("CR", "EN")) else 0
    valueBox(comma(n), "Missing CR/EN", icon = icon("exclamation-circle"),
             color = if (n > 0) "red" else "green")
  })
  
  output$overview_coverage_plot <- renderPlotly({
    req(dashboard)
    df <- tibble(
      dim = c("10km Spatial", "50km Spatial", "Taxonomic"),
      cov = c(as.numeric(dashboard$cells_10km_pct_coverage[1]),
              as.numeric(dashboard$cells_50km_pct_coverage[1]),
              as.numeric(dashboard$taxa_pct_coverage[1]))
    ) |> filter(!is.na(cov)) |>
      mutate(gap = 100 - cov) |>
      pivot_longer(c(cov, gap), names_to = "type", values_to = "pct") |>
      mutate(type = factor(type, c("gap", "cov"), c("Gap", "Covered")))
    
    ggplotly(ggplot(df, aes(dim, pct, fill = type)) + geom_col() + coord_flip() +
               scale_fill_manual(values = c("Gap" = "#E76F51", "Covered" = "#2A9D8F")) +
               labs(x = NULL, y = "%", fill = NULL) + theme_minimal())
  })
  
  output$overview_kingdom_plot <- renderPlotly({
    req(tax_by_kingdom)
    df <- tax_by_kingdom |> slice_head(n = 8) |>
      pivot_longer(c(n_in_gbif, n_missing), names_to = "status", values_to = "n") |>
      mutate(status = factor(status, c("n_missing", "n_in_gbif"), c("Missing", "In GBIF")))
    ggplotly(ggplot(df, aes(reorder(kingdom, n), n, fill = status)) +
               geom_col() + coord_flip() + scale_fill_manual(values = gap_colors) +
               labs(x = NULL, y = "Species", fill = NULL) + theme_minimal())
  })
  
  output$overview_stats_table <- renderTable({
    req(dashboard)
    tibble(Metric = c("10km Cells", "Taxa in GBIF", "Priority Taxa"),
           Value = c(comma(dashboard$cells_10km_with_data[1]),
                     comma(dashboard$taxa_in_gbif[1]),
                     comma(dashboard$n_priority_taxa[1])))
  })
  
  # Spatial outputs
  output$spatial_map <- renderLeaflet({
    req(grid_10km, spatial_filtered())
    map_data <- grid_10km |> left_join(spatial_filtered(), by = "eeacellcode") |>
      mutate(log_occ = ifelse(is.na(occurrences), 0, log10(occurrences + 1)))
    pal <- colorNumeric("viridis", map_data$log_occ, na.color = "#CCC")
    leaflet(map_data) |> addTiles() |>
      addPolygons(fillColor = ~pal(log_occ), fillOpacity = 0.7, weight = 0.5, color = "white") |>
      addLegend("bottomright", pal = pal, values = ~log_occ, title = "log10(occ)")
  })
  
  output$spatial_stats_table <- renderTable({
    req(spatial_filtered())
    sf <- spatial_filtered()
    tibble(Metric = c("Total", "With Data", "Coverage"),
           Value = c(comma(nrow(sf)), comma(sum(sf$occurrences > 0, na.rm = TRUE)),
                     paste0(round(100 * mean(sf$occurrences > 0, na.rm = TRUE), 1), "%")))
  })
  
  output$spatial_grid_plot <- renderPlotly({
    req(comparison_grids)
    df <- comparison_grids |> mutate(empty = n_cells_total - n_cells_with_data) |>
      pivot_longer(c(n_cells_with_data, empty), names_to = "s", values_to = "n")
    ggplotly(ggplot(df, aes(grid_resolution, n, fill = s)) + geom_col() +
               scale_fill_manual(values = c("n_cells_with_data" = "#2A9D8F", "empty" = "#E76F51")) +
               theme_minimal())
  })
  
  output$spatial_hist_plot <- renderPlotly({
    req(spatial_filtered())
    df <- spatial_filtered() |> filter(occurrences > 0)
    ggplotly(ggplot(df, aes(log10(occurrences + 1))) +
               geom_histogram(bins = 30, fill = "#2A9D8F") + theme_minimal())
  })
  
  # Temporal outputs
  output$temporal_trend_plot <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(year) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    ggplotly(ggplot(df, aes(year, occ)) + geom_area(fill = "#2A9D8F", alpha = 0.3) +
               geom_line(color = "#2A9D8F") + scale_y_continuous(labels = comma) + theme_minimal())
  })
  
  output$temporal_seasonal_plot <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(month) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    ggplotly(ggplot(df, aes(factor(month), occ, fill = occ)) + geom_col() +
               scale_fill_viridis_c() + scale_x_discrete(labels = month.abb) + theme_minimal())
  })
  
  output$order_mobilization_plot <- renderPlotly({
    req(order_filtered(), nrow(order_filtered()) > 0)
    ggplotly(ggplot(order_filtered(), aes(period, occurrences, fill = order)) +
               geom_col(position = "dodge") + scale_fill_viridis_d() + theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  
  output$temporal_heatmap <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(year, month) |>
      summarise(occ = sum(occurrences, na.rm = TRUE), .groups = "drop") |>
      mutate(log_occ = log10(occ + 1))
    ggplotly(ggplot(df, aes(factor(month), year, fill = log_occ)) + geom_tile() +
               scale_fill_viridis() + scale_x_discrete(labels = month.abb) + theme_minimal())
  })
  
  # Taxonomic outputs
  output$tax_order_plot <- renderPlotly({
    req(tax_by_order)
    df <- tax_by_order |> filter(!is.na(order), n_taxa >= 10) |> slice_head(n = 20) |>
      mutate(miss = n_taxa - n_in_gbif) |>
      pivot_longer(c(n_in_gbif, miss), names_to = "s", values_to = "n")
    ggplotly(ggplot(df, aes(reorder(order, n), n, fill = s)) + geom_col() + coord_flip() +
               scale_fill_manual(values = c("n_in_gbif" = "#2A9D8F", "miss" = "#E76F51")) +
               theme_minimal())
  })
  
  output$tax_family_plot <- renderPlotly({
    req(tax_by_family)
    df <- tax_by_family |> filter(!is.na(family), n_taxa >= 5) |> slice_head(n = 20)
    ggplotly(ggplot(df, aes(reorder(family, pct_coverage), pct_coverage, fill = pct_coverage)) +
               geom_col() + coord_flip() + scale_fill_viridis_c() + theme_minimal())
  })
  
  output$tax_change_plot <- renderPlotly({
    req(order_change)
    ggplotly(ggplot(order_change, aes(reorder(order, pct_change), pct_change, fill = direction)) +
               geom_col() + coord_flip() + geom_hline(yintercept = 0, linetype = "dashed") +
               scale_fill_manual(values = c("Increased" = "#2A9D8F", "Decreased" = "#E76F51")) +
               theme_minimal())
  })
  
  # Threatened outputs
  output$vb_cr <- renderValueBox({
    n <- if (!is.null(tax_by_threat)) sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "CR"]) else 0
    valueBox(comma(n), "CR Missing", icon = icon("skull"), color = "red")
  })
  output$vb_en <- renderValueBox({
    n <- if (!is.null(tax_by_threat)) sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "EN"]) else 0
    valueBox(comma(n), "EN Missing", icon = icon("exclamation-triangle"), color = "orange")
  })
  output$vb_vu <- renderValueBox({
    n <- if (!is.null(tax_by_threat)) sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "VU"]) else 0
    valueBox(comma(n), "VU Missing", icon = icon("exclamation"), color = "yellow")
  })
  output$vb_nt <- renderValueBox({
    n <- if (!is.null(tax_by_threat)) sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "NT"]) else 0
    valueBox(comma(n), "NT Missing", icon = icon("info-circle"), color = "aqua")
  })
  
  output$threat_coverage_plot <- renderPlotly({
    req(tax_by_threat)
    df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT", "LC")) |>
      mutate(threatStatus = factor(threatStatus, c("CR", "EN", "VU", "NT", "LC")))
    ggplotly(ggplot(df, aes(threatStatus, pct_coverage, fill = threatStatus)) + geom_col() +
               scale_fill_manual(values = threat_colors) + ylim(0, 110) + theme_minimal())
  })
  
  output$threat_missing_plot <- renderPlotly({
    req(tax_by_threat)
    df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT"))
    ggplotly(ggplot(df, aes(threatStatus, n_missing, fill = threatStatus)) + geom_col() +
               scale_fill_manual(values = threat_colors) + theme_minimal())
  })
  
  output$threat_table <- renderDT({
    req(priority_taxa)
    priority_taxa |> filter(threatStatus %in% c("CR", "EN")) |>
      select(any_of(c("scientificName", "threatStatus", "order", "family"))) |>
      datatable(options = list(pageLength = 15), filter = "top")
  })
  
  # Priorities outputs
  output$ib_zero <- renderInfoBox({
    n <- if (!is.null(priority_zero)) nrow(priority_zero) else 0
    infoBox("Zero Cells", comma(n), icon = icon("map-marker-alt"), color = "red")
  })
  output$ib_stale <- renderInfoBox({
    n <- if (!is.null(priority_stale)) nrow(priority_stale) else 0
    infoBox("Stale Cells", comma(n), icon = icon("clock"), color = "orange")
  })
  output$ib_taxa <- renderInfoBox({
    n <- if (!is.null(priority_taxa)) sum(priority_taxa$threatStatus %in% c("CR", "EN")) else 0
    infoBox("CR/EN Taxa", comma(n), icon = icon("exclamation-circle"), color = "red")
  })
  
  output$zero_table <- renderDT({
    req(priority_zero)
    datatable(priority_zero |> slice_head(n = 100), options = list(pageLength = 10))
  })
  
  output$stale_table <- renderDT({
    req(priority_stale)
    datatable(priority_stale |> mutate(years = round(staleness_months/12, 1)) |>
                select(any_of(c("eeacellcode", "years", "total_occurrences"))) |>
                slice_head(n = 100), options = list(pageLength = 10))
  })
  
  output$actions_table <- renderTable({
    tibble(
      Priority = c("🔴 High", "🔴 High", "🟠 Medium"),
      Action = c("Survey zero-coverage cells", "Monitor CR/EN species", "Re-survey stale cells"),
      Target = c(paste(comma(if (!is.null(priority_zero)) nrow(priority_zero) else 0), "cells"),
                 paste(comma(if (!is.null(priority_taxa)) sum(priority_taxa$threatStatus %in% c("CR", "EN")) else 0), "species"),
                 paste(comma(if (!is.null(priority_stale)) nrow(priority_stale) else 0), "cells"))
    )
  })
  
  # Explorer
  output$explorer_table <- renderDT({
    df <- safe_get(input$explorer_ds)
    if (is.null(df)) return(datatable(tibble(Message = "Not available")))
    datatable(df |> slice_head(n = 1000), options = list(pageLength = 20, scrollX = TRUE), filter = "top")
  })
}

shinyApp(ui, server)
