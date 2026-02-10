# =============================================================================
# GBIF SWEDEN GAP ANALYSIS DASHBOARD
# =============================================================================
#
# Interactive dashboard for GBIF staff to identify and prioritize 
# biodiversity data gaps across spatial, temporal, and taxonomic dimensions.
#
# To run: shiny::runApp("shiny_app/gap_analysis")
#
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
library(viridis)
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
priority_taxa <- safe_get("priority_taxa_missing") %||% safe_get("priority_taxa_all")
priority_zero <- safe_get("priority_zero_cells")
priority_stale <- safe_get("priority_stale_cells")
order_change <- safe_get("order_change")
comparison_grids <- safe_get("comparison_grids")

basis_types <- if (!is.null(spatial_gaps)) unique(spatial_gaps$basisofrecord) else "all"
order_choices <- if (!is.null(top_orders)) top_orders$order else c()
current_year <- year(Sys.Date())

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .main-title {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
    "))
  ),
  
  # Header
  div(class = "main-header",
      div(
        div(class = "main-title", "📊 GBIF Sweden Gap Analysis"),
        div(class = "main-subtitle", "Identify and prioritize biodiversity data gaps")
      )
  ),
  
  # Main content
  div(style = "padding: 0 1rem;",
      tabsetPanel(
        id = "main_tabs",
        type = "pills",
        
        # ===== OVERVIEW TAB =====
        tabPanel(
          title = tagList(icon("dashboard"), "Overview"),
          value = "overview",
          
          div(style = "padding: 1.5rem 0;",
              
              # Stats row
              div(class = "stat-grid",
                  div(class = "stat-box",
                      div(class = "stat-value purple", textOutput("stat_spatial", inline = TRUE)),
                      div(class = "stat-label", "Spatial Coverage (10km)")),
                  div(class = "stat-box",
                      div(class = "stat-value teal", textOutput("stat_temporal", inline = TRUE)),
                      div(class = "stat-label", "Years of Data")),
                  div(class = "stat-box",
                      div(class = "stat-value amber", textOutput("stat_taxonomic", inline = TRUE)),
                      div(class = "stat-label", "Taxonomic Coverage")),
                  div(class = "stat-box",
                      div(class = "stat-value red", textOutput("stat_priority", inline = TRUE)),
                      div(class = "stat-label", "Missing CR/EN Species"))
              ),
              
              # Charts row
              fluidRow(
                column(6, div(class = "card",
                              div(class = "card-title", icon("chart-bar"), "Coverage Overview"),
                              plotlyOutput("overview_coverage", height = "300px"))),
                column(6, div(class = "card",
                              div(class = "card-title", icon("sitemap"), "Coverage by Kingdom"),
                              plotlyOutput("overview_kingdom", height = "300px")))
              ),
              
              fluidRow(
                column(12, div(class = "card",
                               div(class = "card-title", icon("table"), "Key Metrics"),
                               tableOutput("overview_table")))
              )
          )
        ),
        
        # ===== SPATIAL TAB =====
        tabPanel(
          title = tagList(icon("map"), "Spatial"),
          value = "spatial",
          
          div(style = "padding: 1.5rem 0;",
              fluidRow(
                column(8, div(class = "card",
                              div(class = "card-title", icon("globe-europe"), "Geographic Coverage"),
                              leafletOutput("spatial_map", height = "550px"))),
                column(4,
                       div(class = "card",
                           div(class = "card-title", icon("sliders-h"), "Display Options"),
                           radioButtons("map_var", NULL,
                                        choices = c("Occurrences" = "occ", 
                                                    "Staleness" = "stale",
                                                    "Coverage" = "coverage"),
                                        selected = "occ")),
                       div(class = "card",
                           div(class = "card-title", icon("info-circle"), "Statistics"),
                           tableOutput("spatial_stats")))
              ),
              fluidRow(
                column(6, div(class = "card",
                              div(class = "card-title", icon("th"), "Grid Comparison"),
                              plotlyOutput("spatial_grid", height = "280px"))),
                column(6, div(class = "card",
                              div(class = "card-title", icon("chart-area"), "Distribution"),
                              plotlyOutput("spatial_hist", height = "280px")))
              )
          )
        ),
        
        # ===== TEMPORAL TAB =====
        tabPanel(
          title = tagList(icon("clock"), "Temporal"),
          value = "temporal",
          
          div(style = "padding: 1.5rem 0;",
              
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
                              plotlyOutput("temporal_trend", height = "320px"))),
                column(4, div(class = "card",
                              div(class = "card-title", icon("calendar-alt"), "Seasonal Pattern"),
                              plotlyOutput("temporal_season", height = "320px")))
              ),
              fluidRow(
                column(12, div(class = "card",
                               div(class = "card-title", icon("th"), "Year × Month Heatmap"),
                               plotlyOutput("temporal_heatmap", height = "350px")))
              )
          )
        ),
        
        # ===== TAXONOMIC TAB =====
        tabPanel(
          title = tagList(icon("leaf"), "Taxonomic"),
          value = "taxonomic",
          
          div(style = "padding: 1.5rem 0;",
              fluidRow(
                column(6, div(class = "card",
                              div(class = "card-title", icon("layer-group"), "Coverage by Order"),
                              plotlyOutput("tax_order", height = "400px"))),
                column(6, div(class = "card",
                              div(class = "card-title", icon("folder-tree"), "Coverage by Family"),
                              plotlyOutput("tax_family", height = "400px")))
              ),
              fluidRow(
                column(12, div(class = "card",
                               div(class = "card-title", icon("exchange-alt"), "Recent vs Historical Change"),
                               plotlyOutput("tax_change", height = "320px")))
              )
          )
        ),
        
        # ===== THREATENED TAB =====
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Threatened"),
          value = "threatened",
          
          div(style = "padding: 1.5rem 0;",
              
              div(class = "stat-grid",
                  div(class = "stat-box",
                      div(class = "stat-value red", textOutput("stat_cr", inline = TRUE)),
                      div(class = "stat-label", "CR Missing")),
                  div(class = "stat-box",
                      div(class = "stat-value amber", textOutput("stat_en", inline = TRUE)),
                      div(class = "stat-label", "EN Missing")),
                  div(class = "stat-box",
                      div(class = "stat-value", style = "color: #eab308;", textOutput("stat_vu", inline = TRUE)),
                      div(class = "stat-label", "VU Missing")),
                  div(class = "stat-box",
                      div(class = "stat-value green", textOutput("stat_nt", inline = TRUE)),
                      div(class = "stat-label", "NT Missing"))
              ),
              
              fluidRow(
                column(6, div(class = "card",
                              div(class = "card-title", icon("shield-alt"), "Coverage by Threat Status"),
                              plotlyOutput("threat_coverage", height = "320px"))),
                column(6, div(class = "card",
                              div(class = "card-title", icon("times-circle"), "Missing Taxa"),
                              plotlyOutput("threat_missing", height = "320px")))
              ),
              
              fluidRow(
                column(12, div(class = "card",
                               div(class = "card-title", icon("list"), "Missing CR & EN Species"),
                               DTOutput("threat_table")))
              )
          )
        ),
        
        # ===== PRIORITIES TAB =====
        tabPanel(
          title = tagList(icon("bullseye"), "Priorities"),
          value = "priorities",
          
          div(style = "padding: 1.5rem 0;",
              
              div(class = "stat-grid", style = "grid-template-columns: repeat(3, 1fr);",
                  div(class = "stat-box",
                      div(class = "stat-value red", textOutput("stat_zero", inline = TRUE)),
                      div(class = "stat-label", "Zero Coverage Cells")),
                  div(class = "stat-box",
                      div(class = "stat-value amber", textOutput("stat_stale", inline = TRUE)),
                      div(class = "stat-label", "Stale Cells (>5 yrs)")),
                  div(class = "stat-box",
                      div(class = "stat-value purple", textOutput("stat_taxa", inline = TRUE)),
                      div(class = "stat-label", "Priority Taxa (CR/EN)"))
              ),
              
              div(class = "card",
                  div(class = "card-title", icon("tasks"), "Recommended Actions"),
                  uiOutput("action_table")),
              
              fluidRow(
                column(6, div(class = "card",
                              div(class = "card-title", icon("map-marker-alt"), "Zero Coverage Cells"),
                              DTOutput("zero_table"))),
                column(6, div(class = "card",
                              div(class = "card-title", icon("hourglass-half"), "Stale Cells"),
                              DTOutput("stale_table")))
              )
          )
        ),
        
        # ===== EXPLORER TAB =====
        tabPanel(
          title = tagList(icon("database"), "Explorer"),
          value = "explorer",
          
          div(style = "padding: 1.5rem 0;",
              div(class = "card",
                  div(class = "card-title", icon("search"), "Data Explorer"),
                  selectInput("explorer_ds", "Select Dataset:",
                              choices = c("Spatial Gaps" = "spatial_gaps_10km",
                                          "Taxonomic Match" = "taxonomic_match_summary",
                                          "Threat Coverage" = "tax_by_threat",
                                          "Kingdom Coverage" = "tax_by_kingdom",
                                          "Order Coverage" = "tax_by_order",
                                          "Priority Taxa" = "priority_taxa_missing",
                                          "Dashboard" = "dashboard")),
                  DTOutput("explorer_table"))
          )
        )
      ),
      
      # Footer
      div(class = "metadata-footer",
          HTML(paste0(
            "Data prepared: ", 
            if (!is.null(app_data$metadata)) format(app_data$metadata$created_at, "%Y-%m-%d %H:%M") else "Unknown",
            " · Datasets: ", if (!is.null(app_data$metadata)) app_data$metadata$n_datasets else "?",
            " · GBIF Sweden"
          ))
      )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive filtered data
  time_filtered <- reactive({
    req(time_summary)
    time_summary |> filter(basisofrecord == "all",
                           year >= input$year_range[1], year <= input$year_range[2])
  })
  
  order_filtered <- reactive({
    req(order_5yr, input$order_select)
    order_5yr |> filter(order %in% input$order_select, period_start >= input$year_range[1])
  })
  
  # ===== OVERVIEW =====
  output$stat_spatial <- renderText({
    if (!is.null(dashboard)) paste0(dashboard$cells_10km_pct_coverage[1], "%") else "?"
  })
  output$stat_temporal <- renderText({
    if (!is.null(dashboard)) as.character(dashboard$year_range[1]) else "?"
  })
  output$stat_taxonomic <- renderText({
    if (!is.null(dashboard)) paste0(dashboard$taxa_pct_coverage[1], "%") else "?"
  })
  output$stat_priority <- renderText({
    if (!is.null(priority_taxa)) comma(sum(priority_taxa$threatStatus %in% c("CR", "EN"))) else "0"
  })
  
  output$overview_coverage <- renderPlotly({
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
    
    plot_ly(df, x = ~pct, y = ~dim, color = ~type, type = "bar", orientation = "h",
            colors = c("Gap" = "#ef4444", "Covered" = "#667eea")) |>
      layout(barmode = "stack", 
             xaxis = list(title = "", ticksuffix = "%", gridcolor = "rgba(255,255,255,0.05)"),
             yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$overview_kingdom <- renderPlotly({
    req(tax_by_kingdom)
    df <- tax_by_kingdom |> slice_head(n = 6)
    plot_ly(df, x = ~n_in_gbif, y = ~reorder(kingdom, n_ref_total), type = "bar",
            name = "In GBIF", marker = list(color = "#667eea"), orientation = "h") |>
      add_trace(x = ~n_missing, name = "Missing", marker = list(color = "#ef4444")) |>
      layout(barmode = "stack",
             xaxis = list(title = "", gridcolor = "rgba(255,255,255,0.05)"),
             yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$overview_table <- renderTable({
    req(dashboard)
    tibble(
      Metric = c("10km Cells with Data", "Total Occurrences", "Taxa in GBIF", 
                 "Median Staleness", "Priority Taxa"),
      Value = c(comma(dashboard$cells_10km_with_data[1]),
                comma(dashboard$total_occurrences[1]),
                comma(dashboard$taxa_in_gbif[1]),
                paste(dashboard$median_staleness_months_10km[1], "months"),
                comma(dashboard$n_priority_taxa[1]))
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # ===== SPATIAL =====
  output$spatial_map <- renderLeaflet({
    req(grid_10km, spatial_gaps)
    sf_data <- spatial_gaps |> filter(basisofrecord == "all")
    map_data <- grid_10km |> left_join(sf_data, by = "eeacellcode") |>
      mutate(log_occ = ifelse(is.na(occurrences), 0, log10(occurrences + 1)))
    
    pal <- colorNumeric("viridis", map_data$log_occ, na.color = "#333")
    leaflet(map_data) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      addPolygons(fillColor = ~pal(log_occ), fillOpacity = 0.7, 
                  weight = 0.3, color = "#444",
                  popup = ~paste0("Cell: ", eeacellcode, "<br>Occ: ", comma(occurrences))) |>
      addLegend("bottomright", pal = pal, values = ~log_occ, title = "log10(occ)")
  })
  
  output$spatial_stats <- renderTable({
    req(spatial_gaps)
    sf <- spatial_gaps |> filter(basisofrecord == "all")
    tibble(
      Metric = c("Total Cells", "With Data", "Coverage"),
      Value = c(comma(nrow(sf)), comma(sum(sf$occurrences > 0, na.rm = TRUE)),
                paste0(round(100 * mean(sf$occurrences > 0, na.rm = TRUE), 1), "%"))
    )
  }, width = "100%")
  
  output$spatial_grid <- renderPlotly({
    req(comparison_grids)
    df <- comparison_grids |> mutate(empty = n_cells_total - n_cells_with_data)
    plot_ly(df, x = ~grid_resolution, y = ~n_cells_with_data, type = "bar",
            name = "With Data", marker = list(color = "#667eea")) |>
      add_trace(y = ~empty, name = "Empty", marker = list(color = "#ef4444")) |>
      layout(barmode = "stack",
             xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$spatial_hist <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps |> filter(basisofrecord == "all", occurrences > 0)
    plot_ly(df, x = ~log10(occurrences + 1), type = "histogram",
            marker = list(color = "#667eea", line = list(color = "#764ba2", width = 1))) |>
      layout(xaxis = list(title = "log10(Occurrences)", gridcolor = "rgba(255,255,255,0.05)"),
             yaxis = list(title = "Count", gridcolor = "rgba(255,255,255,0.05)"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  # ===== TEMPORAL =====
  output$temporal_trend <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(year) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines",
            fill = "tozeroy", fillcolor = "rgba(102, 126, 234, 0.2)",
            line = list(color = "#667eea", width = 2)) |>
      layout(xaxis = list(title = "", gridcolor = "rgba(255,255,255,0.05)"),
             yaxis = list(title = "Occurrences", gridcolor = "rgba(255,255,255,0.05)"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$temporal_season <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(month) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    plot_ly(df, x = ~month, y = ~occ, type = "bar",
            marker = list(color = ~occ, colorscale = "Viridis")) |>
      layout(xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12),
             yaxis = list(title = "", gridcolor = "rgba(255,255,255,0.05)"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$temporal_heatmap <- renderPlotly({
    req(time_filtered())
    df <- time_filtered() |> group_by(year, month) |>
      summarise(occ = sum(occurrences, na.rm = TRUE), .groups = "drop") |>
      mutate(log_occ = log10(occ + 1))
    plot_ly(df, x = ~month, y = ~year, z = ~log_occ, type = "heatmap",
            colorscale = "Inferno") |>
      layout(xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12),
             yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  # ===== TAXONOMIC =====
  output$tax_order <- renderPlotly({
    req(tax_by_order)
    df <- tax_by_order |> filter(!is.na(order), n_taxa >= 10) |> 
      arrange(desc(n_taxa)) |> slice_head(n = 15) |>
      mutate(miss = n_taxa - n_in_gbif)
    plot_ly(df, y = ~reorder(order, n_taxa), x = ~n_in_gbif, type = "bar",
            name = "In GBIF", marker = list(color = "#667eea"), orientation = "h") |>
      add_trace(x = ~miss, name = "Missing", marker = list(color = "#ef4444")) |>
      layout(barmode = "stack",
             xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$tax_family <- renderPlotly({
    req(tax_by_family)
    df <- tax_by_family |> filter(!is.na(family), n_taxa >= 5) |>
      arrange(desc(n_taxa)) |> slice_head(n = 15)
    plot_ly(df, y = ~reorder(family, pct_coverage), x = ~pct_coverage, type = "bar",
            marker = list(color = ~pct_coverage, colorscale = "Viridis"), orientation = "h") |>
      layout(xaxis = list(title = "Coverage %"), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$tax_change <- renderPlotly({
    req(order_change)
    plot_ly(order_change, y = ~reorder(order, pct_change), x = ~pct_change, type = "bar",
            marker = list(color = ifelse(order_change$pct_change >= 0, "#22c55e", "#ef4444")),
            orientation = "h") |>
      layout(xaxis = list(title = "% Change", zeroline = TRUE, zerolinecolor = "#555"),
             yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  # ===== THREATENED =====
  output$stat_cr <- renderText({
    if (!is.null(tax_by_threat)) comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "CR"])) else "0"
  })
  output$stat_en <- renderText({
    if (!is.null(tax_by_threat)) comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "EN"])) else "0"
  })
  output$stat_vu <- renderText({
    if (!is.null(tax_by_threat)) comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "VU"])) else "0"
  })
  output$stat_nt <- renderText({
    if (!is.null(tax_by_threat)) comma(sum(tax_by_threat$n_missing[tax_by_threat$threatStatus == "NT"])) else "0"
  })
  
  output$threat_coverage <- renderPlotly({
    req(tax_by_threat)
    df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT", "LC"))
    plot_ly(df, x = ~threatStatus, y = ~pct_coverage, type = "bar",
            marker = list(color = c("#ef4444", "#f97316", "#eab308", "#84cc16", "#22c55e"))) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Coverage %", range = c(0, 110)),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$threat_missing <- renderPlotly({
    req(tax_by_threat)
    df <- tax_by_threat |> filter(threatStatus %in% c("CR", "EN", "VU", "NT"))
    plot_ly(df, x = ~threatStatus, y = ~n_missing, type = "bar",
            marker = list(color = c("#ef4444", "#f97316", "#eab308", "#84cc16"))) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Missing Taxa"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"))
  })
  
  output$threat_table <- renderDT({
    req(priority_taxa)
    priority_taxa |> filter(threatStatus %in% c("CR", "EN")) |>
      select(any_of(c("scientificName", "threatStatus", "order", "family"))) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE), style = "bootstrap4", filter = "top")
  })
  
  # ===== PRIORITIES =====
  output$stat_zero <- renderText({
    if (!is.null(priority_zero)) comma(nrow(priority_zero)) else "0"
  })
  output$stat_stale <- renderText({
    if (!is.null(priority_stale)) comma(nrow(priority_stale)) else "0"
  })
  output$stat_taxa <- renderText({
    if (!is.null(priority_taxa)) comma(sum(priority_taxa$threatStatus %in% c("CR", "EN"))) else "0"
  })
  
  output$action_table <- renderUI({
    n_zero <- if (!is.null(priority_zero)) nrow(priority_zero) else 0
    n_stale <- if (!is.null(priority_stale)) nrow(priority_stale) else 0
    n_taxa <- if (!is.null(priority_taxa)) sum(priority_taxa$threatStatus %in% c("CR", "EN")) else 0
    
    HTML(paste0('
      <table class="action-table">
        <tr><td><span class="priority-badge priority-high">HIGH</span></td>
            <td>Survey zero-coverage grid cells</td>
            <td style="text-align:right;color:#667eea;">', comma(n_zero), ' cells</td></tr>
        <tr><td><span class="priority-badge priority-high">HIGH</span></td>
            <td>Monitor CR and EN species lacking GBIF records</td>
            <td style="text-align:right;color:#667eea;">', comma(n_taxa), ' species</td></tr>
        <tr><td><span class="priority-badge priority-medium">MEDIUM</span></td>
            <td>Re-survey cells with >5 years since last observation</td>
            <td style="text-align:right;color:#667eea;">', comma(n_stale), ' cells</td></tr>
        <tr><td><span class="priority-badge priority-low">LOW</span></td>
            <td>Expand citizen science programs to low-coverage regions</td>
            <td style="text-align:right;color:#a1a1aa;">Northern Sweden</td></tr>
      </table>'))
  })
  
  output$zero_table <- renderDT({
    req(priority_zero)
    datatable(priority_zero |> slice_head(n = 100), options = list(pageLength = 8, scrollX = TRUE), style = "bootstrap4")
  })
  
  output$stale_table <- renderDT({
    req(priority_stale)
    datatable(priority_stale |> mutate(years = round(staleness_months/12, 1)) |>
                select(any_of(c("eeacellcode", "years"))) |> slice_head(n = 100),
              options = list(pageLength = 8, scrollX = TRUE), style = "bootstrap4")
  })
  
  # ===== EXPLORER =====
  output$explorer_table <- renderDT({
    df <- safe_get(input$explorer_ds)
    if (is.null(df)) return(datatable(tibble(Message = "Not available")))
    datatable(df |> slice_head(n = 500), options = list(pageLength = 15, scrollX = TRUE),
              style = "bootstrap4", filter = "top")
  })
}

shinyApp(ui, server)
