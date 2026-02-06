# app.R
# ==============================================================================
# GBIF Sweden Gap Analysis - Interactive Dashboard
# ==============================================================================
# 
# This Shiny app provides interactive exploration of biodiversity data gaps
# across spatial, temporal, and taxonomic dimensions.
#
# Data source: Output from scripts/12_prepare_shiny_data.R
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
  "shiny_data.rds"
)

data_path <- NULL
for (p in data_paths) {
  if (file.exists(p)) {
    data_path <- p
    break
  }
}

if (is.null(data_path)) {
  stop("Could not find shiny_data.rds. Run scripts/12_prepare_shiny_data.R first.")
}

# Load the data bundle
app_data <- readRDS(data_path)

# Extract commonly used datasets
dashboard <- app_data$dashboard
grid_10km <- app_data$grid_10km
spatial_gaps <- app_data$spatial_gaps
time_summary <- app_data$time_summary
order_5yr <- app_data$order_5yr
top_orders <- app_data$top_orders

# Get available basis of record types
basis_types <- if (!is.null(spatial_gaps)) {
  unique(spatial_gaps$basisofrecord)
} else {
  c("all")
}

# Get available orders
order_choices <- if (!is.null(top_orders)) {
  top_orders$order
} else {
  c()
}

# Current year
current_year <- year(Sys.Date())

# Color palettes
threat_colors <- c("CR" = "#D32F2F", "EN" = "#E64A19", "VU" = "#F57C00", 
                   "NT" = "#FBC02D", "LC" = "#8BC34A", "DD" = "#78909C")

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "GBIF Sweden Gap Analysis",
    titleWidth = 280
  ),
  
 # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Spatial Gaps", tabName = "spatial", icon = icon("map")),
      menuItem("Temporal Gaps", tabName = "temporal", icon = icon("clock")),
      menuItem("Taxonomic Gaps", tabName = "taxonomic", icon = icon("leaf")),
      menuItem("Priority Actions", tabName = "priorities", icon = icon("exclamation-triangle")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    ),
    hr(),
    
    # Global filters
    h4("Filters", style = "padding-left: 15px; color: #fff;"),
    
    pickerInput(
      "basis_filter",
      "Basis of Record:",
      choices = basis_types,
      selected = "all",
      multiple = FALSE
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'temporal' || input.tabs == 'taxonomic'",
      sliderInput(
        "year_range",
        "Year Range:",
        min = 1900,
        max = current_year,
        value = c(1970, current_year),
        step = 1,
        sep = ""
      )
    ),
    
    hr(),
    div(
      style = "padding: 15px; color: #aaa; font-size: 11px;",
      HTML(paste0(
        "Data updated: ", 
        if (!is.null(app_data$metadata)) format(app_data$metadata$created_at, "%Y-%m-%d") else "Unknown",
        "<br>",
        "Datasets: ", 
        if (!is.null(app_data$metadata)) app_data$metadata$n_datasets else "?"
      ))
    )
  ),
  
  # Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-radius: 5px; }
        .info-box { border-radius: 5px; }
        .small-box { border-radius: 5px; }
        .nav-tabs-custom>.tab-content { padding: 15px; }
        .dataTables_wrapper { font-size: 13px; }
      "))
    ),
    
    tabItems(
      # =====================================================================
      # Overview Tab
      # =====================================================================
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("vb_spatial", width = 3),
          valueBoxOutput("vb_temporal", width = 3),
          valueBoxOutput("vb_taxonomic", width = 3),
          valueBoxOutput("vb_priority", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Coverage Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("overview_coverage_plot", height = "350px")
          ),
          box(
            title = "Data by Evidence Type",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("overview_basis_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Quick Stats",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            tableOutput("overview_stats_table")
          )
        )
      ),
      
      # =====================================================================
      # Spatial Tab
      # =====================================================================
      tabItem(
        tabName = "spatial",
        
        fluidRow(
          box(
            title = "Geographic Coverage",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            leafletOutput("spatial_map", height = "600px")
          ),
          box(
            title = "Map Options",
            status = "info",
            width = 4,
            radioButtons(
              "map_variable",
              "Display Variable:",
              choices = c(
                "Occurrences (log)" = "log_occ",
                "Coverage Category" = "category",
                "Zero Coverage" = "zero"
              ),
              selected = "log_occ"
            ),
            hr(),
            h4("Coverage Statistics"),
            tableOutput("spatial_stats_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Zero Coverage by Basis of Record",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("spatial_zero_plot", height = "300px")
          ),
          box(
            title = "Occurrence Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("spatial_dist_plot", height = "300px")
          )
        )
      ),
      
      # =====================================================================
      # Temporal Tab
      # =====================================================================
      tabItem(
        tabName = "temporal",
        
        fluidRow(
          box(
            title = "Historical Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("temporal_trend_plot", height = "350px")
          ),
          box(
            title = "Seasonal Pattern",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("temporal_seasonal_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Data Mobilization by Order (5-Year Periods)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                width = 4,
                pickerInput(
                  "order_select",
                  "Select Orders:",
                  choices = order_choices,
                  selected = head(order_choices, 6),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)
                )
              )
            ),
            plotlyOutput("order_mobilization_plot", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "Year × Month Heatmap",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("temporal_heatmap", height = "400px")
          )
        )
      ),
      
      # =====================================================================
      # Taxonomic Tab
      # =====================================================================
      tabItem(
        tabName = "taxonomic",
        
        fluidRow(
          box(
            title = "Coverage by Threat Status",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("tax_threat_plot", height = "350px")
          ),
          box(
            title = "Coverage by Taxonomic Rank",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("tax_rank_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Mobilization Change by Order (Recent vs Historical)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("tax_order_change_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Coverage by Order",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("tax_order_coverage_plot", height = "400px")
          ),
          box(
            title = "Coverage by Family (Top 20)",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("tax_family_plot", height = "400px")
          )
        )
      ),
      
      # =====================================================================
      # Priorities Tab
      # =====================================================================
      tabItem(
        tabName = "priorities",
        
        fluidRow(
          infoBoxOutput("ib_zero_cells", width = 4),
          infoBoxOutput("ib_stale_cells", width = 4),
          infoBoxOutput("ib_priority_taxa", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Priority Zero Coverage Cells",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            DTOutput("priority_zero_table")
          ),
          box(
            title = "Priority Stale Cells (>5 Years)",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("priority_stale_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Priority Taxa (CR & EN)",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DTOutput("priority_taxa_table")
          )
        )
      ),
      
      # =====================================================================
      # Data Explorer Tab
      # =====================================================================
      tabItem(
        tabName = "explorer",
        
        fluidRow(
          box(
            title = "Data Explorer",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              "explorer_dataset",
              "Select Dataset:",
              choices = c(
                "Spatial Gaps" = "spatial_gaps",
                "Time Summary" = "time_summary",
                "Order 5-Year Summary" = "order_5yr",
                "Taxonomic by Threat" = "tax_by_threat",
                "Taxonomic by Order" = "tax_by_order",
                "Priority Taxa" = "priority_taxa",
                "Cube Totals" = "cube_totals"
              )
            ),
            DTOutput("explorer_table")
          )
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {
  
 # ===========================================================================
  # Reactive Data
  # ===========================================================================
  
  # Filtered spatial data
  spatial_filtered <- reactive({
    req(spatial_gaps)
    spatial_gaps |>
      filter(basisofrecord == input$basis_filter)
  })
  
  # Filtered time data
  time_filtered <- reactive({
    req(time_summary)
    time_summary |>
      filter(
        basisofrecord == input$basis_filter,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
  # Filtered order data
  order_filtered <- reactive({
    req(order_5yr)
    order_5yr |>
      filter(
        order %in% input$order_select,
        period_start >= input$year_range[1],
        period_start <= input$year_range[2]
      )
  })
  
  # ===========================================================================
  # Overview Tab
  # ===========================================================================
  
  output$vb_spatial <- renderValueBox({
    val <- if (!is.null(dashboard)) dashboard$cells_10km_pct_coverage else "?"
    valueBox(
      paste0(val, "%"),
      "Spatial Coverage (10km)",
      icon = icon("map"),
      color = if (!is.null(val) && val >= 75) "green" else "yellow"
    )
  })
  
  output$vb_temporal <- renderValueBox({
    val <- if (!is.null(dashboard)) dashboard$year_range else "?"
    valueBox(
      paste0(val, " yrs"),
      "Temporal Range",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$vb_taxonomic <- renderValueBox({
    val <- if (!is.null(dashboard)) dashboard$taxa_pct_coverage else "?"
    valueBox(
      paste0(val, "%"),
      "Taxonomic Coverage",
      icon = icon("leaf"),
      color = if (!is.null(val) && val >= 75) "green" else "yellow"
    )
  })
  
  output$vb_priority <- renderValueBox({
    val <- if (!is.null(dashboard)) comma(dashboard$n_priority_taxa) else "?"
    valueBox(
      val,
      "Priority Taxa",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$overview_coverage_plot <- renderPlotly({
    req(dashboard)
    
    df <- tibble(
      Dimension = c("10km Spatial", "50km Spatial", "Taxonomic"),
      Coverage = c(
        dashboard$cells_10km_pct_coverage,
        dashboard$cells_50km_pct_coverage,
        dashboard$taxa_pct_coverage
      )
    )
    
    p <- ggplot(df, aes(x = Dimension, y = Coverage, fill = Dimension)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(round(Coverage, 1), "%")), vjust = -0.3, size = 5) +
      scale_fill_viridis_d(option = "viridis", begin = 0.2, end = 0.8) +
      ylim(0, 110) +
      labs(x = NULL, y = "Coverage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y")) |> layout(showlegend = FALSE)
  })
  
  output$overview_basis_plot <- renderPlotly({
    req(app_data$cube_totals)
    
    df <- app_data$cube_totals |>
      filter(grid == "grid10km") |>
      arrange(desc(total_occurrences))
    
    p <- ggplot(df, aes(x = reorder(basisOfRecord, total_occurrences), 
                         y = total_occurrences, fill = total_occurrences)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "plasma") +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Total Occurrences") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$overview_stats_table <- renderTable({
    req(dashboard)
    tibble(
      Metric = c(
        "10km Grid Cells (with data / total)",
        "50km Grid Cells (with data / total)",
        "Year Range",
        "Median Staleness",
        "Taxa (in GBIF / in reference)",
        "Priority Taxa"
      ),
      Value = c(
        glue("{comma(dashboard$cells_10km_with_data)} / {comma(dashboard$cells_10km_total)}"),
        glue("{comma(dashboard$cells_50km_with_data)} / {comma(dashboard$cells_50km_total)}"),
        glue("{dashboard$year_min} – {dashboard$year_max} ({dashboard$year_range} years)"),
        glue("{dashboard$median_staleness_months_10km} months"),
        glue("{comma(dashboard$taxa_in_gbif)} / {comma(dashboard$taxa_in_reference)}"),
        comma(dashboard$n_priority_taxa)
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # ===========================================================================
  # Spatial Tab
  # ===========================================================================
  
  output$spatial_map <- renderLeaflet({
    req(grid_10km, spatial_filtered())
    
    # Join spatial data to grid
    map_data <- grid_10km |>
      left_join(
        spatial_filtered() |> select(eeacellcode, occurrences, gap_zero),
        by = "eeacellcode"
      ) |>
      mutate(
        log_occ = ifelse(is.na(occurrences) | occurrences == 0, 0, log10(occurrences + 1)),
        category = case_when(
          is.na(occurrences) | occurrences == 0 ~ "Zero",
          occurrences <= 10 ~ "Very Low",
          occurrences <= 100 ~ "Low",
          occurrences <= 1000 ~ "Medium",
          TRUE ~ "High"
        )
      )
    
    # Color palette based on selection
    if (input$map_variable == "log_occ") {
      pal <- colorNumeric(viridis(100), domain = c(0, max(map_data$log_occ, na.rm = TRUE)))
      map_data$color_var <- map_data$log_occ
      legend_title <- "log10(occ)"
    } else if (input$map_variable == "category") {
      pal <- colorFactor(viridis(5), domain = c("Zero", "Very Low", "Low", "Medium", "High"))
      map_data$color_var <- map_data$category
      legend_title <- "Category"
    } else {
      pal <- colorFactor(c("#E63946", "#2A9D8F"), domain = c(TRUE, FALSE))
      map_data$color_var <- map_data$gap_zero
      legend_title <- "Zero Coverage"
    }
    
    leaflet(map_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(color_var),
        fillOpacity = 0.7,
        weight = 0.5,
        color = "white",
        popup = ~paste0(
          "<strong>Cell:</strong> ", eeacellcode, "<br>",
          "<strong>Occurrences:</strong> ", comma(occurrences), "<br>",
          "<strong>Category:</strong> ", category
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~color_var,
        title = legend_title
      )
  })
  
  output$spatial_stats_table <- renderTable({
    req(spatial_filtered())
    
    df <- spatial_filtered()
    tibble(
      Metric = c("Total Cells", "Cells with Data", "Coverage %", "Total Occurrences"),
      Value = c(
        comma(nrow(df)),
        comma(sum(df$occurrences > 0, na.rm = TRUE)),
        paste0(round(100 * mean(df$occurrences > 0, na.rm = TRUE), 1), "%"),
        comma(sum(df$occurrences, na.rm = TRUE))
      )
    )
  }, striped = TRUE)
  
  output$spatial_zero_plot <- renderPlotly({
    req(app_data$spatial_overview)
    
    df <- app_data$spatial_overview |>
      filter(grid == "grid10km", basisofrecord != "all")
    
    # Find the pct_zero column (might be named differently)
    zero_col <- names(df)[grepl("pct.*zero|zero.*pct", names(df), ignore.case = TRUE)][1]
    if (is.null(zero_col) || is.na(zero_col)) zero_col <- "pct_zero"
    
    if (!zero_col %in% names(df)) {
      return(plotly_empty() |> layout(title = "No zero coverage data available"))
    }
    
    df <- df |> arrange(.data[[zero_col]])
    
    p <- ggplot(df, aes(x = reorder(basisofrecord, -.data[[zero_col]]), 
                         y = .data[[zero_col]], 
                         fill = .data[[zero_col]])) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "rocket", direction = -1) +
      labs(x = NULL, y = "Zero Coverage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$spatial_dist_plot <- renderPlotly({
    req(spatial_filtered())
    
    df <- spatial_filtered() |> filter(occurrences > 0)
    
    p <- ggplot(df, aes(x = occurrences)) +
      geom_histogram(bins = 50, fill = viridis(1, begin = 0.4), alpha = 0.8) +
      scale_x_log10(labels = comma) +
      labs(x = "Occurrences (log scale)", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===========================================================================
  # Temporal Tab
  # ===========================================================================
  
  output$temporal_trend_plot <- renderPlotly({
    req(time_filtered())
    
    df <- time_filtered() |>
      group_by(year) |>
      summarise(occurrences = sum(occurrences, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(df, aes(x = year, y = occurrences)) +
      geom_area(fill = viridis(1, begin = 0.4), alpha = 0.3) +
      geom_line(color = viridis(1, begin = 0.4), linewidth = 1) +
      scale_y_continuous(labels = comma) +
      labs(x = "Year", y = "Occurrences") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$temporal_seasonal_plot <- renderPlotly({
    req(time_filtered())
    
    df <- time_filtered() |>
      group_by(month) |>
      summarise(occurrences = sum(occurrences, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(df, aes(x = factor(month), y = occurrences, fill = occurrences)) +
      geom_col() +
      scale_fill_viridis_c(option = "plasma") +
      scale_x_discrete(labels = month.abb) +
      labs(x = "Month", y = "Occurrences") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$order_mobilization_plot <- renderPlotly({
    req(order_filtered(), nrow(order_filtered()) > 0)
    
    p <- ggplot(order_filtered(), aes(x = period, y = occurrences, fill = order)) +
      geom_col(position = "dodge") +
      scale_fill_viridis_d(option = "turbo") +
      scale_y_continuous(labels = comma) +
      labs(x = "5-Year Period", y = "Occurrences", fill = "Order") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$temporal_heatmap <- renderPlotly({
    req(time_filtered())
    
    df <- time_filtered() |>
      group_by(year, month) |>
      summarise(occurrences = sum(occurrences, na.rm = TRUE), .groups = "drop") |>
      mutate(log_occ = log10(occurrences + 1))
    
    p <- ggplot(df, aes(x = factor(month), y = year, fill = log_occ)) +
      geom_tile() +
      scale_fill_viridis(option = "inferno") +
      scale_x_discrete(labels = month.abb) +
      labs(x = "Month", y = "Year", fill = "log10(occ)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===========================================================================
  # Taxonomic Tab
  # ===========================================================================
  
  output$tax_threat_plot <- renderPlotly({
    req(app_data$tax_by_threat)
    
    df <- app_data$tax_by_threat |>
      filter(threatStatus %in% c("CR", "EN", "VU", "NT", "LC")) |>
      mutate(threatStatus = factor(threatStatus, levels = c("CR", "EN", "VU", "NT", "LC")))
    
    p <- ggplot(df, aes(x = threatStatus, y = pct_coverage, fill = threatStatus)) +
      geom_col() +
      geom_text(aes(label = paste0(round(pct_coverage, 1), "%")), vjust = -0.3, size = 4) +
      scale_fill_manual(values = threat_colors) +
      ylim(0, 110) +
      labs(x = "Threat Status", y = "Coverage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$tax_rank_plot <- renderPlotly({
    req(app_data$tax_by_rank)
    
    df <- app_data$tax_by_rank |>
      filter(!is.na(taxonRank), n_ref_total >= 10) |>
      arrange(desc(n_ref_total)) |>
      slice_head(n = 15)
    
    p <- ggplot(df, aes(x = reorder(taxonRank, pct_coverage), y = pct_coverage, fill = pct_coverage)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "viridis") +
      labs(x = NULL, y = "Coverage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$tax_order_change_plot <- renderPlotly({
    req(app_data$order_time)
    
    recent_cutoff <- current_year - 10
    historical_cutoff <- current_year - 20
    
    df <- app_data$order_time |>
      filter(order %in% head(top_orders$order, 12)) |>
      mutate(
        era = case_when(
          year >= recent_cutoff ~ "Recent",
          year >= historical_cutoff ~ "Historical",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(era)) |>
      group_by(order, era) |>
      summarise(occurrences = sum(occurrences, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = era, values_from = occurrences) |>
      mutate(
        pct_change = 100 * (Recent - Historical) / Historical,
        direction = ifelse(pct_change >= 0, "Increased", "Decreased")
      ) |>
      arrange(desc(pct_change))
    
    p <- ggplot(df, aes(x = reorder(order, pct_change), y = pct_change, fill = direction)) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      scale_fill_viridis_d(option = "viridis", begin = 0.3, end = 0.7) +
      labs(x = NULL, y = "% Change", fill = NULL) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$tax_order_coverage_plot <- renderPlotly({
    req(app_data$tax_by_order)
    
    df <- app_data$tax_by_order |>
      filter(!is.na(order), order != "", n_ref_total >= 5) |>
      arrange(desc(n_ref_total)) |>
      slice_head(n = 15)
    
    p <- ggplot(df, aes(x = reorder(order, pct_coverage), y = pct_coverage, fill = pct_coverage)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = NULL, y = "Coverage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$tax_family_plot <- renderPlotly({
    req(app_data$tax_by_family)
    
    df <- app_data$tax_by_family |>
      filter(!is.na(family), family != "", n_ref_total >= 5) |>
      arrange(desc(n_ref_total)) |>
      slice_head(n = 20)
    
    p <- ggplot(df, aes(x = reorder(family, pct_coverage), y = pct_coverage, fill = pct_coverage)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "mako") +
      labs(x = NULL, y = "Coverage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ===========================================================================
  # Priorities Tab
  # ===========================================================================
  
  output$ib_zero_cells <- renderInfoBox({
    n <- if (!is.null(app_data$priority_zero)) nrow(app_data$priority_zero) else 0
    infoBox(
      "Zero Coverage Cells",
      comma(n),
      icon = icon("map-marker-alt"),
      color = "red"
    )
  })
  
  output$ib_stale_cells <- renderInfoBox({
    n <- if (!is.null(app_data$priority_stale)) nrow(app_data$priority_stale) else 0
    infoBox(
      "Stale Cells (>5 yrs)",
      comma(n),
      icon = icon("clock"),
      color = "orange"
    )
  })
  
  output$ib_priority_taxa <- renderInfoBox({
    n <- if (!is.null(app_data$priority_taxa)) {
      sum(app_data$priority_taxa$threatStatus %in% c("CR", "EN"), na.rm = TRUE)
    } else 0
    infoBox(
      "Priority Taxa (CR/EN)",
      comma(n),
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output$priority_zero_table <- renderDT({
    req(app_data$priority_zero)
    app_data$priority_zero |>
      slice_head(n = 100) |>
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  output$priority_stale_table <- renderDT({
    req(app_data$priority_stale)
    app_data$priority_stale |>
      mutate(years_stale = round(staleness_months / 12, 1)) |>
      arrange(desc(staleness_months)) |>
      select(eeacellcode, last_ym, years_stale, total_occurrences) |>
      slice_head(n = 100) |>
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  output$priority_taxa_table <- renderDT({
    req(app_data$priority_taxa)
    app_data$priority_taxa |>
      filter(threatStatus %in% c("CR", "EN")) |>
      arrange(threatStatus, scientificName) |>
      select(scientificName, threatStatus, taxonRank, family, order) |>
      slice_head(n = 200) |>
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        filter = "top"
      )
  })
  
  # ===========================================================================
  # Data Explorer Tab
  # ===========================================================================
  
  output$explorer_table <- renderDT({
    req(input$explorer_dataset)
    
    df <- switch(
      input$explorer_dataset,
      "spatial_gaps" = app_data$spatial_gaps,
      "time_summary" = app_data$time_summary,
      "order_5yr" = app_data$order_5yr,
      "tax_by_threat" = app_data$tax_by_threat,
      "tax_by_order" = app_data$tax_by_order,
      "priority_taxa" = app_data$priority_taxa,
      "cube_totals" = app_data$cube_totals
    )
    
    if (is.null(df)) {
      return(datatable(tibble(Message = "Dataset not available")))
    }
    
    datatable(
      df |> slice_head(n = 1000),
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  })
}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui = ui, server = server)
