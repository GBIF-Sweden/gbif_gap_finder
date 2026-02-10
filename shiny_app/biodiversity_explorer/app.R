# =============================================================================
# SWEDISH BIODIVERSITY EXPLORER
# =============================================================================
#
# Interactive tool for exploring Swedish biodiversity data from GBIF.
# Designed for researchers, naturalists, and the general public.
#
# To run: shiny::runApp("shiny_app/explorer")
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
library(stringr)

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
grid_10km <- safe_get("grid_10km")
spatial_gaps <- safe_get("spatial_gaps_10km")
time_summary <- safe_get("time_summary_10km")
tax_match <- safe_get("taxonomic_match_summary")
tax_by_threat <- safe_get("tax_by_threat")
tax_by_kingdom <- safe_get("tax_by_kingdom")
tax_by_order <- safe_get("tax_by_order")
tax_by_family <- safe_get("tax_by_family")
priority_taxa <- safe_get("priority_taxa_missing") %||% safe_get("priority_taxa_all")
dashboard <- safe_get("dashboard")

current_year <- year(Sys.Date())

# Build species list for search
species_list <- if (!is.null(tax_match)) {
  tax_match |> filter(!is.na(scientificName)) |> arrange(scientificName) |> pull(scientificName) |> unique()
} else character(0)

# Threatened species list
threatened_species <- if (!is.null(tax_match)) {
  threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_dyntaxa"), names(tax_match))[1]
  if (!is.na(threat_col)) {
    tax_match |>
      filter(.data[[threat_col]] %in% c("CR", "EN", "VU", "NT")) |>
      mutate(threatStatus = .data[[threat_col]]) |>
      select(any_of(c("scientificName", "threatStatus", "kingdom", "phylum", "class", "order", "family", "matched_any")))
  } else NULL
} else NULL

# Get kingdoms for filter
kingdoms <- if (!is.null(tax_match) && "kingdom" %in% names(tax_match)) {
  sort(unique(tax_match$kingdom[!is.na(tax_match$kingdom) & tax_match$kingdom != ""]))
} else character(0)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .main-title {
        background: linear-gradient(135deg, #06b6d4 0%, #667eea 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      .nav-pills .nav-link.active {
        background: linear-gradient(135deg, #06b6d4 0%, #667eea 100%);
      }
    "))
  ),
  
  # Header
  div(class = "main-header",
      div(
        div(class = "main-title", "🦋 Swedish Biodiversity Explorer"),
        div(class = "main-subtitle", "Discover Sweden's incredible species diversity")
      ),
      div(class = "header-stats",
          div(span("Species: "), span(class = "header-stat-value", 
              if (!is.null(tax_match)) comma(nrow(tax_match)) else "?")),
          div(span("Occurrences: "), span(class = "header-stat-value",
              if (!is.null(dashboard)) comma(dashboard$total_occurrences[1]) else "?"))
      )
  ),
  
  # Main content
  div(style = "padding: 0 1rem;", class = "explorer",
      tabsetPanel(
        id = "main_tabs",
        type = "pills",
        
        # ===== DISCOVER TAB =====
        tabPanel(
          title = tagList(icon("search"), "Discover"),
          value = "discover",
          
          div(style = "padding: 1.5rem 0;",
              
              div(class = "search-box",
                  div(class = "search-title", "🔍 Find a Species"),
                  div(class = "search-subtitle", "Search among ", 
                      if (!is.null(tax_match)) comma(nrow(tax_match)) else "thousands of", " Swedish species"),
                  fluidRow(
                    column(8, offset = 2,
                           selectizeInput("species_search", NULL, choices = NULL,
                                          options = list(placeholder = "Type a species name...", maxOptions = 100)))
                  ),
                  div(style = "text-align: center; margin-top: 1rem;",
                      actionButton("random_species", "🎲 Surprise Me!", class = "random-btn"))
              ),
              
              uiOutput("species_info"),
              
              div(class = "card",
                  div(class = "card-title", icon("lightbulb"), "Did You Know?"),
                  uiOutput("fun_fact"))
          )
        ),
        
        # ===== THREATENED TAB =====
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Threatened Species"),
          value = "threatened",
          
          div(style = "padding: 1.5rem 0;",
              
              div(class = "stat-grid",
                  div(class = "stat-box",
                      div(class = "stat-value", style = "color: #ef4444;", textOutput("t_stat_cr", inline = TRUE)),
                      div(class = "stat-label", "Critically Endangered")),
                  div(class = "stat-box",
                      div(class = "stat-value", style = "color: #f97316;", textOutput("t_stat_en", inline = TRUE)),
                      div(class = "stat-label", "Endangered")),
                  div(class = "stat-box",
                      div(class = "stat-value", style = "color: #eab308;", textOutput("t_stat_vu", inline = TRUE)),
                      div(class = "stat-label", "Vulnerable")),
                  div(class = "stat-box",
                      div(class = "stat-value", style = "color: #84cc16;", textOutput("t_stat_nt", inline = TRUE)),
                      div(class = "stat-label", "Near Threatened"))
              ),
              
              div(class = "filter-section",
                  fluidRow(
                    column(3,
                           div(class = "filter-label", "Threat Status"),
                           pickerInput("threat_filter", NULL, choices = c("CR", "EN", "VU", "NT"),
                                       selected = c("CR", "EN"), multiple = TRUE)),
                    column(3,
                           div(class = "filter-label", "Kingdom"),
                           pickerInput("kingdom_filter", NULL, choices = kingdoms, selected = kingdoms,
                                       multiple = TRUE, options = list(`actions-box` = TRUE))),
                    column(3,
                           div(class = "filter-label", "GBIF Status"),
                           radioButtons("gbif_filter", NULL,
                                        choices = c("All" = "all", "In GBIF" = "yes", "Missing" = "no"),
                                        selected = "all", inline = TRUE)),
                    column(3,
                           div(class = "filter-label", "Search"),
                           textInput("threat_search", NULL, placeholder = "Filter by name..."))
                  )),
              
              div(class = "card",
                  div(class = "card-title", icon("list"), "Threatened Species List"),
                  DTOutput("threatened_table"))
          )
        ),
        
        # ===== EXPLORE MAP TAB =====
        tabPanel(
          title = tagList(icon("map"), "Explore Map"),
          value = "map",
          
          div(style = "padding: 1.5rem 0;",
              fluidRow(
                column(8,
                       div(class = "card",
                           div(class = "card-title", icon("globe-europe"), "Species Richness Across Sweden"),
                           leafletOutput("explore_map", height = "600px"))),
                column(4,
                       div(class = "card",
                           div(class = "card-title", icon("info-circle"), "About the Map"),
                           p(style = "color: #a1a1aa;", 
                             "This map shows the distribution of biodiversity records across Sweden. 
                              Brighter colors indicate more occurrence records."),
                           hr(style = "border-color: rgba(255,255,255,0.1);"),
                           div(class = "card-title", icon("chart-bar"), "Recording Hotspots"),
                           tableOutput("map_hotspots")),
                       div(class = "card",
                           div(class = "card-title", icon("calendar"), "Recording Over Time"),
                           plotlyOutput("map_temporal", height = "200px")))
              )
          )
        ),
        
        # ===== KINGDOMS TAB =====
        tabPanel(
          title = tagList(icon("sitemap"), "Kingdoms"),
          value = "kingdoms",
          
          div(style = "padding: 1.5rem 0;",
              
              div(class = "card",
                  div(class = "card-title", icon("chart-pie"), "Species by Kingdom"),
                  fluidRow(
                    column(6, plotlyOutput("kingdom_pie", height = "350px")),
                    column(6, plotlyOutput("kingdom_coverage", height = "350px"))
                  )),
              
              div(class = "card",
                  div(class = "card-title", icon("layer-group"), "Explore by Kingdom"),
                  uiOutput("kingdom_cards"))
          )
        ),
        
        # ===== PHENOLOGY TAB =====
        tabPanel(
          title = tagList(icon("calendar-alt"), "Seasonality"),
          value = "phenology",
          
          div(style = "padding: 1.5rem 0;",
              
              div(class = "card",
                  div(class = "card-title", icon("sun"), "When Are Species Observed?"),
                  p(style = "color: #a1a1aa; margin-bottom: 1rem;",
                    "Explore the seasonal patterns of biodiversity recording in Sweden."),
                  plotlyOutput("phenology_plot", height = "400px")),
              
              fluidRow(
                column(6, div(class = "card",
                              div(class = "card-title", icon("snowflake"), "Winter vs Summer"),
                              plotlyOutput("season_compare", height = "300px"))),
                column(6, div(class = "card",
                              div(class = "card-title", icon("chart-line"), "Recording Trends"),
                              plotlyOutput("decade_trend", height = "300px")))
              )
          )
        )
      ),
      
      # Footer
      div(class = "metadata-footer",
          HTML(paste0(
            "Data from GBIF Sweden · Updated: ",
            if (!is.null(app_data$metadata)) format(app_data$metadata$created_at, "%Y-%m-%d") else "Unknown",
            " · Explore responsibly 🌿"
          ))
      )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Update species search choices
 updateSelectizeInput(session, "species_search", choices = species_list, server = TRUE)
  
  # Random species button
  observeEvent(input$random_species, {
    if (length(species_list) > 0) {
      random_sp <- sample(species_list, 1)
      updateSelectizeInput(session, "species_search", selected = random_sp)
    }
  })
  
  # Species info display
  output$species_info <- renderUI({
    req(input$species_search, input$species_search != "")
    req(tax_match)
    
    sp <- tax_match |> filter(scientificName == input$species_search)
    if (nrow(sp) == 0) return(NULL)
    sp <- sp[1, ]
    
    threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_dyntaxa"), names(sp))[1]
    threat <- if (!is.na(threat_col) && !is.na(sp[[threat_col]])) sp[[threat_col]] else NA
    in_gbif <- if ("matched_any" %in% names(sp)) sp$matched_any else NA
    
    div(class = "species-card",
        div(class = "species-name", sp$scientificName),
        div(class = "species-taxonomy",
            paste(na.omit(c(sp$kingdom, sp$phylum, sp$class, sp$order, sp$family)), collapse = " → ")),
        div(style = "display: flex; gap: 0.5rem; flex-wrap: wrap;",
            if (!is.na(threat)) div(class = paste0("threat-badge threat-", threat), threat) else NULL,
            if (!is.na(in_gbif)) div(class = paste0("gbif-badge gbif-", ifelse(in_gbif, "yes", "no")),
                                      ifelse(in_gbif, "✓ In GBIF", "✗ Not in GBIF")) else NULL
        )
    )
  })
  
  # Fun facts
  fun_facts <- c(
    "Sweden has over <strong>60,000</strong> known species, from tiny soil mites to majestic moose.",
    "The <strong>Arctic Fox</strong> is one of Sweden's most endangered mammals, with only about 200 individuals remaining.",
    "Sweden's forests cover about <strong>70%</strong> of the country's land area.",
    "The <strong>Eurasian Lynx</strong> is Sweden's only wild cat species.",
    "Swedish waters are home to over <strong>100 species</strong> of fish.",
    "The <strong>White-tailed Eagle</strong> has made a remarkable comeback in Sweden after near extinction.",
    "Sweden has approximately <strong>2,000 species</strong> of lichens.",
    "The <strong>Brown Bear</strong> population in Sweden has grown to over 2,800 individuals."
  )
  
  output$fun_fact <- renderUI({
    div(class = "fun-fact", HTML(sample(fun_facts, 1)))
  })
  
  # ===== THREATENED TAB =====
  output$t_stat_cr <- renderText({
    if (!is.null(threatened_species)) comma(sum(threatened_species$threatStatus == "CR")) else "0"
  })
  output$t_stat_en <- renderText({
    if (!is.null(threatened_species)) comma(sum(threatened_species$threatStatus == "EN")) else "0"
  })
  output$t_stat_vu <- renderText({
    if (!is.null(threatened_species)) comma(sum(threatened_species$threatStatus == "VU")) else "0"
  })
  output$t_stat_nt <- renderText({
    if (!is.null(threatened_species)) comma(sum(threatened_species$threatStatus == "NT")) else "0"
  })
  
  threatened_filtered <- reactive({
    req(threatened_species)
    df <- threatened_species |> filter(threatStatus %in% input$threat_filter)
    if (length(input$kingdom_filter) > 0 && "kingdom" %in% names(df)) {
      df <- df |> filter(kingdom %in% input$kingdom_filter)
    }
    if (input$gbif_filter == "yes") df <- df |> filter(matched_any == TRUE)
    if (input$gbif_filter == "no") df <- df |> filter(matched_any == FALSE)
    if (nchar(input$threat_search) > 0) {
      df <- df |> filter(str_detect(tolower(scientificName), tolower(input$threat_search)))
    }
    df
  })
  
  output$threatened_table <- renderDT({
    req(threatened_filtered())
    threatened_filtered() |>
      select(any_of(c("scientificName", "threatStatus", "kingdom", "order", "family", "matched_any"))) |>
      rename_with(~c("Species", "Status", "Kingdom", "Order", "Family", "In GBIF")[1:length(.)]) |>
      datatable(options = list(pageLength = 15, scrollX = TRUE), style = "bootstrap4", filter = "top")
  })
  
  # ===== MAP TAB =====
  output$explore_map <- renderLeaflet({
    req(grid_10km, spatial_gaps)
    sf_data <- spatial_gaps |> filter(basisofrecord == "all")
    map_data <- grid_10km |> left_join(sf_data, by = "eeacellcode") |>
      mutate(log_occ = ifelse(is.na(occurrences), 0, log10(occurrences + 1)))
    
    pal <- colorNumeric("viridis", map_data$log_occ, na.color = "#333")
    leaflet(map_data) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      addPolygons(fillColor = ~pal(log_occ), fillOpacity = 0.7, weight = 0.3, color = "#444",
                  popup = ~paste0("<strong>", eeacellcode, "</strong><br>", comma(occurrences), " occurrences")) |>
      addLegend("bottomright", pal = pal, values = ~log_occ, title = "Records (log)")
  })
  
  output$map_hotspots <- renderTable({
    req(spatial_gaps)
    spatial_gaps |> filter(basisofrecord == "all") |> arrange(desc(occurrences)) |> slice_head(n = 5) |>
      select(eeacellcode, occurrences) |> mutate(occurrences = comma(occurrences)) |>
      rename(Cell = eeacellcode, Records = occurrences)
  }, width = "100%")
  
  output$map_temporal <- renderPlotly({
    req(time_summary)
    df <- time_summary |> filter(basisofrecord == "all", year >= 2000) |>
      group_by(year) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines+markers",
            line = list(color = "#06b6d4"), marker = list(color = "#06b6d4")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"), margin = list(l = 40, r = 10, t = 10, b = 30))
  })
  
  # ===== KINGDOMS TAB =====
  output$kingdom_pie <- renderPlotly({
    req(tax_by_kingdom)
    plot_ly(tax_by_kingdom, labels = ~kingdom, values = ~n_ref_total, type = "pie",
            marker = list(colors = viridis(nrow(tax_by_kingdom))), textinfo = "label+percent") |>
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             font = list(color = "#a1a1aa"), legend = list(orientation = "h", y = -0.1))
  })
  
  output$kingdom_coverage <- renderPlotly({
    req(tax_by_kingdom)
    plot_ly(tax_by_kingdom, y = ~reorder(kingdom, pct_coverage), x = ~pct_coverage,
            type = "bar", orientation = "h",
            marker = list(color = ~pct_coverage, colorscale = "Viridis")) |>
      layout(xaxis = list(title = "GBIF Coverage %", range = c(0, 100)), yaxis = list(title = ""),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "#a1a1aa"))
  })
  
  output$kingdom_cards <- renderUI({
    req(tax_by_kingdom)
    icons <- c(Animalia = "🦋", Plantae = "🌿", Fungi = "🍄", Chromista = "🦠", Protozoa = "🔬", Bacteria = "🧫")
    cards <- lapply(1:min(6, nrow(tax_by_kingdom)), function(i) {
      k <- tax_by_kingdom[i, ]
      column(2, div(class = "stat-box", style = "text-align: center;",
                    div(class = "kingdom-icon", icons[k$kingdom] %||% "🌍"),
                    div(style = "font-weight: 600; margin-bottom: 0.5rem;", k$kingdom),
                    div(style = "color: #06b6d4; font-family: 'JetBrains Mono';", comma(k$n_ref_total), " species"),
                    div(style = "color: #a1a1aa; font-size: 0.8rem;", k$pct_coverage, "% in GBIF")))
    })
    do.call(fluidRow, cards)
  })
  
  # ===== PHENOLOGY TAB =====
  output$phenology_plot <- renderPlotly({
    req(time_summary)
    df <- time_summary |> filter(basisofrecord == "all", year >= 1990) |>
      group_by(month) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    plot_ly(df, x = ~month, y = ~occ, type = "bar",
            marker = list(color = ~occ, colorscale = list(c(0, "#1e3a5f"), c(1, "#06b6d4")))) |>
      layout(xaxis = list(title = "", ticktext = month.name, tickvals = 1:12, tickangle = -45),
             yaxis = list(title = "Total Occurrences"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "#a1a1aa"))
  })
  
  output$season_compare <- renderPlotly({
    req(time_summary)
    df <- time_summary |> filter(basisofrecord == "all") |>
      mutate(season = case_when(month %in% c(12, 1, 2) ~ "Winter", month %in% 3:5 ~ "Spring",
                                 month %in% 6:8 ~ "Summer", month %in% 9:11 ~ "Autumn")) |>
      group_by(season) |> summarise(occ = sum(occurrences, na.rm = TRUE)) |>
      mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))
    colors <- c(Spring = "#84cc16", Summer = "#eab308", Autumn = "#f97316", Winter = "#06b6d4")
    plot_ly(df, x = ~season, y = ~occ, type = "bar", marker = list(color = colors[as.character(df$season)])) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Occurrences"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "#a1a1aa"))
  })
  
  output$decade_trend <- renderPlotly({
    req(time_summary)
    df <- time_summary |> filter(basisofrecord == "all", year >= 1950) |>
      mutate(decade = paste0(floor(year / 10) * 10, "s")) |>
      group_by(decade) |> summarise(occ = sum(occurrences, na.rm = TRUE))
    plot_ly(df, x = ~decade, y = ~occ, type = "bar", marker = list(color = "#667eea")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Occurrences"),
             paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(color = "#a1a1aa"))
  })
}

shinyApp(ui, server)
