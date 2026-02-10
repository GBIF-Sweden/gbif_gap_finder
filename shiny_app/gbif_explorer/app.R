# shiny_app/gbif_explorer/app.R
# ==============================================================================
# GBIF Explorer — Species & Biodiversity Explorer
# ==============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(data.table)
library(plotly)
library(leaflet)
library(DT)
library(scales)
library(stringr)
library(sf)

# =============================================================================
# DATA LOADING
# =============================================================================

data_path <- "data/shiny_data.rds"
if (!file.exists(data_path)) stop("shiny_data.rds not found. Run scripts/12_prepare_explorer_app_data.R")

message("Loading explorer data from: ", normalizePath(data_path))
app_data <- readRDS(data_path)

safe_get <- function(name) if (name %in% names(app_data)) app_data[[name]] else NULL

species_lookup     <- safe_get("species_lookup")
grid_10km          <- safe_get("grid_10km")
tax_by_threat      <- safe_get("tax_by_threat")
tax_by_kingdom     <- safe_get("tax_by_kingdom")
tax_by_phylum      <- safe_get("tax_by_phylum")
tax_by_class       <- safe_get("tax_by_class")
tax_by_order       <- safe_get("tax_by_order")
tax_by_family      <- safe_get("tax_by_family")
cell_recency       <- safe_get("cell_recency_10km")
spatial_gaps       <- safe_get("spatial_gaps_10km")
metadata           <- safe_get("metadata")
derived_path       <- safe_get("derived_data_path")
file_index_cell    <- safe_get("file_index_cell")
file_index_time    <- safe_get("file_index_time")
cell_species_index <- safe_get("cell_species_index")

# Convert cell_species_index to data.table for fast lookups
if (!is.null(cell_species_index) && !is.data.table(cell_species_index)) {
  cell_species_index <- as.data.table(cell_species_index)
  setkey(cell_species_index, eeacellcode)
}

# Species choices — clean vector for selectize
species_choices <- if (!is.null(species_lookup)) {
  # Deduplicate lookup by species name (keep first = highest occurrences)
  species_lookup <- species_lookup[!duplicated(species_lookup$species), ]
  sp <- unique(na.omit(species_lookup$species))
  sp[nchar(sp) > 0]
} else character(0)

country_name <- tryCatch(yaml::read_yaml("../../config.yml")$country$name, error = function(e) "")

# Palette
pal <- list(
  sage  = "#6b8f71", sage2 = "#8ab090",
  slate = "#5c7a99", slate2 = "#7d9ab5",
  sand  = "#c4a882", sand2  = "#d4c0a0",
  coral = "#c47a6c", coral2 = "#d9a090",
  plum  = "#8b6d8f",
  text  = "#2d2d2d", muted = "#6b6b6b"
)

plotly_layout <- function(p, ...) {
  args <- list(...)
  defaults <- list(gridcolor = "#e8e7e1", zerolinecolor = "#e0dfda")
  if (!is.null(args$xaxis)) args$xaxis <- modifyList(defaults, args$xaxis)
  else args$xaxis <- defaults
  if (!is.null(args$yaxis)) args$yaxis <- modifyList(defaults, args$yaxis)
  else args$yaxis <- defaults
  do.call(layout, c(list(p = p, paper_bgcolor = "transparent",
    plot_bgcolor = "#fafaf7", font = list(color = "#2d2d2d", family = "Outfit"),
    margin = list(t = 30, r = 10)), args))
}


# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

  # Header
  div(class = "main-header",
    div(
      div(class = "main-title",
        if (nchar(country_name) > 0) paste0("\U0001f50d ", country_name, " \u2014 GBIF Explorer")
        else "\U0001f50d GBIF Biodiversity Explorer"),
      div(class = "main-subtitle", "Explore species occurrences, distributions, and biodiversity patterns")
    ),
    div(class = "header-stats",
      if (!is.null(species_lookup)) tagList(
        span("Species: ", span(class = "header-stat-value", comma(length(species_choices)))),
        span("Prepared: ", span(class = "header-stat-value",
          if (!is.null(metadata)) format(metadata$created_at, "%d %b %Y") else "?"))
      )
    )
  ),

  # Main content
  div(style = "padding: 0 1rem;",
    tabsetPanel(
      id = "main_tabs", type = "pills",

      # =====================================================================
      # SPECIES SEARCH TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("search"), "Species Search"),
        value = "species",
        div(style = "padding: 1.25rem 0;",
          div(class = "search-container",
            selectizeInput("species_search", "Search species by name",
              choices = NULL, multiple = FALSE,
              width = "100%")
          ),
          # Summary stats (always visible)
          uiOutput("species_landing_stats"),
          # Species profile (visible after search)
          conditionalPanel(
            condition = "output.has_species_selected",
            fluidRow(
              column(4,
                div(class = "card", uiOutput("species_profile")),
                div(class = "card",
                  div(class = "card-title", icon("calendar-alt"), "Seasonal Pattern"),
                  plotlyOutput("species_seasonal", height = "240px"))
              ),
              column(8,
                div(class = "card",
                  div(class = "card-title", icon("map"), "Distribution"),
                  leafletOutput("species_map", height = "380px")),
                div(class = "card",
                  div(class = "card-title", icon("chart-area"), "Occurrence Trend"),
                  plotlyOutput("species_trend", height = "240px"))
              )
            )
          ),
          # Top species table (visible before search)
          conditionalPanel(
            condition = "!output.has_species_selected",
            div(class = "card",
              div(class = "card-title", icon("trophy"), "Most Recorded Species"),
              DTOutput("top_species_table"))
          )
        )
      ),

      # =====================================================================
      # WHAT LIVES HERE TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("map-marker-alt"), "What Lives Here?"),
        value = "cell_explore",
        div(style = "padding: 1.25rem 0;",
          div(style = "font-size:0.9rem; color:#6b6b6b; margin-bottom:0.75rem;",
            icon("info-circle"), " Click any grid cell on the map to see all recorded species."),
          fluidRow(
            column(7, div(class = "card",
              div(class = "card-title", icon("globe"), "Species Richness"),
              leafletOutput("cell_map", height = "520px"))),
            column(5,
              div(class = "card",
                uiOutput("cell_info"),
                DTOutput("cell_species_table"))
            )
          )
        )
      ),

      # =====================================================================
      # TAXONOMY BROWSER TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("sitemap"), "Taxonomy Browser"),
        value = "taxonomy",
        div(style = "padding: 1.25rem 0;",
          fluidRow(
            column(3, div(class = "card",
              div(class = "card-title", icon("filter"), "Navigate Taxonomy"),
              selectInput("tax_kingdom", "Kingdom", choices = "All", selected = "All"),
              selectInput("tax_phylum", "Phylum", choices = "All", selected = "All"),
              selectInput("tax_class", "Class", choices = "All", selected = "All"),
              selectInput("tax_order", "Order", choices = "All", selected = "All"),
              hr(),
              uiOutput("tax_summary")
            )),
            column(9,
              div(class = "card",
                div(class = "card-title", icon("chart-bar"), "Species by Group"),
                plotlyOutput("tax_browser_chart", height = "420px")),
              div(class = "card",
                div(class = "card-title", icon("table"), "Species List"),
                DTOutput("tax_browser_table"))
            )
          )
        )
      ),

      # =====================================================================
      # THREATENED SPECIES TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("exclamation-triangle"), "Threatened Species"),
        value = "threatened",
        div(style = "padding: 1.25rem 0;",
          div(class = "stat-grid",
            div(class = "stat-box",
              div(class = "stat-value coral", textOutput("stat_cr2", inline = TRUE)),
              div(class = "stat-label", "Critically Endangered")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("stat_en2", inline = TRUE)),
              div(class = "stat-label", "Endangered")),
            div(class = "stat-box",
              div(class = "stat-value slate", textOutput("stat_vu2", inline = TRUE)),
              div(class = "stat-label", "Vulnerable")),
            div(class = "stat-box",
              div(class = "stat-value sage", textOutput("stat_nt2", inline = TRUE)),
              div(class = "stat-label", "Near Threatened"))
          ),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-bar"), "GBIF Coverage by Threat Level"),
              plotlyOutput("threat_coverage_chart", height = "320px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("clock"), "Data Staleness Map"),
              leafletOutput("threat_stale_map", height = "320px")))
          ),
          div(class = "card",
            div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:0.5rem;",
              div(class = "card-title", icon("table"), "All Threatened Species"),
              downloadButton("download_threatened", "Download CSV", class = "btn-download")),
            DTOutput("threatened_table"))
        )
      )
    ),

    # Footer
    div(class = "metadata-footer",
      HTML(paste0(
        "Data prepared: ",
        if (!is.null(metadata)) format(metadata$created_at, "%Y-%m-%d %H:%M") else "Unknown",
        " \u00b7 gbifgaps Explorer"
      ))
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---- Species search: server-side selectize ----
  updateSelectizeInput(session, "species_search",
    choices = species_choices,
    selected = "",
    server = TRUE,
    options = list(
      placeholder = "Type a species name (e.g. Parus major)...",
      maxOptions = 25,
      openOnFocus = FALSE
    )
  )

  # ---- Species-to-time-file mapping (direct lookup, no taxonomy needed) ----
  species_time_map <- safe_get("species_time_map")
  if (!is.null(species_time_map) && !is.data.table(species_time_map)) {
    species_time_map <- as.data.table(species_time_map)
  }

  # ---- Reactives ----
  selected_species <- reactive({
    req(input$species_search, nchar(input$species_search) > 0)
    sp <- species_lookup |> filter(species == input$species_search)
    if (nrow(sp) == 0) return(NULL)
    sp[1, ]
  })

  species_cell_data <- reactive({
    sp <- selected_species()
    req(sp, cell_species_index)
    cell_species_index[species == sp$species]
  })

  species_time_cache <- reactiveValues()

  species_time_data <- reactive({
    sp <- selected_species()
    req(sp)
    key <- sp$species
    if (is.null(species_time_cache[[key]])) {
      # Direct lookup via pre-built mapping
      filepath <- NULL
      if (!is.null(species_time_map)) {
        match <- species_time_map[species == sp$species]
        if (nrow(match) > 0) filepath <- match$time_filepath[1]
      }
      if (is.null(filepath) || !file.exists(filepath)) {
        species_time_cache[[key]] <- data.table()
        return(data.table())
      }
      dt <- fread(filepath)
      dt <- dt[species == sp$species & basisofrecord == "all" & grid == "grid10km"]
      species_time_cache[[key]] <- dt
    }
    species_time_cache[[key]]
  })

  # ===================================================================
  # SPECIES SEARCH TAB
  # ===================================================================

  # Flag for conditionalPanel: is a species selected?
  output$has_species_selected <- reactive({
    sp <- input$species_search
    !is.null(sp) && nchar(sp) > 0
  })
  outputOptions(output, "has_species_selected", suspendWhenHidden = FALSE)

  output$species_landing_stats <- renderUI({
    sp_name <- input$species_search
    if (!is.null(sp_name) && nchar(sp_name) > 0) return(NULL)
    req(species_lookup)

    n_threatened <- if ("threatStatus" %in% names(species_lookup))
      sum(species_lookup$threatStatus %in% c("CR","EN","VU","NT"), na.rm = TRUE) else 0
    n_orders <- length(unique(na.omit(species_lookup$order)))
    n_families <- length(unique(na.omit(species_lookup$family)))

    div(class = "stat-grid",
      div(class = "stat-box",
        div(class = "stat-value sage", comma(length(species_choices))),
        div(class = "stat-label", "Total Species")),
      div(class = "stat-box",
        div(class = "stat-value slate", comma(n_threatened)),
        div(class = "stat-label", "Threatened")),
      div(class = "stat-box",
        div(class = "stat-value sand", comma(n_orders)),
        div(class = "stat-label", "Orders")),
      div(class = "stat-box",
        div(class = "stat-value plum", comma(n_families)),
        div(class = "stat-label", "Families"))
    )
  })

  output$species_profile <- renderUI({
    sp <- selected_species()
    req(sp)

    has_threat <- "threatStatus" %in% names(sp) && !is.na(sp$threatStatus) && sp$threatStatus != ""
    threat_html <- if (has_threat) {
      paste0('<span class="threat-badge ', sp$threatStatus, '">', sp$threatStatus, '</span>')
    } else ""

    n_cells <- if ("n_cells" %in% names(sp) && !is.na(sp$n_cells)) sp$n_cells else "—"

    HTML(paste0(
      '<div class="species-header">',
        '<div class="species-name">', sp$species, '</div> ', threat_html,
      '</div>',
      '<div class="species-meta">',
        '<strong>Kingdom:</strong> ', ifelse(is.na(sp$kingdom), "\u2014", sp$kingdom), '<br>',
        '<strong>Phylum:</strong> ', ifelse(is.na(sp$phylum), "\u2014", sp$phylum), '<br>',
        '<strong>Class:</strong> ', ifelse(is.na(sp$class), "\u2014", sp$class), '<br>',
        '<strong>Order:</strong> ', ifelse(is.na(sp$order), "\u2014", sp$order), '<br>',
        '<strong>Family:</strong> ', ifelse(is.na(sp$family), "\u2014", sp$family),
      '</div>',
      '<div class="species-stats">',
        '<div class="species-stat-item">',
          '<div class="species-stat-num">', comma(sp$total_occurrences), '</div>',
          '<div class="species-stat-label">Occurrences</div></div>',
        '<div class="species-stat-item">',
          '<div class="species-stat-num">', if (is.character(n_cells)) n_cells else comma(n_cells), '</div>',
          '<div class="species-stat-label">Grid Cells</div></div>',
        '<div class="species-stat-item">',
          '<div class="species-stat-num">', if (has_threat) sp$threatStatus else "LC/NE", '</div>',
          '<div class="species-stat-label">Threat Status</div></div>',
      '</div>'
    ))
  })

  # Species map
  output$species_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 16, lat = 63, zoom = 5)
  })

  observe({
    sp_cells <- species_cell_data()
    req(grid_10km)

    if (is.null(sp_cells) || nrow(sp_cells) == 0) {
      leafletProxy("species_map") |> clearShapes() |> clearControls()
      return()
    }

    cell_agg <- sp_cells[, .(occurrences = sum(occurrences, na.rm = TRUE)), by = eeacellcode]
    map_sf <- grid_10km |> inner_join(cell_agg, by = "eeacellcode")

    if (nrow(map_sf) == 0) {
      leafletProxy("species_map") |> clearShapes() |> clearControls()
      return()
    }

    vals <- log10(pmax(map_sf$occurrences, 1))
    pal_fn <- colorNumeric(c("#e8ede9", pal$sage, "#2c5a35"), domain = vals, na.color = "#ddd")

    leafletProxy("species_map") |>
      clearShapes() |> clearControls() |>
      addPolygons(data = map_sf,
        fillColor = ~pal_fn(vals), fillOpacity = 0.75, weight = 0.5, color = "#999",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Occurrences:</strong> ", comma(occurrences))) |>
      addLegend("bottomright", pal = pal_fn, values = vals, title = "log\u2081\u2080(Occ)")
  })

  # Species trend
  output$species_trend <- renderPlotly({
    sp_time <- species_time_data()
    if (is.null(sp_time) || nrow(sp_time) == 0) {
      return(plot_ly(x = 0, y = 0, type = "scatter", mode = "none") |>
        plotly_layout(
          annotations = list(list(text = "Temporal data not available for this species",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(color = "#9a9a9a", size = 13))),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }

    df <- sp_time[, .(ym = as.character(yearmonth), occurrences)
      ][, year := as.integer(str_sub(ym, 1, 4))
      ][, .(occ = sum(occurrences, na.rm = TRUE)), by = year
      ][year >= 1950]
    setorder(df, year)

    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines+markers",
      line = list(color = pal$sage, width = 2),
      marker = list(color = pal$sage, size = 4),
      fill = "tozeroy", fillcolor = "rgba(107,143,113,0.12)",
      hovertemplate = "%{x}: %{y:,.0f} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Occurrences"))
  })

  # Species seasonal
  output$species_seasonal <- renderPlotly({
    sp_time <- species_time_data()
    if (is.null(sp_time) || nrow(sp_time) == 0) {
      return(plot_ly(x = 0, y = 0, type = "scatter", mode = "none") |>
        plotly_layout(
          annotations = list(list(text = "No seasonal data",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(color = "#9a9a9a", size = 13))),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }

    df <- sp_time[, .(ym = as.character(yearmonth), occurrences)
      ][, month := as.integer(str_sub(ym, 6, 7))
      ][!is.na(month) & month >= 1 & month <= 12
      ][, .(occ = sum(occurrences, na.rm = TRUE)), by = month]

    # Radar-like bar chart with month colors
    month_cols <- colorRampPalette(c(pal$slate, pal$sage, pal$sand, pal$coral, pal$slate))(12)

    plot_ly(df, x = ~month, y = ~occ, type = "bar",
      marker = list(color = month_cols[df$month]),
      hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12, dtick = 1),
        yaxis = list(title = "Occurrences"))
  })

  # Top species table
  output$top_species_table <- renderDT({
    req(species_lookup)
    df <- species_lookup |>
      slice_head(n = 200) |>
      select(any_of(c("species", "kingdom", "class", "order", "family",
                        "threatStatus", "total_occurrences", "n_cells")))

    for (cn in c("kingdom", "class", "order", "family", "threatStatus"))
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])

    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
      style = "bootstrap4", filter = "top",
      colnames = c("Species", "Kingdom", "Class", "Order", "Family",
                    "Threat", "Occurrences", "Cells")[seq_along(names(df))]) |>
      formatCurrency("total_occurrences", currency = "", digits = 0)
  }, server = TRUE)

  # ===================================================================
  # WHAT LIVES HERE TAB
  # ===================================================================

  output$cell_map <- renderLeaflet({
    req(grid_10km)

    if (!is.null(spatial_gaps)) {
      sf_data <- spatial_gaps |> filter(basisofrecord == "all")
      map_sf <- grid_10km |>
        left_join(sf_data |> select(eeacellcode, n_species), by = "eeacellcode")
      vals <- log10(pmax(map_sf$n_species, 1, na.rm = TRUE))
      pal_fn <- colorNumeric(c("#f6f5f1", pal$slate, "#2c4a6b"), domain = vals, na.color = "#eee")

      leaflet(map_sf) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal_fn(vals), fillOpacity = 0.5,
          weight = 0.3, color = "#bbb", layerId = ~eeacellcode,
          popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                          "<br><strong>Species:</strong> ", comma(n_species))) |>
        addLegend("bottomright", pal = pal_fn, values = vals, title = "log\u2081\u2080(Species)")
    } else {
      leaflet(grid_10km) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = "#ddd", fillOpacity = 0.3, weight = 0.3, color = "#bbb",
          layerId = ~eeacellcode)
    }
  })

  selected_cell <- reactiveVal(NULL)
  observeEvent(input$cell_map_shape_click, {
    click <- input$cell_map_shape_click
    if (!is.null(click$id)) selected_cell(click$id)
  })

  cell_species_reactive <- reactive({
    cell_id <- selected_cell()
    req(cell_id, cell_species_index)
    dt <- cell_species_index[eeacellcode == cell_id]
    if (nrow(dt) == 0) return(NULL)
    setorder(dt, -occurrences)
    dt
  })

  output$cell_info <- renderUI({
    cell_id <- selected_cell()
    if (is.null(cell_id)) {
      return(div(class = "empty-state",
        icon("mouse-pointer"),
        p("Click a cell on the map to explore")))
    }
    df <- cell_species_reactive()
    n_sp <- if (!is.null(df)) nrow(df) else 0
    total_occ <- if (!is.null(df)) sum(df$occurrences, na.rm = TRUE) else 0

    tagList(
      div(class = "card-title", icon("list"), paste0("Cell: ", cell_id)),
      div(class = "species-stats", style = "grid-template-columns: repeat(2, 1fr); margin-bottom:0.75rem;",
        div(class = "species-stat-item",
          div(class = "species-stat-num", comma(n_sp)),
          div(class = "species-stat-label", "Species")),
        div(class = "species-stat-item",
          div(class = "species-stat-num", comma(total_occ)),
          div(class = "species-stat-label", "Occurrences"))
      )
    )
  })

  output$cell_species_table <- renderDT({
    df <- cell_species_reactive()
    req(df)
    df <- as_tibble(df) |>
      left_join(species_lookup |> select(any_of(c("species", "order", "family", "threatStatus"))), by = "species") |>
      select(any_of(c("species", "occurrences", "order", "family", "threatStatus")))

    for (cn in c("order", "family", "threatStatus"))
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])

    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
      style = "bootstrap4", filter = "top")
  }, server = TRUE)

  # ===================================================================
  # TAXONOMY BROWSER TAB
  # ===================================================================

  observe({
    req(species_lookup)
    updateSelectInput(session, "tax_kingdom",
      choices = c("All", sort(unique(na.omit(species_lookup$kingdom)))))
  })

  observe({
    req(species_lookup)
    df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    updateSelectInput(session, "tax_phylum",
      choices = c("All", sort(unique(na.omit(df$phylum)))))
  })

  observe({
    req(species_lookup)
    df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "All") df <- df |> filter(phylum == input$tax_phylum)
    updateSelectInput(session, "tax_class",
      choices = c("All", sort(unique(na.omit(df$class)))))
  })

  observe({
    req(species_lookup)
    df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "All") df <- df |> filter(phylum == input$tax_phylum)
    if (!is.null(input$tax_class) && input$tax_class != "All") df <- df |> filter(class == input$tax_class)
    updateSelectInput(session, "tax_order",
      choices = c("All", sort(unique(na.omit(df$order)))))
  })

  tax_filtered <- reactive({
    req(species_lookup)
    df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "All") df <- df |> filter(phylum == input$tax_phylum)
    if (!is.null(input$tax_class) && input$tax_class != "All") df <- df |> filter(class == input$tax_class)
    if (!is.null(input$tax_order) && input$tax_order != "All") df <- df |> filter(order == input$tax_order)
    df
  })

  output$tax_summary <- renderUI({
    df <- tax_filtered()
    n_threat <- if ("threatStatus" %in% names(df))
      sum(df$threatStatus %in% c("CR","EN","VU","NT"), na.rm = TRUE) else 0
    HTML(paste0(
      '<div style="font-size:0.85rem; color:#6b6b6b; line-height:1.8;">',
      '<strong>', comma(nrow(df)), '</strong> species<br>',
      '<strong>', comma(sum(df$total_occurrences, na.rm = TRUE)), '</strong> occurrences<br>',
      '<strong>', comma(n_threat), '</strong> threatened',
      '</div>'
    ))
  })

  output$tax_browser_chart <- renderPlotly({
    df <- tax_filtered()
    req(nrow(df) > 0)

    group_col <- if (!is.null(input$tax_order) && input$tax_order != "All") "family"
      else if (!is.null(input$tax_class) && input$tax_class != "All") "order"
      else if (!is.null(input$tax_phylum) && input$tax_phylum != "All") "class"
      else if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") "phylum"
      else "kingdom"

    chart_df <- df |>
      filter(!is.na(.data[[group_col]])) |>
      group_by(group = .data[[group_col]]) |>
      summarise(n_species = n(), total_occ = sum(total_occurrences, na.rm = TRUE),
        n_threatened = if ("threatStatus" %in% names(df)) sum(threatStatus %in% c("CR","EN","VU","NT"), na.rm = TRUE) else 0L, .groups = "drop") |>
      arrange(desc(n_species)) |> slice_head(n = 25)

    plot_ly(chart_df, y = ~reorder(group, n_species), x = ~n_species,
      type = "bar", orientation = "h",
      marker = list(color = pal$sage,
        line = list(color = pal$sage2, width = 0.5)),
      text = ~paste0(comma(total_occ), " occ | ", n_threatened, " threatened"),
      hovertemplate = paste0("<b>%{y}</b><br>Species: %{x}<br>%{text}<extra></extra>")) |>
      plotly_layout(
        xaxis = list(title = "Number of species"),
        yaxis = list(title = ""))
  })

  output$tax_browser_table <- renderDT({
    df <- tax_filtered() |>
      select(any_of(c("species", "kingdom", "phylum", "class", "order", "family",
                        "threatStatus", "total_occurrences", "n_cells"))) |>
      arrange(desc(total_occurrences)) |> slice_head(n = 500)

    for (cn in c("kingdom", "phylum", "class", "order", "family", "threatStatus"))
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])

    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
      style = "bootstrap4", filter = "top")
  }, server = TRUE)

  # ===================================================================
  # THREATENED SPECIES TAB
  # ===================================================================

  threatened_species <- reactive({
    req(species_lookup)
    if (!"threatStatus" %in% names(species_lookup)) return(tibble())
    species_lookup |>
      filter(threatStatus %in% c("CR", "EN", "VU", "NT")) |>
      arrange(factor(threatStatus, levels = c("CR", "EN", "VU", "NT")), desc(total_occurrences))
  })

  output$stat_cr2 <- renderText({ df <- threatened_species(); if (nrow(df) == 0) "—" else comma(sum(df$threatStatus == "CR")) })
  output$stat_en2 <- renderText({ df <- threatened_species(); if (nrow(df) == 0) "—" else comma(sum(df$threatStatus == "EN")) })
  output$stat_vu2 <- renderText({ df <- threatened_species(); if (nrow(df) == 0) "—" else comma(sum(df$threatStatus == "VU")) })
  output$stat_nt2 <- renderText({ df <- threatened_species(); if (nrow(df) == 0) "—" else comma(sum(df$threatStatus == "NT")) })

  output$threat_coverage_chart <- renderPlotly({
    req(tax_by_threat)
    df <- tax_by_threat |>
      filter(threatStatus %in% c("CR", "EN", "VU", "NT")) |>
      mutate(threatStatus = factor(threatStatus, levels = c("CR", "EN", "VU", "NT")))

    colors <- c("CR" = "#c0392b", "EN" = "#e67e22", "VU" = pal$sand, "NT" = pal$slate)

    plot_ly(df, x = ~threatStatus, y = ~pct_coverage, type = "bar",
      marker = list(color = ~colors[as.character(threatStatus)],
        line = list(color = "white", width = 1)),
      text = ~paste0(round(pct_coverage, 1), "%"),
      textposition = "auto", textfont = list(size = 13, color = "#fff"),
      hovertemplate = "%{x}: %{y:.1f}% coverage<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = ""),
        yaxis = list(title = "GBIF Coverage (%)", range = c(0, 105)))
  })

  output$threat_stale_map <- renderLeaflet({
    req(grid_10km)
    if (!is.null(cell_recency)) {
      rec <- cell_recency |> filter(basisofrecord == "all") |> select(eeacellcode, staleness_months)
      map_sf <- grid_10km |> left_join(rec, by = "eeacellcode")
      vals <- map_sf$staleness_months
      pal_fn <- colorNumeric(c(pal$sage, pal$sand, pal$coral), domain = vals, na.color = "#eee")

      leaflet(map_sf) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal_fn(vals), fillOpacity = 0.6, weight = 0.3, color = "#bbb") |>
        addLegend("bottomright", pal = pal_fn, values = vals, title = "Months stale")
    } else {
      leaflet() |> addProviderTiles(providers$CartoDB.Positron) |> setView(16, 63, 5)
    }
  })

  output$threatened_table <- renderDT({
    df <- threatened_species() |>
      select(any_of(c("species", "threatStatus", "kingdom", "phylum", "class",
                        "order", "family", "total_occurrences", "n_cells")))
    for (cn in c("threatStatus", "kingdom", "phylum", "class", "order", "family"))
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])

    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
      style = "bootstrap4", filter = "top")
  }, server = TRUE)

  output$download_threatened <- downloadHandler(
    filename = function() paste0("threatened_species_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(threatened_species(), file)
  )
}

shinyApp(ui = ui, server = server)
