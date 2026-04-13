# shiny_app/gbif_explorer/app.R
# ==============================================================================
# GBIF Explorer — Species & Biodiversity Explorer
# ==============================================================================
# Architecture:
#   - STATIC UI structure (no renderUI for tabs — avoids output destruction)
#   - i18n via renderText/renderUI for individual labels only
#   - Leaflet: static base map + leafletProxy(data=...) for data layers
#   - Same palette and categorical breaks as gap_app
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
# TRANSLATION DICTIONARY
# =============================================================================

i18n <- list(
  en = list(
    app_title = "GBIF Biodiversity Explorer",
    app_subtitle = "Explore species occurrences, distributions, and biodiversity patterns",
    species_label = "Species", prepared = "Prepared",
    region = "Region", municipality = "Municipality",
    all_regions = "All regions", all_municipalities = "All municipalities",
    search_placeholder = "Type a species name (e.g. Parus major)...",
    search_label = "Search species by name",
    total_species = "Total Species", threatened = "Threatened",
    orders = "Orders", families = "Families",
    most_recorded = "Most Recorded Species",
    occurrences = "Occurrences", grid_cells = "Grid Cells",
    threat_status = "Threat Status",
    click_cell = "Click any grid cell on the map to see all recorded species.",
    click_to_explore = "Click a cell on the map to explore",
    display = "Display", map_occ = "Occurrences", map_richness = "Species richness",
    navigate = "Navigate Taxonomy",
    kingdom = "Kingdom", phylum = "Phylum", class_l = "Class", order_l = "Order",
    species_by_group = "Species by Group", species_list = "Species List",
    cr = "Critically Endangered", en = "Endangered", vu = "Vulnerable",
    nt = "Near Threatened", dd = "Data Deficient",
    coverage_by_threat = "GBIF Coverage by Threat Level",
    staleness_map = "Data Staleness Map",
    all_threatened = "All Threatened Species", download_csv = "Download CSV"
  ),
  sv = list(
    app_title = "GBIF Biodiversitetsutforskare",
    app_subtitle = "Utforska artobservationer, utbredning och biodiversitetsmönster",
    species_label = "Arter", prepared = "Förberedd",
    region = "Region", municipality = "Kommun",
    all_regions = "Alla regioner", all_municipalities = "Alla kommuner",
    search_placeholder = "Skriv ett artnamn (t.ex. talgoxe eller Parus major)...",
    search_label = "Sök arter efter namn",
    total_species = "Totalt antal arter", threatened = "Hotade",
    orders = "Ordningar", families = "Familjer",
    most_recorded = "Mest registrerade arter",
    occurrences = "Observationer", grid_cells = "Rutnätsceller",
    threat_status = "Hotstatus",
    click_cell = "Klicka på en ruta på kartan för att se alla registrerade arter.",
    click_to_explore = "Klicka på en ruta för att utforska",
    display = "Visning", map_occ = "Observationer", map_richness = "Artrikedom",
    navigate = "Navigera taxonomi",
    kingdom = "Rike", phylum = "Stam", class_l = "Klass", order_l = "Ordning",
    species_by_group = "Arter per grupp", species_list = "Artlista",
    cr = "Akut hotad", en = "Starkt hotad", vu = "Sårbar",
    nt = "Nära hotad", dd = "Kunskapsbrist",
    coverage_by_threat = "GBIF-täckning per hotnivå",
    staleness_map = "Dataaktualitetskarta",
    all_threatened = "Alla hotade arter", download_csv = "Ladda ner CSV"
  )
)

# =============================================================================
# DATA LOADING
# =============================================================================

data_path <- "data/shiny_data.rds"
if (!file.exists(data_path)) stop("shiny_data.rds not found.")

app_data <- readRDS(data_path)
safe_get <- function(name) if (name %in% names(app_data)) app_data[[name]] else NULL

species_lookup     <- safe_get("species_lookup")
grid_10km          <- safe_get("grid_10km")
tax_by_threat      <- safe_get("tax_by_threat")
tax_by_kingdom     <- safe_get("tax_by_kingdom")
spatial_gaps       <- safe_get("spatial_gaps_10km")
cell_recency       <- safe_get("cell_recency_10km")
metadata           <- safe_get("metadata")
cell_species_index <- safe_get("cell_species_index")
admin_level1       <- safe_get("admin_level1")
admin_level2       <- safe_get("admin_level2")
cell_admin         <- safe_get("cell_admin_lookup")
has_admin          <- !is.null(admin_level1) && !is.null(cell_admin)
match_summary_full <- safe_get("taxonomic_match_summary")

if (!is.null(cell_species_index) && !is.data.table(cell_species_index)) {
  cell_species_index <- as.data.table(cell_species_index)
  setkey(cell_species_index, eeacellcode)
}

has_vernacular <- !is.null(species_lookup) && "vernacular_sv" %in% names(species_lookup)

species_lookup <- if (!is.null(species_lookup)) {
  species_lookup[!duplicated(species_lookup$species), ]
} else NULL

species_choices <- if (!is.null(species_lookup)) {
  sp <- unique(na.omit(species_lookup$species))
  sp[nchar(sp) > 0]
} else character(0)

# Vernacular search choices: "svenskt namn (Scientific)" -> scientific
vernacular_choices <- if (has_vernacular) {
  vn_df <- species_lookup |> filter(!is.na(vernacular_sv), vernacular_sv != "")
  setNames(vn_df$species, paste0(vn_df$vernacular_sv, " (", vn_df$species, ")"))
} else character(0)

country_name <- tryCatch(yaml::read_yaml("../../config.yml")$country$name, error = function(e) "")

region_choices <- if (has_admin) {
  c("All regions" = "", sort(unique(na.omit(cell_admin$admin_name_level1))))
} else character(0)

# Establishment means / scope
has_establishment <- !is.null(species_lookup) && "establishmentMeans" %in% names(species_lookup)
scope_choices <- c("All species" = "all")
if (has_establishment) {
  scope_choices <- c(scope_choices,
    "Native" = "native",
    "Introduced" = "introduced",
    "Invasive" = "invasive")
}

species_time_map <- safe_get("species_time_map")
if (!is.null(species_time_map) && !is.data.table(species_time_map)) {
  species_time_map <- as.data.table(species_time_map)
}

# Palette (matching gap app exactly)
pal <- list(
  sage  = "#6b8f71", sage2 = "#8ab090",
  slate = "#5c7a99", slate2 = "#7d9ab5",
  sand  = "#c4a882", sand2  = "#d4c0a0",
  coral = "#c47a6c", coral2 = "#d9a090",
  plum  = "#8b6d8f"
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
# UI — FULLY STATIC (no renderUI for tabs)
# =============================================================================

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

  div(class = "main-header",
    div(
      uiOutput("header_title_ui"),
      uiOutput("header_subtitle_ui")
    ),
    div(class = "header-stats",
      div(style = "display:flex; align-items:center; gap:1rem; flex-wrap:wrap;",
        div(style = "display:flex; gap:4px;",
          actionButton("lang_en", "EN", class = "btn-sm", style = "padding:2px 8px; font-size:0.75rem;"),
          actionButton("lang_sv", "SV", class = "btn-sm", style = "padding:2px 8px; font-size:0.75rem;")),
        uiOutput("header_stats_ui"),
        if (has_admin) selectInput("region_filter", NULL, choices = region_choices, selected = "", width = "180px"),
        if (has_admin) uiOutput("municipality_filter_ui")
      )
    )
  ),

  div(style = "padding: 0 1rem;",
    tabsetPanel(
      id = "main_tabs", type = "pills",

      # === SPECIES SEARCH ===
      tabPanel(
        title = tagList(icon("search"), "Species Search"),
        value = "species",
        div(style = "padding: 1.25rem 0;",
          div(class = "search-container",
            selectizeInput("species_search", NULL, choices = NULL, multiple = FALSE, width = "100%")),
          uiOutput("species_landing_stats"),
          conditionalPanel(
            condition = "output.has_species_selected",
            fluidRow(
              column(4,
                div(class = "card", uiOutput("species_profile")),
                div(class = "card",
                  div(class = "card-title", icon("calendar-alt"), "Seasonal Pattern"),
                  plotlyOutput("species_seasonal", height = "240px"))),
              column(8,
                div(class = "card",
                  div(class = "card-title", icon("map"), "Distribution"),
                  leafletOutput("species_map", height = "380px")),
                div(class = "card",
                  div(class = "card-title", icon("chart-area"), "Occurrence Trend"),
                  plotlyOutput("species_trend", height = "240px"))))),
          conditionalPanel(
            condition = "!output.has_species_selected",
            div(class = "card",
              div(class = "card-title", icon("trophy"), "Most Recorded Species"),
              DTOutput("top_species_table")))
        )
      ),

      # === WHAT LIVES HERE ===
      tabPanel(
        title = tagList(icon("map-marker-alt"), "What Lives Here?"),
        value = "cell_explore",
        div(style = "padding: 1.25rem 0;",
          uiOutput("cell_explore_hint"),
          fluidRow(
            column(8, div(class = "card",
              div(style = "display:flex; justify-content:space-between; align-items:center;",
                div(class = "card-title", icon("globe"), "What Lives Here?"),
                radioButtons("cell_map_var", NULL,
                  choiceNames = list("Species richness", "Occurrences"),
                  choiceValues = list("richness", "occurrences"),
                  selected = "richness", inline = TRUE)),
              leafletOutput("cell_map", height = "520px"))),
            column(4,
              uiOutput("region_summary_card"),
              div(class = "card",
                uiOutput("cell_info"),
                DTOutput("cell_species_table")))
          )
        )
      ),

      # === TAXONOMY BROWSER ===
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
              if (has_establishment) selectInput("tax_scope", "Scope",
                choices = scope_choices, selected = "all"),
              hr(), uiOutput("tax_summary"))),
            column(9,
              div(class = "card",
                div(class = "card-title", icon("chart-bar"), "Species by Group"),
                plotlyOutput("tax_browser_chart", height = "420px")),
              div(class = "card",
                div(class = "card-title", icon("table"), "Species List"),
                DTOutput("tax_browser_table")))
          )
        )
      ),

      # === THREATENED SPECIES ===
      tabPanel(
        title = tagList(icon("exclamation-triangle"), "Threatened Species"),
        value = "threatened",
        div(style = "padding: 1.25rem 0;",
          div(class = "stat-grid", style = "grid-template-columns: repeat(5, 1fr);",
            div(class = "stat-box", div(class = "stat-value coral", textOutput("stat_cr2", inline = TRUE)),
              div(class = "stat-label", "CR = Critically Endangered")),
            div(class = "stat-box", div(class = "stat-value sand", textOutput("stat_en2", inline = TRUE)),
              div(class = "stat-label", "EN = Endangered")),
            div(class = "stat-box", div(class = "stat-value slate", textOutput("stat_vu2", inline = TRUE)),
              div(class = "stat-label", "VU = Vulnerable")),
            div(class = "stat-box", div(class = "stat-value sage", textOutput("stat_nt2", inline = TRUE)),
              div(class = "stat-label", "NT = Near Threatened")),
            div(class = "stat-box", div(class = "stat-value plum", textOutput("stat_dd2", inline = TRUE)),
              div(class = "stat-label", "DD = Data Deficient"))
          ),
          if (has_establishment) div(class = "filter-section",
            fluidRow(column(3, selectInput("threat_scope", "Scope",
              choices = scope_choices, selected = "all")))),
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
      ),

      # === INVASIVE SPECIES ===
      if (has_establishment) tabPanel(
        title = tagList(icon("bug"), "Invasive Species"),
        value = "invasive",
        div(style = "padding: 1.25rem 0;",
          uiOutput("invasive_stats"),
          fluidRow(
            column(7, div(class = "card",
              div(class = "card-title", icon("map"), "Invasive Species Distribution"),
              leafletOutput("invasive_map", height = "450px"))),
            column(5, div(class = "card",
              div(class = "card-title", icon("chart-line"), "Invasive Species Records Over Time"),
              plotlyOutput("invasive_trend", height = "200px")),
              div(class = "card",
                div(class = "card-title", icon("chart-bar"), "Top Invasive Species by Records"),
                plotlyOutput("invasive_top_chart", height = "240px")))
          ),
          div(class = "card",
            div(class = "card-title", icon("table"), "All Invasive Species"),
            DTOutput("invasive_table"))
        )
      )
    ),

    div(class = "metadata-footer", HTML(paste0(
      if (!is.null(metadata)) format(metadata$created_at, "%Y-%m-%d %H:%M") else "",
      " · gbifgaps Explorer")))
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---- Language ----
  lang <- reactiveVal("en")
  observeEvent(input$lang_en, { lang("en") })
  observeEvent(input$lang_sv, { lang("sv") })
  t <- function(key) { i18n[[lang()]][[key]] %||% i18n[["en"]][[key]] %||% key }

  # ---- i18n: header labels ----
  output$header_title_ui <- renderUI({
    title <- if (nchar(country_name) > 0) paste0("\U0001f50d ", country_name, " — ", t("app_title"))
    else paste0("\U0001f50d ", t("app_title"))
    div(class = "main-title", title)
  })
  output$header_subtitle_ui <- renderUI({ div(class = "main-subtitle", t("app_subtitle")) })
  output$header_stats_ui <- renderUI({
    if (!is.null(species_lookup))
      span(style = "font-size:0.85rem;", t("species_label"), ": ",
        span(class = "header-stat-value", comma(length(species_choices))))
  })
  output$cell_explore_hint <- renderUI({
    div(style = "font-size:0.9rem; color:#6b6b6b; margin-bottom:0.75rem;",
      icon("info-circle"), " ", t("click_cell"))
  })

  # ---- Municipality filter (cascades from region) ----
  output$municipality_filter_ui <- renderUI({
    sel <- input$region_filter
    if (is.null(sel) || sel == "" || !has_admin) return(NULL)
    munis <- cell_admin |> filter(admin_name_level1 == sel, !is.na(admin_name_level2)) |>
      pull(admin_name_level2) |> unique() |> sort()
    if (length(munis) == 0) return(NULL)
    selectInput("municipality_filter", NULL,
      choices = c(setNames("", t("all_municipalities")), setNames(munis, munis)),
      selected = "", width = "180px")
  })

  # ---- Region cells ----
  region_cells <- reactive({
    if (!has_admin) return(NULL)
    sel <- input$region_filter
    if (is.null(sel) || sel == "") return(NULL)
    cells <- cell_admin |> filter(admin_name_level1 == sel)
    muni <- input$municipality_filter
    if (!is.null(muni) && muni != "" && "admin_name_level2" %in% names(cell_admin))
      cells <- cells |> filter(admin_name_level2 == muni)
    cells |> pull(eeacellcode)
  })

  # ---- Species search ----
  observe({
    if (lang() == "sv" && length(vernacular_choices) > 0) {
      all_ch <- c(vernacular_choices, setNames(species_choices, species_choices))
      updateSelectizeInput(session, "species_search", choices = all_ch, selected = "",
        server = TRUE, options = list(placeholder = t("search_placeholder"), maxOptions = 25))
    } else {
      updateSelectizeInput(session, "species_search", choices = species_choices, selected = "",
        server = TRUE, options = list(placeholder = t("search_placeholder"), maxOptions = 25))
    }
  })

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
      filepath <- NULL
      if (!is.null(species_time_map)) {
        m <- species_time_map[species == sp$species]
        if (nrow(m) > 0) filepath <- m$time_filepath[1]
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

  output$has_species_selected <- reactive({
    sp <- input$species_search
    !is.null(sp) && nchar(sp) > 0
  })
  outputOptions(output, "has_species_selected", suspendWhenHidden = FALSE)

  # ---- Species landing stats ----
  output$species_landing_stats <- renderUI({
    if (!is.null(input$species_search) && nchar(input$species_search) > 0) return(NULL)
    req(species_lookup)
    n_threat <- sum(species_lookup$threatStatus %in% c("CR","EN","VU","NT","DD"), na.rm = TRUE)
    div(class = "stat-grid",
      div(class = "stat-box", div(class = "stat-value sage", comma(length(species_choices))),
        div(class = "stat-label", t("total_species"))),
      div(class = "stat-box", div(class = "stat-value slate", comma(n_threat)),
        div(class = "stat-label", t("threatened"))),
      div(class = "stat-box", div(class = "stat-value sand", comma(length(unique(na.omit(species_lookup$order))))),
        div(class = "stat-label", t("orders"))),
      div(class = "stat-box", div(class = "stat-value plum", comma(length(unique(na.omit(species_lookup$family))))),
        div(class = "stat-label", t("families"))))
  })

  # ---- Species profile ----
  output$species_profile <- renderUI({
    sp <- selected_species(); req(sp)
    has_threat <- "threatStatus" %in% names(sp) && !is.na(sp$threatStatus) && sp$threatStatus != ""
    threat_html <- if (has_threat) paste0('<span class="threat-badge ', sp$threatStatus, '">', sp$threatStatus, '</span>') else ""
    n_cells <- if ("n_cells" %in% names(sp) && !is.na(sp$n_cells)) comma(sp$n_cells) else "\u2014"
    vn_sv <- if (has_vernacular && !is.na(sp$vernacular_sv)) sp$vernacular_sv else NULL

    HTML(paste0(
      '<div class="species-header"><div class="species-name">', sp$species, '</div> ', threat_html, '</div>',
      if (!is.null(vn_sv)) paste0('<div style="font-size:1rem;color:#6b6b6b;font-style:italic;">', vn_sv, '</div>') else '',
      if ("establishmentMeans" %in% names(sp) && !is.na(sp$establishmentMeans) && sp$establishmentMeans != "")
        paste0('<div style="margin:0.3rem 0;"><span style="display:inline-block;padding:2px 10px;border-radius:4px;font-size:0.8rem;font-weight:500;',
          'background:', switch(sp$establishmentMeans,
            native = "rgba(107,143,113,0.15);color:#4a7050",
            introduced = "rgba(196,168,130,0.2);color:#8a7040",
            invasive = "rgba(196,122,108,0.2);color:#a04030",
            "rgba(150,150,150,0.15);color:#666"),
          ';">', sp$establishmentMeans, '</span></div>') else '',
      '<div class="species-meta">',
        '<strong>', t("kingdom"), ':</strong> ', sp$kingdom %||% "\u2014", '<br>',
        '<strong>', t("phylum"), ':</strong> ', sp$phylum %||% "\u2014", '<br>',
        '<strong>', t("class_l"), ':</strong> ', sp$class %||% "\u2014", '<br>',
        '<strong>', t("order_l"), ':</strong> ', sp$order %||% "\u2014", '<br>',
        '<strong>Family:</strong> ', sp$family %||% "\u2014",
      '</div>',
      '<div class="species-stats">',
        '<div class="species-stat-item"><div class="species-stat-num">', comma(sp$total_occurrences), '</div>',
          '<div class="species-stat-label">', t("occurrences"), '</div></div>',
        '<div class="species-stat-item"><div class="species-stat-num">', n_cells, '</div>',
          '<div class="species-stat-label">', t("grid_cells"), '</div></div>',
        '<div class="species-stat-item"><div class="species-stat-num">', if (has_threat) sp$threatStatus else "LC/NE", '</div>',
          '<div class="species-stat-label">', t("threat_status"), '</div></div>',
      '</div>'))
  })

  # ---- Species map: STATIC base + proxy for data (same pattern as gap_app) ----
  output$species_map <- renderLeaflet({
    m <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 16, lat = 63, zoom = 5)
    if (!is.null(admin_level1))
      m <- m |> addPolygons(data = admin_level1, group = "admin1",
        fillColor = "transparent", fillOpacity = 0,
        weight = 1.5, color = "#333", opacity = 0.3, label = ~admin_name,
        labelOptions = labelOptions(style = list("font-size" = "12px", "font-weight" = "bold")))
    if (!is.null(admin_level2))
      m <- m |> addPolygons(data = admin_level2, group = "admin2",
        fillColor = "transparent", fillOpacity = 0,
        weight = 0.8, color = "#666", opacity = 0.2, dashArray = "3,3", label = ~admin_name)
    m
  })

  observe({
    sp_cells <- tryCatch(species_cell_data(), error = function(e) NULL)
    proxy <- leafletProxy("species_map")
    proxy |> clearGroup("sp_data") |> clearControls()

    if (is.null(sp_cells) || nrow(sp_cells) == 0 || is.null(grid_10km)) return()

    cell_agg <- sp_cells[, .(occurrences = sum(as.numeric(occurrences), na.rm = TRUE)), by = eeacellcode]
    map_sf <- grid_10km |> inner_join(cell_agg, by = "eeacellcode")
    if (nrow(map_sf) == 0) return()

    # Categorical occurrence density — SAME PATTERN AS GAP APP
    map_sf <- map_sf |>
      mutate(occ_cat = case_when(
        occurrences <= 10 ~ "1\u201310",
        occurrences <= 100 ~ "11\u2013100",
        occurrences <= 1000 ~ "101\u20131,000",
        TRUE ~ ">1,000"
      ),
      occ_cat = factor(occ_cat, levels = c("1\u201310", "11\u2013100", "101\u20131,000", ">1,000")))

    occ_pal <- colorFactor(
      palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral),
      domain = levels(map_sf$occ_cat), na.color = "#ddd")

    leafletProxy("species_map", data = map_sf) |>
      addPolygons(group = "sp_data",
        fillColor = ~occ_pal(occ_cat), fillOpacity = 0.75, weight = 0.5, color = "#999",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Occurrences:</strong> ", comma(occurrences))) |>
      addLegend("bottomright", pal = occ_pal, values = ~occ_cat, title = "Occurrences") |>
      fitBounds(
        st_bbox(map_sf)["xmin"], st_bbox(map_sf)["ymin"],
        st_bbox(map_sf)["xmax"], st_bbox(map_sf)["ymax"])
  })

  # ---- Species trend ----
  output$species_trend <- renderPlotly({
    sp_time <- species_time_data()
    if (is.null(sp_time) || nrow(sp_time) == 0)
      return(plotly_empty() |> plotly_layout())
    df <- sp_time[, .(ym = as.character(yearmonth), occurrences)
      ][, year := as.integer(str_sub(ym, 1, 4))
      ][, .(occ = sum(as.numeric(occurrences), na.rm = TRUE)), by = year
      ][year >= 1950]
    setorder(df, year)
    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines+markers",
      line = list(color = pal$sage, width = 2), marker = list(color = pal$sage, size = 4),
      fill = "tozeroy", fillcolor = "rgba(107,143,113,0.12)",
      hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") |>
      plotly_layout(xaxis = list(title = ""), yaxis = list(title = t("occurrences")))
  })

  # ---- Species seasonal ----
  output$species_seasonal <- renderPlotly({
    sp_time <- species_time_data()
    if (is.null(sp_time) || nrow(sp_time) == 0)
      return(plotly_empty() |> plotly_layout())
    df <- sp_time[, .(ym = as.character(yearmonth), occurrences)
      ][, month := as.integer(str_sub(ym, 6, 7))
      ][!is.na(month) & month >= 1 & month <= 12
      ][, .(occ = sum(as.numeric(occurrences), na.rm = TRUE)), by = month]
    month_cols <- colorRampPalette(c(pal$slate, pal$sage, pal$sand, pal$coral, pal$slate))(12)
    plot_ly(df, x = ~month, y = ~occ, type = "bar",
      marker = list(color = month_cols[df$month]),
      hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") |>
      plotly_layout(xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12), yaxis = list(title = t("occurrences")))
  })

  # ---- Top species table ----
  output$top_species_table <- renderDT({
    req(species_lookup)
    df <- species_lookup |> slice_head(n = 200)
    if (lang() == "sv" && has_vernacular) {
      df <- df |> select(any_of(c("vernacular_sv", "species", "kingdom", "class", "order", "family", "threatStatus", "total_occurrences", "n_cells")))
    } else {
      df <- df |> select(any_of(c("species", "kingdom", "class", "order", "family", "threatStatus", "total_occurrences", "n_cells")))
    }
    for (cn in c("kingdom","class","order","family","threatStatus")) if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"), style = "bootstrap4", filter = "top") |>
      formatCurrency("total_occurrences", currency = "", digits = 0)
  }, server = TRUE)

  # =================================================================
  # WHAT LIVES HERE TAB
  # =================================================================

  output$cell_map <- renderLeaflet({
    req(grid_10km)
    leaflet(grid_10km) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = "#ddd", fillOpacity = 0.3, weight = 0.3, color = "#bbb", layerId = ~eeacellcode)
  })

  observe({
    req(grid_10km, spatial_gaps, input$cell_map_var)
    sf_data <- spatial_gaps |> filter(basisofrecord == "all")

    if (input$cell_map_var == "richness" && "n_species" %in% names(sf_data)) {
      map_sf <- grid_10km |>
        left_join(sf_data |> select(eeacellcode, n_species), by = "eeacellcode") |>
        mutate(cat = case_when(
          is.na(n_species) | n_species == 0 ~ "No data",
          n_species <= 10 ~ "1\u201310", n_species <= 100 ~ "11\u2013100",
          n_species <= 1000 ~ "101\u20131,000", TRUE ~ ">1,000"),
          cat = factor(cat, levels = c("1\u201310","11\u2013100","101\u20131,000",">1,000","No data")))
      map_pal <- colorFactor(
        palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral, "#ddd"),
        domain = levels(map_sf$cat), na.color = "#ddd")
      legend_title <- t("map_richness")
    } else {
      map_sf <- grid_10km |>
        left_join(sf_data |> select(eeacellcode, occurrences), by = "eeacellcode") |>
        mutate(cat = case_when(
          is.na(occurrences) | occurrences == 0 ~ "No data",
          occurrences <= 100 ~ "1\u2013100", occurrences <= 1000 ~ "101\u20131,000",
          occurrences <= 10000 ~ "1,001\u201310,000", occurrences <= 100000 ~ "10,001\u2013100,000",
          TRUE ~ ">100,000"),
          cat = factor(cat, levels = c("1\u2013100","101\u20131,000","1,001\u201310,000","10,001\u2013100,000",">100,000","No data")))
      map_pal <- colorFactor(
        palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral, "#8b2020", "#ddd"),
        domain = levels(map_sf$cat), na.color = "#ddd")
      legend_title <- t("map_occ")
    }

    # Add region/municipality to popups
    if (has_admin) {
      map_sf <- map_sf |>
        left_join(
          cell_admin |> select(eeacellcode, admin_name_level1, any_of("admin_name_level2")),
          by = "eeacellcode")
    }

    has_region_col <- "admin_name_level1" %in% names(map_sf)
    has_muni_col <- "admin_name_level2" %in% names(map_sf)

    if (input$cell_map_var == "richness") {
      if (has_region_col) {
        map_sf$popup_text <- paste0(
          "<strong>Cell:</strong> ", map_sf$eeacellcode,
          "<br><strong>Region:</strong> ", ifelse(is.na(map_sf$admin_name_level1), "\u2014", map_sf$admin_name_level1),
          if (has_muni_col) paste0("<br><strong>Municipality:</strong> ", ifelse(is.na(map_sf$admin_name_level2), "\u2014", map_sf$admin_name_level2)) else "",
          "<br><strong>Species:</strong> ", comma(map_sf$n_species))
      } else {
        map_sf$popup_text <- paste0("<strong>Cell:</strong> ", map_sf$eeacellcode,
          "<br><strong>Species:</strong> ", comma(map_sf$n_species))
      }
    } else {
      if (has_region_col) {
        map_sf$popup_text <- paste0(
          "<strong>Cell:</strong> ", map_sf$eeacellcode,
          "<br><strong>Region:</strong> ", ifelse(is.na(map_sf$admin_name_level1), "\u2014", map_sf$admin_name_level1),
          if (has_muni_col) paste0("<br><strong>Municipality:</strong> ", ifelse(is.na(map_sf$admin_name_level2), "\u2014", map_sf$admin_name_level2)) else "",
          "<br><strong>Occurrences:</strong> ", comma(map_sf$occurrences))
      } else {
        map_sf$popup_text <- paste0("<strong>Cell:</strong> ", map_sf$eeacellcode,
          "<br><strong>Occurrences:</strong> ", comma(map_sf$occurrences))
      }
    }

    leafletProxy("cell_map", data = map_sf) |>
      clearShapes() |> clearControls() |>
      addPolygons(fillColor = ~map_pal(cat), fillOpacity = 0.6, weight = 0.3, color = "#bbb",
        layerId = ~eeacellcode, popup = ~popup_text) |>
      addLegend("bottomright", pal = map_pal, values = ~cat, title = legend_title)

    # Zoom to selected region
    sel <- input$region_filter
    if (!is.null(sel) && sel != "" && !is.null(admin_level1)) {
      region_sf <- admin_level1 |> filter(admin_name == sel)
      if (nrow(region_sf) > 0) {
        bbox <- st_bbox(region_sf)
        leafletProxy("cell_map") |>
          fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
      }
    }
  })

  # ---- Region summary card ----
  output$region_summary_card <- renderUI({
    cells <- region_cells()
    if (is.null(cells)) return(NULL)
    label <- input$municipality_filter
    if (is.null(label) || label == "") label <- input$region_filter
    n_sp <- 0; n_occ <- 0
    if (!is.null(cell_species_index)) {
      rd <- cell_species_index[eeacellcode %in% cells]
      n_sp <- uniqueN(rd$species); n_occ <- sum(as.numeric(rd$occurrences), na.rm = TRUE)
    }
    div(class = "card",
      div(class = "card-title", icon("map-pin"), label),
      div(class = "stat-grid", style = "grid-template-columns: repeat(3, 1fr);",
        div(class = "stat-box", div(class = "stat-value sage", comma(length(cells))),
          div(class = "stat-label", t("grid_cells"))),
        div(class = "stat-box", div(class = "stat-value slate", comma(n_sp)),
          div(class = "stat-label", t("species_label"))),
        div(class = "stat-box", div(class = "stat-value sand", comma(n_occ)),
          div(class = "stat-label", t("occurrences")))))
  })

  # ---- Cell click ----
  selected_cell <- reactiveVal(NULL)
  observeEvent(input$cell_map_shape_click, {
    click <- input$cell_map_shape_click
    if (!is.null(click$id)) selected_cell(click$id)
  })

  cell_species_reactive <- reactive({
    cell_id <- selected_cell(); req(cell_id, cell_species_index)
    dt <- cell_species_index[eeacellcode == cell_id]
    if (nrow(dt) == 0) return(NULL)
    setorder(dt, -occurrences); dt
  })

  output$cell_info <- renderUI({
    cell_id <- selected_cell()
    if (is.null(cell_id)) return(div(class = "empty-state", icon("mouse-pointer"), p(t("click_to_explore"))))
    df <- cell_species_reactive()
    n_sp <- if (!is.null(df)) nrow(df) else 0
    total_occ <- if (!is.null(df)) sum(df$occurrences, na.rm = TRUE) else 0
    cell_region <- ""
    if (has_admin) {
      info <- cell_admin |> filter(eeacellcode == cell_id)
      if (nrow(info) > 0) cell_region <- paste0(info$admin_name_level1[1],
        if ("admin_name_level2" %in% names(info) && !is.na(info$admin_name_level2[1])) paste0(", ", info$admin_name_level2[1]) else "")
    }
    tagList(
      div(class = "card-title", icon("list"), paste0("Cell: ", cell_id)),
      if (nchar(cell_region) > 0) div(style = "font-size:0.85rem;color:#6b6b6b;margin-bottom:0.5rem;", cell_region),
      div(class = "species-stats", style = "grid-template-columns: repeat(2, 1fr); margin-bottom:0.75rem;",
        div(class = "species-stat-item", div(class = "species-stat-num", comma(n_sp)), div(class = "species-stat-label", t("species_label"))),
        div(class = "species-stat-item", div(class = "species-stat-num", comma(total_occ)), div(class = "species-stat-label", t("occurrences")))))
  })

  output$cell_species_table <- renderDT({
    df <- cell_species_reactive(); req(df)
    df <- as_tibble(df) |>
      left_join(species_lookup |> select(any_of(c("species","order","family","threatStatus","vernacular_sv","establishmentMeans"))), by = "species")
    if (lang() == "sv" && "vernacular_sv" %in% names(df))
      df <- df |> select(any_of(c("vernacular_sv","species","occurrences","establishmentMeans","order","family","threatStatus")))
    else
      df <- df |> select(any_of(c("species","occurrences","establishmentMeans","order","family","threatStatus")))
    for (cn in c("order","family","threatStatus","establishmentMeans")) if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"), style = "bootstrap4", filter = "top")
  }, server = TRUE)

  # =================================================================
  # TAXONOMY BROWSER
  # =================================================================

  observe({ req(species_lookup)
    updateSelectInput(session, "tax_kingdom", choices = c("All", sort(unique(na.omit(species_lookup$kingdom))))) })
  observe({ req(species_lookup); df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    updateSelectInput(session, "tax_phylum", choices = c("All", sort(unique(na.omit(df$phylum))))) })
  observe({ req(species_lookup); df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "All") df <- df |> filter(phylum == input$tax_phylum)
    updateSelectInput(session, "tax_class", choices = c("All", sort(unique(na.omit(df$class))))) })
  observe({ req(species_lookup); df <- species_lookup
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "All") df <- df |> filter(phylum == input$tax_phylum)
    if (!is.null(input$tax_class) && input$tax_class != "All") df <- df |> filter(class == input$tax_class)
    updateSelectInput(session, "tax_order", choices = c("All", sort(unique(na.omit(df$order))))) })

  tax_filtered <- reactive({ req(species_lookup); df <- species_lookup
    # Apply scope filter
    scope <- input$tax_scope
    if (!is.null(scope) && scope != "all" && "establishmentMeans" %in% names(df)) {
      if (scope == "native") df <- df |> filter(establishmentMeans == "native")
      else if (scope == "introduced") df <- df |> filter(establishmentMeans %in% c("introduced", "naturalised"))
      else if (scope == "invasive") df <- df |> filter(establishmentMeans == "invasive")
    }
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") df <- df |> filter(kingdom == input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "All") df <- df |> filter(phylum == input$tax_phylum)
    if (!is.null(input$tax_class) && input$tax_class != "All") df <- df |> filter(class == input$tax_class)
    if (!is.null(input$tax_order) && input$tax_order != "All") df <- df |> filter(order == input$tax_order)
    df })

  output$tax_summary <- renderUI({
    df <- tax_filtered()
    n_threat <- sum(df$threatStatus %in% c("CR","EN","VU","NT","DD"), na.rm = TRUE)
    HTML(paste0('<div style="font-size:0.85rem;color:#6b6b6b;line-height:1.8;">',
      '<strong>', comma(nrow(df)), '</strong> ', t("species_label"), '<br>',
      '<strong>', comma(sum(df$total_occurrences, na.rm = TRUE)), '</strong> ', t("occurrences"), '<br>',
      '<strong>', comma(n_threat), '</strong> ', t("threatened"), '</div>'))
  })

  output$tax_browser_chart <- renderPlotly({
    df <- tax_filtered(); req(nrow(df) > 0)
    group_col <- if (!is.null(input$tax_order) && input$tax_order != "All") "family"
      else if (!is.null(input$tax_class) && input$tax_class != "All") "order"
      else if (!is.null(input$tax_phylum) && input$tax_phylum != "All") "class"
      else if (!is.null(input$tax_kingdom) && input$tax_kingdom != "All") "phylum"
      else "kingdom"
    chart_df <- df |> filter(!is.na(.data[[group_col]])) |>
      group_by(group = .data[[group_col]]) |>
      summarise(n_species = n(), total_occ = sum(total_occurrences, na.rm = TRUE),
        n_threatened = sum(threatStatus %in% c("CR","EN","VU","NT","DD"), na.rm = TRUE), .groups = "drop") |>
      arrange(desc(n_species)) |> slice_head(n = 30)
    plot_ly(chart_df, y = ~reorder(group, n_species), x = ~n_species, type = "bar", orientation = "h",
      marker = list(color = pal$sage), text = ~paste0(comma(total_occ), " occ"),
      hovertemplate = "<b>%{y}</b><br>Species: %{x}<br>%{text}<extra></extra>") |>
      plotly_layout(xaxis = list(title = t("species_label")), yaxis = list(title = ""))
  })

  output$tax_browser_table <- renderDT({
    df <- tax_filtered()
    if (lang() == "sv" && has_vernacular)
      df <- df |> select(any_of(c("vernacular_sv","species","kingdom","phylum","class","order","family","threatStatus","total_occurrences","n_cells")))
    else
      df <- df |> select(any_of(c("species","kingdom","phylum","class","order","family","threatStatus","total_occurrences","n_cells")))
    df <- df |> arrange(desc(total_occurrences)) |> slice_head(n = 500)
    for (cn in c("kingdom","phylum","class","order","family","threatStatus")) if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"), style = "bootstrap4", filter = "top")
  }, server = TRUE)

  # =================================================================
  # THREATENED SPECIES
  # =================================================================

  threatened_species <- reactive({
    req(species_lookup)
    if (!"threatStatus" %in% names(species_lookup)) return(tibble())
    df <- species_lookup |> filter(threatStatus %in% c("CR","EN","VU","NT","DD"))
    # Apply scope filter
    scope <- input$threat_scope
    if (!is.null(scope) && scope != "all" && "establishmentMeans" %in% names(df)) {
      if (scope == "native") df <- df |> filter(establishmentMeans == "native")
      else if (scope == "introduced") df <- df |> filter(establishmentMeans %in% c("introduced", "naturalised"))
      else if (scope == "invasive") df <- df |> filter(establishmentMeans == "invasive")
    }
    df |> arrange(factor(threatStatus, levels = c("CR","EN","VU","NT","DD")), desc(total_occurrences))
  })

  output$stat_cr2 <- renderText({ df <- threatened_species(); comma(sum(df$threatStatus == "CR")) })
  output$stat_en2 <- renderText({ df <- threatened_species(); comma(sum(df$threatStatus == "EN")) })
  output$stat_vu2 <- renderText({ df <- threatened_species(); comma(sum(df$threatStatus == "VU")) })
  output$stat_nt2 <- renderText({ df <- threatened_species(); comma(sum(df$threatStatus == "NT")) })
  output$stat_dd2 <- renderText({ df <- threatened_species(); comma(sum(df$threatStatus == "DD")) })

  output$threat_coverage_chart <- renderPlotly({
    req(tax_by_threat)
    df <- tax_by_threat |> filter(threatStatus %in% c("CR","EN","VU","NT","DD")) |>
      mutate(threatStatus = factor(threatStatus, levels = c("CR","EN","VU","NT","DD")))
    colors <- c("CR" = pal$coral, "EN" = pal$sand, "VU" = pal$slate, "NT" = pal$sage, "DD" = pal$plum)
    plot_ly(df, x = ~threatStatus, y = ~pct_coverage, type = "bar",
      marker = list(color = ~colors[as.character(threatStatus)]),
      text = ~paste0(round(pct_coverage, 1), "%"), textposition = "auto",
      textfont = list(size = 13, color = "#fff"),
      hovertemplate = "%{x}: %{y:.1f}%<extra></extra>") |>
      plotly_layout(xaxis = list(title = ""), yaxis = list(title = "Coverage (%)", range = c(0, 105)))
  })

  output$threat_stale_map <- renderLeaflet({
    req(grid_10km)
    if (!is.null(cell_recency)) {
      rec <- cell_recency |> filter(basisofrecord == "all") |> select(eeacellcode, staleness_months)
      map_sf <- grid_10km |> left_join(rec, by = "eeacellcode") |>
        mutate(stale_cat = case_when(
          is.na(staleness_months) ~ "No data",
          staleness_months <= 12 ~ "< 1 year", staleness_months <= 36 ~ "1\u20133 years",
          staleness_months <= 60 ~ "3\u20135 years", staleness_months <= 120 ~ "5\u201310 years",
          TRUE ~ "> 10 years"),
          stale_cat = factor(stale_cat, levels = c("< 1 year","1\u20133 years","3\u20135 years","5\u201310 years","> 10 years","No data")))
      stale_pal <- colorFactor(
        palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral, "#8b2020", "#ddd"),
        domain = levels(map_sf$stale_cat), na.color = "#ddd")
      m <- leaflet(map_sf) |> addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~stale_pal(stale_cat), fillOpacity = 0.6, weight = 0.3, color = "#bbb") |>
        addLegend("bottomright", pal = stale_pal, values = ~stale_cat, title = "Data recency")
      if (!is.null(admin_level1))
        m <- m |> addPolygons(data = admin_level1, group = "admin1",
          fillColor = "transparent", fillOpacity = 0, weight = 1.5, color = "#333", opacity = 0.3, label = ~admin_name)
      m
    } else {
      leaflet() |> addProviderTiles(providers$CartoDB.Positron) |> setView(16, 63, 5)
    }
  })

  output$threatened_table <- renderDT({
    df <- threatened_species()
    if (lang() == "sv" && has_vernacular)
      df <- df |> select(any_of(c("vernacular_sv","species","threatStatus","establishmentMeans","kingdom","phylum","class","order","family","total_occurrences","n_cells")))
    else
      df <- df |> select(any_of(c("species","threatStatus","establishmentMeans","kingdom","phylum","class","order","family","total_occurrences","n_cells")))
    for (cn in c("threatStatus","establishmentMeans","kingdom","phylum","class","order","family")) if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    datatable(df, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"), style = "bootstrap4", filter = "top")
  }, server = TRUE)

  output$download_threatened <- downloadHandler(
    filename = function() paste0("threatened_species_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(threatened_species(), file))

  # =================================================================
  # INVASIVE SPECIES TAB
  # =================================================================

  invasive_species <- reactive({
    req(species_lookup)
    if (!"establishmentMeans" %in% names(species_lookup)) return(tibble())
    species_lookup |> filter(establishmentMeans == "invasive") |>
      arrange(desc(total_occurrences))
  })

  output$invasive_stats <- renderUI({
    inv <- invasive_species()
    if (nrow(inv) == 0) return(NULL)
    n_total <- nrow(inv)
    n_in_gbif <- sum(inv$total_occurrences > 0, na.rm = TRUE)
    n_threatened <- sum(inv$threatStatus %in% c("CR","EN","VU","NT"), na.rm = TRUE)
    total_occ <- sum(inv$total_occurrences, na.rm = TRUE)

    div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
      div(class = "stat-box",
        div(class = "stat-value coral", comma(n_total)),
        div(class = "stat-label", "Invasive Species")),
      div(class = "stat-box",
        div(class = "stat-value sage", comma(n_in_gbif)),
        div(class = "stat-label", "With GBIF Records")),
      div(class = "stat-box",
        div(class = "stat-value sand", comma(total_occ)),
        div(class = "stat-label", "Total Occurrences")),
      div(class = "stat-box",
        div(class = "stat-value plum", comma(n_threatened)),
        div(class = "stat-label", "Also Threatened"))
    )
  })

  output$invasive_map <- renderLeaflet({
    req(grid_10km, cell_species_index, species_lookup)
    inv <- invasive_species()
    if (nrow(inv) == 0) return(leaflet() |> addProviderTiles(providers$CartoDB.Positron) |> setView(16, 63, 5))

    inv_species <- inv$species
    inv_cells <- cell_species_index[species %in% inv_species,
      .(n_invasive = uniqueN(species), occ_invasive = sum(as.numeric(occurrences), na.rm = TRUE)),
      by = eeacellcode]

    map_sf <- grid_10km |> inner_join(inv_cells, by = "eeacellcode") |>
      mutate(cat = case_when(
        n_invasive <= 1 ~ "1",
        n_invasive <= 5 ~ "2\u20135",
        n_invasive <= 10 ~ "6\u201310",
        TRUE ~ ">10"),
        cat = factor(cat, levels = c("1", "2\u20135", "6\u201310", ">10")))

    inv_pal <- colorFactor(
      palette = c(pal$sand, pal$coral, "#b03020", "#8b2020"),
      domain = levels(map_sf$cat), na.color = "#ddd")

    m <- leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~inv_pal(cat), fillOpacity = 0.7, weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
          "<br><strong>Invasive species:</strong> ", n_invasive,
          "<br><strong>Occurrences:</strong> ", comma(occ_invasive))) |>
      addLegend("bottomright", pal = inv_pal, values = ~cat, title = "Invasive species")

    if (!is.null(admin_level1))
      m <- m |> addPolygons(data = admin_level1, group = "admin1",
        fillColor = "transparent", fillOpacity = 0,
        weight = 1.5, color = "#333", opacity = 0.3, label = ~admin_name)
    m
  })

  output$invasive_trend <- renderPlotly({
    req(species_lookup, cell_species_index)
    inv <- invasive_species()
    if (nrow(inv) == 0) return(plotly_empty() |> plotly_layout())

    # Use species_time_map to get temporal data for invasive species
    if (!is.null(species_time_map)) {
      inv_time_files <- species_time_map[species %in% inv$species]
      if (nrow(inv_time_files) > 0) {
        # Sample up to 50 files to keep it fast
        files_to_read <- unique(inv_time_files$time_filepath)
        if (length(files_to_read) > 50) files_to_read <- files_to_read[1:50]
        inv_time <- rbindlist(lapply(files_to_read, function(f) {
          if (!file.exists(f)) return(NULL)
          dt <- fread(f, select = c("species", "basisofrecord", "grid", "yearmonth", "occurrences"))
          dt <- dt[species %in% inv$species & basisofrecord == "all" & grid == "grid10km"]
          dt[, year := as.integer(substr(as.character(yearmonth), 1, 4))]
          dt[year >= 1980, .(occ = sum(as.numeric(occurrences), na.rm = TRUE)), by = year]
        }))
        if (nrow(inv_time) > 0) {
          inv_time <- inv_time[, .(occ = sum(occ)), by = year]
          setorder(inv_time, year)
          return(plot_ly(inv_time, x = ~year, y = ~occ, type = "scatter", mode = "lines+markers",
            line = list(color = pal$coral, width = 2), marker = list(color = pal$coral, size = 3),
            fill = "tozeroy", fillcolor = "rgba(196,122,108,0.15)",
            hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") |>
            plotly_layout(xaxis = list(title = ""), yaxis = list(title = t("occurrences"))))
        }
      }
    }
    plotly_empty() |> plotly_layout()
  })

  output$invasive_top_chart <- renderPlotly({
    inv <- invasive_species()
    if (nrow(inv) == 0) return(plotly_empty() |> plotly_layout())
    df <- inv |> slice_head(n = 15)

    # Use vernacular names in Swedish mode
    if (lang() == "sv" && has_vernacular && "vernacular_sv" %in% names(df)) {
      df <- df |> mutate(label = ifelse(!is.na(vernacular_sv), vernacular_sv, species))
    } else {
      df <- df |> mutate(label = species)
    }

    plot_ly(df, y = ~reorder(label, total_occurrences), x = ~total_occurrences,
      type = "bar", orientation = "h",
      marker = list(color = pal$coral),
      hovertemplate = "<b>%{y}</b><br>%{x:,} occurrences<extra></extra>") |>
      plotly_layout(xaxis = list(title = t("occurrences")), yaxis = list(title = ""))
  })

  output$invasive_table <- renderDT({
    inv <- invasive_species()
    if (lang() == "sv" && has_vernacular)
      inv <- inv |> select(any_of(c("vernacular_sv","species","kingdom","class","order","family","threatStatus","total_occurrences","n_cells")))
    else
      inv <- inv |> select(any_of(c("species","kingdom","class","order","family","threatStatus","total_occurrences","n_cells")))
    for (cn in c("kingdom","class","order","family","threatStatus")) if (cn %in% names(inv)) inv[[cn]] <- as.factor(inv[[cn]])
    datatable(inv, options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"), style = "bootstrap4", filter = "top")
  }, server = TRUE)
}

shinyApp(ui = ui, server = server)
