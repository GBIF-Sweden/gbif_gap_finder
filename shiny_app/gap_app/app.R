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
library(stringr)

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
tax_by_kingdom  <- safe_get("tax_by_kingdom")
tax_by_phylum   <- safe_get("tax_by_phylum")
tax_by_class    <- safe_get("tax_by_class")
priority_zero   <- safe_get("priority_zero_cells")
priority_stale  <- safe_get("priority_stale_cells")
comparison_grids <- safe_get("comparison_grids")
metadata        <- safe_get("metadata")
spatial_overview <- safe_get("spatial_overview")

# Administrative boundaries (optional)
admin_level1     <- safe_get("admin_level1")
admin_level2     <- safe_get("admin_level2")
has_admin        <- !is.null(admin_level1) || !is.null(admin_level2)

# New: last year & Troudet data
cell_last_year       <- safe_get("cell_last_year")
overview_last_year   <- safe_get("overview_last_year")
priority_resolved    <- safe_get("priority_resolved_last_year")
troudet_bias         <- safe_get("troudet_bias")
troudet_bias_order   <- safe_get("troudet_bias_order")
troudet_bias_family  <- safe_get("troudet_bias_family")
order_time_summary   <- safe_get("order_time_summary")
family_time_summary  <- safe_get("family_time_summary")
last_year_ref        <- safe_get("last_year")  # now a yearmonth cutoff, e.g. 202504
recent_label_stored  <- safe_get("recent_label")  # e.g. "Apr 2025 – Mar 2026"

# Dyntaxa-filtered versions (for scope toggle)
dyntaxa_time_summary        <- safe_get("dyntaxa_time_summary")
dyntaxa_order_time_summary  <- safe_get("dyntaxa_order_time_summary")
dyntaxa_family_time_summary <- safe_get("dyntaxa_family_time_summary")
dyntaxa_cell_summary        <- safe_get("dyntaxa_cell_summary")
dyntaxa_cell_recency        <- safe_get("dyntaxa_cell_recency")
dyntaxa_spatial_gaps         <- safe_get("dyntaxa_spatial_gaps")
dyntaxa_basis_recent        <- safe_get("dyntaxa_basis_recent")
all_basis_recent            <- safe_get("all_basis_recent")

# Establishment means / scope data
match_summary_full   <- safe_get("taxonomic_match_summary")
tax_by_establishment <- safe_get("tax_by_establishment")
has_establishment    <- !is.null(match_summary_full) &&
                        "establishmentMeans" %in% names(match_summary_full)

# Exclude orders from analysis (e.g. Primates = Homo sapiens in Dyntaxa)
# This should match parameters.taxonomic.exclude_orders in config.yml
EXCLUDE_ORDERS <- c("Primates")

if (!is.null(match_summary_full) && length(EXCLUDE_ORDERS) > 0 &&
    "order" %in% names(match_summary_full)) {
  n_before <- nrow(match_summary_full)
  match_summary_full <- match_summary_full |>
    dplyr::filter(!order %in% EXCLUDE_ORDERS)
  n_excluded <- n_before - nrow(match_summary_full)
  if (n_excluded > 0) message("Excluded ", n_excluded, " taxa from orders: ",
                               paste(EXCLUDE_ORDERS, collapse = ", "))
}

# Dyntaxa scope / invasive species data
species_scope_lookup  <- safe_get("species_scope_lookup")
tax_by_invasive       <- safe_get("tax_by_invasive")
kingdom_cell_recency  <- safe_get("kingdom_cell_recency")
tax_cell_recency      <- safe_get("tax_cell_recency")
has_dyntaxa_scope     <- !is.null(species_scope_lookup) &&
                         "in_dyntaxa" %in% names(species_scope_lookup)

# Apply order exclusion to pre-computed tables
if (length(EXCLUDE_ORDERS) > 0) {
  filter_orders <- function(df) {
    if (!is.null(df) && "order" %in% names(df))
      df |> dplyr::filter(!order %in% EXCLUDE_ORDERS)
    else df
  }
  troudet_bias        <- filter_orders(troudet_bias)
  troudet_bias_order  <- filter_orders(troudet_bias_order)
  troudet_bias_family <- filter_orders(troudet_bias_family)
  tax_by_order        <- filter_orders(tax_by_order)
  tax_by_family       <- filter_orders(tax_by_family)
}
has_kingdom_recency   <- !is.null(kingdom_cell_recency)
has_tax_cell_recency  <- !is.null(tax_cell_recency) &&
                         "class" %in% names(tax_cell_recency)

# Kingdom choices for spatial filter
spatial_kingdom_choices <- if (has_kingdom_recency) {
  c("All kingdoms" = "", sort(unique(kingdom_cell_recency$kingdom)))
} else character(0)

# Build scope choices
scope_choices <- c("All species" = "all")
if (has_establishment) {
  scope_choices <- c(scope_choices,
    "Present only" = "present",
    "Native (present)" = "native_present",
    "Introduced (present)" = "introduced_present",
    "Invasive" = "invasive")
}

# Derived
basis_types   <- if (!is.null(spatial_gaps)) sort(unique(spatial_gaps$basisofrecord)) else "all"
basis_types_no_all <- basis_types[basis_types != "all"]
order_choices <- if (!is.null(top_orders)) top_orders$order else character(0)
current_year  <- year(Sys.Date())

# Try to read country name from config
country_name <- tryCatch({
  config_files <- c("../../configs/config_SE.yml", "../../config.yml")
  found <- ""
  for (cf in config_files) {
    if (file.exists(cf)) { found <- yaml::read_yaml(cf)$country$name; break }
  }
  if (is.null(found)) "" else found
}, error = function(e) "")

# Cascading filter choices (for taxonomic tab)
kingdom_choices <- if (!is.null(tax_by_order) && "kingdom" %in% names(tax_by_order)) {
  sort(unique(tax_by_order$kingdom[!is.na(tax_by_order$kingdom) & tax_by_order$kingdom != ""]))
} else character(0)

# Label for recent period (last 12 months)
last_year_label <- if (!is.null(recent_label_stored)) {
  recent_label_stored
} else if (!is.null(last_year_ref)) {
  as.character(last_year_ref)
} else {
  "last 12 months"
}

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
  
  p <- do.call(layout, c(list(
    p = p,
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#fafaf7",
    font = list(color = "#2d2d2d", family = "Outfit"),
    margin = list(l = 60, r = 30, t = 40, b = 60)
  ), args))
  
  # Apply reduced toolbar to every plotly chart
  p |> plotly::config(
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c(
      "zoom2d", "pan2d", "lasso2d", "select2d", "autoScale2d",
      "hoverCompareCartesian", "hoverClosestCartesian",
      "toggleSpikelines"
    ),
    toImageButtonOptions = list(
      format = "png", width = 1400, height = 800, scale = 2,
      filename = "gbifgaps_chart"
    )
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
      div(style = "display:flex; align-items:center; gap:1.5rem;",
        if (!is.null(metadata)) tagList(
          span("Prepared: ", span(class = "header-stat-value",
            format(metadata$created_at, "%d %b %Y"))),
          tags$a(href = paste0("https://www.gbif.org/dataset/search?publishingCountry=",
              if (!is.null(metadata$country_code)) metadata$country_code else "SE"),
            target = "_blank", style = "text-decoration: none; color: inherit;",
            span("Datasets: ", span(class = "header-stat-value", metadata$n_datasets)))
        ),
        div(style = "display:flex; align-items:center; gap:0.4rem;",
          span(style = "font-size:0.8rem; color:#6b6b6b;", "Record type:"),
          selectInput("basis_filter", NULL,
            choices = basis_types,
            selected = "all",
            width = "180px")),
        if (has_dyntaxa_scope) div(style = "display:flex; align-items:center; gap:0.4rem;",
          span(style = "font-size:0.8rem; color:#6b6b6b;", "Scope:"),
          selectInput("dyntaxa_scope", NULL,
            choices = c(
              "Dyntaxa species (gap analysis)" = "dyntaxa",
              "All GBIF Sweden (overview)" = "all_gbif"),
            selected = "dyntaxa",
            width = "240px"))
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

          # About section
          div(class = "card", style = "margin-bottom: 1.25rem; border-left: 4px solid var(--sage);",
            div(class = "card-title", icon("info-circle"), "About This Dashboard"),
            div(style = "font-size: 1rem; line-height: 1.7; color: var(--text-secondary);",
              p("This dashboard analyses gaps in GBIF occurrence data using a national taxonomy backbone as reference.",
                "It identifies spatial, temporal, and taxonomic gaps to guide data mobilisation priorities."),
              tags$ul(style = "margin: 0.5rem 0;",
                tags$li(tags$strong("Taxonomy backbone: "), metadata$taxonomy_name %||% "National checklist",
                  if (!is.null(metadata$taxonomy_doi)) tagList(" — ", tags$a(href = metadata$taxonomy_doi, "DOI", target = "_blank"))),
                tags$li(tags$strong("Threat status: "), "Swedish Red List (authoritative source for CR, EN, VU, NT, DD)"),
                tags$li(tags$strong("Grids: "), "EEA reference grids at 10 km resolution (EPSG:3035)"),
                tags$li(tags$strong("Scope filter: "), "Use the scope dropdown on Taxonomic and Threatened tabs to filter by native/introduced/invasive status")
              ),
              p(style = "font-size: 0.9rem; color: var(--text-muted); margin-top: 0.5rem;",
                "Data last updated: ", metadata$build_date %||% "unknown",
                " | Pipeline: gbifgaps")
            )
          ),

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

          # Establishment means breakdown (if available)
          if (has_establishment) div(class = "card",
            div(class = "card-title", icon("seedling"), "Species by Establishment Means"),
            div(class = "info-note",
              "Coverage breakdown by origin. ",
              em("Unclassified"), " species (no establishment means in the backbone) ",
              "inflate gap numbers — use the Scope filter in the Taxonomic tab to focus on native or introduced species."),
            plotlyOutput("overview_establishment", height = "220px")
          ),

          # Last year highlight row
          div(class = "card", style = "margin-bottom: 1rem;",
            div(class = "card-title", icon("calendar-plus"),
              paste0("Year in Review: ", last_year_label)),
            div(class = "info-note", style = "margin-bottom: 0.75rem;",
              "Observations ", tags$strong("dated"), " in this period. Records published to GBIF during this time may cover earlier observation dates."),
            div(class = "stat-grid", style = "grid-template-columns: repeat(5, 1fr);",
              div(class = "stat-box",
                div(class = "stat-value sage", textOutput("ov_ly_occ", inline = TRUE)),
                div(class = "stat-label", paste0("Observations Dated (", last_year_label, ")"))),
              div(class = "stat-box",
                div(class = "stat-value plum", textOutput("ov_ly_published", inline = TRUE)),
                div(class = "stat-label", paste0("Published to GBIF (", last_year_label, ")"))),
              div(class = "stat-box",
                div(class = "stat-value slate", textOutput("ov_ly_cells", inline = TRUE)),
                div(class = "stat-label", "Cells Active")),
              div(class = "stat-box",
                div(class = "stat-value sand", textOutput("ov_ly_new_cells", inline = TRUE)),
                div(class = "stat-label", "Newly Covered Cells")),
              div(class = "stat-box",
                div(class = "stat-value coral", textOutput("ov_ly_resolved", inline = TRUE)),
                div(class = "stat-label", "Priority Cells Resolved"))
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

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--sage);",
            actionLink("spatial_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.spatial_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("This tab shows how biodiversity observations are distributed across Sweden's ",
                  "10 km grid cells. Each cell is coloured by the selected metric: total occurrences, ",
                  "data recency (how recently each cell was surveyed), species richness, or observations ",
                  "from the last 12 months."),
                p("Use the ", tags$strong("Kingdom filter"), " to isolate specific taxonomic groups. ",
                  "Bird observations dominate Swedish GBIF data, so filtering to non-Aves groups ",
                  "can reveal sampling gaps that are otherwise hidden."),
                p(tags$strong("Data recency"), " shows how stale each cell's most recent observation is. ",
                  "Red cells have not been surveyed in over 10 years and should be prioritised for resurvey.")
              )
            )
          ),

          fluidRow(
            column(8, div(class = "card",
              div(class = "card-title", icon("globe-europe"), "Geographic Coverage"),
              leafletOutput("spatial_map", height = "520px"))),
            column(4,
              div(class = "card",
                div(class = "card-title", icon("sliders-h"), "Display"),
                radioButtons("map_var", NULL,
                  choices = setNames(
                    c("occ", "stale", "richness", "last_year_obs", "last_year_pub"),
                    c("Occurrences", "Data recency", "Species richness",
                      paste0("Observed (", last_year_label, ")"),
                      paste0("Published (", last_year_label, ")"))),
                  selected = "occ"),
                if (has_kingdom_recency) tagList(
                  tags$hr(style = "margin: 0.5rem 0; border-color: #eee;"),
                  div(class = "filter-label", "Taxonomic filter"),
                  uiOutput("spatial_kingdom_filter_ui"),
                  uiOutput("spatial_class_filter_ui")
                )),
              if (has_admin) div(class = "card",
                div(class = "card-title", icon("border-all"), "Administrative Boundaries"),
                if (!is.null(admin_level1)) checkboxInput("show_admin1", "Show regions", value = TRUE),
                if (!is.null(admin_level2)) checkboxInput("show_admin2", "Show municipalities", value = FALSE)
              ),
              div(class = "card",
                div(class = "card-title", icon("info-circle"), "Statistics"),
                tableOutput("spatial_stats")))
          ),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("th"), "Grid Comparison"),
              plotlyOutput("spatial_grid", height = "260px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-area"), "Occurrence Distribution (10km cells)"),
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

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--slate);",
            actionLink("temporal_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.temporal_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("This tab shows when biodiversity observations were made. The ", tags$strong("historical trend"),
                  " shows total occurrences per year; the ", tags$strong("seasonal pattern"),
                  " reveals monthly collection biases."),
                p("The ", tags$strong("heatmap"), " shows year \u00d7 month intensity. ",
                  "Switch between log scale (better for spotting patterns across orders of magnitude) ",
                  "and linear scale (better for comparing absolute numbers). ",
                  "Use the taxonomic filters above to isolate specific groups."),
                p("The sharp increase in recent decades is largely driven by citizen science ",
                  "(especially Artportalen/iNaturalist). Filtering by kingdom or order can ",
                  "reveal which groups are driving temporal trends.")
              )
            )
          ),

          div(class = "filter-section",
            fluidRow(
              column(2,
                div(class = "filter-label", "Year Range"),
                sliderInput("year_range", NULL, min = 1900, max = current_year,
                  value = c(1970, current_year), step = 1, sep = "")),
              column(2,
                div(class = "filter-label", "Kingdom"),
                selectInput("temp_kingdom", NULL,
                  choices = c("All" = "", kingdom_choices), selected = "")),
              column(2,
                div(class = "filter-label", "Phylum"),
                uiOutput("temp_phylum_ui")),
              column(2,
                div(class = "filter-label", "Class"),
                uiOutput("temp_class_ui")),
              column(2,
                div(class = "filter-label", "Order"),
                uiOutput("temp_order_ui")),
              column(2,
                div(class = "filter-label", "Family"),
                uiOutput("temp_family_ui"))
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
              div(style = "display:flex; align-items:center; justify-content:space-between;",
                div(class = "card-title", icon("th"), "Year \u00d7 Month Heatmap"),
                radioButtons("heatmap_scale", NULL,
                  choices = c("Log scale" = "log", "Linear" = "linear", "Binned" = "binned"),
                  selected = "log", inline = TRUE)
              ),
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

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--sand);",
            actionLink("basis_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.basis_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("Each GBIF occurrence record has a ", tags$strong("basis of record"),
                  " describing how the observation was made. The main types are: ",
                  tags$em("Human Observation"), " (field sightings, citizen science), ",
                  tags$em("Preserved Specimen"), " (museum/herbarium collections), ",
                  tags$em("Machine Observation"), " (camera traps, acoustic sensors), ",
                  tags$em("Fossil Specimen"), " (paleontological collections), and ",
                  tags$em("Material Sample"), " (DNA, tissue, environmental samples)."),
                p(tags$strong("Occurrences by Basis of Record"), " shows the overall share of each record type. ",
                  "Use the 'Last 12 Months' toggle to see how recent data collection compares to historical records. ",
                  "When toggled on, bars show prior records (faded) and recent additions (solid)."),
                p(tags$strong("Temporal Trend"), " shows the time series for a single selected basis type. ",
                  "The sharp increase in Human Observations since ~2010 reflects the growth of citizen science ",
                  "(primarily Artportalen and iNaturalist)."),
                p(tags$strong("Spatial Coverage"), " shows what percentage of Sweden's 10 km grid cells have at least ",
                  "one record of each type. Human Observations cover the most cells; museum specimens are concentrated ",
                  "in fewer areas."),
                p(tags$strong("Species Coverage"), " shows the total number of species detections summed across all cells. ",
                  "Note: a species recorded in 3 cells counts as 3, not 1. This measures sampling breadth, not unique species count."),
                p(tags$strong("Spatial Distribution"), " maps where each basis type has data, using binned occurrence categories.")
              )
            )
          ),

          uiOutput("basis_stat_boxes"),

          # Last 12 months toggle
          div(class = "filter-section", style = "margin-bottom: 1rem;",
            div(style = "display: flex; align-items: center; gap: 1.5rem;",
              div(class = "filter-label", style = "margin-bottom: 0; white-space: nowrap;",
                icon("calendar-alt", style = "margin-right: 0.3rem;"), "HIGHLIGHT LAST 12 MONTHS"),
              radioButtons("basis_last_year_mode", NULL,
                choices = setNames(
                  c("off", "observed", "published"),
                  c("Off",
                    paste0("Observed (", last_year_label, ")"),
                    paste0("Published to GBIF (", last_year_label, ")"))),
                selected = "off", inline = TRUE)
            )
          ),

          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-pie"), "Occurrences by Basis of Record"),
              plotlyOutput("basis_pie", height = "380px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-line"), "Temporal Trend by Basis"),
              selectInput("basis_timeline_select", "Select basis:",
                choices = basis_types_no_all, width = "250px"),
              plotlyOutput("basis_timeline", height = "330px")))
          ),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("map"), "Spatial Coverage by Basis of Record"),
              plotlyOutput("basis_spatial_bar", height = "340px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("dna"), "Unique Species by Basis of Record"),
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

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--sand);",
            actionLink("taxonomic_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.taxonomic_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("This tab compares GBIF occurrence data against the national taxonomy backbone (Dyntaxa). ",
                  "For each taxonomic group, it shows how many known species have GBIF records and ",
                  "how sampling effort is distributed across groups."),
                p("The ", tags$strong("Taxonomic Bias"), " chart (Troudet-style) reveals whether groups are ",
                  "over- or under-represented relative to their known species richness. ",
                  "If a group has 10% of all known species but only 1% of all occurrences, it is under-sampled."),
                p("Use the ", tags$strong("Scope filter"), " to focus on native, introduced, or invasive species. ",
                  "The ", tags$strong("Last 12 Months"), " toggle highlights recent sampling effort, ",
                  "showing whether recent data collection is addressing historical biases or reinforcing them."),
                p(tags$strong("Note:"), " Gap metrics are only meaningful when the Dyntaxa scope is active. ",
                  "When viewing 'All GBIF Sweden', completeness percentages are not shown because there is no ",
                  "reference checklist to measure against for non-Dyntaxa taxa.")
              )
            )
          ),

          # Scope info banner (shown when in All GBIF mode)
          uiOutput("scope_info_banner"),

          # Cascading taxonomy filters
          div(class = "filter-section",
            fluidRow(
              column(2,
                div(class = "filter-label", "Kingdom"),
                selectInput("tax_kingdom", NULL,
                  choices = c("All" = "", kingdom_choices),
                  selected = "")),
              column(2,
                div(class = "filter-label", "Phylum"),
                uiOutput("tax_phylum_ui")),
              column(2,
                div(class = "filter-label", "Class"),
                uiOutput("tax_class_ui")),
              column(2,
                div(class = "filter-label", "Order"),
                uiOutput("tax_order_filter_ui")),
              column(2,
                div(class = "filter-label", "Family"),
                uiOutput("tax_family_filter_ui")),
              if (has_establishment) column(2,
                div(class = "filter-label", "Scope"),
                selectInput("tax_scope", NULL,
                  choices = scope_choices, selected = "present"))
            ),
            # Separate row: last 12 months toggle
            tags$hr(style = "margin: 0.75rem 0; border-color: var(--border-light);"),
            div(style = "display: flex; align-items: center; gap: 1.5rem;",
              div(class = "filter-label", style = "margin-bottom: 0; white-space: nowrap;",
                icon("calendar-alt", style = "margin-right: 0.3rem;"), "HIGHLIGHT LAST 12 MONTHS"),
              radioButtons("tax_last_year_mode", NULL,
                choices = setNames(
                  c("off", "observed", "published"),
                  c("Off",
                    paste0("Observed (", last_year_label, ")"),
                    paste0("Published to GBIF (", last_year_label, ")"))),
                selected = "off", inline = TRUE)
            )
          ),

          # Taxonomy reference info
          div(class = "info-note", style = "margin-bottom: 1rem;",
            icon("book", style = "margin-right: 0.3rem;"),
            "Taxonomy backbone: ", tags$strong(metadata$taxonomy_name %||% "Dyntaxa"),
            " — ",
            tags$a(href = "https://www.gbif.org/dataset/de8934f4-a136-481c-a87a-b0b202b80a31",
              "View on GBIF", target = "_blank", style = "color: var(--sage);"),
            " | ",
            tags$a(href = "https://namnochslansen.artfakta.se/",
              "Browse Dyntaxa", target = "_blank", style = "color: var(--sage);"),
            ". The scope filter above controls which species are included (present, native, introduced, invasive)."
          ),

          # Active filter breadcrumb
          uiOutput("tax_filter_breadcrumb"),

          # Troudet-style bias figure
          div(class = "card",
            div(class = "card-title", icon("balance-scale"), "Taxonomic Bias in Occurrence Data"),
            div(class = "info-note",
              "Deviation from proportional sampling: if a group has ", em("p"), "% of all known species, ",
              "it should ideally have ", em("p"), "% of all occurrences. ",
              "Green = over-represented, red = under-represented. ",
              "Drill down using the filters — the chart auto-adjusts to class, order, or family level."),
            plotlyOutput("troudet_bias_chart", height = "500px")),

          # Coverage: species count vs coverage %
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("layer-group"), "Species Count by Order"),
              plotlyOutput("tax_order", height = "450px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("folder-tree"), "Coverage (%) by Family"),
              plotlyOutput("tax_family", height = "450px")))
          ),

          # Recent vs Historical — as info cards
          div(class = "card",
            div(class = "card-title", icon("exchange-alt"), "Recent vs Historical Sampling Intensity"),
            div(class = "info-note",
              strong("Baseline: "), "Historical = all records before 2000. ",
              "Recent = records from 2000 onwards. ",
              "Filtered by the taxonomy selections above."),
            uiOutput("tax_change_cards"))
        )
      ),

      # =====================================================================
      # THREATENED TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("exclamation-triangle"), "Threatened"),
        value = "threatened",
        div(style = "padding: 1.25rem 0;",

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--coral);",
            actionLink("threatened_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.threatened_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("This tab shows coverage of species on the ", tags$strong("Swedish Red List"),
                  " — the national assessment of extinction risk. Categories range from ",
                  tags$strong("CR"), " (Critically Endangered, highest risk) through ",
                  tags$strong("DD"), " (Data Deficient, insufficient information to assess)."),
                p("A species is 'missing' if it appears on the Red List but has no matching GBIF occurrence records. ",
                  "Missing CR and EN species are the highest conservation data priorities — ",
                  "without occurrence data, it is impossible to track population trends or model habitat suitability."),
                p("Use the taxonomic filters to identify which groups have the largest gaps in threatened species coverage.")
              )
            )
          ),

          div(class = "stat-grid", style = "grid-template-columns: repeat(5, 1fr);",
            div(class = "stat-box",
              div(class = "stat-value coral", textOutput("stat_cr", inline = TRUE)),
              div(class = "stat-label", "CR = Critically Endangered")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("stat_en", inline = TRUE)),
              div(class = "stat-label", "EN = Endangered")),
            div(class = "stat-box",
              div(class = "stat-value", style = "color:#b8a060;", textOutput("stat_vu", inline = TRUE)),
              div(class = "stat-label", "VU = Vulnerable")),
            div(class = "stat-box",
              div(class = "stat-value sage", textOutput("stat_nt", inline = TRUE)),
              div(class = "stat-label", "NT = Near Threatened")),
            div(class = "stat-box",
              div(class = "stat-value slate", textOutput("stat_dd", inline = TRUE)),
              div(class = "stat-label", "DD = Data Deficient"))
          ),
          div(class = "filter-section",
            fluidRow(
              column(2, selectInput("threat_kingdom", "Kingdom",
                choices = c("All" = "", kingdom_choices), selected = "")),
              column(2, uiOutput("threat_phylum_ui")),
              column(2, uiOutput("threat_class_ui")),
              column(2, uiOutput("threat_order_ui")),
              column(2,
                if (has_establishment) selectInput("threat_scope", "Scope",
                  choices = scope_choices, selected = "present")
              )
            )),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("shield-alt"), "Coverage by Threat Status"),
              plotlyOutput("threat_coverage", height = "300px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("times-circle"), "Missing Taxa by Status"),
              plotlyOutput("threat_missing", height = "300px")))
          ),
          div(class = "card",
            div(class = "card-title", icon("list"), "Missing Threatened Species"),
            div(class = "info-note",
              "Species in the national taxonomy backbone with a threat status ",
              "that have no matching GBIF occurrence records. ",
              "Includes CR, EN, VU, NT and DD (Data Deficient). ",
              "Use the filters above and column filters below to narrow results."),
            DTOutput("threat_table"))
        )
      ),

      # =====================================================================
      # PUBLISHER TAB (NEW)
      # =====================================================================
      tabPanel(
        title = tagList(icon("building"), "Publishers"),
        value = "publishers",
        div(style = "padding: 1.25rem 0;",

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--plum);",
            actionLink("publisher_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.publisher_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("This tab shows which organisations contribute occurrence data to GBIF for this country. ",
                  "Understanding publisher composition helps assess data infrastructure resilience."),
                p(tags$strong("Single-publisher cells"), " are geographically fragile: if that organisation stops contributing, ",
                  "the cell loses all coverage. A healthy data infrastructure has multiple publishers per cell."),
                p("The ", tags$strong("Published to GBIF Over Time"), " chart shows when records were ",
                  tags$em("added to GBIF"), " (mobilisation date), not when they were observed. ",
                  "This reveals the pace of data mobilisation and whether it is accelerating or stalling.")
              )
            )
          ),

          # Publisher stats
          div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
            div(class = "stat-box",
              div(class = "stat-value sage", textOutput("pub_n_publishers", inline = TRUE)),
              div(class = "stat-label", "Publishers")),
            div(class = "stat-box",
              div(class = "stat-value slate", textOutput("pub_n_datasets", inline = TRUE)),
              div(class = "stat-label", "Datasets")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("pub_single_cells", inline = TRUE)),
              div(class = "stat-label", "Single-Publisher Cells")),
            div(class = "stat-box",
              div(class = "stat-value coral", textOutput("pub_top_pct", inline = TRUE)),
              div(class = "stat-label", "Top Publisher Share"))
          ),

          div(class = "info-note", style = "margin-bottom: 1rem;",
            "Which organisations contribute GBIF data for this country? ",
            "Cells served by a single publisher are fragile — if that publisher stops contributing, the cell loses all coverage."),

          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-bar"), "Top Publishers by Occurrences"),
              plotlyOutput("pub_top_chart", height = "450px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-pie"), "Top Publishers by Species Coverage"),
              plotlyOutput("pub_species_chart", height = "450px")))
          ),

          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("map"), "Publisher Dependency per Cell"),
              div(class = "info-note", "Cells coloured by the number of publishers contributing data. ",
                tags$span(style = "color: var(--coral);", "Red cells"), " depend on a single publisher."),
              leafletOutput("pub_dependency_map", height = "450px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("calendar-alt"), "Published to GBIF Over Time"),
              div(class = "info-note", "When records were published (added to GBIF), not when they were observed."),
              plotlyOutput("pub_time_chart", height = "450px")))
          ),

          div(class = "card",
            div(class = "card-title", icon("table"), "All Publishers"),
            DTOutput("pub_table"))
        )
      ),

      # =====================================================================
      # PRIORITIES TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("bullseye"), "Priorities"),
        value = "priorities",
        div(style = "padding: 1.25rem 0;",

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--coral);",
            actionLink("priorities_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.priorities_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 0.9rem; line-height: 1.6; color: var(--text-secondary);",
                p("This tab synthesises findings from the spatial, temporal, and taxonomic analyses into ",
                  tags$strong("actionable priorities"), " for data mobilisation."),
                p(tags$strong("Zero-coverage cells"), " have never been surveyed and are the highest spatial priority. ",
                  tags$strong("Stale cells"), " have data older than 5 years and need resurvey to track change. ",
                  tags$strong("Missing threatened species"), " (CR/EN) lack any GBIF records and are ",
                  "critical for conservation assessment."),
                p("The ", tags$strong("Next 12 Months"), " section projects realistic targets based on recent performance, ",
                  "setting goals at 1.5\u00d7 the rate achieved in the last 12 months.")
              )
            )
          ),

          # Recommended Actions — one card per gap dimension
          div(class = "card",
            div(class = "card-title", icon("tasks"), "Recommended Actions"),
            div(class = "info-note",
              "Concrete goals derived from the gap analysis. Each action targets a specific dimension ",
              "of data completeness with measurable outcomes."),
            uiOutput("action_goals")),

          # Next 12 Months — based on last 12 months performance
          div(class = "card",
            div(class = "card-title", icon("chart-line"),
              paste0("Next 12 Months — Based on ", last_year_label, " Performance")),
            div(class = "info-note",
              "What was achieved in the last 12 months, and what could be targeted next. ",
              "Targets are set at 1.5\u00d7 the recent rate to encourage growth."),
            uiOutput("next_12_months")),

          # Export
          div(class = "card",
            div(style = "display:flex; align-items:center; justify-content:space-between;",
              div(
                div(class = "card-title", icon("download"), "Export Action Plan"),
                div(style = "font-size:0.85rem; color:#6b6b6b;",
                  "Download all priority items as a single CSV.")),
              div(style = "padding-left:1rem;",
                downloadButton("download_action_plan", "Download CSV",
                  class = "btn-download", style = "white-space:nowrap;"))
            )),

          # Maps: zero coverage + stale cells
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
          ),

          # Taxonomic mobilization targets
          div(class = "card",
            div(class = "card-title", icon("seedling"), "Taxonomic Mobilization Targets"),
            div(class = "info-note",
              "Orders and families with the largest gap between known species and GBIF coverage."),
            fluidRow(
              column(6, plotlyOutput("priority_undersampled_orders", height = "380px")),
              column(6, plotlyOutput("priority_undersampled_families", height = "380px"))
            ))
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
                    "Coverage by Kingdom"  = "tax_by_kingdom",
                    "Coverage by Phylum"   = "tax_by_phylum",
                    "Coverage by Class"    = "tax_by_class",
                    "Coverage by Order"    = "tax_by_order",
                    "Coverage by Family"   = "tax_by_family",
                    "Coverage by Rank"     = "tax_by_rank",
                    "Troudet Bias (Class)" = "troudet_bias",
                    "Troudet Bias (Order)" = "troudet_bias_order",
                    "Cell Last Year"       = "cell_last_year",
                    "Order Summary"        = "order_summary",
                    "Priority Zero Cells"  = "priority_zero_cells",
                    "Priority Stale Cells" = "priority_stale_cells",
                    "Resolved Last Year"   = "priority_resolved_last_year",
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

  # Dyntaxa scope reactive: TRUE = gap analysis mode, FALSE = all GBIF overview
  dyntaxa_mode <- reactive({
    scope <- input$dyntaxa_scope
    is.null(scope) || scope == "dyntaxa"
  })

  # Info banner when in "All GBIF" mode (shown at top of relevant tabs)
  output$scope_info_banner <- renderUI({
    if (!dyntaxa_mode()) {
      div(class = "card", style = "margin-bottom: 1rem; background: #fff8e1; border-left: 4px solid #f0ad4e;",
        div(style = "display:flex; align-items:center; gap:0.5rem; padding: 0.5rem 0;",
          icon("info-circle", style = "color: #f0ad4e; font-size: 1.2rem;"),
          div(style = "font-size: 0.9rem; color: #6b6b6b;",
            tags$strong("All GBIF Sweden mode:"),
            " Showing all occurrence data including taxa outside the Dyntaxa backbone. ",
            "Gap analysis metrics (completeness %, missing species) require the Dyntaxa scope ",
            "and are not shown in this view."
          )
        )
      )
    }
  })

  # ===================================================================
  # TEMPORAL — with taxonomy filters via order_temporal
  # ===================================================================

  # Build order→taxonomy mapping for temporal filtering
  order_tax_map <- if (!is.null(tax_by_order) && all(c("kingdom", "phylum", "class", "order") %in% names(tax_by_order))) {
    tax_by_order |> distinct(kingdom, phylum, class, order)
  } else NULL

  # Family→taxonomy mapping
  family_tax_map <- if (!is.null(tax_by_family) && all(c("kingdom", "phylum", "class", "order", "family") %in% names(tax_by_family))) {
    tax_by_family |> distinct(kingdom, phylum, class, order, family)
  } else NULL

  # Temporal taxonomy cascade — Phylum
  output$temp_phylum_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(order_tax_map) && !is.null(input$temp_kingdom) && input$temp_kingdom != "") {
      ph <- order_tax_map |> filter(kingdom == input$temp_kingdom) |> pull(phylum) |> unique() |> sort()
      ch <- c("All" = "", setNames(ph, ph))
    }
    selectInput("temp_phylum", NULL, choices = ch, selected = "")
  })

  # Temporal taxonomy cascade — Class
  output$temp_class_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(order_tax_map)) {
      df <- order_tax_map
      if (!is.null(input$temp_kingdom) && input$temp_kingdom != "") df <- df |> filter(kingdom == input$temp_kingdom)
      if (!is.null(input$temp_phylum) && input$temp_phylum != "") df <- df |> filter(phylum == input$temp_phylum)
      cl <- sort(unique(df$class))
      ch <- c("All" = "", setNames(cl, cl))
    }
    selectInput("temp_class", NULL, choices = ch, selected = "")
  })

  # Temporal taxonomy cascade — Order
  output$temp_order_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(order_tax_map)) {
      df <- order_tax_map
      if (!is.null(input$temp_kingdom) && input$temp_kingdom != "") df <- df |> filter(kingdom == input$temp_kingdom)
      if (!is.null(input$temp_phylum) && input$temp_phylum != "") df <- df |> filter(phylum == input$temp_phylum)
      if (!is.null(input$temp_class) && input$temp_class != "") df <- df |> filter(class == input$temp_class)
      ord <- sort(unique(df$order))
      if (length(ord) <= 200) ch <- c("All" = "", setNames(ord, ord))
    }
    selectInput("temp_order", NULL, choices = ch, selected = "")
  })

  # Temporal taxonomy cascade — Family
  output$temp_family_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(family_tax_map)) {
      df <- family_tax_map
      if (!is.null(input$temp_kingdom) && input$temp_kingdom != "") df <- df |> filter(kingdom == input$temp_kingdom)
      if (!is.null(input$temp_phylum) && input$temp_phylum != "") df <- df |> filter(phylum == input$temp_phylum)
      if (!is.null(input$temp_class) && input$temp_class != "") df <- df |> filter(class == input$temp_class)
      if (!is.null(input$temp_order) && input$temp_order != "") df <- df |> filter(order == input$temp_order)
      fam <- sort(unique(df$family))
      if (length(fam) <= 200) ch <- c("All" = "", setNames(fam, fam))
    }
    selectInput("temp_family", NULL, choices = ch, selected = "")
  })

  # Helper: get filtered orders based on temporal taxonomy selections
  temp_filtered_orders <- reactive({
    if (is.null(order_tax_map)) return(NULL)
    df <- order_tax_map
    if (!is.null(input$temp_kingdom) && input$temp_kingdom != "") df <- df |> filter(kingdom == input$temp_kingdom)
    if (!is.null(input$temp_phylum) && input$temp_phylum != "") df <- df |> filter(phylum == input$temp_phylum)
    if (!is.null(input$temp_class) && input$temp_class != "") df <- df |> filter(class == input$temp_class)
    if (!is.null(input$temp_order) && input$temp_order != "") df <- df |> filter(order == input$temp_order)
    df$order
  })

  # Helper: get filtered families
  temp_filtered_families <- reactive({
    if (is.null(family_tax_map)) return(NULL)
    if (is.null(input$temp_family) || input$temp_family == "") return(NULL)
    input$temp_family
  })

  # Temporal data: use Dyntaxa-filtered versions when scope toggle is active.
  # Within each scope, use family_time_summary if family filter active,
  # order_time_summary if order/class/phylum/kingdom active, else time_summary.
  temp_data <- reactive({
    sel_family <- temp_filtered_families()
    orders <- temp_filtered_orders()
    has_tax_filter <- !is.null(input$temp_kingdom) && input$temp_kingdom != ""

    # Select data source based on Dyntaxa scope toggle
    use_dyntaxa <- dyntaxa_mode()

    ts       <- if (use_dyntaxa && !is.null(dyntaxa_time_summary)) dyntaxa_time_summary else time_summary
    ots      <- if (use_dyntaxa && !is.null(dyntaxa_order_time_summary)) dyntaxa_order_time_summary else order_time_summary
    fts      <- if (use_dyntaxa && !is.null(dyntaxa_family_time_summary)) dyntaxa_family_time_summary else family_time_summary

    if (!is.null(sel_family) && !is.null(fts)) {
      # Family-level filtering
      fts |>
        filter(basisofrecord == "all",
               family == sel_family,
               year >= input$year_range[1],
               year <= input$year_range[2])
    } else if (has_tax_filter && !is.null(ots) && !is.null(orders)) {
      # Order-level filtering
      ots |>
        filter(basisofrecord == "all",
               order %in% orders,
               year >= input$year_range[1],
               year <= input$year_range[2])
    } else {
      req(ts)
      ts |>
        filter(basisofrecord == basis_selected(),
               year >= input$year_range[1],
               year <= input$year_range[2])
    }
  })

  output$temporal_trend <- renderPlotly({
    df <- temp_data()
    yearly <- df |> group_by(year) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")

    plot_ly(yearly, x = ~year, y = ~occ, type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = paste0(pal$sage, "33"),
      line = list(color = pal$sage, width = 2)) |>
      plotly_layout(
        xaxis = list(title = "", range = c(input$year_range[1], input$year_range[2])),
        yaxis = list(title = "Occurrences"))
  })

  output$temporal_season <- renderPlotly({
    df <- temp_data()
    monthly <- df |> filter(!is.na(month)) |> group_by(month) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")

    if (nrow(monthly) == 0) {
      return(plotly_empty() |> plotly_layout(
        annotations = list(list(text = "No monthly data available",
          showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))))
    }

    month_cols <- colorRampPalette(c(pal$slate, pal$sage, pal$sand, pal$coral))(12)
    plot_ly(monthly, x = ~month, y = ~occ, type = "bar",
      marker = list(color = month_cols)) |>
      plotly_layout(
        xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12),
        yaxis = list(title = ""))
  })

  output$temporal_heatmap <- renderPlotly({
    df <- temp_data()
    hm <- df |> filter(!is.na(month)) |> group_by(year, month) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      mutate(log_occ = log10(occ + 1))

    if (nrow(hm) == 0) {
      return(plotly_empty() |> plotly_layout(
        annotations = list(list(text = "No monthly data available",
          showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))))
    }

    # Scale selection
    scale_mode <- if (!is.null(input$heatmap_scale)) input$heatmap_scale else "log"

    heatmap_cs <- list(
      list(0, "#f6f5f1"), list(0.25, "#c8dbc6"),
      list(0.5, pal$sage), list(0.75, pal$sand),
      list(1, pal$coral))

    if (scale_mode == "binned") {
      # Categorical bins like the spatial histogram
      hm <- hm |> mutate(
        occ_bin = case_when(
          occ == 0 ~ 0L,
          occ <= 10 ~ 1L,
          occ <= 100 ~ 2L,
          occ <= 1000 ~ 3L,
          occ <= 10000 ~ 4L,
          occ <= 100000 ~ 5L,
          TRUE ~ 6L
        ),
        occ_label = case_when(
          occ == 0 ~ "0",
          occ <= 10 ~ "1–10",
          occ <= 100 ~ "11–100",
          occ <= 1000 ~ "101–1K",
          occ <= 10000 ~ "1K–10K",
          occ <= 100000 ~ "10K–100K",
          TRUE ~ ">100K"
        )
      )

      # Discrete colorscale mapped to bin integers 0–6
      binned_cs <- list(
        list(0, "#f6f5f1"), list(0.167, "#e8ede9"),
        list(0.333, "#c8dbc6"), list(0.5, pal$sage),
        list(0.667, pal$sand), list(0.833, pal$coral),
        list(1, "#3d4f6a"))

      p <- plot_ly(hm, x = ~month, y = ~year, z = ~occ_bin, type = "heatmap",
        colorscale = binned_cs, customdata = ~occ_label,
        zmin = 0, zmax = 6,
        hovertemplate = "Year: %{y}<br>Month: %{x}<br>%{customdata} occurrences<extra></extra>",
        colorbar = list(
          title = "Occurrences",
          tickvals = c(0, 1, 2, 3, 4, 5, 6),
          ticktext = c("0", "1–10", "11–100", "101–1K", "1K–10K", "10K–100K", ">100K")
        ))

    } else if (scale_mode == "linear") {
      p <- plot_ly(hm, x = ~month, y = ~year, z = ~occ, type = "heatmap",
        colorscale = heatmap_cs,
        hovertemplate = "Year: %{y}<br>Month: %{x}<br>Occurrences: %{z:,.0f}<extra></extra>",
        colorbar = list(title = "Occurrences"))

    } else {
      # Log scale (default)
      p <- plot_ly(hm, x = ~month, y = ~year, z = ~log_occ, type = "heatmap",
        colorscale = heatmap_cs, customdata = ~occ,
        hovertemplate = "Year: %{y}<br>Month: %{x}<br>Occurrences: %{customdata:,.0f}<extra></extra>",
        colorbar = list(title = "log10(occ)"))
    }

    p |> plotly_layout(
      xaxis = list(title = "", ticktext = month.abb, tickvals = 1:12),
      yaxis = list(title = ""))
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

  # Last year stats
  output$ov_ly_occ <- renderText({
    if (!is.null(overview_last_year)) comma(overview_last_year$occ_last_year) else "?"
  })
  output$ov_ly_cells <- renderText({
    if (!is.null(overview_last_year)) comma(overview_last_year$cells_active_last_year) else "?"
  })
  output$ov_ly_new_cells <- renderText({
    if (!is.null(overview_last_year)) comma(overview_last_year$cells_newly_covered) else "0"
  })
  output$ov_ly_resolved <- renderText({
    if (!is.null(overview_last_year) && !is.null(overview_last_year$cells_resolved))
      comma(overview_last_year$cells_resolved)
    else "0"
  })
  output$ov_ly_published <- renderText({
    if (!is.null(overview_last_year) && !is.null(overview_last_year$occ_published_last_year))
      comma(overview_last_year$occ_published_last_year)
    else "N/A"
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

  # Establishment means overview chart
  output$overview_establishment <- renderPlotly({
    req(tax_by_establishment)
    df <- tax_by_establishment |>
      mutate(
        label = case_when(
          is.na(establishmentMeans) | establishmentMeans == "" ~ "Unclassified",
          establishmentMeans == "native" ~ "Native",
          establishmentMeans == "introduced" ~ "Introduced",
          establishmentMeans == "invasive" ~ "Invasive",
          establishmentMeans == "naturalised" ~ "Naturalised",
          establishmentMeans == "uncertain" ~ "Uncertain",
          TRUE ~ establishmentMeans
        ),
        label = factor(label, levels = c("Native", "Introduced", "Invasive",
                                          "Naturalised", "Uncertain", "Unclassified"))
      ) |>
      arrange(label)

    estab_colors <- c(Native = pal$sage, Introduced = pal$sand, Invasive = pal$coral,
                       Naturalised = pal$slate, Uncertain = pal$plum, Unclassified = "#ccc")

    plot_ly(df, x = ~label, y = ~pct_coverage, type = "bar",
      marker = list(color = estab_colors[as.character(df$label)]),
      text = ~paste0(pct_coverage, "% (", comma(n_in_gbif), " / ", comma(n_ref_total), ")"),
      textposition = "auto", textfont = list(size = 11, color = "#fff"),
      hovertemplate = "<b>%{x}</b><br>%{text}<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = ""),
        yaxis = list(title = "GBIF Coverage (%)", range = c(0, 105)))
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

  # Spatial kingdom filter — filtered by Dyntaxa scope when active
  output$spatial_kingdom_filter_ui <- renderUI({
    kingdoms <- if (has_kingdom_recency) {
      sort(unique(kingdom_cell_recency$kingdom))
    } else character(0)

    # Filter to Dyntaxa kingdoms when in Dyntaxa mode
    if (dyntaxa_mode() && has_dyntaxa_scope) {
      dyntaxa_kingdoms <- species_scope_lookup |>
        filter(in_dyntaxa == TRUE, !is.na(kingdom), kingdom != "") |>
        pull(kingdom) |> unique()
      kingdoms <- intersect(kingdoms, dyntaxa_kingdoms)
    }

    ch <- c("All kingdoms" = "", setNames(sort(kingdoms), sort(kingdoms)))
    selectInput("spatial_kingdom_filter", "Kingdom", choices = ch, selected = "")
  })

  # Spatial class filter — cascading from kingdom selection, filtered by Dyntaxa scope
  output$spatial_class_filter_ui <- renderUI({
    ch <- c("All classes" = "")
    if (has_tax_cell_recency &&
        !is.null(input$spatial_kingdom_filter) &&
        input$spatial_kingdom_filter != "") {
      classes <- tax_cell_recency |>
        filter(kingdom == input$spatial_kingdom_filter, class != "Unplaced") |>
        pull(class) |> unique() |> sort()

      # Filter to Dyntaxa classes when in Dyntaxa mode
      if (dyntaxa_mode() && has_dyntaxa_scope) {
        dyntaxa_classes <- species_scope_lookup |>
          filter(in_dyntaxa == TRUE,
                 kingdom == input$spatial_kingdom_filter,
                 !is.na(class), class != "") |>
          pull(class) |> unique()
        classes <- intersect(classes, dyntaxa_classes)
      }

      ch <- c("All classes" = "", setNames(sort(classes), sort(classes)))
    }
    selectInput("spatial_class_filter", "Class", choices = ch, selected = "")
  })

  # Reactive map update when map_var, basis, or taxonomy filter changes
  observe({
    req(grid_10km, spatial_gaps, input$map_var)

    # Select data source based on Dyntaxa scope toggle
    use_dyntaxa <- dyntaxa_mode()
    active_spatial   <- if (use_dyntaxa && !is.null(dyntaxa_spatial_gaps)) dyntaxa_spatial_gaps else spatial_gaps
    active_recency   <- if (use_dyntaxa && !is.null(dyntaxa_cell_recency)) dyntaxa_cell_recency else cell_recency

    sf_base <- active_spatial |> filter(basisofrecord == basis_selected())

    # Taxonomy filter state
    kingdom_filter_active <- !is.null(input$spatial_kingdom_filter) &&
                             input$spatial_kingdom_filter != ""
    class_filter_active <- !is.null(input$spatial_class_filter) &&
                           input$spatial_class_filter != ""

    if (input$map_var == "stale") {
      # Use class-level recency if class filter is active
      if (class_filter_active && has_tax_cell_recency) {
        rec <- tax_cell_recency |>
          filter(kingdom == input$spatial_kingdom_filter,
                 class == input$spatial_class_filter) |>
          select(eeacellcode, staleness_months)
      } else if (kingdom_filter_active && has_kingdom_recency) {
        rec <- kingdom_cell_recency |>
          filter(kingdom == input$spatial_kingdom_filter) |>
          select(eeacellcode, staleness_months)
      } else if (!is.null(active_recency)) {
        rec <- active_recency |> filter(basisofrecord == basis_selected()) |>
          select(eeacellcode, staleness_months)
      } else {
        return()
      }
      map_sf <- grid_10km |> left_join(rec, by = "eeacellcode")
      # Categorical staleness: much more meaningful than raw months
      map_sf <- map_sf |>
        mutate(stale_cat = case_when(
          is.na(staleness_months) ~ "No data",
          staleness_months <= 12 ~ "< 1 year",
          staleness_months <= 36 ~ "1–3 years",
          staleness_months <= 60 ~ "3–5 years",
          staleness_months <= 120 ~ "5–10 years",
          TRUE ~ "> 10 years"
        ),
        stale_cat = factor(stale_cat, levels = c(
          "< 1 year", "1–3 years", "3–5 years", "5–10 years", "> 10 years", "No data")))
      stale_pal <- colorFactor(
        palette = c("#4a9ba5", "#6b8f71", pal$sand, pal$coral, "#3d4f6a", "#ddd"),
        domain = levels(map_sf$stale_cat), na.color = "#ddd")
      legend_title <- if (class_filter_active) {
        paste0("Data recency (", input$spatial_class_filter, ")")
      } else if (kingdom_filter_active) {
        paste0("Data recency (", input$spatial_kingdom_filter, ")")
      } else {
        "Data recency"
      }
      popup_fn <- ~paste0("Cell: ", eeacellcode, "<br>Staleness: ",
                          ifelse(is.na(staleness_months), "No data",
                                 paste0(round(staleness_months / 12, 1), " years")))

      leafletProxy("spatial_map", data = map_sf) |>
        clearShapes() |> clearControls() |>
        addPolygons(
          fillColor   = ~stale_pal(stale_cat),
          fillOpacity = 0.7, weight = 0.3, color = "#999",
          popup       = popup_fn) |>
        addLegend("bottomright", pal = stale_pal, values = ~stale_cat, title = legend_title)
      return()

    } else if (input$map_var == "richness") {
      map_sf <- grid_10km |> left_join(
        sf_base |> select(eeacellcode, n_species), by = "eeacellcode")
      map_sf <- map_sf |>
        mutate(sp_cat = case_when(
          is.na(n_species) | n_species == 0 ~ "No data",
          n_species <= 10 ~ "1–10",
          n_species <= 100 ~ "11–100",
          n_species <= 1000 ~ "101–1,000",
          TRUE ~ "> 1,000"
        ),
        sp_cat = factor(sp_cat, levels = c(
          "1–10", "11–100", "101–1,000", "> 1,000", "No data")))
      sp_pal <- colorFactor(
        palette = c("#4a9ba5", "#6b8f71", pal$sand, pal$coral, "#ddd"),
        domain = levels(map_sf$sp_cat), na.color = "#ddd")
      popup_fn <- ~paste0("Cell: ", eeacellcode, "<br>Species: ", comma(n_species))

      leafletProxy("spatial_map", data = map_sf) |>
        clearShapes() |> clearControls() |>
        addPolygons(
          fillColor   = ~sp_pal(sp_cat),
          fillOpacity = 0.7, weight = 0.3, color = "#999",
          popup       = popup_fn) |>
        addLegend("bottomright", pal = sp_pal, values = ~sp_cat, title = "Species count")
      return()

    } else if (input$map_var == "last_year_obs" && !is.null(cell_last_year)) {
      ly <- cell_last_year |> select(eeacellcode, prior, last_year, newly_covered)
      map_sf <- grid_10km |> left_join(ly, by = "eeacellcode") |>
        mutate(
          last_year = replace_na(last_year, 0),
          prior = replace_na(prior, 0),
          newly_covered = replace_na(newly_covered, FALSE)
        )
      map_sf$fill_col <- case_when(
        map_sf$newly_covered ~ pal$coral,
        map_sf$last_year > 0 ~ pal$sage,
        TRUE ~ "#e0dfda"
      )
      popup_fn <- ~paste0("Cell: ", eeacellcode,
                          "<br>Observed ", last_year_label, ": ", comma(last_year),
                          "<br>Prior: ", comma(prior),
                          if_else(newly_covered, "<br><strong>Newly covered!</strong>", ""))

      leafletProxy("spatial_map", data = map_sf) |>
        clearShapes() |> clearControls() |>
        addPolygons(
          fillColor   = ~fill_col,
          fillOpacity = 0.7, weight = 0.3, color = "#999",
          popup       = popup_fn) |>
        addLegend("bottomright",
          colors = c(pal$coral, pal$sage, "#e0dfda"),
          labels = c("Newly covered", paste0("Observed in ", last_year_label), "No observations"),
          title = paste0("Observed ", last_year_label))
      return()

    } else if (input$map_var == "last_year_pub") {
      # Published to GBIF in last 12 months — computed from parquet at cell level
      # For now, use cell_last_year as fallback (same data until cell-level published is available)
      pub_cell <- safe_get("cell_published_last_year")
      if (is.null(pub_cell) && !is.null(cell_last_year)) {
        # Fallback: show message that published cell data isn't available yet
        pub_cell <- cell_last_year |>
          select(eeacellcode) |>
          mutate(pub_last_year = 0, pub_prior = 0)
      }
      if (!is.null(pub_cell)) {
        map_sf <- grid_10km |> left_join(
          pub_cell |> select(eeacellcode, pub_last_year), by = "eeacellcode") |>
          mutate(pub_last_year = replace_na(pub_last_year, 0))

        map_sf <- map_sf |>
          mutate(pub_cat = case_when(
            pub_last_year == 0 ~ "None",
            pub_last_year <= 100 ~ "1–100",
            pub_last_year <= 1000 ~ "101–1K",
            pub_last_year <= 10000 ~ "1K–10K",
            TRUE ~ "> 10K"
          ),
          pub_cat = factor(pub_cat, levels = c("1–100", "101–1K", "1K–10K", "> 10K", "None")))

        pub_pal <- colorFactor(
          palette = c("#4a9ba5", pal$sage, pal$sand, pal$plum, "#e0dfda"),
          domain = levels(map_sf$pub_cat), na.color = "#e0dfda")

        leafletProxy("spatial_map", data = map_sf) |>
          clearShapes() |> clearControls() |>
          addPolygons(
            fillColor = ~pub_pal(pub_cat),
            fillOpacity = 0.7, weight = 0.3, color = "#999",
            popup = ~paste0("Cell: ", eeacellcode,
                            "<br>Published ", last_year_label, ": ", comma(pub_last_year))) |>
          addLegend("bottomright", pal = pub_pal, values = ~pub_cat,
            title = paste0("Published ", last_year_label))
      }
      return()

    } else {
      # Occurrences: fixed categorical breaks
      map_sf <- grid_10km |> left_join(
        sf_base |> select(eeacellcode, occurrences), by = "eeacellcode")
      map_sf <- map_sf |>
        mutate(occ_cat = case_when(
          is.na(occurrences) | occurrences == 0 ~ "No data",
          occurrences <= 100 ~ "1–100",
          occurrences <= 1000 ~ "101–1,000",
          occurrences <= 10000 ~ "1,001–10,000",
          occurrences <= 100000 ~ "10,001–100,000",
          TRUE ~ "> 100,000"
        ),
        occ_cat = factor(occ_cat, levels = c(
          "1–100", "101–1,000", "1,001–10,000", "10,001–100,000", "> 100,000", "No data")))
      occ_pal <- colorFactor(
        palette = c("#4a9ba5", "#6b8f71", pal$sand, pal$coral, "#3d4f6a", "#ddd"),
        domain = levels(map_sf$occ_cat), na.color = "#ddd")
      popup_fn <- ~paste0("Cell: ", eeacellcode, "<br>Occurrences: ", comma(occurrences))

      leafletProxy("spatial_map", data = map_sf) |>
        clearShapes() |> clearControls() |>
        addPolygons(
          fillColor   = ~occ_pal(occ_cat),
          fillOpacity = 0.7, weight = 0.3, color = "#999",
          popup       = popup_fn) |>
        addLegend("bottomright", pal = occ_pal, values = ~occ_cat, title = "Occurrences")
    }
  })

  # Admin boundary overlay (independent observe — doesn't redraw the main map)
  observe({
    req(grid_10km)
    proxy <- leafletProxy("spatial_map")
    proxy |> clearGroup("admin1") |> clearGroup("admin2")

    if (!is.null(admin_level1) && !is.null(input$show_admin1) && input$show_admin1) {
      proxy |> addPolygons(
        data = admin_level1, group = "admin1",
        fillColor = "transparent", fillOpacity = 0,
        weight = 2, color = "#333", opacity = 0.6,
        label = ~admin_name,
        labelOptions = labelOptions(
          style = list("font-size" = "12px", "font-weight" = "bold"),
          direction = "auto"))
    }

    if (!is.null(admin_level2) && !is.null(input$show_admin2) && input$show_admin2) {
      proxy |> addPolygons(
        data = admin_level2, group = "admin2",
        fillColor = "transparent", fillOpacity = 0,
        weight = 1, color = "#666", opacity = 0.4,
        dashArray = "3,3",
        label = ~admin_name,
        labelOptions = labelOptions(
          style = list("font-size" = "11px"),
          direction = "auto"))
    }
  })

  # Also overlay on priority maps
  observe({
    if (!is.null(admin_level1) && !is.null(input$show_admin1) && input$show_admin1) {
      for (map_id in c("zero_map", "stale_map")) {
        leafletProxy(map_id) |> clearGroup("admin1") |>
          addPolygons(data = admin_level1, group = "admin1",
            fillColor = "transparent", fillOpacity = 0,
            weight = 2, color = "#333", opacity = 0.6,
            label = ~admin_name)
      }
    } else {
      leafletProxy("zero_map") |> clearGroup("admin1")
      leafletProxy("stale_map") |> clearGroup("admin1")
    }
  })

  output$spatial_stats <- renderTable({
    req(spatial_gaps)
    active_sg <- if (dyntaxa_mode() && !is.null(dyntaxa_spatial_gaps)) dyntaxa_spatial_gaps else spatial_gaps
    sf <- active_sg |> filter(basisofrecord == basis_selected())
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
    
    # Create categorical brackets
    df <- df |> mutate(
      occ_bracket = case_when(
        occurrences <= 10 ~ "1–10",
        occurrences <= 100 ~ "11–100",
        occurrences <= 1000 ~ "101–1K",
        occurrences <= 10000 ~ "1K–10K",
        occurrences <= 100000 ~ "10K–100K",
        TRUE ~ ">100K"
      ),
      occ_bracket = factor(occ_bracket, levels = c("1–10", "11–100", "101–1K", "1K–10K", "10K–100K", ">100K"))
    )
    
    bracket_counts <- df |> count(occ_bracket, .drop = FALSE)
    
    plot_ly(bracket_counts, x = ~occ_bracket, y = ~n, type = "bar",
      marker = list(color = pal$sage),
      hovertemplate = "%{x}: %{y} cells<extra></extra>") |>
      plotly_layout(
        title = list(text = "Occurrence distribution per 10km cell", font = list(size = 13)),
        xaxis = list(title = "Occurrences per cell"),
        yaxis = list(title = "Number of 10km cells"))
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

  # Stat boxes — use basis_recent data (correct per-basis totals from cube)
  output$basis_stat_boxes <- renderUI({
    br <- if (dyntaxa_mode() && !is.null(dyntaxa_basis_recent)) dyntaxa_basis_recent
          else if (!is.null(all_basis_recent)) all_basis_recent
          else NULL

    if (is.null(br)) {
      # Fallback to spatial_gaps
      req(spatial_gaps)
      sg <- spatial_gaps |> filter(basisofrecord != "all")
      basis_summary <- sg |>
        group_by(basisofrecord) |>
        summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
        arrange(desc(total_occ)) |>
        slice_head(n = 4)
    } else {
      basis_summary <- br |>
        filter(basisofrecord != "all") |>
        arrange(desc(occ_total)) |>
        slice_head(n = 4) |>
        rename(total_occ = occ_total)
    }

    color_classes <- c("sage", "slate", "sand", "coral")

    div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
      tagList(lapply(seq_len(nrow(basis_summary)), function(i) {
        b <- basis_summary[i, ]
        div(class = "stat-box",
          div(class = paste("stat-value", color_classes[i]), comma(b$total_occ)),
          div(class = "stat-label", str_replace_all(b$basisofrecord, "_", " "))
        )
      }))
    )
  })

  # Pie / stacked bar — switches based on last-12-months toggle
  output$basis_pie <- renderPlotly({
    br <- if (dyntaxa_mode() && !is.null(dyntaxa_basis_recent)) dyntaxa_basis_recent
          else if (!is.null(all_basis_recent)) all_basis_recent
          else NULL

    ly_mode <- if (!is.null(input$basis_last_year_mode)) input$basis_last_year_mode else "off"

    if (!is.null(br) && ly_mode != "off") {
      # Stacked horizontal bar: prior + last 12 months
      df <- br |>
        filter(basisofrecord != "all") |>
        arrange(desc(occ_total)) |>
        mutate(label_text = str_replace_all(basisofrecord, "_", " "))

      if (ly_mode == "observed") {
        recent_vals <- df$occ_last_year
        prior_vals  <- df$occ_prior
        recent_name <- paste0("Last 12 months (", last_year_label, ")")
        prior_name  <- "Prior"
      } else {
        recent_vals <- df$pub_last_year
        prior_vals  <- df$pub_prior
        recent_name <- paste0("Published last 12 months (", last_year_label, ")")
        prior_name  <- "Published prior"
      }

      plot_ly(df, y = ~reorder(label_text, occ_total), x = prior_vals,
        type = "bar", orientation = "h", name = prior_name,
        marker = list(color = "#d4c0a0"),
        hovertemplate = paste0("%{y}<br>", prior_name, ": %{x:,.0f}<extra></extra>")) |>
        add_trace(x = recent_vals, name = recent_name,
          marker = list(color = pal$sage),
          hovertemplate = paste0("%{y}<br>", recent_name, ": %{x:,.0f}<extra></extra>")) |>
        plotly_layout(
          barmode = "stack",
          xaxis = list(title = "Number of occurrences"),
          yaxis = list(title = ""),
          margin = list(l = 160),
          legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center",
                        font = list(size = 10)))
    } else {
      # Default pie chart
      if (!is.null(br)) {
        df <- br |>
          filter(basisofrecord != "all") |>
          arrange(desc(occ_total)) |>
          mutate(
            pct = round(100 * occ_total / sum(occ_total), 1),
            label_text = str_replace_all(basisofrecord, "_", " ")
          )
        vals <- df$occ_total
      } else {
        req(spatial_gaps)
        df <- spatial_gaps |>
          filter(basisofrecord != "all") |>
          group_by(basisofrecord) |>
          summarise(occ_total = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
          arrange(desc(occ_total)) |>
          mutate(
            pct = round(100 * occ_total / sum(occ_total), 1),
            label_text = str_replace_all(basisofrecord, "_", " ")
          )
        vals <- df$occ_total
      }

      cols <- sapply(df$basisofrecord, get_basis_color)

      plot_ly(df, labels = ~label_text, values = vals, type = "pie",
        marker = list(colors = cols, line = list(color = "#fff", width = 1.5)),
        textinfo = "percent",
        textposition = "inside",
        insidetextorientation = "horizontal",
        textfont = list(size = 11, color = "#fff"),
        hovertemplate = "%{label}<br>%{value:,.0f} occurrences<br>%{percent}<extra></extra>") |>
        plotly_layout(
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center",
                        font = list(size = 9)),
          margin = list(l = 10, r = 10, t = 10, b = 80))
    }
  })

  # Timeline — single basis at a time, uses time_summary (has per-basis rows)
  output$basis_timeline <- renderPlotly({
    req(time_summary, input$basis_timeline_select)
    sel <- input$basis_timeline_select

    ts <- if (dyntaxa_mode() && !is.null(dyntaxa_time_summary)) dyntaxa_time_summary else time_summary

    df <- ts |>
      filter(basisofrecord == sel, !is.na(year), year >= 1970) |>
      group_by(year) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")

    if (nrow(df) == 0) {
      return(plotly_empty() |> plotly_layout(
        annotations = list(list(text = paste0("No data for ", str_replace_all(sel, "_", " ")),
          showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))))
    }

    base_col <- get_basis_color(sel)

    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = paste0(base_col, "33"),
      line = list(color = base_col, width = 2),
      hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of occurrences"))
  })

  # Spatial coverage bar — uses scope-aware spatial_gaps (zero-filled grid)
  output$basis_spatial_bar <- renderPlotly({
    req(spatial_gaps)
    active_sg <- if (dyntaxa_mode() && !is.null(dyntaxa_spatial_gaps)) dyntaxa_spatial_gaps else spatial_gaps
    df <- active_sg |>
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
      text = ~paste0(pct, "%"),
      textposition = "outside", textfont = list(size = 9, color = "#333"),
      cliponaxis = FALSE,
      hovertemplate = "%{y}<br>%{x:.1f}% (%{text} cells)<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "% of 10 km grid cells with data", range = c(0, 120)),
        yaxis = list(title = ""),
        margin = list(l = 160, r = 60))
  })

  # Species coverage bar — uses scope-aware spatial_gaps (zero-filled grid)
  output$basis_species_bar <- renderPlotly({
    req(spatial_gaps)
    active_sg <- if (dyntaxa_mode() && !is.null(dyntaxa_spatial_gaps)) dyntaxa_spatial_gaps else spatial_gaps
    df <- active_sg |>
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
      textposition = "outside", textfont = list(size = 9, color = "#333"),
      cliponaxis = FALSE,
      hovertemplate = "%{y}<br>%{x:,.0f} unique species<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Number of unique species recorded"),
        yaxis = list(title = ""),
        margin = list(l = 160, r = 60))
  })

  # Spatial map for selected basis — uses scope-aware spatial_gaps
  output$basis_map <- renderLeaflet({
    req(grid_10km, spatial_gaps, input$basis_map_select)
    sel <- input$basis_map_select

    active_sg <- if (dyntaxa_mode() && !is.null(dyntaxa_spatial_gaps)) dyntaxa_spatial_gaps else spatial_gaps
    sg <- active_sg |> filter(basisofrecord == sel)
    map_sf <- grid_10km |> left_join(sg |> select(eeacellcode, occurrences, n_species), by = "eeacellcode")

    # Binned categories
    map_sf <- map_sf |>
      mutate(
        occ_cat = case_when(
          is.na(occurrences) | occurrences == 0 ~ "No data",
          occurrences <= 10 ~ "1\u201310",
          occurrences <= 100 ~ "11\u2013100",
          occurrences <= 1000 ~ "101\u20131K",
          occurrences <= 10000 ~ "1K\u201310K",
          TRUE ~ "> 10K"
        ),
        occ_cat = factor(occ_cat, levels = c(
          "1\u201310", "11\u2013100", "101\u20131K", "1K\u201310K", "> 10K", "No data"
        ))
      )

    bin_pal <- colorFactor(
      palette = c("#c8dbc6", "#6b8f71", pal$sand, pal$coral, "#3d4f6a", "#ddd"),
      domain = levels(map_sf$occ_cat), na.color = "#ddd")

    leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~bin_pal(occ_cat), fillOpacity = 0.65,
        weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Occurrences:</strong> ", comma(occurrences),
                        "<br><strong>Species:</strong> ", comma(n_species))) |>
      addLegend("bottomright", pal = bin_pal, values = ~occ_cat,
        title = str_replace_all(sel, "_", " "))
  })

  # ===================================================================
  # TAXONOMIC — Cascading Filters (#6)
  # ===================================================================

  # Reactive: filtered phylum choices based on selected kingdom
  output$tax_phylum_ui <- renderUI({
    choices <- c("All" = "")
    if (!is.null(tax_by_order) && "phylum" %in% names(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "") {
        df <- df |> filter(kingdom == input$tax_kingdom)
      }
      ph <- sort(unique(df$phylum[!is.na(df$phylum) & df$phylum != ""]))
      choices <- c("All" = "", setNames(ph, ph))
    }
    selectInput("tax_phylum", NULL, choices = choices, selected = "")
  })

  # Reactive: filtered class choices based on selected kingdom + phylum
  output$tax_class_ui <- renderUI({
    choices <- c("All" = "")
    if (!is.null(tax_by_order) && "class" %in% names(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "") {
        df <- df |> filter(kingdom == input$tax_kingdom)
      }
      if (!is.null(input$tax_phylum) && input$tax_phylum != "") {
        df <- df |> filter(phylum == input$tax_phylum)
      }
      cl <- sort(unique(df$class[!is.na(df$class) & df$class != ""]))
      choices <- c("All" = "", setNames(cl, cl))
    }
    selectInput("tax_class", NULL, choices = choices, selected = "")
  })

  # Reactive: filtered order choices (for additional narrowing)
  output$tax_order_filter_ui <- renderUI({
    choices <- c("All" = "")
    if (!is.null(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "") {
        df <- df |> filter(kingdom == input$tax_kingdom)
      }
      if (!is.null(input$tax_phylum) && input$tax_phylum != "") {
        df <- df |> filter(phylum == input$tax_phylum)
      }
      if (!is.null(input$tax_class) && input$tax_class != "") {
        df <- df |> filter(class == input$tax_class)
      }
      ord <- sort(unique(df$order[!is.na(df$order) & df$order != ""]))
      if (length(ord) <= 200) {
        choices <- c("All" = "", setNames(ord, ord))
      }
    }
    selectInput("tax_order_filter", NULL, choices = choices, selected = "")
  })

  # Reactive: filtered family choices
  output$tax_family_filter_ui <- renderUI({
    choices <- c("All" = "")
    if (!is.null(tax_by_family)) {
      df <- tax_by_family
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "") df <- df |> filter(kingdom == input$tax_kingdom)
      if (!is.null(input$tax_phylum) && input$tax_phylum != "") df <- df |> filter(phylum == input$tax_phylum)
      if (!is.null(input$tax_class) && input$tax_class != "") df <- df |> filter(class == input$tax_class)
      if (!is.null(input$tax_order_filter) && input$tax_order_filter != "") df <- df |> filter(order == input$tax_order_filter)
      fam <- sort(unique(df$family[!is.na(df$family) & df$family != ""]))
      if (length(fam) <= 200) {
        choices <- c("All" = "", setNames(fam, fam))
      }
    }
    selectInput("tax_family_filter", NULL, choices = choices, selected = "")
  })

  # Helper: apply cascading filters to a data frame (now includes family)
  # ---- Scope filter: filter match_summary by establishment means ----
  scoped_match_summary <- reactive({
    req(match_summary_full)
    ms <- match_summary_full |> as_tibble()
    scope <- input$tax_scope
    if (is.null(scope) || scope == "all") return(ms)

    if (scope == "present") {
      ms <- ms |> filter(occurrenceStatus == "present" | is.na(occurrenceStatus) | occurrenceStatus == "")
    } else if (scope == "native_present") {
      ms <- ms |> filter(establishmentMeans == "native", occurrenceStatus == "present")
    } else if (scope == "introduced_present") {
      ms <- ms |> filter(establishmentMeans %in% c("introduced", "naturalised"),
                          occurrenceStatus == "present")
    } else if (scope == "invasive") {
      ms <- ms |> filter(establishmentMeans == "invasive")
    }
    ms
  })

  # ---- Recompute Troudet from scoped match_summary ----
  scoped_troudet_class <- reactive({
    ms <- scoped_match_summary()
    req(nrow(ms) > 0)
    ms |>
      filter(!is.na(class), class != "") |>
      group_by(kingdom, phylum, class) |>
      summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE),
                total_occ = sum(gbif_total_occ, na.rm = TRUE), .groups = "drop") |>
      mutate(total_known = sum(n_known_species), total_occ_all = sum(total_occ),
             ideal_occ = (n_known_species / total_known) * total_occ_all,
             bias = total_occ - ideal_occ,
             occ_prior = total_occ, occ_last_year = 0,
             pub_last_year = 0, pub_prior = total_occ)
  })

  scoped_troudet_order <- reactive({
    ms <- scoped_match_summary()
    req(nrow(ms) > 0)
    ms |>
      filter(!is.na(order), order != "") |>
      group_by(kingdom, phylum, class, order) |>
      summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE),
                total_occ = sum(gbif_total_occ, na.rm = TRUE), .groups = "drop") |>
      mutate(total_known = sum(n_known_species), total_occ_all = sum(total_occ),
             ideal_occ = (n_known_species / total_known) * total_occ_all,
             bias = total_occ - ideal_occ,
             occ_prior = total_occ, occ_last_year = 0,
             pub_last_year = 0, pub_prior = total_occ)
  })

  scoped_troudet_family <- reactive({
    ms <- scoped_match_summary()
    req(nrow(ms) > 0)
    ms |>
      filter(!is.na(family), family != "") |>
      group_by(kingdom, phylum, class, order, family) |>
      summarise(n_known_species = n(), n_in_gbif = sum(matched_any, na.rm = TRUE),
                total_occ = sum(gbif_total_occ, na.rm = TRUE), .groups = "drop") |>
      mutate(total_known = sum(n_known_species), total_occ_all = sum(total_occ),
             ideal_occ = (n_known_species / total_known) * total_occ_all,
             bias = total_occ - ideal_occ,
             occ_prior = total_occ, occ_last_year = 0,
             pub_last_year = 0, pub_prior = total_occ)
  })

  # ---- Recompute tax_by_order / tax_by_family from scoped data ----
  scoped_tax_by_order <- reactive({
    ms <- scoped_match_summary()
    req(nrow(ms) > 0)
    ms |>
      filter(!is.na(order), order != "") |>
      group_by(kingdom, phylum, class, order) |>
      summarise(n_taxa = n(), n_in_gbif = sum(matched_any, na.rm = TRUE),
                n_missing = n_taxa - n_in_gbif,
                pct_coverage = round(100 * n_in_gbif / n_taxa, 1), .groups = "drop") |>
      arrange(desc(n_taxa))
  })

  scoped_tax_by_family <- reactive({
    ms <- scoped_match_summary()
    req(nrow(ms) > 0)
    ms |>
      filter(!is.na(family), family != "") |>
      group_by(kingdom, phylum, class, order, family) |>
      summarise(n_taxa = n(), n_in_gbif = sum(matched_any, na.rm = TRUE),
                n_missing = n_taxa - n_in_gbif,
                pct_coverage = round(100 * n_in_gbif / n_taxa, 1), .groups = "drop") |>
      arrange(desc(n_taxa))
  })

  # Helper: choose pre-computed or scoped data
  use_scope <- reactive({
    # "present" scope can use pre-computed data (it's close enough to "all"
    # since absent species rarely have GBIF records). Only native/introduced/invasive
    # need on-the-fly recomputation.
    !is.null(input$tax_scope) && input$tax_scope %in% c("native_present", "introduced_present", "invasive")
  })

  apply_tax_filters <- function(df) {
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "" && "kingdom" %in% names(df)) {
      df <- df |> filter(kingdom == input$tax_kingdom)
    }
    if (!is.null(input$tax_phylum) && input$tax_phylum != "" && "phylum" %in% names(df)) {
      df <- df |> filter(phylum == input$tax_phylum)
    }
    if (!is.null(input$tax_class) && input$tax_class != "" && "class" %in% names(df)) {
      df <- df |> filter(class == input$tax_class)
    }
    if (!is.null(input$tax_order_filter) && input$tax_order_filter != "" && "order" %in% names(df)) {
      df <- df |> filter(order == input$tax_order_filter)
    }
    if (!is.null(input$tax_family_filter) && input$tax_family_filter != "" && "family" %in% names(df)) {
      df <- df |> filter(family == input$tax_family_filter)
    }
    df
  }

  # ===================================================================
  # TAXONOMIC — Filter Breadcrumb
  # ===================================================================

  output$tax_filter_breadcrumb <- renderUI({
    parts <- c()
    if (!is.null(input$tax_kingdom) && input$tax_kingdom != "")
      parts <- c(parts, input$tax_kingdom)
    if (!is.null(input$tax_phylum) && input$tax_phylum != "")
      parts <- c(parts, input$tax_phylum)
    if (!is.null(input$tax_class) && input$tax_class != "")
      parts <- c(parts, input$tax_class)
    if (!is.null(input$tax_order_filter) && input$tax_order_filter != "")
      parts <- c(parts, input$tax_order_filter)
    if (!is.null(input$tax_family_filter) && input$tax_family_filter != "")
      parts <- c(parts, input$tax_family_filter)

    if (length(parts) == 0) return(NULL)

    breadcrumb <- paste(parts, collapse = " \u2192 ")
    div(
      style = paste0(
        "margin-bottom: 1rem; padding: 0.5rem 1rem; ",
        "background: var(--bg-card, #f8f6f3); border-radius: 6px; ",
        "border-left: 4px solid var(--sage, #7a9a7e); ",
        "font-size: 0.9rem; color: var(--text-secondary, #666); ",
        "display: flex; align-items: center; gap: 0.5rem;"
      ),
      icon("filter", style = "color: var(--sage, #7a9a7e);"),
      tags$span("Showing:"),
      tags$strong(breadcrumb, style = "color: var(--text-primary, #333);"),
      actionLink("tax_clear_filters", tagList(icon("times-circle"), "Clear"),
        style = "margin-left: auto; font-size: 0.85rem; color: var(--coral, #c47a6c); text-decoration: none;")
    )
  })

  # Clear all taxonomic filters
  observeEvent(input$tax_clear_filters, {
    updateSelectInput(session, "tax_kingdom", selected = "")
  })

  # ===================================================================
  # TAXONOMIC — Troudet Bias Figure (#7)
  # ===================================================================

  output$troudet_bias_chart <- renderPlotly({
    # Clean hierarchical drill:
    # No filter       → kingdoms
    # Kingdom selected → phyla in that kingdom
    # Phylum selected  → classes in that phylum
    # Class selected   → orders in that class
    # Order selected   → families in that order

    has_kingdom <- !is.null(input$tax_kingdom) && input$tax_kingdom != ""
    has_phylum <- !is.null(input$tax_phylum) && input$tax_phylum != ""
    has_class <- !is.null(input$tax_class) && input$tax_class != ""
    has_order <- !is.null(input$tax_order_filter) && input$tax_order_filter != ""
    has_family <- !is.null(input$tax_family_filter) && input$tax_family_filter != ""

    # Choose data source: scoped (on-the-fly) or pre-computed
    scoped <- use_scope()

    t_family <- if (scoped) scoped_troudet_family() else safe_get("troudet_bias_family")
    t_order  <- if (scoped) scoped_troudet_order()  else troudet_bias_order
    t_class  <- if (scoped) scoped_troudet_class()  else troudet_bias

    if (has_order && !is.null(t_family)) {
      df <- apply_tax_filters(t_family) |> mutate(label = family)
    } else if (has_class && !is.null(t_order)) {
      df <- apply_tax_filters(t_order) |> mutate(label = order)
    } else if (has_phylum && !is.null(t_class)) {
      df <- apply_tax_filters(t_class) |> mutate(label = class)
    } else if (has_kingdom && !is.null(t_class)) {
      # Kingdom selected → aggregate to phyla
      df <- apply_tax_filters(t_class) |>
        group_by(kingdom, phylum) |>
        summarise(
          n_known_species = sum(n_known_species, na.rm = TRUE),
          n_in_gbif = sum(n_in_gbif, na.rm = TRUE),
          occ_prior = sum(occ_prior, na.rm = TRUE),
          occ_last_year = sum(occ_last_year, na.rm = TRUE),
          total_occ = sum(total_occ, na.rm = TRUE),
          pub_last_year = sum(pub_last_year, na.rm = TRUE),
          pub_prior = sum(pub_prior, na.rm = TRUE),
          .groups = "drop") |>
        mutate(
          total_known = sum(n_known_species),
          ideal_occ = (n_known_species / total_known) * sum(total_occ),
          bias = total_occ - ideal_occ,
          label = phylum
        )
    } else if (!is.null(t_class)) {
      # No filter → aggregate to kingdoms
      df <- t_class |>
        group_by(kingdom) |>
        summarise(
          n_known_species = sum(n_known_species, na.rm = TRUE),
          n_in_gbif = sum(n_in_gbif, na.rm = TRUE),
          occ_prior = sum(occ_prior, na.rm = TRUE),
          occ_last_year = sum(occ_last_year, na.rm = TRUE),
          total_occ = sum(total_occ, na.rm = TRUE),
          pub_last_year = sum(pub_last_year, na.rm = TRUE),
          pub_prior = sum(pub_prior, na.rm = TRUE),
          .groups = "drop") |>
        mutate(
          total_known = sum(n_known_species),
          ideal_occ = (n_known_species / total_known) * sum(total_occ),
          bias = total_occ - ideal_occ,
          label = kingdom
        )
    } else {
      return(plotly_empty() |> plotly_layout())
    }

    if (nrow(df) == 0) return(plotly_empty() |> plotly_layout())

    # Show all groups (capped at 40 for readability)
    n <- min(40, nrow(df))

    df <- df |>
      arrange(desc(abs(bias))) |>
      slice_head(n = n) |>
      mutate(
        bar_col = ifelse(bias >= 0, pal$sage, pal$coral),
        direction = ifelse(bias >= 0, "Over-represented", "Under-represented")
      )

    # Dynamic height based on number of bars
    ly_mode <- if (!is.null(input$tax_last_year_mode)) input$tax_last_year_mode else "off"
    
    # Determine which last-year column to use
    if (ly_mode == "observed" && "occ_last_year" %in% names(df)) {
      ly_col <- "occ_last_year"
      prior_col <- "occ_prior"
      ly_label <- paste0("Observed ", last_year_label)
    } else if (ly_mode == "published" && "pub_last_year" %in% names(df)) {
      ly_col <- "pub_last_year"
      prior_col <- "pub_prior"
      ly_label <- paste0("Published ", last_year_label)
    } else {
      ly_col <- NULL
    }
    
    show_ly <- !is.null(ly_col)

    if (show_ly) {
      df <- df |>
        mutate(
          bias_prior = .data[[prior_col]] - ideal_occ,
          bias_last_year = .data[[ly_col]],
          bar_col_ly = ifelse(bias >= 0, pal$sage2, pal$sand)
        )

      p <- plot_ly(df, y = ~reorder(label, bias), orientation = "h") |>
        add_trace(x = ~bias_prior, type = "bar", name = "Prior years",
          marker = list(color = ~bar_col),
          hovertemplate = "%{y}: %{x:,.0f}<extra>Prior</extra>") |>
        add_trace(x = ~bias_last_year, type = "bar",
          name = ly_label,
          marker = list(color = ~bar_col_ly),
          hovertemplate = paste0("%{y}: +%{x:,.0f}<extra>", ly_label, "</extra>"))
    } else {
      p <- plot_ly(df, y = ~reorder(label, bias), x = ~bias, type = "bar",
        marker = list(color = ~bar_col), orientation = "h",
        hovertemplate = "%{y}: %{x:,.0f} occurrences<extra>%{customdata}</extra>",
        customdata = ~direction)
    }

    p |> plotly_layout(
      barmode = if (show_ly) "stack" else "relative",
      xaxis = list(
        title = "Deviation from proportional sampling (occurrences)",
        zeroline = TRUE, zerolinecolor = "#666", zerolinewidth = 1),
      yaxis = list(title = ""),
      legend = list(orientation = "h", y = -0.15),
      shapes = list(
        list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = n - 0.5,
             line = list(color = "#666", width = 1, dash = "dot"))
      ))
  })

  # ===================================================================
  # TAXONOMIC — Coverage Charts (with filters + last year)
  # ===================================================================

  output$tax_order <- renderPlotly({
    tbo <- if (use_scope()) scoped_tax_by_order() else tax_by_order
    req(tbo)

    df <- apply_tax_filters(tbo) |>
      filter(!is.na(order)) |>
      arrange(desc(n_taxa)) |>
      slice_head(n = 40) |>
      mutate(
        miss = n_taxa - n_in_gbif,
        label = paste0(round(pct_coverage, 1), "%"))

    p <- plot_ly(df, y = ~reorder(order, n_taxa), x = ~n_in_gbif, type = "bar",
      name = "In GBIF", marker = list(color = pal$sage),
      text = ~label, textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      orientation = "h") |>
      add_trace(x = ~miss, name = "Missing from GBIF",
        marker = list(color = pal$sand2),
        text = "", textposition = "none")

    ly_mode2 <- if (!is.null(input$tax_last_year_mode)) input$tax_last_year_mode else "off"
    
    if (ly_mode2 == "observed" && "occ_last_year" %in% names(df)) {
      ly_val <- df$occ_last_year
      ly_lab <- paste0("Observed ", last_year_label)
    } else if (ly_mode2 == "published" && "pub_last_year" %in% names(df)) {
      ly_val <- df$pub_last_year
      ly_lab <- paste0("Published ", last_year_label)
    } else {
      ly_val <- NULL
    }

    if (!is.null(ly_val)) {
      p <- p |> add_annotations(
        x = ~n_in_gbif / 2,
        y = ~order,
        text = ~ifelse(ly_val > 0,
          paste0(ly_lab, ": ", comma(ly_val), " occ"), ""),
        showarrow = FALSE,
        font = list(size = 9, color = pal$plum),
        xanchor = "center", yanchor = "bottom")
    }

    p |> plotly_layout(
        barmode = "stack",
        xaxis = list(title = "Species count"),
        yaxis = list(title = "", categoryorder = "total ascending"),
        legend = list(orientation = "h", y = -0.15))
  })

  output$tax_family <- renderPlotly({
    tbf <- if (use_scope()) scoped_tax_by_family() else tax_by_family
    req(tbf)

    df <- apply_tax_filters(tbf) |>
      filter(!is.na(family)) |>
      arrange(desc(n_taxa)) |>
      slice_head(n = 40) |>
      mutate(label = paste0(round(pct_coverage, 1), "%"))

    plot_ly(df, y = ~reorder(family, pct_coverage), x = ~pct_coverage, type = "bar",
      marker = list(color = pal$slate),
      text = ~label, textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      orientation = "h") |>
      plotly_layout(
        xaxis = list(title = "Coverage (%)", range = c(0, 105)),
        yaxis = list(title = "", categoryorder = "total ascending"))
  })

  # Recent vs Historical — info cards showing multiplier
  output$tax_change_cards <- renderUI({
    if (is.null(order_5yr)) return(div("No temporal order data available."))

    # Get filtered orders
    filtered_orders <- NULL
    if (!is.null(order_tax_map)) {
      filt <- order_tax_map
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "") filt <- filt |> filter(kingdom == input$tax_kingdom)
      if (!is.null(input$tax_phylum) && input$tax_phylum != "") filt <- filt |> filter(phylum == input$tax_phylum)
      if (!is.null(input$tax_class) && input$tax_class != "") filt <- filt |> filter(class == input$tax_class)
      if (!is.null(input$tax_order_filter) && input$tax_order_filter != "") filt <- filt |> filter(order == input$tax_order_filter)
      filtered_orders <- filt$order
    }

    df <- order_5yr
    if (!is.null(filtered_orders)) df <- df |> filter(order %in% filtered_orders)

    df <- df |>
      mutate(era = ifelse(period_start >= 2000, "Recent", "Historical")) |>
      group_by(order, era) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = era, values_from = occ, values_fill = 0) |>
      filter(Historical > 0) |>
      mutate(
        multiplier = round(Recent / Historical, 1),
        direction = ifelse(Recent >= Historical, "up", "down")
      ) |>
      arrange(desc(abs(multiplier - 1))) |>
      slice_head(n = 12)

    if (nrow(df) == 0) return(div("No orders with both historical and recent data for current filters."))

    # Build info cards in a grid
    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      mult_text <- if (row$multiplier >= 2) {
        paste0(row$multiplier, "\u00d7 more")
      } else if (row$multiplier >= 1) {
        paste0("+", round((row$multiplier - 1) * 100), "%")
      } else {
        paste0(round((1 - row$multiplier) * 100), "% less")
      }
      col <- if (row$direction == "up") pal$sage else pal$coral
      icon_char <- if (row$direction == "up") "\u25b2" else "\u25bc"

      div(style = "background:#fafaf7; border-radius:8px; padding:0.75rem; text-align:center;",
        div(style = paste0("font-size:1.4rem; font-weight:600; color:", col, ";"),
          icon_char, " ", mult_text),
        div(style = "font-size:0.85rem; color:#6b6b6b; margin-top:0.25rem;", row$order),
        div(style = "font-size:0.75rem; color:#999;",
          comma(row$Historical), " \u2192 ", comma(row$Recent))
      )
    })

    div(style = "display:grid; grid-template-columns:repeat(4, 1fr); gap:0.75rem; margin-top:0.75rem;",
      tagList(cards))
  })

  # ===================================================================
  # THREATENED — with taxonomy filters and DD
  # ===================================================================

  # Threatened tab taxonomy cascade
  output$threat_phylum_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(tax_by_order) && !is.null(input$threat_kingdom) && input$threat_kingdom != "") {
      ph <- tax_by_order |> filter(kingdom == input$threat_kingdom) |> pull(phylum) |> unique() |> sort()
      ch <- c("All" = "", setNames(ph, ph))
    }
    selectInput("threat_phylum", "Phylum", choices = ch, selected = "")
  })
  output$threat_class_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$threat_kingdom) && input$threat_kingdom != "") df <- df |> filter(kingdom == input$threat_kingdom)
      if (!is.null(input$threat_phylum) && input$threat_phylum != "") df <- df |> filter(phylum == input$threat_phylum)
      cl <- sort(unique(df$class))
      ch <- c("All" = "", setNames(cl, cl))
    }
    selectInput("threat_class", "Class", choices = ch, selected = "")
  })
  output$threat_order_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$threat_kingdom) && input$threat_kingdom != "") df <- df |> filter(kingdom == input$threat_kingdom)
      if (!is.null(input$threat_phylum) && input$threat_phylum != "") df <- df |> filter(phylum == input$threat_phylum)
      if (!is.null(input$threat_class) && input$threat_class != "") df <- df |> filter(class == input$threat_class)
      ord <- sort(unique(df$order))
      if (length(ord) <= 100) ch <- c("All" = "", setNames(ord, ord))
    }
    selectInput("threat_order", "Order", choices = ch, selected = "")
  })

  # Helper: filter taxonomic_match_summary by threat tab filters + scope
  threat_filtered_taxa <- reactive({
    req(safe_get("taxonomic_match_summary"))
    ms <- safe_get("taxonomic_match_summary") |> as_tibble()
    threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
    if (is.na(threat_col)) return(NULL)
    ms <- ms |> mutate(threatStatus = .data[[threat_col]]) |>
      filter(!is.na(threatStatus), threatStatus %in% c("CR", "EN", "VU", "NT", "LC", "DD"))

    # Apply scope filter
    scope <- input$threat_scope
    if (!is.null(scope) && scope != "all" && "establishmentMeans" %in% names(ms)) {
      if (scope == "present") {
        ms <- ms |> filter(occurrenceStatus == "present" | is.na(occurrenceStatus) | occurrenceStatus == "")
      } else if (scope == "native_present") {
        ms <- ms |> filter(establishmentMeans == "native", occurrenceStatus == "present")
      } else if (scope == "introduced_present") {
        ms <- ms |> filter(establishmentMeans %in% c("introduced", "naturalised"), occurrenceStatus == "present")
      } else if (scope == "invasive") {
        ms <- ms |> filter(establishmentMeans == "invasive")
      }
    }

    if (!is.null(input$threat_kingdom) && input$threat_kingdom != "" && "kingdom" %in% names(ms))
      ms <- ms |> filter(kingdom == input$threat_kingdom)
    if (!is.null(input$threat_phylum) && input$threat_phylum != "" && "phylum" %in% names(ms))
      ms <- ms |> filter(phylum == input$threat_phylum)
    if (!is.null(input$threat_class) && input$threat_class != "" && "class" %in% names(ms))
      ms <- ms |> filter(class == input$threat_class)
    if (!is.null(input$threat_order) && input$threat_order != "" && "order" %in% names(ms))
      ms <- ms |> filter(order == input$threat_order)
    ms
  })

  output$stat_cr <- renderText({
    ms <- threat_filtered_taxa()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "CR"], na.rm = TRUE)) else "0"
  })
  output$stat_en <- renderText({
    ms <- threat_filtered_taxa()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "EN"], na.rm = TRUE)) else "0"
  })
  output$stat_vu <- renderText({
    ms <- threat_filtered_taxa()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "VU"], na.rm = TRUE)) else "0"
  })
  output$stat_nt <- renderText({
    ms <- threat_filtered_taxa()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "NT"], na.rm = TRUE)) else "0"
  })
  output$stat_dd <- renderText({
    ms <- threat_filtered_taxa()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "DD"], na.rm = TRUE)) else "0"
  })

  output$threat_coverage <- renderPlotly({
    ms <- threat_filtered_taxa()
    if (is.null(ms) || nrow(ms) == 0) return(plotly_empty())

    has_estab <- "establishmentMeans" %in% names(ms)
    scope <- input$threat_scope

    if (has_estab && (is.null(scope) || scope == "all")) {
      # Stacked bars: native vs introduced vs invasive within each threat level
      df <- ms |>
        mutate(estab = case_when(
          establishmentMeans == "native" ~ "Native",
          establishmentMeans %in% c("introduced", "naturalised") ~ "Introduced",
          establishmentMeans == "invasive" ~ "Invasive",
          establishmentMeans == "uncertain" ~ "Uncertain",
          TRUE ~ "Unclassified"
        )) |>
        group_by(threatStatus, estab) |>
        summarise(n_total = n(), n_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop") |>
        mutate(pct = round(100 * n_gbif / n_total, 1)) |>
        filter(threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))

      estab_cols <- c(Native = pal$sage, Introduced = pal$sand, Invasive = pal$coral,
                       Uncertain = pal$slate, Unclassified = "#ccc")

      plot_ly(df, x = ~threatStatus, y = ~n_total, color = ~estab, type = "bar",
        colors = estab_cols,
        text = ~paste0(estab, ": ", n_total, " (", pct, "% in GBIF)"),
        hovertemplate = "%{text}<extra></extra>") |>
        plotly_layout(
          barmode = "stack",
          xaxis = list(title = ""),
          yaxis = list(title = "Species count"),
          legend = list(orientation = "h", y = -0.15))
    } else {
      # Simple bars (when scope is filtered — already one establishment category)
      df <- ms |>
        group_by(threatStatus) |>
        summarise(n_total = n(), n_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop") |>
        mutate(pct = round(100 * n_gbif / n_total, 1)) |>
        filter(threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))

      threat_cols <- c(CR = pal$coral, EN = pal$sand, VU = "#b8a060",
                       NT = pal$sage, LC = pal$sage2, DD = pal$slate)

      plot_ly(df, x = ~threatStatus, y = ~pct, type = "bar",
        marker = list(color = threat_cols[df$threatStatus]),
        text = ~paste0(round(pct, 1), "%"), textposition = "auto") |>
        plotly_layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Coverage %", range = c(0, 110)))
    }
  })

  output$threat_missing <- renderPlotly({
    ms <- threat_filtered_taxa()
    if (is.null(ms) || nrow(ms) == 0) return(plotly_empty())

    has_estab <- "establishmentMeans" %in% names(ms)
    scope <- input$threat_scope

    if (has_estab && (is.null(scope) || scope == "all")) {
      # Stacked by establishment means
      df <- ms |>
        filter(!matched_any) |>
        mutate(estab = case_when(
          establishmentMeans == "native" ~ "Native",
          establishmentMeans %in% c("introduced", "naturalised") ~ "Introduced",
          establishmentMeans == "invasive" ~ "Invasive",
          establishmentMeans == "uncertain" ~ "Uncertain",
          TRUE ~ "Unclassified"
        )) |>
        group_by(threatStatus, estab) |>
        summarise(n_missing = n(), .groups = "drop") |>
        filter(threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))

      estab_cols <- c(Native = pal$sage, Introduced = pal$sand, Invasive = pal$coral,
                       Uncertain = pal$slate, Unclassified = "#ccc")

      plot_ly(df, x = ~threatStatus, y = ~n_missing, color = ~estab, type = "bar",
        colors = estab_cols,
        hovertemplate = "%{x}: %{y} missing (%{fullData.name})<extra></extra>") |>
        plotly_layout(
          barmode = "stack",
          xaxis = list(title = ""),
          yaxis = list(title = "Missing taxa"),
          legend = list(orientation = "h", y = -0.15))
    } else {
      df <- ms |>
        group_by(threatStatus) |>
        summarise(n_missing = sum(!matched_any, na.rm = TRUE), .groups = "drop") |>
        filter(threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))

      threat_cols <- c(CR = pal$coral, EN = pal$sand, VU = "#b8a060",
                       NT = pal$sage, DD = pal$slate)

      plot_ly(df, x = ~threatStatus, y = ~n_missing, type = "bar",
        marker = list(color = threat_cols[df$threatStatus]),
        text = ~comma(n_missing), textposition = "auto") |>
        plotly_layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Missing taxa"))
    }
  })

  output$threat_table <- renderDT({
    ms <- threat_filtered_taxa()
    if (!is.null(ms) && nrow(ms) > 0) {
      df <- ms |>
        filter(!matched_any, threatStatus %in% c("CR", "EN", "VU", "NT", "DD")) |>
        select(any_of(c("scientificName", "threatStatus", "establishmentMeans",
                         "kingdom", "phylum", "class", "order", "family"))) |>
        arrange(factor(threatStatus, levels = c("CR", "EN", "VU", "NT", "DD")), order, family)
    } else {
      df <- tibble(Message = "No missing threatened species found for current filters.")
    }
    for (cn in c("threatStatus", "establishmentMeans", "kingdom", "phylum", "class", "order", "family")) {
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    }
    datatable(df, options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap4", filter = "top")
  })

  # ===================================================================
  # PUBLISHERS
  # ===================================================================

  publisher_summary <- safe_get("publisher_summary")
  publisher_cell_dep <- safe_get("publisher_cell_dependency")
  published_time <- safe_get("published_time_summary")

  output$pub_n_publishers <- renderText({
    if (!is.null(publisher_summary)) comma(nrow(publisher_summary)) else "?"
  })
  output$pub_n_datasets <- renderText({
    if (!is.null(publisher_summary)) comma(sum(publisher_summary$n_datasets, na.rm = TRUE)) else "?"
  })
  output$pub_single_cells <- renderText({
    if (!is.null(publisher_cell_dep)) {
      n <- sum(publisher_cell_dep$n_publishers == 1)
      total <- nrow(publisher_cell_dep)
      paste0(comma(n), " / ", comma(total))
    } else "?"
  })
  output$pub_top_pct <- renderText({
    if (!is.null(publisher_summary) && nrow(publisher_summary) > 0) {
      top_occ <- max(publisher_summary$total_occurrences, na.rm = TRUE)
      total_occ <- sum(publisher_summary$total_occurrences, na.rm = TRUE)
      paste0(round(100 * top_occ / total_occ, 1), "%")
    } else "?"
  })

  output$pub_top_chart <- renderPlotly({
    req(publisher_summary)
    df <- publisher_summary |>
      arrange(desc(total_occurrences)) |>
      head(20) |>
      mutate(label = if ("publisher_name" %in% names(publisher_summary))
        ifelse(!is.na(publisher_name), publisher_name, substr(publishingorgkey, 1, 12))
        else substr(publishingorgkey, 1, 12))

    plot_ly(df, y = ~reorder(label, total_occurrences), x = ~total_occurrences,
      type = "bar", orientation = "h",
      marker = list(color = pal$sage),
      hovertemplate = "<b>%{y}</b><br>%{x:,} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Total Occurrences"),
        yaxis = list(title = ""),
        margin = list(l = 200))
  })

  output$pub_species_chart <- renderPlotly({
    req(publisher_summary)
    df <- publisher_summary |>
      arrange(desc(n_species)) |>
      head(20) |>
      mutate(label = if ("publisher_name" %in% names(publisher_summary))
        ifelse(!is.na(publisher_name), publisher_name, substr(publishingorgkey, 1, 12))
        else substr(publishingorgkey, 1, 12))

    plot_ly(df, y = ~reorder(label, n_species), x = ~n_species,
      type = "bar", orientation = "h",
      marker = list(color = pal$slate),
      hovertemplate = "<b>%{y}</b><br>%{x:,} species<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Species Count"),
        yaxis = list(title = ""),
        margin = list(l = 200))
  })

  output$pub_dependency_map <- renderLeaflet({
    req(publisher_cell_dep, grid_10km)

    map_sf <- grid_10km |>
      left_join(publisher_cell_dep |> select(eeacellcode, n_publishers),
        by = "eeacellcode") |>
      mutate(
        n_publishers = replace_na(n_publishers, 0L),
        dep_cat = case_when(
          n_publishers == 0 ~ "No data",
          n_publishers == 1 ~ "1 (fragile)",
          n_publishers <= 3 ~ "2\u20133",
          n_publishers <= 5 ~ "4\u20135",
          TRUE ~ "6+"
        ),
        dep_cat = factor(dep_cat, levels = c("No data", "1 (fragile)", "2\u20133", "4\u20135", "6+")))

    dep_pal <- colorFactor(
      palette = c("#f0f0f0", pal$coral, pal$sand, pal$sage, pal$slate),
      domain = levels(map_sf$dep_cat), na.color = "#ddd")

    m <- leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~dep_pal(dep_cat), fillOpacity = 0.7,
        weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
          "<br><strong>Publishers:</strong> ", n_publishers)) |>
      addLegend("bottomright", pal = dep_pal, values = ~dep_cat,
        title = "Publishers per cell")

    if (!is.null(admin_level1))
      m <- m |> addPolygons(data = admin_level1, group = "admin1",
        fillColor = "transparent", fillOpacity = 0,
        weight = 1.5, color = "#333", opacity = 0.3, label = ~admin_name)
    m
  })

  output$pub_time_chart <- renderPlotly({
    req(published_time)
    df <- published_time |>
      mutate(year_published = as.integer(year_published)) |>
      filter(!is.na(year_published),
             year_published >= 2000,
             year_published <= year(Sys.Date()) + 1) |>
      group_by(year_published) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      arrange(year_published)

    if (nrow(df) == 0) {
      return(plotly_empty() |> plotly_layout(
        annotations = list(list(text = "No published time data available",
          showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5))))
    }

    plot_ly(df, x = ~year_published, y = ~occ, type = "bar",
      marker = list(color = pal$plum),
      hovertemplate = "%{x}: %{y:,.0f} records<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Year Published to GBIF", dtick = 1),
        yaxis = list(title = "Occurrences"))
  })

  output$pub_table <- renderDT({
    req(publisher_summary)
    df <- publisher_summary |>
      arrange(desc(total_occurrences)) |>
      mutate(
        pct = round(100 * total_occurrences / sum(total_occurrences, na.rm = TRUE), 2),
        name = if ("publisher_name" %in% names(publisher_summary))
          ifelse(!is.na(publisher_name), publisher_name, publishingorgkey)
          else publishingorgkey
      ) |>
      select(name, total_occurrences, pct, n_species, n_cells, n_datasets, min_year, max_year)

    datatable(df,
      colnames = c("Publisher", "Occurrences", "Share %", "Species", "Cells", "Datasets", "From", "To"),
      options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
      style = "bootstrap4") |>
      formatRound("pct", 2) |>
      formatRound("total_occurrences", digits = 0, mark = ",")
  }, server = TRUE)

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
  output$stat_resolved <- renderText({
    if (!is.null(priority_resolved)) comma(nrow(priority_resolved)) else "0"
  })

  # ---- Recommended Actions (goal-oriented) ----
  output$action_goals <- renderUI({
    n_zero  <- if (!is.null(priority_zero) && nrow(priority_zero) > 0) nrow(priority_zero) else {
      if (!is.null(dashboard)) dashboard$n_zero_coverage_cells[1] else 0
    }
    n_stale <- if (!is.null(priority_stale)) nrow(priority_stale) else 0
    n_taxa_missing <- if (!is.null(dashboard)) dashboard$n_priority_taxa[1] else 0

    # Threatened missing (CR + EN specifically)
    n_cr_en <- 0
    if (!is.null(match_summary_full)) {
      ms <- match_summary_full |> as_tibble()
      threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
      if (!is.na(threat_col)) {
        n_cr_en <- ms |> filter(.data[[threat_col]] %in% c("CR", "EN"), !matched_any) |> nrow()
      }
    }

    # Native species gap
    native_gap <- ""
    invasive_gap <- ""
    if (has_establishment && !is.null(match_summary_full)) {
      ms <- match_summary_full |> as_tibble()
      native_stats <- ms |> filter(establishmentMeans == "native")
      invasive_stats <- ms |> filter(establishmentMeans == "invasive")
      n_native_missing <- sum(!native_stats$matched_any, na.rm = TRUE)
      n_native_total <- nrow(native_stats)
      native_cov <- if (n_native_total > 0) round(100 * sum(native_stats$matched_any) / n_native_total, 1) else 0
      n_invasive_no_recent <- 0
      if (!is.null(cell_recency)) {
        # Invasive species in cells with stale data
        n_invasive_total <- nrow(invasive_stats)
        n_invasive_in_gbif <- sum(invasive_stats$matched_any, na.rm = TRUE)
        n_invasive_missing <- n_invasive_total - n_invasive_in_gbif
      } else {
        n_invasive_total <- nrow(invasive_stats)
        n_invasive_missing <- sum(!invasive_stats$matched_any, na.rm = TRUE)
      }
    }

    # Build goal cards
    goal_style <- "display:flex; align-items:flex-start; gap:1rem; padding:0.75rem 0; border-bottom:1px solid #eee;"
    icon_style <- "font-size:1.5rem; min-width:2rem; text-align:center; padding-top:0.2rem;"
    num_style <- "font-family:'IBM Plex Mono',monospace; font-size:1.2rem; font-weight:600;"

    goals <- tagList(
      # Spatial
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$coral, ";"), icon("map")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Spatial: Fill zero-coverage cells"),
          div(style = "font-size:0.85rem; color:#6b6b6b; margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$coral, ";"), comma(n_zero)),
            " grid cells have never been surveyed. Target these for new field campaigns or citizen science events.")),
      ),
      # Temporal
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$sand, ";"), icon("clock")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Temporal: Resurvey stale cells"),
          div(style = "font-size:0.85rem; color:#6b6b6b; margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$sand, ";"), comma(n_stale)),
            " cells have not been surveyed in over 5 years. Prioritise cells with historically high diversity for resurvey.")),
      ),
      # Taxonomic
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$plum, ";"), icon("leaf")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Taxonomic: Close species coverage gaps"),
          div(style = "font-size:0.85rem; color:#6b6b6b; margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$plum, ";"), comma(n_taxa_missing)),
            " species in the national backbone have no GBIF records. Focus on under-sampled orders and families shown below.")),
      ),
      # Threatened
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$coral, ";"), icon("exclamation-triangle")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Threatened: Monitor CR and EN species"),
          div(style = "font-size:0.85rem; color:#6b6b6b; margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$coral, ";"), comma(n_cr_en)),
            " critically endangered or endangered species lack any GBIF occurrence data. These are the highest priority for targeted surveys.")),
      )
    )

    # Native/invasive goal (only if data available)
    if (has_establishment && !is.null(match_summary_full)) {
      goals <- tagList(goals,
        div(style = paste0(goal_style, " border-bottom:none;"),
          div(style = paste0(icon_style, " color:", pal$sage, ";"), icon("seedling")),
          div(style = "flex:1;",
            div(style = "font-size:1.05rem; font-weight:500;", "Native species: Improve baseline coverage"),
            div(style = "font-size:0.85rem; color:#6b6b6b; margin-top:0.25rem;",
              "Native species coverage is ",
              span(style = paste0(num_style, " color:", pal$sage, ";"), paste0(native_cov, "%")),
              " (", comma(n_native_missing), " native species missing). ",
              if (n_invasive_missing > 0) paste0(comma(n_invasive_missing),
                " of ", comma(n_invasive_total), " known invasive species also lack GBIF data — monitor for spread detection.")
              else "All known invasive species have GBIF records."
            ))
        )
      )
    }

    goals
  })

  # ---- Next 12 Months targets ----
  output$next_12_months <- renderUI({
    # What was achieved in the last 12 months
    ly_occ <- if (!is.null(overview_last_year)) overview_last_year$occ_last_year else 0
    ly_cells <- if (!is.null(overview_last_year)) overview_last_year$cells_active_last_year else 0
    ly_new_cells <- if (!is.null(overview_last_year)) overview_last_year$cells_newly_covered else 0
    ly_resolved <- if (!is.null(priority_resolved)) nrow(priority_resolved) else 0

    # Compute species added in last 12 months (approximate from match_summary)
    ly_species <- 0
    if (!is.null(match_summary_full)) {
      # Species with any occurrence in the recent period — approximate from total
      # Use overview data if available
      ly_species <- if (!is.null(overview_last_year$species_last_year))
        overview_last_year$species_last_year else round(ly_occ / 50)  # rough estimate
    }

    # Targets: 1.5× what was achieved (aspirational but grounded)
    target_mult <- 1.5
    target_new_cells <- ceiling(ly_new_cells * target_mult)
    target_resolved <- ceiling(max(ly_resolved, 5) * target_mult)

    n_zero <- if (!is.null(priority_zero)) nrow(priority_zero) else 0
    n_stale <- if (!is.null(priority_stale)) nrow(priority_stale) else 0
    n_cr_en <- 0
    if (!is.null(match_summary_full)) {
      ms <- match_summary_full |> as_tibble()
      threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
      if (!is.na(threat_col))
        n_cr_en <- ms |> filter(.data[[threat_col]] %in% c("CR", "EN"), !matched_any) |> nrow()
    }

    row_style <- "display:flex; align-items:center; padding:0.6rem 0; border-bottom:1px solid #f0f0f0;"
    label_style <- "flex:2; font-size:0.95rem;"
    achieved_style <- "flex:1; text-align:center; font-family:'IBM Plex Mono',monospace; font-size:1.05rem;"
    target_style <- "flex:1; text-align:center; font-family:'IBM Plex Mono',monospace; font-size:1.05rem; font-weight:600;"
    header_style <- "display:flex; padding:0.4rem 0; border-bottom:2px solid #ddd; margin-bottom:0.25rem;"

    tagList(
      div(style = header_style,
        div(style = paste0(label_style, " font-weight:600;"), "Metric"),
        div(style = paste0(achieved_style, " font-weight:600; color:", pal$slate, ";"), paste0("Achieved (", last_year_label, ")")),
        div(style = paste0(target_style, " font-weight:600; color:", pal$sage, ";"), "Next 12 Months Target")
      ),
      div(style = row_style,
        div(style = label_style, "New occurrence records"),
        div(style = achieved_style, comma(ly_occ)),
        div(style = paste0(target_style, " color:", pal$sage, ";"), paste0(comma(ceiling(ly_occ * target_mult)), "+"))
      ),
      div(style = row_style,
        div(style = label_style, "Cells with active recording"),
        div(style = achieved_style, comma(ly_cells)),
        div(style = paste0(target_style, " color:", pal$sage, ";"), paste0(comma(ceiling(ly_cells * target_mult)), "+"))
      ),
      div(style = row_style,
        div(style = label_style, "Newly covered cells (previously zero)"),
        div(style = achieved_style, comma(ly_new_cells)),
        div(style = paste0(target_style, " color:", pal$sage, ";"),
          paste0(comma(target_new_cells), " / ", comma(n_zero), " remaining"))
      ),
      div(style = row_style,
        div(style = label_style, "Priority zero-cells resolved"),
        div(style = achieved_style, comma(ly_resolved)),
        div(style = paste0(target_style, " color:", pal$sage, ";"), comma(target_resolved))
      ),
      div(style = paste0(row_style, " border-bottom:none;"),
        div(style = label_style, "CR/EN species with new records"),
        div(style = achieved_style, "\u2014"),
        div(style = paste0(target_style, " color:", pal$coral, ";"),
          paste0("Target: ", comma(min(n_cr_en, 20)), " of ", comma(n_cr_en), " missing"))
      )
    )
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
      palette = c(pal$sand, pal$coral, "#3d4f6a"),
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
  # PRIORITIES — Taxonomic Mobilization Targets
  # ===================================================================

  output$priority_undersampled_orders <- renderPlotly({
    req(troudet_bias_order)
    df <- troudet_bias_order |>
      filter(bias < 0) |>
      arrange(bias) |>
      slice_head(n = 15) |>
      mutate(gap_species = n_known_species - n_in_gbif)

    plot_ly(df, y = ~reorder(order, -bias), x = ~gap_species, type = "bar",
      orientation = "h", marker = list(color = pal$coral),
      text = ~paste0(gap_species, " missing"), textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      hovertemplate = "%{y}<br>%{x} species missing from GBIF<extra></extra>") |>
      plotly_layout(
        title = list(text = "Most under-sampled orders", font = list(size = 14)),
        xaxis = list(title = "Species missing from GBIF"),
        yaxis = list(title = ""))
  })

  output$priority_undersampled_families <- renderPlotly({
    req(tax_by_family)
    df <- tax_by_family |>
      filter(n_missing > 0) |>
      arrange(desc(n_missing)) |>
      slice_head(n = 15)

    plot_ly(df, y = ~reorder(family, n_missing), x = ~n_missing, type = "bar",
      orientation = "h", marker = list(color = pal$sand),
      text = ~paste0(n_missing, " missing"), textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      hovertemplate = "%{y}<br>%{x} species missing from GBIF<extra></extra>") |>
      plotly_layout(
        title = list(text = "Most under-sampled families", font = list(size = 14)),
        xaxis = list(title = "Species missing from GBIF"),
        yaxis = list(title = ""))
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
