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

# Establishment means / scope data
match_summary_full   <- safe_get("taxonomic_match_summary")
tax_by_establishment <- safe_get("tax_by_establishment")
has_establishment    <- !is.null(match_summary_full) &&
                        "establishmentMeans" %in% names(match_summary_full)

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
country_name  <- tryCatch(yaml::read_yaml("../../config.yml")$country$name, error = function(e) "")

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
            div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
              div(class = "stat-box",
                div(class = "stat-value sage", textOutput("ov_ly_occ", inline = TRUE)),
                div(class = "stat-label", paste0("New Records (", last_year_label, ")"))),
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
          fluidRow(
            column(8, div(class = "card",
              div(class = "card-title", icon("globe-europe"), "Geographic Coverage"),
              leafletOutput("spatial_map", height = "520px"))),
            column(4,
              div(class = "card",
                div(class = "card-title", icon("sliders-h"), "Display"),
                radioButtons("map_var", NULL,
                  choices = setNames(
                    c("occ", "stale", "richness", "last_year"),
                    c("Occurrences", "Data recency",
                      "Species richness", paste0(last_year_label, " additions"))),
                  selected = "occ")),
              if (has_admin) div(class = "card",
                div(class = "card-title", icon("border-all"), "Administrative Boundaries"),
                if (!is.null(admin_level1)) checkboxInput("show_admin1", "Show regions", value = FALSE),
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
          uiOutput("basis_stat_boxes"),
          fluidRow(
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-pie"), "Occurrences by Basis of Record"),
              plotlyOutput("basis_pie", height = "340px"))),
            column(6, div(class = "card",
              div(class = "card-title", icon("chart-line"), "Temporal Trend by Basis"),
              selectInput("basis_timeline_select", "Select basis:",
                choices = basis_types_no_all, width = "250px"),
              plotlyOutput("basis_timeline", height = "290px")))
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

          # Cascading taxonomy filters — now includes Family + Scope
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
              column(2,
                if (has_establishment) tagList(
                  div(class = "filter-label", "Scope"),
                  selectInput("tax_scope", NULL,
                    choices = scope_choices, selected = "all")
                ) else tagList(
                  div(class = "filter-label", "Last Year"),
                  checkboxInput("tax_show_last_year", paste0("Highlight ", last_year_label),
                    value = FALSE)
                ))
            ),
            if (has_establishment) fluidRow(
              column(10),
              column(2,
                div(class = "filter-label", "Last Year"),
                checkboxInput("tax_show_last_year", paste0("Highlight ", last_year_label),
                  value = FALSE))
            )
          ),

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
                  choices = scope_choices, selected = "all")
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
      # PRIORITIES TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("bullseye"), "Priorities"),
        value = "priorities",
        div(style = "padding: 1.25rem 0;",

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

  # Temporal data: use family_time_summary if family filter active,
  # order_time_summary if order/class/phylum/kingdom active, else time_summary
  temp_data <- reactive({
    sel_family <- temp_filtered_families()
    orders <- temp_filtered_orders()
    has_tax_filter <- !is.null(input$temp_kingdom) && input$temp_kingdom != ""

    if (!is.null(sel_family) && !is.null(family_time_summary)) {
      # Family-level filtering
      family_time_summary |>
        filter(basisofrecord == "all",
               family == sel_family,
               year >= input$year_range[1],
               year <= input$year_range[2])
    } else if (has_tax_filter && !is.null(order_time_summary) && !is.null(orders)) {
      # Order-level filtering
      order_time_summary |>
        filter(basisofrecord == "all",
               order %in% orders,
               year >= input$year_range[1],
               year <= input$year_range[2])
    } else {
      req(time_summary)
      time_summary |>
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

    heatmap_cs <- list(
      list(0, "#f6f5f1"), list(0.25, "#c8dbc6"),
      list(0.5, pal$sage), list(0.75, pal$sand),
      list(1, pal$coral))
    plot_ly(hm, x = ~month, y = ~year, z = ~log_occ, type = "heatmap",
      colorscale = heatmap_cs,
      hovertemplate = "Year: %{y}<br>Month: %{x}<br>log10(occ): %{z:.1f}<extra></extra>") |>
      plotly_layout(
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

  # Reactive map update when map_var or basis changes
  observe({
    req(grid_10km, spatial_gaps, input$map_var)

    sf_base <- spatial_gaps |> filter(basisofrecord == basis_selected())

    if (input$map_var == "stale" && !is.null(cell_recency)) {
      rec <- cell_recency |> filter(basisofrecord == basis_selected()) |>
        select(eeacellcode, staleness_months)
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
        palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral, "#8b2020", "#ddd"),
        domain = levels(map_sf$stale_cat), na.color = "#ddd")
      legend_title <- "Data recency"
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
        palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral, "#ddd"),
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

    } else if (input$map_var == "last_year" && !is.null(cell_last_year)) {
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
                          "<br>", last_year_label, " occ: ", comma(last_year),
                          "<br>Prior occ: ", comma(prior),
                          if_else(newly_covered, "<br><strong>Newly covered!</strong>", ""))

      leafletProxy("spatial_map", data = map_sf) |>
        clearShapes() |> clearControls() |>
        addPolygons(
          fillColor   = ~fill_col,
          fillOpacity = 0.7, weight = 0.3, color = "#999",
          popup       = popup_fn) |>
        addLegend("bottomright",
          colors = c(pal$coral, pal$sage, "#e0dfda"),
          labels = c("Newly covered", paste0("Active in ", last_year_label), "No data in this year"),
          title = paste0(last_year_label, " additions"))
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
        palette = c("#2A9D8F", "#6b8f71", pal$sand, pal$coral, "#8b2020", "#ddd"),
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
        title = list(text = "Distribution per 10km cell", font = list(size = 13)),
        xaxis = list(title = "log10(Occurrences + 1)"),
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

  # Pie chart with readable labels
  output$basis_pie <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps |>
      filter(basisofrecord != "all") |>
      group_by(basisofrecord) |>
      summarise(total_occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_occ)) |>
      mutate(
        pct = round(100 * total_occ / sum(total_occ), 1),
        label_text = str_replace_all(basisofrecord, "_", " ")
      )

    cols <- sapply(df$basisofrecord, get_basis_color)

    plot_ly(df, labels = ~label_text, values = ~total_occ, type = "pie",
      marker = list(colors = cols, line = list(color = "#fff", width = 1.5)),
      textinfo = "label+percent",
      textposition = "outside",
      textfont = list(size = 11),
      outsidetextfont = list(size = 10),
      hovertemplate = "%{label}<br>%{value:,.0f} occurrences<br>%{percent}<extra></extra>") |>
      plotly_layout(showlegend = FALSE,
        margin = list(l = 40, r = 40, t = 20, b = 20))
  })

  # Timeline — single basis at a time
  output$basis_timeline <- renderPlotly({
    req(time_summary, input$basis_timeline_select)
    sel <- input$basis_timeline_select

    df <- time_summary |>
      filter(basisofrecord == sel, !is.na(year), year >= 1970) |>
      group_by(year) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")

    base_col <- get_basis_color(sel)

    plot_ly(df, x = ~year, y = ~occ, type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = paste0(base_col, "33"),
      line = list(color = base_col, width = 2),
      hovertemplate = "%{x}: %{y:,.0f}<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Occurrences"))
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
             occ_prior = total_occ, occ_last_year = 0)
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
             occ_prior = total_occ, occ_last_year = 0)
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
             occ_prior = total_occ, occ_last_year = 0)
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
    !is.null(input$tax_scope) && input$tax_scope != "all"
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
    show_ly <- !is.null(input$tax_show_last_year) && input$tax_show_last_year &&
               "occ_last_year" %in% names(df)

    if (show_ly) {
      df <- df |>
        mutate(
          bias_prior = occ_prior - ideal_occ,
          bias_last_year = occ_last_year,
          bar_col_ly = ifelse(bias >= 0, pal$sage2, pal$sand)
        )

      p <- plot_ly(df, y = ~reorder(label, bias), orientation = "h") |>
        add_trace(x = ~bias_prior, type = "bar", name = "Prior years",
          marker = list(color = ~bar_col),
          hovertemplate = "%{y}: %{x:,.0f}<extra>Prior</extra>") |>
        add_trace(x = ~bias_last_year, type = "bar",
          name = last_year_label,
          marker = list(color = ~bar_col_ly),
          hovertemplate = "%{y}: +%{x:,.0f}<extra>Last year</extra>")
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

    show_ly <- !is.null(input$tax_show_last_year) && input$tax_show_last_year &&
               "occ_last_year" %in% names(df)

    p <- plot_ly(df, y = ~reorder(order, n_taxa), x = ~n_in_gbif, type = "bar",
      name = "In GBIF", marker = list(color = pal$sage),
      text = ~label, textposition = "auto",
      textfont = list(size = 10, color = "#fff"),
      orientation = "h") |>
      add_trace(x = ~miss, name = "Missing from GBIF",
        marker = list(color = pal$sand2),
        text = "", textposition = "none")

    if (show_ly && "occ_last_year" %in% names(df)) {
      p <- p |> add_annotations(
        x = ~n_in_gbif / 2,
        y = ~order,
        text = ~ifelse(occ_last_year > 0,
          paste0(last_year_label, ": ", comma(occ_last_year), " occ"), ""),
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
