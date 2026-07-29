# =============================================================================
# GBIF Gap Finder — Dashboard
# =============================================================================
# Interactive dashboard for identifying and prioritising GBIF data gaps
# across spatial, temporal, and taxonomic dimensions.
#
# To run: shiny::runApp("shiny_app/gap_finder")
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

# Resolve the per-country data bundle. GBIF_GAP_COUNTRY is the single project-wide
# country selector (see R/globals.R): set it, or let the app auto-detect a lone bundle.
resolve_data_path <- function() {
  cc <- Sys.getenv("GBIF_GAP_COUNTRY", "")
  if (nzchar(cc)) {
    p <- file.path("data", cc, "shiny_data.rds")
    if (file.exists(p)) return(p)
    stop(sprintf("GBIF_GAP_COUNTRY='%s' is set but %s does not exist.", cc, p))
  }
  per_country <- Sys.glob(file.path("data", "*", "shiny_data.rds"))
  if (length(per_country) == 1L) return(per_country)
  if (length(per_country) > 1L)
    stop("Multiple country bundles found (",
         paste(basename(dirname(per_country)), collapse = ", "),
         "); set the GBIF_GAP_COUNTRY environment variable to choose one.")
  stop("No data/<CC>/shiny_data.rds found. Set GBIF_GAP_COUNTRY and run scripts/11_prepare_gap_finder_data.R")
}

data_path <- resolve_data_path()

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
# DERIVE zero-coverage cells from spatial_gaps — the single 10km source the
# Overview and Spatial tab already use — rather than the priority file, which 07
# builds by stacking 10km AND 50km empties. This makes Priorities == Overview by
# construction, regardless of that file's columns or grid labels.
if (!is.null(spatial_gaps) &&
    all(c("basisofrecord", "has_data", "eeacellcode") %in% names(spatial_gaps))) {
  priority_zero <- spatial_gaps |>
    dplyr::filter(basisofrecord == "all", !has_data) |>
    dplyr::transmute(
      eeacellcode,
      n_species       = 0L,
      occurrences     = 0,
      priority_reason = "Zero coverage - never sampled",
      priority_level  = "HIGH"
    )
}
priority_stale  <- safe_get("priority_stale_cells")
comparison_grids <- safe_get("comparison_grids")
metadata        <- safe_get("metadata")
spatial_overview <- safe_get("spatial_overview")

# ---- T-D5 marine coverage toggle ------------------------------------------
# cell_marine_lookup (eeacellcode + marine) is written by script 11 when the
# grid carries EEZ sea cells (marine.enabled). Precompute the marine cell codes
# once; the header toggle (server) filters the spatial coverage + recency views
# to land only when asked. Absent lookup / no marine cells => toggle hidden.
cell_marine_lookup <- safe_get("cell_marine_lookup")
marine_codes <- if (!is.null(cell_marine_lookup) &&
                    all(c("eeacellcode", "marine") %in% names(cell_marine_lookup))) {
  as.character(cell_marine_lookup$eeacellcode[cell_marine_lookup$marine %in% TRUE])
} else character(0)
has_marine <- length(marine_codes) > 0

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
last_year_ref        <- safe_get("last_year")  # observed cutoff (yearmonth), e.g. 202504
recent_label_stored  <- safe_get("recent_label")  # e.g. "Apr 2025 - Mar 2026"

# Full-GBIF per-basis recent layer (Record Types tab + Overview)
all_basis_recent            <- safe_get("all_basis_recent")

# Establishment means / scope data
match_summary_full   <- safe_get("taxonomic_match_summary")
tax_by_establishment <- safe_get("tax_by_establishment")
has_establishment    <- !is.null(match_summary_full) &&
                        "establishmentMeans" %in% names(match_summary_full)

# Order exclusions (e.g. Primates = Homo sapiens) are applied once at the source,
# in the pipeline, via parameters.taxonomic.exclude_orders in the country config
# (script 09b). Every taxonomic table the app loads — match summary, by-order /
# by-family coverage, Troudet bias — is therefore already filtered, so no in-app
# exclusion is needed.

# Scope-flag lookup (in_dyntaxa / invasive / sensitive / threatened) for the Concern
# tab + data browser
species_scope_lookup  <- safe_get("species_scope_lookup")
tax_by_invasive       <- safe_get("tax_by_invasive")
kingdom_cell_recency  <- safe_get("kingdom_cell_recency")
tax_cell_recency      <- safe_get("tax_cell_recency")

# Species of Concern — scope-specific data (from 09c via 11)
threatened_spatial_gaps <- safe_get("threatened_spatial_gaps")
threatened_basis_recent <- safe_get("threatened_basis_recent")
threatened_cell_summary <- safe_get("threatened_cell_summary")
invasive_spatial_gaps   <- safe_get("invasive_spatial_gaps")
invasive_basis_recent   <- safe_get("invasive_basis_recent")
invasive_cell_summary   <- safe_get("invasive_cell_summary")
invasive_time_summary   <- safe_get("invasive_time_summary")
sensitive_spatial_gaps   <- safe_get("sensitive_spatial_gaps")
sensitive_basis_recent   <- safe_get("sensitive_basis_recent")
sensitive_cell_summary   <- safe_get("sensitive_cell_summary")

# Publisher taxonomy cross-tabs (from 06a via 11)
publisher_taxonomy      <- safe_get("publisher_taxonomy")
publisher_cell_taxonomy <- safe_get("publisher_cell_taxonomy")

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

# D3: the temporal tab defaults to the last *complete* year. The latest year in
# the data is the snapshot (partial) year, so treat it as partial and default the
# slider's upper bound to the year before it; the partial year stays selectable.
data_max_year <- if (!is.null(time_summary) && "year" %in% names(time_summary) &&
                     any(!is.na(time_summary$year))) {
  as.integer(max(time_summary$year, na.rm = TRUE))
} else current_year
last_complete_year <- max(data_max_year - 1L, 1971L)

# Country name travels in the data bundle (stamped by script 11 from the
# active config), so the app shows whichever country it was built for.
country_name <- tryCatch({
  cn <- app_data$metadata$country_name
  if (is.null(cn) || is.na(cn) || !nzchar(cn)) "" else cn
}, error = function(e) "")

# Adjective / demonym for headline labels (e.g. "Total Swedish Occurrences"),
# derived from the country code so the Norway and other ports read correctly.
country_adjective <- {
  cc <- tryCatch(app_data$metadata$country_code, error = function(e) NULL)
  demonyms <- c(SE = "Swedish", NO = "Norwegian", FI = "Finnish", DK = "Danish",
                DE = "German", FR = "French", GB = "British", NL = "Dutch", IE = "Irish")
  key <- toupper(if (is.null(cc) || is.na(cc)) "" else as.character(cc))
  if (nzchar(key) && key %in% names(demonyms)) demonyms[[key]]
  else if (nzchar(country_name)) country_name else ""
}

# Cascading filter choices (for taxonomic tab)
kingdom_choices <- if (!is.null(tax_by_order) && "kingdom" %in% names(tax_by_order)) {
  sort(unique(tax_by_order$kingdom[!is.na(tax_by_order$kingdom) & tax_by_order$kingdom != ""]))
} else character(0)

# Publisher tab: taxonomic filter choices
pub_kingdom_choices <- if (!is.null(publisher_taxonomy) && "kingdom" %in% names(publisher_taxonomy)) {
  sort(unique(publisher_taxonomy$kingdom[
      !is.na(publisher_taxonomy$kingdom) & publisher_taxonomy$kingdom != ""
    ]))
} else character(0)

# Truncate long publisher names for chart labels
truncate_name <- function(x, max_chars = 40) {
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 1), "\u2026"), x)
}

# Accessible inline glossary tooltip (EAA). Renders a dotted-underlined term that
# reveals a short definition on hover AND keyboard focus (tabindex = 0); the
# definition is also exposed to screen readers via aria-label. Full definitions
# live in the Overview "Methods, limitations & glossary" panel.
gloss <- function(term, definition) {
  tags$span(
    class = "gloss", tabindex = "0",
    `data-tip` = definition,
    `aria-label` = paste0(term, ": ", definition),
    term
  )
}

# Consistent "measurement -> interpretation -> action" guide shown at the top of
# each analysis tab (plain-language framing for a broad, non-specialist audience).
read_guide <- function(measures, interpret, action) {
  div(class = "read-guide", role = "note", `aria-label` = "How to read this tab",
    div(tags$span(class = "rg-label", "What it measures"), measures),
    div(tags$span(class = "rg-label", "How to read it"), interpret),
    div(tags$span(class = "rg-label", "What to do"), action))
}

# Download handler that writes a data frame (geometry dropped) to CSV.
# Used for the map "Download cell data" buttons and the Data & Sources table.
dl_csv <- function(data_fun, prefix) {
  downloadHandler(
    filename = function() paste0(prefix, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      d <- tryCatch(data_fun(), error = function(e) NULL)
      if (is.null(d) || !NROW(d))
        d <- tibble::tibble(Message = "No data available for the current selection.")
      if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
      readr::write_csv(d, file)
    }
  )
}

# Small right-aligned CSV button placed beneath a map (exports the map's cells).
map_dl_btn <- function(id, label = "Download cell data (CSV)") {
  div(style = "margin-top:0.6rem; text-align:right;",
    downloadButton(id, label, class = "btn-download",
      style = "font-size:0.8rem; padding:3px 12px;"))
}

# Classify publishers by name into institutional categories
classify_publisher <- function(name) {
  name_lc <- tolower(name)
  dplyr::case_when(
    # Citizen science — public observation platforms & recording networks
    grepl("artdatabanken|artportalen|inaturalist|naturalist|ebird|bird\\.se|observation\\.org|svalan|fågel|naturgucker|naturglucker", name_lc) ~ "Citizen science",
    # Private sector — consultancies & companies
    grepl("\\bab\\b|aktiebolag|consult|calluna|ecocom|ecogain|naturcentrum|medins|pelagia|enetjärn|greensway|\\bwsp\\b|sweco|niras|\\bafry\\b|\\bltd\\b|\\bllc\\b|\\binc\\b|gmbh|corporation", name_lc) ~ "Private sector",
    # Research data — universities, museums, herbaria, government agencies,
    # sequencing facilities, field stations & marine institutes, and any other
    # research/institutional publisher (default). Derived from publisher_name.
    TRUE ~ "Research data"
  )
}

# Label for recent period (last 12 months)
last_year_label <- if (!is.null(recent_label_stored)) {
  recent_label_stored
} else if (!is.null(last_year_ref)) {
  as.character(last_year_ref)
} else {
  "last 12 months"
}

# Plotly theme helper — light background, warm palette
plotly_layout <- function(p, ..., dl_title = NULL) {
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
  
  base_layout <- list(
    p = p,
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#fafaf7",
    font = list(color = "#2d2d2d", family = "Outfit"),
    margin = list(l = 60, r = 30, t = 40, b = 60)
  )
  # Optional in-plot title so downloaded chart images stay self-describing for
  # crowded (> 20 group) bar charts. Callers pass dl_title conditionally.
  if (!is.null(dl_title)) {
    base_layout$title <- list(text = dl_title, x = 0.02, xanchor = "left",
      font = list(size = 14, family = "Outfit", color = "#2d2d2d"))
    base_layout$margin$t <- 70
  }
  p <- do.call(layout, c(base_layout, args))
  
  # Apply reduced toolbar to every plotly chart
  p |> plotly::config(
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "zoom2d", "pan2d", "lasso2d", "select2d", "autoScale2d",
      "hoverCompareCartesian", "hoverClosestCartesian",
      "toggleSpikelines"
    ),
    toImageButtonOptions = list(
      format = "png", width = 1400, height = 800, scale = 2,
      filename = "gbif_gap_finder_chart"
    )
  )
}

# ==============================================================================
# GBIF-style taxonomic groups
# ==============================================================================
# Curated mixed-rank mapping used on the Troudet chart's landing view (when no
# kingdom is selected). Modeled on the groups GBIF uses on country pages.
# Each group has:
#   - name         : display label
#   - match_col    : the column in the data to match on ("kingdom", "phylum", or "class")
#   - values       : one or more allowed values in that column
#
# Rows that don't match any group fall into "Other". Order here is the display
# order (top of chart = top of list, but the chart re-sorts by |bias|).
#
# If users want to edit the mapping for a different country, this is the only
# place to change. Stored as a tibble for ease of filtering.

gbif_style_groups <- tibble::tribble(
  ~name,             ~match_col, ~values,
  "Birds",           "class",    list("Aves"),
  "Mammals",         "class",    list("Mammalia"),
  "Reptiles",        "class",    list("Reptilia", "Squamata", "Testudines"),
  "Amphibians",      "class",    list("Amphibia"),
  "Fish",            "class",    list("Actinopterygii", "Chondrichthyes",
                                       "Cephalaspidomorphi", "Myxini", "Sarcopterygii"),
  "Insects",         "class",    list("Insecta"),
  "Arachnids",       "class",    list("Arachnida"),
  "Molluscs",        "phylum",   list("Mollusca"),
  "Crustaceans",     "class",    list("Malacostraca", "Maxillopoda", "Branchiopoda",
                                       "Ostracoda", "Hexanauplia"),
  "Other invertebrates", "phylum", list("Annelida", "Platyhelminthes", "Nematoda",
                                         "Cnidaria", "Echinodermata", "Porifera",
                                         "Bryozoa", "Rotifera", "Tardigrada"),
  "Vascular plants", "phylum",   list("Tracheophyta"),
  "Mosses & liverworts", "phylum", list("Bryophyta", "Marchantiophyta", "Anthocerotophyta"),
  "Algae",           "phylum",   list("Chlorophyta", "Rhodophyta", "Ochrophyta",
                                       "Charophyta", "Bacillariophyta", "Haptophyta",
                                       "Cryptophyta"),
  "Fungi",           "kingdom",  list("Fungi"),
  "Bacteria & Archaea", "kingdom", list("Bacteria", "Archaea"),
  "Protists",        "kingdom",  list("Protozoa", "Chromista")
)

#' Aggregate a per-class or per-order Troudet data frame into GBIF-style groups.
#' Any row that doesn't match any group goes into "Other".
#' @param df  A data frame with kingdom, phylum, class columns plus numeric cols.
#' @return  A data frame with one row per group + an added `label` column.
aggregate_to_gbif_groups <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Assign each row to a group (or "Other")
  df <- df |>
    mutate(gbif_group = NA_character_)

  for (i in seq_len(nrow(gbif_style_groups))) {
    g <- gbif_style_groups[i, ]
    col <- g$match_col
    vals <- unlist(g$values)
    if (!col %in% names(df)) next
    df <- df |>
      mutate(gbif_group = ifelse(is.na(gbif_group) & .data[[col]] %in% vals,
                                 g$name, gbif_group))
  }
  df <- df |> mutate(gbif_group = ifelse(is.na(gbif_group), "Other", gbif_group))

  # Aggregate (sum numeric cols; take kingdom from first row for compatibility)
  num_cols <- intersect(
    c("n_known_species", "n_in_gbif",
      "occ_prior", "occ_last_year", "total_occ"),
    names(df)
  )

  agg <- df |>
    group_by(gbif_group) |>
    summarise(across(all_of(num_cols), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop")

  # Recompute bias from the aggregated counts
  if (all(c("n_known_species", "total_occ") %in% names(agg))) {
    agg <- agg |>
      mutate(
        total_known = sum(n_known_species, na.rm = TRUE),
        total_occ_all = sum(total_occ, na.rm = TRUE),
        pct_known = n_known_species / total_known,
        ideal_occ = pct_known * total_occ_all,
        bias = total_occ - ideal_occ,
        label = gbif_group
      )
  } else {
    agg <- agg |> mutate(label = gbif_group)
  }

  agg
}

# Palette
pal <- list(
  sage  = "#2A7F62", sage2 = "#44BB99",
  slate = "#4477AA", slate2 = "#77AADD",
  sand  = "#CCBB44", sand2  = "#EEDD88",
  coral = "#EE6677", coral2 = "#FFAABB",
  plum  = "#AA3377",
  text  = "#2d2d2d", muted = "#6b6b6b"
)


# =============================================================================
# UI
# =============================================================================

# Source citation for a national checklist (red list / invasives / sensitive),
# pulled from the resolved provenance baked into the bundle by 01b
# (metadata$data_sources$checklists). Returns NULL when the checklist or its
# provenance is absent, so the Concern sub-tabs degrade gracefully.
checklist_cite <- function(key, prefix = "Source") {
  cl <- tryCatch(metadata$data_sources$checklists[[key]], error = function(e) NULL)
  if (is.null(cl)) return(NULL)
  title <- cl$title %||% cl$name %||% cl$label %||% key
  has_doi <- !is.null(cl$doi) && !is.na(cl$doi)
  href <- if (has_doi) {
    cl$doi
  } else if (!is.null(cl$dataset_key) && !is.na(cl$dataset_key)) {
    paste0("https://www.gbif.org/dataset/", cl$dataset_key)
  } else NULL
  link_txt <- if (has_doi) sub("https://doi.org/", "", cl$doi) else "View on GBIF"
  div(class = "info-note", style = "margin-top: 0.75rem; font-size: 0.9rem;",
    icon("book", style = "margin-right: 0.3rem;"),
    tags$strong(paste0(prefix, ": ")), title,
    if (!is.null(href)) tagList(" — ",
      tags$a(href = href, link_txt, target = "_blank",
        style = "color: var(--sage); text-decoration: underline;")))
}

# External reference link for a taxon (Wikipedia article by scientific name).
# Rendered in a compact "Ref" column (escape = FALSE) so the plain scientificName
# column stays clean for search / filtering / CSV export.
taxon_ref <- function(name) {
  u <- vapply(name, function(x) {
    if (is.na(x) || !nzchar(x)) "" else utils::URLencode(gsub(" ", "_", x), reserved = TRUE)
  }, character(1))
  ifelse(nzchar(u), sprintf(
    "<a href=\"https://en.wikipedia.org/wiki/%s\" target=\"_blank\" rel=\"noopener\">Wikipedia</a>",
    u), "")
}

ui <- fluidPage(

  tags$head(
    tags$title(if (nchar(country_name) > 0) paste0("GBIF Gap Finder \u2014 ", country_name) else "GBIF Gap Finder"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Header
  div(class = "main-header",
    div(
      tags$h1(class = "main-title",
        if (nchar(country_name) > 0) paste0("\U0001f4ca ", country_name, " — GBIF Gap Finder")
        else "\U0001f4ca GBIF Gap Finder"),
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
          span(style = "font-size:1rem; color:#6b6b6b;", "Record type:"),
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

          # ---- Auto-generated "key findings" strip (top of Overview) ----
          # A "what matters most" summary for policymakers: computed live from the
          # bundle (never hardcoded); each finding reuses the same reactive its tab
          # uses so the numbers reconcile, and a missing bundle object drops its
          # card (graceful degradation for the Norway/Finland port).
          uiOutput("key_findings"),

          # Welcome / orientation — always visible, integrates the persona guide
          div(class = "card", style = "margin-bottom: 1.25rem; border-left: 4px solid var(--sage);",
            tags$h2(class = "card-title", icon("compass"), "What this dashboard shows \u2014 and where to start"),
            p(style = "color: var(--text-secondary); margin: 0 0 0.85rem; line-height: 1.7;",
              "GBIF brings together hundreds of millions of species records from museums, researchers, ",
              "and citizen scientists \u2014 but that coverage is uneven. Some places, time periods, and species ",
              "are recorded far more thoroughly than others. This dashboard maps those gaps for ",
              tags$strong(if (nchar(country_name) > 0) country_name else "the country"),
              ": it shows where the records on GBIF are thin, so that limited survey time, digitisation effort, ",
              "data mobilisation, and funding can be aimed where they will do the most good."),
            p(style = "color: var(--text-secondary); margin: 0 0 1.25rem; line-height: 1.7;",
              "It looks at four kinds of gap \u2014 ", tags$strong("where"), " records are missing across the map, ",
              tags$strong("when"), " recording tails off, ", tags$strong("which"), " species are under-recorded, ",
              "and ", tags$strong("who"), " is publishing the data \u2014 with extra attention to threatened, invasive, ",
              "and sensitive species. It also sorts records by ", tags$strong("type"),
              " \u2014 field observations, preserved museum specimens, and DNA sequences \u2014 so you can see not just ",
              "how much data a place has, but whether it is the kind that can support your work. ",
              "Every figure on every tab is drawn from the same GBIF data, compared against ",
              tags$strong(if (nchar(country_name) > 0) country_name else "the country"),
              "'s national species checklist, so the numbers stay consistent as you explore."),

            p(style = "margin: 0 0 1.25rem; line-height: 1.7; padding: 0.7rem 0.9rem; background: var(--slate-light); border-left: 3px solid var(--slate); border-radius: 0 var(--radius-sm) var(--radius-sm) 0; color: var(--text-secondary);",
              tags$strong("One important caveat:"), " a gap here means missing ", tags$em("GBIF"), " records \u2014 not ",
              "that a species is absent, unstudied, or unmonitored. Recent or non-digitised data may exist outside ",
              "GBIF, so treat these views as a guide to where to look, not as conclusions."),

            div(style = "font-family: 'Fraunces', serif; font-size: 1.28rem; font-weight: 600; color: var(--text-primary); margin: 0 0 0.85rem;",
              "New here? Jump straight to what matters to you:"),
            fluidRow(
              column(4, div(style = "height: 100%; padding: 1rem; background: var(--bg-subtle); border-radius: var(--radius); border-left: 4px solid var(--sage);",
                tags$h3(style = "font-family: 'Fraunces', serif; font-size: 1.05rem; margin: 0 0 0.4rem;", "Researchers & field biologists"),
                p(style = "color: var(--text-secondary); line-height: 1.55; margin-bottom: 0.85rem;",
                  "Find under-recorded species and regions worth targeting for new fieldwork or specimen digitisation."),
                actionButton("goto_taxonomic", tagList(icon("leaf"), " Find taxonomic gaps"), class = "btn-primary"))),
              column(4, div(style = "height: 100%; padding: 1rem; background: var(--bg-subtle); border-radius: var(--radius); border-left: 4px solid var(--slate);",
                tags$h3(style = "font-family: 'Fraunces', serif; font-size: 1.05rem; margin: 0 0 0.4rem;", "Data publishers & collections"),
                p(style = "color: var(--text-secondary); line-height: 1.55; margin-bottom: 0.85rem;",
                  "See where published records fill gaps, and which map cells depend on a single data publisher."),
                actionButton("goto_publishers", tagList(icon("building"), " Open Publishers"), class = "btn-primary"))),
              column(4, div(style = "height: 100%; padding: 1rem; background: var(--bg-subtle); border-radius: var(--radius); border-left: 4px solid var(--coral);",
                tags$h3(style = "font-family: 'Fraunces', serif; font-size: 1.05rem; margin: 0 0 0.4rem;", "Policy makers & GBIF nodes"),
                p(style = "color: var(--text-secondary); line-height: 1.55; margin-bottom: 0.85rem;",
                  "Get a ranked, exportable to-do list for mobilising data on threatened and invasive species."),
                actionButton("goto_priorities", tagList(icon("bullseye"), " See Priorities"), class = "btn-primary")))
            ),

            p(style = "font-size:1rem; color: var(--text-muted); margin: 1.25rem 0 0; line-height: 1.6;",
              "Built from GBIF occurrence records, ",
              tags$strong(if (nchar(country_name) > 0) country_name else "the country"),
              "'s national Red List, the GRIIS register of invasive species, and a 10 km reference grid. ",
              "The ", tags$strong("Data & Sources"), " tab lists every dataset with its DOI and citation. ",
              "Data last updated: ",
              if (!is.null(metadata$created_at)) format(metadata$created_at, "%Y-%m-%d %H:%M") else "unknown",
              "."),

            # Methods, limitations & glossary — native <details> (keyboard-accessible, no JS)
            tags$details(class = "methods-details",
              tags$summary("Methods, limitations & glossary"),
              tags$h3("How these figures are produced"),
              tags$p(
                "Every tab draws on the same GBIF occurrence data for ",
                tags$strong(if (nchar(country_name) > 0) country_name else "the country"),
                ", summarised as an ",
                gloss("occurrence cube", "A GBIF export that pre-aggregates records into counts per species × 10 km cell × month × basis of record, instead of one row per record."),
                " on a 10 km reference grid and checked against the national ",
                gloss("taxonomy backbone", "The national species checklist used as the reference list of taxa — every backbone species is checked for GBIF records."),
                ". Coverage, gaps, and threatened / invasive / sensitive counts are all measured against that one reference, so the numbers stay consistent across tabs."),
              tags$h3("Limitations"),
              tags$ul(
                tags$li("A ", tags$em("gap"), " means missing ", tags$strong("GBIF"),
                        " records — not that a species is absent, unstudied, or unmonitored. Recent or non-digitised data may exist outside GBIF."),
                tags$li("Coverage reflects what has been published to GBIF; apparent under-recording and taxonomic bias are properties of the data, not necessarily of nature."),
                tags$li(gloss("Sensitive species", "Species whose coordinates GBIF generalises to protect them from collection or disturbance."),
                        " have generalised coordinates, so their maps are approximate."),
                tags$li("Threatened counts use the CR / EN / VU / NT Red List categories; Data Deficient (DD) is reported separately and is not counted as threatened.")
              ),
              tags$h3(id = "glossary", "Glossary"),
              tags$dl(class = "glossary",
                tags$dt("Occurrence cube"),
                tags$dd("A GBIF export that pre-aggregates records into counts per species × 10 km cell × month × basis of record, instead of one row per record."),
                tags$dt("Taxonomy backbone"),
                tags$dd("The national species checklist used as the reference against which GBIF coverage is measured."),
                tags$dt("Basis of record"),
                tags$dd("How a record was made — e.g. human observation, preserved specimen, material (DNA) sample, or machine observation."),
                tags$dt("Establishment means"),
                tags$dd("Whether a taxon is native, introduced, naturalised, or invasive in the country."),
                tags$dt("Scope-filtered summary"),
                tags$dd("A summary restricted to a subset of taxa — e.g. only threatened, invasive, or sensitive species — rather than all of GBIF."),
                tags$dt("Single-publisher cell"),
                tags$dd("A 10 km grid cell whose records all come from one data publisher — an infrastructure vulnerability and a partnership opportunity."),
                tags$dt("Troudet sampling bias"),
                tags$dd("A comparison of each group's share of known species against its share of GBIF records; negative bias means the group is under-recorded relative to its richness."),
                tags$dt("Stale cell"),
                tags$dd("A grid cell with no GBIF-mediated records within the recency window (e.g. the last 5 or 10 years), measured from the data snapshot date."),
                tags$dt("Observed vs published"),
                tags$dd("Records are counted by when the organism was recorded (its event / observation date), not by when the dataset was published to GBIF — so the temporal charts show observation dates.")
              )
            )
          ),

          # Top-level stats
          div(class = "stat-grid",
            div(class = "stat-box",
              div(class = "stat-value sage", textOutput("ov_total_occ", inline = TRUE)),
              div(class = "stat-label", if (nzchar(country_adjective)) paste0("Total ", country_adjective, " Occurrences") else "Total Occurrences")),
            div(class = "stat-box",
              div(class = "stat-value slate", textOutput("ov_species", inline = TRUE)),
              div(class = "stat-label", "Species in GBIF")),
            div(class = "stat-box",
              div(class = "stat-value sand", textOutput("ov_year_range", inline = TRUE)),
              div(class = "stat-label", "Year Range")),
            div(class = "stat-box",
              div(class = "stat-value plum", textOutput("ov_cells_total", inline = TRUE)),
              div(class = "stat-label", "Grid Cells (10 km)"))
          ),

          # Four gap summary panels with visual indicators
          fluidRow(
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("map"), "Spatial Gaps"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$sage, ";"),
                    textOutput("ov_spatial_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_spatial_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_spatial_detail", inline = TRUE)),
                div(style = "margin-top:0.6rem;",
                  actionLink("ov_go_spatial", tagList("Open the Spatial tab ", icon("arrow-right")),
                    style = "font-weight:600;"))
              )
            ),
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("clock"), "Temporal Gaps"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$slate, ";"),
                    textOutput("ov_temporal_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_temporal_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_temporal_detail", inline = TRUE)),
                div(style = "margin-top:0.6rem;",
                  actionLink("ov_go_temporal", tagList("Open the Temporal tab ", icon("arrow-right")),
                    style = "font-weight:600;"))
              )
            )
          ),
          fluidRow(
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("leaf"), "Taxonomic Gaps"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$sand, ";"),
                    textOutput("ov_tax_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_tax_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_tax_detail", inline = TRUE)),
                div(style = "margin-top:0.6rem;",
                  actionLink("ov_go_taxonomic", tagList("Open the Taxonomic tab ", icon("arrow-right")),
                    style = "font-weight:600;"))
              )
            ),
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("exclamation-triangle"), "Threatened Species"),
                div(style = "display:flex; align-items:center; gap:1rem; margin-bottom:0.75rem;",
                  div(class = "gap-metric", style = paste0("color:", pal$coral, ";"),
                    textOutput("ov_threat_pct", inline = TRUE)),
                  div(style = "flex:1;", uiOutput("ov_threat_bar"))
                ),
                div(class = "gap-detail",
                  textOutput("ov_threat_detail", inline = TRUE)),
                div(style = "margin-top:0.6rem;",
                  actionLink("ov_go_concern", tagList("Open the Species of Concern tab ", icon("arrow-right")),
                    style = "font-weight:600;"))
              )
            )
          ),

          # Species of Concern: Currently in GBIF + Unmonitored
          fluidRow(
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("shield-alt"), "Species of Concern Currently in GBIF"),
                div(class = "info-note", style = "margin: -0.25rem 0 0.6rem; font-size:1rem;",
                  "Species ", tags$strong("in GBIF"), " (at least one occurrence record) out of the ",
                  "total on the national backbone, by category."),
                div(class = "stat-grid", style = "grid-template-columns: repeat(3, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value coral ratio", textOutput("ov_concern_threat", inline = TRUE)),
                    div(class = "stat-label", "Threatened"),
                    div(class = "stat-sublabel", "in GBIF / total")),
                  div(class = "stat-box",
                    div(class = "stat-value sand ratio", textOutput("ov_concern_inv", inline = TRUE)),
                    div(class = "stat-label", "Invasive"),
                    div(class = "stat-sublabel", "in GBIF / total")),
                  div(class = "stat-box",
                    div(class = "stat-value plum ratio", textOutput("ov_concern_sens", inline = TRUE)),
                    div(class = "stat-label", "Sensitive"),
                    div(class = "stat-sublabel", "in GBIF / total"))
                )
              )
            ),
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("exclamation-triangle"), "Unmonitored"),
                div(class = "info-note", style = "margin: -0.25rem 0 0.6rem; font-size:1rem;",
                  "Species of concern with ", tags$strong("no GBIF records yet"), " — ",
                  "explore them on the ", tags$strong("Species of Concern"), " tab."),
                div(class = "stat-grid", style = "grid-template-columns: repeat(3, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value coral", textOutput("ov_unmon_cr", inline = TRUE)),
                    div(class = "stat-label", "CR Species")),
                  div(class = "stat-box",
                    div(class = "stat-value coral", textOutput("ov_unmon_en", inline = TRUE)),
                    div(class = "stat-label", "EN Species")),
                  div(class = "stat-box",
                    div(class = "stat-value sand", textOutput("ov_unmon_inv", inline = TRUE)),
                    div(class = "stat-label", "Invasive Species"))
                )
              )
            )
          ),

          # Publishers + Basis of Record
          fluidRow(
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("building"), "Publishers"),
                div(class = "stat-grid", style = "grid-template-columns: repeat(2, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value sage", textOutput("ov_n_publishers", inline = TRUE)),
                    div(class = "stat-label", "Publishers")),
                  div(class = "stat-box",
                    div(class = "stat-value coral", textOutput("ov_single_pub_cells", inline = TRUE)),
                    div(class = "stat-label", "Single-Publisher Cells"))
                ),
                div(class = "gap-detail", style = "margin-top: 0.5rem;",
                  textOutput("ov_publisher_detail", inline = TRUE))
              )
            ),
            column(6,
              div(class = "card",
                tags$h2(class = "card-title", icon("clipboard-list"), "Basis of Record"),
                div(class = "stat-grid", style = "grid-template-columns: repeat(2, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value sage", textOutput("ov_bor_obs_pct", inline = TRUE)),
                    div(class = "stat-label", "Human Observations")),
                  div(class = "stat-box",
                    div(class = "stat-value sand", textOutput("ov_bor_spec_pct", inline = TRUE)),
                    div(class = "stat-label", "Preserved Specimens"))
                ),
                div(class = "gap-detail", style = "margin-top: 0.5rem;",
                  textOutput("ov_bor_detail", inline = TRUE))
              )
            )
          ),

          # Establishment means breakdown (if available)
          if (has_establishment) div(class = "card",
            tags$h2(class = "card-title", icon("seedling"), "Species by Establishment Means"),
            div(class = "info-note",
              "Coverage breakdown by origin. ",
              em("Unclassified"), " species (no establishment means in the backbone) ",
              "inflate gap numbers — see the Species of Concern tab to explore native, introduced, or invasive species."),
            plotlyOutput("overview_establishment", height = "220px")
          ),

          # Last 12 months highlight row
          div(class = "card", style = "margin-bottom: 1rem;",
            tags$h2(class = "card-title", icon("calendar-plus"),
              paste0("The Last 12 Months in Review: ", last_year_label)),
            div(class = "info-note", style = "margin-bottom: 0.75rem;",
              "Observations ", tags$strong("dated"), " in this period \u2014 i.e. with an event date in the last 12 months."),
            div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
              div(class = "stat-box",
                div(class = "stat-value sage", textOutput("ov_ly_occ", inline = TRUE)),
                div(class = "stat-label", paste0("Observations Dated (", last_year_label, ")"))),
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
          )
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
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
                p("This tab turns what the other tabs found into a clear, prioritised to-do list \u2014 ",
                  "where records are missing, and what to do about it. It is built for the people who decide ",
                  "where time and money go: GBIF node staff, collection managers, and data coordinators."),
                p("The ", tags$strong("Recommended Actions"), " below are concrete, countable goals, ",
                  "grouped by the kind of gap each one closes:"),
                tags$ul(
                  tags$li(tags$strong("Spatial:"), " Grid cells with no GBIF records are the highest ",
                    "priority. Stale cells (no GBIF records newer than 5 years) may warrant resurvey \u2014 after ",
                    "checking whether recent data exists outside GBIF."),
                  tags$li(tags$strong("Taxonomic:"), " Missing threatened species (CR/EN with zero GBIF records) are ",
                    "critical for conservation. Under-sampled orders (high species richness, low occurrence count) ",
                    "indicate systematic collection biases."),
                  tags$li(tags$strong("Species of Concern:"), " Sensitive species with degraded coordinates, ",
                    "invasive species lacking monitoring data, and threatened species without any GBIF records ",
                    "all require targeted attention."),
                  tags$li(tags$strong("Temporal:"), " Cells and taxa with only historical records and no recent activity ",
                    "may represent abandoned monitoring programmes or shifted survey effort."),
                  tags$li(tags$strong("Infrastructure:"), " Single-publisher cells and under-diversified geographic regions ",
                    "indicate data resilience risks. Use the Publishers tab to identify taxonomic groups ",
                    "where additional data sources are needed.")
                ),
                p("The ", tags$strong("Next 12 Months"), " section projects realistic targets based on recent performance. ",
                  "Targets are set at 1.5\u00d7 the rate achieved in the last 12 months — ambitious but achievable. ",
                  "These projections help frame discussions with funders, data holders, and institutional partners ",
                  "about what is possible with sustained effort."),
                p("Use the ", tags$strong("Export"), " button to download the priority lists as a spreadsheet for ",
                  "sharing with stakeholders, incorporating into grant proposals, or feeding into institutional work plans.")
              )
            )
          ),

          # Recommended Actions — one card per gap dimension
          div(class = "card",
            tags$h2(class = "card-title", icon("tasks"), "Recommended Actions"),
            div(class = "info-note",
              "Concrete goals derived from the gap analysis. Each action targets a specific dimension ",
              "of data completeness with measurable outcomes."),
            uiOutput("action_goals")),

          # Next 12 Months — based on last 12 months performance
          div(class = "card",
            tags$h2(class = "card-title", icon("chart-line"),
              paste0("Next 12 Months — Based on ", last_year_label, " Performance")),
            div(class = "info-note",
              "What was achieved in the last 12 months, and what could be targeted next. ",
              "Targets are set at 1.5\u00d7 the recent rate to encourage growth."),
            uiOutput("next_12_months")),

          # Export
          div(class = "card",
            div(style = "display:flex; align-items:center; justify-content:space-between;",
              div(
                tags$h2(class = "card-title", icon("download"), "Export Action Plan"),
                div(style = "font-size:1rem; color:#6b6b6b;",
                  "Download all priority items as a single CSV.")),
              div(style = "padding-left:1rem;",
                downloadButton("download_action_plan", "Download CSV",
                  class = "btn-download", style = "white-space:nowrap;"))
            )),

          # Maps: zero coverage + stale cells
          fluidRow(
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("map-marker-alt"), "Zero Coverage Cells"),
              div(class = "info-note", style = "margin-top:0;",
                "Each square is a 10 km grid cell with ", tags$strong("no records at all"),
                " \u2014 never recorded in GBIF (which may mean never surveyed, surveyed but not digitised, or recorded only outside GBIF). Click a cell for its code."),
              leafletOutput("zero_map", height = "400px"),
              div(style = "margin-top:0.75rem;"),
              DTOutput("zero_table"))),
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("hourglass-half"), "Stale Cells"),
              div(class = "info-note", style = "margin-top:0;",
                "Cells whose newest ", tags$strong("GBIF record"), " is over five years old. ",
                "Recent data may exist outside GBIF \u2014 check other sources before treating these as survey gaps. Click a cell for details."),
              leafletOutput("stale_map", height = "400px"),
              div(style = "margin-top:0.75rem;"),
              DTOutput("stale_table")))
          ),

          # Taxonomic mobilization targets
          div(class = "card",
            tags$h2(class = "card-title", icon("seedling"), "Taxonomic Mobilization Targets"),
            div(class = "info-note",
              "Orders and families with the largest gap between known species and GBIF coverage."),
            fluidRow(
              column(6, plotlyOutput("priority_undersampled_orders", height = "380px")),
              column(6, plotlyOutput("priority_undersampled_families", height = "380px"))
            ))
        )
      ),

      # =====================================================================
      # SPATIAL TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("map"), "Spatial"),
        value = "spatial",
        div(style = "padding: 1.25rem 0;",

          read_guide(
            "How many GBIF records and species each 10 km cell holds, and how recent they are.",
            "Red or pale cells are gaps or stale; blue cells are well covered. A gap means missing GBIF records — not necessarily absent biodiversity.",
            "Target empty and red cells for fieldwork or data mobilisation; for stale cells, check national or regional sources before treating them as survey gaps."),

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--sage);",
            actionLink("spatial_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.spatial_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
                p("This tab shows how biodiversity observations are distributed across the country's ",
                  "10 km EEA reference grid cells. Each cell is coloured by the selected metric: total occurrences, ",
                  "data recency (how recently each cell was surveyed), species richness, or observations ",
                  "from the last 12 months."),
                p("Use the ", tags$strong("Kingdom filter"), " to isolate specific taxonomic groups. ",
                  "Bird observations dominate Swedish GBIF data, so filtering to non-Aves groups ",
                  "can reveal sampling gaps that are otherwise hidden. The ", tags$strong("Class filter"),
                  " allows further refinement within a kingdom."),
                p(tags$strong("Data recency"), " shows how stale each cell's most recent observation is. ",
                  "Red cells have no GBIF-mediated records dated within the last 10 years \u2014 recent data may exist outside GBIF, so check national/regional sources before prioritising resurvey. ",
                  "Orange cells (5\u201310 years) are approaching staleness. Green cells have data from the ",
                  "last 5 years."),
                p(tags$strong("Occurrence distribution"), " (histogram) shows how records are spread across cells. ",
                  "A healthy dataset has a smooth distribution; a spike at the low end indicates many cells ",
                  "with only token data (1\u201310 records), which may be insufficient for ecological analysis."),
                p("Grid cells are based on the European Environment Agency (EEA) reference grid at 10 km resolution. ",
                  "The grid is clipped to the country boundary using GADM administrative boundaries.")
              )
            )
          ),

          fluidRow(
            column(8, div(class = "card",
              tags$h2(class = "card-title", icon("globe-europe"), "Geographic Coverage"),
              div(class = "info-note", style = "margin-top:0;",
                "Each square is a 10 km grid cell; darker means more of the selected measure. ",
                "Use the ", tags$strong("Display"), " panel to switch between records, species count, ",
                "recent activity, and how out-of-date a cell is. Click a cell for details."),
              leafletOutput("spatial_map", height = "520px"), map_dl_btn("spatial_map_dl"))),
            column(4,
              div(class = "card",
                tags$h2(class = "card-title", icon("sliders-h"), "Display"),
                # T-D5: coverage-area toggle at the top of the Display panel
                if (has_marine) tagList(
                  div(class = "filter-label", "Coverage area"),
                  radioGroupButtons("coverage_area", label = NULL,
                    choices = c("Land + sea" = "land_sea", "Land only" = "land_only"),
                    selected = "land_sea", size = "sm"),
                  tags$hr(style = "margin: 0.6rem 0; border-color: #eee;")
                ),
                radioButtons("map_var", NULL,
                  choices = setNames(
                    c("occ", "stale", "richness", "last_year_obs"),
                    c("Occurrences", "Data recency", "Species richness",
                      paste0("Observed (", last_year_label, ")"))),
                  selected = "occ"),
                if (has_kingdom_recency) tagList(
                  tags$hr(style = "margin: 0.5rem 0; border-color: #eee;"),
                  div(class = "filter-label", "Taxonomic filter"),
                  uiOutput("spatial_kingdom_filter_ui"),
                  uiOutput("spatial_class_filter_ui")
                )),
              if (has_admin) div(class = "card",
                tags$h2(class = "card-title", icon("border-all"), "Administrative Boundaries"),
                if (!is.null(admin_level1)) checkboxInput("show_admin1", "Show regions", value = TRUE),
                if (!is.null(admin_level2)) checkboxInput("show_admin2", "Show municipalities", value = FALSE)
              ),
              div(class = "card",
                tags$h2(class = "card-title", icon("info-circle"), "Statistics"),
                tableOutput("spatial_stats")))
          ),
          fluidRow(
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("th"), "Grid Comparison"),
              plotlyOutput("spatial_grid", height = "260px"))),
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("chart-area"), "Occurrence Distribution (10km cells)"),
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

          read_guide(
            "When the country's GBIF records were collected — the distribution of occurrences by observation date (not GBIF publication date).",
            "Dips and a falling recent tail show periods with little digitised data; the current year looks low mainly because it is still incomplete.",
            "Prioritise digitising collections from under-covered periods; compare trends using complete years, not the partial current one."),

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--slate);",
            actionLink("temporal_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.temporal_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
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
                sliderInput("year_range", NULL, min = 1900, max = data_max_year,
                  value = c(1970, last_complete_year), step = 1, sep = "")),
              column(2,
                div(class = "filter-label", "Kingdom"),
                selectizeInput("temp_kingdom", NULL,
                  choices = c("All" = "", kingdom_choices), selected = "",
                  options = list(allowEmptyOption = TRUE))),
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
              tags$h2(class = "card-title", icon("chart-line"), "Historical Trend"),
              plotlyOutput("temporal_trend", height = "300px"))),
            column(4, div(class = "card",
              tags$h2(class = "card-title", icon("calendar-alt"), "Seasonal Pattern"),
              plotlyOutput("temporal_season", height = "300px")))
          ),
          fluidRow(
            column(12, div(class = "card",
              div(style = "display:flex; align-items:center; justify-content:space-between;",
                tags$h2(class = "card-title", icon("th"), "Year \u00d7 Month Heatmap"),
                radioButtons("heatmap_scale", NULL,
                  choices = c("Log scale" = "log", "Linear" = "linear", "Binned" = "binned"),
                  selected = "log", inline = TRUE)
              ),
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

          read_guide(
            "How much of the national checklist has any GBIF records — by rank, kingdom, and group.",
            "Low coverage means many backbone species have no GBIF records; negative sampling bias means a group is under-recorded relative to its share of known species.",
            "Focus mobilisation on under-covered orders and families, and on the most negatively biased groups."),

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--sand);",
            actionLink("taxonomic_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.taxonomic_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
                p(tags$strong("What this tab measures: "),
                  "GBIF-mediated occurrence data assessed against the national taxonomy backbone (Dyntaxa). ",
                  "Unlike the Spatial, Temporal, Record Types and Publisher tabs — which show ", tags$strong("all"),
                  " GBIF records for Sweden with no reference filter — every completeness and gap figure here is ",
                  "relative to the national checklist: of the species Dyntaxa lists, how many have GBIF records, ",
                  "and how sampling effort is distributed across groups."),
                p("The ", tags$strong("Taxonomic Bias"), " chart (following Troudet et al., 2017) reveals whether groups are ",
                  "over- or under-represented relative to their known species richness. ",
                  "If a group has 10% of all known species but only 1% of all occurrences, it is under-sampled. ",
                  "The default landing view uses ", tags$strong("GBIF-style groups"), " — curated mixed-rank ",
                  "categories (Birds, Mammals, Insects, Vascular Plants, Fungi, etc.) that match how ",
                  "GBIF's country pages present data. Switch to 'Kingdoms' for the standard taxonomic hierarchy."),
                p("The ", tags$strong("Exclusion filter"), " lets you remove dominant groups (e.g. Aves) from the bias chart ",
                  "to reveal patterns among less-sampled taxa. The ", tags$strong("cascade filters"),
                  " (Kingdom \u2192 Phylum \u2192 Class \u2192 Order \u2192 Family) let you drill into any group, and the ",
                  tags$strong("active filter breadcrumb"), " shows your current drill-down path."),
                p("The ", tags$strong("Last 12 Months"), " toggle highlights recent observed sampling effort, ",
                  "showing whether recent data collection is addressing historical biases or reinforcing them. ",
                  "When toggled on, the species count chart displays the number of occurrences observed in the ",
                  "last 12 months as annotations to the right of each bar. ",
                  "For sub-population views (native, introduced, invasive, threatened species), ",
                  "see the ", tags$strong("Species of Concern"), " tab."),
                p(tags$strong("Species Count by Order"), " and ", tags$strong("Species Coverage by Family"),
                  " show how many species in each group are present in GBIF vs the national backbone. ",
                  "Green bars indicate species found in GBIF; sand-coloured bars show missing species."),
                p(tags$strong("Reference population: "),
                  "completeness percentages use the species-rank taxa in the national checklist (Dyntaxa) ",
                  "as the denominator \u2014 excluding microbial kingdoms (Bacteria, Archaea, Viruses) and ",
                  "Homo sapiens. A species counts as \u201Cin GBIF\u201D when at least one occurrence resolves to it.")
              )
            )
          ),

          # Cascading taxonomy filters
          div(class = "filter-section",
            fluidRow(
              column(2,
                div(class = "filter-label", "Kingdom"),
                selectizeInput("tax_kingdom", NULL,
                  choices = c("All" = "", kingdom_choices),
                  selected = "", options = list(allowEmptyOption = TRUE))),
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
                uiOutput("tax_family_filter_ui"))
            ),
            # Separate row: last 12 months toggle
            tags$hr(style = "margin: 0.75rem 0; border-color: var(--border-light);"),
            div(style = "display: flex; align-items: center; gap: 1.5rem;",
              div(class = "filter-label", style = "margin-bottom: 0; white-space: nowrap;",
                icon("calendar-alt", style = "margin-right: 0.3rem;"), "HIGHLIGHT LAST 12 MONTHS"),
              radioButtons("tax_last_year_mode", NULL,
                choices = setNames(
                  c("off", "observed"),
                  c("Off",
                    paste0("Observed (", last_year_label, ")"))),
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
            tags$a(href = "https://artfakta.se/",
              "Browse Dyntaxa", target = "_blank", style = "color: var(--sage);"),
            ". The scope filter above controls which species are included (present, native, introduced, invasive)."
          ),

          # Active filter breadcrumb
          uiOutput("tax_filter_breadcrumb"),

          # Troudet-style bias figure
          div(class = "card",
            tags$h2(class = "card-title", icon("balance-scale"), "Taxonomic Bias in Occurrence Data"),
            div(class = "info-note",
              "Deviation from proportional sampling: if a group has ", em("p"), "% of all known species, ",
              "it should ideally have ", em("p"), "% of all occurrences. ",
              "Green = over-represented, red = under-represented. ",
              "The landing view shows curated GBIF-style groups (Birds, Mammals, Vascular plants, etc.) or ",
              "plain kingdoms. ",
              "Drill down using the filters above — the chart auto-adjusts to phylum, class, order, or family. ",
              "Use ", tags$em("Exclude groups"), " to hide dominant taxa (e.g. Aves) so smaller groups become visible."),

            # Troudet-specific controls: landing view + exclusion multi-select
            fluidRow(
              column(5,
                div(class = "filter-label", "Landing-view grouping"),
                radioButtons("troudet_landing", NULL,
                  choices = c("GBIF-style groups" = "gbif_groups",
                              "By kingdom" = "kingdom"),
                  selected = "gbif_groups", inline = TRUE)),
              column(7,
                div(class = "filter-label", "Exclude groups from chart"),
                selectizeInput("troudet_exclude", NULL,
                  choices = NULL, multiple = TRUE,
                  options = list(
                    placeholder = "Type a group to exclude",
                    plugins = list("remove_button")
                  ),
                  width = "100%"))
            ),

            plotlyOutput("troudet_bias_chart", height = "720px")),

          # Coverage: species count vs coverage %
          fluidRow(
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("layer-group"), "Species Count by Order"),
              plotlyOutput("tax_order", height = "720px"))),
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("folder-tree"), "Coverage (%) by Family"),
              plotlyOutput("tax_family", height = "720px")))
          ),

          # Recent vs Historical — as info cards
          div(class = "card",
            tags$h2(class = "card-title", icon("exchange-alt"), "Recent vs Historical Sampling Intensity"),
            div(class = "info-note",
              strong("Baseline: "), "Historical = all records before 2000. ",
              "Recent = records from 2000 onwards. ",
              "Filtered by the taxonomy selections above."),
            uiOutput("tax_change_cards"))
        )
      ),

      # =====================================================================
      # SPECIES OF CONCERN TAB (replaces standalone Threatened tab)
      # Threatened + Invasive + Sensitive as sub-tabs with shared filters.
      # =====================================================================
      tabPanel(
        title = tagList(icon("shield-alt"), "Concern"),
        value = "species_of_concern",
        div(style = "padding: 1.25rem 0;",

          read_guide(
            "GBIF coverage of threatened (CR/EN/VU/NT), invasive, and sensitive species.",
            "Species shown as unmonitored have no GBIF records at all — the highest conservation-data priority. Sensitive-species maps are approximate, as GBIF generalises their coordinates.",
            "Mobilise records for unmonitored threatened and invasive species; verify against national monitoring before acting."),

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--coral);",
            actionLink("concern_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.concern_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
                p("This tab focuses on three categories of species that require special attention ",
                  "for conservation monitoring and data mobilisation:"),
                tags$ul(
                  tags$li(tags$strong("Threatened"), " — species on the national Red List, categorised by ",
                    "the IUCN framework as Critically Endangered (CR), Endangered (EN), Vulnerable (VU), ",
                    "Near Threatened (NT), or Data Deficient (DD). A 'missing' threatened species is one ",
                    "that appears on the Red List but has zero matching GBIF occurrence records. ",
                    "These are the highest conservation data priority: without occurrence data, ",
                    "range modelling, population trend analysis, and habitat suitability assessments ",
                    "cannot be performed."),
                  tags$li(tags$strong("Invasive"), " — species listed as invasive in ",
                    "GRIIS, the Global Register of Introduced and Invasive Species for Sweden ",
                    "(only taxa GRIIS explicitly flags as invasive, not all introduced or alien species). ",
                    "Because GBIF occurrence cubes are aggregated at the species level, this flag is ",
                    "applied at the species level too: a species is marked invasive if ",
                    tags$strong("any of its listed forms"), " — including a subspecies or variety — ",
                    "appears as invasive in GRIIS. The count therefore answers ",
                    tags$em("\u201cwhich species have an invasive form that may warrant monitoring\u201d"),
                    ", not whether one specific subspecies is invasive. Monitoring invasive species ",
                    "requires spatially and temporally complete occurrence data to detect range expansion, ",
                    "evaluate management interventions, and trigger early-warning alerts. Species missing ",
                    "from GBIF represent blind spots in the national invasive-species surveillance network."),
                  tags$li(tags$strong("Sensitive"), " — species whose precise location data is restricted ",
                    "in GBIF to protect them from collection pressure, habitat disturbance, or trade. ",
                    "These records may show generalised coordinates (e.g. country centroid or 50 km grid) ",
                    "rather than exact localities. This affects the accuracy of spatial gap analysis for ",
                    "these species, and the true distribution may be much better known than GBIF data suggests.")
                ),
                p("The taxonomy cascade filters at the top apply across all three sub-tabs. ",
                  "Use the ", tags$strong("Scope"), " filter to restrict to native, introduced, ",
                  "or invasive species (where establishment-means data is available). ",
                  "Threat status comes from SLU Artdatabanken's Red List, the invasive flag from ",
                  "GRIIS Sweden, and the sensitive flag from the SLU Restricted Access Species list. ",
                  "The Red List and sensitive flags are matched at the rank each list publishes — a ",
                  "listed subspecies stays a subspecies — whereas the invasive flag is rolled up to ",
                  "species level, as described above. A few non-species entries (hybrids, colour morphs, ",
                  "slash-aggregates) have no species-level occurrence equivalent and are not flagged."),
                p("Prioritisation tip: start with the Threatened sub-tab to identify missing CR/EN species, ",
                  "then check the Invasive sub-tab for unmonitored invasive species in the same taxonomic groups. ",
                  "Species that are both threatened and invasive (e.g. a threatened native species in a genus ",
                  "with invasive congeners) may warrant especially urgent data mobilisation."),
                p(tags$strong("Reference lists in use"),
                  " — resolved to the exact GBIF-published datasets; the title below ",
                  "confirms each edition, and the DOIs travel in the data bundle:"),
                checklist_cite("redlist"),
                checklist_cite("invasives"),
                checklist_cite("sensitive")
              )
            )
          ),

          # Shared taxonomy cascade filters
          div(class = "filter-section",
            fluidRow(
              column(2, selectizeInput("concern_kingdom", "Kingdom",
                choices = c("All" = "", kingdom_choices), selected = "",
                options = list(allowEmptyOption = TRUE))),
              column(2, uiOutput("concern_phylum_ui")),
              column(2, uiOutput("concern_class_ui")),
              column(2, uiOutput("concern_order_ui")),
              column(2,
                if (has_establishment) selectInput("concern_scope", "Scope",
                  choices = scope_choices, selected = "all")
              )
            )),

          # Sub-tabs
          tabsetPanel(
            id = "concern_subtabs", type = "tabs",

            # ---- THREATENED SUB-TAB ----
            tabPanel(
              title = tagList(icon("exclamation-triangle"), "Threatened"),
              value = "threatened",
              div(style = "padding: 1rem 0;",
                div(class = "stat-grid", style = "grid-template-columns: repeat(5, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value coral", textOutput("concern_cr", inline = TRUE)),
                    div(class = "stat-label", "CR Missing")),
                  div(class = "stat-box",
                    div(class = "stat-value sand", textOutput("concern_en", inline = TRUE)),
                    div(class = "stat-label", "EN Missing")),
                  div(class = "stat-box",
                    div(class = "stat-value", style = "color:#EE8866;", textOutput("concern_vu", inline = TRUE)),
                    div(class = "stat-label", "VU Missing")),
                  div(class = "stat-box",
                    div(class = "stat-value sage", textOutput("concern_nt", inline = TRUE)),
                    div(class = "stat-label", "NT Missing")),
                  div(class = "stat-box",
                    div(class = "stat-value slate", textOutput("concern_dd", inline = TRUE)),
                    div(class = "stat-label", "DD Missing"))
                ),
                div(style = "margin: 0.1rem 0 0.9rem; font-size: 0.95rem; color: #555;",
                  icon("shield-alt"), " ",
                  textOutput("concern_threat_coverage_line", inline = TRUE)),
                fluidRow(
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("shield-alt"), "Coverage by Threat Status"),
                    plotlyOutput("concern_threat_coverage", height = "300px"))),
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("times-circle"), "Missing Taxa by Status"),
                    plotlyOutput("concern_threat_missing", height = "300px")))
                ),
                fluidRow(
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("map"), "Where Threatened Species Occur"),
                    div(class = "info-note",
                      "Spatial distribution of occurrence records for threatened species (CR/EN/VU/NT). ",
                      "Cells with no data represent spatial gaps in threatened species monitoring."),
                    leafletOutput("concern_threat_map", height = "400px"), map_dl_btn("concern_threat_map_dl"))),
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("clipboard-list"), "How Threatened Species Are Recorded"),
                    div(class = "info-note",
                      "Basis of record breakdown for threatened species. A reliance on preserved specimens ",
                      "with few recent human observations may indicate monitoring gaps."),
                    plotlyOutput("concern_threat_bor", height = "400px")))
                ),
                div(class = "card",
                  tags$h2(class = "card-title", icon("list"), "Missing Threatened Species"),
                  div(class = "info-note",
                    "Species in the national taxonomy backbone with a Red List status ",
                    "that have no matching GBIF occurrence records. ",
                    "Use the filters above and column filters below to narrow results."),
                  DTOutput("concern_threat_table")),
                checklist_cite("redlist")
              )
            ),

            # ---- INVASIVE SUB-TAB ----
            tabPanel(
              title = tagList(icon("bug"), "Invasive"),
              value = "invasive",
              div(style = "padding: 1rem 0;",
                div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value coral", textOutput("concern_inv_total", inline = TRUE)),
                    div(class = "stat-label", "Known Invasive")),
                  div(class = "stat-box",
                    div(class = "stat-value sage", textOutput("concern_inv_in_gbif", inline = TRUE)),
                    div(class = "stat-label", "In GBIF")),
                  div(class = "stat-box",
                    div(class = "stat-value sand", textOutput("concern_inv_missing", inline = TRUE)),
                    div(class = "stat-label", "Missing from GBIF")),
                  div(class = "stat-box",
                    div(class = "stat-value slate", textOutput("concern_inv_pct", inline = TRUE)),
                    div(class = "stat-label", "Coverage"))
                ),
                div(class = "info-note", style = "margin: 0.25rem 0 1rem;",
                  tags$strong("Known Invasive"), " counts species with at least one form ",
                  "(species, subspecies, or variety) listed as invasive in GRIIS Sweden, ",
                  "rolled up and matched at species level. ",
                  tags$em("Missing from GBIF"), " means the species has zero matching occurrence records."),
                fluidRow(
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("chart-bar"), "Invasive Species by Order"),
                    div(class = "info-note",
                      "Orders with many unmonitored invasive species are high priorities ",
                      "for targeted data mobilisation."),
                    plotlyOutput("concern_inv_by_order", height = "480px"))),
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("chart-bar"), "Invasive Species by Family"),
                    plotlyOutput("concern_inv_by_family", height = "480px")))
                ),
                fluidRow(
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("map"), "Where Invasive Species Occur"),
                    div(class = "info-note",
                      "Spatial distribution of occurrence records for invasive species. ",
                      "Gaps may indicate areas where invasive species are present but unmonitored."),
                    leafletOutput("concern_inv_map", height = "400px"), map_dl_btn("concern_inv_map_dl"))),
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("chart-line"), "Invasive Species Observations Over Time"),
                    div(class = "info-note",
                      "Temporal trend of invasive species occurrences. ",
                      "Rising trends may reflect genuine range expansion or increased monitoring effort."),
                    plotlyOutput("concern_inv_temporal", height = "400px")))
                ),
                div(class = "card",
                  tags$h2(class = "card-title", icon("clipboard-list"), "How Invasive Species Are Recorded"),
                  div(class = "info-note",
                    "Basis of record breakdown for invasive species. Effective invasive species monitoring ",
                    "relies on recent human observations rather than historical preserved specimens."),
                  plotlyOutput("concern_inv_bor", height = "300px")),
                div(class = "card",
                  tags$h2(class = "card-title", icon("table"), "Invasive Species Details"),
                  div(class = "info-note",
                    "Species with at least one form listed as invasive in GRIIS Sweden, matched at species level. ",
                    "Species missing from GBIF cannot be monitored for range expansion."),
                  DTOutput("concern_inv_table")),
                checklist_cite("invasives")
              )
            ),

            # ---- SENSITIVE SUB-TAB ----
            tabPanel(
              title = tagList(icon("eye-slash"), "Sensitive"),
              value = "sensitive",
              div(style = "padding: 1rem 0;",
                div(class = "stat-grid", style = "grid-template-columns: repeat(4, 1fr);",
                  div(class = "stat-box",
                    div(class = "stat-value coral", textOutput("concern_sens_total", inline = TRUE)),
                    div(class = "stat-label", "Known Sensitive")),
                  div(class = "stat-box",
                    div(class = "stat-value sage", textOutput("concern_sens_in_gbif", inline = TRUE)),
                    div(class = "stat-label", "In GBIF")),
                  div(class = "stat-box",
                    div(class = "stat-value sand", textOutput("concern_sens_missing", inline = TRUE)),
                    div(class = "stat-label", "Missing from GBIF")),
                  div(class = "stat-box",
                    div(class = "stat-value slate", textOutput("concern_sens_pct", inline = TRUE)),
                    div(class = "stat-label", "Coverage"))
                ),
                div(class = "card",
                  tags$h2(class = "card-title", icon("info-circle"), "About Sensitive Species"),
                  div(class = "info-note",
                    "Sensitive species have restricted location data in GBIF to protect them from ",
                    "collection pressure, habitat disturbance, or trade. Their occurrence records may ",
                    "show generalised coordinates (e.g. country centroid) rather than precise locations. ",
                    "The generalization category (5 km, 25 km, or 50 km) indicates the radius within which ",
                    "coordinates are randomised. At 10 km grid resolution, 5 km-generalised species retain ",
                    "reasonable spatial accuracy; 25 km and 50 km species should be interpreted with caution.")),
                div(class = "card",
                  tags$h2(class = "card-title", icon("ruler-combined"), "Coordinate Generalization Categories"),
                  div(class = "info-note",
                    "Number of sensitive species by the degree of coordinate degradation applied in GBIF. ",
                    "Species with larger generalization radii have less reliable spatial data."),
                  div(class = "stat-grid", style = "grid-template-columns: repeat(3, 1fr); margin-top: 0.5rem;",
                    div(class = "stat-box",
                      div(class = "stat-value sage", textOutput("concern_sens_gen5", inline = TRUE)),
                      div(class = "stat-label", "5 km")),
                    div(class = "stat-box",
                      div(class = "stat-value sand", textOutput("concern_sens_gen25", inline = TRUE)),
                      div(class = "stat-label", "25 km")),
                    div(class = "stat-box",
                      div(class = "stat-value coral", textOutput("concern_sens_gen50", inline = TRUE)),
                      div(class = "stat-label", "50 km"))
                  )),
                fluidRow(
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("map"), "Where Sensitive Species Occur"),
                    div(class = "info-note",
                      "Spatial distribution of occurrence records for sensitive species. ",
                      "Coordinates are generalised by GBIF, so cell-level accuracy varies ",
                      "by species (see generalization categories above). Interpret with care."),
                    leafletOutput("concern_sens_map", height = "400px"), map_dl_btn("concern_sens_map_dl"))),
                  column(6, div(class = "card",
                    tags$h2(class = "card-title", icon("clipboard-list"), "How Sensitive Species Are Recorded"),
                    div(class = "info-note",
                      "Basis of record breakdown for sensitive species. Understanding how these species ",
                      "are documented helps assess whether active monitoring is occurring despite ",
                      "the coordinate restrictions."),
                    plotlyOutput("concern_sens_bor", height = "400px")))
                ),
                div(class = "card",
                  tags$h2(class = "card-title", icon("table"), "Sensitive Species Details"),
                  div(class = "info-note",
                    "All species flagged as sensitive in the national restricted access list. ",
                    "The generalization column shows how much coordinate degradation is applied. ",
                    "Use the column filters to focus on specific taxonomic groups or generalization levels."),
                  DTOutput("concern_sens_table")),
                checklist_cite("sensitive")
              )
            )
          )
        )
      ),

      # =====================================================================
      # PUBLISHER TAB (NEW)
      # =====================================================================
      tabPanel(
        title = tagList(icon("building"), "Publishers"),
        value = "publishers",
        div(style = "padding: 1.25rem 0;",

          read_guide(
            "Which organisations publish the country's GBIF data, and how concentrated that publishing is.",
            "A few publishers usually dominate the volume; a cell served by a single publisher is an infrastructure vulnerability.",
            "Broaden the contributor base for single-publisher cells, and approach dominant publishers as partnership opportunities."),

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--plum);",
            actionLink("publisher_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.publisher_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
                p("This tab shows which organisations publish biodiversity occurrence data ",
                  "to GBIF for this country. Understanding the publisher landscape helps assess ",
                  "data infrastructure resilience, identify potential data partnerships, and ",
                  "recognise under-represented data holders."),
                p(tags$strong("Taxonomic filter:"), " Use the kingdom/class/order filters to see which ",
                  "publishers contribute data for specific taxonomic groups. This reveals whether bird data ",
                  "comes mainly from citizen science while insect data depends on museum collections, for example. ",
                  "The dependency map also updates to show per-cell publisher coverage for the selected group."),
                p(tags$strong("Publisher category:"), " Publishers are classified by name into three ",
                  "categories: ", tags$em("Citizen science"), " (Artdatabanken/Artportalen, iNaturalist, eBird, etc.), ",
                  tags$em("Private sector"), " (environmental consultancies and companies), and ",
                  tags$em("Research data"), " (universities, museums, herbaria, government agencies, ",
                  "sequencing facilities, field stations and marine institutes). ",
                  "Bars in the charts are colour-coded by category."),
                p(tags$strong("Publisher Dependency per Cell"), " maps each 10 km grid cell by the number ",
                  "of distinct publishers contributing data. ",
                  "Cells with a single publisher are both an infrastructure vulnerability and a partnership opportunity \u2014 ",
                  "if that organisation paused contributing, the cell would lose coverage, so broadening the ",
                  "contributor base safeguards it. This reflects the publishing infrastructure, not the publisher."),
                p("The ", tags$strong("All Publishers"), " table shows every contributing organisation with ",
                  "their occurrence count, species count, category, and percentage share.")
              )
            )
          ),

          # Taxonomy + type filter row
          div(class = "filter-section",
            fluidRow(
              column(2, selectizeInput("pub_kingdom", "Kingdom",
                choices = c("All" = "", pub_kingdom_choices), selected = "",
                options = list(allowEmptyOption = TRUE))),
              column(2, selectizeInput("pub_class", "Class",
                choices = c("All" = ""), selected = "",
                options = list(maxOptions = 500, allowEmptyOption = TRUE))),
              column(2, selectizeInput("pub_order", "Order",
                choices = c("All" = ""), selected = "",
                options = list(maxOptions = 500, allowEmptyOption = TRUE))),
              column(2, selectizeInput("pub_type_filter", "Publisher category",
                choices = c("All categories" = "",
                  "Citizen science" = "Citizen science",
                  "Private sector" = "Private sector",
                  "Research data" = "Research data"),
                selected = "", options = list(allowEmptyOption = TRUE)))
            )),

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
            "Use the taxonomic filters to explore which publishers dominate for specific groups."),

          # Optional log scale for the two volume charts (helps when one publisher
          # dominates the linear axis)
          div(style = "display:flex; justify-content:flex-end; align-items:center; gap:0.6rem; margin-bottom:0.75rem;",
            tags$span(style = "font-weight:600; color:var(--text-secondary);", "Bar axis scale:"),
            radioGroupButtons("pub_scale", label = NULL,
              choices = c("Linear" = "linear", "Log" = "log"),
              selected = "linear", size = "sm")),

          fluidRow(
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("chart-bar"), "Top Publishers by Occurrences"),
              plotlyOutput("pub_top_chart", height = "450px"))),
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("chart-pie"), "Top Publishers by Species Coverage"),
              plotlyOutput("pub_species_chart", height = "450px")))
          ),

          fluidRow(
            column(12, div(class = "card",
              tags$h2(class = "card-title", icon("map"), "Publisher Dependency per Cell"),
              div(class = "info-note", "Cells coloured by the number of publishers contributing data. ",
                "Cells with a single publisher are an infrastructure vulnerability and a partnership opportunity \u2014 broadening the contributor base safeguards their coverage."),
              leafletOutput("pub_dependency_map", height = "450px"), map_dl_btn("pub_dependency_map_dl")))
          ),

          div(class = "card",
            tags$h2(class = "card-title", icon("table"), "All Publishers"),
            DTOutput("pub_table"))
        )
      ),

      # =====================================================================
      # BASIS OF RECORD TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("layer-group"), "Record Types"),
        value = "basis_tab",
        div(style = "padding: 1.25rem 0;",

          read_guide(
            "The mix of record types behind the data — human observations, preserved specimens, DNA / material samples, and machine observations.",
            "Observations dominate the volume, but specimens and DNA support different uses such as verification and sequencing; the pie groups the smallest types as Other.",
            "Where a place has data but not the record type your work needs, target that type for mobilisation."),

          # About section (expandable)
          div(class = "card", style = "margin-bottom: 1rem; border-left: 4px solid var(--sand);",
            actionLink("basis_about_toggle", tagList(
              icon("info-circle"), " About this tab",
              icon("chevron-down", style = "float:right; margin-top:3px;")
            ), style = "font-weight: 500; color: var(--text-primary); text-decoration: none;"),
            conditionalPanel(
              condition = "input.basis_about_toggle % 2 == 1",
              div(style = "margin-top: 0.75rem; font-size: 1rem; line-height: 1.65; color: var(--text-secondary);",
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
                  c("off", "observed"),
                  c("Off",
                    paste0("Observed (", last_year_label, ")"))),
                selected = "off", inline = TRUE)
            )
          ),

          fluidRow(
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("chart-pie"), "Occurrences by Basis of Record"),
              plotlyOutput("basis_pie", height = "380px"))),
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("chart-line"), "Temporal Trend by Basis"),
              selectInput("basis_timeline_select", "Select basis:",
                choices = basis_types_no_all, width = "250px"),
              plotlyOutput("basis_timeline", height = "330px")))
          ),
          fluidRow(
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("map"), "Spatial Coverage by Basis of Record"),
              plotlyOutput("basis_spatial_bar", height = "340px"))),
            column(6, div(class = "card",
              tags$h2(class = "card-title", icon("dna"), "Unique Species by Basis of Record"),
              plotlyOutput("basis_species_bar", height = "340px")))
          ),
          div(class = "card",
            tags$h2(class = "card-title", icon("map"), "Spatial Distribution per Basis of Record"),
            div(class = "info-note", style = "margin-top:0;",
              "Where records of the selected ", tags$strong("record type"),
              " come from \u2014 e.g. human observations vs preserved specimens. ",
              "Each square is a 10 km cell; darker means more records. Click a cell for details."),
            selectInput("basis_map_select", NULL,
              choices = basis_types_no_all, width = "250px"),
            leafletOutput("basis_map", height = "450px"), map_dl_btn("basis_map_dl"))
        )
      ),

      # =====================================================================
      # DATA EXPLORER TAB
      # =====================================================================
      tabPanel(
        title = tagList(icon("database"), "Data & sources"),
        value = "explorer",
        div(style = "padding: 1.25rem 0;",

          # 1 — Citable foundation: the cube downloads
          div(class = "card",
            tags$h2(class = "card-title", icon("database"), "Built on these GBIF downloads"),
            div(class = "info-note", style = "margin-bottom: 0.75rem;",
              "This entire analysis is built on two citable GBIF occurrence cubes. ",
              "If you use these results, cite the downloads below \u2014 it is how the ",
              "data publishers who make this possible receive credit."),
            uiOutput("ds_cubes")),

          # 2 — Credit the contributing datasets
          div(class = "card",
            tags$h2(class = "card-title", icon("sitemap"), "The datasets that make this possible"),
            uiOutput("ds_contrib_headline"),
            div(class = "info-note", style = "margin: 0.5rem 0 0.75rem;",
              "Every gap in this dashboard exists because these datasets were shared ",
              "openly through GBIF. Search and sort the datasets that contributed records ",
              "to the analysis \u2014 each links to its GBIF page and DOI."),
            DTOutput("ds_contrib_table")),

          # 3 — National reference lists
          div(class = "card",
            tags$h2(class = "card-title", icon("list-check"), "National reference lists"),
            div(class = "info-note", style = "margin-bottom: 0.75rem;",
              "Species scope \u2014 taxonomy, threat status, invasive and sensitive lists \u2014 ",
              "is defined by these national authorities, resolved to their current GBIF DOIs."),
            uiOutput("ds_checklists")),

          # 4 — (secondary) Download derived analysis outputs
          div(class = "card",
            tags$h2(class = "card-title", icon("download"), "Download analysis outputs"),
            div(class = "info-note", style = "margin-bottom: 0.75rem;",
              "These are ", tags$em("derived products"), " of the analysis, not source data. ",
              "If you reuse them, please cite the GBIF downloads above. ",
              "Select an output to browse, filter, and export."),
            fluidRow(
              column(6,
                selectInput("explorer_ds", "Select Gap Explorer Dataset:",
                  choices = c(
                    # Spatial
                    "Spatial Gaps (10km)"  = "spatial_gaps_10km",
                    "Cell Recency (10km)"  = "cell_recency_10km",
                    "Cell Last Year"       = "cell_last_year",
                    # Taxonomic
                    "Coverage by Kingdom"  = "tax_by_kingdom",
                    "Coverage by Phylum"   = "tax_by_phylum",
                    "Coverage by Class"    = "tax_by_class",
                    "Coverage by Order"    = "tax_by_order",
                    "Coverage by Family"   = "tax_by_family",
                    "Coverage by Rank"     = "tax_by_rank",
                    "Troudet Bias (Class)" = "troudet_bias",
                    "Troudet Bias (Order)" = "troudet_bias_order",
                    "Troudet Bias (Family)" = "troudet_bias_family",
                    # Species of Concern
                    "Taxonomic Match Summary" = "taxonomic_match_summary",
                    "Species Scope Lookup"    = "species_scope_lookup",
                    "Threatened Species (scope)" = "threatened_basis_recent",
                    "Invasive Species (scope)"   = "invasive_basis_recent",
                    "Sensitive Species (scope)"  = "sensitive_basis_recent",
                    # Publishers
                    "Publisher Summary"        = "publisher_summary",
                    "Publisher Taxonomy"       = "publisher_taxonomy",
                    "Publisher Cell Dependency" = "publisher_cell_dependency",
                    # Priorities
                    "Priority Zero Cells"  = "priority_zero_cells",
                    "Priority Stale Cells" = "priority_stale_cells",
                    "Resolved Last Year"   = "priority_resolved_last_year",
                    # Overview
                    "Order Summary"        = "order_summary",
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
        " \u00b7 gbif_gap_finder"
      ))
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # Persona "start here" buttons -> jump to the relevant tab
  observeEvent(input$goto_taxonomic,  updateTabsetPanel(session, "main_tabs", selected = "taxonomic"))
  observeEvent(input$goto_publishers, updateTabsetPanel(session, "main_tabs", selected = "publishers"))
  observeEvent(input$goto_priorities, updateTabsetPanel(session, "main_tabs", selected = "priorities"))
  # Overview gap-panel jump links
  observeEvent(input$ov_go_spatial,   updateTabsetPanel(session, "main_tabs", selected = "spatial"))
  observeEvent(input$ov_go_temporal,  updateTabsetPanel(session, "main_tabs", selected = "temporal"))
  observeEvent(input$ov_go_taxonomic, updateTabsetPanel(session, "main_tabs", selected = "taxonomic"))
  observeEvent(input$ov_go_concern,   updateTabsetPanel(session, "main_tabs", selected = "species_of_concern"))
  # Key-findings strip jump links (distinct ids from the gap-panel links above)
  observeEvent(input$kf_go_spatial,    updateTabsetPanel(session, "main_tabs", selected = "spatial"))
  observeEvent(input$kf_go_priorities, updateTabsetPanel(session, "main_tabs", selected = "priorities"))
  observeEvent(input$kf_go_concern,    updateTabsetPanel(session, "main_tabs", selected = "species_of_concern"))
  observeEvent(input$kf_go_taxonomic,  updateTabsetPanel(session, "main_tabs", selected = "taxonomic"))
  observeEvent(input$kf_go_publishers, updateTabsetPanel(session, "main_tabs", selected = "publishers"))
  observeEvent(input$kf_go_momentum,   updateTabsetPanel(session, "main_tabs", selected = "priorities"))

  # ---- Reactive filtered data ----
  basis_selected <- reactive({
    b <- input$basis_filter
    if (is.null(b) || b == "") "all" else b
  })

  # ---- T-D5 coverage-area (marine) filter -----------------------------------
  # One reactive layer that, in "Land only" mode, drops EEZ sea cells
  # (marine == TRUE, by eeacellcode) from the spatial coverage + recency views.
  # No-op when the bundle carries no marine cells or the toggle is Land + sea.
  coverage_area <- reactive({
    if (!has_marine || is.null(input$coverage_area)) "land_sea" else input$coverage_area
  })
  land_only <- reactive(identical(coverage_area(), "land_only"))

  drop_marine <- function(df) {
    if (is.null(df) || !has_marine || !land_only()) return(df)
    if (!"eeacellcode" %in% names(df)) return(df)
    df[!(as.character(df$eeacellcode) %in% marine_codes), , drop = FALSE]
  }

  # Filtered reactive views consumed by the governed renders (Overview coverage,
  # Spatial map/stats, Priorities zero/stale).
  grid_10km_r      <- reactive(
    if (land_only() && !is.null(grid_10km))
      grid_10km[!(as.character(grid_10km$eeacellcode) %in% marine_codes), ]
    else grid_10km)
  spatial_gaps_r   <- reactive(drop_marine(spatial_gaps))
  cell_recency_r   <- reactive(drop_marine(cell_recency))
  priority_zero_r  <- reactive(drop_marine(priority_zero))
  priority_stale_r <- reactive(drop_marine(priority_stale))

  # ===================================================================
  # SINGLE SOURCES OF TRUTH
  # Every headline number shown in more than one place is computed ONCE
  # here, from the SAME full-GBIF dataset the dedicated tab reads. Overview
  # and Priorities consume these instead of the precomputed `dashboard`
  # object, so the tabs never disagree. Nothing here is hardcoded.
  # (Occurrence tabs are all full GBIF; only Taxonomy and Species of Concern
  # are measured against the national checklist — labelled on those tabs.)
  # ===================================================================

  # Spatial coverage — mirrors the Spatial tab's `spatial_stats`
  # (spatial_gaps, basis "all"). Overview's spatial panel == Spatial tab.
  truth_spatial <- reactive({
    sg <- spatial_gaps_r()
    req(sg)
    sf <- sg |> filter(basisofrecord == "all")
    total <- nrow(sf)
    with_data <- sum(sf$has_data, na.rm = TRUE)
    list(
      total_cells  = total,
      cells_data   = with_data,
      cells_zero   = total - with_data,
      pct_coverage = if (total > 0) round(100 * mean(sf$has_data, na.rm = TRUE), 1) else 0,
      total_occ    = sum(as.numeric(sf$occurrences), na.rm = TRUE)
    )
  })

  # Taxonomic coverage — from match_summary_full (matched_any), the SAME
  # object the Species of Concern tab and the Overview threat panel use,
  # so taxonomic coverage, concern figures and the Concern tab all agree
  # on one (denominator-corrected) population.
  truth_taxonomic <- reactive({
    req(match_summary_full)
    ms <- as_tibble(match_summary_full)
    n_ref  <- nrow(ms)
    n_gbif <- sum(ms$matched_any, na.rm = TRUE)
    list(
      n_reference  = n_ref,
      n_in_gbif    = n_gbif,
      n_missing    = n_ref - n_gbif,
      pct_coverage = if (n_ref > 0) round(100 * n_gbif / n_ref, 1) else 0
    )
  })

  # Record types — mirrors the Record Types tab's `basis_stat_boxes`
  # (all_basis_recent, the correct per-basis cube totals), with the same
  # spatial_gaps fallback.
  truth_basis <- reactive({
    br <- if (!is.null(all_basis_recent)) all_basis_recent else NULL
    if (is.null(br)) {
      req(spatial_gaps)
      br <- spatial_gaps |> filter(basisofrecord != "all") |>
        group_by(basisofrecord) |>
        summarise(occ_total = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop")
    } else {
      br <- br |> filter(basisofrecord != "all")
    }
    total <- sum(as.numeric(br$occ_total), na.rm = TRUE)
    getp  <- function(b) sum(as.numeric(br$occ_total[br$basisofrecord == b]), na.rm = TRUE)
    list(
      total    = total,
      obs_pct  = if (total > 0) round(100 * getp("HUMAN_OBSERVATION")  / total, 1) else 0,
      spec_pct = if (total > 0) round(100 * getp("PRESERVED_SPECIMEN") / total, 1) else 0,
      n_types  = sum(br$occ_total > 0, na.rm = TRUE)
    )
  })

  # Year span — from the same time_summary the Temporal tab reads.
  truth_years <- reactive({
    ts <- time_summary
    req(ts)
    yrs <- ts$year[!is.na(ts$year)]
    list(min = if (length(yrs)) min(yrs) else NA_real_,
         max = if (length(yrs)) max(yrs) else NA_real_,
         span = if (length(yrs)) (max(yrs) - min(yrs) + 1) else NA_real_)
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
      ph <- order_tax_map |>
        filter(kingdom == input$temp_kingdom) |> pull(phylum) |> unique() |> sort()
      ch <- c("All" = "", setNames(ph, ph))
    }
    selectizeInput("temp_phylum", NULL, choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Temporal taxonomy cascade — Class
  output$temp_class_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(order_tax_map)) {
      df <- order_tax_map
      if (!is.null(input$temp_kingdom) && input$temp_kingdom != "")
        df <- df |> filter(kingdom == input$temp_kingdom)
      if (!is.null(input$temp_phylum) && input$temp_phylum != "")
        df <- df |> filter(phylum == input$temp_phylum)
      cl <- sort(unique(df$class))
      ch <- c("All" = "", setNames(cl, cl))
    }
    selectizeInput("temp_class", NULL, choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Temporal taxonomy cascade — Order
  output$temp_order_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(order_tax_map)) {
      df <- order_tax_map
      if (!is.null(input$temp_kingdom) && input$temp_kingdom != "")
        df <- df |> filter(kingdom == input$temp_kingdom)
      if (!is.null(input$temp_phylum) && input$temp_phylum != "")
        df <- df |> filter(phylum == input$temp_phylum)
      if (!is.null(input$temp_class) && input$temp_class != "")
        df <- df |> filter(class == input$temp_class)
      ord <- sort(unique(df$order))
      if (length(ord) <= 200) ch <- c("All" = "", setNames(ord, ord))
    }
    selectizeInput("temp_order", NULL, choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Temporal taxonomy cascade — Family
  output$temp_family_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(family_tax_map)) {
      df <- family_tax_map
      if (!is.null(input$temp_kingdom) && input$temp_kingdom != "")
        df <- df |> filter(kingdom == input$temp_kingdom)
      if (!is.null(input$temp_phylum) && input$temp_phylum != "")
        df <- df |> filter(phylum == input$temp_phylum)
      if (!is.null(input$temp_class) && input$temp_class != "")
        df <- df |> filter(class == input$temp_class)
      if (!is.null(input$temp_order) && input$temp_order != "")
        df <- df |> filter(order == input$temp_order)
      fam <- sort(unique(df$family))
      if (length(fam) <= 200) ch <- c("All" = "", setNames(fam, fam))
    }
    selectizeInput("temp_family", NULL, choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Helper: get filtered orders based on temporal taxonomy selections
  temp_filtered_orders <- reactive({
    if (is.null(order_tax_map)) return(NULL)
    df <- order_tax_map
    if (!is.null(input$temp_kingdom) && input$temp_kingdom != "")
      df <- df |> filter(kingdom == input$temp_kingdom)
    if (!is.null(input$temp_phylum) && input$temp_phylum != "")
      df <- df |> filter(phylum == input$temp_phylum)
    if (!is.null(input$temp_class) && input$temp_class != "")
      df <- df |> filter(class == input$temp_class)
    if (!is.null(input$temp_order) && input$temp_order != "")
      df <- df |> filter(order == input$temp_order)
    df$order
  })

  # Helper: get filtered families
  temp_filtered_families <- reactive({
    if (is.null(family_tax_map)) return(NULL)
    if (is.null(input$temp_family) || input$temp_family == "") return(NULL)
    input$temp_family
  })

  # Temporal data: full GBIF time summaries (occurrence tab, no reference filter).
  # Within each scope, use family_time_summary if family filter active,
  # order_time_summary if order/class/phylum/kingdom active, else time_summary.
  temp_data <- reactive({
    sel_family <- temp_filtered_families()
    orders <- temp_filtered_orders()
    has_tax_filter <- !is.null(input$temp_kingdom) && input$temp_kingdom != ""

    ts  <- time_summary
    ots <- order_time_summary
    fts <- family_time_summary

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

    # D3: when the partial (latest/snapshot) year is in view, grey it + label it so
    # an incomplete final year does not read as a genuine decline.
    show_partial <- input$year_range[2] >= data_max_year &&
      data_max_year > last_complete_year
    trend_shapes <- list(); trend_anns <- list()
    if (show_partial) {
      trend_shapes <- list(list(type = "rect", xref = "x", yref = "paper",
        x0 = last_complete_year + 0.5, x1 = data_max_year + 0.5, y0 = 0, y1 = 1,
        fillcolor = "#9aa0a6", opacity = 0.18, line = list(width = 0), layer = "below"))
      trend_anns <- list(list(x = data_max_year, y = 1, xref = "x", yref = "paper",
        text = paste0(data_max_year, " partial"), showarrow = FALSE,
        xanchor = "right", yanchor = "bottom",
        font = list(size = 10, color = "#5f6368")))
    }

    plot_ly(yearly, x = ~year, y = ~occ, type = "scatter", mode = "lines",
      fill = "tozeroy",
      fillcolor = paste0(pal$sage, "33"),
      line = list(color = pal$sage, width = 2)) |>
      plotly_layout(
        xaxis = list(title = "Year", range = c(input$year_range[1], input$year_range[2])),
        yaxis = list(title = "Number of occurrences"),
        shapes = trend_shapes, annotations = trend_anns)
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

    month_cols <- rep(pal$sage, 12)  # single hue: months are labelled on the x-axis
    plot_ly(monthly, x = ~month, y = ~occ, type = "bar",
      marker = list(color = month_cols)) |>
      plotly_layout(
        xaxis = list(title = "Month", ticktext = month.abb, tickvals = 1:12),
        yaxis = list(title = "Number of occurrences"))
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
      list(0, "#f6f5f1"), list(0.25, "#cfe0ee"),
      list(0.5, "#88b1d4"), list(0.75, "#3f72a8"),
      list(1, "#1d456b"))

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
        list(0, "#f0efea"), list(0.167, "#dbe6f2"),
        list(0.333, "#aac4dd"), list(0.5, "#7aa6cc"),
        list(0.667, "#4f82ad"), list(0.833, "#2f5f8a"),
        list(1, "#1d456b"))

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
        colorbar = list(title = "Occurrences (log scale)"))
    }

    p |> plotly_layout(
      xaxis = list(title = "Month", ticktext = month.abb, tickvals = 1:12),
      yaxis = list(title = "Year"))
  })

  # ===================================================================
  # OVERVIEW
  # ===================================================================

  output$ov_total_occ <- renderText({
    comma(truth_spatial()$total_occ)
  })
  output$ov_species <- renderText({
    comma(truth_taxonomic()$n_in_gbif)
  })
  output$ov_year_range <- renderText({
    ty <- truth_years()
    if (!is.na(ty$min)) paste0(ty$min, "\u2013", ty$max) else "?"
  })
  output$ov_cells_total <- renderText({
    comma(truth_spatial()$total_cells)
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

  # Spatial gap panel — from truth_spatial (same source as the Spatial tab)
  output$ov_spatial_pct <- renderText({
    paste0(truth_spatial()$pct_coverage, "% coverage")
  })
  output$ov_spatial_detail <- renderText({
    ts <- truth_spatial()
    paste0(comma(ts$cells_data), " of ", comma(ts$total_cells),
           " 10km grid cells have occurrence data. ",
           comma(ts$cells_zero), " cells have zero records.")
  })

  # Temporal gap panel
  output$ov_temporal_pct <- renderText({
    if (!is.null(dashboard)) paste0(dashboard$pct_stale_5y_10km[1], "% stale (>5yr)")
    else "?"
  })
  output$ov_temporal_detail <- renderText({
    ty <- truth_years()
    span_txt <- paste0("Data spans ", ty$span, " years (", ty$min, "\u2013", ty$max, "). ")
    # Cell-staleness figures read the precomputed `dashboard` (computed in 10
    # from the full cell_recency). With the scope toggle removed, that is the
    # same full population the Spatial tab's recency map reads, so they agree.
    if (!is.null(dashboard))
      paste0(span_txt,
             "Median cell staleness: ", dashboard$median_staleness_months_10km[1], " months. ",
             round(dashboard$pct_stale_1y_10km[1], 1), "% of cells not sampled in the past year.")
    else span_txt
  })

  # Taxonomic gap panel — from truth_taxonomic (match_summary_full, same as Concern)
  output$ov_tax_pct <- renderText({
    paste0(truth_taxonomic()$pct_coverage, "% coverage")
  })
  output$ov_tax_detail <- renderText({
    tt <- truth_taxonomic()
    paste0(comma(tt$n_in_gbif), " of ", comma(tt$n_reference),
           " backbone species found in GBIF. ",
           comma(tt$n_missing), " species have no occurrence records.")
  })

  # Threatened panel
  # Derived from the SAME object the Species of Concern tab uses
  # (match_summary_full + matched_any), so the Overview gap panel, the
  # "Species of Concern in GBIF" box, and the Concern tab always agree. (The
  # old tax_by_threat source could drift: a different "in GBIF" definition.)
  # Threatened set = CR/EN/VU/NT, matching the tab.
  ov_threat_stats <- {
    if (!is.null(match_summary_full) && "matched_any" %in% names(match_summary_full)) {
      ms <- as_tibble(match_summary_full)
      tc <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
      if (!is.na(tc)) {
        in_set <- ms[[tc]] %in% c("CR", "EN", "VU", "NT")
        n_ref  <- sum(in_set, na.rm = TRUE)
        n_gbif <- sum(in_set & ms$matched_any, na.rm = TRUE)
        list(available = n_ref > 0, n_ref_total = n_ref, n_in_gbif = n_gbif,
             n_missing = n_ref - n_gbif,
             pct = if (n_ref > 0) round(100 * n_gbif / n_ref, 1) else 0)
      } else list(available = FALSE)
    } else list(available = FALSE)
  }

  output$ov_threat_pct <- renderText({
    if (isTRUE(ov_threat_stats$available))
      paste0(comma(ov_threat_stats$n_missing), " missing")
    else "No threat data"
  })
  output$ov_threat_detail <- renderText({
    if (isTRUE(ov_threat_stats$available))
      paste0(comma(ov_threat_stats$n_ref_total), " threatened species in backbone, ",
             comma(ov_threat_stats$n_in_gbif), " found in GBIF (",
             ov_threat_stats$pct, "% coverage).")
    else "Threat status data not available in the current backbone. Enable a red list in config.yml to see threatened species analysis."
  })

  # Species of Concern info boxes (Overview) — all counts come from
  # match_summary_full, the same object the Species of Concern tab filters, so
  # the Overview ties out to it. "In GBIF" = matched_any (>= 1 occurrence);
  # "unmonitored" = a species of concern with no GBIF records at all.
  ov_concern <- reactive({
    ms <- match_summary_full
    if (is.null(ms)) return(NULL)
    ms  <- as_tibble(ms)
    tc  <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
    thr <- if (!is.na(tc)) ms[[tc]] else rep(NA_character_, nrow(ms))
    matched <- if ("matched_any" %in% names(ms)) ms$matched_any else rep(FALSE, nrow(ms))
    inv <- if ("is_invasive" %in% names(ms)) ms$is_invasive else rep(FALSE, nrow(ms))
    sen <- if ("is_sensitive" %in% names(ms)) ms$is_sensitive else rep(FALSE, nrow(ms))
    is_thr <- thr %in% c("CR", "EN", "VU", "NT")
    list(
      thr_total   = sum(is_thr, na.rm = TRUE),
      thr_in_gbif = sum(is_thr & matched, na.rm = TRUE),
      inv_total   = sum(inv, na.rm = TRUE),
      inv_in_gbif = sum(inv & matched, na.rm = TRUE),
      sen_total   = sum(sen, na.rm = TRUE),
      sen_in_gbif = sum(sen & matched, na.rm = TRUE),
      unmon_cr    = sum(thr == "CR" & !matched, na.rm = TRUE),
      unmon_en    = sum(thr == "EN" & !matched, na.rm = TRUE),
      unmon_inv   = sum(inv & !matched, na.rm = TRUE)
    )
  })
  output$ov_concern_threat <- renderText({
    cc <- ov_concern()
    if (is.null(cc)) "?" else paste0(comma(cc$thr_in_gbif), " / ", comma(cc$thr_total))
  })
  output$ov_concern_inv    <- renderText({
    cc <- ov_concern()
    if (is.null(cc)) "?" else paste0(comma(cc$inv_in_gbif), " / ", comma(cc$inv_total))
  })
  output$ov_concern_sens   <- renderText({
    cc <- ov_concern()
    if (is.null(cc)) "?" else paste0(comma(cc$sen_in_gbif), " / ", comma(cc$sen_total))
  })
  output$ov_unmon_cr  <- renderText({
    cc <- ov_concern()
    if (is.null(cc)) "?" else comma(cc$unmon_cr)
  })
  output$ov_unmon_en  <- renderText({
    cc <- ov_concern()
    if (is.null(cc)) "?" else comma(cc$unmon_en)
  })
  output$ov_unmon_inv <- renderText({
    cc <- ov_concern()
    if (is.null(cc)) "?" else comma(cc$unmon_inv)
  })

  # Publisher info box
  output$ov_n_publishers <- renderText({
    if (!is.null(publisher_summary)) comma(nrow(publisher_summary)) else "?"
  })
  output$ov_single_pub_cells <- renderText({
    if (!is.null(publisher_cell_dep))
      comma(sum(publisher_cell_dep$n_publishers == 1))
    else "?"
  })
  output$ov_publisher_detail <- renderText({
    if (!is.null(publisher_summary) && nrow(publisher_summary) > 0) {
      top_name <- if ("publisher_name" %in% names(publisher_summary)) {
        ps <- publisher_summary |> arrange(desc(total_occurrences))
        ifelse(!is.na(ps$publisher_name[1]), ps$publisher_name[1], "Unknown")
      } else "Unknown"
      top_pct <- round(100 * max(publisher_summary$total_occurrences) /
                       sum(publisher_summary$total_occurrences), 1)
      paste0("Largest publisher: ", truncate_name(top_name, 35), " (", top_pct, "% of records).")
    } else ""
  })

  # Basis of Record info box — from truth_basis (same source as the Record Types tab)
  output$ov_bor_obs_pct <- renderText({
    paste0(truth_basis()$obs_pct, "%")
  })
  output$ov_bor_spec_pct <- renderText({
    paste0(truth_basis()$spec_pct, "%")
  })
  output$ov_bor_detail <- renderText({
    paste0(truth_basis()$n_types, " record types represented across the dataset.")
  })

  # Overview coverage chart — REMOVED (Coverage Overview deleted)
  # Overview temporal span — REMOVED (Temporal Span deleted)

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
    make_progress_bar(truth_spatial()$pct_coverage, pal$sage, "#e8ede9")
  })

  # Temporal progress bar (inverse: % not stale = freshness)
  # Staleness reads the precomputed `dashboard` (full cell_recency, via 10) —
  # the same full population the recency map uses now the scope toggle is gone.
  output$ov_temporal_bar <- renderUI({
    pct <- if (!is.null(dashboard)) 100 - as.numeric(dashboard$pct_stale_5y_10km[1]) else 0
    make_progress_bar(pct, pal$slate, "#e2e8ee")
  })

  # Taxonomic progress bar
  output$ov_tax_bar <- renderUI({
    make_progress_bar(truth_taxonomic()$pct_coverage, pal$sand, "#ede8df")
  })

  # Threatened progress bar
  output$ov_threat_bar <- renderUI({
    if (isTRUE(ov_threat_stats$available)) {
      make_progress_bar(ov_threat_stats$pct, pal$coral, "#ede3e0")
    } else {
      tags$div(
        style = "background:#eee; border-radius:6px; height:10px; width:100%;",
        tags$div(style = "background:#ccc; height:100%; width:0%; border-radius:6px;")
      )
    }
  })

  # ---- Auto-generated "key findings" strip -------------------------------
  # Computes 4-6 headline gaps live from the bundle and lays them out as a lead
  # "most severe" card + a responsive row. Every figure reuses the same object
  # its tab reads (truth_spatial, priority_stale, ov_threat_stats,
  # troudet_bias_order, publisher_cell_dep, overview_last_year), so the strip
  # reconciles with the tabs by construction. Each finding is guarded so a
  # missing bundle object OR a zero-value gap simply drops that card rather than
  # showing a meaningless "0" (e.g. a country with complete spatial coverage).
  output$key_findings <- renderUI({

    adj <- if (nzchar(country_adjective)) paste0(country_adjective, " ") else ""
    cn  <- if (nzchar(country_name)) country_name else "the country"

    # Each finding: sev = severity % used for auto-ranking (NA = never headline).
    findings <- list()

    # 1 - Spatial coverage gap: zero-coverage 10 km cells (== nrow(priority_zero))
    if (!is.null(spatial_gaps)) {
      ts <- tryCatch(truth_spatial(), error = function(e) NULL)
      if (!is.null(ts) && isTRUE(ts$total_cells > 0) && isTRUE(ts$cells_zero > 0)) {
        zpct <- round(100 * ts$cells_zero / ts$total_cells, 1)
        findings$spatial <- list(
          sev = zpct, color = pal$sage,
          num = comma(ts$cells_zero),
          sub = paste0(zpct, "% of ", comma(ts$total_cells), " cells"),
          headline = tagList("10 km map cells have ", tags$strong("zero"), " ", adj,
            "GBIF records — never sampled: ", comma(ts$cells_zero), " of ",
            comma(ts$total_cells), " cells (", zpct, "%) sit blank on the map."),
          line = tagList("10 km cells never sampled — ", tags$strong("zero"), " ", adj, "records."),
          link = "kf_go_spatial", link_text = "See the coverage map")
      }
    }

    # 2 - Staleness: cells with no GBIF records newer than 5 years (== stat_stale)
    if (!is.null(priority_stale_r()) && nrow(priority_stale_r()) > 0) {
      n_stale <- nrow(priority_stale_r())
      tc   <- tryCatch(truth_spatial()$total_cells, error = function(e) NA_real_)
      spct <- if (!is.na(tc) && tc > 0) round(100 * n_stale / tc, 1) else NA_real_
      findings$stale <- list(
        sev = spct, color = pal$slate,
        num = comma(n_stale),
        sub = if (!is.na(spct)) paste0(spct, "% of ", comma(tc), " cells") else NULL,
        headline = tagList(comma(n_stale), " grid cells have ",
          tags$strong("no records newer than 5 years"),
          " — prime candidates for resurvey before recent change goes unrecorded."),
        line = tagList("cells with ", tags$strong("no records newer than 5 years"),
          " — resurvey candidates."),
        link = "kf_go_priorities", link_text = "Open Priorities")
    }

    # 3 - Threatened-species gap: CR/EN/VU/NT with no GBIF records (== ov_threat_stats)
    if (isTRUE(ov_threat_stats$available) && isTRUE(ov_threat_stats$n_missing > 0)) {
      n_missing <- ov_threat_stats$n_missing
      n_ref     <- ov_threat_stats$n_ref_total
      tpct <- if (n_ref > 0) round(100 * n_missing / n_ref, 1) else NA_real_
      findings$threat <- list(
        sev = tpct, color = pal$coral,
        num = comma(n_missing),
        sub = paste0("of ", comma(n_ref), " threatened species"),
        headline = tagList(comma(n_missing), " of ", comma(n_ref), " threatened species (",
          gloss("CR / EN / VU / NT", "IUCN Red List categories: Critically Endangered, Endangered, Vulnerable, Near Threatened."),
          ") have ", tags$strong("no GBIF records at all"), " — conservation blind spots."),
        line = tagList("threatened species (CR/EN/VU/NT) with ",
          tags$strong("no GBIF records"), "."),
        link = "kf_go_concern", link_text = "Open Species of Concern")
    }

    # 4 - Most under-represented group: largest negative Troudet bias by order
    if (!is.null(troudet_bias_order) &&
        all(c("order", "bias", "n_known_species", "n_in_gbif") %in% names(troudet_bias_order))) {
      tb <- troudet_bias_order |>
        dplyr::filter(!is.na(bias), bias < 0) |>
        dplyr::arrange(bias)
      if (nrow(tb) > 0) {
        g <- tb[1, ]
        gap_sp <- max(0, g$n_known_species - g$n_in_gbif)
        findings$group <- list(
          sev = NA_real_, color = pal$sand,
          num = as.character(g$order), num_is_text = TRUE,
          sub = paste0(comma(gap_sp), " of ", comma(g$n_known_species), " species missing"),
          headline = tagList(tags$strong(as.character(g$order)), " is the group most ",
            gloss("under-recorded for its diversity", "Troudet sampling bias: the group's share of GBIF records is far below its share of known species."),
            " — ", comma(gap_sp), " of its ", comma(g$n_known_species), " ", adj,
            "species have no GBIF records."),
          line = tagList("the group most ",
            gloss("under-recorded", "Its share of GBIF records is far below its share of known species (Troudet bias)."),
            " for its diversity."),
          link = "kf_go_taxonomic", link_text = "Open Taxonomic")
      }
    }

    # 5 - Publisher-dependency risk: single-publisher cells (== pub_single_cells)
    if (!is.null(publisher_cell_dep) && !is.null(grid_10km) &&
        "n_publishers" %in% names(publisher_cell_dep)) {
      n_single <- sum(publisher_cell_dep$n_publishers == 1, na.rm = TRUE)
      g_total  <- nrow(grid_10km)
      if (n_single > 0) {
        ppct <- if (g_total > 0) round(100 * n_single / g_total, 1) else NA_real_
        findings$publisher <- list(
          sev = ppct, color = pal$plum,
          num = comma(n_single),
          sub = if (!is.na(ppct)) paste0(ppct, "% of ", comma(g_total), " cells") else NULL,
          headline = tagList(comma(n_single), " grid cells rely on a ",
            tags$strong("single data publisher"),
            " — an infrastructure risk if that one source pauses or withdraws."),
          line = tagList("cells rely on a ", tags$strong("single data publisher"),
            " — an infrastructure risk."),
          link = "kf_go_publishers", link_text = "Open Publishers")
      }
    }

    # 6 - Recent momentum: cells newly covered in the last 12 months (good news)
    if (!is.null(overview_last_year) && !is.null(overview_last_year$cells_newly_covered) &&
        isTRUE(overview_last_year$cells_newly_covered > 0)) {
      n_new <- overview_last_year$cells_newly_covered
      findings$momentum <- list(
        sev = NA_real_, color = pal$sage2,
        num = comma(n_new),
        sub = paste0("in ", last_year_label),
        headline = tagList(comma(n_new), " cells gained their ", tags$strong("first-ever"),
          " GBIF records in ", last_year_label, " — momentum to build on."),
        line = tagList("cells gained their ", tags$strong("first-ever"),
          " records in the last 12 months."),
        link = "kf_go_momentum", link_text = "Open Priorities")
    }

    if (length(findings) == 0) return(NULL)

    # Auto-rank: headline = present finding with the worst severity %; if none has
    # a comparable %, fall back to the first present finding.
    sevs <- vapply(findings,
      function(f) if (is.null(f$sev) || is.na(f$sev)) -Inf else f$sev, numeric(1))
    head_key <- if (any(is.finite(sevs))) names(findings)[which.max(sevs)] else names(findings)[1]

    # Card builders - reuse .card + CSS vars; inline layout only (no new stylesheet).
    kf_headline <- function(f) {
      div(class = "card", style = paste0(
          "margin:0 0 1rem; border-left:6px solid ", f$color, "; background:var(--bg-subtle);"),
        div(style = "font-family:'IBM Plex Mono',monospace; font-size:0.72rem; letter-spacing:0.12em; text-transform:uppercase; color:var(--text-muted); margin-bottom:0.4rem;",
          icon("exclamation-triangle"), " Most severe gap"),
        div(style = "display:flex; align-items:baseline; gap:0.9rem; flex-wrap:wrap;",
          div(style = paste0("font-family:'IBM Plex Mono',monospace; font-weight:700; line-height:1; color:",
              f$color, "; font-size:", if (isTRUE(f$num_is_text)) "2.1rem" else "2.9rem", ";"), f$num),
          if (!is.null(f$sub))
            div(style = "font-size:1rem; color:var(--text-muted);", f$sub)),
        p(style = "margin:0.6rem 0 0.75rem; line-height:1.6; font-size:1.08rem; color:var(--text-secondary);",
          f$headline),
        actionLink(f$link, tagList(f$link_text, " ", icon("arrow-right")), style = "font-weight:600;"))
    }

    kf_card <- function(f) {
      div(class = "card", style = paste0("margin:0; height:100%; border-top:4px solid ", f$color, ";"),
        div(style = paste0("font-family:'IBM Plex Mono',monospace; font-weight:700; line-height:1.05; color:",
            f$color, "; font-size:", if (isTRUE(f$num_is_text)) "1.35rem" else "1.9rem", ";"), f$num),
        if (!is.null(f$sub))
          div(class = "stat-label", style = "margin:0.15rem 0 0.5rem;", f$sub),
        p(style = "margin:0 0 0.65rem; line-height:1.5; font-size:0.95rem; color:var(--text-secondary);",
          f$line),
        actionLink(f$link, tagList(f$link_text, " ", icon("arrow-right")),
          style = "font-weight:600; font-size:0.9rem;"))
    }

    rest_keys <- setdiff(names(findings), head_key)

    div(style = "margin-bottom:1.25rem;",
      div(style = "font-family:'Fraunces',serif; font-size:1.35rem; font-weight:600; color:var(--text-primary); margin:0 0 0.2rem;",
        "What matters most"),
      p(style = "color:var(--text-muted); margin:0 0 0.9rem; font-size:0.95rem; line-height:1.5;",
        "Auto-generated from this data release — the biggest ",
        tags$strong(if (nzchar(country_adjective)) country_adjective else cn),
        " biodiversity-data gaps right now. Each links to the tab with the full detail."),
      kf_headline(findings[[head_key]]),
      if (length(rest_keys) > 0)
        div(style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(230px, 1fr)); gap:1rem;",
          lapply(rest_keys, function(k) kf_card(findings[[k]])))
    )
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
        xaxis = list(title = "Establishment means"),
        yaxis = list(title = "GBIF coverage (%)", range = c(0, 105)))
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

  # Spatial kingdom filter -- all GBIF kingdoms (no reference filter).
  output$spatial_kingdom_filter_ui <- renderUI({
    kingdoms <- if (has_kingdom_recency) {
      sort(unique(kingdom_cell_recency$kingdom))
    } else character(0)

    ch <- c("All kingdoms" = "", setNames(sort(kingdoms), sort(kingdoms)))
    selectizeInput("spatial_kingdom_filter", "Kingdom", choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Spatial class filter — cascading from kingdom selection
  output$spatial_class_filter_ui <- renderUI({
    ch <- c("All classes" = "")
    if (has_tax_cell_recency &&
        !is.null(input$spatial_kingdom_filter) &&
        input$spatial_kingdom_filter != "") {
      classes <- tax_cell_recency |>
        filter(kingdom == input$spatial_kingdom_filter, class != "Unplaced") |>
        pull(class) |> unique() |> sort()

      ch <- c("All classes" = "", setNames(sort(classes), sort(classes)))
    }
    selectizeInput("spatial_class_filter", "Class", choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Reactive map update when map_var, basis, or taxonomy filter changes
  observe({
    req(grid_10km, spatial_gaps, input$map_var)

    # T-D5: in Land only mode, drop EEZ sea cells from the drawn grid + coverage
    # + recency so this map matches the toggle. No-op in Land + sea / no marine.
    grid_10km    <- grid_10km_r()
    spatial_gaps <- spatial_gaps_r()
    cell_recency <- cell_recency_r()

    active_spatial <- spatial_gaps
    active_recency <- cell_recency

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
        # Colour-blind-safe RdYlBu: recent = blue → stale = red; grey = no data
        palette = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c", "#dddddd"),
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
        # Colour-blind-safe RdYlBu: few = red → many = blue; grey = no data
        palette = c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6", "#dddddd"),
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
        map_sf$newly_covered ~ "#E69F00",
        map_sf$last_year > 0 ~ pal$slate,
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
          colors = c("#E69F00", pal$slate, "#e0dfda"),
          labels = c("Newly covered", paste0("Observed in ", last_year_label), "No observations"),
          title = paste0("Observed ", last_year_label))
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
        # Colour-blind-safe RdYlBu: few = red → many = blue; grey = no data
        palette = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6", "#dddddd"),
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
    active_sg <- spatial_gaps_r()
    sf <- active_sg |> filter(basisofrecord == basis_selected())
    tibble(
      Metric = c("Total 10km Cells", "Cells with Data", "Empty Cells", "Coverage",
                  "Total Occurrences", "Median per Cell"),
      Value = c(comma(nrow(sf)),
                comma(sum(sf$has_data, na.rm = TRUE)),
                comma(nrow(sf) - sum(sf$has_data, na.rm = TRUE)),
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
        xaxis = list(title = "Grid resolution"),
        yaxis = list(title = "Number of grid cells"))
  })

  output$spatial_hist <- renderPlotly({
    req(spatial_gaps)
    df <- spatial_gaps_r() |> filter(basisofrecord == basis_selected(), occurrences > 0)
    
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
    "HUMAN_OBSERVATION"     = "#2A7F62",
    "PRESERVED_SPECIMEN"    = "#4477AA",
    "MACHINE_OBSERVATION"   = "#CCBB44",
    "OBSERVATION"           = "#AA3377",
    "MATERIAL_SAMPLE"       = "#EE6677",
    "OCCURRENCE"            = "#7daa90",
    "LITERATURE"            = "#a89060",
    "LIVING_SPECIMEN"       = "#8fa4b8",
    "FOSSIL_SPECIMEN"       = "#b8967a",
    "humanObservation"      = "#2A7F62",
    "preservedSpecimen"     = "#4477AA",
    "machineObservation"    = "#CCBB44",
    "observation"           = "#AA3377",
    "materialSample"        = "#EE6677",
    "occurrence"            = "#7daa90",
    "literature"            = "#a89060",
    "livingSpecimen"        = "#8fa4b8",
    "fossilSpecimen"        = "#b8967a"
  )
  get_basis_color <- function(b) ifelse(b %in% names(basis_colors), basis_colors[b], "#999999")

  # Stat boxes — use basis_recent data (correct per-basis totals from cube)
  output$basis_stat_boxes <- renderUI({
    br <- if (!is.null(all_basis_recent)) all_basis_recent else NULL

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
    br <- if (!is.null(all_basis_recent)) all_basis_recent else NULL

    ly_mode <- if (!is.null(input$basis_last_year_mode)) input$basis_last_year_mode else "off"

    if (!is.null(br) && ly_mode != "off") {
      # Stacked horizontal bar: prior + last 12 months
      df <- br |>
        filter(basisofrecord != "all") |>
        arrange(desc(occ_total)) |>
        mutate(label_text = str_replace_all(basisofrecord, "_", " "))

      recent_vals <- df$occ_last_year
      prior_vals  <- df$occ_prior
      recent_name <- paste0("Last 12 months (", last_year_label, ")")
      prior_name  <- "Prior"

      plot_ly(df, y = ~reorder(label_text, occ_total), x = prior_vals,
        type = "bar", orientation = "h", name = prior_name,
        marker = list(color = "#EEDD88"),
        hovertemplate = paste0("%{y}<br>", prior_name, ": %{x:,.0f}<extra></extra>")) |>
        add_trace(x = recent_vals, name = recent_name,
          marker = list(color = pal$sage),
          hovertemplate = paste0("%{y}<br>", recent_name, ": %{x:,.0f}<extra></extra>")) |>
        plotly_layout(
          barmode = "stack",
          xaxis = list(title = list(text = "Number of occurrences", standoff = 10)),
          yaxis = list(title = ""),
          margin = list(l = 160, b = 110),
          legend = list(orientation = "h", y = -0.32, x = 0.5, xanchor = "center",
                        font = list(size = 11)))
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

      # Roll up basis classes below 1% of the total into "Other (minor types)"
      # so the dominant class (usually human observations) doesn't hide the rest.
      # The minor classes are listed on the "Other" slice's hover.
      total_occ  <- sum(df$occ_total)
      minor_mask <- df$occ_total < 0.01 * total_occ
      if (sum(minor_mask) >= 2) {
        major <- df[!minor_mask, , drop = FALSE]
        minor <- df[minor_mask, , drop = FALSE]
        minor_breakdown <- paste0("<br>Includes: ",
          paste0(minor$label_text, " (", comma(minor$occ_total), ")", collapse = ", "))
        plot_df <- bind_rows(
          major |> select(basisofrecord, occ_total, pct, label_text),
          data.frame(basisofrecord = "OTHER_MINOR",
                     occ_total  = sum(minor$occ_total),
                     pct        = round(100 * sum(minor$occ_total) / total_occ, 1),
                     label_text = "Other (minor types)",
                     stringsAsFactors = FALSE)
        )
        cols <- c(sapply(major$basisofrecord, get_basis_color), "#999999")
        hover_text <- c(
          paste0(major$label_text, "<br>", comma(major$occ_total), " occurrences<br>", major$pct, "%"),
          paste0("Other (minor types)<br>", comma(sum(minor$occ_total)), " occurrences<br>",
                 round(100 * sum(minor$occ_total) / total_occ, 1), "%", minor_breakdown)
        )
      } else {
        plot_df    <- df |> select(basisofrecord, occ_total, pct, label_text)
        cols       <- sapply(plot_df$basisofrecord, get_basis_color)
        hover_text <- paste0(plot_df$label_text, "<br>", comma(plot_df$occ_total),
                             " occurrences<br>", plot_df$pct, "%")
      }

      plot_ly(plot_df, labels = ~label_text, values = ~occ_total, type = "pie",
        sort = FALSE,
        marker = list(colors = cols, line = list(color = "#fff", width = 1.5)),
        textinfo = "label+percent",
        textposition = "inside",
        insidetextorientation = "horizontal",
        textfont = list(size = 12, color = "#fff"),
        hovertext = hover_text,
        hovertemplate = "%{hovertext}<extra></extra>") |>
        plotly_layout(
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center",
                        font = list(size = 11)),
          margin = list(l = 10, r = 10, t = 10, b = 80))
    }
  })

  # Timeline — single basis at a time, uses time_summary (has per-basis rows)
  output$basis_timeline <- renderPlotly({
    req(time_summary, input$basis_timeline_select)
    sel <- input$basis_timeline_select

    ts <- time_summary

    df <- ts |>
      filter(basisofrecord == sel, !is.na(year), year >= 1500, year <= current_year) |>
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
    active_sg <- spatial_gaps
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
      textposition = "outside", textfont = list(size = 11, color = "#333"),
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
    active_sg <- spatial_gaps
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
      textposition = "outside", textfont = list(size = 11, color = "#333"),
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

    active_sg <- spatial_gaps
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
      # Colour-blind-safe RdYlBu: few = red → many = blue; grey = no data
      palette = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6", "#dddddd"),
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
    selectizeInput("tax_phylum", NULL, choices = choices, selected = "", options = list(allowEmptyOption = TRUE))
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
    selectizeInput("tax_class", NULL, choices = choices, selected = "", options = list(allowEmptyOption = TRUE))
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
    selectizeInput("tax_order_filter", NULL, choices = choices, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Reactive: filtered family choices
  output$tax_family_filter_ui <- renderUI({
    choices <- c("All" = "")
    if (!is.null(tax_by_family)) {
      df <- tax_by_family
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "")
        df <- df |> filter(kingdom == input$tax_kingdom)
      if (!is.null(input$tax_phylum) && input$tax_phylum != "")
        df <- df |> filter(phylum == input$tax_phylum)
      if (!is.null(input$tax_class) && input$tax_class != "")
        df <- df |> filter(class == input$tax_class)
      if (!is.null(input$tax_order_filter) && input$tax_order_filter != "")
        df <- df |> filter(order == input$tax_order_filter)
      fam <- sort(unique(df$family[!is.na(df$family) & df$family != ""]))
      if (length(fam) <= 200) {
        choices <- c("All" = "", setNames(fam, fam))
      }
    }
    selectizeInput("tax_family_filter", NULL, choices = choices, selected = "", options = list(allowEmptyOption = TRUE))
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
        "font-size:1rem; color: var(--text-secondary, #666); ",
        "display: flex; align-items: center; gap: 0.5rem;"
      ),
      icon("filter", style = "color: var(--sage, #7a9a7e);"),
      tags$span("Showing:"),
      tags$strong(breadcrumb, style = "color: var(--text-primary, #333);"),
      actionLink("tax_clear_filters", tagList(icon("times-circle"), "Clear"),
        style = "margin-left: auto; font-size:1rem; color: var(--coral, #EE6677); text-decoration: none;")
    )
  })

  # Clear all taxonomic filters
  observeEvent(input$tax_clear_filters, {
    updateSelectInput(session, "tax_kingdom", selected = "")
  })

  # ===================================================================
  # TAXONOMIC — Troudet Bias Figure (#7)
  # ===================================================================

  # Populate troudet_exclude choices based on what's currently shown in the
  # chart (depends on drill-down state and landing-view mode).
  observe({
    has_kingdom <- !is.null(input$tax_kingdom) && input$tax_kingdom != ""
    has_phylum <- !is.null(input$tax_phylum) && input$tax_phylum != ""
    has_class <- !is.null(input$tax_class) && input$tax_class != ""
    has_order <- !is.null(input$tax_order_filter) && input$tax_order_filter != ""

    t_family <- safe_get("troudet_bias_family")
    t_order  <- troudet_bias_order
    t_class  <- troudet_bias

    labels <- character(0)
    if (has_order && !is.null(t_family)) {
      df <- apply_tax_filters(t_family)
      labels <- sort(unique(df$family))
    } else if (has_class && !is.null(t_order)) {
      df <- apply_tax_filters(t_order)
      labels <- sort(unique(df$order))
    } else if (has_phylum && !is.null(t_class)) {
      df <- apply_tax_filters(t_class)
      labels <- sort(unique(df$class))
    } else if (has_kingdom && !is.null(t_class)) {
      df <- apply_tax_filters(t_class)
      labels <- sort(unique(df$phylum))
    } else if (!is.null(t_class)) {
      landing_mode <- if (!is.null(input$troudet_landing)) input$troudet_landing else "gbif_groups"
      if (landing_mode == "gbif_groups") {
        agg <- aggregate_to_gbif_groups(t_class)
        labels <- sort(unique(agg$gbif_group))
      } else {
        labels <- sort(unique(t_class$kingdom))
      }
    }

    labels <- labels[!is.na(labels) & labels != ""]
    updateSelectizeInput(session, "troudet_exclude",
                         choices = labels,
                         selected = intersect(input$troudet_exclude, labels),
                         server = FALSE)
  })

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

    # Data source: always pre-computed from script 11
    t_family <- safe_get("troudet_bias_family")
    t_order  <- troudet_bias_order
    t_class  <- troudet_bias

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
      # No filter → landing view. Use GBIF-style groups or plain kingdoms
      # depending on the radio toggle.
      landing_mode <- if (!is.null(input$troudet_landing)) input$troudet_landing else "gbif_groups"

      if (landing_mode == "gbif_groups") {
        df <- aggregate_to_gbif_groups(t_class)
      } else {
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
      }
    } else {
      return(plotly_empty() |> plotly_layout())
    }

    # Apply exclusion filter (user-specified groups to hide from the chart)
    if (!is.null(input$troudet_exclude) && length(input$troudet_exclude) > 0) {
      df <- df |> filter(!label %in% input$troudet_exclude)
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
      yaxis = list(title = "", tickmode = "linear", dtick = 1,
        automargin = TRUE, tickfont = list(size = 11)),
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
    tbo <- tax_by_order
    req(tbo)

    df <- apply_tax_filters(tbo) |>
      filter(!is.na(order)) |>
      arrange(desc(n_taxa)) |>
      slice_head(n = 40) |>
      mutate(
        miss = n_taxa - n_in_gbif,
        label = paste0(round(pct_coverage, 1), "%"))

    p <- plot_ly(df, y = ~reorder(order, n_taxa), x = ~n_in_gbif, type = "bar",
      name = "In GBIF", marker = list(color = pal$slate),
      text = ~label, textposition = "auto",
      textfont = list(size = 11, color = "#fff"),
      orientation = "h") |>
      add_trace(x = ~miss, name = "Missing from GBIF",
        marker = list(color = pal$sand2),
        text = "", textposition = "none")

    ly_mode2 <- if (!is.null(input$tax_last_year_mode)) input$tax_last_year_mode else "off"
    
    if (ly_mode2 == "observed" && "occ_last_year" %in% names(df)) {
      ly_val <- df$occ_last_year
      ly_lab <- paste0("Observed ", last_year_label)
    } else {
      ly_val <- NULL
    }

    if (!is.null(ly_val)) {
      # Place annotation to the right of the full stacked bar (n_taxa = n_in_gbif + miss),
      # with dark readable text on a light background chip so it stays legible
      # regardless of the bar colors behind it.
      p <- p |> add_annotations(
        x = ~n_taxa,
        xshift = 6,
        y = ~order,
        text = ~ifelse(ly_val > 0,
          paste0("\u25CF ", comma(ly_val), " in last 12 mo"), ""),
        showarrow = FALSE,
        font = list(size = 11, color = pal$coral, family = "Calibri"),
        bgcolor = "rgba(255,255,255,0.85)",
        bordercolor = pal$coral, borderwidth = 0.5, borderpad = 2,
        xanchor = "left", yanchor = "middle")
    }

    p |> plotly_layout(
        barmode = "stack",
        xaxis = list(title = "Number of species", automargin = TRUE),
        yaxis = list(title = "", categoryorder = "total ascending",
          tickmode = "linear", dtick = 1, automargin = TRUE,
          tickfont = list(size = 11)),
        margin = list(r = 160, t = 70),
        legend = list(orientation = "h", y = -0.15),
        dl_title = if (nrow(df) > 20) "Species coverage by order (in GBIF vs missing)" else NULL)
  })

  output$tax_family <- renderPlotly({
    tbf <- tax_by_family
    req(tbf)

    df <- apply_tax_filters(tbf) |>
      filter(!is.na(family)) |>
      arrange(desc(n_taxa)) |>
      slice_head(n = 40) |>
      mutate(label = paste0(round(pct_coverage, 1), "%"))

    plot_ly(df, y = ~reorder(family, pct_coverage), x = ~pct_coverage, type = "bar",
      marker = list(color = pal$slate),
      text = ~label, textposition = "auto",
      textfont = list(size = 11, color = "#fff"),
      orientation = "h") |>
      plotly_layout(
        xaxis = list(title = "Coverage (%)", range = c(0, 105)),
        yaxis = list(title = "", categoryorder = "total ascending",
          tickmode = "linear", dtick = 1, automargin = TRUE,
          tickfont = list(size = 11)),
        dl_title = if (nrow(df) > 20) "Taxonomic coverage by family (%)" else NULL)
  })

  # Recent vs Historical — info cards showing multiplier
  output$tax_change_cards <- renderUI({
    if (is.null(order_5yr)) return(div("No temporal order data available."))

    # Get filtered orders
    filtered_orders <- NULL
    if (!is.null(order_tax_map)) {
      filt <- order_tax_map
      if (!is.null(input$tax_kingdom) && input$tax_kingdom != "")
        filt <- filt |> filter(kingdom == input$tax_kingdom)
      if (!is.null(input$tax_phylum) && input$tax_phylum != "")
        filt <- filt |> filter(phylum == input$tax_phylum)
      if (!is.null(input$tax_class) && input$tax_class != "")
        filt <- filt |> filter(class == input$tax_class)
      if (!is.null(input$tax_order_filter) && input$tax_order_filter != "")
        filt <- filt |> filter(order == input$tax_order_filter)
      filtered_orders <- filt$order
    }

    df <- order_5yr
    if (!is.null(filtered_orders)) df <- df |> filter(order %in% filtered_orders)

    df <- df |>
      mutate(era = ifelse(period_start >= 2000, "Recent", "Historical")) |>
      group_by(order, era) |>
      summarise(occ = sum(as.numeric(occurrences), na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = era, values_from = occ, values_fill = 0)

    # A filter selection — or data sitting entirely on one side of the year-2000
    # cutoff — can leave only one era column after the pivot. Guarantee both so
    # the comparison below never references a missing column.
    if (!"Historical" %in% names(df)) df$Historical <- 0
    if (!"Recent" %in% names(df)) df$Recent <- 0

    df <- df |>
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
        div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;", row$order),
        div(style = "font-size:0.92rem; color:var(--text-muted);",
          comma(row$Historical), " \u2192 ", comma(row$Recent))
      )
    })

    div(style = "display:grid; grid-template-columns:repeat(4, 1fr); gap:0.75rem; margin-top:0.75rem;",
      tagList(cards))
  })

  # ===================================================================
  # SPECIES OF CONCERN — Shared Taxonomy Cascade
  # ===================================================================

  output$concern_phylum_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(tax_by_order) && !is.null(input$concern_kingdom) && input$concern_kingdom != "") {
      ph <- tax_by_order |>
        filter(kingdom == input$concern_kingdom) |> pull(phylum) |> unique() |> sort()
      ch <- c("All" = "", setNames(ph, ph))
    }
    selectizeInput("concern_phylum", "Phylum", choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  output$concern_class_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$concern_kingdom) && input$concern_kingdom != "")
        df <- df |> filter(kingdom == input$concern_kingdom)
      if (!is.null(input$concern_phylum) && input$concern_phylum != "")
        df <- df |> filter(phylum == input$concern_phylum)
      cl <- sort(unique(df$class))
      ch <- c("All" = "", setNames(cl, cl))
    }
    selectizeInput("concern_class", "Class", choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  output$concern_order_ui <- renderUI({
    ch <- c("All" = "")
    if (!is.null(tax_by_order)) {
      df <- tax_by_order
      if (!is.null(input$concern_kingdom) && input$concern_kingdom != "")
        df <- df |> filter(kingdom == input$concern_kingdom)
      if (!is.null(input$concern_phylum) && input$concern_phylum != "")
        df <- df |> filter(phylum == input$concern_phylum)
      if (!is.null(input$concern_class) && input$concern_class != "")
        df <- df |> filter(class == input$concern_class)
      ord <- sort(unique(df$order))
      if (length(ord) <= 100) ch <- c("All" = "", setNames(ord, ord))
    }
    selectizeInput("concern_order", "Order", choices = ch, selected = "", options = list(allowEmptyOption = TRUE))
  })

  # Shared reactive: filter match_summary by concern-tab filters + scope
  concern_filtered_taxa <- reactive({
    req(match_summary_full)
    # Use the SAME base object as the Overview. match_summary_full is
    # taxonomic_match_summary with the Primates (Homo sapiens) exclusion applied
    # and inherits 09b's species-rank / kingdom scoping, so the Concern tab's
    # totals tie out to the Overview's Species-of-Concern figures instead of
    # silently counting a different population.
    ms <- match_summary_full |> as_tibble()

    # Apply scope filter
    scope <- input$concern_scope
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

    # Apply taxonomy cascade
    if (!is.null(input$concern_kingdom) && input$concern_kingdom != "" && "kingdom" %in% names(ms))
      ms <- ms |> filter(kingdom == input$concern_kingdom)
    if (!is.null(input$concern_phylum) && input$concern_phylum != "" && "phylum" %in% names(ms))
      ms <- ms |> filter(phylum == input$concern_phylum)
    if (!is.null(input$concern_class) && input$concern_class != "" && "class" %in% names(ms))
      ms <- ms |> filter(class == input$concern_class)
    if (!is.null(input$concern_order) && input$concern_order != "" && "order" %in% names(ms))
      ms <- ms |> filter(order == input$concern_order)
    ms
  })

  # ===================================================================
  # THREATENED SUB-TAB
  # ===================================================================

  concern_threat_data <- reactive({
    ms <- concern_filtered_taxa()
    if (is.null(ms)) return(NULL)
    threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
    if (is.na(threat_col)) return(NULL)
    ms |> mutate(threatStatus = .data[[threat_col]]) |>
      filter(!is.na(threatStatus), threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))
  })

  output$concern_cr <- renderText({
    ms <- concern_threat_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "CR"], na.rm = TRUE)) else "0"
  })
  output$concern_en <- renderText({
    ms <- concern_threat_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "EN"], na.rm = TRUE)) else "0"
  })
  output$concern_vu <- renderText({
    ms <- concern_threat_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "VU"], na.rm = TRUE)) else "0"
  })
  output$concern_nt <- renderText({
    ms <- concern_threat_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "NT"], na.rm = TRUE)) else "0"
  })
  output$concern_dd <- renderText({
    ms <- concern_threat_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any[ms$threatStatus == "DD"], na.rm = TRUE)) else "0"
  })

  # Aggregate threatened coverage, computed identically to the Overview's
  # ov_threat_stats (CR/EN/VU/NT over the same population). At the default
  # scope this equals the Overview figure exactly; it then tracks the Concern
  # tab's scope/taxonomy filters so the two never silently disagree.
  output$concern_threat_coverage_line <- renderText({
    ms <- concern_threat_data()
    if (is.null(ms)) return("Threat coverage unavailable")
    thr <- ms[ms$threatStatus %in% c("CR", "EN", "VU", "NT"), ]
    n_ref <- nrow(thr)
    if (n_ref == 0) return("No threatened (CR/EN/VU/NT) species in the current selection")
    n_gbif <- sum(thr$matched_any, na.rm = TRUE)
    paste0(round(100 * n_gbif / n_ref, 1), "% of threatened species in GBIF \u2014 ",
           comma(n_gbif), " of ", comma(n_ref), " (CR/EN/VU/NT)")
  })

  output$concern_threat_coverage <- renderPlotly({
    ms <- concern_threat_data()
    if (is.null(ms) || nrow(ms) == 0) return(plotly_empty())

    has_estab <- "establishmentMeans" %in% names(ms)
    scope <- input$concern_scope

    if (has_estab && (is.null(scope) || scope == "all")) {
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

      estab_cols <- c(Native = pal$slate, Introduced = pal$sand, Invasive = pal$coral,
                       Uncertain = pal$plum, Unclassified = "#ccc")

      plot_ly(df, x = ~threatStatus, y = ~n_total, color = ~estab, type = "bar",
        colors = estab_cols,
        text = ~paste0(estab, ": ", n_total, " (", pct, "% in GBIF)"),
        hovertemplate = "%{text}<extra></extra>") |>
        plotly_layout(barmode = "stack",
          xaxis = list(title = "Threat status"), yaxis = list(title = "Number of species"),
          legend = list(orientation = "h", y = -0.15))
    } else {
      df <- ms |>
        group_by(threatStatus) |>
        summarise(n_total = n(), n_gbif = sum(matched_any, na.rm = TRUE), .groups = "drop") |>
        mutate(pct = round(100 * n_gbif / n_total, 1)) |>
        filter(threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))

      threat_cols <- c(CR = pal$coral, EN = "#EE8866", VU = pal$sand, NT = pal$slate, DD = "#999999")

      plot_ly(df, x = ~threatStatus, y = ~pct, type = "bar",
        marker = list(color = threat_cols[df$threatStatus]),
        text = ~paste0(round(pct, 1), "%"), textposition = "auto") |>
        plotly_layout(xaxis = list(title = "Threat status"),
          yaxis = list(title = "Coverage %", range = c(0, 110)))
    }
  })

  output$concern_threat_missing <- renderPlotly({
    ms <- concern_threat_data()
    if (is.null(ms) || nrow(ms) == 0) return(plotly_empty())

    has_estab <- "establishmentMeans" %in% names(ms)
    scope <- input$concern_scope

    if (has_estab && (is.null(scope) || scope == "all")) {
      df <- ms |> filter(!matched_any) |>
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

      estab_cols <- c(Native = pal$slate, Introduced = pal$sand, Invasive = pal$coral,
                       Uncertain = pal$plum, Unclassified = "#ccc")

      plot_ly(df, x = ~threatStatus, y = ~n_missing, color = ~estab, type = "bar",
        colors = estab_cols,
        hovertemplate = "%{x}: %{y} missing (%{fullData.name})<extra></extra>") |>
        plotly_layout(barmode = "stack",
          xaxis = list(title = "Threat status"), yaxis = list(title = "Number of missing species"),
          legend = list(orientation = "h", y = -0.15))
    } else {
      df <- ms |>
        group_by(threatStatus) |>
        summarise(n_missing = sum(!matched_any, na.rm = TRUE), .groups = "drop") |>
        filter(threatStatus %in% c("CR", "EN", "VU", "NT", "DD"))

      threat_cols <- c(CR = pal$coral, EN = "#EE8866", VU = pal$sand, NT = pal$slate, DD = "#999999")

      plot_ly(df, x = ~threatStatus, y = ~n_missing, type = "bar",
        marker = list(color = threat_cols[df$threatStatus]),
        text = ~comma(n_missing), textposition = "auto") |>
        plotly_layout(xaxis = list(title = "Threat status"), yaxis = list(title = "Number of missing species"))
    }
  })

  output$concern_threat_table <- renderDT({
    ms <- concern_threat_data()
    if (!is.null(ms) && nrow(ms) > 0) {
      df <- ms |>
        filter(!matched_any, threatStatus %in% c("CR", "EN", "VU", "NT", "DD")) |>
        select(any_of(c("scientificName", "threatStatus", "establishmentMeans",
                         "kingdom", "phylum", "class", "order", "family"))) |>
        arrange(factor(threatStatus, levels = c("CR", "EN", "VU", "NT", "DD")), order, family)
    } else {
      df <- tibble(Message = "No missing threatened species for current filters.")
    }
    for (cn in c("threatStatus", "establishmentMeans", "kingdom", "phylum", "class", "order", "family")) {
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    }
    if ("scientificName" %in% names(df)) df$Ref <- taxon_ref(df$scientificName)
    datatable(df, extensions = "Buttons", rownames = FALSE, escape = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
        buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4", filter = "top")
  })

  # Threatened species map
  output$concern_threat_map <- renderLeaflet({
    req(grid_10km)
    sg <- threatened_spatial_gaps
    if (is.null(sg)) return(leaflet() |> addProviderTiles(providers$CartoDB.Positron))

    sg_all <- sg |> filter(basisofrecord == "all")
    map_sf <- grid_10km |> left_join(sg_all |> select(eeacellcode, occurrences, n_species), by = "eeacellcode")
    map_sf <- map_sf |>
      mutate(
        occ_cat = case_when(
          is.na(occurrences) | occurrences == 0 ~ "No data",
          occurrences <= 10 ~ "1\u201310",
          occurrences <= 100 ~ "11\u2013100",
          occurrences <= 1000 ~ "101\u20131K",
          TRUE ~ "> 1K"
        ),
        occ_cat = factor(occ_cat, levels = c("1\u201310", "11\u2013100", "101\u20131K", "> 1K", "No data"))
      )

    threat_map_pal <- colorFactor(
      # Colour-blind-safe RdYlBu: few = red → many = blue; grey = no data
      palette = c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6", "#dddddd"),
      domain = levels(map_sf$occ_cat), na.color = "#ddd")

    m <- leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~threat_map_pal(occ_cat), fillOpacity = 0.65,
        weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Threatened occ:</strong> ", comma(occurrences),
                        "<br><strong>Threatened spp:</strong> ", comma(n_species))) |>
      addLegend("bottomright", pal = threat_map_pal, values = ~occ_cat,
        title = "Threatened species")
    if (!is.null(admin_level1)) {
      m <- m |> addPolygons(data = admin_level1, group = "admin1",
        fill = FALSE, weight = 1.2, color = "#888", opacity = 0.5)
    }
    m
  })

  # Threatened species BOR chart
  output$concern_threat_bor <- renderPlotly({
    br <- threatened_basis_recent
    if (is.null(br) || nrow(br) == 0) return(plotly_empty())

    df <- br |>
      filter(basisofrecord != "all") |>
      mutate(bor_label = str_replace_all(basisofrecord, "_", " ") |> str_to_title()) |>
      arrange(desc(occ_total))

    plot_ly(df, y = ~reorder(bor_label, occ_total), x = ~occ_total, type = "bar",
      orientation = "h", marker = list(color = pal$sage),
      text = ~comma(occ_total), textposition = "auto",
      hovertemplate = "%{y}: %{x:,} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Number of occurrences"),
        yaxis = list(title = ""))
  })

  # ===================================================================
  # INVASIVE SUB-TAB
  # ===================================================================

  concern_invasive_data <- reactive({
    ms <- concern_filtered_taxa()
    if (is.null(ms) || !"is_invasive" %in% names(ms)) return(NULL)
    ms |> filter(is_invasive == TRUE)
  })

  output$concern_inv_total <- renderText({
    ms <- concern_invasive_data()
    if (!is.null(ms)) comma(nrow(ms)) else "0"
  })
  output$concern_inv_in_gbif <- renderText({
    ms <- concern_invasive_data()
    if (!is.null(ms)) comma(sum(ms$matched_any, na.rm = TRUE)) else "0"
  })
  output$concern_inv_missing <- renderText({
    ms <- concern_invasive_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any, na.rm = TRUE)) else "0"
  })
  output$concern_inv_pct <- renderText({
    ms <- concern_invasive_data()
    if (!is.null(ms) && nrow(ms) > 0) {
      paste0(round(100 * sum(ms$matched_any) / nrow(ms), 1), "%")
    } else "0%"
  })

  output$concern_inv_by_order <- renderPlotly({
    ms <- concern_invasive_data()
    if (is.null(ms) || nrow(ms) == 0) return(plotly_empty())

    df <- ms |>
      filter(!is.na(order), order != "") |>
      group_by(order) |>
      summarise(n_total = n(), n_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = sum(!matched_any, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(n_total)) |> slice_head(n = 20)

    plot_ly(df, y = ~reorder(order, n_total), x = ~n_gbif, type = "bar",
      name = "In GBIF", marker = list(color = pal$slate), orientation = "h") |>
      add_trace(x = ~n_missing, name = "Missing", marker = list(color = pal$coral)) |>
      plotly_layout(barmode = "stack",
        xaxis = list(title = "Number of species"),
        yaxis = list(title = "", tickmode = "linear", dtick = 1,
          automargin = TRUE, tickfont = list(size = 11)),
        legend = list(orientation = "h", y = -0.15))
  })

  output$concern_inv_by_family <- renderPlotly({
    ms <- concern_invasive_data()
    if (is.null(ms) || nrow(ms) == 0) return(plotly_empty())

    df <- ms |>
      filter(!is.na(family), family != "") |>
      group_by(family) |>
      summarise(n_total = n(), n_gbif = sum(matched_any, na.rm = TRUE),
        n_missing = sum(!matched_any, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(n_total)) |> slice_head(n = 20)

    plot_ly(df, y = ~reorder(family, n_total), x = ~n_gbif, type = "bar",
      name = "In GBIF", marker = list(color = pal$slate), orientation = "h") |>
      add_trace(x = ~n_missing, name = "Missing", marker = list(color = pal$sand)) |>
      plotly_layout(barmode = "stack",
        xaxis = list(title = "Number of species"),
        yaxis = list(title = "", tickmode = "linear", dtick = 1,
          automargin = TRUE, tickfont = list(size = 11)),
        legend = list(orientation = "h", y = -0.15))
  })

  output$concern_inv_table <- renderDT({
    ms <- concern_invasive_data()
    if (!is.null(ms) && nrow(ms) > 0) {
      df <- ms |>
        select(any_of(c("scientificName", "matched_any", "gbif_total_occ",
                         "kingdom", "phylum", "class", "order", "family",
                         "establishmentMeans"))) |>
        mutate(status = ifelse(matched_any, "In GBIF", "Missing")) |>
        arrange(matched_any, order, family)
    } else {
      df <- tibble(Message = "No invasive species data for current filters.")
    }
    for (cn in c("kingdom", "phylum", "class", "order", "family", "status")) {
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    }
    if ("scientificName" %in% names(df)) df$Ref <- taxon_ref(df$scientificName)
    datatable(df, extensions = "Buttons", rownames = FALSE, escape = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
        buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4", filter = "top")
  })

  # Invasive species map
  output$concern_inv_map <- renderLeaflet({
    req(grid_10km)
    sg <- invasive_spatial_gaps
    if (is.null(sg)) return(leaflet() |> addProviderTiles(providers$CartoDB.Positron))

    sg_all <- sg |> filter(basisofrecord == "all")
    map_sf <- grid_10km |> left_join(sg_all |> select(eeacellcode, occurrences, n_species), by = "eeacellcode")
    map_sf <- map_sf |>
      mutate(
        occ_cat = case_when(
          is.na(occurrences) | occurrences == 0 ~ "No data",
          occurrences <= 10 ~ "1\u201310",
          occurrences <= 100 ~ "11\u2013100",
          occurrences <= 1000 ~ "101\u20131K",
          TRUE ~ "> 1K"
        ),
        occ_cat = factor(occ_cat, levels = c("1\u201310", "11\u2013100", "101\u20131K", "> 1K", "No data"))
      )

    inv_map_pal <- colorFactor(
      # Colour-blind-safe RdYlBu: few = red → many = blue; grey = no data
      palette = c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6", "#dddddd"),
      domain = levels(map_sf$occ_cat), na.color = "#ddd")

    m <- leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~inv_map_pal(occ_cat), fillOpacity = 0.65,
        weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Invasive occ:</strong> ", comma(occurrences),
                        "<br><strong>Invasive spp:</strong> ", comma(n_species))) |>
      addLegend("bottomright", pal = inv_map_pal, values = ~occ_cat,
        title = "Invasive species")
    if (!is.null(admin_level1)) {
      m <- m |> addPolygons(data = admin_level1, group = "admin1",
        fill = FALSE, weight = 1.2, color = "#888", opacity = 0.5)
    }
    m
  })

  # Invasive species temporal trend
  output$concern_inv_temporal <- renderPlotly({
    ts <- invasive_time_summary
    if (is.null(ts) || nrow(ts) == 0) return(plotly_empty())

    df <- ts |>
      filter(basisofrecord == "all") |>
      mutate(
        yearmonth = as.integer(gsub("-", "", as.character(yearmonth))),
        year = as.integer(substr(as.character(yearmonth), 1, 4))
      ) |>
      group_by(year) |>
      summarise(occurrences = sum(as.numeric(occurrences), na.rm = TRUE),
                n_species = max(n_species, na.rm = TRUE), .groups = "drop") |>
      filter(!is.na(year), year >= 1900)

    plot_ly(df, x = ~year, y = ~occurrences, type = "bar",
      marker = list(color = pal$coral),
      hovertemplate = "Year %{x}: %{y:,} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of occurrences"))
  })

  # Invasive species BOR chart
  output$concern_inv_bor <- renderPlotly({
    br <- invasive_basis_recent
    if (is.null(br) || nrow(br) == 0) return(plotly_empty())

    df <- br |>
      filter(basisofrecord != "all") |>
      mutate(bor_label = str_replace_all(basisofrecord, "_", " ") |> str_to_title()) |>
      arrange(desc(occ_total))

    plot_ly(df, y = ~reorder(bor_label, occ_total), x = ~occ_total, type = "bar",
      orientation = "h", marker = list(color = pal$coral),
      text = ~comma(occ_total), textposition = "auto",
      hovertemplate = "%{y}: %{x:,} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Number of occurrences"),
        yaxis = list(title = ""))
  })

  # ===================================================================
  # SENSITIVE SUB-TAB
  # ===================================================================

  concern_sensitive_data <- reactive({
    ms <- concern_filtered_taxa()
    if (is.null(ms) || !"is_sensitive" %in% names(ms)) return(NULL)
    ms |> filter(is_sensitive == TRUE)
  })

  output$concern_sens_total <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms)) comma(nrow(ms)) else "0"
  })
  output$concern_sens_in_gbif <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms)) comma(sum(ms$matched_any, na.rm = TRUE)) else "0"
  })
  output$concern_sens_missing <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms)) comma(sum(!ms$matched_any, na.rm = TRUE)) else "0"
  })
  output$concern_sens_pct <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms) && nrow(ms) > 0) {
      paste0(round(100 * sum(ms$matched_any) / nrow(ms), 1), "%")
    } else "0%"
  })

  # Generalization category stat boxes
  output$concern_sens_gen5 <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms) && "sensitivity_category" %in% names(ms)) {
      n <- sum(grepl("5", ms$sensitivity_category) &
               !grepl("25|50", ms$sensitivity_category), na.rm = TRUE)
      comma(n)
    } else "?"
  })
  output$concern_sens_gen25 <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms) && "sensitivity_category" %in% names(ms)) {
      comma(sum(grepl("25", ms$sensitivity_category), na.rm = TRUE))
    } else "?"
  })
  output$concern_sens_gen50 <- renderText({
    ms <- concern_sensitive_data()
    if (!is.null(ms) && "sensitivity_category" %in% names(ms)) {
      comma(sum(grepl("50", ms$sensitivity_category), na.rm = TRUE))
    } else "?"
  })

  # Sensitive species map
  output$concern_sens_map <- renderLeaflet({
    req(grid_10km)
    sg <- sensitive_spatial_gaps
    if (is.null(sg)) return(leaflet() |> addProviderTiles(providers$CartoDB.Positron))

    sg_all <- sg |> filter(basisofrecord == "all")
    map_sf <- grid_10km |> left_join(sg_all |> select(eeacellcode, occurrences, n_species), by = "eeacellcode")
    map_sf <- map_sf |>
      mutate(
        occ_cat = case_when(
          is.na(occurrences) | occurrences == 0 ~ "No data",
          occurrences <= 10 ~ "1\u201310",
          occurrences <= 100 ~ "11\u2013100",
          occurrences <= 1000 ~ "101\u20131K",
          TRUE ~ "> 1K"
        ),
        occ_cat = factor(occ_cat, levels = c("1\u201310", "11\u2013100", "101\u20131K", "> 1K", "No data"))
      )

    sens_map_pal <- colorFactor(
      palette = c("#c6c8db", pal$slate, pal$plum, "#3d4f6a", "#ddd"),
      domain = levels(map_sf$occ_cat), na.color = "#ddd")

    m <- leaflet(map_sf) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~sens_map_pal(occ_cat), fillOpacity = 0.65,
        weight = 0.3, color = "#bbb",
        popup = ~paste0("<strong>Cell:</strong> ", eeacellcode,
                        "<br><strong>Sensitive occ:</strong> ", comma(occurrences),
                        "<br><strong>Sensitive spp:</strong> ", comma(n_species),
                        "<br><em>Coordinates may be generalised</em>")) |>
      addLegend("bottomright", pal = sens_map_pal, values = ~occ_cat,
        title = "Sensitive species") |>
      addControl(position = "topright", html = paste0(
        "<div style=\"background:rgba(255,255,255,0.9); padding:4px 8px; border-radius:4px; ",
        "font-size:11px; max-width:230px; color:#444;\"><strong>Note:</strong> sensitive-species ",
        "coordinates are generalised (5–50 km); cell locations are approximate.</div>"))
    if (!is.null(admin_level1)) {
      m <- m |> addPolygons(data = admin_level1, group = "admin1",
        fill = FALSE, weight = 1.2, color = "#888", opacity = 0.5)
    }
    m
  })

  # Sensitive species BOR chart
  output$concern_sens_bor <- renderPlotly({
    br <- sensitive_basis_recent
    if (is.null(br) || nrow(br) == 0) return(plotly_empty())

    df <- br |>
      filter(basisofrecord != "all") |>
      mutate(bor_label = str_replace_all(basisofrecord, "_", " ") |> str_to_title()) |>
      arrange(desc(occ_total))

    plot_ly(df, y = ~reorder(bor_label, occ_total), x = ~occ_total, type = "bar",
      orientation = "h", marker = list(color = pal$plum),
      text = ~comma(occ_total), textposition = "auto",
      hovertemplate = "%{y}: %{x:,} occurrences<extra></extra>") |>
      plotly_layout(
        xaxis = list(title = "Number of occurrences"),
        yaxis = list(title = ""))
  })

  output$concern_sens_table <- renderDT({
    ms <- concern_sensitive_data()
    if (!is.null(ms) && nrow(ms) > 0) {
      df <- ms |>
        select(any_of(c("scientificName", "sensitivity_category", "matched_any", "gbif_total_occ",
                         "threatStatus", "kingdom", "phylum", "class", "order", "family"))) |>
        mutate(status = ifelse(matched_any, "In GBIF", "Missing")) |>
        rename(any_of(c(generalization = "sensitivity_category"))) |>
        arrange(matched_any, order, family)
    } else {
      df <- tibble(Message = "No sensitive species data for current filters.")
    }
    for (cn in c("generalization", "threatStatus", "kingdom", "phylum", "class", "order", "family", "status")) {
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    }
    if ("scientificName" %in% names(df)) df$Ref <- taxon_ref(df$scientificName)
    datatable(df, extensions = "Buttons", rownames = FALSE, escape = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
        buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4", filter = "top")
  })

  # ===================================================================
  # PUBLISHERS
  # ===================================================================

  publisher_summary <- safe_get("publisher_summary")
  publisher_cell_dep <- safe_get("publisher_cell_dependency")

  # Drop the "GBIF Sweden" placeholder publisher — a holding record for
  # orphaned/legacy datasets, not a real data publisher (ROADMAP D10). Capture
  # its org key(s) first so taxonomy-filtered views (which re-aggregate
  # publisher_taxonomy by orgkey) exclude it as well.
  gbif_sweden_keys <- character(0)
  if (!is.null(publisher_summary) && "publisher_name" %in% names(publisher_summary)) {
    is_gbif_se <- !is.na(publisher_summary$publisher_name) &
      grepl("^gbif[ _-]?sweden$", trimws(tolower(publisher_summary$publisher_name)))
    if ("publishingorgkey" %in% names(publisher_summary))
      gbif_sweden_keys <- unique(publisher_summary$publishingorgkey[is_gbif_se])
    publisher_summary <- publisher_summary[!is_gbif_se, , drop = FALSE]
  }

  # Cascading taxonomy filters — server-side selectize to avoid large-option warnings
  observe({
    choices <- c("All" = "")
    if (!is.null(publisher_taxonomy) && "class" %in% names(publisher_taxonomy)) {
      df <- publisher_taxonomy
      if (!is.null(input$pub_kingdom) && input$pub_kingdom != "")
        df <- df |> filter(kingdom == input$pub_kingdom)
      cls <- sort(unique(df$class[!is.na(df$class) & df$class != "" & df$class != "Unplaced"]))
      choices <- c("All" = "", setNames(cls, cls))
    }
    updateSelectizeInput(session, "pub_class", choices = choices, selected = "", server = TRUE)
  })

  observe({
    choices <- c("All" = "")
    if (!is.null(publisher_taxonomy) && "order" %in% names(publisher_taxonomy)) {
      df <- publisher_taxonomy
      if (!is.null(input$pub_kingdom) && input$pub_kingdom != "")
        df <- df |> filter(kingdom == input$pub_kingdom)
      if (!is.null(input$pub_class) && input$pub_class != "")
        df <- df |> filter(class == input$pub_class)
      ords <- sort(unique(df$order[!is.na(df$order) & df$order != "" & df$order != "Unplaced"]))
      choices <- c("All" = "", setNames(ords, ords))
    }
    updateSelectizeInput(session, "pub_order", choices = choices, selected = "", server = TRUE)
  })

  # Reactive: is a taxonomy filter active?
  pub_tax_active <- reactive({
    (!is.null(input$pub_kingdom) && input$pub_kingdom != "") ||
    (!is.null(input$pub_class) && input$pub_class != "") ||
    (!is.null(input$pub_order) && input$pub_order != "")
  })

  # Reactive: filter publisher_taxonomy by selected taxonomy
  pub_tax_filtered <- reactive({
    if (!pub_tax_active() || is.null(publisher_taxonomy)) return(NULL)
    pt <- publisher_taxonomy
    if (!is.null(input$pub_kingdom) && input$pub_kingdom != "")
      pt <- pt |> filter(kingdom == input$pub_kingdom)
    if (!is.null(input$pub_class) && input$pub_class != "")
      pt <- pt |> filter(class == input$pub_class)
    if (!is.null(input$pub_order) && input$pub_order != "")
      pt <- pt |> filter(order == input$pub_order)
    pt
  })

  # Reactive: filtered publisher summary (taxonomy + type)
  pub_filtered <- reactive({
    if (pub_tax_active() && !is.null(pub_tax_filtered())) {
      pub_agg <- pub_tax_filtered() |>
        filter(!publishingorgkey %in% gbif_sweden_keys) |>
        group_by(publishingorgkey) |>
        summarise(
          total_occurrences = sum(total_occurrences, na.rm = TRUE),
          n_species = sum(n_species, na.rm = TRUE),
          n_cells = sum(n_cells, na.rm = TRUE),
          .groups = "drop"
        )
      # Join metadata from publisher_summary
      if (!is.null(publisher_summary)) {
        meta_cols <- intersect(
          c("publishingorgkey", "publisher_name",
            "dominant_bor", "dominant_bor_pct", "n_datasets", "min_year", "max_year"),
          names(publisher_summary))
        pub_agg <- pub_agg |>
          left_join(publisher_summary |> select(any_of(meta_cols)), by = "publishingorgkey")
      }
      pub_agg
    } else {
      publisher_summary
    }
  })

  # Apply publisher type filter + classify by name
  pub_filtered_typed <- reactive({
    df <- pub_filtered()
    if (is.null(df)) return(NULL)
    # Classify by publisher name
    if ("publisher_name" %in% names(df)) {
      df <- df |> mutate(publisher_category = classify_publisher(
        ifelse(!is.na(publisher_name), publisher_name, "")))
    }
    # Apply type filter
    type_sel <- input$pub_type_filter
    if (!is.null(type_sel) && type_sel != "" && "publisher_category" %in% names(df)) {
      df <- df |> filter(publisher_category == type_sel)
    }
    df
  })

  output$pub_n_publishers <- renderText({
    df <- pub_filtered_typed()
    if (!is.null(df)) comma(nrow(df)) else "?"
  })
  output$pub_n_datasets <- renderText({
    df <- pub_filtered_typed()
    if (!is.null(df) && "n_datasets" %in% names(df))
      comma(sum(df$n_datasets, na.rm = TRUE))
    else "?"
  })
  output$pub_single_cells <- renderText({
    if (!is.null(publisher_cell_dep) && !is.null(grid_10km)) {
      n <- sum(publisher_cell_dep$n_publishers == 1)
      total <- nrow(grid_10km)
      paste0(comma(n), " / ", comma(total))
    } else "?"
  })
  output$pub_top_pct <- renderText({
    df <- pub_filtered_typed()
    if (!is.null(df) && nrow(df) > 0) {
      top_occ <- max(df$total_occurrences, na.rm = TRUE)
      total_occ <- sum(df$total_occurrences, na.rm = TRUE)
      paste0(round(100 * top_occ / total_occ, 1), "%")
    } else "?"
  })

  # Category color palette for publisher charts
  pub_cat_colors <- c(
    "Citizen science" = "#2A7F62",
    "Private sector"  = "#CCBB44",
    "Research data"   = "#4477AA"
  )

  output$pub_top_chart <- renderPlotly({
    df <- pub_filtered_typed()
    req(df, nrow(df) > 0)
    x_type <- if (isTRUE(input$pub_scale == "log")) "log" else "linear"
    df <- df |>
      arrange(desc(total_occurrences)) |>
      head(20) |>
      mutate(
        rank = rev(row_number()),
        full_name = if ("publisher_name" %in% names(df))
          ifelse(!is.na(publisher_name), publisher_name, publishingorgkey)
          else publishingorgkey,
        short_name = truncate_name(full_name, 45)
      )

    if ("publisher_category" %in% names(df)) {
      plot_ly(df, y = ~rank, x = ~total_occurrences,
        color = ~publisher_category, colors = pub_cat_colors,
        type = "bar", orientation = "h",
        text = ~short_name, textposition = "inside", insidetextanchor = "start",
        textfont = list(size = 13, color = "#ffffff"),
        hovertext = ~paste0("<b>", full_name, "</b><br>",
                       comma(total_occurrences), " occurrences<br>",
                       publisher_category),
        hovertemplate = "%{hovertext}<extra></extra>",
        hoverlabel = list(font = list(size = 14))) |>
        plotly_layout(
          xaxis = list(title = "Total number of occurrences", type = x_type),
          yaxis = list(title = "", tickmode = "array",
            tickvals = df$rank, ticktext = seq_len(nrow(df))),
          legend = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center"),
          margin = list(l = 40, b = 80))
    } else {
      plot_ly(df, y = ~rank, x = ~total_occurrences,
        type = "bar", orientation = "h",
        marker = list(color = pal$sage),
        text = ~short_name, textposition = "inside", insidetextanchor = "start",
        textfont = list(size = 13, color = "#ffffff"),
        hovertext = ~paste0("<b>", full_name, "</b><br>",
                       comma(total_occurrences), " occurrences"),
        hovertemplate = "%{hovertext}<extra></extra>",
        hoverlabel = list(font = list(size = 14))) |>
        plotly_layout(
          xaxis = list(title = "Total number of occurrences", type = x_type),
          yaxis = list(title = "", tickmode = "array",
            tickvals = df$rank, ticktext = seq_len(nrow(df))),
          margin = list(l = 40, b = 80))
    }
  })

  output$pub_species_chart <- renderPlotly({
    df <- pub_filtered_typed()
    req(df, nrow(df) > 0)
    x_type <- if (isTRUE(input$pub_scale == "log")) "log" else "linear"
    df <- df |>
      arrange(desc(n_species)) |>
      head(20) |>
      mutate(
        rank = rev(row_number()),
        full_name = if ("publisher_name" %in% names(df))
          ifelse(!is.na(publisher_name), publisher_name, publishingorgkey)
          else publishingorgkey,
        short_name = truncate_name(full_name, 45)
      )

    if ("publisher_category" %in% names(df)) {
      plot_ly(df, y = ~rank, x = ~n_species,
        color = ~publisher_category, colors = pub_cat_colors,
        type = "bar", orientation = "h",
        text = ~short_name, textposition = "inside", insidetextanchor = "start",
        textfont = list(size = 13, color = "#ffffff"),
        hovertext = ~paste0("<b>", full_name, "</b><br>",
                       comma(n_species), " species<br>",
                       publisher_category),
        hovertemplate = "%{hovertext}<extra></extra>",
        hoverlabel = list(font = list(size = 14))) |>
        plotly_layout(
          xaxis = list(title = "Number of unique species", type = x_type),
          yaxis = list(title = "", tickmode = "array",
            tickvals = df$rank, ticktext = seq_len(nrow(df))),
          legend = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center"),
          margin = list(l = 40, b = 80))
    } else {
      plot_ly(df, y = ~rank, x = ~n_species,
        type = "bar", orientation = "h",
        marker = list(color = pal$slate),
        text = ~short_name, textposition = "inside", insidetextanchor = "start",
        textfont = list(size = 13, color = "#ffffff"),
        hovertext = ~paste0("<b>", full_name, "</b><br>",
                       comma(n_species), " species"),
        hovertemplate = "%{hovertext}<extra></extra>",
        hoverlabel = list(font = list(size = 14))) |>
        plotly_layout(
          xaxis = list(title = "Number of unique species", type = x_type),
          yaxis = list(title = "", tickmode = "array",
            tickvals = df$rank, ticktext = seq_len(nrow(df))),
          margin = list(l = 40, b = 80))
    }
  })

  # Dependency map — reactive to taxonomy filter
  output$pub_dependency_map <- renderLeaflet({
    req(grid_10km)

    # If taxonomy filter is active AND we have per-cell taxonomy data, recompute
    if (pub_tax_active() && !is.null(publisher_cell_taxonomy)) {
      pct <- publisher_cell_taxonomy
      if (!is.null(input$pub_kingdom) && input$pub_kingdom != "")
        pct <- pct |> filter(kingdom == input$pub_kingdom)
      if (!is.null(input$pub_class) && input$pub_class != "")
        pct <- pct |> filter(class == input$pub_class)
      if (!is.null(input$pub_order) && input$pub_order != "")
        pct <- pct |> filter(order == input$pub_order)

      cell_dep <- pct |>
        group_by(eeacellcode) |>
        summarise(n_publishers = n_distinct(publishingorgkey), .groups = "drop")
    } else {
      req(publisher_cell_dep)
      cell_dep <- publisher_cell_dep |> select(eeacellcode, n_publishers)
    }

    map_sf <- grid_10km |>
      left_join(cell_dep, by = "eeacellcode") |>
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
      palette = c("#e8e8e8", "#D55E00", "#E69F00", "#56B4E9", "#0072B2"),
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

  output$pub_table <- renderDT({
    df <- pub_filtered_typed()
    req(df)
    df <- df |>
      arrange(desc(total_occurrences)) |>
      mutate(
        pct = round(100 * total_occurrences / sum(total_occurrences, na.rm = TRUE), 2),
        name = if ("publisher_name" %in% names(df))
          ifelse(!is.na(publisher_name), publisher_name, publishingorgkey)
          else publishingorgkey
      )

    select_cols <- "name"
    col_names <- "Publisher"
    if ("publisher_category" %in% names(df)) {
      select_cols <- c(select_cols, "publisher_category")
      col_names <- c(col_names, "Category")
    }
    select_cols <- c(select_cols, "total_occurrences", "pct", "n_species", "n_cells")
    col_names <- c(col_names, "Occurrences", "Share %", "Species", "Cells")
    if ("n_datasets" %in% names(df)) {
      select_cols <- c(select_cols, "n_datasets")
      col_names <- c(col_names, "Datasets")
    }
    if (all(c("min_year", "max_year") %in% names(df))) {
      select_cols <- c(select_cols, "min_year", "max_year")
      col_names <- c(col_names, "From", "To")
    }

    df <- df |> select(any_of(select_cols))
    for (cn in c("publisher_category")) {
      if (cn %in% names(df)) df[[cn]] <- as.factor(df[[cn]])
    }

    datatable(df,
      colnames = col_names, extensions = "Buttons",
      options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                     buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4", filter = "top") |>
      formatRound("pct", 2) |>
      formatRound("total_occurrences", digits = 0, mark = ",")
  }, server = FALSE)

  # ===================================================================
  # PRIORITIES
  # ===================================================================

  output$stat_zero <- renderText({
    if (!is.null(priority_zero_r()) && nrow(priority_zero_r()) > 0) {
      comma(nrow(priority_zero_r()))
    } else if (!is.null(dashboard)) {
      comma(dashboard$n_zero_coverage_cells[1])
    } else "0"
  })
  output$stat_stale <- renderText({
    if (!is.null(priority_stale_r())) comma(nrow(priority_stale_r())) else "0"
  })
  output$stat_taxa <- renderText({
    comma(truth_taxonomic()$n_missing)
  })
  output$stat_resolved <- renderText({
    if (!is.null(priority_resolved)) comma(nrow(priority_resolved)) else "0"
  })

  # ---- Recommended Actions (goal-oriented) ----
  output$action_goals <- renderUI({
    n_zero  <- if (!is.null(priority_zero_r()) && nrow(priority_zero_r()) > 0) nrow(priority_zero_r()) else {
      if (!is.null(dashboard)) dashboard$n_zero_coverage_cells[1] else 0
    }
    n_stale <- if (!is.null(priority_stale_r())) nrow(priority_stale_r()) else 0
    n_taxa_missing <- truth_taxonomic()$n_missing

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
      native_cov <- if (n_native_total > 0) {
        round(100 * sum(native_stats$matched_any) / n_native_total, 1)
      } else 0
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
          div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$coral, ";"), comma(n_zero)),
            " grid cells have never been surveyed. Target these for new field campaigns or citizen science events.")),
      ),
      # Temporal
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$sand, ";"), icon("clock")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Temporal: Resurvey stale cells"),
          div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$sand, ";"), comma(n_stale)),
            " cells have no GBIF records newer than 5 years. After checking for data outside GBIF, prioritise cells with historically high diversity for resurvey.")),
      ),
      # Taxonomic
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$plum, ";"), icon("leaf")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Taxonomic: Close species coverage gaps"),
          div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$plum, ";"), comma(n_taxa_missing)),
            " species in the national backbone have no GBIF records. Focus on under-sampled orders and families shown below.")),
      ),
      # Threatened
      div(style = goal_style,
        div(style = paste0(icon_style, " color:", pal$coral, ";"), icon("exclamation-triangle")),
        div(style = "flex:1;",
          div(style = "font-size:1.05rem; font-weight:500;", "Threatened: Monitor CR and EN species"),
          div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
            span(style = paste0(num_style, " color:", pal$coral, ";"), comma(n_cr_en)),
            " critically endangered or endangered species lack any GBIF occurrence data. These are the highest priority for targeted surveys.")),
      )
    )

    # Native/invasive goal (only if data available)
    if (has_establishment && !is.null(match_summary_full)) {
      goals <- tagList(goals,
        div(style = goal_style,
          div(style = paste0(icon_style, " color:", pal$sage, ";"), icon("seedling")),
          div(style = "flex:1;",
            div(style = "font-size:1.05rem; font-weight:500;", "Native species: Improve baseline coverage"),
            div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
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

    # Sensitive species goal
    n_sensitive <- 0
    n_sensitive_in_gbif <- 0
    if (!is.null(match_summary_full) && "is_sensitive" %in% names(match_summary_full)) {
      ms_sens <- match_summary_full |> as_tibble() |> filter(is_sensitive == TRUE)
      n_sensitive <- nrow(ms_sens)
      n_sensitive_in_gbif <- sum(ms_sens$matched_any, na.rm = TRUE)
    }
    if (n_sensitive > 0) {
      goals <- tagList(goals,
        div(style = goal_style,
          div(style = paste0(icon_style, " color:", pal$plum, ";"), icon("eye-slash")),
          div(style = "flex:1;",
            div(style = "font-size:1.05rem; font-weight:500;", "Sensitive species: Assess data availability"),
            div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
              span(style = paste0(num_style, " color:", pal$plum, ";"), comma(n_sensitive)),
              " species have restricted coordinates in GBIF (generalised to 5\u201350 km). ",
              comma(n_sensitive_in_gbif), " have occurrence records. ",
              "Spatial gap analysis is less reliable for these species \u2014 see the Species of Concern tab for details."
            ))
        )
      )
    }

    # Publisher infrastructure goal
    n_single_pub <- 0
    n_total_cells <- 0
    if (!is.null(publisher_cell_dep) && !is.null(grid_10km)) {
      n_single_pub <- sum(publisher_cell_dep$n_publishers == 1)
      n_total_cells <- nrow(grid_10km)
    }
    if (n_single_pub > 0) {
      goals <- tagList(goals,
        div(style = paste0(goal_style, " border-bottom:none;"),
          div(style = paste0(icon_style, " color:", pal$slate, ";"), icon("building")),
          div(style = "flex:1;",
            div(style = "font-size:1.05rem; font-weight:500;", "Infrastructure: Diversify data sources"),
            div(style = "font-size:1rem; color:var(--text-secondary); margin-top:0.25rem;",
              span(style = paste0(num_style, " color:", pal$slate, ";"), comma(n_single_pub)),
              " of ", comma(n_total_cells), " grid cells depend on a single publisher. ",
              "Engage additional data holders (museums, universities, citizen science platforms) to improve resilience. ",
              "See the Publishers tab to identify which taxonomic groups are under-served."
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

    # Target-setting parameters — planning goals, NOT measured data.
    # Lifted to named, tunable constants so there are no buried magic numbers.
    target_mult            <- 1.5   # aspirational multiplier on last-12-months achievement
    min_resolved_floor     <- 5L    # floor so the resolved target is never trivially small
    cr_en_target_cap       <- 20L   # per-year cap on the CR/EN survey target
    single_pub_target_frac <- 0.1   # share of single-publisher cells to diversify
    single_pub_target_cap  <- 50L   # cap on the single-publisher diversification target

    target_new_cells <- ceiling(ly_new_cells * target_mult)
    target_resolved  <- ceiling(max(ly_resolved, min_resolved_floor) * target_mult)

    n_zero <- if (!is.null(priority_zero_r())) nrow(priority_zero_r()) else 0
    n_stale <- if (!is.null(priority_stale_r())) nrow(priority_stale_r()) else 0
    n_cr_en <- 0
    if (!is.null(match_summary_full)) {
      ms <- match_summary_full |> as_tibble()
      threat_col <- intersect(c("threatStatus", "threatStatus_redlist", "threatStatus_backbone"), names(ms))[1]
      if (!is.na(threat_col))
        n_cr_en <- ms |> filter(.data[[threat_col]] %in% c("CR", "EN"), !matched_any) |> nrow()
    }

    row_style <- "display:flex; align-items:center; padding:0.6rem 0; border-bottom:1px solid #f0f0f0;"
    label_style <- "flex:2; font-size:1rem;"
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
        div(style = paste0(target_style, " color:", pal$sage, ";"),
            paste0(comma(ceiling(ly_occ * target_mult)), "+"))
      ),
      div(style = row_style,
        div(style = label_style, "Cells with active recording"),
        div(style = achieved_style, comma(ly_cells)),
        div(style = paste0(target_style, " color:", pal$sage, ";"),
            paste0(comma(ceiling(ly_cells * target_mult)), "+"))
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
      div(style = paste0(row_style),
        div(style = label_style, "CR/EN species with new records"),
        div(style = achieved_style, "\u2014"),
        div(style = paste0(target_style, " color:", pal$coral, ";"),
          paste0("Target: ", comma(min(n_cr_en, cr_en_target_cap)), " of ", comma(n_cr_en), " missing"))
      ),
      div(style = paste0(row_style, " border-bottom:none;"),
        div(style = label_style, "Single-publisher cells diversified"),
        div(style = achieved_style, "\u2014"),
        div(style = paste0(target_style, " color:", pal$slate, ";"), {
          n_sp <- if (!is.null(publisher_cell_dep)) sum(publisher_cell_dep$n_publishers == 1) else 0
          if (n_sp > 0) paste0(
            "Target: ", comma(min(ceiling(n_sp * single_pub_target_frac), single_pub_target_cap)),
            " of ", comma(n_sp)) else "\u2014"
        })
      )
    )
  })

  # Zero coverage map
  output$zero_map <- renderLeaflet({
    req(grid_10km)

    if (is.null(priority_zero_r()) || nrow(priority_zero_r()) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = 16, lat = 63, zoom = 5)
      )
    }

    zero_codes <- priority_zero_r()$eeacellcode
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
    req(priority_zero_r())
    datatable(priority_zero_r() |> slice_head(n = 100),
      extensions = "Buttons",
      options = list(pageLength = 6, scrollX = TRUE, dom = "Bfrtip",
                     buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4")
  })

  # Stale cells map — color-coded by staleness
  output$stale_map <- renderLeaflet({
    req(grid_10km)

    if (is.null(priority_stale_r()) || nrow(priority_stale_r()) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = 16, lat = 63, zoom = 5)
      )
    }

    stale_join <- priority_stale_r() |>
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

    pal_stale <- colorBin(
      # Binned (not continuous) so the legend reads as discrete recency bands.
      # Colour-blind-safe RdYlBu: recent = blue → stale = red.
      palette = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
      domain = yrs,
      bins = c(0, 1, 3, 5, 10, Inf),
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
    req(priority_stale_r())
    datatable(
      priority_stale_r() |>
        mutate(years_stale = round(staleness_months / 12, 1)) |>
        select(any_of(c("eeacellcode", "years_stale", "total_occurrences",
                         "last_ym", "priority_level"))) |>
        arrange(desc(years_stale)) |>
        slice_head(n = 100),
      extensions = "Buttons",
      options = list(pageLength = 6, scrollX = TRUE, dom = "Bfrtip",
                     buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4")
  })

  # Export combined action plan
  output$download_action_plan <- downloadHandler(
    filename = function() {
      paste0("action_plan_", Sys.Date(), ".csv")
    },
    content = function(file) {
      parts <- list()

      # Zero coverage cells
      if (!is.null(priority_zero_r()) && nrow(priority_zero_r()) > 0) {
        parts[[1]] <- priority_zero_r() |>
          mutate(priority_type = "zero_coverage") |>
          select(priority_type, any_of(c("eeacellcode", "grid", "priority_reason", "priority_level")))
      }

      # Stale cells
      if (!is.null(priority_stale_r()) && nrow(priority_stale_r()) > 0) {
        parts[[length(parts) + 1]] <- priority_stale_r() |>
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

      # Sensitive species (from match_summary)
      if (!is.null(match_summary_full) && "is_sensitive" %in% names(match_summary_full)) {
        sens <- match_summary_full |> as_tibble() |> filter(is_sensitive == TRUE) |>
          mutate(priority_type = "sensitive_species",
                 status = ifelse(matched_any, "in_gbif", "missing")) |>
          select(priority_type, any_of(c("scientificName", "status", "sensitivity_category",
                   "gbif_total_occ", "threatStatus", "kingdom", "order", "family")))
        combined <- bind_rows(combined, sens)
      }

      # Single-publisher cells
      if (!is.null(publisher_cell_dep)) {
        single_pub <- publisher_cell_dep |>
          filter(n_publishers == 1) |>
          mutate(priority_type = "single_publisher_cell") |>
          select(priority_type, any_of(c("eeacellcode", "n_publishers", "total_occurrences")))
        combined <- bind_rows(combined, single_pub)
      }

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
      textfont = list(size = 11, color = "#fff"),
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
      textfont = list(size = 11, color = "#fff"),
      hovertemplate = "%{y}<br>%{x} species missing from GBIF<extra></extra>") |>
      plotly_layout(
        title = list(text = "Most under-sampled families", font = list(size = 14)),
        xaxis = list(title = "Species missing from GBIF"),
        yaxis = list(title = ""))
  })

  # ===================================================================
  # DATA & SOURCES  (provenance baked in by 01b -> metadata$data_sources)
  # ===================================================================

  ds_meta <- reactive({ metadata$data_sources })

  # 1 — Cube downloads: DOI + record count + citation
  output$ds_cubes <- renderUI({
    ds <- ds_meta(); cubes <- ds$cubes
    if (is.null(cubes) || !length(cubes))
      return(div(class = "info-note", "Source provenance is not available in this bundle."))
    tagList(lapply(cubes, function(cb) {
      div(style = "padding: 0.6rem 0; border-bottom: 1px solid #eee;",
        div(style = "font-weight: 600;",
          cb$label %||% "GBIF cube",
          if (!is.null(cb$records) && !is.na(cb$records))
            tags$span(style = "font-weight: 400; color: var(--text-secondary);",
              sprintf("  \u2014  %s records", format(cb$records, big.mark = ",")))),
        if (!is.null(cb$doi) && !is.na(cb$doi))
          div(tags$a(href = cb$doi, cb$doi, target = "_blank",
            style = "text-decoration: underline;")),
        if (!is.null(cb$citation) && !is.na(cb$citation))
          div(style = "font-size:1rem; color: #555; margin-top: 0.25rem;", cb$citation))
    }))
  })

  # 2 — Contributing datasets: headline + searchable table
  output$ds_contrib_headline <- renderUI({
    ds <- ds_meta()
    n <- ds$n_contributing_datasets %||% 0L; p <- ds$n_publishers %||% 0L
    if (!length(n) || n == 0) return(NULL)
    div(style = "font-size: 1.05rem; font-weight: 600; color: var(--coral, #c0654f);",
      sprintf("%s datasets from %s publishers made this analysis possible",
        format(n, big.mark = ","), format(p, big.mark = ",")))
  })

  output$ds_contrib_table <- renderDT({
    ds <- ds_meta(); cd <- ds$contributing_datasets
    if (is.null(cd) || !nrow(cd))
      return(datatable(tibble(Message = "Contributing dataset list not available in this bundle."),
        options = list(dom = "t"), rownames = FALSE))
    disp <- data.frame(
      Dataset = ifelse(is.na(cd$dataset_key),
        htmltools::htmlEscape(cd$title %||% ""),
        sprintf('<a href="https://www.gbif.org/dataset/%s" target="_blank">%s</a>',
          cd$dataset_key, htmltools::htmlEscape(cd$title %||% cd$dataset_key))),
      Publisher = cd$publisher %||% NA_character_,
      Records   = cd$records,
      DOI       = ifelse(is.na(cd$doi), "",
        sprintf('<a href="%s" target="_blank">%s</a>', cd$doi,
          sub("https://doi.org/", "", cd$doi))),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(disp, escape = FALSE, rownames = FALSE, extensions = "Buttons",
      options = list(pageLength = 10, order = list(list(2, "desc")), scrollX = TRUE,
        dom = "Bfrtip", buttons = list(list(extend = "csv", text = "Download CSV"))),
      style = "bootstrap4", filter = "top") |>
      formatRound("Records", digits = 0)
  }, server = FALSE)

  # 3 — National reference lists with resolved DOIs
  output$ds_checklists <- renderUI({
    ds <- ds_meta(); cl <- ds$checklists
    if (is.null(cl) || !length(cl))
      return(div(class = "info-note", "Reference-list provenance is not available in this bundle."))
    tagList(lapply(cl, function(c1) {
      div(style = "padding: 0.5rem 0; border-bottom: 1px solid #eee;",
        tags$strong(tools::toTitleCase(c1$label %||% c1$name %||% "Reference list")), ": ",
        c1$title %||% c1$name %||% "",
        if (!is.null(c1$doi) && !is.na(c1$doi))
          tagList("  ", tags$a(href = c1$doi, sub("https://doi.org/", "", c1$doi),
            target = "_blank", style = "text-decoration: underline;")))
    }))
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

  # ===================================================================
  # CSV downloads for maps (cell-level underlying data)
  # ===================================================================
  output$spatial_map_dl <- dl_csv(function() {
    if (is.null(spatial_gaps)) return(NULL)
    spatial_gaps |> filter(basisofrecord == basis_selected())
  }, "spatial_coverage_cells")

  output$basis_map_dl <- dl_csv(function() {
    if (is.null(spatial_gaps) || is.null(input$basis_map_select)) return(NULL)
    spatial_gaps |> filter(basisofrecord == input$basis_map_select)
  }, "record_type_cells")

  output$concern_threat_map_dl <- dl_csv(function() {
    if (is.null(threatened_spatial_gaps)) return(NULL)
    threatened_spatial_gaps |> filter(basisofrecord == "all")
  }, "threatened_species_cells")

  output$concern_inv_map_dl <- dl_csv(function() {
    if (is.null(invasive_spatial_gaps)) return(NULL)
    invasive_spatial_gaps |> filter(basisofrecord == "all")
  }, "invasive_species_cells")

  output$concern_sens_map_dl <- dl_csv(function() {
    if (is.null(sensitive_spatial_gaps)) return(NULL)
    sensitive_spatial_gaps |> filter(basisofrecord == "all")
  }, "sensitive_species_cells")

  output$pub_dependency_map_dl <- dl_csv(function() {
    if (is.null(publisher_cell_dep)) return(NULL)
    publisher_cell_dep
  }, "publisher_dependency_cells")
}

shinyApp(ui, server)
