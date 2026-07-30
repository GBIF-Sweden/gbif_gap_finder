# GBIF Gap Finder — Task List

> Companion to `ROADMAP.Rmd`. This is the **doing** list, sorted into three tiers:
> **major work**, **technical**, and **minor / cosmetic**.
>
> **Correctness round complete (2026-07).** A full audit reconciled every headline number
> across Overview / Taxonomic / Concern, fixed the miscalculations (see *Recently closed*),
> and added `scripts/12_reconcile.R` as a reconciliation guardrail (run manually via `run_reconcile()`). **Current focus now:**
> (1) align the analysis reports (`analysis/*.Rmd`) with the corrected app numbers, then
> (2) the Tier-3 UX / framing / accessibility polish. The analytical scope is complete.
>
> Task IDs (`T-R*` refactor · `T-I*`/`T-A*` integrity/perf · `T-D*` data/decision · `T-Q*`
> quality) match the roadmap. **File/line references drift — verify against the current tree
> before editing.**
>
> Operational note: app-side fixes take effect on the next **`app.R` redeploy**; pipeline
> fixes ride the next **`tar_make()` rerun**. Each item below is tagged where it matters.

---

## Tier 1 — Major work

*Substantial builds.* **Sequencing (updated 2026-07-28 with Lena):** the **key-findings panel**
(2026-07-24) and the **b3verse cube-stack** (2026-07-27) have shipped. **Gap Trends** is the next
build; **family-level resolution** waits on Kevin's per-group call; the **CoL backbone migration**
is parked pending GBIF; **Norway replication** is de-risked but **deprioritised to the end of the
list**.

- [x] **Auto-generated "key findings" panel (Overview)** — *done 2026-07-24 (commit `5fef6f6`).*
  A top "what matters" strip with auto-computed findings (spatial coverage, staleness,
  threatened-species gap, most under-represented group, publisher dependency, recent momentum), each
  reusing its tab's reactive so the numbers reconcile; zero-value gaps and missing bundle objects
  drop their card, and the most severe gap is auto-ranked as the headline. *The persona "start here"
  nav was already built; this was the remaining Overview piece.* **App-side only (app.R →
  redeploy, no `tar_make`).** *(§1.11.2)*

- [ ] **Snapshot-based "Gap Trends" retrospective** *(supersedes T-D3; scheduled for the following sprint, ~next week — 2026-07-24)*
  - [ ] Generate cubes from GBIF monthly snapshots (DuckDB/Arrow) instead of SQL-API downloads.
  - [ ] Multi-snapshot runner across dates (2023/2024/2025); collect dashboard metrics.
  - [ ] New "Gap Trends" tab/report (coverage %, zero-coverage cells, stale-cell trend,
    threatened trajectory, publisher diversity).
  - [ ] Rebuild "published to GBIF over time" from dated snapshots (replaces the retired cube column).

- [x] **b3verse cube-stack rewrite** — *done 2026-07-27 (branch `feature/b3verse-cube-stack`).*
  Cube schema migrated to the **b-cubed superset** (adds `mincoordinateuncertaintyinmeters`,
  `mintemporaluncertainty`, `distinctobservers`; grid radius 0); downloads automated via
  `rgbif::occ_download_sql()` with a manual-query fallback; SQL canonicalised into
  `sql/gbif_occurrence_cube.sql` (the query *is* the cube spec). Live SE re-download succeeded
  (50.6M / 26.6M rows, 17 cols) and the pipeline runs green on it. *Optional follow-ons:* auto-update
  config with fresh download keys; wire the uncertainty measures into an app view; feed the superset
  to `b3gbi::process_cube()`.

- [x] ✅ **Catalogue of Life (COL) backbone migration — RESOLVED (2026-07-30).** GBIF replied: the
  migration is complete on the current backend and fresh SQL cubes return clean COL keys. Repairs are
  **in + running green** (character `specieskey`, COL-native Tier 4, COL provenance pinned) and the
  Dyntaxa→COL crosswalk augment (`scripts/09a1` → Tier 5 in `09a`) is wired in. The earlier
  "~5.7 % legacy integer" reading came from the **April** download (`0020270-260409193756587`, built
  mid-migration) plus valid **numeric COL ids** (e.g. `67343` = *Anemone nemorosa*) miscounted as
  legacy — NOT a permanent dual-key design. Direct scan of the fresh cube `0017309`: 76,291
  alphanumeric + 298 numeric COL keys, **zero** ≥7-digit nub keys; hedgehog only under COL `3B2C2`. The
  augment is kept as key-format-agnostic insurance. See `claude/finding-cube-mix-RESOLVED-2026-07-30.md`
  (which corrects `claude/finding-cube-key-mix-2026-07-27.md`).

- [ ] **Family-level resolution** *(T-D1 → build)*
  - [x] Measure first: build the family×cell recency cross-tab; report row count + serialized
    `.rds` size; compare to the class-level bundle and the ~1.2M-row `publisher_cell_taxonomy`.
    — *done 2026-07-24: family×cell ≈ 2 M rows / ~110 MB RAM / ~19–29 MB compressed at 10km
    (11.6× class-level, larger than `publisher_cell_taxonomy`), and sparse (36 % of groups ≤5
    occ) → **drill-down-only**. See `claude/finding-family-resolution-2026-07-24.md`.*
  - [ ] Decide family-as-default vs drill-down-only — **gated on Kevin's per-group call (confirmed 2026-07-24); parked until then.** Recommendation stands: drill-down-only, per-group and/or occ>5. Then build.

- [ ] **Norway replication, end-to-end (Phase 2)** — *deprioritised to the end (2026-07-28); still
  de-risked and ready whenever wanted (ran clean end-to-end before the session-5 changes; the
  prereqs — `dyntaxa_*`→`backbone_*` rename T-R7 + per-country data folders + single
  `GBIF_GAP_COUNTRY` — are done, so a clean re-run is expected).*
  - [ ] Fill `config_NO.yml` (taxonomy DOI, red-list DOI, GBIF cube DOIs).
  - [ ] Download NO cubes + EEA grid clipped to Norway + Nortaxa DwC-A; run full pipeline.
  - [ ] Document Sweden-specific assumptions that break (column names, red-list format, synonym
    structure).

---

## Tier 2 — Technical

*Pipeline and code: correctness, portability, hygiene. No UI.*

### Pipeline refactor stragglers
- [x] **T-R3** — *done 2026-07 (two patches).* **Correctness** (`gap_finder_tr3_snapshot_window.patch`):
  script 11's order-trend / fallback windows now anchor on the cube snapshot year, not
  `year(Sys.Date())`, so `order_change`/`order_5yr` stop drifting with the run date. **Relocation**
  (`gap_finder_tr3_pure_loader.patch`): the order-trend, overview-last-year, and Troudet computation
  moved into script 10 (writes CSVs); script 11 is now a **pure loader** (~260 lines lighter). No
  `_targets.R` change — script 10's output glob picks up the new CSVs. *(rerun: `tar_destroy()` → `tar_make()`.)*
- [x] **T-R5** — Stop 09b re-classifying the backbone (line ~103): read 09a's classified output
  instead of reloading `taxa_reference_current.rds` and re-running `classify_accepted()`.
- [x] **T-R6** — Remove `add_yearmonth_cols()` (script 11, line ~50, 6 call sites) once all 09c
  outputs are confirmed to carry year/month.
- [x] **T-R7** — Rename `dyntaxa_*` data keys → `backbone_*`. *Done (app.R + main keys clean).* Minor
  mop-up left: two legacy fallback keys (`dyntaxa_time_summary`/`_cell_last_year`, 11:655–656) and the
  `has_dyntaxa_scope` flag (11:956); `dyntaxa_redlist_category` (09b:152) is a real Dyntaxa source column — keep.

### Data integrity & robustness
- [x] **T-I1** — 03 red-list/sensitive join. *Resolved (2026-07), opposite to the original hunch:*
  the Swedish red-list and sensitive DwC-A keep authorship in a separate column, so **exact
  `scientificName` match is correct** (canonicalising over-matched Dyntaxa hybrids). Only GRIIS
  invasive carries authorship — its canonical join now also restricts to species-rank, non-hybrid
  taxa (490→337). Revisit only if a non-Swedish backbone bundles authorship into `scientificName`.
- [x] **T-I2** — 02: warn if `parse_10km()`-derived 50 km codes don't exist in the grid.
- [x] **T-I3** — 07: cache the country boundary (avoid full-grid `st_union`).
- [x] **T-I4** — 09b/06b `by_order`/`by_family` double-count. *Verified clean (2026-07 audit):*
  order/family files are disjoint and aggregation is by key, so no double-count. No action.

- [x] **Pipeline hardening (targets DAG + 04 guard)** — *done 2026-07-28
  (`gap_finder_hardening.patch`).* Declared the undeclared **09a → 06b** dependency in `_targets.R`
  (09a reads 06b's `species_summary` CSVs — had forced manual `tar_invalidate` 4×); **04** now fails
  loud when no cube parquet is produced (was a silent empty manifest → cryptic downstream
  `stopifnot`); and `sql/gbif_occurrence_cube.sql` is tracked as a `raw_data` **file-dependency** so
  editing the cube query invalidates the download. Rebuilt green + `12_reconcile` clean. *(rerun done)*

### Performance & hygiene
- [x] **T-A1** — 02→07: cache `cellcodes_10km.txt` / `cellcodes_50km.txt`.
- [x] **T-A2** — 06a: document or remove the unused `poly_id`.
- [x] **T-Q1** — `run.R`: bring `cli` usage in line with the rest of the scripts.
- [x] **T-Q2** — Standardise on `|>`; enforce 100-char width; doc headers. *(app.R prose strings + script 11 header left by design)*
- [x] **`publisher_cell_taxonomy` bundle size** — measured 2026-07-23: 6.4% of bundle / ~11 MB gzip,
  not the heavy item → NOT pre-aggregated (see claude/finding-bundle-size-2026-07-23.md).

### Portability
- [x] **Per-country app data folders** — *done 2026-07-24 (commits `2c70d0d` + `9c12c23`).*
  `shiny_app/.../data/{CC}/shiny_data.rds` so country bundles coexist; script 11 (output path),
  `app.R` (single `GBIF_GAP_COUNTRY` load path), Dockerfile (`ARG`/COPY), CI, `.gitignore`,
  `_targets.R` all aligned.

### Deferred
- [x] **T-D5** — Swedish marine cells: the EEZ is now in the grid universe. *Done 2026-07-28
  (`gap_finder_td5_marine.patch`, branch `feature/td5-marine`): config-gated `marine.enabled`, SE on,
  full Swedish EEZ via mregions2 (MRGID 5694), both resolutions. Rebuilt green — 10 km 97.8 % coverage
  / 138 zero-coverage sea cells (Swedish sea ≈ 91 %), 50 km 100 %. The EEA grid already contained sea
  cells, so the clip widening (centroid ∈ land ∪ EEZ) did the work; the `marine` cell flag stays inert
  until the app's terrestrial-only filter is built (`gap_finder_td5_marine_flag.patch` ready). See
  `claude/finding-td5-marine-coverage.md`.* *(rerun)*
- [ ] **T-D7.3** — Decide whether to fold the recency > 10-years category into Priorities.

---

## Tier 3 — Minor / cosmetic  ⭐ current focus

*App alignment, readability, and communication. This is the active sprint.*

### Calculations & cross-tab alignment
- [x] **Numbers reconcile after the rerun** — done in the 2026-07 audit; `scripts/12_reconcile.R`
  is available as a manual guardrail (`run_reconcile()`), not wired into `tar_make()`. Overview / Taxonomic / Concern agree.
- [x] **Overview "threatened" matches Concern exactly** — verified; both read `match_summary_full`.
- [x] **Align `analysis/*.Rmd` reports with the app** — *done 2026-07 (`gap_finder_report_parity.patch`).*
  Reports now compute threatened/concern counts from `match_summary` (not `tax_by_threat`), keep
  **DD** out of the threatened set, and use the app's 3-category publisher classifier. Verified in R:
  report `threat_cov` == app `ov_threat_stats`; classifier output identical to the app's. *(re-knit reports.)*
- [ ] Decide whether the publisher dependency map should respond to the category filter.
  *(Note: the Publishers **count** 434 vs 419 is not a bug — 419 is the count for a selected
  taxonomic group; at "All" it is 434, matching the Overview.)*

### Look & readability
- [x] Finish the font-size pass (readable ~1rem throughout) — *done 2026-07 (`gap_finder_readability_2.patch`): every sub-11px chart label bumped to 11.*
- [x] Clearer, plainer descriptions on every tab — *done 2026-07 (`gap_finder_framing_1.patch`): per-tab measurement → interpretation → action guide.*
- [x] Larger, more visible download buttons — *done 2026-07 (`gap_finder_readability_1.patch`).*
- [x] Make the stale-cell colour scale legible — *done 2026-07 (`gap_finder_eaa_cb_maps.patch`): binned + colour-blind-safe.*
- [x] **P1.5 / T-D6.2** — Log-transformed view for publisher charts; Record Types pie fix — *done 2026-07:
  Record Types "minor types" rollup (`readability_1`) + optional Linear/Log toggle on the publisher
  volume charts (`readability_2`).*
- [x] Data download buttons on all tables and maps — *done 2026-07-24 (`gap_finder_data_downloads_chart_titles.patch`):
  CSV export now on the Data & Sources contributing-datasets table (DT Buttons, client-side) and a "Download cell
  data (CSV)" button beneath the Spatial, Record-Types, and the three Concern (threatened/invasive/sensitive) maps
  plus the Publisher-dependency map; the Priorities zero/stale maps already export via their companion tables. (redeploy)*
- [x] Bar titles in downloaded chart images when > 20 groups. — *done 2026-07-24
  (`gap_finder_data_downloads_chart_titles.patch`): `plotly_layout(dl_title=)` bakes a title into the taxonomic
  order/family bars (which reach 40 bars) whenever > 20 groups are shown, so the downloaded PNG stays self-describing. (redeploy)*

### Framing & communication
- [x] Consistent **measurement → interpretation → action** structure in tab text — *done 2026-07
  (`gap_finder_framing_1.patch`): guide strip on the 6 analysis tabs.*
- [x] Reword spatial staleness ("no GBIF-mediated records in the last 10 years"; check national/regional
  data before prioritising resurvey) — *already in place; confirmed 2026-07.*
- [x] Frame single-publisher cells as infrastructure vulnerability / partnership opportunity — *done 2026-07
  (`gap_finder_framing_1.patch`).*
- [x] Label "Total Swedish Occurrences" (not "Total Occurrences") — *done 2026-07 (`gap_finder_framing_1.patch`),
  demonym-aware so the Norway port reads correctly.*
- [x] Make the observed-vs-published distinction visible in charts/labels — *done 2026-07
  (`gap_finder_framing_2.patch`): glossary entry + temporal guide note.*
- [x] Short "how to interpret…" examples — *done 2026-07: the per-tab "How to read it" guide + glossary
  cover stale cells, missing threatened species, and taxonomic bias.*
- [x] Hyperlinks in Overview graph panels to jump to the relevant tab — *done 2026-07
  (`gap_finder_framing_2.patch`).*

### Accessibility — EAA (legal obligation)
- [x] Glossary / "what does this mean?" tooltips for technical terms — *done 2026-07
  (`gap_finder_eaa_a11y_structure.patch`): accessible `gloss()` tooltips (hover **and** keyboard
  focus, screen-reader `aria-label`) plus a full Glossary inside the Methods panel.* *(redeploy.)*
- [x] Colour-blind-safe per-chart palettes; avoid red/green — *done 2026-07
  (`gap_finder_eaa_cb_palettes.patch` + `gap_finder_eaa_cb_maps.patch`): categorical charts
  (in-GBIF/missing, threat, establishment), all choropleth maps (RdYlBu — blue = covered/recent,
  red = gap/stale), dependency map, and the Priorities stale map binned. Sensitive map was already
  CB-safe.* *(redeploy.)*
- [x] Logical header structure; a "methods & limitations" expandable section — *done 2026-07
  (`gap_finder_eaa_a11y_structure.patch`): `h1` → `h2` (59 card titles) → `h3`; native `<details>`
  Methods, limitations & glossary panel on the Overview.* *(redeploy.)*

### Taxonomic UX
- [x] External taxon links (Wikipedia / Artfakta) for non-specialists. — *done 2026-07-24
  (`gap_finder_taxon_ux.patch`): a "Ref" column with a Wikipedia article link (`taxon_ref()`,
  `escape = FALSE`) on the Concern threat/invasive/sensitive tables; scientificName stays plain for
  search/CSV. Artfakta deferred — its taxon-URL scheme isn't documented; add once confirmed. (redeploy)*
- [x] Consistent Latin/English in the exclusion search ("Aves" vs "Birds"). — *done 2026-07-24
  (`gap_finder_taxon_ux.patch`): exclusion placeholder is now vocabulary-neutral ("Type a group to
  exclude"), removing the Aves-vs-Birds mismatch. Drill-down still shows Latin ranks — class/order/family
  have no standard English names. (redeploy)*
- [x] Fix broken links / missing row names in taxonomic tables. — *done 2026-07-24
  (`gap_finder_taxonomic_concern_polish.patch`): dead "Browse Dyntaxa" link → `artfakta.se`;
  `rownames = FALSE` on the Concern threat/invasive/sensitive tables. (redeploy)*
- [x] ~~Colour-code mobilization targets by higher taxonomy (plant families green, insects red, fungi brown).~~ *(won't do 2026-07-24: redundant — the Troudet bias chart already uses colour for over/under-representation (sage/coral); recolouring by taxonomy would drop that signal and reintroduce red/green, which the EAA round removed. Decision: drop.)*

### Species of Concern — finishing touches
- [x] Cite + link the source checklists (red list, GRIIS, sensitive list) in the tab. — *done
  2026-07-24 (`gap_finder_taxonomic_concern_polish.patch`): `checklist_cite()` adds a source line +
  resolved-DOI link to each Concern sub-tab, from `metadata$data_sources$checklists`. (redeploy)*
- [x] Put the 25/50 km generalised-coordinate caution directly on sensitive-species maps. — *done
  2026-07-24 (`gap_finder_concern_finishing.patch`): persistent "coordinates generalised (5–50 km)"
  caption on the sensitive map via leaflet `addControl`. (redeploy)*
- [ ] Swedish common names in the bottom table. *(blocked app-side: no `vernacularName` in the bundle
  or pipeline — needs 03 to carry Dyntaxa vernacular names through 09a/09b into `match_summary`, then a rebuild.)*
- [x] Confirm the April-2026 red list is the one in use; link it in "About." — *done 2026-07-24
  (`gap_finder_concern_finishing.patch`): current edition is the **Swedish Red List 2025**
  (`swedishredlist2025`; the list is 5-yearly, so there is no 2026 edition). Concern "About" now lists
  the reference lists with resolved titles + DOIs, so the edition is verifiable in-app. (redeploy)*

### Priorities & Record Types
- [ ] Document what the Priorities "targets for the next 12 months" are based on.
- [ ] Integrate the sampling-bias data into the Priorities tab (remaining Priorities-tab review item).
- [ ] Basis of record "last 12 months" per-basis breakdown — data exists in 09c's
  `basis_recent_<scope>_<grid>.csv`; app integration pending.

### Temporal & Data tab
- [x] **D3** — Default to the last *complete* year on the temporal tab; keep the current partial
  year selectable but visually distinguished (greyed/dashed). — *done 2026-07-23
  (`gap_finder_data_temporal_polish.patch`): slider defaults to the last complete year
  (`data_max_year - 1`, from `time_summary`, not `Sys.Date()`); partial year still selectable +
  greyed/labelled in the trend chart. (redeploy)*
- [x] Rename "Select Dataset" → "Select Gap Explorer Dataset" on the Data tab. — *done 2026-07-23
  (`gap_finder_data_temporal_polish.patch`): explorer label "Select output:" →
  "Select Gap Explorer Dataset:". (redeploy)*
- [x] Confirm sensitive `sensitivity_category` flows end-to-end; add it to the species scope
  lookup CSV for the Data tab. — *done 2026-07-23 (`gap_finder_sensitivity_scope_lookup.patch`):
  09a carries it into `recon`, 09c writes it to `species_scope_summary.csv`; surfaces as a column
  in the Data-tab "Species Scope Lookup" (SE: 134 sensitive spp × 5/25/50 km).*

### Metadata, citation & docs
- [ ] Recommended citation for the app (separate from the data DOI, which is done).
- [ ] Version / contact / data-provenance block (DOI part done; add version + contact).
- [ ] GitHub documentation + user manual, including framing and interpretation guidance.

### Calls to action
- [ ] Direct publisher CTAs ("Do you hold data for these taxa?", "Can your institution help fill
  these cells?", "Contact GBIF Sweden for publication support.").

---

## Recently closed

*Compacted so the tiers above stay scannable. Items tagged "(rerun)" ride the next `tar_make()`;
"(redeploy)" takes effect on the next `app.R` deploy.*

**Audit & correctness round (2026-07)** — full reconciliation of every headline number, shipped as pipeline fixes on `fix/audit`:
- [x] **`threatened_in_reference` miscount** — script 10 counted threat *categories* (≤4); now sums species (`n_ref_total`). *(rerun)*
- [x] **"Cells active last year" ~12× overcount** — script 11 summed monthly distinct-cell counts; now counts each cell once. *(rerun)*
- [x] **Per-cell occurrence double-count** — script 07 summed the synthetic `"all"` basis row; now excluded. *(rerun)*
- [x] **"All GBIF" = all GBIF** — `restrict_to_backbone_scope: false`; occurrence tabs count every kingdom and 09c's `all` scope is genuinely all. Overrides part of T-R4. *(rerun)*
- [x] **Cell staleness anchored to the cube snapshot date** (`globals::get_snapshot_date`), replacing `Sys.Date()` / data-max — reproducible, consistent across 08/09c. *(rerun)*
- [x] **50 km grid clipped centroid-in-country** (like 10 km) instead of derived from data cells — fixes the structural 100 %-coverage / 0-empty-cells at 50 km. *(rerun)*
- [x] **"Poorly sampled" redefined** — bottom-10 % occurrences OR < 3 cells (config-driven), replacing the degenerate `< 1` test. *(rerun)*
- [x] **Concern checklist matching corrected** — red-list/sensitive back to exact match (178 sensitive, 4 859 threatened); invasive de-duped to species-rank non-hybrid (490→337). *(rerun)* *(T-I1)*
- [x] **Input guards (04/05)** — 04 aborts on missing required cube columns + fixes `ds$num_rows`; 05 hard-stops on cube cells absent from the grid. *(rerun)*
- [x] **`scripts/12_reconcile.R`** — new reconciliation guardrail (run manually via `run_reconcile()`) asserting the headline numbers agree across layers.

- [x] **T-S1** — App made metadata-driven; kills the Norway "Sweden/Dyntaxa" title bug. *(redeploy)*
- [x] **Species of Concern tab** built — Threatened / Invasive / Sensitive sub-tabs (replaced the old Threatened tab).
- [x] **Persona "start here" navigation** (jump-to-tab by user group). *(D13)*
- [x] **Troudet exclusion** multi-select filter (hide dominant taxa like Aves).
- [x] **Troudet default GBIF-style groups** — curated mixed-rank mapping on the landing view.
- [x] **DOI + citation** surfaced on Data & Sources; cube + reference DOIs resolved and linked. *(D9)*
- [x] **Filters: selectable "All" / reset** on every taxonomy + kingdom filter, all tabs. *(D8)*
- [x] **Publisher overhaul** — GBIF Sweden hidden (D10); three-category classifier (D11);
  `publisher_taxonomy` / `publisher_cell_taxonomy` bundled so filters populate. *(rerun)*
- [x] **T-R4** — Backbone kingdom scope filter at the cube (06a/06b/08); cleans match rate, makes
  Overview totals consistent with scoped tabs; keeps NA-kingdom rows. *(rerun)*
- [x] **Taxonomic gap denominator fix** — restricted to species/subspecies ranks, microbial
  kingdoms excluded; **Overview↔Concern mismatch** fixed. *(rerun — verify in Tier 3)*
- [x] **T-D7** — Border zero-cells fixed via centroid-in-country + `has_data` fallback. *(rerun)*
- [x] **plotly axis-label thinning** across the Taxonomic + Concern charts (linear ticks, automargin).
- [x] **T-R1** — `calc_metric()` now logs the failing expression + error instead of a silent `NA`.
- [x] **Unclassified rank-bucketing** — order-less taxa (reptiles → Squamata) no longer dropped.
- [x] **App-wide font enlargement** (initial pass — continue in Tier 3).
- [x] **`year_published` retired** from the cube SQL and all downstream code.
- [x] **T-D4** — Fossil-timeline floor lowered so pre-1970 specimen history shows. *(redeploy)*
- [x] **CI / Git-LFS Docker hotfix (v0.4.2)** — demo image now bundles the ~90 MB data, not the LFS stub.
- [x] **Project rename** to `gbif-gap-finder`; second "Biodiversity Explorer" app removed.

---

### Suggested next session
Report↔app parity, **EAA accessibility**, **readability & chart legibility**, and **plain-language
framing** are all **done** (2026-07, redeploy pending) — seven `gap_finder_*.patch` files in the repo
root. Small Tier-3 leftovers: CSV download buttons on the remaining tables (Data & Sources) + maps,
and bar titles baked into downloaded chart images (>20 groups). With the communication layer
essentially complete, and **T-R3** (script 11 → pure loader) and **T-R7** (`dyntaxa_*`→`backbone_*`)
are now done, the remaining pipeline-hygiene refactors are **T-R5** (stop 09b re-classifying the
backbone) and **T-R6** (drop `add_yearmonth_cols`), plus the smaller T-I/T-A/T-Q items. Norway
replication is deprioritised.
