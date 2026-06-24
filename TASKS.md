# GBIF Gap Finder — Task List

> Companion to `ROADMAP.Rmd`. This is the **doing** list, sorted into three tiers:
> **major work**, **technical**, and **minor / cosmetic**.
>
> **Current focus: app alignment & cosmetic work (Tier 3).** The analytical scope and the
> pipeline-heavy work are essentially complete — what's left in the short term is
> consistency, readability, and communication.
>
> Task IDs (`T-R*` refactor · `T-I*`/`T-A*` integrity/perf · `T-D*` data/decision · `T-Q*`
> quality) match the roadmap. **File/line references drift — verify against the current tree
> before editing.**
>
> Operational note: app-side fixes take effect on the next **`app.R` redeploy**; pipeline
> fixes ride the next **`tar_make()` rerun**. Each item below is tagged where it matters.

---

## Tier 1 — Major work

*Substantial builds. Mostly post-Ebbe; none are on the immediate critical path.*

- [ ] **Auto-generated "key findings" panel (Overview)** — a top "what matters" panel with
  4–6 auto findings (largest spatial gaps, most underrepresented groups, key threatened-species
  gaps, stale areas, publisher-dependency risks). *The persona "start here" nav is already
  built; this auto-panel is the remaining Overview piece.* App-side. *(§1.11.2)*

- [ ] **Norway replication, end-to-end (Phase 2)**
  - [ ] Fill `config_NO.yml` (taxonomy DOI, red-list DOI, GBIF cube DOIs).
  - [ ] Download NO cubes + EEA grid clipped to Norway + Nortaxa DwC-A; run full pipeline.
  - [ ] Document Sweden-specific assumptions that break (column names, red-list format, synonym
    structure). *Prereq: the `dyntaxa_*` key rename (T-R7) and per-country data folders.*

- [ ] **Snapshot-based "Gap Trends" retrospective** *(supersedes T-D3)*
  - [ ] Generate cubes from GBIF monthly snapshots (DuckDB/Arrow) instead of SQL-API downloads.
  - [ ] Multi-snapshot runner across dates (2023/2024/2025); collect dashboard metrics.
  - [ ] New "Gap Trends" tab/report (coverage %, zero-coverage cells, stale-cell trend,
    threatened trajectory, publisher diversity).
  - [ ] Rebuild "published to GBIF over time" from dated snapshots (replaces the retired cube column).

- [ ] **b3verse cube-stack rewrite** *("Bundle B"; scope & timing TBD)* — migrate the cube schema
  to the b3verse / b-cubed standard; automate downloads via `rgbif::occ_download_sql()`; canonicalise
  the SQL so the `GROUP BY` query *is* the cube spec. *Overlaps with the snapshot retrospective
  (cube generation); the snapshot/diff use case lives there.*

- [ ] **Family-level resolution** *(T-D1 → build)*
  - [ ] Measure first: build the family×cell recency cross-tab; report row count + serialized
    `.rds` size; compare to the class-level bundle and the ~1.2M-row `publisher_cell_taxonomy`.
  - [ ] Decide family-as-default vs drill-down-only (consult Kevin per group); then build.

---

## Tier 2 — Technical

*Pipeline and code: correctness, portability, hygiene. No UI.*

### Pipeline refactor stragglers
- [ ] **T-R3** — Migrate computation out of script 11: Section 12 (overview last-year stats) and
  Section 13 (Troudet bias) → 09c or a new 09d; reduce 11 to a true pure loader; update
  `_targets.R` deps.
- [ ] **T-R5** — Stop 09b re-classifying the backbone (line ~103): read 09a's classified output
  instead of reloading `taxa_reference_current.rds` and re-running `classify_accepted()`.
- [ ] **T-R6** — Remove `add_yearmonth_cols()` (script 11, line ~50, 6 call sites) once all 09c
  outputs are confirmed to carry year/month.
- [ ] **T-R7** — Rename `dyntaxa_*` data keys → `backbone_*` across 09c filename suffixes, the
  script 11 loader, and `app.R`. **Before Norway ships publicly.**

### Data integrity & robustness
- [ ] **T-I1** — 03: strip authorship / match canonical name in the red-list join (matters for
  non-Swedish backbones).
- [ ] **T-I2** — 02: warn if `parse_10km()`-derived 50 km codes don't exist in the grid.
- [ ] **T-I3** — 07: cache the country boundary (avoid full-grid `st_union`).
- [ ] **T-I4** — 09b/06b: staleness check before 06b reruns (`by_order` / `by_family` double-count risk).

### Performance & hygiene
- [ ] **T-A1** — 02→07: cache `cellcodes_10km.txt` / `cellcodes_50km.txt`.
- [ ] **T-A2** — 06a: document or remove the unused `poly_id`.
- [ ] **T-Q1** — `run.R`: bring `cli` usage in line with the rest of the scripts.
- [ ] **T-Q2** — Standardise on `|>`; enforce 100-char width; doc headers.
- [ ] **`publisher_cell_taxonomy` bundle size** — if ~1.2M rows is too heavy for the Shiny bundle,
  pre-aggregate to class level instead of order level.

### Portability
- [ ] **Per-country app data folders** — `shiny_app/.../data/{CC}/shiny_data.rds` so country
  bundles coexist. Touches script 11 (output path), `app.R` (load path), Dockerfile (COPY),
  `.gitignore`, `_targets.R`.

### Deferred
- [ ] ⏸ **T-D5** — Swedish marine cells: include the EEZ in the country clip. Geometry change
  needing marine boundary data; cosmetic, not correctness. *(Note: opposite direction to the
  T-D7 land-clip tightening — marine cells already carrying Swedish data are kept via `has_data`.)*
- [ ] **T-D7.3** — Decide whether to fold the recency > 10-years category into Priorities.

---

## Tier 3 — Minor / cosmetic  ⭐ current focus

*App alignment, readability, and communication. This is the active sprint.*

### Calculations & cross-tab alignment
- [ ] **Verify numbers reconcile after the rerun** — gap / coverage figures consistent across
  Overview, Taxonomic, and Concern (post-rerun check on the denominator + Overview↔Concern
  fixes; use `diagnose_unmatched.R`).
- [ ] Confirm the Overview "threatened" figure matches the Concern tab exactly.
- [ ] Decide whether the publisher dependency map should respond to the category filter.

### Look & readability
- [ ] Finish the font-size pass (readable ~1rem throughout).
- [ ] Clearer, plainer descriptions on every tab.
- [ ] Larger, more visible download buttons.
- [ ] Make the stale-cell colour scale legible.
- [ ] **P1.5 / T-D6.2** — Log-transformed view for publisher charts; Record Types pie chart-choice
  fix (log-scaled bar or "minor types" rollup) so minor basis classes aren't swamped by ~119 M
  human observations.
- [ ] Data download buttons on all tables and maps (coverage, beyond button size above).
- [ ] Bar titles in downloaded chart images when > 20 groups.

### Framing & communication
- [ ] Consistent **measurement → interpretation → action** structure in tab text.
- [ ] Reword spatial staleness ("no GBIF-mediated records in the last 10 years"; suggest checking
  national/regional data before prioritising resurvey).
- [ ] Frame single-publisher cells as infrastructure vulnerability / partnership opportunity.
- [ ] Label "Total Swedish Occurrences" (not "Total Occurrences").
- [ ] Make the observed-vs-published distinction visible in charts/labels.
- [ ] Short "how to interpret…" examples (stale cell, missing threatened species, taxonomic bias).
- [ ] Hyperlinks in Overview graph headers to jump to the relevant tab.

### Accessibility — EAA (legal obligation)
- [ ] Glossary / "what does this mean?" tooltips for technical terms (occurrence cubes, backbone,
  scope-filtered summaries, establishment means, basis of record, single-publisher cells, Troudet
  bias) — support the terms, don't remove them.
- [ ] Colour-blind-safe per-chart palettes (per-chart, not a wholesale swap); avoid red/green.
- [ ] Logical header structure; a "methods & limitations" expandable section.

### Taxonomic UX
- [ ] External taxon links (Wikipedia / Artfakta) for non-specialists.
- [ ] Consistent Latin/English in the exclusion search ("Aves" vs "Birds").
- [ ] Fix broken links / missing row names in taxonomic tables.
- [ ] Colour-code mobilization targets by higher taxonomy (plant families green, insects red, fungi brown).

### Species of Concern — finishing touches
- [ ] Cite + link the source checklists (red list, GRIIS, sensitive list) in the tab.
- [ ] Put the 25/50 km generalised-coordinate caution directly on sensitive-species maps.
- [ ] Swedish common names in the bottom table.
- [ ] Confirm the April-2026 red list is the one in use; link it in "About."

### Priorities & Record Types
- [ ] Document what the Priorities "targets for the next 12 months" are based on.
- [ ] Integrate the sampling-bias data into the Priorities tab (remaining Priorities-tab review item).
- [ ] Basis of record "last 12 months" per-basis breakdown — data exists in 09c's
  `basis_recent_<scope>_<grid>.csv`; app integration pending.

### Temporal & Data tab
- [ ] **D3** — Default to the last *complete* year on the temporal tab; keep the current partial
  year selectable but visually distinguished (greyed/dashed).
- [ ] Rename "Select Dataset" → "Select Gap Explorer Dataset" on the Data tab.
- [ ] Confirm sensitive `sensitivity_category` flows end-to-end; add it to the species scope
  lookup CSV for the Data tab.

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
Start with the **Tier 3 calculations check** (verify the rerun numbers reconcile across
Overview / Taxonomic / Concern), then take the highest-visibility **look & framing** items —
that is the bulk of what "next updates" means now.
