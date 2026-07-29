# GBIF Gap Finder — User Manual

*A field guide to reading the dashboard and turning gaps into action. For Sweden.*

The Gap Finder is an interactive dashboard that shows **where Sweden's GBIF‑mediated
biodiversity data is thin** — geographically, over time, and across the tree of life — and
turns that into a prioritised, actionable picture. It is built for the people who can help
close those gaps: **data holders and collection managers** deciding what to digitise or
publish, and the biodiversity‑informatics community coordinating national coverage.

The deployed app lives at the GBIF Sweden site (currently
`https://test.gbif.se/gap-finder/`). This manual explains what each part means and how to
read it honestly.

---

## Start here (the 60‑second version)

- **What it shows.** How much biodiversity occurrence data has been published to GBIF for
  Sweden, and — measured against the national species checklist (Dyntaxa) — where the
  holes are: empty map cells, out‑of‑date areas, under‑recorded species groups, and
  threatened / invasive species with no records at all.
- **The one caveat that matters most.** A *gap* means **missing GBIF records** — not that a
  species is absent, unstudied, or unmonitored. Recent or non‑digitised data may exist
  outside GBIF. Every number here describes the *data*, not nature.
- **If you hold data:** jump to *For data holders* below. In short — filter to your
  taxonomic group, look at the **Spatial** map and the **Priorities** tab to see where your
  records would matter most, then publish them to GBIF (via GBIF Sweden).
- **If you're prioritising work internally:** start on **Overview**, then **Priorities**.

---

## For data holders: using it to decide what to mobilise

You do not need to read every tab. This is the shortest path from "we have some data" to
"here's where it fills a national gap":

1. **Narrow to your world.** Use the taxonomic filters (Kingdom → Phylum → Class → Order →
   Family) and, where offered, the **scope** filter to focus on your group. Bird records
   dominate Swedish GBIF data, so if you work on anything else, filtering *out* Aves is
   what makes the real gaps visible.
2. **See the geographic holes for that group.** On the **Spatial** tab, red or empty cells
   are places with few or no GBIF records for your selection — candidate areas where your
   observations or specimens would add coverage that currently doesn't exist.
3. **Check what's most urgent.** The **Priorities** tab ranks the gaps: never‑sampled and
   long‑stale cells, and the taxonomic groups furthest below their fair share of records.
4. **Look at species of concern.** On **Species of Concern**, "missing" threatened or
   invasive species have **zero** GBIF records — the highest‑value records you could
   possibly mobilise.
5. **Act.** If you hold relevant data, publishing it to GBIF (through GBIF Sweden / an IPT)
   is how it lands here. See *Contributing data* at the end.

A gap you can fill is an opportunity, not a criticism of anyone's dataset — most cells that
depend on a single publisher simply haven't had a second contributor yet.

---

## The tabs, one by one

The dashboard has ten views. Most tabs carry an **"About this tab"** expander and a short
plain‑language summary at the top; this section mirrors and extends those.

### Overview

The landing page. Headline totals (occurrences, species in GBIF, year range, number of
10 km cells) and four gap summaries — Spatial, Temporal, Taxonomic, Threatened — each with
a link straight to the detailed tab. A **"Last 12 months in review"** row highlights recent
activity (records *dated* in the last 12 months, by observation date). The **"Methods,
limitations & glossary"** expander here is the canonical reference for how every figure is
produced.

### Priorities

Turns what the other tabs found into a ranked to‑do list for the people who decide where
survey effort and mobilisation money go. It surfaces never‑sampled and **stale** cells (no
GBIF records newer than ~5 years — worth resurvey *after* checking whether recent data
exists outside GBIF), the most **under‑sampled orders and families** (largest gap between
known species and GBIF coverage), and infrastructure risks (single‑publisher cells). A
**"Next 12 months"** section projects realistic targets at ~1.5× the rate achieved in the
last 12 months. For a data holder, this is the shortlist of where a contribution moves the
national needle most.

### Spatial

How biodiversity observations are distributed across Sweden's **10 km EEA reference grid**.
Each cell is coloured by a metric you choose: total occurrences, data **recency**, species
richness, or observations. Read it as: **red or pale cells are gaps or stale; blue cells
are well covered.** The recency view flags cells with no GBIF records in the last 10 years
(red) or 5–10 years (orange). Filtering to non‑Aves groups reveals sampling gaps that bird
data otherwise hides. The distribution histogram shows whether many cells hold only token
data (1–10 records). Target empty and red cells for fieldwork or data mobilisation — but
for stale cells, check national/regional sources first before treating them as true survey
gaps.

### Temporal

**When** the records were collected — the distribution of occurrences by **observation
(event) date, not GBIF publication date.** Switch between log scale (spotting patterns
across orders of magnitude) and linear (comparing absolute numbers), and filter by group.
The sharp rise in recent decades is largely citizen science (Artportalen / iNaturalist).
Two honest reading rules: dips and a falling recent tail mark periods with little digitised
data, and the **current year always looks low because it is still incomplete** — compare
trends using complete years only. For data holders, under‑covered periods are where
digitising historical collections pays off.

### Taxonomic

How much of the **national checklist** has GBIF records — by rank, kingdom, and group.
**This is the one family of tabs measured *relative to* Dyntaxa:** unlike Spatial,
Temporal, Record Types and Publishers (which show all GBIF records with no reference
filter), every completeness and gap figure here asks "of the species Dyntaxa lists, how
many have GBIF records, and how is effort distributed?" The **Troudet sampling‑bias** chart
(after Troudet et al., 2017) shows whether a group is over‑ or under‑represented relative to
its share of known species — 10% of known species but 1% of records means under‑sampled
(red); over‑represented groups show green. Toggle **GBIF‑style groups** (Birds, Mammals,
Insects, Vascular Plants, Fungi…) or standard **Kingdoms**, drill down through the
hierarchy, and exclude dominant groups (e.g. Aves) to see the rest. Completeness uses
species‑rank Dyntaxa taxa as the denominator, excluding microbial kingdoms (Bacteria,
Archaea, Viruses) and *Homo sapiens*; a species counts as "in GBIF" when at least one
occurrence resolves to it.

### Species of Concern

GBIF coverage of the species that most need it, across three sub‑tabs sharing one taxonomy
filter. A species shown as **missing / unmonitored has no GBIF records at all** — the
highest conservation‑data priority.

- **Threatened** — national Red List species in the IUCN categories CR, EN, VU, NT (and DD
  reported separately, not counted as threatened). Missing threatened species can't support
  range modelling, trend analysis, or habitat assessment. Source: SLU Artdatabanken's Red
  List, matched at the rank each list publishes.
- **Invasive** — species GRIIS explicitly flags as invasive for Sweden (not all introduced
  or alien taxa). Because cubes aggregate at species level, this flag is rolled up to
  species: it answers "which species have an invasive form that may warrant monitoring,"
  not whether one specific subspecies is invasive. Missing species are blind spots in
  invasive‑species surveillance.
- **Sensitive** — species whose precise locations GBIF restricts to protect them from
  collection, disturbance, or trade. Their maps use **generalised coordinates** (e.g.
  country centroid or a coarse grid), so spatial gap analysis for them is approximate and
  the true distribution may be far better known than GBIF shows.

*Prioritisation tip (from the app):* start with the Threatened sub‑tab for missing CR/EN
species, then check the Invasive sub‑tab for unmonitored invasives in the same groups.

### Publishers

Which organisations publish Sweden's GBIF data, and how concentrated that publishing is. A
few publishers usually dominate the volume. The **dependency map** flags **single‑publisher
cells** — a 10 km cell whose records all come from one organisation. That's both an
infrastructure vulnerability (if one publisher stops, the cell goes dark) and a partnership
opportunity (a second contributor safeguards it). Use the taxonomic filters to see which
publishers cover which groups.

### Record Types

Occurrences broken down by **basis of record** — human observation, preserved specimen,
material (DNA) sample, machine observation, and so on — with a **last‑12‑months** toggle to
compare recent activity against the all‑time picture per record type, plus spatial,
temporal, and per‑basis species views. This reveals where a whole evidence type is thin:
areas or taxa covered only by observations and not by specimens (or vice versa) are fragile,
and are natural targets for collection digitisation.

### Data & Sources

Every dataset behind the dashboard, resolved to the exact GBIF‑published editions with their
**DOIs** — the provenance travels inside the data bundle. Use this tab to cite sources and
to confirm exactly which edition of each list (Red List, GRIIS, sensitive species, taxonomy)
a given run used.

---

## Controls you'll use everywhere

- **Scope / national‑backbone toggle.** Switch between gap analysis *against the Dyntaxa
  backbone* and a *full‑GBIF overview*, and narrow to sub‑populations: **threatened**,
  **invasive**, **sensitive**, or by **establishment means** (native, introduced, invasive).
  Scope switching is a fast lookup, not a recomputation.
- **Taxonomic cascade filters.** Kingdom → Phylum → Class → Order → Family, applied across
  the relevant tabs. The single most useful move on most tabs is removing Aves.
- **Grid resolution.** Maps and cell metrics use the **10 km** EEA grid (primary) with a
  **50 km** option for a coarser national view. Grids are Europe‑wide (EPSG:3035, ETRS89‑
  LAEA) clipped to Sweden via GADM boundaries; an optional marine (EEZ) mode adds sea cells.
- **Last 12 months.** A recent‑activity window (by observation date) available on Overview,
  Record Types, and the concern tabs — use it to see whether recent effort is closing
  historical gaps or reinforcing them.
- **Log / linear scales** on volume charts, and **CSV download** buttons on the data tables
  and maps.

---

## Reading the numbers honestly

These are the interpretation rules the dashboard is built around. They matter most for
anyone deciding where to spend effort.

- **A gap is a data gap.** Empty or red means *no GBIF records for the current selection* —
  not that nothing lives there or that no one has studied it. Non‑digitised, embargoed, or
  outside‑GBIF data may exist. Always sanity‑check striking gaps against national/regional
  knowledge before acting.
- **Coverage ≠ completeness.** Coverage reflects what has been *published to GBIF*. Apparent
  under‑recording and taxonomic bias are properties of the data pipeline and of collecting
  effort, not necessarily of nature.
- **Two different denominators.** Spatial / Temporal / Record Types / Publishers describe
  *all* GBIF records for Sweden. Taxonomic / Species of Concern describe coverage *of the
  national checklist*. Don't compare a percentage from one family against the other.
- **Birds dominate.** Aves is so heavily recorded it flattens everything else; filter it out
  to see real structure in most other groups.
- **The current year is incomplete.** It will always look like a drop. Compare complete
  years.
- **Stale isn't automatically a gap.** A cell with nothing new in 5–10 years may simply not
  have had its recent data digitised yet — check before scheduling a resurvey.
- **Sensitive‑species maps are approximate** by design (generalised coordinates).
- **Invasive flags are species‑level** rollups from GRIIS, not subspecies‑specific.
- **Threatened = CR/EN/VU/NT.** Data Deficient (DD) is shown separately and not counted as
  threatened.
- **Matching is conservative.** Taxonomic matching to the backbone is deliberately cautious,
  so coverage is more likely to be *under*‑ than over‑stated.

---

## Glossary

- **Occurrence cube** — a GBIF export that pre‑aggregates records into counts per species ×
  10 km cell × month × basis of record, instead of one row per record.
- **Taxonomy backbone** — the national species checklist (Dyntaxa) used as the reference
  against which GBIF coverage is measured.
- **Basis of record** — how a record was made: human observation, preserved specimen,
  material (DNA) sample, machine observation, etc.
- **Establishment means** — whether a taxon is native, introduced, naturalised, or invasive
  in the country.
- **Scope‑filtered summary** — a summary restricted to a subset of taxa (e.g. only
  threatened, invasive, or sensitive species) rather than all of GBIF.
- **Single‑publisher cell** — a 10 km cell whose records all come from one publisher; an
  infrastructure vulnerability and a partnership opportunity.
- **Troudet sampling bias** — a comparison of each group's share of known species against
  its share of GBIF records; negative bias = under‑recorded relative to its richness.
- **Stale cell** — a cell with no GBIF records within the recency window (e.g. the last 5 or
  10 years), measured from the data snapshot date.
- **Observed vs published** — records are counted by when the organism was recorded (event /
  observation date), not by when the dataset was published to GBIF.
- **`in_dyntaxa`** — whether a GBIF species matches the national backbone; gap metrics apply
  only to these.

---

## Data sources

The dashboard integrates five national sources for Sweden (each resolved to a specific DOI,
listed live on the **Data & Sources** tab):

| Source | Role | Sweden edition |
|---|---|---|
| National taxonomy backbone | Reference species pool | [Dyntaxa](https://doi.org/10.15468/j43wfc) |
| National Red List | Threat status (CR, EN, VU, NT, DD) | [Swedish Red List 2025](https://doi.org/10.15468/zbbyqv) |
| Invasive species register | `is_invasive` flag | [GRIIS Sweden](https://doi.org/10.15468/i57bff) |
| Sensitive species list | `is_sensitive` + generalisation category | [Restricted Access Species](https://doi.org/10.15468/jwbtsb) |
| GBIF occurrence cubes | Aggregated occurrences per cell | GBIF SQL API |

Spatial reference: EEA 10 km + 50 km grids; administrative boundaries from GADM.

Under the hood, every GBIF species is matched to the backbone through a 4‑tier
reconciliation (accepted‑name → synonym → infraspecific collapse → GBIF API lookup),
reaching ~99.8% occurrence coverage — so the taxonomic figures are robust to naming
differences.

---

## How to cite

If you use the Gap Finder or its outputs, please cite:

> Thöle, L. *gbif_gap_finder: a reproducible pipeline for biodiversity data gap analysis.*
> Version 0.4.3 (2026‑06‑24). GBIF Sweden, Swedish Museum of Natural History (NRM).
> https://github.com/GBIF-Sweden/gbif_gap_finder

Please also cite the underlying data sources by their DOIs (above / on the Data & Sources
tab). Full machine‑readable citation metadata is in `CITATION.cff` at the repository root.

---

## Contributing data

The most direct way to close a gap you can see here is to publish the relevant records to
GBIF. If you hold occurrence data for Swedish taxa — especially for under‑covered areas, or
for threatened or invasive species shown as missing — GBIF Sweden can help you publish it
(for example via an IPT / Darwin Core Archive). Contact GBIF Sweden at the Swedish Museum of
Natural History, or open an issue on the
[project repository](https://github.com/GBIF-Sweden/gbif_gap_finder).

---

## Getting help & feedback

Questions about a metric or a definition are answered in each tab's **About** expander and
in the Overview **Methods, limitations & glossary** panel. For issues, corrections, or
suggestions, use the project repository. This manual describes the dashboard's content and
interpretation; the repository README covers installation and the analysis pipeline.

*Last updated: 2026‑07‑29.*
