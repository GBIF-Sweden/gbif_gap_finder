# External dependency register + drift audit (2026-09-07)

*Written after the deployed Spatial tab started showing an "API KEY REQUIRED" watermark on every
map. That break and the July CoL backbone break are the same failure mode: **an external dependency
changed under us, silently, and we found out only when something visibly broke**. This document
enumerates every external dependency the project has, how (or whether) it is pinned, and what
happens when it moves. Method: read-only device bridge; repo `~/Desktop/LOCAL/GBIF/GapAnalyses/`
`gbif_gap_finder`, branch `main`, HEAD `b57ed05` (tag `v0.5.0`); patches verified in the cloud.*

## Verdict

**Nine of the twenty-one external dependencies below fail silently** — they degrade a number, a
name, or a map without stopping the build. That is the whole problem: `targets` protects us against
*our* code changing, and `renv.lock` protects us against *packages* changing, but nothing watches
the services and files those packages talk to.

The two breaks we have actually suffered were both in that silent set:

- **CoL backbone (2026-07-27)** — GBIF swapped its backbone to Catalogue of Life. `specieskey` went
  alphanumeric, `/v1/species/{key}/synonyms` 400'd on every key, ~19,865 errors were cached as
  negatives, Tier 4 collapsed 3,179 → 92 and missing-threatened inflated 227 → 374. The pipeline
  ran green throughout.
- **CARTO basemaps (2026-08/09)** — CARTO began requiring an API key for its raster basemap
  endpoint and started retiring it. Unkeyed tiles still render, with a repeating "API KEY REQUIRED"
  watermark. Nothing in this repo changed; the provider did.

Neither was detectable from inside the build. Both would have been caught by a preflight that
*asserts* on the shape of what comes back, which is the primary recommendation below.

---

## The register

**Pinned** = we control the version we get. **Failure** = what a user or the numbers see when it
changes. **Loud** 🔊 = the build stops or shouts; **silent** 🔇 = the build is green and something is
quietly wrong.

### 1. Tile providers and front-end assets

| # | Dependency | Where called | Pinned? | On change | |
|---|---|---|---|---|---|
| D1 | **CARTO raster basemaps** `basemaps.cartocdn.com` (via `providers$CartoDB.Positron`) | `app.R` ×15: 2766, 3351, 4088, 4110, 4241, 4263, 4376, 4397, 4742, 5066, 5078, 5083, 5108, 5123, 5138 | No — provider name resolved by `leaflet.providers` at runtime | **Already broken.** Watermark on every map. Note `CartoDB.PositronNoLabels` is the *same* endpoint and is watermarked too | 🔇 |
| D2 | **`leaflet.providers` provider table** (URL templates + attribution) | `leaflet::providers` | Yes — `leaflet.providers 2.0.0` in `renv.lock`, restored into the image by `renv::restore()` | A provider entry could change URL or gain a key requirement on a package bump | 🔇 |
| D3 | **Esri `World_Light_Gray_Base`** (the incoming default) | `app.R` after the basemap patch | No | Keyless today. **Esri has scheduled retirement for December 2029** | 🔇 |
| D4 | **CDN JS/CSS/fonts in the app** | — | n/a | **None.** `app.R` loads only local `www/styles.css`; no CDN scripts, stylesheets or fonts | ✅ |

D4 is the good news: after D1 is fixed, the tile provider is the app's entire third-party
front-end surface.

### 2. GBIF API surfaces

| # | Dependency | Where called | Pinned? | On change | |
|---|---|---|---|---|---|
| D5 | **`occ_download_sql`** (cube download) | `01a`, SQL rendered by `render_cube_sql` (`R/globals.R:172–200`) | Download **keys** recorded to `provenance/cube_downloads_{CC}.yml`; the SQL *dialect* is not pinned | A dialect or column change fails the download loudly, but a changed *interpretation* (as in July) is silent | 🔇 |
| D6 | **`classificationdetails['<COL key>']`** in the cube SQL | `sql/gbif_occurrence_cube.sql`, `${COL_CHECKLIST_KEY}` | Checklist **key** pinned in all 3 configs; the **CoL release behind that key is not** | A new CoL Extended Release changes keys and names under a stable dataset key | 🔇 |
| D7 | **`/v1/species/{key}/synonyms`** (Tier 4) | `09a:580` | No | This is exactly what broke in July | 🔇 (now partly guarded) |
| D8 | **`/v1/species?datasetKey=&sourceId=`** (CoL taxonID → usage key) | `09a:564` | No | Tier 4 silently yields nothing | 🔇 |
| D9 | **`/v2/species/match`** (crosswalk) | `09a1:280`, bulk path `09a1:191` | No — **v2 is a moving surface** | Tier 5 augment silently stops rescuing its ~162 species | 🔇 |
| D10 | **`/v1/organization/{uuid}`** (publisher names) | `06a:439` | No | Publisher names become `NA` — and are **cached that way** (see D19) | 🔇 |
| D11 | **`/v1/dataset/{key}` + DOI resolution** | `01b:55` `GBIF_API`, `01b:85–152` | No | `01b` already **fails loudly on unresolved cubes** and warns on config-vs-GBIF DOI drift | 🔊 |

D11 is the model the rest should follow — it resolves, compares against config, and shouts on
mismatch.

### 3. Taxonomic backbones and lists

| # | Dependency | Where called | Pinned? | On change | |
|---|---|---|---|---|---|
| D12 | **CoL Extended Release** | key `7ddf754f-…`, hardcoded as the fallback default at `R/globals.R:195` and `09a1:55`, set in all 3 configs | Key yes, **release no** | Silent taxonomic drift across the whole reconciliation | 🔇 |
| D13 | **Dyntaxa DwC-A download** | `configs/config_SE.yml:32` | URL pinned — **and carries a live `subscription-key` in a tracked file** (see F1) | Key revoked or rotated → download fails | 🔊 |
| D14 | **Dyntaxa file layout** — `Taxon.csv`, `VernacularName.csv` | `03:41`, `03:43` (config-overridable names) | Filenames configurable; **columns are not asserted** | Missing `VernacularName` is handled (`03:921`, skips with a message); a renamed *column* is silent | 🔇 |
| D15 | **Dyntaxa `taxonId` LSID scheme** (`urn:lsid:dyntaxa.se:Taxon:219026`) | `03:195–201` `extract_numeric_id()` — regex `(?<=:)[0-9]+$` | No | A scheme change yields `NA` ids across the backbone | 🔇 |
| D16 | **`taxonomy.version`** | `03:82`, default `"1.2"` | A **config string, not a resolved fact** — it is whatever we typed | Records a version we did not verify | 🔇 |
| D17 | **Red list / GRIIS / sensitive DwC-A** + IUCN category vocabulary | `config_SE.yml:39,49,61`; threat lookup `03:442`; sensitivity `03:740–804` | Export URLs pinned; **category vocabulary is not** | A renamed category silently drops species from "threatened" | 🔇 |

### 4. Geospatial sources

| # | Dependency | Where called | Pinned? | On change | |
|---|---|---|---|---|---|
| D18 | **GADM** | `01a:359` `gadm(version = "4.1", resolution = 1)` | **Version hardcoded in code, not config** | Reproducible today; a change needs a code edit | 🔊-ish |
| D19 | **EEA reference grids** 10 km / 50 km | manual download (`01a:134–135`), local `.gpkg` (`02:29`) | **Effectively pinned by the file on disk** — the strongest pinning in the project | Nothing changes until someone replaces the file | ✅ |
| D20 | **Marine Regions EEZ** via `mregions2::mrp_get` | `02:182–208` | No — live download, cached to `.gpkg`; local override `marine.eez_file` | Already **aborts with a readable message** on failure; the *cached* file then silently ages | 🔊 then 🔇 |

### 5. Environment and CI

| # | Dependency | Where | Pinned? | On change | |
|---|---|---|---|---|---|
| D21 | **`renv.lock` ↔ image** | `Dockerfile.gap_finder` | **Consistent by construction** — the image is built by `renv::restore(lockfile = 'renv.lock')`, so container packages *are* the lockfile. No drift is possible on this path | — | ✅ |
| D22 | **Base image `rocker/r-ver:4.5.2`** | `Dockerfile.gap_finder` | Tag-pinned, **not digest-pinned** | Tag can be re-pushed; rebuild silently differs | 🔇 |
| D23 | **apt packages** | `Dockerfile.gap_finder` | Unpinned (`apt-get install` = latest) | GDAL/GEOS/PROJ can move under `sf`/`terra` between rebuilds | 🔇 |
| D24 | **CI actions** (checkout, setup-buildx, login, metadata, build-push) | both workflows | Major-pinned to the Node 24 releases as of 2026-09-08; **not SHA-pinned** | **Hard deadline hit once already:** GitHub removes Node 20 from the runners on **2026-09-23**, and all five majors previously in use declared `using: node20`. Bumped 2026-09-08. A re-pushed major tag can still change under us | 🔊 then 🔇 |

**On the leaflet.providers hypothesis:** it does not hold, and the mechanism is worth recording so
nobody re-tests it. The Dockerfile restores site-library straight from `renv.lock`, so the running
container has exactly the pinned `leaflet.providers 2.0.0` / `leaflet 2.2.3`. The basemap break is
provider-side, not package-side.

### 6. Caches

| # | Cache | Negative results? | Expiry | |
|---|---|---|---|---|
| D25 | `publisher_name_cache.rds` (`06a:422`) | **Yes — `NA_character_` written on failure at `06a:447` and `06a:450`** | **None** | 🔇 |
| D26 | `col_synonym_cache.rds` (`09a`) | Transport errors correctly **not** cached (`09a:96–101`); genuine empties and 400s are | **None** | 🔇 |
| D27 | `col_crosswalk_cache.rds` (`09a1`) | Same design as D26 | **None** | 🔇 |
| D28 | `data/NO/proc/gbif_name_cache.rds` | The **old poisoned cache**, still on disk for NO | — | 🔇 |

---

## Findings

Severity: 🔴 act now · 🟠 will bite · 🟡 worth doing · ⚪ hygiene.

| # | Sev | Finding |
|---|-----|---------|
| **F1** | 🔴 | **A live API secret is committed.** `configs/config_SE.yml:32` carries `subscription-key=4b068709e7f2427d9fc76bf42d8e2b57` inside the Dyntaxa export URL, in a git-tracked file. Two problems at once: a credential in history, and a dependency that dies silently when it is rotated. **Rotate the key, move it to `Renviron` as `DYNTAXA_SUBSCRIPTION_KEY`, and interpolate it at read time.** Note that rotating does not remove it from git history. |
| **F2** | 🔴 | **The basemap is broken in production** (D1). Fixed by `gap_finder_basemap.patch`. |
| **F3** | 🟠 | **`publisher_name_cache.rds` caches failures forever** (D25). This is the Tier-4 trap in a quieter register: one bad API run permanently pins a set of publishers to "unknown", and nothing ever retries. A single-run outage becomes a permanent data defect. |
| **F4** | 🟠 | **No cache is expirable at all** (D25–D27). Even the well-behaved caches have no TTL and no "rebuild from scratch" switch other than deleting files by hand. |
| **F5** | 🟠 | **We pin keys, not versions** (D6, D12, D16). The CoL checklist key is pinned in three places, but the CoL *release* behind it is neither pinned nor recorded, so a new release is invisible in the diff. Same for `taxonomy.version`, which is a string we typed rather than a fact we resolved. |
| **F6** | 🟠 | **GADM version is hardcoded in code** (`01a:359`), not config — inconsistent with every other source, and invisible to the Norway port. |
| **F7** | 🟡 | **The `/v2/species/match` surface is a moving target** (D9) and Tier 5 fails silently to zero rescues rather than erroring. |
| **F8** | 🟡 | **Vocabulary changes are unguarded** (D17). A renamed IUCN category or sensitivity level silently shrinks a count with no schema violation. |
| **F9** | 🟡 | **Dyntaxa LSID parsing is an unasserted regex** (D15). A scheme change produces `NA` ids, not an error. |
| **F10** | ⚪ | **Base image and apt packages are not digest-pinned** (D22, D23) — GDAL/GEOS/PROJ can move under `sf`/`terra` between rebuilds of the "same" image. |
| **F12** | 🟠 | **The CI actions carried a dated kill switch and nothing was watching it.** Node 20 leaves the GitHub runners on **2026-09-23**; all five actions in both workflows ran on it. Surfaced only as a yellow annotation on the v0.5.1 run, 15 days before both workflows would have stopped building images and syncing `CITATION.cff`. Majors bumped 2026-09-08. **This is the register's own failure mode, caught by luck rather than by the preflight** — H-1 should assert on CI action runtimes too, or Dependabot should own them. |
| **F11** | ⚪ | **The stale NO cache** `data/NO/proc/gbif_name_cache.rds` (D28) should be deleted before the Norway port runs. |

---

## Hardening, cheapest first

### H-1 — `scripts/00_preflight.R` (the main recommendation)

One script that pings every live dependency and **asserts on the shape of the answer**, not merely
on a 200. It is the piece that would have caught both historical breaks.

- Runnable standalone: `Rscript scripts/00_preflight.R`, and via `run_preflight()` in `run.R`.
- Wired as `tar_target(preflight)` ahead of `raw_data`, so a full `tar_make()` fails at the front.
- **Fails loudly with a readable message**: one line per dependency, `OK` / `WARN` / `FAIL`, a
  summary table, and a non-zero exit on any FAIL.
- `--offline` skips network checks so it can run in CI.

The assertions that matter — each one maps to a break we have had or nearly had:

| Check | Asserts | Would have caught |
|---|---|---|
| Basemap tile | `HTTP 200` **and** `content-type: image/*` **and** plausible byte size for the configured provider | D1/D3 (a watermarked tile is still 200 — size and provider identity are the tell) |
| CoL checklist | `/v1/dataset/{col_key}` resolves; **record the release version/date** | D6, D12 |
| Tier-4 round trip | one known taxonID → usage key → synonyms returns ≥1 row | The July break, exactly |
| `/v2/species/match` | one known name returns a match with the expected fields | D9 |
| Dyntaxa | archive URL reachable; `Taxon.csv` present; **required columns present**; LSID regex matches a sample | D13, D14, D15 |
| Red list | archive reachable; **category vocabulary is a subset of the expected set** | D17 |
| Marine / EEZ | `mregions2` installed and the service answers, or `marine.eez_file` exists | D20 |
| Grids | both `.gpkg` present and readable | D19 |
| Caches | age and negative-entry count per cache, warn past a threshold | D25–D28 |

### H-2 — Resolved versions into provenance at run time

Drift should show up as a **diff**, not a surprise. Extend `data_sources_meta` (`01b:229–239`) with
a `resolved` block written on every run and committed:

```
resolved:
  col_release:        <version + date from /v1/dataset/{col_key}>
  dyntaxa_published:  <pubDate from the DwC-A EML>
  redlist_published:  <pubDate>
  gadm_version:       "4.1"
  basemap_provider:   "Esri.WorldGrayCanvas"
  gbif_api_v1/v2:     <reachable + sampled at>
  resolved_at:        <timestamp>
```

Because `provenance/` is version-controlled, a CoL release bump becomes a one-line diff in a commit
instead of a number that moved for no visible reason. This is the piece that turns silent into
loud without adding a single new alert.

### H-3 — Cache hygiene

- Give every cache a TTL (`parameters.cache.max_age_days`, default ~30) and a `force_refresh` flag.
- **Stop caching failures in `06a`** — leave failed publisher lookups uncached so they retry, exactly
  as `09a:96–101` already does for transport errors.
- Have the preflight report cache age and negative-entry counts.

### H-4 — Pin what is unpinned, config what is hardcoded

| Item | Now | Should be |
|---|---|---|
| GADM version/resolution | hardcoded `01a:359` | `parameters.spatial.gadm_version` / `_resolution` |
| Dyntaxa subscription key | in a tracked config URL | `Renviron`, interpolated at read time |
| Basemap provider | hardcoded ×15 in `app.R` | `GAP_FINDER_BASEMAP` env, one helper *(shipped)* |
| CoL checklist key fallback | duplicated at `globals.R:195` + `09a1:55` | one shared constant; configs stay authoritative |
| Base image | `rocker/r-ver:4.5.2` | `@sha256:…` digest |
| CI actions | `@v4` / `@v5` / `@v6` | commit SHAs |
| `taxonomy.version` | typed string `03:82` | resolved from the archive EML, config as fallback |

---

## Related

- `claude/finding-specieskey-type-2026-07-27.md` — the CoL backbone break in full.
- `audit/housekeeping-2026-07-30.md` — internal wiring audit (this one is its external counterpart).
- Baseline that must not regress: matched **76.6%** · Tier-4 **3,459** · missing-threatened **119** ·
  occ **99.75%**.
