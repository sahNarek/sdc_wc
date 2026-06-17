# SDC World Cup 2026 — Match Analysis Pipeline

R project for **El Mundial de los Datos** (Sports Data Campus). It reads StatsBomb-style JSON match files, builds analysis-ready dataframes with correct team and player names, and produces reusable, article-grade visualizations for match reports.

**Development match:** Germany 7–1 Curaçao (`4036731`)  
**Assigned matches (pending data):** Argentina vs Algeria, Portugal vs Uzbekistan, Algeria vs Austria

---

## Table of contents

1. [System overview](#system-overview)
2. [Prerequisites](#prerequisites)
3. [Local setup](#local-setup)
4. [Project structure](#project-structure)
5. [Quick start](#quick-start)
6. [Workflow step by step](#workflow-step-by-step)
7. [Adding new match data](#adding-new-match-data)
8. [Configuration](#configuration)
9. [Visualizations](#visualizations)
10. [Styling & export rules](#styling--export-rules)
11. [Troubleshooting](#troubleshooting)
12. [Roadmap](#roadmap)

---

## System overview

The pipeline has three layers:

```
JSON files (data/raw/{provider}/ or legacy data_sample/)
        ↓
  R provider layer (R/providers/{slug}/) → parse, map names
        ↓
  Canonical schema (R/core/schema.R)
        ↓
  wc_matches.rda (data/processed/{provider}/)
        ↓
  R viz layer (R/viz/*.R)       → ggplot charts (UC1–UC8)
        ↓
  R Markdown reports (reports/) → HTML/PDF + PNG exports (output/)
```

| Step | What happens |
|------|----------------|
| **Ingest** | JSON is read per provider from `data/raw/{provider}/matches/{id}/v*/` (legacy `data_sample/` still works for StatsBomb). |
| **Parse** | Provider-specific parsers flatten events; `game_ids.csv` supplies display names. |
| **Store** | Combined objects are saved to `data/processed/{provider}/wc_matches.rda`. |
| **Visualize** | Generic functions in `R/viz/` take a `match_id` and return ggplot objects (no provider names in chart text). |
| **Publish** | `02_match_report_template.Rmd` knits per-provider report sections and saves PNGs to `output/figures/{match_id}/{provider}/`. |

---

## Prerequisites

### Required software

| Requirement | Version | Notes |
|-------------|---------|--------|
| **R** | ≥ 4.2 | [CRAN](https://cran.r-project.org/) or [rig](https://github.com/r-lib/rig) on macOS |
| **Internet** | — | First run installs CRAN packages, Google Fonts, and (optionally) TinyTeX |

### Recommended (optional)

| Tool | Purpose |
|------|---------|
| **RStudio** | Open `sdc_wc.Rproj` for interactive work |
| **pandoc** | Usually bundled with RStudio / `rmarkdown`; needed for HTML/PDF |
| **TinyTeX** | PDF reports — installed automatically via `setup_local.R --pdf` |

### System libraries (OS package manager)

Needed for shot-map icons (`magick`, `rsvg`). The setup script checks for these; install if icon rendering fails.

**macOS (Homebrew):**

```bash
brew install imagemagick librsvg
```

**Ubuntu / Debian:**

```bash
sudo apt-get update
sudo apt-get install -y libmagick++-dev librsvg2-dev libcurl4-openssl-dev libssl-dev libxml2-dev
```

**Windows:** Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and [ImageMagick](https://imagemagick.org/script/download.php#windows); `magick` and `rsvg` CRAN binaries usually work without extra steps.

### R packages (installed automatically)

Core: `tidyverse`, `jsonlite`, `yaml`, `scales`, `grid`, `showtext`, `sysfonts`, `rmarkdown`

Visualizations: `ggimage`, `rsvg`, `patchwork`, `magick`

PDF: `tinytex` (optional, via `--pdf` flag)

### Match data (not in git)

You must provide raw match JSON locally (StatsBomb under `data/raw/statsbomb/` or legacy `data_sample/`). See [Adding new match data](#adding-new-match-data).

---

## Local setup

Run once on a new machine (or after cloning):

```bash
cd sdc_wc

# Install R packages, create folders, rasterise shot icons
Rscript scripts/setup_local.R

# Include TinyTeX if you need PDF reports
Rscript scripts/setup_local.R --pdf

# Verify packages only (no installs)
Rscript scripts/setup_local.R --check
```

**Full pipeline** (setup → build → report) in one command:

```bash
Rscript scripts/run_all.R                         # match 4036731, HTML, statsbomb
Rscript scripts/run_all.R 4036731 both statsbomb  # HTML + PDF (needs --pdf setup)
Rscript scripts/run_all.R 4036731 html all        # all providers with data
Rscript scripts/run_all.R 4036731 html statsbomb --skip-setup
```

---

## Project structure

```
sdc_wc/
├── config/
│   ├── matches.yml              # Match IDs + cross-provider mapping
│   └── providers.yml            # Provider labels and enabled flags
├── data/
│   ├── raw/
│   │   ├── statsbomb/matches/   # StatsBomb JSON (preferred)
│   │   └── wyscout/             # Wyscout raw data (future)
│   └── processed/
│       └── {provider}/wc_matches.rda
├── data_sample/                 # Legacy StatsBomb path (still supported)
├── R/
│   ├── core/                    # paths, schema, registry, build_all
│   ├── providers/
│   │   ├── statsbomb/           # StatsBomb ingestion
│   │   └── wyscout/             # Wyscout scaffold
│   ├── viz/                     # Reusable chart functions
│   └── render_report.R
├── reports/
│   ├── _setup.R
│   ├── _provider_sections.Rmd   # UC blocks (included per provider)
│   ├── 01_build_data.Rmd
│   └── 02_match_report_template.Rmd
├── scripts/
│   ├── setup_local.R
│   ├── run_all.R                # setup + build + report
│   ├── run_build.R              # CLI: build data (provider arg)
│   └── run_report.R             # CLI: knit report (format + providers)
```

---

## Quick start

### 1. Clone the repo and add data locally

Raw JSON is **not** in git. Copy your `data_sample/` folder into the project root (see [Adding new match data](#adding-new-match-data)).

### 2. Set up the environment

```bash
cd sdc_wc
Rscript scripts/setup_local.R --pdf
```

### 3. Build the dataset

```bash
Rscript scripts/run_build.R
```

### 4. Generate report and figures

```bash
Rscript scripts/run_build.R statsbomb
Rscript scripts/run_report.R 4036731 html statsbomb
Rscript scripts/run_report.R 4036731 both statsbomb,wyscout
Rscript scripts/run_report.R 4036731 html all
```

Or use the all-in-one script:

```bash
Rscript scripts/run_all.R 4036731 both statsbomb
```

Individual steps:

```bash
# HTML (default provider: statsbomb)
Rscript scripts/run_report.R

# PDF (installs TinyTeX on first run)
Rscript scripts/run_report.R 4036731 pdf statsbomb

# Both HTML and PDF
Rscript scripts/run_report.R 4036731 both statsbomb
```

### 5. Open outputs

- HTML report: `output/reports/Germany_Curacao_statsbomb.html` (teams + provider suffix)
- PDF report: `output/reports/Germany_Curacao_statsbomb.pdf`
- Figures: `output/figures/4036731/statsbomb/`

### From RStudio

```r
source("reports/_setup.R")
load_project()

# Build data
wc_matches <- build_all_matches()

# Inspect
wc_matches$meta
compute_team_shots_goals(wc_matches$events, match_id = 4036731)

# Knit report
rmarkdown::render(
  "reports/02_match_report_template.Rmd",
  output_dir = "output/reports",
  params = list(match_id = 4036731)
)

# PDF export (same content as HTML)
render_match_report(match_id = 4036731, format = "pdf")
render_match_report(match_id = 4036731, format = "both")
```

---

## Workflow step by step

### Step 1 — Load the project

Always start a session with:

```r
source("reports/_setup.R")
load_project()
```

This loads packages, registers fonts (Barlow Condensed + Open Sans), and sources all `R/` and `R/viz/` scripts.

### Step 2 — Build `wc_matches.rda`

```r
wc_matches <- build_all_matches(provider = "statsbomb")
# Or explicit IDs:
wc_matches <- build_all_matches(match_ids = c(4036731, 4036724), provider = "statsbomb")
```

The saved object contains canonical tables (all providers use the same structure):

| Object | Description |
|--------|-------------|
| `meta` | One row per match (`match_id`, `provider`, `provider_match_id`, teams, score, …) |
| `events` | Flattened event data with viz-layer column aliases |
| `lineups` | Long-format lineups |
| `players` | Player lookup with `player_display_name` |
| `teams` | Team lookup |
| `player_match_stats` | Per-player match metrics |
| `team_match_stats` | Per-team match metrics |
| `provider` | Provider slug for this `.rda` file |
| `config` | Contents of `config/matches.yml` |

Reload without rebuilding:

```r
load("data/processed/statsbomb/wc_matches.rda")
# or
data <- load_match_data("statsbomb", 4036731)
```

### Step 3 — Create individual charts

```r
events <- wc_matches$events %>% filter(match_id == 4036731)
pms    <- wc_matches$player_match_stats %>% filter(match_id == 4036731)

p <- viz_defensive_heatmap(events, match_id = 4036731)
print(p)

save_figure(
  p,
  "output/figures/4036731/statsbomb/Defensive_Heatmap_Germany_vs_Curacao.png",
  format = "16_9"
)
```

### Step 4 — Knit the full match report

**HTML:**

```r
rmarkdown::render(
  "reports/02_match_report_template.Rmd",
  output_dir = "output/reports",
  output_format = "html_document",
  params = list(match_id = 4036731, providers = c("statsbomb"))
)
```

**PDF** (all charts embedded, same sections as HTML):

```r
render_match_report(match_id = 4036731, format = "pdf", providers = "statsbomb")
# or both formats at once:
render_match_report(match_id = 4036731, format = "both", providers = "all")
```

From the shell:

```bash
Rscript scripts/run_report.R 4036731 pdf statsbomb
Rscript scripts/run_report.R 4036731 both all
```

PDF export uses **TinyTeX** (installed automatically on first PDF render). Requires an internet connection once for the LaTeX setup.

---

## Adding new match data

### Where files go

Place downloaded JSON under (preferred):

```
data/raw/statsbomb/matches/{STATS_BOMB_MATCH_ID}/v1/
├── events.json                 # required
├── lineups.json                # required
├── player_match_stats.json     # required
├── team_match_stats.json       # required
└── 360_frames.json             # optional
```

Legacy path (still supported):

```
data_sample/matches/{STATS_BOMB_MATCH_ID}/v1/
```

If updated data arrives in `v2/`, `v3/`, etc., drop files there — the pipeline **automatically uses the highest version** that contains each file.

Optional season-level data (already in sample):

```
data_sample/seasons/43_316/matches/v*/matches.json
data_sample/seasons/43_316/player_season_stats/...
data_sample/seasons/43_316/team_season_stats/...
```

### Step-by-step: add a new match

**1. Obtain JSON** from SDC / StatsBomb export for your match.

**2. Create the folder** (example: Argentina vs Algeria, ID `4036737`):

```bash
mkdir -p data_sample/matches/4036737/v1
# Copy JSON files into v1/
```

**3. Verify required files exist:**

```bash
ls data_sample/matches/4036737/v1/
# events.json  lineups.json  player_match_stats.json  team_match_stats.json
```

**4. Update `config/matches.yml`:**

```yaml
development:
  primary_match_id: 4036737        # switch when analysing this match
  primary_label: "Argentina vs Algeria"
  sample_match_ids:
    - 4036731                      # keep previous matches if needed
    - 4036737                      # add new ID

assigned:
  - id: WC26-019
    statsbomb_id: 4036737
    label: "Argentina vs Algeria"
    status: available              # change from pending when data exists
```

**5. (Optional) Confirm the match in `game_ids.csv`** — the `Statsbomb ID` column is used for Spanish display names (`pais_1`, `pais_2`, stadium, etc.).

**6. Rebuild and test:**

```bash
Rscript scripts/run_build.R
Rscript scripts/run_report.R 4036737
```

**7. In R, sanity-check:**

```r
load_project()
match_data_available(4036737)   # should return TRUE
build_match(4036737)$meta
```

### Adding multiple matches at once

```r
load_project()
all_ids <- list_available_match_ids()   # scans data_sample/matches/
wc_matches <- build_all_matches(match_ids = all_ids)
```

Or list explicit IDs in `config/matches.yml` under `sample_match_ids`.

### What not to commit

`data_sample/`, `data/processed/`, and `output/` are in `.gitignore`. Share raw JSON through SDC channels, not git. Only code, config, and documentation belong in the repository.

---

## Configuration

`config/matches.yml` controls which matches are built:

```yaml
development:
  primary_match_id: 4036731
  primary_label: "Germany vs Curacao"
  sample_match_ids:
    - 4036731

assigned:
  - id: WC26-019
    statsbomb_id: 4036737
    label: "Argentina vs Algeria"
    status: pending
```

- `sample_match_ids` — IDs passed to `build_all_matches()` by default (via `scripts/run_build.R`).
- `assigned` — your tournament matches; set `status: available` when JSON is on disk.
- Missing IDs are skipped with a message, not an error.

### Chart titles and featured players

Report params in `reports/02_match_report_template.Rmd` control titles per match:

```yaml
params:
  match_id: 4036737
  chart_titles:
    shots_per90: "Disparos por 90 minutos"
    shot_map_left_foot_suffix: "mapa de tiros (pie izquierdo)"
  featured_icon_player: "Lionel Messi"
  featured_left_foot_player: null   # null = auto-pick top left-foot shooter
```

Defaults are built from match metadata via `default_chart_titles(meta)` in `R/viz/viz_theme.R`. Every `viz_*()` function accepts `title` and `subtitle`; player charts also accept `title_suffix`.

---

## Visualizations

Functions follow **Working-with-R.pdf** (StatsBomb guide). All accept `events_df` and optional `match_id`.

| # | Function | Description |
|---|----------|-------------|
| 1 | `viz_team_shots_goals()` | Shots by team (goals labelled on bars) |
| 2 | `viz_team_shots_bar()` | Horizontal shots bar chart |
| 3 | `viz_player_shots_per90()` | Top shooters per 90 minutes |
| 4 | `viz_player_pass_map()` | Completed passes into the penalty area |
| 5 | `viz_xg_xga_contribution()` | Stacked non-penalty xG + xG assisted per 90 |
| 6 | `viz_defensive_heatmap()` | Zone heatmap — single-hue gradient; `heat_color` configurable |
| 7 | `viz_shot_map()` | Shot map (ggplot shapes) — `shot_color` configurable |
| 7b | `viz_shot_map_icons()` | Shot map (footprint/head SVG icons) — `shot_color` configurable |
| 8 | `viz_shot_map_goal_net()` | Shot map + goal-mouth panel with trajectories and xG labels |

Reports export **six palette variants** per single-hue chart (Blue, Orange, Green, Red, Purple, Cyan), e.g. `Shot_Map_Goal_Net_Musiala_Green.png`.

Helper functions: `compute_*()` variants return dataframes; `top_goal_scorer()`, `top_left_foot_shooter()`, `iterate_sdc_palette()`, `save_figure()`, `figure_slug()`.

---

## Styling & export rules

Follows **Style Guide for Articles and Social Media**:

| Element | Font |
|---------|------|
| Chart titles | Barlow Condensed Bold |
| Axis labels, legends, captions | Open Sans Regular |

**Colour palette** (mandatory for graphics):

| Colour | Hex |
|--------|-----|
| Blue | `#1F77B4` |
| Orange | `#FF7F0E` |
| Green | `#2CA02C` |
| Red | `#D62728` |
| Purple | `#9467BD` |
| Cyan | `#17BECF` |

Heatmaps and shot maps use a **single-hue gradient** (light tint → base colour). Pass any SDC palette hex via `heat_color` / `shot_color`, or use `iterate_sdc_palette()` to export all six variants.

**Export dimensions** (`save_figure(..., format = ...)`):

| Format | Pixels | Max size |
|--------|--------|----------|
| `"16_9"` | 1280 × 750 | 500 KB |
| `"4_5"` | 864 × 1080 | 500 KB |
| `"1_1"` | 800 × 800 | 500 KB |

File names use descriptive slugs, e.g. `Defensive_Heatmap_Germany_vs_Curacao.png`.

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| `No event data found for match X` | Check `data_sample/matches/X/v1/events.json` exists. |
| `No data for match X` in report | Run `build_all_matches()` and ensure ID is in `sample_match_ids`. |
| Fonts look wrong | Run `load_project()` — needs internet once for Google Fonts. |
| Heatmap is solid colour | Ensure you are on latest `R/viz/viz_use_case_06.R` (uses `geom_rect`, not `geom_bin2d`). |
| Package missing | `Rscript scripts/setup_local.R` |
| PDF render fails | `Rscript scripts/setup_local.R --pdf`, then retry report |
| Icon shot map broken | Install ImageMagick + librsvg (see [Prerequisites](#prerequisites)); run `setup_local.R` |

For JSON field definitions see `docs/data_dictionary.md`.

---

## Roadmap

- **Docker** — containerised pipeline with mounted `data_sample/`, env-based credentials, and one-command reports. See [`docs/TODO_DOCKER.md`](docs/TODO_DOCKER.md).

---

## References

- `Working-with-R.pdf` — StatsBomb R visualization patterns
- `Style Guide for Articles and Social Media.pdf` — SDC typography, palette, image specs
- `game_ids.csv` — full World Cup schedule and StatsBomb IDs
