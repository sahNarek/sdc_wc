# Multi-provider architecture

## Rationale

The pipeline separates **provider-specific ingestion** from **provider-agnostic visualization and reporting**.

- Each data vendor (StatsBomb, Wyscout, …) owns its raw layout, parsers, and `build_match_*()` function under `R/providers/{slug}/`.
- All providers emit the same canonical tables (`meta`, `events`, `lineups`, …) via `normalize_match_bundle()` in `R/core/schema.R`.
- Charts in `R/viz/` never reference a provider; reports segment by provider section only.

## Layout

```
data/
├── raw/{provider}/matches/{provider_match_id}/v*/
└── processed/{provider}/wc_matches.rda

R/
├── core/           # paths, schema, registry, build_all, load_match_data
├── providers/      # one folder per vendor
├── viz/            # unchanged chart functions
└── render_report.R

config/
├── matches.yml     # canonical match IDs + cross-provider mapping
└── providers.yml   # enabled flags + section labels
```

## Adding a provider

1. Create `R/providers/{slug}/` with `build_match_{slug}()`.
2. Register in `R/core/registry.R`.
3. Add an entry to `config/providers.yml`.
4. Map IDs in `config/matches.yml` / `game_ids.csv`.

No changes to `R/viz/` or `reports/_provider_sections.Rmd` are required.

## Regression check (StatsBomb)

```bash
Rscript scripts/run_all.R 4036731 html statsbomb --skip-setup
```

Figures are written to `output/figures/{match_id}/statsbomb/`.

Report filenames follow `{Home}_{Away}_{provider}.html|pdf` (e.g. `Germany_Curacao_statsbomb`, `Germany_Curacao_all` when multiple providers are rendered).
