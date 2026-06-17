# Wyscout data dictionary (all_data bundle)

## Source layout

Local bundle path (gitignored):

```
all_data /                          # note: may be named "all_data " with trailing space
├── WYSCOUT/
│   ├── gold/matches/{wyscout_id}/v*/match.csv   # player-level match file (primary)
│   ├── raw/matches/{wyscout_id}/v*/match.json   # match metadata
│   ├── raw/matches/{wyscout_id}/v*/formations.json
│   ├── raw/matches/{wyscout_id}/v*/directions.json
│   └── gold/seasons/{season_id}/*.csv           # season aggregates
└── STATSBOMB/raw/...                            # optional StatsBomb mirror
```

**Development match:** Wyscout `5827925` ↔ StatsBomb `4036731` (Germany 7–1 Curaçao)

## Gold `match.csv` columns

| Column | Use |
|--------|-----|
| `match_id` | Wyscout match ID |
| `home_team_name`, `away_team_name` | Team labels |
| `home_score_ft`, `away_score_ft` | Full-time score |
| `team_id`, `team_name`, `player_id`, `player_shortname` | Lineups |
| `minutes_played`, `goals`, `assists` | Player match stats |
| `tactical_position`, `shirt_number`, `is_starter` | Lineup detail |

## Event stream gap

The current `all_data` Wyscout bundle **does not include** per-event JSON (no shots, passes, or coordinates). Therefore:

| UC | Status |
|----|--------|
| UC1–UC8 (event charts) | **Not available** — skipped in reports |
| UC-W1 `viz_wyscout_goals_assists()` | Goals + assists bar chart |
| UC-W2 `viz_wyscout_minutes_played()` | Minutes bar chart |

When Wyscout event exports are added under `raw/matches/{id}/v*/events.json`, extend `R/providers/wyscout/parse_events.R` to populate the canonical `events` table and UC1–UC8 will render automatically.

## Canonical mapping

| Canonical table | Wyscout source |
|-----------------|----------------|
| `meta` | `match.csv` scores + `game_ids.csv` display names |
| `lineups` | `match.csv` player rows |
| `player_match_stats` | `match.csv` minutes, goals, assists |
| `team_match_stats` | derived from first `match.csv` row (home/away) |
| `events` | empty (schema-compatible tibble) |

## ID resolution

- **Canonical `match_id`** in processed `.rda`: StatsBomb ID (`4036731`)
- **`provider_match_id`**: Wyscout ID (`5827925`) from `game_ids.csv` → `Wyscout ID`

## Python / notebook scripts in all_data

| Path | Purpose |
|------|---------|
| `SCRIPTS/templates/02-disparos-euro-2020.ipynb` | StatsBomb shot viz template (reference styling) |
| `SCRIPTS/templates/03-pases-progresivos-euro-2020.ipynb` | Progressive passes template |
| `SCRIPTS/Post/scripts/*.ipynb` | Blog post generation (not ingestion) |

No Wyscout-specific ETL scripts were found; ingestion is implemented directly from the gold CSV layout above.

## Build commands

```bash
Rscript scripts/run_build.R wyscout
Rscript scripts/run_report.R 4036731 html all
```

Processed output: `data/processed/wyscout/wc_matches.rda`
