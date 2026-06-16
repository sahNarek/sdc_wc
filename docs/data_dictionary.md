# Data Dictionary (worked example: Germany vs Curaçao, `4036731`)

## Directory layout

```
data_sample/
├── competitions/v*/competitions.json
├── seasons/43_316/
│   ├── matches/v*/matches.json
│   ├── player_season_stats/v*/
│   └── team_season_stats/v*/
└── matches/{match_id}/v*/
    ├── events.json
    ├── 360_frames.json
    ├── lineups.json
    ├── player_match_stats.json
    └── team_match_stats.json
```

**Version rule:** use the highest `vN` folder where the requested file exists.

## Match files (`4036731`)

| File | Description | Join key |
|------|-------------|----------|
| `events.json` | Array of event objects | `id` (UUID), `match_id` (added on parse) |
| `lineups.json` | Per-team squads with positions | `team_id`, `player_id` |
| `player_match_stats.json` | Aggregated player metrics | `player_id`, `team_id`, `match_id` |
| `team_match_stats.json` | Aggregated team metrics | `team_id`, `match_id` |
| `360_frames.json` | Freeze frames linked to shots | `event_uuid` → `events.id` |

## Events schema (flattened)

| Column | Source JSON path |
|--------|------------------|
| `type_name` | `type.name` |
| `team_id` / `team_name` | `team.id` / `team.name` |
| `player_id` / `player_name` | `player.id` / `player.name` |
| `location_x/y` | `location[1]`, `location[2]` |
| `shot_statsbomb_xg` | `shot.statsbomb_xg` |
| `shot_outcome_name` | `shot.outcome.name` |
| `shot_key_pass_id` | `shot.key_pass_id` |
| `pass_end_location_x/y` | `pass.end_location` |
| `pass_outcome_name` | `pass.outcome.name` (NA = completed) |

StatsBombR-style aliases (`type.name`, `location.x`, etc.) are added by `add_statsbomb_aliases()`.

## Name mapping

1. **Canonical players:** `lineups.json` → `player_name`, `player_nickname`
2. **Display name:** nickname if present, else full name
3. **Publication labels:** `game_ids.csv` → `pais_1`, `pais_2` (e.g. Alemania, Curazao)

## Processed object (`wc_matches.rda`)

| Object | Description |
|--------|-------------|
| `meta` | One row per match (teams, score, stadium) |
| `events` | All flattened events |
| `lineups` | Long-format lineups |
| `players` | Player lookup with `player_display_name` |
| `teams` | Team lookup |
| `player_match_stats` | Raw player match stats |
| `team_match_stats` | Raw team match stats |

## Germany vs Curaçao validation

- Score: Germany 7–1 Curaçao (`team_match_goals` in `team_match_stats.json`)
- Teams: `Germany` (id 770), `Curaçao` (id 4886)
- Event count: ~3,000+ events per match
