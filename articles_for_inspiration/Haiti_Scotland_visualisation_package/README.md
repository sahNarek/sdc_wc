# Haiti vs Scotland Visualisation Package

This package contains the reproducible Python code and minimum data needed to recreate:

- The cumulative xG timeline.
- Scotland's decisive goal-sequence map.
- The six-slide Instagram carousel.

## Package Contents

```text
analysis/
  Haiti_vs_Scotland_visualisations.ipynb
  generate_instagram_carousel.py
  requirements.txt
  visual_data/
    viz_1_xg_timeline.csv
    viz_2_goal_sequence.csv
    supporting_match_metrics.csv
  fonts/
    BarlowCondensed-Bold.ttf
    OpenSans-Variable.ttf
    OFL-BarlowCondensed.txt
    OFL-OpenSans.txt
  instagram_assets/
    flag_haiti.png
    flag_scotland.png
.reference_materials/
  brand_assets/
    Identidad-Mundial-26-SDC_Vertical-Redes.jpg
examples/
  article_charts/
  instagram_carousel/
```

## Installation

Python 3.10 or newer is recommended.

```bash
python -m venv .venv
```

Windows:

```bash
.venv\Scripts\activate
pip install -r analysis/requirements.txt
```

macOS or Linux:

```bash
source .venv/bin/activate
pip install -r analysis/requirements.txt
```

## Recreate the Article Charts

Open this notebook from the package root:

```text
analysis/Haiti_vs_Scotland_visualisations.ipynb
```

Run all cells. The generated images will be written to:

```text
analysis/visuals_python/
```

The notebook automatically finds the included CSV and font files.

## Recreate the Instagram Carousel

Run this command from the package root:

```bash
python analysis/generate_instagram_carousel.py
```

The six slides and contact sheet will be written to:

```text
analysis/instagram_carousel/
```

Each slide is exported at `864 x 1080` pixels.

## Adapting the Code to Another Match

The visualisation code expects the same columns as the included CSV files.

### xG timeline

`viz_1_xg_timeline.csv` contains one row per shot, including:

- `match_minute`
- `team`
- `player`
- `xg`
- `cumulative_xg`
- `outcome`
- `start_x`, `start_y`
- `is_goal`

### Goal sequence

`viz_2_goal_sequence.csv` contains the ordered events in the selected attacking move:

- `sequence_order`
- `event_type`
- `player`
- `recipient`
- `start_x`, `start_y`
- `end_x`, `end_y`
- `xg`
- `outcome`

### Supporting metrics

`supporting_match_metrics.csv` contains the summary values used in the carousel.

For a new match, create CSV files with the same structure and update the titles, team names, colors and narrative annotations in the Python code.

## Main Libraries

- Pandas for data preparation.
- NumPy for numerical operations.
- Matplotlib for all chart construction.
- Pillow for image processing and compression.

## Typography and Assets

- Chart titles: Barlow Condensed Bold.
- Body text: Open Sans.
- The included fonts use the SIL Open Font License.
- The social-media template belongs to Sports Data Campus and should only be used according to the project's publication guidelines.
- Flag assets are sourced from Wikimedia Commons.

## Important Note

The included CSV files are small derived datasets prepared for these visualisations. When adapting the workflow, use match data that your team is authorised to access and share.

