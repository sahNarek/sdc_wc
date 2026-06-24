from pathlib import Path
import textwrap

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager
from matplotlib.patches import Arc, Circle, FancyArrowPatch, FancyBboxPatch, Rectangle
from PIL import Image, ImageOps


ROOT = Path(__file__).resolve().parents[1]
ANALYSIS_DIR = ROOT / "analysis"
DATA_DIR = ANALYSIS_DIR / "visual_data"
OUTPUT_DIR = ANALYSIS_DIR / "instagram_carousel"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

TEMPLATE_PATH = (
    ROOT
    / ".reference_materials"
    / "brand_assets"
    / "Identidad-Mundial-26-SDC_Vertical-Redes.jpg"
)
ASSET_DIR = ANALYSIS_DIR / "instagram_assets"
HAITI_FLAG_PATH = ASSET_DIR / "flag_haiti.png"
SCOTLAND_FLAG_PATH = ASSET_DIR / "flag_scotland.png"
TITLE_FONT_PATH = ANALYSIS_DIR / "fonts" / "BarlowCondensed-Bold.ttf"
BODY_FONT_PATH = ANALYSIS_DIR / "fonts" / "OpenSans-Variable.ttf"

for font_path in [TITLE_FONT_PATH, BODY_FONT_PATH]:
    font_manager.fontManager.addfont(str(font_path))

TITLE_FONT = font_manager.FontProperties(fname=str(TITLE_FONT_PATH))
BODY_FONT = font_manager.FontProperties(fname=str(BODY_FONT_PATH))

COLORS = {
    "scotland": "#1F77B4",
    "haiti": "#D62728",
    "orange": "#FF7F0E",
    "green": "#2CA02C",
    "purple": "#9467BD",
    "cyan": "#17BECF",
    "ink": "#14213D",
    "muted": "#5B6577",
    "grid": "#D9E1E8",
    "white": "#FFFFFF",
    "offwhite": "#F7F8F5",
    "pitch": "#F1F4ED",
}

mpl.rcParams.update(
    {
        "font.family": BODY_FONT.get_name(),
        "text.color": COLORS["ink"],
        "axes.labelcolor": COLORS["muted"],
        "xtick.color": COLORS["muted"],
        "ytick.color": COLORS["muted"],
    }
)

SHOTS = pd.read_csv(DATA_DIR / "viz_1_xg_timeline.csv")
SEQUENCE = pd.read_csv(DATA_DIR / "viz_2_goal_sequence.csv")
METRICS = pd.read_csv(DATA_DIR / "supporting_match_metrics.csv")

TEMPLATE = Image.open(TEMPLATE_PATH).convert("RGB")
TEMPLATE = ImageOps.fit(TEMPLATE, (864, 1080), method=Image.Resampling.LANCZOS)


def title_fp(size):
    return font_manager.FontProperties(fname=str(TITLE_FONT_PATH), size=size)


def body_fp(size, weight="normal"):
    return font_manager.FontProperties(
        fname=str(BODY_FONT_PATH),
        size=size,
        weight=weight,
    )


def panel(canvas, x, y, w, h, face="white", alpha=0.96, radius=0.025, edge=None):
    patch = FancyBboxPatch(
        (x, y),
        w,
        h,
        boxstyle=f"round,pad=0.012,rounding_size={radius}",
        facecolor=face,
        edgecolor=edge or "none",
        linewidth=1,
        alpha=alpha,
        transform=canvas.transAxes,
        zorder=2,
    )
    canvas.add_patch(patch)
    return patch


def add_text(
    fig,
    x,
    y,
    text,
    size,
    *,
    title=False,
    color=None,
    ha="left",
    va="top",
    weight="normal",
    linespacing=1.05,
):
    return fig.text(
        x,
        y,
        text,
        fontproperties=title_fp(size) if title else body_fp(size, weight),
        color=color or COLORS["ink"],
        ha=ha,
        va=va,
        linespacing=linespacing,
        zorder=20,
    )


def new_slide(number, eyebrow, title, subtitle=None, title_size=35, subtitle_y=0.775):
    fig = plt.figure(figsize=(8.64, 10.8), dpi=100)
    fig.patch.set_facecolor(COLORS["offwhite"])

    background = fig.add_axes([0, 0, 1, 1])
    background.imshow(TEMPLATE)
    background.axis("off")

    canvas = fig.add_axes([0, 0, 1, 1])
    canvas.axis("off")
    canvas.set_xlim(0, 1)
    canvas.set_ylim(0, 1)

    add_text(fig, 0.055, 0.965, eyebrow.upper(), 12, title=True, color=COLORS["purple"])
    add_text(fig, 0.055, 0.925, title, title_size, title=True, linespacing=0.92)
    if subtitle:
        add_text(
            fig,
            0.055,
            subtitle_y,
            subtitle,
            11.5,
            color=COLORS["muted"],
            linespacing=1.16,
            va="bottom",
        )

    page = FancyBboxPatch(
        (0.90, 0.035),
        0.055,
        0.038,
        boxstyle="round,pad=0.005,rounding_size=0.01",
        facecolor=COLORS["purple"],
        edgecolor="none",
        transform=canvas.transAxes,
        zorder=20,
    )
    canvas.add_patch(page)
    add_text(fig, 0.9275, 0.054, f"{number}/6", 10, title=True, color="white", ha="center", va="center")
    return fig, canvas


def save_slide(fig, filename):
    png_path = OUTPUT_DIR / f"{filename}.tmp.png"
    jpg_path = OUTPUT_DIR / f"{filename}.jpg"
    fig.savefig(png_path, dpi=100, facecolor=fig.get_facecolor(), edgecolor="none")
    with Image.open(png_path) as image:
        image.convert("RGB").save(
            jpg_path,
            "JPEG",
            quality=91,
            optimize=True,
            progressive=True,
            subsampling=0,
        )
    png_path.unlink(missing_ok=True)
    plt.close(fig)
    return jpg_path


def metric_card(
    fig,
    canvas,
    x,
    y,
    w,
    value,
    label,
    color,
    *,
    h=0.105,
    value_size=27,
    label_size=10.5,
    centered=False,
):
    panel(canvas, x, y, w, h, face=COLORS["white"], edge=COLORS["grid"], alpha=0.98)
    text_x = x + w / 2 if centered else x + 0.022
    text_ha = "center" if centered else "left"
    add_text(fig, text_x, y + h * 0.74, value, value_size, title=True, color=color, ha=text_ha)
    add_text(
        fig,
        text_x,
        y + h * 0.27,
        label.upper(),
        label_size,
        title=True,
        color=COLORS["muted"],
        ha=text_ha,
    )


def comparison_metric_card(fig, canvas, x, y, w, left_value, right_value, label):
    panel(canvas, x, y, w, 0.105, face=COLORS["white"], edge=COLORS["grid"], alpha=0.98)
    center = x + w / 2
    offset = 0.066
    add_text(fig, center - offset, y + 0.078, left_value, 25, title=True, color=COLORS["haiti"], ha="center")
    add_text(fig, center, y + 0.078, "-", 24, title=True, color=COLORS["ink"], ha="center")
    add_text(fig, center + offset, y + 0.078, right_value, 25, title=True, color=COLORS["scotland"], ha="center")
    add_text(fig, center, y + 0.03, label.upper(), 10.5, title=True, color=COLORS["muted"], ha="center")


def add_flag(fig, path, bounds):
    flag_ax = fig.add_axes(bounds, zorder=15)
    flag_ax.imshow(Image.open(path).convert("RGB"))
    flag_ax.axis("off")
    for spine in flag_ax.spines.values():
        spine.set_visible(True)
        spine.set_color("white")
        spine.set_linewidth(1.5)
    return flag_ax


def draw_pitch(ax, half=False):
    line = "#AAB7AA"
    ax.set_facecolor(COLORS["pitch"])
    if half:
        ax.set_xlim(60, 121.5)
    else:
        ax.set_xlim(-1.5, 121.5)
    ax.set_ylim(81.5, -1.5)
    ax.set_aspect("equal")

    ax.add_patch(Rectangle((0, 0), 120, 80, fill=False, ec=line, lw=1.3))
    ax.plot([60, 60], [0, 80], color=line, lw=1.2)
    ax.add_patch(Circle((60, 40), 9.15, fill=False, ec=line, lw=1.2))
    ax.add_patch(Circle((60, 40), 0.45, color=line))
    for x in [0, 102]:
        ax.add_patch(Rectangle((x, 18), 18, 44, fill=False, ec=line, lw=1.2))
    for x in [0, 114]:
        ax.add_patch(Rectangle((x, 30), 6, 20, fill=False, ec=line, lw=1.2))
    ax.add_patch(Circle((12, 40), 0.45, color=line))
    ax.add_patch(Circle((108, 40), 0.45, color=line))
    ax.add_patch(Arc((12, 40), 18.3, 18.3, theta1=310, theta2=50, ec=line, lw=1.2))
    ax.add_patch(Arc((108, 40), 18.3, 18.3, theta1=130, theta2=230, ec=line, lw=1.2))
    ax.axis("off")


def arrow(ax, row, color, lw=2.0, linestyle="-", zorder=4):
    ax.add_patch(
        FancyArrowPatch(
            (row.start_x, row.start_y),
            (row.end_x, row.end_y),
            arrowstyle="-|>",
            mutation_scale=11,
            linewidth=lw,
            linestyle=linestyle,
            color=color,
            shrinkA=1,
            shrinkB=1,
            zorder=zorder,
        )
    )


def draw_vertical_attacking_half(ax):
    line = "#AAB7AA"
    ax.set_facecolor(COLORS["white"])
    ax.add_patch(Rectangle((0, 0), 80, 60, fill=False, ec=line, lw=1.4))
    ax.add_patch(Rectangle((18, 0), 44, 18, fill=False, ec=line, lw=1.4))
    ax.add_patch(Rectangle((30, 0), 20, 6, fill=False, ec=line, lw=1.4))
    ax.add_patch(Rectangle((36, -1.6), 8, 1.6, fill=False, ec=line, lw=1.4, clip_on=False))
    ax.add_patch(Circle((40, 12), 0.5, color=line))
    ax.add_patch(Arc((40, 12), 18.3, 18.3, theta1=35, theta2=145, ec=line, lw=1.4))
    ax.set_xlim(-2, 82)
    ax.set_ylim(61, -2)
    ax.set_aspect("equal")
    ax.axis("off")


def slide_1():
    fig, canvas = new_slide(
        1,
        "Group C | Haiti 0-1 Scotland",
        "SCOTLAND WON THE SCORE.\nHAITI WON THE NUMBERS.",
        "A single 26-second move decided a match\nHaiti controlled for long stretches.",
        title_size=37,
        subtitle_y=0.785,
    )

    panel(canvas, 0.055, 0.555, 0.89, 0.205, face=COLORS["ink"], alpha=0.98)
    add_text(fig, 0.19, 0.715, "HAITI", 27, title=True, color="white", ha="center")
    add_text(fig, 0.81, 0.715, "SCOTLAND", 27, title=True, color="white", ha="center")
    add_flag(fig, HAITI_FLAG_PATH, [0.115, 0.595, 0.15, 0.09])
    add_flag(fig, SCOTLAND_FLAG_PATH, [0.735, 0.595, 0.15, 0.09])
    add_text(fig, 0.50, 0.677, "0  -  1", 55, title=True, color="white", ha="center", va="center")
    add_text(fig, 0.50, 0.585, "McGinn 28'", 17, title=True, color=COLORS["orange"], ha="center")

    comparison_metric_card(fig, canvas, 0.055, 0.405, 0.265, "15", "9", "Shots")
    comparison_metric_card(fig, canvas, 0.367, 0.405, 0.265, "1.28", "1.00", "Expected goals")
    comparison_metric_card(fig, canvas, 0.68, 0.405, 0.265, "55", "31", "Deep progressions")

    panel(canvas, 0.10, 0.19, 0.80, 0.155, face="#FFF3E9", alpha=0.98)
    add_text(fig, 0.50, 0.31, "THE PARADOX", 17, title=True, color=COLORS["orange"], ha="center")
    add_text(
        fig,
        0.50,
        0.255,
        "Haiti created more volume, territory and pressure.\nScotland produced the one sequence that mattered.",
        22,
        title=True,
        ha="center",
        linespacing=1.02,
    )
    return save_slide(fig, "01_cover_scoreboard_vs_numbers")


def slide_2():
    fig, canvas = new_slide(
        2,
        "Performance comparison",
        "THE SCORELINE HID\nTHE PERFORMANCE",
        "Haiti led in attacking volume, progression\nand pressing outcomes.",
        title_size=37,
        subtitle_y=0.785,
    )
    panel(canvas, 0.055, 0.13, 0.89, 0.64, face=COLORS["white"], edge=COLORS["grid"], alpha=0.98)

    ax = fig.add_axes([0.19, 0.23, 0.67, 0.43])
    categories = ["SHOTS", "xG", "DEEP\nPROGRESSIONS", "PRESSURE\nREGAINS"]
    haiti = np.array([15, 1.28, 55, 33], dtype=float)
    scotland = np.array([9, 1.00, 31, 19], dtype=float)
    h_norm = haiti / np.maximum(haiti, scotland)
    s_norm = scotland / np.maximum(haiti, scotland)
    y = np.arange(len(categories))[::-1]

    ax.barh(y + 0.16, h_norm, height=0.27, color=COLORS["haiti"], zorder=3)
    ax.barh(y - 0.16, s_norm, height=0.27, color=COLORS["scotland"], zorder=3)
    for i, yi in enumerate(y):
        h_label = f"{haiti[i]:.2f}" if i == 1 else f"{int(haiti[i])}"
        s_label = f"{scotland[i]:.2f}" if i == 1 else f"{int(scotland[i])}"
        ax.text(h_norm[i] + 0.025, yi + 0.16, h_label, fontproperties=title_fp(14), va="center", color=COLORS["haiti"])
        ax.text(s_norm[i] + 0.025, yi - 0.16, s_label, fontproperties=title_fp(14), va="center", color=COLORS["scotland"])

    ax.set_yticks(y, categories, fontproperties=title_fp(13))
    ax.set_xlim(0, 1.08)
    ax.set_xticks([])
    ax.grid(axis="x", color=COLORS["grid"], lw=0.8)
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.tick_params(axis="y", length=0, pad=6)
    ax.set_facecolor("white")

    add_flag(fig, HAITI_FLAG_PATH, [0.31, 0.69, 0.065, 0.038])
    add_text(fig, 0.3425, 0.682, "HAITI", 11.5, title=True, color=COLORS["haiti"], ha="center")
    add_flag(fig, SCOTLAND_FLAG_PATH, [0.585, 0.69, 0.065, 0.038])
    add_text(fig, 0.6175, 0.682, "SCOTLAND", 11.5, title=True, color=COLORS["scotland"], ha="center")

    add_text(
        fig,
        0.08,
        0.185,
        "The bars are scaled within each metric. Labels show the original match values.",
        10.5,
        color=COLORS["muted"],
    )
    return save_slide(fig, "02_performance_comparison")


def slide_3():
    fig, canvas = new_slide(
        3,
        "Expected goals timeline",
        "SCOTLAND STRUCK EARLY.\nHAITI KEPT BUILDING.",
        "Scotland converted the decisive first-half burst.\nHaiti's xG kept rising until stoppage time.",
        title_size=35,
        subtitle_y=0.785,
    )
    panel(canvas, 0.055, 0.13, 0.89, 0.64, face=COLORS["white"], edge=COLORS["grid"], alpha=0.98)
    ax = fig.add_axes([0.115, 0.235, 0.78, 0.39])

    for team, color in [("Scotland", COLORS["scotland"]), ("Haiti", COLORS["haiti"])]:
        team_df = SHOTS[SHOTS["team"].eq(team)].sort_values("match_minute")
        x = np.r_[0, team_df["match_minute"], 100]
        y = np.r_[0, team_df["cumulative_xg"], team_df["cumulative_xg"].iloc[-1]]
        ax.step(x, y, where="post", color=color, lw=3.0, zorder=3)
        ax.scatter(
            team_df["match_minute"],
            team_df["cumulative_xg"],
            color=color,
            edgecolor="white",
            linewidth=1,
            s=34,
            zorder=4,
        )

    goal = SHOTS[SHOTS["is_goal"].astype(str).str.lower().eq("true")].iloc[0]
    ax.axvline(goal.match_minute, color=COLORS["orange"], lw=1.5, linestyle=(0, (3, 4)))
    ax.scatter(goal.match_minute, goal.cumulative_xg, s=120, color=COLORS["orange"], edgecolor="white", lw=2, zorder=6)
    ax.text(goal.match_minute + 2, 1.43, "GOAL 28'", fontproperties=title_fp(12), color=COLORS["orange"])
    ax.axvspan(75, 100, color=COLORS["haiti"], alpha=0.06)

    ax.set_xlim(0, 100)
    ax.set_ylim(0, 1.55)
    ax.set_xticks([0, 15, 30, 45, 60, 75, 90, 100])
    ax.set_xticklabels(["0'", "15'", "30'", "45'", "60'", "75'", "90'", "100'"], fontproperties=body_fp(10))
    ax.set_yticks([0, 0.5, 1.0, 1.5])
    ax.set_yticklabels(["0.0", "0.5", "1.0", "1.5"], fontproperties=body_fp(10))
    ax.grid(color=COLORS["grid"], lw=0.8)
    ax.set_xlabel("MATCH MINUTE", fontproperties=title_fp(11), labelpad=8)
    ax.set_ylabel("CUMULATIVE xG", fontproperties=title_fp(11), labelpad=8)
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.tick_params(length=0)
    ax.set_facecolor("white")

    metric_card(fig, canvas, 0.09, 0.655, 0.25, "1.28 xG", "Haiti", COLORS["haiti"], centered=True)
    metric_card(fig, canvas, 0.375, 0.655, 0.25, "1.00 xG", "Scotland", COLORS["scotland"], centered=True)
    metric_card(fig, canvas, 0.66, 0.655, 0.25, "0.58 xG", "Haiti after 75'", COLORS["orange"], centered=True)

    add_text(
        fig,
        0.095,
        0.18,
        "Haiti generated six shots after 75'. Scotland generated none.",
        15,
        title=True,
    )
    return save_slide(fig, "03_xg_timeline")


def slide_4():
    fig, canvas = new_slide(
        4,
        "The decisive sequence",
        "26 SECONDS. SIX PASSES.\nONE DECISIVE GOAL.",
        "Controlled circulation became a direct right-sided attack,\nthen a rebound finish.",
        title_size=36,
        subtitle_y=0.785,
    )
    panel(canvas, 0.055, 0.12, 0.89, 0.65, face=COLORS["white"], edge=COLORS["grid"], alpha=0.98)

    metric_card(fig, canvas, 0.09, 0.67, 0.23, "26", "Seconds", COLORS["orange"], h=0.085, value_size=29, label_size=9.5, centered=True)
    metric_card(fig, canvas, 0.385, 0.67, 0.23, "6", "Completed passes", COLORS["orange"], h=0.085, value_size=29, label_size=9.5, centered=True)
    metric_card(fig, canvas, 0.68, 0.67, 0.23, "0.48", "Combined xG", COLORS["orange"], h=0.085, value_size=29, label_size=9.5, centered=True)

    ax = fig.add_axes([0.075, 0.285, 0.85, 0.36])
    draw_pitch(ax)
    passes = SEQUENCE[SEQUENCE["event_type"].eq("Pass")].reset_index(drop=True)
    carries = SEQUENCE[
        SEQUENCE["event_type"].eq("Carry")
        & SEQUENCE["player"].isin(["Grant Campbell Hanley", "Ben Doak"])
    ]
    shots = SEQUENCE[SEQUENCE["event_type"].eq("Shot")]

    for number, row in enumerate(passes.itertuples(index=False), start=1):
        arrow(ax, row, COLORS["scotland"], lw=2.3 if number == 4 else 1.8)
        mx = row.start_x + 0.58 * (row.end_x - row.start_x)
        my = row.start_y + 0.58 * (row.end_y - row.start_y)
        ax.scatter(mx, my, s=130, color=COLORS["scotland"], edgecolor="white", lw=1.4, zorder=7)
        ax.text(mx, my, str(number), color="white", fontproperties=title_fp(7), ha="center", va="center", zorder=8)

    for row in carries.itertuples(index=False):
        arrow(ax, row, COLORS["cyan"], lw=2.0, linestyle=(0, (2, 2)))

    adams = shots[shots["player"].eq("Che Adams")].iloc[0]
    mcginn = shots[shots["player"].eq("John McGinn")].iloc[0]
    arrow(ax, adams, COLORS["purple"], lw=2.2, linestyle=(0, (2, 2)), zorder=6)
    arrow(ax, mcginn, COLORS["orange"], lw=3.0, zorder=7)
    for row, label, color in [(adams, "A", COLORS["purple"]), (mcginn, "B", COLORS["orange"])]:
        ax.scatter(row.start_x, row.start_y, s=190, color=color, edgecolor="white", lw=1.6, zorder=9)
        ax.text(row.start_x, row.start_y, label, color="white", fontproperties=title_fp(8), ha="center", va="center", zorder=10)

    labels = {
        "McTominay": (35.1, 35.2, -38, -12),
        "Robertson": (43.3, 10.7, -10, 25),
        "Hendry": (28.8, 33.1, -37, 16),
        "Hanley": (48.3, 61.1, -12, 18),
        "Adams": (117.2, 45.2, -31, 15),
        "Doak": (113.6, 60.7, -4, -23),
        "McGinn": (106.3, 42.2, -28, -22),
    }
    for name, (x, y, dx, dy) in labels.items():
        ax.annotate(
            name,
            xy=(x, y),
            xytext=(dx, dy),
            textcoords="offset points",
            fontproperties=title_fp(10),
            bbox=dict(boxstyle="round,pad=0.25", fc="white", ec=COLORS["scotland"], lw=0.7),
            arrowprops=dict(arrowstyle="-", color=COLORS["muted"], lw=0.7),
            ha="center",
            zorder=12,
        )

    panel(canvas, 0.09, 0.16, 0.82, 0.105, face="#FFF3E9", alpha=0.98)
    add_text(
        fig,
        0.12,
        0.238,
        "Hanley -> Adams -> Doak -> Adams shot -> McGinn rebound",
        15,
        title=True,
        color=COLORS["ink"],
    )
    add_text(
        fig,
        0.12,
        0.195,
        "The two shots produced almost half of Scotland's total event-level xG.",
        11.5,
        color=COLORS["muted"],
    )
    return save_slide(fig, "04_decisive_goal_sequence")


def slide_5():
    fig, canvas = new_slide(
        5,
        "Final 25 minutes",
        "HAITI PUSHED.\nSCOTLAND SURVIVED.",
        "From the 75th minute onward, the game moved\nalmost entirely toward Scotland's goal.",
        title_size=38,
        subtitle_y=0.785,
    )
    panel(canvas, 0.055, 0.12, 0.89, 0.65, face=COLORS["white"], edge=COLORS["grid"], alpha=0.98)

    metric_card(fig, canvas, 0.075, 0.655, 0.25, "6", "Haiti shots", COLORS["haiti"], centered=True)
    metric_card(fig, canvas, 0.375, 0.655, 0.25, "0.58", "Haiti xG", COLORS["haiti"], centered=True)
    metric_card(fig, canvas, 0.675, 0.655, 0.25, "0", "Scotland shots", COLORS["scotland"], centered=True)

    pitch_ax = fig.add_axes([0.18, 0.285, 0.64, 0.31], zorder=5)
    draw_vertical_attacking_half(pitch_ax)
    late_haiti = SHOTS[(SHOTS["team"].eq("Haiti")) & (SHOTS["match_minute"].ge(75))]
    shot_x = late_haiti["start_y"]
    shot_y = 120 - late_haiti["start_x"]
    sizes = 180 + late_haiti["xg"] * 1000
    pitch_ax.scatter(
        shot_x,
        shot_y,
        s=sizes,
        color=COLORS["haiti"],
        alpha=0.82,
        edgecolor="white",
        linewidth=1.8,
        zorder=5,
    )
    best_chance = late_haiti.loc[late_haiti["xg"].idxmax()]
    pitch_ax.scatter(
        [best_chance["start_y"]],
        [120 - best_chance["start_x"]],
        s=[180 + best_chance["xg"] * 1000 + 130],
        facecolor="none",
        edgecolor=COLORS["orange"],
        linewidth=3,
        zorder=7,
    )

    add_text(
        fig,
        0.50,
        0.215,
        "Pierrot's 90+3' chance: 0.31 xG, saved by Angus Gunn",
        14,
        title=True,
        ha="center",
    )
    add_text(
        fig,
        0.50,
        0.172,
        "Bubble size represents shot quality.",
        10.5,
        color=COLORS["muted"],
        ha="center",
    )
    return save_slide(fig, "05_haiti_final_push")


def slide_6():
    fig, canvas = new_slide(
        6,
        "Match takeaway",
        "ONE SEQUENCE WON IT.",
        "Scotland's efficiency beat Haiti's pressure,\nprogression and late chance volume.",
        title_size=40,
        subtitle_y=0.785,
    )
    panel(canvas, 0.075, 0.49, 0.39, 0.265, face="#EAF4FA", alpha=0.98)
    panel(canvas, 0.535, 0.49, 0.39, 0.265, face="#FCECED", alpha=0.98)

    add_text(fig, 0.27, 0.715, "SCOTLAND'S EDGE", 16, title=True, color=COLORS["scotland"], ha="center")
    add_text(
        fig,
        0.27,
        0.665,
        "Direct progression\nDoak's acceleration\nMcGinn's second-ball reaction\nDefensive resilience",
        15,
        title=True,
        ha="center",
        linespacing=1.35,
    )

    add_text(fig, 0.73, 0.715, "HAITI'S LESSON", 16, title=True, color=COLORS["haiti"], ha="center")
    add_text(
        fig,
        0.73,
        0.665,
        "Territorial control\nMore shots and xG\nStrong pressure regains\nFinishing remained decisive",
        15,
        title=True,
        ha="center",
        linespacing=1.35,
    )

    panel(canvas, 0.055, 0.235, 0.89, 0.19, face=COLORS["ink"], alpha=0.98)
    add_text(fig, 0.50, 0.385, "READ THE FULL MATCH ANALYSIS", 18, title=True, color=COLORS["orange"], ha="center")
    add_text(fig, 0.50, 0.33, "Link in bio", 31, title=True, color="white", ha="center")
    add_text(
        fig,
        0.50,
        0.275,
        "The World Cup of Data | Sports Data Campus",
        12.5,
        color="white",
        ha="center",
    )

    add_text(
        fig,
        0.055,
        0.17,
        "Analysis: Regina Khalil, Ulrich Haarmann, Imrane Talay and Jairo Rodríguez",
        10.5,
        color=COLORS["muted"],
    )
    return save_slide(fig, "06_takeaway_and_cta")


def make_contact_sheet(paths):
    thumbnails = []
    for path in paths:
        image = Image.open(path).convert("RGB")
        image.thumbnail((288, 360), Image.Resampling.LANCZOS)
        thumbnails.append(image.copy())

    sheet = Image.new("RGB", (288 * 3, 360 * 2), "white")
    for index, image in enumerate(thumbnails):
        x = (index % 3) * 288
        y = (index // 3) * 360
        sheet.paste(image, (x, y))
    sheet.save(OUTPUT_DIR / "instagram_carousel_contact_sheet.jpg", quality=92, optimize=True)


def main():
    outputs = [
        slide_1(),
        slide_2(),
        slide_3(),
        slide_4(),
        slide_5(),
        slide_6(),
    ]
    make_contact_sheet(outputs)
    for path in outputs:
        with Image.open(path) as image:
            print(f"{path.name}: {image.width}x{image.height}, {path.stat().st_size / 1024:.1f} KB")
    print(f"Contact sheet: {OUTPUT_DIR / 'instagram_carousel_contact_sheet.jpg'}")


if __name__ == "__main__":
    main()
