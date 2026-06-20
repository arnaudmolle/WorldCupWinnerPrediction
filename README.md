# World Cup 2026 – Probabilistic Winner Prediction (R)

Predict the 2026 FIFA Men's World Cup winner using local Kaggle CSV data,
recency-weighted team features, Elo ratings, a player quality layer, and
Monte Carlo tournament simulation.

---

## How the pipeline works

```
fetch_data.R          →  wc2026_output/  →  predict_wc2026.R
  reads local CSVs           .rds caches        models + simulation
  builds features            team_features      win probabilities
                             player_agg         plots + CSV
```

The two scripts are independent:

| Script | Role |
|---|---|
| `fetch_data.R` | Reads local CSVs, cleans, engineers features, saves `.rds` caches |
| `predict_wc2026.R` | Loads the `.rds` caches, fits models, simulates the tournament, writes plots |

Run `fetch_data.R` once (or when you refresh the CSVs).
Run `predict_wc2026.R` as many times as you like — it only reads the caches.

---

## Required data files  (place in `data/`)

### Team stats — [martj42 International Football Results](https://www.kaggle.com/datasets/martj42/international-football-results-from-1872-to-2017)

Download and unzip into `data/`. You need all four CSV files:

| File | Columns used |
|---|---|
| `results.csv` | `date, home_team, away_team, home_score, away_score, tournament, neutral` |
| `goalscorers.csv` | `date, home_team, away_team, team, scorer, own_goal, penalty` |
| `shootouts.csv` | `date, home_team, away_team, winner` |
| `former_names.csv` | `current, former` (used to resolve historic name variants) |

### Player stats — [hubertsidorowicz Football Players Stats 2024-2025](https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025)

Download `players_data_light-2024_2025.csv` (or the full version) into `data/`.

| File | Columns used |
|---|---|
| `players_data_light-2024_2025.csv` | `Player, Nation, Pos, Squad, Comp, Age, Min, Gls, Ast, xG, xAG, Tkl, Int` |

> The Nation column uses FBref 3-letter codes (e.g. `ENG`, `FRA`). The script
> maps these to canonical country names automatically.

### Optional

Place a `data/fifa_ranking_2026.csv` with columns `Team, Rank, Points` to
override the Elo-derived ranking. If absent, Elo ratings computed from the
full match history are used instead.

---

## Setup

### Install R packages

Run once in an R session:

```r
install.packages(c(
  "dplyr","tidyr","purrr","stringr","readr","tibble","lubridate",
  "scales","janitor",                                # fetch_data.R
  "BradleyTerry2","lme4",                            # models
  "ggplot2","ggrepel","patchwork","igraph","ggraph", # plots
  "sf","rnaturalearth","rnaturalearthdata",           # world map
  "fmsb","viridis","RColorBrewer",                   # radar + colours
  "progressr","glue"                                 # utilities
))
```

`brms` (optional Bayesian upgrade) requires a C++ toolchain and Stan:

```r
install.packages("brms")
install.packages("cmdstanr", repos = "https://mc-stan.org/r-packages/")
cmdstanr::install_cmdstan()   # downloads and compiles CmdStan; takes ~5 min
```

On Windows, install [RTools](https://cran.r-project.org/bin/windows/Rtools/)
before compiling Stan.

---

## Running on Windows

**Option A — RStudio (easiest)**

Open `fetch_data.R` and click **Source**, then open `predict_wc2026.R`
and click **Source**.

**Option B — PowerShell with full Rscript path**

Replace the R version number with yours:

```powershell
# Step 1: fetch data
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' `
    'C:\Users\arnau\Desktop\WorldCupWinner\fetch_data.R'

# Step 2: run models + simulation
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' `
    'C:\Users\arnau\Desktop\WorldCupWinner\predict_wc2026.R'
```

**Option C — Add R to PATH (persistent)**

```powershell
setx PATH "$env:PATH;C:\Program Files\R\R-4.4.2\bin"
# restart PowerShell, then:
Rscript fetch_data.R
Rscript predict_wc2026.R
```

---

## Top-of-script switches

`fetch_data.R`:

```r
DATA_DIR      <- "data"      # folder with your CSVs
OUT_DIR       <- "wc2026_output"
CUTOFF_YR     <- 2022        # only matches ≥ this year used for recent features
                             # (full history still used for Elo)
ELO_K         <- 30          # Elo K-factor
```

`predict_wc2026.R`:

```r
force_refresh <- FALSE       # TRUE = ignore .rds caches, re-read CSVs
n_sim         <- 10000L      # Monte Carlo simulation runs
```

## Configuration via `config.yaml`

You can tune most pipeline choices via `config.yaml` at the repo root.
Copy the example `config.yaml` (provided) and change values such as:

- rescaling.global_rescale: true  — rescale features against the global
  international pool instead of only the 48 finalists (recommended experiment)
- monte_carlo.n_simulations: 10000
- ensemble.bt_weight / poisson_weight — blend between Bradley-Terry and Poisson
- ensemble.knockout_extra_time_compression — reduce favourite edge in KO

The pipeline will read `config.yaml` automatically when you run the scripts.

---

## Outputs

All files are written to `wc2026_output/`.

### Data caches (written by `fetch_data.R`)

| File | Contents |
|---|---|
| `all_results_parsed.rds` | Cleaned, recency-weighted match table |
| `team_features.rds` / `.csv` | 48-team feature matrix (Elo, form, PageRank, player quality) |
| `player_agg.rds` | Per-team squad aggregates (goals/90, xG/90, tackles/90, …) |
| `player_nation_coverage.csv` | Which of the 48 WC nations have player data |
| `data_proof.csv` | **Diagnostic proof** – row counts, feature source, coverage stats |
| `data_diagnostics.txt` | Human-readable coverage report + top-10 strength ranking |

### Model outputs (written by `predict_wc2026.R`)

| File | Contents |
|---|---|
| `final_predictions.csv` | Per-team win probability, CI, Elo, BT ability |
| `win_probabilities.rds` / `.png` | Monte Carlo results (bar chart) |
| `h2h_heatmap.png` | 16×16 head-to-head win-probability matrix |
| `team_strength_map.png` | World choropleth coloured by team strength |
| `strength_radar.png` | Spider charts for top-8 teams |
| `h2h_network.png` | Directed head-to-head network graph |
| `bracket_tree.png` | Deterministic most-likely tournament tree (who wins each match) |
| `bracket_group_matches.csv` | Group-stage most-likely match results |
| `bracket_knockout.csv` | Knockout-stage most-likely match results |

---

## Proof that data was used

After running `fetch_data.R`, open **`wc2026_output/data_proof.csv`**:

```
metric,value
timestamp,2026-06-12 …
results_csv_rows,49398
recent_matches_used,3847
goalscorer_records,18202
elo_teams_computed,312
wc_teams_in_features,48
teams_with_5plus_matches,44
teams_with_player_data,32
feature_source,match_data + Elo + PageRank + player_stats
```

And **`wc2026_output/player_nation_coverage.csv`** lists which of the 48 WC
nations appeared in the player dataset.

The `team_features.csv` column `data_source_flag` shows one of:

| Flag | Meaning |
|---|---|
| `full` | Team has both recent match data AND player stats |
| `team_only` | Match data present, no player stats for that nation |
| `player_only` | Player stats present, fewer than 5 recent matches |
| `proxy_only` | Elo-only (no match data, no player stats) |

---

## How features are computed

### Team features (from `results.csv`)

| Feature | Method |
|---|---|
| `elo_current` | Sequential Elo from 1872 to present (K=30, home advantage=100 pts) |
| `w_gf`, `w_ga`, `w_gd` | Weighted goals for/against/diff (exp decay, 2-yr half-life) |
| `win_rate`, `form_last5` | Win rate and last-5 points average (recency ≥ 2022) |
| `q_pts_per`, `q_win_rate`| Qualifier-only weighted stats |
| `pagerank` | 10-iteration PageRank on the H2H win-graph (WC teams only) |

### Player features (from `players_data_light-2024_2025.csv`)

| Feature | Method |
|---|---|
| `goals_p90` | Total goals / (total minutes / 90), per WC nation |
| `assists_p90` | Idem for assists |
| `xg_p90`, `xag_p90` | Expected goals / expected assists per 90 |
| `tackles_p90`, `interceptions_p90` | Defensive actions per 90 |

All player stats are aggregated across all players of a given WC nation in
the dataset (Big-5 European leagues + any other covered league).

### Composite `team_strength` (0–1)

```
0.30 × elo_sc
0.15 × attack_str      (w_gf rescaled)
0.15 × defence_str     (−w_ga rescaled)
0.10 × form_sc
0.10 × pagerank_sc
0.05 × qualifier_sc
0.10 × player_attack_sc  (goals_p90 + xg_p90, rescaled)
0.05 × player_defence_sc (tackles_p90 + int_p90, rescaled)
```

---

## Models

| Model | Description | Use |
|---|---|---|
| **Elo** | Classic sequential rating from full match history | Prior strength estimate |
| **Bradley-Terry** | Paired-comparison MLE on recency-weighted match pairs | Win probability P(A beats B) |
| **Poisson / Dixon-Coles** | Mixed-effects GLM for goal counts (lme4) | Scoreline + draw probability |
| **Ensemble** | 50/50 blend of BT win-prob and Poisson win-prob | Match simulation |
| **brms** (optional) | Full Bayesian hierarchical Poisson with Stan | Posterior uncertainty |

---

## Troubleshooting

**"No such file" errors for CSVs**
Check that all four martj42 files and the player CSV are in the `data/`
folder with exact names listed above.

**Nation column doesn't map to WC teams**
Open `wc2026_output/player_nation_coverage.csv`. If a nation shows
`player_data = FALSE`, the 3-letter code in that row's `Nation` field wasn't
recognised. Add it to `CANONICAL_MAP` at the top of `fetch_data.R`.

**Elo all equal to 1500**
Means `results.csv` wasn't loaded. Check the file path and column names
match those listed in the table above (`home_score` / `away_score`, not
`home_goals`).

**Radar chart error "must be 3 or more variables"**
Usually means fewer than 3 of the 5 feature columns survived into
`team_features`. Run `fetch_data.R` first and confirm `team_features.csv`
contains `elo_sc`, `attack_str`, `defence_str`, `form_sc`, `pr_sc`.

**brms / Stan compilation fails on Windows**
Install [RTools 4.4](https://cran.r-project.org/bin/windows/Rtools/) and
verify with `pkgbuild::has_build_tools()`. Then re-run
`cmdstanr::install_cmdstan()`.

---

## Extending the pipeline

| Extension | Where |
|---|---|
| Add more player seasons | Drop additional `players_data-20XX_20YY.csv` into `data/` and stack them before aggregating in Section 6 |
| Injury adjustments | After loading `team_features.rds`, subtract from `team_strength` for key missing players before simulation |
| Live tournament updating | After each group match, append the result to `results.csv` and re-run `fetch_data.R` with `force_refresh <- TRUE` |
| Bayesian upgrade | Uncomment the `brms` block in `predict_wc2026.R` (requires Stan) |
| Bookmaker calibration | Compare `final_predictions.csv` win probabilities with odds from football-data.co.uk and fit Platt scaling |