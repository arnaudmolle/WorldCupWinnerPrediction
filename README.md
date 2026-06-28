# World Cup 2026 – Probabilistic Winner Prediction (R)

Predict the 2026 FIFA Men's World Cup winner using local Kaggle CSV data,
recency-weighted team features, Elo ratings, a player quality layer, and
Monte Carlo tournament simulation.

---

## How the pipeline works

```
fetch_data.R          →  wc2026_output/  →  simulate.R
  reads local CSVs           .rds caches        models + simulation
  builds features            team_features      win probabilities
                             player_agg         plots + CSVs
```

The two scripts are independent:

| Script | Role |
|---|---|
| `fetch_data.R` | Reads local CSVs, cleans, engineers features, saves `.rds` caches |
| `simulate.R` | Loads the `.rds` caches, fits models, runs the Monte Carlo simulation, builds the deterministic "most-likely path" bracket, and writes all plots/CSVs |

Run `fetch_data.R` once (or whenever you refresh the source CSVs).
Run `simulate.R` as many times as you like — it only reads the caches and
never touches the raw data files.

> Note: some earlier versions of this repo referred to the modelling script
> as `predict_wc2026.R`. The current version is `simulate.R` — same role,
> single file, covers modelling, simulation, and visualisation together.

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
> maps these to canonical country names automatically via `CANONICAL_MAP`.

### Optional

Place a `data/fifa_ranking_2026.csv` with columns `Team, Rank, Points` to
override the Elo-derived ranking. If absent, Elo ratings computed from the
full match history are used instead.

---

## Setup

### Install R packages

`simulate.R` checks for and auto-installs any missing packages on first run,
but you can pre-install everything:

```r
install.packages(c(
  "dplyr","tidyr","purrr","stringr","readr","tibble","lubridate",
  "ggplot2","scales","glue","progressr",          # core + simulation progress
  "BradleyTerry2","lme4",                          # models
  "rnaturalearth","sf",                            # world map
  "countrycode",
  "rvest","xml2",                                  # FIFA Round-of-32 draw scraping
  "fmsb","RColorBrewer","viridis","janitor",        # radar + colours
  "yaml"                                            # config.yaml support
))
```

`brms` (optional Bayesian upgrade, not used by default) requires a C++
toolchain and Stan:

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

Open `fetch_data.R` and click **Source**, then open `simulate.R`
and click **Source**.

**Option B — PowerShell with full Rscript path**

Replace the R version number with yours:

```powershell
# Step 1: fetch data
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' `
    'C:\Users\arnau\Desktop\WorldCupWinner\fetch_data.R'

# Step 2: run models + simulation
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' `
    'C:\Users\arnau\Desktop\WorldCupWinner\simulate.R'
```

**Option C — Add R to PATH (persistent)**

```powershell
setx PATH "$env:PATH;C:\Program Files\R\R-4.4.2\bin"
# restart PowerShell, then:
Rscript fetch_data.R
Rscript simulate.R
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

`simulate.R`:

```r
n_sim <- 10000L   # Monte Carlo simulation runs (overridden by config.yaml if present)
```

## Configuration via `config.yaml`

You can tune most pipeline choices via `config.yaml` at the repo root.
`simulate.R` reads it automatically (falling back to internal defaults if
absent or if a key is missing) and uses it to override:

| Key | Effect |
|---|---|
| `monte_carlo.random_seed` | Seeds both the group-stage and knockout RNG, for reproducible runs |
| `monte_carlo.n_simulations` | Overrides `n_sim` (default 10,000) |
| `ensemble.bt_weight` / `ensemble.poisson_weight` | Blend ratio between Bradley-Terry and Poisson win probabilities (default 0.5 / 0.5) |
| `ensemble.knockout_extra_time_compression` | Factor (default 0.6) that pulls knockout win probabilities toward 50/50, reflecting extra-time variance |
| `rescaling.global_rescale` | Rescale component features against the full international pool rather than just the 48 finalists (set in `fetch_data.R`) |

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
| `team_strength_contributions.csv` | Weighted breakdown of every component feeding `team_strength`, per team |

### Model outputs (written by `simulate.R`)

| File | Contents |
|---|---|
| `final_predictions.csv` | Per-team win probability, 95% CI, Elo, BT ability, team_strength, and other feature columns |
| `win_probabilities.rds` / `win_probabilities.png` | Monte Carlo results — bar chart with 95% confidence intervals |
| `h2h_heatmap.png` | Pairwise head-to-head win-probability matrix for every team with a nonzero title chance |
| `team_strength_map.png` | World choropleth coloured by team strength (England/Scotland share the GBR polygon — a Natural Earth limitation) |
| `strength_radar.png` | Spider charts comparing component scores for the top 8 teams |
| `player_stats_top10.png` | Squad-level player aggregates (goals/xG/assists/tackles per 90) for the top 10 predicted teams |
| `data_quality_probabilities.png` | Win-probability bar chart coloured by `data_source_flag`, so you can see which predictions rest on full data vs. proxies |
| `team_strength_breakdown.png` | Stacked bar of what's driving `team_strength` for the top 15 teams, from `team_strength_contributions.csv` |
| `simulation_proof.csv` | **Audit trail** — timestamp, valid simulation count, BT/Poisson fit status, data coverage, top predicted team |
| `bracket_group_matches.csv` | Deterministic "most-likely" group-stage results (each match decided by its highest-probability outcome) |
| `bracket_knockout.csv` | Deterministic knockout bracket with per-match win probabilities |
| `bracket_tree.png` | Visual tournament tree for the most-likely path, ending in a single predicted champion |

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

`simulate.R` re-prints this table at the top of its own run, and writes a
second audit file, **`wc2026_output/simulation_proof.csv`**, covering the
modelling/simulation side specifically: how many simulations were valid, how
many teams the Bradley-Terry model actually converged on, whether the
Poisson model fitted or fell back to a proxy, and which `data_source_flag`
values are present across the 48 teams.

The `team_features.csv` column `data_source_flag` shows one of:

| Flag | Meaning |
|---|---|
| `full` | Team has both recent match data AND player stats |
| `team_only` | Match data present, no player stats for that nation |
| `player_only` | Player stats present, fewer than 5 recent matches |
| `proxy_only` | No real match or player data — Elo/team_strength proxy row only |

---

## How features are computed

### Team features (from `results.csv`)

| Feature | Method |
|---|---|
| `elo_current` | Sequential Elo from 1872 to present (K=30, home advantage=100 pts) |
| `w_gf`, `w_ga`, `w_gd` | Weighted goals for/against/diff (exp decay, 2-yr half-life) |
| `win_rate`, `form_last5` | Win rate and last-5 points average (recency ≥ 2022) |
| `q_pts_per`, `q_win_rate`| Qualifier-only weighted stats |
| `pagerank` | 10-iteration PageRank on the head-to-head win-graph (WC teams only) |

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
0.15 × attack_str       (w_gf rescaled)
0.15 × defence_str      (−w_ga rescaled)
0.10 × form_sc
0.10 × pr_sc            (PageRank)
0.05 × q_sc             (qualifier points)
0.10 × player_attack_sc   (goals_p90 + xg_p90, rescaled)
0.05 × player_defence_sc  (tackles_p90 + interceptions_p90, rescaled)
```

Parameters affecting `team_strength`:

- **Weights (`team_strength_weights`)**: Two weight sets exist in `config.yaml` — `without_player_data` and `with_player_data`. These control the relative contribution of each normalized component (Elo, attack/defence, form, PageRank, qualifier stats, and optional player terms). Change these values to rebalance the composite score; each block should sum to 1.0.
- **Global rescaling (`global_rescale`)**: When `TRUE` (in `config.yaml`), component scores such as `elo_sc`, `attack_str`, `defence_str`, `form_sc`, `pr_sc`, and `q_sc` are rescaled against a global international pool rather than only the 48 finalists. Enabling this reduces regional inflation for teams from weak confederations. The repo `config.yaml` default is `true`; if no `config.yaml` is present, `fetch_data.R` falls back to its own internal default.
- **Shrinkage (`shrinkage_tau`)**: Empirical-Bayes shrinkage applied to weighted goals-for (`w_gf`) and goals-against (`w_ga`) before rescaling. Higher `shrinkage_tau` pulls small-sample teams toward the global mean, stabilising estimates for nations with few recent matches. Recommended default is `10`.
- **Player terms**: `player_attack_sc` and `player_defence_sc` are built from per-90 squad aggregates (`goals_p90`, `xg_p90`, `tackles_p90`, `interceptions_p90`), rescaled to 0–1. Teams without player coverage receive filled median values and are marked `has_player_data = FALSE` (see `player_nation_coverage.csv`).
- **Auditability**: `fetch_data.R` writes `wc2026_output/team_strength_contributions.csv`, decomposing each team's `team_strength` into its weighted components, and `simulate.R` turns this into the `team_strength_breakdown.png` chart — use either to inspect exactly which terms drive any given team's ranking.

---

## Models

| Model | Description | Use |
|---|---|---|
| **Elo** | Classic sequential rating from full match history | Prior strength estimate, input to `elo_sc` |
| **Bradley-Terry** | Paired-comparison MLE on recency-weighted match outcomes, restricted to WC-vs-WC matches | Win probability P(A beats B) based purely on historical results |
| **Poisson (mixed-effects, lme4)** | Random-intercept Poisson GLM on goals scored, with team attack/defence random effects, home advantage, and a player-stat modifier on the linear predictor | Expected goals per side → win/draw/loss probabilities and simulated scorelines |
| **Ensemble** | Weighted blend of the Bradley-Terry and Poisson win probabilities (`bt_weight` / `poisson_weight` in `config.yaml`, default 50/50) | Used for knockout-round match resolution |
| **brms** (optional, not wired into `simulate.R` by default) | Full Bayesian hierarchical Poisson with Stan | Posterior uncertainty, if you want to extend the pipeline |

If the Bradley-Terry fit fails (or there are fewer than 10 WC-vs-WC matches),
every team falls back to a proxy ability derived from `team_strength`. If the
Poisson `glmer` fit fails (or there are fewer than 30 match rows), goal
expectations fall back to a simpler formula based on `team_strength` and the
player-attack/defence modifiers directly — no random effects.

### How a match is actually decided

This differs between the group stage and the knockout stage:

- **Group stage**: each match's outcome (and scoreline) is sampled directly
  from the **Poisson model alone** — independent Poisson draws for each
  side's expected goals, with results nudged to stay consistent with the
  sampled win/draw/loss outcome.
- **Knockout stage**: the **ensemble** (Bradley-Terry + Poisson, blended per
  `ensemble.bt_weight`/`poisson_weight`) decides the win probability, and
  that probability is then compressed toward 50/50 by
  `ensemble.knockout_extra_time_compression` (default 0.6) to reflect the
  added variance of single-elimination, extra-time football.
- **Monte Carlo simulation** (`n_sim`, default 10,000 runs) replays the full
  48-team tournament — 12 groups → top 2 per group + 8 best third-placed
  teams → Round of 32 → knockouts — end to end, each time using fresh random
  draws from the above models, to build the final win-probability
  distribution in `win_probabilities.png` / `final_predictions.csv`.
- **Deterministic "most-likely path" bracket** (`bracket_tree.png`): a
  separate, non-random pass through the same structure that always takes the
  single most probable outcome at every match (using the official FIFA
  Round-of-32 third-place draw mapping when it can be scraped, with a greedy
  same-group-avoidance fallback otherwise). This is the "if the favourite
  always won" bracket — easier to sanity-check by eye than the Monte Carlo
  percentages, but a single deterministic path rather than a probability
  distribution.

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

**Radar chart error / radar skipped**
The radar plot needs at least 3 of the available component columns
(`elo_sc`, `attack_str`, `defence_str`, `form_sc`, `pr_sc`, `q_sc`,
`player_attack_sc`, `player_defence_sc`) and at least 2 teams with a nonzero
win probability. Run `fetch_data.R` first and confirm `team_features.csv`
contains these columns.

**World map shows grey countries / "iso3 codes not found"**
`simulate.R` joins on `iso_a3_eh` (not `iso_a3`) specifically because
`rnaturalearth` sets `iso_a3 = "-99"` for France, Norway, and a few others.
If a country still shows up grey, check the console warning it prints —
it lists exactly which ISO3 codes didn't match `world_sf$iso_a3_eh`, so you
can fix the mapping in the `wc_iso3` table rather than guessing.

**England and Scotland show the same colour on the map**
Expected — Natural Earth has no separate England/Scotland polygon, only a
single GBR shape. Whichever of the two is processed last in the `wc_iso3`
table "wins" the colour. This is a genuine limitation of a country-level
choropleth, not a bug.

**brms / Stan compilation fails on Windows**
Install [RTools 4.4](https://cran.r-project.org/bin/windows/Rtools/) and
verify with `pkgbuild::has_build_tools()`. Then re-run
`cmdstanr::install_cmdstan()`.

---

## Extending the pipeline

| Extension | Where |
|---|---|
| Add more player seasons | Drop additional `players_data-20XX_20YY.csv` into `data/` and stack them before aggregating in `fetch_data.R` |
| Injury adjustments | After loading `team_features.rds`, subtract from `team_strength` for key missing players before simulation |
| Live tournament updating | After each group match, append the result to `results.csv` and re-run `fetch_data.R` with `force_refresh <- TRUE` |
| Bayesian upgrade | Wire in a `brms` fit in `simulate.R` Block 3B in place of (or alongside) the `lme4` Poisson model (requires Stan) |
| Bookmaker calibration | Compare `final_predictions.csv` win probabilities with odds from football-data.co.uk and fit Platt scaling |
| Custom Round-of-32 draw | Edit the `apply_fifa_r32_mapping()` fallback in Block 4B of `simulate.R` if FIFA's official Annex C mapping changes or can't be scraped |