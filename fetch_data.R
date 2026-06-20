#!/usr/bin/env Rscript
################################################################################
##  fetch_data.R  –  WC 2026 Prediction Pipeline  (Data Layer)
##
##  READS (from data/ folder – local Kaggle CSVs):
##    data/results.csv          – martj42 international results
##                                cols: date, home_team, away_team, home_score,
##                                      away_score, tournament, city, country, neutral
##    data/goalscorers.csv      – martj42 goalscorers
##                                cols: date, home_team, away_team, team,
##                                      scorer, own_goal, penalty
##    data/shootouts.csv        – martj42 shootout outcomes
##                                cols: date, home_team, away_team, winner, first_shooter
##    data/former_names.csv     – martj42 historic name aliases
##                                cols: current, former, start_date, end_date
##    data/players_data_light-2024_2025.csv  (or full version)
##                              – hubertsidorowicz FBref-sourced player stats
##                                cols: Player, Nation, Pos, Squad, Comp, Age,
##                                      Born, MP, Starts, Min, 90s,
##                                      Gls, Ast, G+A, xG, xAG, npxG,
##                                      Tkl, TklW, Blocks, Int, Clr, Err,
##                                      (+ many more in full version)
##
##  WRITES (to wc2026_output/ ):
##    all_results_parsed.rds    – recency-weighted match results
##    team_features.rds         – team-level feature matrix (48 WC teams)
##    player_agg.rds            – squad-level player aggregates (per 90)
##    player_nation_coverage.csv– which WC nations have player data
##    data_proof.csv            – diagnostic: row counts & feature sources
##    data_diagnostics.txt      – text coverage report
##
##  All downstream model/simulation code reads only the .rds files above.
################################################################################

## ── USER SETTINGS (overridable by config.yaml) ──────────────────────────────
# Defaults; if a `config.yaml` exists in the repo root, its values will
# override these. This lets users tune the pipeline without editing code.
cfg <- list()
if (file.exists("config.yaml")) {
  if (!requireNamespace("yaml", quietly = TRUE))
    install.packages("yaml", repos = "https://cloud.r-project.org")
  cfg <- yaml::read_yaml("config.yaml")
}

DATA_DIR  <- if (!is.null(cfg$paths$data_dir)) cfg$paths$data_dir else "data"
OUT_DIR   <- if (!is.null(cfg$paths$output_dir)) cfg$paths$output_dir else "wc2026_output"
CUTOFF_YR <- if (!is.null(cfg$recency$cutoff_year)) cfg$recency$cutoff_year else 2022
ELO_K     <- if (!is.null(cfg$elo$elo_k)) cfg$elo$elo_k else 30
ELO_START <- if (!is.null(cfg$elo$elo_start)) cfg$elo$elo_start else 1500

# Extra tunables used further down
HOME_ADVANTAGE   <- if (!is.null(cfg$elo$home_advantage)) cfg$elo$home_advantage else 100
half_life_days  <- if (!is.null(cfg$recency$half_life_days)) cfg$recency$half_life_days else 730
shrinkage_tau    <- if (!is.null(cfg$rescaling$shrinkage_tau)) cfg$rescaling$shrinkage_tau else 5
global_rescale   <- if (!is.null(cfg$rescaling$global_rescale)) cfg$rescaling$global_rescale else TRUE

## Competition weights (fallbacks match previous behaviour)
comp_w_wc_finals   <- if (!is.null(cfg$competition_weights$world_cup_finals)) cfg$competition_weights$world_cup_finals else 2.0
comp_w_wc_qualifier<- if (!is.null(cfg$competition_weights$world_cup_qualifier)) cfg$competition_weights$world_cup_qualifier else 1.1
comp_w_major<- if (!is.null(cfg$competition_weights$major_continental_or_nations_league)) cfg$competition_weights$major_continental_or_nations_league else 1.2
comp_w_frd  <- if (!is.null(cfg$competition_weights$friendly)) cfg$competition_weights$friendly else 0.6
comp_w_def  <- if (!is.null(cfg$competition_weights$other_default)) cfg$competition_weights$other_default else 0.9

# Team-strength weights (use with_player_data if available)
weights_no_players  <- if (!is.null(cfg$team_strength_weights$without_player_data)) cfg$team_strength_weights$without_player_data else list(elo=0.35, attack=0.20, defence=0.15, form_last5=0.15, pagerank=0.10, qualifier=0.05)
weights_with_players<- if (!is.null(cfg$team_strength_weights$with_player_data)) cfg$team_strength_weights$with_player_data else list(elo=0.30, attack=0.15, defence=0.15, form_last5=0.10, pagerank=0.10, qualifier=0.05, player_attack=0.10, player_defence=0.05)

dir.create(OUT_DIR,  showWarnings = FALSE)
dir.create(DATA_DIR, showWarnings = FALSE)

## ── PACKAGES ──────────────────────────────────────────────────────────────────
pkgs <- c("dplyr","tidyr","purrr","stringr","readr","tibble","lubridate",
          "scales","janitor","yaml")
new  <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

cat("══════════════════════════════════════════════════\n")
cat(" WC 2026 Prediction – Data Fetch & Feature Build\n")
cat("══════════════════════════════════════════════════\n\n")

################################################################################
##  SECTION 0 – Team name canonicalisation
##  All names throughout the pipeline are normalised to these canonical strings.
################################################################################

CANONICAL_MAP <- c(
  ## Kaggle / FBref variants → canonical
  "USA"                          = "United States",
  "United States of America"     = "United States",
  "US"                           = "United States",
  "Korea Republic"               = "South Korea",
  "Republic of Korea"            = "South Korea",
  "Curacao"                      = "Curaçao",
  "Curazao"                      = "Curaçao",
  "Turkey"                       = "Türkiye",
  "Ivory Coast"                  = "Côte d'Ivoire",
  "Cote d'Ivoire"                = "Côte d'Ivoire",
  "Cote dIvoire"                 = "Côte d'Ivoire",
  "DR Congo"                     = "Congo DR",
  "Democratic Republic of Congo" = "Congo DR",
  "Congo"                        = "Congo DR",        # only if DR qualifier confirmed
  "DRC"                          = "Congo DR",
  "Bosnia-Herzegovina"           = "Bosnia and Herzegovina",
  "Bosnia & Herzegovina"         = "Bosnia and Herzegovina",
  "Bosnia-Herzegowina"           = "Bosnia and Herzegovina",
  "Cape Verde"                   = "Cabo Verde",
  "Czech Republic"               = "Czechia",
  "Northern Ireland"             = "Northern Ireland", # not qualified – keep distinct
  "IR Iran"                      = "Iran",
  "Trinidad and Tobago"          = "Trinidad and Tobago",
  ## FBref Nation column (2-letter codes resolved separately in player section)
  "ENG"  = "England",  "FRA" = "France",   "ESP" = "Spain",
  "GER"  = "Germany",  "BRA" = "Brazil",   "ARG" = "Argentina",
  "POR"  = "Portugal", "NED" = "Netherlands","BEL" = "Belgium",
  "SUI"  = "Switzerland","CRO" = "Croatia", "AUT" = "Austria",
  "SRB"  = "Serbia",   "DEN" = "Denmark",  "UKR" = "Ukraine",
  "TUR"  = "Türkiye",  "SWE" = "Sweden",   "NOR" = "Norway",
  "BIH"  = "Bosnia and Herzegovina","CZE" = "Czechia",
  "URU"  = "Uruguay",  "COL" = "Colombia", "ECU" = "Ecuador",
  "PAR"  = "Paraguay", "MAR" = "Morocco",  "SEN" = "Senegal",
  "EGY"  = "Egypt",    "RSA" = "South Africa","ALG" = "Algeria",
  "GHA"  = "Ghana",    "CPV" = "Cabo Verde","TUN" = "Tunisia",
  "CIV"  = "Côte d'Ivoire","COD" = "Congo DR",
  "JPN"  = "Japan",    "KOR" = "South Korea","IRN" = "Iran",
  "KSA"  = "Saudi Arabia","AUS" = "Australia",
  "JOR"  = "Jordan",   "UZB" = "Uzbekistan","IRQ" = "Iraq",
  "QAT"  = "Qatar",    "USA" = "United States","MEX" = "Mexico",
  "CAN"  = "Canada",   "PAN" = "Panama",   "SCO" = "Scotland",
  "HAI"  = "Haiti",    "CUW" = "Curaçao"
)

canon <- function(x) dplyr::recode(as.character(x), !!!CANONICAL_MAP, .default = as.character(x))

## Official 48 WC 2026 qualified nations (canonical names)
WC_TEAMS <- unique(canon(c(
  ## UEFA (16)
  "France","Spain","England","Portugal","Germany","Netherlands","Belgium",
  "Switzerland","Croatia","Austria","Serbia","Denmark","Ukraine","Türkiye",
  "Sweden","Norway","Bosnia and Herzegovina","Czechia",
  ## CONMEBOL (6)
  "Brazil","Argentina","Colombia","Uruguay","Ecuador","Paraguay",
  ## CAF (10)
  "Morocco","Senegal","Egypt","South Africa","Algeria",
  "Ghana","Cabo Verde","Tunisia","Côte d'Ivoire","Congo DR",
  ## AFC (8 + host Qatar counts here)
  "Japan","South Korea","Iran","Saudi Arabia","Australia",
  "Jordan","Uzbekistan","Iraq","Qatar",
  ## CONCACAF (6 incl. 3 hosts)
  "United States","Mexico","Canada","Panama","Scotland","Haiti",
  ## Intercontinental
  "Curaçao"
)))

## Official groups (confirmed post March 31 2026)
WC_GROUPS <- list(
  A = canon(c("Mexico",       "South Korea",          "South Africa",  "Czechia")),
  B = canon(c("Canada",       "Switzerland",          "Qatar",         "Bosnia and Herzegovina")),
  C = canon(c("Brazil",       "Morocco",              "Scotland",      "Haiti")),
  D = canon(c("United States","Australia",            "Paraguay",      "Türkiye")),
  E = canon(c("Germany",      "Ecuador",              "Côte d'Ivoire", "Curaçao")),
  F = canon(c("Netherlands",  "Japan",                "Tunisia",       "Sweden")),
  G = canon(c("Belgium",      "Iran",                 "Egypt",         "New Zealand")),  # placeholder if NZ confirmed
  H = canon(c("Spain",        "Uruguay",              "Saudi Arabia",  "Cabo Verde")),
  I = canon(c("France",       "Senegal",              "Norway",        "Iraq")),
  J = canon(c("Argentina",    "Austria",              "Algeria",       "Jordan")),
  K = canon(c("Portugal",     "Colombia",             "Uzbekistan",    "Congo DR")),
  L = canon(c("England",      "Croatia",              "Panama",        "Ghana"))
)

## Cross-check: every group team must be in WC_TEAMS
group_teams_all <- unique(unlist(WC_GROUPS))
unknown_in_groups <- setdiff(group_teams_all, WC_TEAMS)
if (length(unknown_in_groups) > 0) {
  cat("⚠  Teams in groups but not in WC_TEAMS list – adding them:\n   ",
      paste(unknown_in_groups, collapse=", "), "\n")
  WC_TEAMS <- unique(c(WC_TEAMS, unknown_in_groups))
}
cat("✓  WC_TEAMS:", length(WC_TEAMS), "teams\n")
cat("✓  WC_GROUPS:", length(WC_GROUPS), "groups,",
    length(group_teams_all), "unique group teams\n\n")


################################################################################
##  SECTION 1 – Load & validate required CSV files
################################################################################

cat("── Section 1  Loading CSV files ──────────────────\n")

required_files <- list(
  results     = file.path(DATA_DIR, "results.csv"),
  goalscorers = file.path(DATA_DIR, "goalscorers.csv"),
  shootouts   = file.path(DATA_DIR, "shootouts.csv"),
  former_names= file.path(DATA_DIR, "former_names.csv"),
  ## player file – try light version first, fall back to full
  players     = {
    pf <- file.path(DATA_DIR, "players_data_light-2024_2025.csv")
    if (!file.exists(pf)) file.path(DATA_DIR, "players_data-2024_2025.csv")
    else pf
  }
)

missing_files <- names(required_files)[!file.exists(unlist(required_files))]
if (length(missing_files) > 0) {
  cat("⚠  Missing files (will produce empty frames for those sections):\n   ",
      paste(missing_files, collapse = ", "), "\n")
}

## Helper: safe CSV reader with informative message
read_safe <- function(path, type = "d") {
  if (!file.exists(path)) {
    cat("  SKIP (not found):", path, "\n")
    return(NULL)
  }
  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  cat("  Loaded", formatC(nrow(df), format="d", big.mark=","), "rows:",
      basename(path), "\n")
  df
}

raw_results      <- read_safe(required_files$results)
raw_goalscorers  <- read_safe(required_files$goalscorers)
raw_shootouts    <- read_safe(required_files$shootouts)
raw_former_names <- read_safe(required_files$former_names)
raw_players      <- read_safe(required_files$players)

## Validate column presence ------------------------------------------------
validate_cols <- function(df, required_cols, label) {
  if (is.null(df)) { cat("  SKIP validation:", label, "(no data)\n"); return(FALSE) }
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    cat("  ⚠ ", label, "missing cols:", paste(missing, collapse=", "), "\n")
    cat("     Available:", paste(names(df), collapse=", "), "\n")
    return(FALSE)
  }
  cat("  ✓ ", label, "columns OK\n")
  TRUE
}

ok_results <- validate_cols(raw_results,
  c("date","home_team","away_team","home_score","away_score","tournament"),
  "results.csv")
ok_goalscorers <- validate_cols(raw_goalscorers,
  c("date","home_team","away_team","team","scorer"),
  "goalscorers.csv")
ok_players <- validate_cols(raw_players,
  c("Player","Nation","Pos","Squad","Min","Gls","Ast"),
  "players_data_light")
cat("\n")


################################################################################
##  SECTION 2 – Build former-names lookup  (Kaggle martj42 former_names.csv)
##  Allows us to reconcile historic team names → current canonical name.
##  former_names.csv cols: current, former, start_date, end_date
################################################################################

cat("── Section 2  Former-names lookup ────────────────\n")

if (!is.null(raw_former_names)) {
  former_lookup <- raw_former_names |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::select(current, former) |>
    dplyr::mutate(current = canon(current)) |>
    dplyr::distinct()

  ## Extend CANONICAL_MAP with former-name entries
  extra_map <- setNames(former_lookup$current, former_lookup$former)
  ## Only add entries not already in CANONICAL_MAP to avoid override
  extra_map <- extra_map[!names(extra_map) %in% names(CANONICAL_MAP)]
  CANONICAL_MAP <- c(CANONICAL_MAP, extra_map)

  ## Redefine canon() with expanded map
  canon <- function(x) dplyr::recode(as.character(x), !!!CANONICAL_MAP, .default = as.character(x))
  cat("  Added", length(extra_map), "former-name aliases → canon() updated\n\n")
} else {
  cat("  former_names.csv not available – skipping\n\n")
}


################################################################################
##  SECTION 3 – Process match results  (martj42 results.csv)
##
##  results.csv columns:
##    date, home_team, away_team, home_score, away_score,
##    tournament, city, country, neutral
##
##  Strategy:
##   • Use ALL historical matches for Elo computation (from earliest date).
##   • Use only matches from CUTOFF_YR onward for recency-weighted features.
##   • Qualifier / WC matches get higher weight than friendlies.
##   • Exponential time decay: half-life = 2 years.
################################################################################

cat("── Section 3  Match results processing ───────────\n")

if (!is.null(raw_results) && ok_results) {

  matches <- raw_results |>
    janitor::clean_names() |>
    dplyr::rename(
      home_goals = home_score,
      away_goals = away_score,
      competition = tournament
    ) |>
    dplyr::mutate(
      date       = as.Date(date),
      home_team  = canon(home_team),
      away_team  = canon(away_team),
      home_goals = as.integer(home_goals),
      away_goals = as.integer(away_goals),
      neutral    = as.logical(neutral)
    ) |>
    dplyr::filter(!is.na(date), !is.na(home_goals), !is.na(away_goals)) |>
    dplyr::arrange(date)

  cat("  Total matches after cleaning:", format(nrow(matches), big.mark=","), "\n")
  cat("  Date range:", as.character(min(matches$date)), "–",
      as.character(max(matches$date)), "\n")

  ## Enriched with shootout winner (for correct Elo updates in KO matches)
  if (!is.null(raw_shootouts)) {
    so <- raw_shootouts |>
      janitor::clean_names() |>
      dplyr::mutate(date = as.Date(date),
                    home_team = canon(home_team),
                    away_team = canon(away_team),
                    winner    = canon(winner)) |>
      dplyr::select(date, home_team, away_team, shootout_winner = winner)
    matches <- dplyr::left_join(matches, so, by = c("date","home_team","away_team"))
  } else {
    matches$shootout_winner <- NA_character_
  }

  ## ── Elo calculation (full history) ───────────────────────────────────────
  ## Uses all matches chronologically; neutral-venue matches have no home boost.
  ## HOME_ADVANTAGE comes from config (or default above)
  HOME_ADVANTAGE <- HOME_ADVANTAGE

  elo_ratings <- tibble::tibble(
    team = unique(c(matches$home_team, matches$away_team)),
    elo  = ELO_START
  )

  elo_expected <- function(r_a, r_b) 1 / (1 + 10^((r_b - r_a) / 400))

  cat("  Computing Elo ratings from full history …")
  for (i in seq_len(nrow(matches))) {
    m  <- matches[i, ]
    ha <- if (isTRUE(m$neutral)) 0 else HOME_ADVANTAGE
    ra <- elo_ratings$elo[elo_ratings$team == m$home_team]
    rb <- elo_ratings$elo[elo_ratings$team == m$away_team]
    if (length(ra) == 0) { elo_ratings <- dplyr::add_row(elo_ratings, team=m$home_team, elo=ELO_START); ra <- ELO_START }
    if (length(rb) == 0) { elo_ratings <- dplyr::add_row(elo_ratings, team=m$away_team, elo=ELO_START); rb <- ELO_START }

    ## actual scores (1 = win, 0.5 = draw, 0 = loss; shootout = 0.5+epsilon)
    if (!is.na(m$shootout_winner)) {
      s_a <- if (m$shootout_winner == m$home_team) 0.55 else 0.45
    } else {
      s_a <- dplyr::case_when(m$home_goals > m$away_goals ~ 1,
                               m$home_goals == m$away_goals ~ 0.5,
                               TRUE ~ 0)
    }
    e_a <- elo_expected(ra + ha, rb)
    new_ra <- ra + ELO_K * (s_a - e_a)
    new_rb <- rb + ELO_K * ((1 - s_a) - (1 - e_a))
    elo_ratings$elo[elo_ratings$team == m$home_team] <- new_ra
    elo_ratings$elo[elo_ratings$team == m$away_team] <- new_rb
  }
  cat(" done\n")

  ## ── Recent matches (CUTOFF_YR onward) for weighted features ──────────────
  recent <- matches |>
    dplyr::filter(lubridate::year(date) >= CUTOFF_YR)

  cat("  Recent matches (≥", CUTOFF_YR, "):", format(nrow(recent), big.mark=","), "\n")

  ## Competition weight
  recent <- recent |>
    dplyr::mutate(
      days_ago    = as.numeric(Sys.Date() - date),
      time_weight = exp(-days_ago / half_life_days),
      comp_weight = dplyr::case_when(
        ## World Cup qualifiers should be weighted differently from finals
        stringr::str_detect(competition, "(?i)qualifier") ~ comp_w_wc_qualifier,
        ## If it's a World Cup match but not a qualifier, treat as finals
        stringr::str_detect(competition, "(?i)World Cup") & !stringr::str_detect(competition, "(?i)qualifier") ~ comp_w_wc_finals,
        stringr::str_detect(competition,
          "(?i)UEFA Euro|Copa America|Africa Cup|Asian Cup|CONCACAF|Nations League|\
           Gold Cup|AFCON|Confederations") ~ comp_w_major,
        stringr::str_detect(competition, "(?i)friendly") ~ comp_w_frd,
        TRUE ~ comp_w_def
      ),
      match_weight = time_weight * comp_weight
    )

  ## Save parsed all_results (used by model script)
  saveRDS(recent, file.path(OUT_DIR, "all_results_parsed.rds"))
  cat("  Saved: all_results_parsed.rds\n\n")

} else {
  cat("  ⚠  results.csv not available – match features will be FIFA-proxy only\n\n")
  matches <- tibble::tibble()
  recent  <- tibble::tibble()
  elo_ratings <- tibble::tibble(team = WC_TEAMS, elo = ELO_START)
}


################################################################################
##  SECTION 4 – Goalscorer data  (martj42 goalscorers.csv)
##  Used to build per-team goal-contribution features and recency goal counts.
##  goalscorers.csv: date, home_team, away_team, team, scorer, own_goal, penalty
################################################################################

cat("── Section 4  Goalscorer features ────────────────\n")

if (!is.null(raw_goalscorers) && ok_goalscorers) {
  scorers <- raw_goalscorers |>
    janitor::clean_names() |>
    dplyr::mutate(
      date      = as.Date(date),
      home_team = canon(home_team),
      away_team = canon(away_team),
      team      = canon(team),
      own_goal  = as.logical(own_goal),
      penalty   = as.logical(penalty)
    ) |>
    dplyr::filter(!is.na(date), lubridate::year(date) >= CUTOFF_YR,
                  !isTRUE(own_goal))  # exclude own goals

  ## Goals per team (recent, non-OG)
  team_goals <- scorers |>
    dplyr::filter(team %in% WC_TEAMS) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      total_goals_since_cutoff = n(),
      pen_goals   = sum(penalty, na.rm = TRUE),
      open_play_g = total_goals_since_cutoff - pen_goals
    )

  cat("  Goal records (WC teams, ≥", CUTOFF_YR, "):",
      nrow(team_goals), "teams with goal data\n\n")
} else {
  team_goals <- tibble::tibble(
    team = character(), total_goals_since_cutoff = integer(),
    pen_goals = integer(), open_play_g = integer()
  )
  cat("  goalscorers.csv not available\n\n")
}


################################################################################
##  SECTION 5 – Team feature engineering
##  Combines: Elo, recent results, qualifier stats, goalscorer data.
##  All features for WC_TEAMS only (exactly 48 teams in the simulation).
################################################################################

cat("── Section 5  Team feature engineering ───────────\n")

has_results <- nrow(recent) >= 10

if (has_results) {

  ## Long format: one row per team per match. Build both a global view
  ## (long_all) and a WC-only view (long). The global view is used when
  ## `global_rescale` is enabled in config.yaml so rescaling uses all
  ## teams in results.csv rather than just the 48 finalists.
  long_all <- dplyr::bind_rows(
    recent |> dplyr::transmute(
      date, match_weight, competition,
      team = home_team, opponent = away_team,
      gf = home_goals, ga = away_goals, neutral),
    recent |> dplyr::transmute(
      date, match_weight, competition,
      team = away_team, opponent = home_team,
      gf = away_goals, ga = home_goals, neutral)
  ) |>
    dplyr::mutate(
      result    = dplyr::case_when(gf > ga ~ "W", gf < ga ~ "L", TRUE ~ "D"),
      points    = dplyr::case_when(result == "W" ~ 3L,
                                    result == "D" ~ 1L, TRUE ~ 0L),
      goal_diff = gf - ga
    )

  ## WC-only view used for the 48 finalists
  long <- long_all |> dplyr::filter(team %in% WC_TEAMS)

  ## Pre-compute form_last5 BEFORE the main summarise to avoid
  ## dplyr::cur_data() which is deprecated and causes vctrs size errors
  ## in dplyr >= 1.1. Strategy: sort by date, keep last 5 rows per team,
  ## average points.
  form_last5_df <- long |>
    dplyr::arrange(date) |>
    dplyr::group_by(team) |>
    dplyr::slice_tail(n = 5) |>
    dplyr::summarise(form_last5 = mean(points), .groups = "drop")

  if (global_rescale) {
    form_last5_df_all <- long_all |>
      dplyr::arrange(date) |>
      dplyr::group_by(team) |>
      dplyr::slice_tail(n = 5) |>
      dplyr::summarise(form_last5 = mean(points), .groups = "drop")
  }

  ## Core weighted aggregates (WC-only)
  tf_core <- long |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      n_matches    = n(),
      weighted_pts = sum(match_weight * points),
      w_gf         = sum(match_weight * gf) / sum(match_weight),
      w_ga         = sum(match_weight * ga) / sum(match_weight),
      w_gd         = w_gf - w_ga,
      win_rate     = mean(result == "W"),
      draw_rate    = mean(result == "D"),
      cs_rate      = mean(ga == 0),
      .groups      = "drop"
    ) |>
    dplyr::left_join(form_last5_df, by = "team")

  if (global_rescale) {
    tf_core_all <- long_all |>
      dplyr::group_by(team) |>
      dplyr::summarise(
        n_matches    = n(),
        weighted_pts = sum(match_weight * points),
        w_gf         = sum(match_weight * gf) / sum(match_weight),
        w_ga         = sum(match_weight * ga) / sum(match_weight),
        w_gd         = w_gf - w_ga,
        win_rate     = mean(result == "W"),
        draw_rate    = mean(result == "D"),
        cs_rate      = mean(ga == 0),
        .groups      = "drop"
      )
  }

  ## Qualifier-specific stats
  tf_qual <- long |>
    dplyr::filter(stringr::str_detect(competition,
                  "(?i)qualifier|World Cup|WC qual")) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      q_n       = n(),
      q_pts_per = mean(points),
      q_gd_per  = mean(goal_diff),
      q_win_rate= mean(result == "W")
    )

  ## Head-to-head network: PageRank-based strength
  ## Build edge list: A beat B → weighted directed edge A→B
  h2h_edges <- dplyr::bind_rows(
    recent |>
      dplyr::filter(home_team %in% WC_TEAMS, away_team %in% WC_TEAMS,
                    home_goals > away_goals) |>
      dplyr::transmute(from = home_team, to = away_team, w = match_weight),
    recent |>
      dplyr::filter(home_team %in% WC_TEAMS, away_team %in% WC_TEAMS,
                    away_goals > home_goals) |>
      dplyr::transmute(from = away_team, to = home_team, w = match_weight)
  )

  ## Simple PageRank approximation (10 iterations)
  if (nrow(h2h_edges) > 0) {
    pr_teams <- WC_TEAMS
    pr_score <- setNames(rep(1 / length(pr_teams), length(pr_teams)), pr_teams)
    for (iter in 1:10) {
      new_pr <- rep(0, length(pr_teams))
      names(new_pr) <- pr_teams
      for (t in pr_teams) {
        wins_over_t <- h2h_edges |> dplyr::filter(to == t)
        if (nrow(wins_over_t) == 0) next
        for (j in seq_len(nrow(wins_over_t))) {
          src <- wins_over_t$from[j]
          wt  <- wins_over_t$w[j]
          out_w <- sum(h2h_edges$w[h2h_edges$from == src])
          if (out_w > 0 && src %in% names(pr_score))
            new_pr[t] <- new_pr[t] + 0.85 * pr_score[src] * wt / out_w
        }
        new_pr[t] <- new_pr[t] + 0.15 / length(pr_teams)
      }
      pr_score <- new_pr / sum(new_pr)
    }
    pagerank_df <- tibble::tibble(team = names(pr_score),
                                  pagerank = as.numeric(pr_score))
  } else {
    pagerank_df <- tibble::tibble(team = WC_TEAMS,
                                  pagerank = 1 / length(WC_TEAMS))
  }

  ## Assemble: scaffold from WC_TEAMS so all 48 always have a row
  team_features <- tibble::tibble(team = WC_TEAMS) |>
    dplyr::left_join(tf_core,    by = "team") |>
    dplyr::left_join(tf_qual,    by = "team") |>
    dplyr::left_join(pagerank_df,by = "team") |>
    dplyr::left_join(
      elo_ratings |> dplyr::filter(team %in% WC_TEAMS) |>
        dplyr::rename(elo_current = elo),
      by = "team"
    ) |>
    dplyr::left_join(team_goals, by = "team") |>
    ## Fill NA with neutral defaults (team has no recent data → average-ish)
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)),
      n_matches    = tidyr::replace_na(n_matches, 0L),
      elo_current  = tidyr::replace_na(elo_current, ELO_START),
      pagerank     = tidyr::replace_na(pagerank, 1 / length(WC_TEAMS))
    )

  ## Normalised composite strength (0 → 1)
  ## Apply mild empirical-Bayes shrinkage to goals-per-game estimates
  ## to reduce over-weighting of extremes from small or unbalanced samples.
  if (global_rescale && exists("tf_core_all")) {
    global_w_gf <- mean(tf_core_all$w_gf, na.rm = TRUE)
    global_w_ga <- mean(tf_core_all$w_ga, na.rm = TRUE)
  } else {
    global_w_gf <- mean(team_features$w_gf, na.rm = TRUE)
    global_w_ga <- mean(team_features$w_ga, na.rm = TRUE)
  }
  tau <- shrinkage_tau   # shrinkage strength (tunable)

  ## If global rescale requested, precompute ranges for rescaling after
  ## empirical Bayes shrinkage so we rescale against the global distribution.
  if (global_rescale && exists("tf_core_all")) {
    shr_w_gf_all <- (tf_core_all$w_gf * tf_core_all$n_matches + global_w_gf * tau) / (tf_core_all$n_matches + tau)
    shr_w_ga_all <- (tf_core_all$w_ga * tf_core_all$n_matches + global_w_ga * tau) / (tf_core_all$n_matches + tau)
    form_last5_range_all <- if (exists("form_last5_df_all")) range(form_last5_df_all$form_last5, na.rm = TRUE) else c(0,1)
    elo_range_all <- range(elo_ratings$elo, na.rm = TRUE)
  }

  team_features <- team_features |>
    dplyr::mutate(
      shr_w_gf = (w_gf * n_matches + global_w_gf * tau) / (n_matches + tau),
      shr_w_ga = (w_ga * n_matches + global_w_ga * tau) / (n_matches + tau)
    ) |>
    dplyr::mutate(
      elo_sc = if (global_rescale && exists("elo_range_all"))
                 scales::rescale(elo_current, to = c(0,1), from = elo_range_all)
               else scales::rescale(elo_current, to = c(0,1)),
      attack_str = if (global_rescale && exists("shr_w_gf_all"))
                     scales::rescale(shr_w_gf, to = c(0,1), from = range(shr_w_gf_all, na.rm = TRUE))
                   else scales::rescale(shr_w_gf, to = c(0,1)),
      defence_str = if (global_rescale && exists("shr_w_ga_all"))
                      scales::rescale(-shr_w_ga, to = c(0,1), from = range(-shr_w_ga_all, na.rm = TRUE))
                    else scales::rescale(-shr_w_ga, to = c(0,1)),
      form_sc = if (global_rescale && exists("form_last5_range_all"))
                  scales::rescale(form_last5, to = c(0,1), from = form_last5_range_all)
                else scales::rescale(form_last5, to = c(0,1)),
      pr_sc = scales::rescale(pagerank, to = c(0,1)),
      q_sc  = scales::rescale(q_pts_per, to = c(0,1)),
      ## Weighted composite (tunable weights)
      team_strength = as.numeric(weights_no_players$elo) * elo_sc    +
              as.numeric(weights_no_players$attack) * attack_str +
              as.numeric(weights_no_players$defence) * defence_str +
              as.numeric(weights_no_players$form_last5) * form_sc   +
              as.numeric(weights_no_players$pagerank) * pr_sc     +
              as.numeric(weights_no_players$qualifier) * q_sc
    ) |>
    dplyr::select(-shr_w_gf, -shr_w_ga)

  cat("  Team features built from match data.\n")
  cat("  Teams with ≥5 recent matches:",
      sum(team_features$n_matches >= 5, na.rm = TRUE), "/", nrow(team_features), "\n")
  feature_source <- "match_data + Elo + PageRank"

} else {

  cat("  ⚠  No recent matches – building features from Elo proxy only\n")
  team_features <- tibble::tibble(team = WC_TEAMS) |>
    dplyr::left_join(
      elo_ratings |> dplyr::filter(team %in% WC_TEAMS) |>
        dplyr::rename(elo_current = elo),
      by = "team"
    ) |>
    dplyr::mutate(
      elo_current   = tidyr::replace_na(elo_current, ELO_START),
      n_matches     = 0L,
      weighted_pts  = 0, w_gf = 0, w_ga = 0, w_gd = 0,
      win_rate      = 0, draw_rate = 0, form_last5 = 0, cs_rate = 0,
      q_n = 0L, q_pts_per = 0, q_gd_per = 0, q_win_rate = 0,
      pagerank      = 1 / length(WC_TEAMS),
      elo_sc        = scales::rescale(elo_current, to = c(0,1)),
      attack_str    = 0.5, defence_str = 0.5, form_sc = 0.5,
      pr_sc         = 0.5, q_sc        = 0.5,
      team_strength = elo_sc
    )
  feature_source <- "Elo_only"
}

cat("\n")


################################################################################
##  SECTION 6 – Player statistics  (hubertsidorowicz Kaggle dataset)
##
##  players_data_light-2024_2025.csv columns (key ones used here):
##    Player, Nation, Pos, Squad, Comp, Age, Born,
##    MP, Starts, Min, 90s,
##    Gls, Ast, G+A, xG, xAG, npxG,
##    Tkl, TklW, Blocks, Int, Clr, Err
##
##  Nation column is a 3-letter FBref code (ENG, FRA, …) or sometimes
##  written as "eng ENG", "fr FRA" – we parse the uppercase code.
##
##  Strategy:
##   1. Parse Nation → canonical country name.
##   2. Keep only players whose Nation maps to one of the 48 WC_TEAMS.
##   3. Aggregate per-90 stats per team.
##   4. Blend with team_features.
################################################################################

cat("── Section 6  Player statistics ──────────────────\n")

if (!is.null(raw_players) && ok_players) {

  players <- raw_players |>
    janitor::clean_names() |>
    ## Rename common spelling variants
    dplyr::rename_with(~ dplyr::case_when(
      . == "gls"  ~ "goals",
      . == "ast"  ~ "assists",
      . == "min"  ~ "minutes",
      . == "xg"   ~ "xg",
      . == "xag"  ~ "xag",
      . == "tkl"  ~ "tackles",
      . == "int"  ~ "interceptions",
      . == "x90s" ~ "x90s",
      TRUE ~ .
    )) |>
    ## Parse Nation: FBref format is "xx NNN" (flag + 3-letter code) or just "NNN"
    dplyr::mutate(
      nation_raw  = as.character(nation),
      ## Extract the uppercase 3-letter code (last word of the field)
      nation_code = stringr::str_extract(nation_raw, "[A-Z]{2,3}$"),
      ## Map to canonical country name
      nation_canon = canon(nation_code),
      ## Also try the full string (some datasets write full country name)
      nation_full  = dplyr::if_else(
        nation_canon == nation_code,          # mapping didn't change it (still code)
        canon(stringr::str_trim(nation_raw)), # try full string
        nation_canon
      ),
      ## Minutes as numeric
      minutes = suppressWarnings(
        as.numeric(stringr::str_remove_all(as.character(minutes), ","))
      )
    ) |>
    dplyr::filter(!is.na(minutes), minutes > 0)

  ## Resolve to WC nation
  players <- players |>
    dplyr::mutate(
      wc_nation = dplyr::if_else(nation_full %in% WC_TEAMS, nation_full, NA_character_)
    )

  ## Coverage report
  wc_players <- dplyr::filter(players, !is.na(wc_nation))
  nations_covered <- sort(unique(wc_players$wc_nation))
  cat("  Players in dataset:", format(nrow(players), big.mark=","), "\n")
  cat("  WC-nation players: ", format(nrow(wc_players), big.mark=","), "\n")
  cat("  WC nations covered:", length(nations_covered), "/", length(WC_TEAMS), "\n")
  cat("  Nations NOT covered:",
      paste(setdiff(WC_TEAMS, nations_covered), collapse=", "), "\n\n")

  ## Save coverage table
  coverage_tbl <- tibble::tibble(
    team           = WC_TEAMS,
    player_data    = WC_TEAMS %in% nations_covered,
    n_players      = sapply(WC_TEAMS, function(t)
                       sum(wc_players$wc_nation == t, na.rm = TRUE))
  )
  readr::write_csv(coverage_tbl,
                   file.path(OUT_DIR, "player_nation_coverage.csv"))
  cat("  Saved: player_nation_coverage.csv\n")

  ## Per-90 aggregates per squad
  ## Compute per-90 rates for each numeric stat, then average within team
  ninety_base <- if ("x90s" %in% names(wc_players)) "x90s" else NA_character_

  player_agg <- wc_players |>
    dplyr::group_by(wc_nation) |>
    dplyr::summarise(
      n_players       = n(),
      total_min       = sum(minutes, na.rm = TRUE),
      ## Weighted average per-90 (weight by minutes played)
      goals_p90       = sum(as.numeric(goals),        na.rm=TRUE) / (sum(minutes,na.rm=TRUE)/90),
      assists_p90     = sum(as.numeric(assists),       na.rm=TRUE) / (sum(minutes,na.rm=TRUE)/90),
      xg_p90          = if ("xg" %in% names(wc_players))
                          sum(as.numeric(xg), na.rm=TRUE) / (sum(minutes,na.rm=TRUE)/90)
                        else NA_real_,
      xag_p90         = if ("xag" %in% names(wc_players))
                          sum(as.numeric(xag), na.rm=TRUE) / (sum(minutes,na.rm=TRUE)/90)
                        else NA_real_,
      tackles_p90     = if ("tackles" %in% names(wc_players))
                          sum(as.numeric(tackles), na.rm=TRUE) / (sum(minutes,na.rm=TRUE)/90)
                        else NA_real_,
      interceptions_p90 = if ("interceptions" %in% names(wc_players))
                            sum(as.numeric(interceptions), na.rm=TRUE) / (sum(minutes,na.rm=TRUE)/90)
                          else NA_real_,
      ## Age diversity
      avg_age         = if ("age" %in% names(wc_players))
                          mean(suppressWarnings(as.numeric(age)), na.rm=TRUE)
                        else NA_real_
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(team = wc_nation)

  saveRDS(player_agg, file.path(OUT_DIR, "player_agg.rds"))
  cat("  Saved: player_agg.rds\n\n")

  ## Merge player aggregates into team_features
  player_str_cols <- c("goals_p90","assists_p90","xg_p90","tackles_p90",
                       "interceptions_p90")

  team_features <- team_features |>
    dplyr::left_join(player_agg |>
                       dplyr::select(team, n_players, total_min,
                                     dplyr::any_of(player_str_cols),
                                     dplyr::any_of("avg_age")),
                     by = "team") |>
    dplyr::mutate(
      has_player_data = !is.na(goals_p90),
      dplyr::across(dplyr::any_of(player_str_cols),
                    ~tidyr::replace_na(., median(., na.rm=TRUE))),
      ## Player quality composite score (rescaled 0-1)
      player_attack_sc  = scales::rescale(
        tidyr::replace_na(goals_p90, 0) + tidyr::replace_na(xg_p90, 0),
        to = c(0, 1)),
      player_defence_sc = scales::rescale(
        tidyr::replace_na(tackles_p90, 0) + tidyr::replace_na(interceptions_p90, 0),
        to = c(0, 1)),
      ## Update composite team_strength to include player quality (use config weights)
      team_strength = as.numeric(weights_with_players$elo) * elo_sc +
              as.numeric(weights_with_players$attack) * attack_str +
              as.numeric(weights_with_players$defence) * defence_str +
              as.numeric(weights_with_players$form_last5) * form_sc +
              as.numeric(weights_with_players$pagerank) * pr_sc +
              as.numeric(weights_with_players$qualifier) * q_sc +
              as.numeric(weights_with_players$player_attack) * player_attack_sc +
              as.numeric(weights_with_players$player_defence) * player_defence_sc
    )

  feature_source <- "match_data + Elo + PageRank + player_stats"
  cat("  team_strength updated with player quality scores\n")

} else {
  cat("  Player CSV not available – player features skipped\n")
  player_agg <- tibble::tibble()
  saveRDS(player_agg, file.path(OUT_DIR, "player_agg.rds"))
  team_features <- team_features |>
    dplyr::mutate(has_player_data = FALSE,
                  player_attack_sc = 0.5,
                  player_defence_sc = 0.5)
}

cat("\n")


################################################################################
##  SECTION 7 – Final validation & save
################################################################################

cat("── Section 7  Validation & save ──────────────────\n")

## Guarantee all 48 WC teams have a row (in case any were dropped)
team_features <- tibble::tibble(team = WC_TEAMS) |>
  dplyr::left_join(team_features, by = "team") |>
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)),
    team_strength = dplyr::if_else(team_strength == 0,
                                   0.5, team_strength)  # floor for any team still at 0
  )

## Check groups are fully covered
all_group_teams  <- unique(unlist(WC_GROUPS))
in_features      <- all_group_teams %in% team_features$team
if (!all(in_features)) {
  cat("  ⚠  Teams in groups but MISSING from features:\n   ",
      paste(all_group_teams[!in_features], collapse=", "), "\n")
} else {
  cat("  ✓  All", length(all_group_teams), "group teams present in features\n")
}

## Coverage counts
team_features <- team_features |>
  dplyr::mutate(
    data_source_flag = dplyr::case_when(
      n_matches >= 5 & has_player_data  ~ "full",
      n_matches >= 5 & !has_player_data ~ "team_only",
      n_matches <  5 & has_player_data  ~ "player_only",
      TRUE                              ~ "proxy_only"
    )
  )

coverage_summary <- team_features |>
  dplyr::count(data_source_flag, name = "n_teams")
cat("  Feature coverage:\n")
print(as.data.frame(coverage_summary), row.names = FALSE)

## Save
saveRDS(team_features,  file.path(OUT_DIR, "team_features.rds"))
readr::write_csv(team_features, file.path(OUT_DIR, "team_features.csv"))
cat("  Saved: team_features.rds + .csv\n")

## Proof-of-data diagnostic CSV
proof <- tibble::tibble(
  metric              = c(
    "timestamp",
    "results_csv_rows",
    "recent_matches_used",
    "goalscorer_records",
    "elo_teams_computed",
    "wc_teams_in_features",
    "teams_with_5plus_matches",
    "teams_with_player_data",
    "feature_source"
  ),
  value = c(
    as.character(Sys.time()),
    if (!is.null(raw_results))     as.character(nrow(raw_results))     else "0",
    if (nrow(recent) > 0)          as.character(nrow(recent))          else "0",
    if (nrow(team_goals) > 0)      as.character(sum(team_goals$total_goals_since_cutoff)) else "0",
    as.character(nrow(elo_ratings)),
    as.character(nrow(team_features)),
    as.character(sum(team_features$n_matches >= 5, na.rm=TRUE)),
    as.character(sum(team_features$has_player_data, na.rm=TRUE)),
    feature_source
  )
)
readr::write_csv(proof, file.path(OUT_DIR, "data_proof.csv"))
cat("  Saved: data_proof.csv\n")

## Text diagnostics
diag_lines <- c(
  paste0("Generated: ", Sys.time()),
  paste0("Feature source: ", feature_source),
  "",
  paste0("WC teams total: ", length(WC_TEAMS)),
  paste0("Results CSV rows: ",
         if (!is.null(raw_results)) nrow(raw_results) else "N/A"),
  paste0("Recent matches (>=", CUTOFF_YR, "): ",
         if (nrow(recent) > 0) nrow(recent) else 0),
  paste0("Teams with match data: ",
         sum(team_features$n_matches > 0, na.rm=TRUE)),
  paste0("Teams with player data: ",
         sum(team_features$has_player_data, na.rm=TRUE)),
  "",
  "── Team features top 10 by strength ──",
  capture.output(
    print(team_features |>
            dplyr::arrange(dplyr::desc(team_strength)) |>
            dplyr::select(team, team_strength, elo_current,
                          n_matches, has_player_data,
                          data_source_flag) |>
            head(10),
          n = 10)
  ),
  "",
  "── Teams missing match data ──",
  paste(team_features$team[team_features$n_matches < 3], collapse=", ")
)
writeLines(diag_lines, file.path(OUT_DIR, "data_diagnostics.txt"))
cat("  Saved: data_diagnostics.txt\n")

cat("\n══════════════════════════════════════════════════\n")
cat(" Data fetch complete.\n")
cat("  Outputs in:", OUT_DIR, "\n")
cat("  Next step: source('predict_wc2026.R')\n")
cat("══════════════════════════════════════════════════\n")