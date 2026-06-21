#!/usr/bin/env Rscript
################################################################################
##  simulate.R  –  WC 2026 Prediction  (Model + Simulation + Visualisation)
##
##  Run AFTER fetch_data.R. Reads these files from wc2026_output/:
##
##    all_results_parsed.rds   cols: date, home_team, away_team,
##                                   home_goals, away_goals,
##                                   competition, match_weight
##
##    team_features.rds        cols (all produced by fetch_data.R):
##                                   team, elo_current, elo_sc,
##                                   attack_str, defence_str, form_sc,
##                                   pr_sc, q_sc, team_strength,
##                                   n_matches, w_gf, w_ga, w_gd,
##                                   win_rate, form_last5, cs_rate,
##                                   q_pts_per, q_win_rate, pagerank,
##                                   has_player_data, data_source_flag,
##                                   goals_p90, xg_p90, assists_p90,
##                                   tackles_p90, interceptions_p90,
##                                   player_attack_sc, player_defence_sc
##
##    player_agg.rds           cols: team, goals_p90, xg_p90,
##                                   assists_p90, tackles_p90,
##                                   interceptions_p90, n_players,
##                                   total_min, avg_age
##
##  Writes to wc2026_output/:
##    final_predictions.csv, win_probabilities.rds/.png,
##    h2h_heatmap.png, team_strength_map.png, strength_radar.png,
##    player_stats_top10.png, data_quality_probabilities.png,
##    simulation_proof.csv
################################################################################

dir.create("wc2026_output", showWarnings = FALSE)

## ── PACKAGES ──────────────────────────────────────────────────────────────────
required_pkgs <- c(
  "dplyr","tidyr","purrr","stringr","readr","tibble","lubridate",
  "ggplot2","scales","glue","progressr",
  "BradleyTerry2","lme4",
  "rnaturalearth","sf",
  "countrycode",
  "rvest","xml2",
  "fmsb","RColorBrewer","viridis","janitor"
)
new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org", quiet = TRUE)
}
invisible(lapply(required_pkgs, library, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)

n_sim <- 10000L   # Monte Carlo runs (default)

# Read config.yaml if present to override defaults (mirrors fetch_data.R)
cfg <- list()
if (file.exists("config.yaml")) {
  if (!requireNamespace("yaml", quietly = TRUE))
    install.packages("yaml", repos = "https://cloud.r-project.org")
  cfg <- yaml::read_yaml("config.yaml")
}

if (!is.null(cfg$monte_carlo$random_seed)) set.seed(cfg$monte_carlo$random_seed)
if (!is.null(cfg$monte_carlo$n_simulations)) n_sim <- as.integer(cfg$monte_carlo$n_simulations)

# Ensemble and knockout tuning
bt_w <- if (!is.null(cfg$ensemble$bt_weight)) cfg$ensemble$bt_weight else 0.5
poisson_w <- if (!is.null(cfg$ensemble$poisson_weight)) cfg$ensemble$poisson_weight else 0.5
ko_compression <- if (!is.null(cfg$ensemble$knockout_extra_time_compression)) cfg$ensemble$knockout_extra_time_compression else 0.6

## ── CANONICAL MAP  (identical to fetch_data.R) ────────────────────────────────
CANONICAL_MAP <- c(
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
  "Congo"                        = "Congo DR",
  "DRC"                          = "Congo DR",
  "Bosnia-Herzegovina"           = "Bosnia and Herzegovina",
  "Bosnia & Herzegovina"         = "Bosnia and Herzegovina",
  "Bosnia-Herzegowina"           = "Bosnia and Herzegovina",
  "Cape Verde"                   = "Cabo Verde",
  "Czech Republic"               = "Czechia",
  "IR Iran"                      = "Iran"
)
canon <- function(x) dplyr::recode(as.character(x), !!!CANONICAL_MAP, .default = as.character(x))

## Official 48 WC 2026 teams
WC_TEAMS <- unique(canon(c(
  "France","Spain","England","Portugal","Germany","Netherlands","Belgium",
  "Switzerland","Croatia","Austria","Serbia","Denmark","Ukraine","Türkiye",
  "Sweden","Norway","Bosnia and Herzegovina","Czechia",
  "Brazil","Argentina","Colombia","Uruguay","Ecuador","Paraguay",
  "Morocco","Senegal","Egypt","South Africa","Algeria",
  "Ghana","Cabo Verde","Tunisia","Côte d'Ivoire","Congo DR",
  "Japan","South Korea","Iran","Saudi Arabia","Australia",
  "Jordan","Uzbekistan","Iraq","Qatar",
  "United States","Mexico","Canada","Panama","Scotland","Haiti",
  "Curaçao"
)))

## Official groups (confirmed post March 31 2026)
WC_GROUPS <- list(
  A = canon(c("Mexico",        "South Korea",   "South Africa",          "Czechia")),
  B = canon(c("Canada",        "Switzerland",   "Qatar",                 "Bosnia and Herzegovina")),
  C = canon(c("Brazil",        "Morocco",       "Scotland",              "Haiti")),
  D = canon(c("United States", "Australia",     "Paraguay",              "Türkiye")),
  E = canon(c("Germany",       "Ecuador",       "Côte d'Ivoire",         "Curaçao")),
  F = canon(c("Netherlands",   "Japan",         "Tunisia",               "Sweden")),
  G = canon(c("Belgium",       "Iran",          "Egypt",                 "New Zealand")),
  H = canon(c("Spain",         "Uruguay",       "Saudi Arabia",          "Cabo Verde")),
  I = canon(c("France",        "Senegal",       "Norway",                "Iraq")),
  J = canon(c("Argentina",     "Austria",       "Algeria",               "Jordan")),
  K = canon(c("Portugal",      "Colombia",      "Uzbekistan",            "Congo DR")),
  L = canon(c("England",       "Croatia",       "Panama",                "Ghana"))
)
group_teams_all <- unique(unlist(WC_GROUPS))
extra_teams     <- setdiff(group_teams_all, WC_TEAMS)
if (length(extra_teams) > 0) {
  message("Adding group teams missing from WC_TEAMS: ", paste(extra_teams, collapse = ", "))
  WC_TEAMS <- unique(c(WC_TEAMS, extra_teams))
}


################################################################################
##  BLOCK 0 – Load & validate caches from fetch_data.R
################################################################################

cat("══════════════════════════════════════════════════\n")
cat(" WC 2026 – Simulation & Visualisation\n")
cat("══════════════════════════════════════════════════\n\n")

## Show data provenance written by fetch_data.R
proof_path <- "wc2026_output/data_proof.csv"
if (file.exists(proof_path)) {
  cat("── Data provenance (fetch_data.R) ────────────────\n")
  print(as.data.frame(readr::read_csv(proof_path, show_col_types = FALSE)),
        row.names = FALSE)
  cat("\n")
}

load_cache <- function(path, label, required = TRUE) {
  if (!file.exists(path)) {
    if (required) stop(label, " not found: ", path, "\n  → Run fetch_data.R first.")
    message("  ⚠  ", label, " not found – continuing without it.")
    return(NULL)
  }
  obj <- readRDS(path)
  cat("  ✓ ", label, "–",
      if (is.data.frame(obj)) paste(nrow(obj), "rows ×", ncol(obj), "cols")
      else class(obj), "\n")
  obj
}

cat("── Loading caches ────────────────────────────────\n")
all_results   <- load_cache("wc2026_output/all_results_parsed.rds",  "all_results_parsed",  required = TRUE)
team_features <- load_cache("wc2026_output/team_features.rds",        "team_features",       required = TRUE)
player_agg    <- load_cache("wc2026_output/player_agg.rds",           "player_agg",          required = FALSE)
if (is.null(player_agg)) player_agg <- tibble::tibble()
cat("\n")

## ── Ensure every group team has a row in team_features ───────────────────────
missing_tf <- setdiff(group_teams_all, team_features$team)
if (length(missing_tf) > 0) {
  cat("⚠  Adding proxy rows for teams missing from team_features:\n   ",
      paste(missing_tf, collapse = ", "), "\n\n")
  proxy <- tibble::tibble(
    team              = missing_tf,
    elo_current       = 1400, elo_sc       = 0.35,
    attack_str        = 0.35, defence_str  = 0.35,
    form_sc           = 0.35, pr_sc        = 0.35, q_sc = 0.35,
    team_strength     = 0.35,
    n_matches         = 0L,
    w_gf = 1.0, w_ga = 1.3, w_gd = -0.3,
    win_rate = 0.30, form_last5 = 0.9, cs_rate = 0.18,
    q_pts_per = 0.9, q_win_rate = 0.28, pagerank = 0.02,
    has_player_data   = FALSE,
    player_attack_sc  = 0.35, player_defence_sc = 0.35,
    data_source_flag  = "proxy_only"
  )
  team_features <- dplyr::bind_rows(team_features, proxy)
}

## ── Ensure columns from fetch_data.R actually exist; create safe fallbacks ───
ensure_col <- function(df, col, default = 0.5) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}
team_features <- team_features |>
  ensure_col("elo_sc",            0.5) |>
  ensure_col("attack_str",        0.5) |>
  ensure_col("defence_str",       0.5) |>
  ensure_col("form_sc",           0.5) |>
  ensure_col("pr_sc",             0.5) |>
  ensure_col("q_sc",              0.5) |>
  ensure_col("form_last5",        1.5) |>
  ensure_col("q_pts_per",         1.5) |>
  ensure_col("team_strength",     0.5) |>
  ensure_col("has_player_data",   FALSE) |>
  ensure_col("player_attack_sc",  0.5) |>
  ensure_col("player_defence_sc", 0.5) |>
  ensure_col("data_source_flag",  "proxy_only")

## Report feature inventory
cat("── team_features column inventory ───────────────\n")
cat("  Rows:", nrow(team_features), " | Cols:", ncol(team_features), "\n")
if ("data_source_flag" %in% names(team_features)) {
  print(as.data.frame(dplyr::count(team_features, data_source_flag)), row.names = FALSE)
}
cat("\n")

has_results <- nrow(all_results) >= 10


################################################################################
##  BLOCK 3A – Bradley-Terry paired-comparison model
##
##  Input columns from all_results_parsed.rds:
##    home_team, away_team, home_goals, away_goals, match_weight
##
##  Output: bt_df_full (team, bt_ability, bt_se) merged into team_features
################################################################################

cat("── Block 3A  Bradley-Terry ───────────────────────\n")

bt_df <- NULL   # initialise so later code can safely check is.null(bt_df)

if (has_results) {
  bt_matches <- all_results |>
    dplyr::filter(
      home_team %in% WC_TEAMS,
      away_team %in% WC_TEAMS,
      !is.na(home_goals),
      !is.na(away_goals)
    )
  cat("  BT input matches (WC vs WC only):", nrow(bt_matches), "\n")

  if (nrow(bt_matches) >= 10) {
    teams_bt <- unique(c(bt_matches$home_team, bt_matches$away_team))
    n_bt     <- length(teams_bt)
    W        <- matrix(0, n_bt, n_bt, dimnames = list(teams_bt, teams_bt))

    for (i in seq_len(nrow(bt_matches))) {
      r  <- bt_matches[i, ]
      wt <- r$match_weight
      hm <- r$home_team
      aw <- r$away_team
      if (r$home_goals > r$away_goals) {
        W[hm, aw] <- W[hm, aw] + wt
      } else if (r$away_goals > r$home_goals) {
        W[aw, hm] <- W[aw, hm] + wt
      } else {
        W[hm, aw] <- W[hm, aw] + wt * 0.5
        W[aw, hm] <- W[aw, hm] + wt * 0.5
      }
    }

    bt_fit <- tryCatch(
      BradleyTerry2::BTm(
        outcome = W,
        player1 = BradleyTerry2::row.player(W),
        player2 = BradleyTerry2::col.player(W)
      ),
      error = function(e) { warning("BT failed: ", e$message); NULL }
    )

    if (!is.null(bt_fit)) {
      bt_ab <- BradleyTerry2::BTabilities(bt_fit)
      bt_df <- tibble::tibble(
        team       = rownames(bt_ab),
        bt_ability = bt_ab[, "ability"],
        bt_se      = bt_ab[, "s.e."]
      )
      cat("  ✓ BT converged on", nrow(bt_df), "teams\n")
    } else {
      cat("  ⚠  BT failed – using team_strength proxy\n")
    }
  } else {
    cat("  ⚠  Too few WC-vs-WC matches for BT – using proxy\n")
  }
} else {
  cat("  ⚠  No match data – using team_strength proxy\n")
}

## Build complete bt_df_full covering all 48 WC teams.
## Teams in the BT model use their fitted ability;
## others get a proxy derived from team_strength.
bt_df_full <- team_features |>
  dplyr::select(team, team_strength) |>
  dplyr::mutate(
    bt_ability_proxy = team_strength * 3 - 1.5,
    bt_se_proxy      = 0.35
  )

if (!is.null(bt_df)) {
  bt_df_full <- bt_df_full |>
    dplyr::left_join(bt_df, by = "team") |>
    dplyr::mutate(
      bt_ability = dplyr::coalesce(bt_ability, bt_ability_proxy),
      bt_se      = dplyr::coalesce(bt_se,      bt_se_proxy)
    ) |>
    dplyr::select(team, bt_ability, bt_se)
} else {
  bt_df_full <- bt_df_full |>
    dplyr::rename(bt_ability = bt_ability_proxy, bt_se = bt_se_proxy) |>
    dplyr::select(team, bt_ability, bt_se)
}

## Merge into team_features (drop any pre-existing copies first)
team_features <- team_features |>
  dplyr::select(-dplyr::any_of(c("bt_ability", "bt_se"))) |>
  dplyr::left_join(bt_df_full, by = "team")

## Safe BT win-probability lookup
bt_win_prob <- function(ta, tb) {
  la <- bt_df_full$bt_ability[bt_df_full$team == ta]
  lb <- bt_df_full$bt_ability[bt_df_full$team == tb]
  if (!length(la) || !length(lb) || is.na(la) || is.na(lb)) return(0.5)
  plogis(la - lb)
}

cat("  BT range: [", round(min(bt_df_full$bt_ability), 2),
    ",", round(max(bt_df_full$bt_ability), 2), "]\n\n")


################################################################################
##  BLOCK 3B – Poisson / Dixon-Coles model
##
##  Input columns from all_results_parsed.rds:
##    home_team, away_team, home_goals, away_goals, match_weight
##
##  team_features columns used for player modifier in predict_goals():
##    player_attack_sc, player_defence_sc   (produced by fetch_data.R Sec 6)
##
##  Fallback predict_goals() uses team_strength + player scalars when
##  the glmer fails or there are too few rows.
################################################################################

cat("── Block 3B  Poisson model ───────────────────────\n")

dc_fit <- NULL   # initialise

if (has_results && nrow(all_results) >= 30) {

  goals_df <- dplyr::bind_rows(
    all_results |> dplyr::transmute(
      scored   = home_goals, attack  = home_team,
      defence  = away_team,  home_adv = 1L, weight = match_weight),
    all_results |> dplyr::transmute(
      scored   = away_goals, attack  = away_team,
      defence  = home_team,  home_adv = 0L, weight = match_weight)
  ) |>
    dplyr::filter(!is.na(scored), attack %in% WC_TEAMS, defence %in% WC_TEAMS)

  cat("  Poisson input rows:", nrow(goals_df), "\n")

  dc_fit <- tryCatch(
    lme4::glmer(
      scored ~ home_adv + (1 | attack) + (1 | defence),
      data    = goals_df, family = poisson(), weights = goals_df$weight
    ),
    error = function(e) { warning("glmer failed: ", e$message); NULL }
  )

  if (!is.null(dc_fit)) {
    re <- lme4::ranef(dc_fit)
    attack_re  <- re$attack  |>
      tibble::rownames_to_column("team") |>
      dplyr::rename(attack_re  = `(Intercept)`)
    defence_re <- re$defence |>
      tibble::rownames_to_column("team") |>
      dplyr::rename(defence_re = `(Intercept)`)

    ## Merge RE back (drop old copies to prevent duplicate columns)
    team_features <- team_features |>
      dplyr::select(-dplyr::any_of(c("attack_re", "defence_re"))) |>
      dplyr::left_join(attack_re,  by = "team") |>
      dplyr::left_join(defence_re, by = "team") |>
      dplyr::mutate(
        attack_re  = tidyr::replace_na(attack_re,  0),
        defence_re = tidyr::replace_na(defence_re, 0)
      )

    mu0     <- lme4::fixef(dc_fit)["(Intercept)"]
    ha_coef <- lme4::fixef(dc_fit)["home_adv"]

    ## Safe RE accessor
    get_re <- function(t, col) {
      v <- team_features[[col]][team_features$team == t]
      if (!length(v) || is.na(v)) 0 else v
    }

    ## Player modifier on log-scale (small effect, ±0.18 max)
    player_log_adj <- function(ta, tb) {
      pa   <- team_features$player_attack_sc[team_features$team == ta]
      pd_b <- team_features$player_defence_sc[team_features$team == tb]
      pa   <- if (!length(pa)   || is.na(pa))   0.5 else pa
      pd_b <- if (!length(pd_b) || is.na(pd_b)) 0.5 else pd_b
      0.35 * (pa - 0.5) - 0.35 * (pd_b - 0.5)
    }

    predict_goals <- function(ta, tb, neutral = TRUE) {
      a_off <- get_re(ta, "attack_re")
      b_def <- get_re(tb, "defence_re")
      b_off <- get_re(tb, "attack_re")
      a_def <- get_re(ta, "defence_re")
      ha    <- if (neutral) 0 else ha_coef
      list(
        lambda_a = exp(mu0 + a_off - b_def + ha    + player_log_adj(ta, tb)),
        lambda_b = exp(mu0 + b_off - a_def          + player_log_adj(tb, ta))
      )
    }

    cat("  ✓ Poisson fitted. intercept =", round(mu0, 3),
        "| home_adv =", round(ha_coef, 3), "\n\n")
  }
}

## Fallback predict_goals: team_strength + player scalars → Poisson lambdas
if (is.null(dc_fit)) {
  cat(if (has_results && nrow(all_results) >= 30)
        "  ⚠  glmer failed – using team_strength fallback\n\n"
      else
        "  ⚠  <30 matches – using team_strength fallback\n\n")

  predict_goals <- function(ta, tb, neutral = TRUE) {
    sa <- team_features$team_strength[team_features$team == ta]
    sb <- team_features$team_strength[team_features$team == tb]
    pa   <- team_features$player_attack_sc[team_features$team == ta]
    pd_b <- team_features$player_defence_sc[team_features$team == tb]
    sa   <- if (!length(sa)   || is.na(sa))   0.5 else sa
    sb   <- if (!length(sb)   || is.na(sb))   0.5 else sb
    pa   <- if (!length(pa)   || is.na(pa))   0.5 else pa
    pd_b <- if (!length(pd_b) || is.na(pd_b)) 0.5 else pd_b
    ## Player adjustments for both directions (ta vs tb and tb vs ta)
    pa_b <- team_features$player_attack_sc[team_features$team == tb]
    pd_a <- team_features$player_defence_sc[team_features$team == ta]
    pa_b <- if (!length(pa_b) || is.na(pa_b)) 0.5 else pa_b
    pd_a <- if (!length(pd_a) || is.na(pd_a)) 0.5 else pd_a
    adj_a <- 0.35 * (pa - 0.5) - 0.35 * (pd_b - 0.5)
    adj_b <- 0.35 * (pa_b - 0.5) - 0.35 * (pd_a - 0.5)
    list(
      lambda_a = max(0.3, (1.0 + (sa - 0.5) * 1.6) * exp(adj_a)),
      lambda_b = max(0.3, (1.0 + (sb - 0.5) * 1.6) * exp(adj_b))
    )
  }
}

## score_win_prob: single function that always works (uses predict_goals above)
score_win_prob <- function(ta, tb, max_goals = 10L) {
  L  <- tryCatch(predict_goals(ta, tb),
                 error = function(e) list(lambda_a = 1.2, lambda_b = 1.2))
  pm <- outer(dpois(0:max_goals, max(0.01, L$lambda_a)),
              dpois(0:max_goals, max(0.01, L$lambda_b)))
  c(
    win_a = sum(pm[upper.tri(pm, diag = FALSE)]),
    draw  = sum(diag(pm)),
    win_b = sum(pm[lower.tri(pm, diag = FALSE)])
  )
}

## Ensemble: 50 % BT + 50 % Poisson
ensemble_win_prob <- function(ta, tb, extra_time = FALSE) {
  bt_p  <- bt_win_prob(ta, tb)
  dc    <- score_win_prob(ta, tb)
  denom <- unname(dc["win_a"]) + unname(dc["win_b"])
  dc_p  <- if (denom > 0) unname(dc["win_a"]) / denom else bt_p
  weight_sum <- bt_w + poisson_w
  pa    <- (bt_w * bt_p + poisson_w * dc_p) / max(1e-12, weight_sum)
  if (extra_time) pa <- 0.5 + (pa - 0.5) * ko_compression
  c(p_a = as.numeric(pa), p_b = as.numeric(1 - pa))
}


################################################################################
##  BLOCK 4 – Monte Carlo tournament simulation
##  Format: 12 groups of 4 → top 2 (24) + 8 best 3rd-place = 32 teams → KO
################################################################################

cat("── Block 4  Monte Carlo (", scales::comma(n_sim), "runs ) ────────\n")

simulate_group <- function(group_teams) {
  matchups  <- combn(group_teams, 2, simplify = FALSE)
  standings <- tibble::tibble(team = group_teams,
                               pts = 0L, gd = 0L, gf = 0L)
  for (mu in matchups) {
    ta <- mu[[1]]; tb <- mu[[2]]
    dc <- score_win_prob(ta, tb)
    r  <- sample(
      c("a_win","draw","b_win"), 1,
      prob = c(unname(dc["win_a"]), unname(dc["draw"]), unname(dc["win_b"]))
    )
    L  <- tryCatch(predict_goals(ta, tb),
                   error = function(e) list(lambda_a = 1.2, lambda_b = 1.2))
    ga <- rpois(1, max(0.01, L$lambda_a))
    gb <- rpois(1, max(0.01, L$lambda_b))
    if (r == "a_win" && ga <= gb) ga <- as.integer(gb + 1L)
    if (r == "b_win" && gb <= ga) gb <- as.integer(ga + 1L)
    if (r == "draw"  && ga != gb) gb <- ga

    standings$gf[standings$team == ta] <- standings$gf[standings$team == ta] + ga
    standings$gf[standings$team == tb] <- standings$gf[standings$team == tb] + gb
    standings$gd[standings$team == ta] <- standings$gd[standings$team == ta] + (ga - gb)
    standings$gd[standings$team == tb] <- standings$gd[standings$team == tb] + (gb - ga)

    pts_a <- dplyr::case_when(r == "a_win" ~ 3L, r == "draw" ~ 1L, TRUE ~ 0L)
    pts_b <- dplyr::case_when(r == "b_win" ~ 3L, r == "draw" ~ 1L, TRUE ~ 0L)
    standings$pts[standings$team == ta] <- standings$pts[standings$team == ta] + pts_a
    standings$pts[standings$team == tb] <- standings$pts[standings$team == tb] + pts_b
  }
  standings |>
    dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd), dplyr::desc(gf)) |>
    dplyr::mutate(rank = dplyr::row_number())
}

simulate_knockout <- function(ta, tb) {
  p <- ensemble_win_prob(ta, tb, extra_time = TRUE)
  sample(c(ta, tb), 1L, prob = c(p["p_a"], p["p_b"]))
}

simulate_tournament <- function() {
  grp  <- lapply(WC_GROUPS, simulate_group)
  top2 <- unlist(lapply(grp, function(g) g$team[g$rank <= 2]))         # 24
  third <- lapply(grp, function(g) g[g$rank == 3, ]) |>
    dplyr::bind_rows() |>
    dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd), dplyr::desc(gf)) |>
    dplyr::slice_head(n = 8) |>
    dplyr::pull(team)
  bracket <- c(top2, third)   # 32 teams
  while (length(bracket) > 1) {
    bracket <- vapply(
      seq(1, length(bracket), by = 2),
      function(i) simulate_knockout(bracket[i], bracket[i + 1]),
      FUN.VALUE = character(1)
    )
  }
  bracket
}

w_seed <- if (!is.null(cfg$monte_carlo$random_seed)) as.integer(cfg$monte_carlo$random_seed) else NULL
if (!is.null(w_seed)) set.seed(w_seed)
winners <- character(n_sim)
progressr::with_progress({
  pg <- progressr::progressor(n_sim)
  for (i in seq_len(n_sim)) {
    winners[i] <- tryCatch(simulate_tournament(), error = function(e) NA_character_)
    if (i %% 500 == 0) pg(amount = 500)
  }
})

n_valid <- sum(!is.na(winners))
cat("\n  Valid simulations:", scales::comma(n_valid), "/", scales::comma(n_sim), "\n")

## Build win_probs with ALL 48 WC teams (zero-win teams still appear)
win_probs_raw <- table(winners[!is.na(winners)]) |>
  sort(decreasing = TRUE) |>
  as.data.frame() |>
  dplyr::rename(team = Var1, n_wins = Freq) |>
  dplyr::mutate(
    win_pct   = n_wins / n_valid,
    win_pct_l = qbeta(0.025, n_wins + 1, n_valid - n_wins + 1),
    win_pct_u = qbeta(0.975, n_wins + 1, n_valid - n_wins + 1)
  )

win_probs <- tibble::tibble(team = WC_TEAMS) |>
  dplyr::left_join(win_probs_raw, by = "team") |>
  dplyr::mutate(
    n_wins    = tidyr::replace_na(n_wins, 0L),
    win_pct   = tidyr::replace_na(win_pct, 0),
    win_pct_l = tidyr::replace_na(win_pct_l, 0),
    win_pct_u = tidyr::replace_na(win_pct_u, qbeta(0.975, 1, n_valid + 1))
  ) |>
  dplyr::arrange(dplyr::desc(win_pct))

saveRDS(win_probs, "wc2026_output/win_probabilities.rds")
cat("\n══ Top 10 predicted winners ══\n")
print(head(win_probs[, c("team","win_pct","n_wins")], 10))
cat("\n")


################################################################################
##  BLOCK 5 – Final predictions table
##  Only references columns that fetch_data.R actually produces.
################################################################################

cat("── Block 5  Final predictions table ─────────────\n")

output_cols <- intersect(
  c("elo_current","team_strength","bt_ability","n_matches",
    "data_source_flag","has_player_data",
    "attack_str","defence_str","form_last5","q_pts_per",
    "w_gf","w_ga","goals_p90","xg_p90","player_attack_sc"),
  names(team_features)
)

final_table <- win_probs |>
  dplyr::left_join(
    team_features |> dplyr::select(team, dplyr::all_of(output_cols)),
    by = "team"
  ) |>
  dplyr::arrange(dplyr::desc(win_pct)) |>
  dplyr::mutate(
    pred_rank   = dplyr::row_number(),
    win_pct_fmt = scales::percent(win_pct, accuracy = 0.1),
    ci_95       = glue::glue(
      "[{scales::percent(win_pct_l, accuracy=0.1)}, ",
      "{scales::percent(win_pct_u, accuracy=0.1)}]"
    )
  )

readr::write_csv(final_table, "wc2026_output/final_predictions.csv")
cat("  Saved: final_predictions.csv (", nrow(final_table), "rows ×",
    ncol(final_table), "cols)\n\n")


################################################################################
##  BLOCK 6 – Simulation proof  (audit what data actually drove the result)
################################################################################

cat("── Block 6  Simulation proof ─────────────────────\n")

sim_proof <- tibble::tibble(
  metric = c(
    "timestamp",
    "n_simulations_requested",
    "n_simulations_valid",
    "wc_teams_in_bracket",
    "all_results_rows",
    "bt_fitted_teams",
    "bt_ability_spread",
    "poisson_model_fitted",
    "poisson_attack_re_teams",
    "teams_with_player_data",
    "data_source_breakdown",
    "top_team",
    "top_team_win_pct",
    "model_ensemble"
  ),
  value = c(
    as.character(Sys.time()),
    as.character(n_sim),
    as.character(n_valid),
    as.character(length(group_teams_all)),
    as.character(nrow(all_results)),
    as.character(if (!is.null(bt_df)) nrow(bt_df) else 0),
    as.character(round(diff(range(bt_df_full$bt_ability, na.rm = TRUE)), 3)),
    as.character(!is.null(dc_fit)),
    as.character(if ("attack_re" %in% names(team_features))
                   sum(!is.na(team_features$attack_re)) else 0),
    as.character(sum(team_features$has_player_data, na.rm = TRUE)),
    if ("data_source_flag" %in% names(team_features))
      paste(sort(unique(team_features$data_source_flag)), collapse = " | ")
    else "unknown",
    as.character(final_table$team[1]),
    scales::percent(final_table$win_pct[1], accuracy = 0.1),
    "50% Bradley-Terry + 50% Poisson (player modifier on lambda)"
  )
)

readr::write_csv(sim_proof, "wc2026_output/simulation_proof.csv")
cat("  Saved: simulation_proof.csv\n")
print(as.data.frame(sim_proof), row.names = FALSE)
cat("\n")


################################################################################
##  BLOCK 7 – Visualisations
################################################################################

cat("── Block 7  Visualisations ───────────────────────\n")

## Helper: viridis-compatible fill scale (avoids viridis package dependency)
plasma_fill <- ggplot2::scale_fill_viridis_c(option = "plasma", guide = "none")


## 7A  Win-probability bar chart (all teams with > 0 wins) ──────────────────
bar_data <- win_probs |> dplyr::filter(win_pct > 0)

p_probs <- ggplot2::ggplot(
  bar_data,
  ggplot2::aes(x = reorder(team, win_pct), y = win_pct, fill = win_pct)
) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = win_pct_l, ymax = win_pct_u),
    width = 0.25, colour = "grey30", linewidth = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = scales::percent(win_pct, accuracy = 0.1)),
    hjust = -0.12, size = 2.8
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, max(bar_data$win_pct_u, na.rm = TRUE) * 1.25),
    expand = c(0, 0)
  ) +
  plasma_fill +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = "2026 FIFA World Cup – Predicted Win Probabilities",
    subtitle = glue::glue(
      "Monte Carlo: {scales::comma(n_valid)} valid simulations | 95% CI | ",
      "feature source: {sim_proof$value[sim_proof$metric == 'data_source_breakdown']}"
    ),
    x = NULL, y = "Tournament Win Probability",
    caption  = "Models: Bradley-Terry + Dixon-Coles Poisson (50/50 ensemble) | player modifier on lambda"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title         = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle      = ggplot2::element_text(colour = "grey40", size = 8),
    panel.grid.major.y = ggplot2::element_blank()
  )

ggplot2::ggsave("wc2026_output/win_probabilities.png", p_probs,
                width = 12, height = max(8, nrow(bar_data) * 0.28), dpi = 150)
cat("  Saved: win_probabilities.png\n")


## 7B  World-map choropleth ─────────────────────────────────────────────────
world_sf <- tryCatch(
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
  error = function(e) NULL
)

if (!is.null(world_sf)) {
  ## ISO3 mapping: derive iso3 codes from team names using `countrycode`
  teams_for_map <- sort(unique(team_features$team))
  iso3 <- countrycode::countrycode(teams_for_map, origin = 'country.name', destination = 'iso3c', warn = FALSE)
  ## Manual fixes for names countrycode may not map cleanly
  manual_iso <- c(
    "Côte d'Ivoire" = "CIV",
    "Curaçao" = "CUW",
    "Cabo Verde" = "CPV",
    "South Korea" = "KOR",
    "United States" = "USA",
    "Türkiye" = "TUR",
    "Congo DR" = "COD",
    "Scotland" = "GBR",
    "New Zealand" = "NZL",
    "Saudi Arabia" = "SAU"
  )
  for (n in names(manual_iso)) {
    iso3[teams_for_map == n] <- manual_iso[[n]]
  }
  country_iso <- tibble::tibble(team = teams_for_map, iso_a3 = iso3)

  map_sf <- world_sf |>
    dplyr::left_join(
      country_iso |>
        dplyr::left_join(
          team_features |> dplyr::select(team, team_strength),
          by = "team"
        ),
      by = "iso_a3"
    )

  p_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_sf, fill = "grey92",
                     colour = "white", linewidth = 0.15) +
    ggplot2::geom_sf(
      data = dplyr::filter(map_sf, !is.na(team_strength)),
      ggplot2::aes(fill = team_strength),
      colour = "white", linewidth = 0.2
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma", na.value = "grey92",
      name = "Team\nStrength", limits = c(0, 1)
    ) +
    ggplot2::coord_sf(crs = sf::st_crs("ESRI:54030")) +
    ggplot2::labs(
      title    = "2026 FIFA World Cup – Team Strength by Country",
      subtitle = glue::glue(
        "Elo + form + qualifier stats + player quality | ",
        "full data: {sum(team_features$data_source_flag == 'full', na.rm=TRUE)} teams"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave("wc2026_output/team_strength_map.png", p_map,
                  width = 18, height = 10, dpi = 150)
  cat("  Saved: team_strength_map.png\n")
} else {
  cat("  ⚠  rnaturalearth unavailable – world map skipped\n")
}


## 7C  Radar chart (top 8 teams by win probability) ─────────────────────────
## Uses columns that fetch_data.R ACTUALLY produces:
##   elo_sc, attack_str, defence_str, form_sc, pr_sc
## (NOT form_last5_sc / fifa_strength / q_pts_sc which don't exist)

radar_col_candidates <- c(
  "Elo"       = "elo_sc",
  "Attack"    = "attack_str",
  "Defence"   = "defence_str",
  "Form"      = "form_sc",
  "PageRank"  = "pr_sc",
  "Qual"      = "q_sc",
  "Plyr Att"  = "player_attack_sc",
  "Plyr Def"  = "player_defence_sc"
)

## Keep at most 6 dims that actually exist in team_features
radar_cols_use  <- radar_col_candidates[unname(radar_col_candidates) %in% names(team_features)]
radar_cols_use  <- head(radar_cols_use, 6)   # fmsb works best with ≤6 dims
radar_labels    <- names(radar_cols_use)
radar_col_names <- unname(radar_cols_use)

radar_teams <- intersect(head(win_probs$team[win_probs$win_pct > 0], 8),
                          team_features$team)

if (length(radar_teams) >= 2 && length(radar_col_names) >= 3) {
  rf <- team_features |>
    dplyr::filter(team %in% radar_teams) |>
    dplyr::select(team, dplyr::all_of(radar_col_names)) |>
    dplyr::mutate(dplyr::across(-team, function(x) {
      rng <- range(x, na.rm = TRUE)
      if (rng[2] == rng[1]) return(rep(0.5, length(x)))
      scales::rescale(x, to = c(0, 1))
    }))

  mat_data <- as.data.frame(rf[, -1])
  rownames(mat_data) <- rf$team
  names(mat_data)    <- radar_labels          # use human-readable labels

  max_row  <- setNames(rep(1, ncol(mat_data)), radar_labels)
  min_row  <- setNames(rep(0, ncol(mat_data)), radar_labels)
  radar_df <- rbind(max_row, min_row, mat_data)

  png("wc2026_output/strength_radar.png", width = 1600, height = 1000, res = 130)
  n_r <- min(2L, ceiling(length(radar_teams) / 4))
  n_c <- ceiling(length(radar_teams) / n_r)
  par(mfrow = c(n_r, n_c), mar = c(0.5, 0.5, 2.5, 0.5))
  pal <- RColorBrewer::brewer.pal(max(3L, length(radar_teams)), "Set1")

  for (i in seq_len(length(radar_teams))) {
    fmsb::radarchart(
      radar_df[c(1, 2, i + 2), ],
      axistype    = 1, seg = 4,
      pcol        = pal[i],
      pfcol       = grDevices::adjustcolor(pal[i], 0.25),
      plwd        = 2,
      cglcol      = "grey70", cglty = 1,
      axislabcol  = "grey50",
      caxislabels = seq(0, 1, 0.25),
      vlabels     = radar_labels,
      vlcex       = 0.82,
      title       = rownames(mat_data)[i]
    )
  }
  dev.off()
  cat("  Saved: strength_radar.png  dims: [",
      paste(radar_labels, collapse = ", "), "]\n")
} else {
  cat("  ⚠  Radar skipped: teams =", length(radar_teams),
      "| dims =", length(radar_col_names), "\n")
}


## 7D  H2H win-probability heatmap (all teams with ≥1 win) ──────────────────
heat_teams <- win_probs$team[win_probs$win_pct > 0]
n_heat     <- length(heat_teams)

heat_mat <- expand.grid(team_a = heat_teams, team_b = heat_teams,
                         stringsAsFactors = FALSE) |>
  dplyr::mutate(
    p_a = mapply(function(a, b) {
      if (a == b) NA_real_ else ensemble_win_prob(a, b)["p_a"]
    }, team_a, team_b)
  )

p_heat <- ggplot2::ggplot(
  heat_mat,
  ggplot2::aes(x = team_b, y = team_a, fill = p_a)
) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(label = ifelse(is.na(p_a), "",
                                scales::percent(p_a, accuracy = 1))),
    size = 2.0, colour = "white", fontface = "bold"
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#1B2A4A", mid = "grey92", high = "#E63946",
    midpoint = 0.5, na.value = "white",
    name = "Win Prob\n(row team)"
  ) +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::labs(
    title    = "Head-to-Head Win Probability Matrix – WC 2026",
    subtitle = "Row team's probability of beating column team (ensemble model)",
    x = "Opponent", y = "Team"
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 0, size = 6.5),
    axis.text.y     = ggplot2::element_text(size = 6.5),
    panel.grid      = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", size = 11)
  )

dim_heat <- max(14, ceiling(n_heat * 0.42))
ggplot2::ggsave("wc2026_output/h2h_heatmap.png", p_heat,
                width = dim_heat, height = dim_heat, dpi = 150)
cat("  Saved: h2h_heatmap.png (", n_heat, "×", n_heat, ")\n")


## 7E  Player stats per squad (top 10 teams) ────────────────────────────────
## player_agg from fetch_data.R has cols: team, goals_p90, xg_p90,
## assists_p90, tackles_p90, interceptions_p90
##
## We also check for the older "national_team" / "squad_*" naming
## in case a user has a cache from an earlier version.

if (nrow(player_agg) > 0) {

  ## Normalise team column name
  if ("national_team" %in% names(player_agg) && !"team" %in% names(player_agg))
    player_agg <- dplyr::rename(player_agg, team = national_team)

  ## Normalise metric column names (old squad_*_p90 → new *_p90)
  col_renames <- c(
    squad_goals_p90    = "goals_p90",
    squad_xg_p90       = "xg_p90",
    squad_assists_p90  = "assists_p90",
    squad_tackles_p90  = "tackles_p90"
  )
  for (old in names(col_renames)) {
    new <- col_renames[[old]]
    if (old %in% names(player_agg) && !new %in% names(player_agg))
      player_agg <- dplyr::rename(player_agg, !!new := !!old)
  }

  ## Available player metrics to plot
  plot_cols <- intersect(
    c("goals_p90","xg_p90","assists_p90","tackles_p90","interceptions_p90"),
    names(player_agg)
  )

  if (length(plot_cols) >= 1 && "team" %in% names(player_agg)) {
    top10 <- head(win_probs$team[win_probs$win_pct > 0], 10)

    pa_long <- player_agg |>
      dplyr::filter(team %in% top10) |>
      dplyr::select(team, dplyr::all_of(plot_cols)) |>
      tidyr::pivot_longer(-team, names_to = "metric", values_to = "value") |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(
        metric = stringr::str_replace_all(metric, "_", " ") |>
                   stringr::str_to_title()
      )

    if (nrow(pa_long) > 0) {
      teams_with_data <- length(unique(pa_long$team))
      p_pl <- ggplot2::ggplot(
        pa_long,
        ggplot2::aes(x = reorder(team, value), y = value, fill = team)
      ) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::facet_wrap(~metric, scales = "free_x") +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_viridis_d(guide = "none") +
        ggplot2::labs(
          title    = "Player Stats – Top 10 Predicted WC 2026 Teams",
          subtitle = glue::glue(
            "Squad per-90 aggregates | ",
            "{teams_with_data}/10 teams have player data | ",
            "Source: hubertsidorowicz 2024-25"
          ),
          x = NULL, y = "Per 90 minutes",
          caption  = paste("Metrics:", paste(plot_cols, collapse = ", "))
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(face = "bold"),
          plot.subtitle = ggplot2::element_text(colour = "grey40", size = 9)
        )

      ggplot2::ggsave("wc2026_output/player_stats_top10.png", p_pl,
                      width = 14, height = 7, dpi = 160)
      cat("  Saved: player_stats_top10.png (", teams_with_data,
          "teams, cols:", paste(plot_cols, collapse = ", "), ")\n")
    } else {
      cat("  ⚠  Player stats: no rows for top-10 teams – check nation coverage\n")
      cat("     player_agg teams:", paste(head(player_agg$team, 10), collapse = ", "), "\n")
    }
  } else {
    cat("  ⚠  Player stats: expected columns not found in player_agg\n")
    cat("     Available:", paste(names(player_agg), collapse = ", "), "\n")
  }
} else {
  cat("  ℹ  player_agg empty – player stats plot skipped\n")
}


## 7F  Data-quality bar chart: colour by data_source_flag ───────────────────
if ("data_source_flag" %in% names(team_features)) {
  flag_data <- team_features |>
    dplyr::left_join(win_probs |> dplyr::select(team, win_pct), by = "team") |>
    dplyr::mutate(win_pct = tidyr::replace_na(win_pct, 0))

  p_dq <- ggplot2::ggplot(
    flag_data,
    ggplot2::aes(x = reorder(team, win_pct), y = win_pct,
                 fill = data_source_flag)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = c(full         = "#2A9D8F",
                 team_only    = "#E9C46A",
                 player_only  = "#457B9D",
                 proxy_only   = "#E63946"),
      name   = "Data source",
      drop   = FALSE
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "WC 2026 Win Probabilities – Coloured by Data Completeness",
      subtitle = paste0(
        "Green = full (match + player data) | ",
        "Yellow = team stats only | ",
        "Red = Elo proxy only\n",
        "full: ", sum(flag_data$data_source_flag == "full", na.rm=TRUE), " | ",
        "team_only: ", sum(flag_data$data_source_flag == "team_only", na.rm=TRUE), " | ",
        "proxy_only: ", sum(flag_data$data_source_flag == "proxy_only", na.rm=TRUE)
      ),
      x = NULL, y = "Win Probability"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(colour = "grey40", size = 8),
      panel.grid.major.y = ggplot2::element_blank()
    )

  ggplot2::ggsave("wc2026_output/data_quality_probabilities.png", p_dq,
                  width = 12, height = 14, dpi = 150)
  cat("  Saved: data_quality_probabilities.png\n")
}
################################################################################
##  BLOCK 4B – Deterministic "most likely path" bracket
##
##  PASTE THIS BLOCK INTO simulate.R, right after BLOCK 4 (Monte Carlo) and
##  before BLOCK 5. It reuses objects already in scope at that point:
##    WC_GROUPS, score_win_prob(), predict_goals(), ensemble_win_prob(),
##    team_features, OUT_DIR-equivalent ("wc2026_output")
##
##  Unlike Monte Carlo (which samples randomly many times), this traces ONE
##  path through the tournament always taking the higher-probability result
##  at every single match — i.e. "if the model's favourite always won, what
##  would the bracket look like?". This is what most "prediction bracket"
##  graphics show, and it's far easier to sanity-check by eye than a
##  Monte Carlo win-percentage table.
##
##  Outputs:
##    wc2026_output/bracket_group_matches.csv
##    wc2026_output/bracket_knockout.csv
##    wc2026_output/bracket_tree.png
################################################################################

cat("── Block 4B  Most-likely bracket path ────────────\n")

## ---- Group stage: always resolve each match to its most likely outcome ----
most_likely_group <- function(group_teams, group_label) {
  matchups  <- combn(group_teams, 2, simplify = FALSE)
  standings <- tibble::tibble(team = group_teams, pts = 0, gd = 0, gf = 0)
  match_rows <- list()

  for (mu in matchups) {
    ta <- mu[[1]]; tb <- mu[[2]]
    dc <- score_win_prob(ta, tb)                       # named c(win_a, draw, win_b)
    outcome <- names(dc)[which.max(dc)]                # most likely of the 3

    L <- tryCatch(predict_goals(ta, tb),
                  error = function(e) list(lambda_a = 1.2, lambda_b = 1.2))
    ga <- round(L$lambda_a); gb <- round(L$lambda_b)
    if (outcome == "win_a" && ga <= gb) ga <- gb + 1
    if (outcome == "win_b" && gb <= ga) gb <- ga + 1
    if (outcome == "draw"  && ga != gb) gb <- ga

    standings$gf[standings$team == ta] <- standings$gf[standings$team == ta] + ga
    standings$gf[standings$team == tb] <- standings$gf[standings$team == tb] + gb
    standings$gd[standings$team == ta] <- standings$gd[standings$team == ta] + (ga - gb)
    standings$gd[standings$team == tb] <- standings$gd[standings$team == tb] + (gb - ga)

    pts_a <- if (outcome == "win_a") 3L else if (outcome == "draw") 1L else 0L
    pts_b <- if (outcome == "win_b") 3L else if (outcome == "draw") 1L else 0L
    standings$pts[standings$team == ta] <- standings$pts[standings$team == ta] + pts_a
    standings$pts[standings$team == tb] <- standings$pts[standings$team == tb] + pts_b

    match_rows[[length(match_rows) + 1]] <- tibble::tibble(
      group   = group_label, stage = "Group", team_a = ta, team_b = tb,
      score   = paste0(ga, "-", gb),
      winner  = if (outcome == "draw") "Draw" else if (outcome == "win_a") ta else tb
    )
  }

  standings <- standings |>
    dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd), dplyr::desc(gf)) |>
    dplyr::mutate(rank = dplyr::row_number(), group = group_label)

  list(standings = standings, matches = dplyr::bind_rows(match_rows))
}

group_results <- lapply(names(WC_GROUPS), function(g)
  most_likely_group(WC_GROUPS[[g]], g))

bracket_group_matches <- dplyr::bind_rows(lapply(group_results, `[[`, "matches"))
group_standings       <- dplyr::bind_rows(lapply(group_results, `[[`, "standings"))

readr::write_csv(bracket_group_matches, "wc2026_output/bracket_group_matches.csv")
cat("  Saved: bracket_group_matches.csv\n")

## ---- Build 32-team bracket: top-2 per group (24) + best 8 third-placed ----
top2   <- group_standings |> dplyr::filter(rank <= 2) |> dplyr::arrange(group, rank)
thirds <- group_standings |>
  dplyr::filter(rank == 3) |>
  dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd), dplyr::desc(gf)) |>
  dplyr::slice_head(n = 8)

## Attempt to use FIFA's official Round-of-32 Annex C mapping (495 combos).
## If available online (Wikipedia/FIFA), parse the combinations table and
## deterministically place the 8 best third-placed teams into their
## Round-of-32 slots. If parsing fails, fall back to the legacy greedy
## swap approach used previously.
team_to_group <- unlist(lapply(names(WC_GROUPS), function(g) setNames(rep(g, length(WC_GROUPS[[g]])), WC_GROUPS[[g]])))

apply_fifa_r32_mapping <- function(top2_vec, thirds_vec) {
  # top2_vec: 24 teams (ordered by group + rank)
  # thirds_vec: 8 teams (ordered best-third ranking)
  # returns a 32-vector bracket if successful, otherwise NULL
  key_groups <- sort(unique(unname(team_to_group[thirds_vec])))
  if (length(key_groups) != 8) return(NULL)
  key <- paste(key_groups, collapse = ",")

  # Try to fetch & parse the Wikipedia table for Annex C
  url <- "https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_knockout_stage"
  ok <- tryCatch({
    page <- xml2::read_html(url)
    tbls <- rvest::html_nodes(page, "table")
    dfs <- lapply(tbls, function(x) tryCatch(rvest::html_table(x, fill = TRUE), error = function(e) NULL))
    dfs <- Filter(function(x) is.data.frame(x) && ncol(x) >= 10, dfs)
    rows <- do.call(rbind, lapply(dfs, as.data.frame))
    rows <- as.data.frame(rows, stringsAsFactors = FALSE)
    if (nrow(rows) < 10) stop("no table rows")

    # For each row, extract group letters (A-L) appearing as single letters
    # and the mapping tokens like '3E', '3J', etc. We build a lookup from
    # sorted-present-groups -> mapping vector (8 letters)
    mapping_list <- list()
    for (i in seq_len(nrow(rows))) {
      r <- unlist(rows[i, ])
      # tokens like '3E' or '3A' indicate mapping; single letters A-L indicate presence
      tokens <- unlist(strsplit(paste(r, collapse = " "), "\\s+"))
      tokens <- tokens[tokens != "" & !is.na(tokens)]
      pres <- unique(grep("^[A-L]$", tokens, value = TRUE))
      maps <- unique(grep("^3[A-L]$", tokens, value = TRUE))
      if (length(pres) == 8 && length(maps) == 8) {
        k <- paste(sort(pres), collapse = ",")
        # strip leading '3' -> keep letter order as found in maps
        mapping_list[[k]] <- substring(maps, 2)
      }
    }

    if (length(mapping_list) == 0) stop("no mapping rows parsed")

    # lookup for our key
    if (!key %in% names(mapping_list)) return(NULL)
    slot_groups <- mapping_list[[key]]
    # slot_groups is an 8-char vector like c('E','J','I',...)
    # Now place thirds_vec into bracket slots in the same order as slot_groups
    # Identify which R32 matches accept best-3rd teams and their slot indices
    # We reuse the existing bracket ordering convention: top2 (24) in group order
    # followed by 8 third slots — we will replace those 8 positions according
    # to slot_groups ordering.
    br <- c(as.character(top2_vec), rep(NA_character_, 8))
    # Build mapping from group letter -> team name for thirds_vec
    grp_to_team <- setNames(thirds_vec, unname(team_to_group[thirds_vec]))
    # Fill br[25:32] in the order of slot_groups
    for (j in seq_along(slot_groups)) {
      g <- slot_groups[j]
      br[24 + j] <- grp_to_team[[g]]
    }
    if (any(is.na(br))) return(NULL)
    br
  }, error = function(e) {
    NULL
  })
  ok
}

# Try authoritative mapping first, else fall back to greedy swap
bracket32 <- apply_fifa_r32_mapping(top2$team, thirds) 
if (is.null(bracket32)) {
  # Fallback: original naive concatenation with greedy swaps to avoid same-group
  bracket32 <- c(top2$team, thirds)
  stopifnot(length(bracket32) == 32)
  fix_round32_pairs <- function(br) {
    br <- as.character(br)
    for (i in seq(1, length(br), by = 2)) {
      a <- br[i]; b <- br[i+1]
      if (!is.null(team_to_group[a]) && !is.null(team_to_group[b]) && team_to_group[a] == team_to_group[b]) {
        swap_idx <- NA
        for (j in seq(i+2, length(br))) {
          if (team_to_group[br[j]] != team_to_group[a]) { swap_idx <- j; break }
        }
        if (!is.na(swap_idx)) {
          tmp <- br[swap_idx]; br[swap_idx] <- br[i+1]; br[i+1] <- tmp
        }
      }
    }
    br
  }
  bracket32 <- fix_round32_pairs(bracket32)
}

## ---- Knockout rounds: always advance the higher win-probability team ----
most_likely_knockout <- function(ta, tb, stage_label) {
  p <- ensemble_win_prob(ta, tb, extra_time = TRUE)
  winner <- if (unname(p["p_a"]) >= unname(p["p_b"])) ta else tb
  list(
    winner = winner,
    row = tibble::tibble(
      stage = stage_label, team_a = ta, team_b = tb,
      prob_a = round(unname(p["p_a"]), 3),
      prob_b = round(unname(p["p_b"]), 3),
      winner = winner
    )
  )
}
`%||%` <- function(a, b) if (length(a)) a else b   # tiny safety helper

round_names    <- c("Round of 32","Round of 16","Quarterfinal","Semifinal","Final")
current_round  <- bracket32
ko_rows         <- list()
round_idx       <- 1

while (length(current_round) > 1) {
  next_round <- character(0)
  for (i in seq(1, length(current_round), by = 2)) {
    res <- most_likely_knockout(current_round[i], current_round[i + 1],
                                 round_names[round_idx])
    ko_rows[[length(ko_rows) + 1]] <- res$row
    next_round <- c(next_round, res$winner)
  }
  current_round <- next_round
  round_idx <- round_idx + 1
}

bracket_knockout <- dplyr::bind_rows(ko_rows)
champion <- current_round[1]

readr::write_csv(bracket_knockout, "wc2026_output/bracket_knockout.csv")
cat("  Predicted champion (most-likely path):", champion, "\n")
cat("  Saved: bracket_knockout.csv\n\n")


################################################################################
##  BLOCK 4C – Tournament tree plot
################################################################################

cat("── Block 4C  Bracket tree plot ───────────────────\n")

## Assign x = round index, y = slot position, with parent y = mean of children y
ko <- bracket_knockout |>
  dplyr::mutate(round_no = match(stage, round_names))

n_rounds <- max(ko$round_no)
## y-positions: Round of 32 gets 16 evenly spaced slots; each later round's
## slot y = average of the two feeding slots (classic bracket layout)
slots <- list()
slots[[1]] <- ko |> dplyr::filter(round_no == 1) |>
  dplyr::mutate(slot = dplyr::row_number(),
                y    = rev(seq_len(dplyr::n())))
for (r in 2:n_rounds) {
  prev <- slots[[r - 1]]
  this <- ko |> dplyr::filter(round_no == r) |> dplyr::mutate(slot = dplyr::row_number())
  this$y <- vapply(this$slot, function(s) {
    mean(prev$y[c(2*s - 1, 2*s)])
  }, numeric(1))
  slots[[r]] <- this
}
plot_df <- dplyr::bind_rows(slots) |>
  dplyr::mutate(x = round_no,
                label_top = team_a, label_bot = team_b,
                y_top = y + 0.18, y_bot = y - 0.18)

## Connector segments: from each match's winner-y to the next round slot
seg_df <- plot_df |>
  dplyr::mutate(x_end = x + 1) |>
  dplyr::group_by(round_no) |>
  dplyr::mutate(next_slot = ceiling(dplyr::row_number() / 2)) |>
  dplyr::ungroup()

p_bracket <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = plot_df, ggplot2::aes(x = x, xend = x + 0.9, y = y_top, yend = y_top),
    colour = "grey70", linewidth = 0.4
  ) +
  ggplot2::geom_segment(
    data = plot_df, ggplot2::aes(x = x, xend = x + 0.9, y = y_bot, yend = y_bot),
    colour = "grey70", linewidth = 0.4
  ) +
  ggplot2::geom_text(
    data = plot_df,
    ggplot2::aes(x = x + 0.05, y = y_top,
                 label = label_top,
                 fontface = ifelse(label_top == winner, "bold", "plain"),
                 colour  = ifelse(label_top == winner, "#2A9D8F", "grey30")),
    hjust = 0, size = 3.0, vjust = -0.4
  ) +
  ggplot2::geom_text(
    data = plot_df,
    ggplot2::aes(x = x + 0.05, y = y_bot,
                 label = label_bot,
                 fontface = ifelse(label_bot == winner, "bold", "plain"),
                 colour  = ifelse(label_bot == winner, "#2A9D8F", "grey30")),
    hjust = 0, size = 3.0, vjust = 1.2
  ) +
  ggplot2::geom_text(
    data = plot_df,
    ggplot2::aes(x = x + 0.45, y = (y_top + y_bot) / 2,
                 label = scales::percent(pmax(prob_a, 1 - prob_a), accuracy = 1)),
    size = 2.2, colour = "grey45"
  ) +
  ggplot2::geom_point(data = plot_df, ggplot2::aes(x = x + 0.9, y = (y_top + y_bot)/2, colour = winner), size = 1.8, show.legend = FALSE) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_x_continuous(
    breaks = seq_len(n_rounds), labels = round_names[seq_len(n_rounds)],
    limits = c(0.8, n_rounds + 1.6)
  ) +
  ggplot2::scale_y_continuous(breaks = NULL) +
  ggplot2::labs(
    title    = "WC 2026 – Most Likely Bracket Path",
    subtitle = glue::glue("Predicted champion: {champion}  |  ",
                          "Each match shows the favourite's win probability"),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(face = "bold"),
    plot.title      = ggplot2::element_text(face = "bold", size = 14)
  )

ggplot2::ggsave("wc2026_output/bracket_tree.png", p_bracket,
                width = 16, height = 10, dpi = 150)
cat("  Saved: bracket_tree.png\n")
cat("\n══════════════════════════════════════════════════\n")
cat(" Most-likely-path bracket complete. Champion:", champion, "\n")
cat("══════════════════════════════════════════════════\n")

cat("\n══════════════════════════════════════════════════\n")
cat(" Simulation complete.\n")
cat("  Outputs in: wc2026_output/\n\n")
cat("  final_predictions.csv          – full ranked table\n")
cat("  win_probabilities.png          – bar chart with 95% CI\n")
cat("  simulation_proof.csv           – audit of what data was used\n")
cat("  data_quality_probabilities.png – coverage by team\n")
cat("  team_strength_map.png          – world choropleth\n")
cat("  h2h_heatmap.png                – pairwise win-prob matrix\n")
cat("  strength_radar.png             – radar charts (top 8)\n")
cat("  player_stats_top10.png         – squad player aggregates\n")
cat("══════════════════════════════════════════════════\n")