################################################################################
##  World Cup 2026 – Probabilistic Winner Prediction in R
##
##  Architecture
##  ─────────────────────────────────────────────────────────────────────────
##  BLOCK 0  │ Package installation & setup
##  BLOCK 1  │ Data acquisition  (TeamStats + PlayerStats switches)
##            │   1A – National-team match results  (FBref via worldfootballR)
##            │   1B – Player-level stats           (FBref via worldfootballR)
##            │   1C – Supplementary squad values   (Transfermarkt)
##  BLOCK 2  │ Feature engineering
##            │   2A – Team features  (form, qualifier stats, Elo, head-to-head)
##            │   2B – Player aggregate features per squad
##            │   2C – Head-to-head league graph + map visualisation
##  BLOCK 3  │ Modelling
##            │   3A – Bradley-Terry paired-comparison model  (probabilistic)
##            │   3B – Dixon-Coles Poisson model              (score-based)
##            │   3C – Bayesian ensemble with Stan / brms     (optional)
##  BLOCK 4  │ Tournament simulation (Monte Carlo, 10 000 runs)
##  BLOCK 5  │ Visualisations
##            │   5A – Team strength radar / spider charts
##            │   5B – Win-probability bar chart
##            │   5C – Bracket / knockout tree
##            │   5D – Head-to-head network on world map
##  BLOCK 6  │ Output – ranked probability table
##
##  DATA SOURCES (free / open)
##  ─────────────────────────────────────────────────────────────────────────
##  • worldfootballR  →  FBref + Transfermarkt + Understat
##  • football-data.org free tier  (fixtures, results, standings)
##  • FIFA ranking CSV  (manually downloadable from fifa.com)
##  • ClubElo.com      (historical Elo ratings, free JSON API)
##
##  AUTHOR NOTE
##  Set the two switches below and provide your optional API key.
##  Run BLOCK 0 once to install packages, then source the whole file.
################################################################################



################################################################################
##  BLOCK 0 – Package installation & loading
################################################################################

required_pkgs <- c(
  # Data collection
  "worldfootballR",   # FBref / Transfermarkt / Understat
  "httr2",            # HTTP requests (football-data.org API)
  "jsonlite",         # JSON parsing
  "rvest",            # HTML scraping fallback
  # Data wrangling
  "dplyr", "tidyr", "purrr", "stringr", "lubridate", "janitor",
  # Modelling
  "BradleyTerry2",    # paired-comparison / Bradley-Terry model
  "lme4",             # mixed-effects Poisson (Dixon-Coles alternative)
  "brms",             # Bayesian regression (Stan backend, optional)
  # Visualisation
  "ggplot2", "ggrepel", "patchwork",
  "igraph",           # network graph (head-to-head league)
  "ggraph",           # ggplot2-based network plots
  "maps", "sf", "rnaturalearth", "rnaturalearthdata",  # world map
  "fmsb",             # radar / spider charts
  "scales", "viridis", "RColorBrewer",
  # Utilities
  "progressr", "tictoc", "glue", "here"
)

new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
  # worldfootballR: prefer GitHub dev version for latest FBref URLs
  if ("worldfootballR" %in% new_pkgs) {
    if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
    devtools::install_github("JaseZiv/worldfootballR")
  }
}

invisible(lapply(required_pkgs, library, character.only = TRUE))
options(dplyr.summarise.inform = FALSE)


################################################################################
##  World Cup 2026 – Probabilistic Winner Prediction in R  (v2 – fixed)
##
##  FIXES vs v1
##  ─────────────────────────────────────────────────────────────────────────
##  [BUG-1]  GROUPS – updated to the OFFICIAL confirmed draw (all 48 slots
##            filled after March 31 2026 playoffs). Team names now use the
##            same canonical names as the rest of the code.
##
##  [BUG-2]  Block 6 crashed with "Column `fifa_rank` doesn't exist".
##            Fixed: use dplyr::any_of() for all optional columns, and add
##            a safe fallback when fifa_ranking was the placeholder.
##
##  [BUG-3]  Radar chart: "The number of variables must be 3 or more."
##            Fixed: guard against <3 numeric columns; supply NaN-imputed
##            defaults for missing feature columns so there are always 5
##            dimensions when radar is requested.
##
##  [BUG-4]  Block 1A name-mapping loop used undefined variable `comp`.
##            Fixed: use the competition column already present in the df.
##
##  [BUG-5]  simulate_group() points arithmetic was wrong for away wins
##            (pts_b expression used 3L - pts_a - ... which was garbled).
##            Fixed: clean case_when logic.
##
##  [BUG-6]  predict_goals() silently returned wrong length vector when a
##            team was missing from team_features, causing rpois() to fail.
##            Fixed: explicit length-0 guard with a safe fallback lambda.
##
##  [BUG-7]  bt_df fallback was built from team_features before bt_ability
##            was available, causing circular reference in some code paths.
##            Fixed: build bt_df unconditionally at model stage.
##
##  [BUG-8]  force_refresh flag was referenced in the intl competitions
##            fetch but the cache check also cached the PRE-parsed raw
##            frame instead of the parsed all_results frame, so re-running
##            with force_refresh=FALSE still re-parsed (harmless but slow).
##            Fixed: cache the final parsed all_results.
##
##  DATA NOTE
##  ─────────────────────────────────────────────────────────────────────────
##  The official 2026 FIFA World Cup groups (confirmed after March 31 2026):
##
##    A: Mexico, South Korea, South Africa, Czechia
##    B: Canada, Switzerland, Qatar, Bosnia and Herzegovina
##    C: Brazil, Morocco, Scotland, Haiti
##    D: USA, Australia, Paraguay, Türkiye
##    E: Germany, Ecuador, Ivory Coast, Curaçao
##    F: Netherlands, Japan, Tunisia, Sweden
##    G: Belgium, Iran, Egypt, New Zealand
##    H: Spain, Uruguay, Saudi Arabia, Cape Verde
##    I: France, Senegal, Norway, Iraq
##    J: Argentina, Austria, Algeria, Jordan
##    K: Portugal, Colombia, Uzbekistan, Congo DR
##    L: England, Croatia, Panama, Ghana
################################################################################

## ── USER SWITCHES ──────────────────────────────────────────────────────────────
TeamStats    <- TRUE   # fetch national-team match results (FBref + football-data.org)
PlayerStats  <- TRUE   # fetch individual player stats from FBref
force_refresh <- FALSE  # set TRUE to bypass cached RDS files

FOOTBALL_DATA_KEY <- Sys.getenv("FOOTBALL_DATA_KEY")  # optional; set in .Renviron

dir.create("wc2026_output", showWarnings = FALSE)
dir.create("data",          showWarnings = FALSE)

# Run mode: 'all' (default) | 'fetch' (only data fetching & preprocessing) |
# 'simulate' (only modelling + simulation + visualisations). You can set
# this from an external script before sourcing this file, e.g.
# `run_mode <- 'fetch'; source('predict_wc2026.R')`
if (!exists("run_mode")) run_mode <- "all"


################################################################################
##  BLOCK 0 – Packages
################################################################################

required_pkgs <- c(
  "worldfootballR","httr2","jsonlite","rvest",
  "dplyr","tidyr","purrr","stringr","lubridate","janitor","readr","tibble",
  "rlang",
  "BradleyTerry2","lme4",
  "ggplot2","ggrepel","patchwork","igraph","ggraph",
  "sf","rnaturalearth","rnaturalearthdata",
  "fmsb","scales","viridis","RColorBrewer",
  "progressr","glue"
)

new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly=TRUE)]
if (length(new_pkgs) > 0) {
  message("Installing: ", paste(new_pkgs, collapse=", "))
  install.packages(setdiff(new_pkgs, "worldfootballR"),
                   repos = "https://cloud.r-project.org", quiet = TRUE)
  if ("worldfootballR" %in% new_pkgs) {
    if (!requireNamespace("devtools", quietly=TRUE)) install.packages("devtools")
    devtools::install_github("JaseZiv/worldfootballR", quiet=TRUE)
  }
}
invisible(lapply(required_pkgs, library, character.only=TRUE))
options(dplyr.summarise.inform = FALSE)

## ── Canonical team-name map ────────────────────────────────────────────────────
## All team names throughout the script resolve to these canonical strings.
CANONICAL_MAP <- c(
  "USA"                       = "United States",
  "United States of America"  = "United States",
  "Korea Republic"            = "South Korea",
  "Republic of Korea"         = "South Korea",
  "Curacao"                   = "Curaçao",
  "Turkey"                    = "Türkiye",
  "Ivory Coast"               = "Côte d'Ivoire",
  "Cote d'Ivoire"             = "Côte d'Ivoire",
  "Cote dIvoire"              = "Côte d'Ivoire",
  "DR Congo"                  = "Congo DR",
  "Democratic Republic of Congo" = "Congo DR",
  "DRC"                       = "Congo DR",
  "Bosnia-Herzegovina"        = "Bosnia and Herzegovina",
  "Bosnia & Herzegovina"      = "Bosnia and Herzegovina",
  "Bosnia-Herzegowina"        = "Bosnia and Herzegovina",
  "Cape Verde"                = "Cabo Verde",
  "Scotland"                  = "Scotland",
  "Haiti"                     = "Haiti",
  "Qatar"                     = "Qatar",
  "Paraguay"                  = "Paraguay",
  "Iraq"                      = "Iraq",
  "Norway"                    = "Norway",
  "Sweden"                    = "Sweden",
  "Czechia"                   = "Czechia",
  "Czech Republic"            = "Czechia"
)

canon <- function(x) dplyr::recode(x, !!!CANONICAL_MAP, .default = x)

## Official 48 WC2026 team names (canonical)
WC_TEAMS <- canon(c(
  # UEFA (16)
  "France","Spain","England","Portugal","Germany","Netherlands","Belgium",
  "Switzerland","Croatia","Austria","Serbia","Denmark","Ukraine","Türkiye",
  "Sweden","Norway",
  # New UEFA qualifiers
  "Bosnia and Herzegovina","Czechia",
  # CONMEBOL (6)
  "Brazil","Argentina","Colombia","Uruguay","Ecuador","Paraguay",
  # CAF (10)
  "Morocco","Senegal","Egypt","South Africa","Algeria",
  "Ghana","Cabo Verde","Tunisia","Côte d'Ivoire","Congo DR",
  # AFC (9)
  "Japan","South Korea","Iran","Saudi Arabia","Australia",
  "Jordan","Uzbekistan","Iraq","Qatar",
  # CONCACAF (6 incl hosts)
  "United States","Mexico","Canada","Panama","Scotland","Haiti",
  # OFC (0 – New Zealand didn't qualify; Scotland via CONCACAF path)
  # Intercontinental
  "Curaçao"
))
WC_TEAMS <- unique(WC_TEAMS)

## Official 2026 WC groups – ALL CONFIRMED (updated March 31 2026) [BUG-1 FIX]
WC_GROUPS <- list(
  A = canon(c("Mexico",      "South Korea",           "South Africa",          "Czechia")),
  B = canon(c("Canada",      "Switzerland",           "Qatar",                 "Bosnia and Herzegovina")),
  C = canon(c("Brazil",      "Morocco",               "Scotland",              "Haiti")),
  D = canon(c("United States","Australia",            "Paraguay",              "Türkiye")),
  E = canon(c("Germany",     "Ecuador",               "Côte d'Ivoire",         "Curaçao")),
  F = canon(c("Netherlands", "Japan",                 "Tunisia",               "Sweden")),
  G = canon(c("Belgium",     "Iran",                  "Egypt",                 "New Zealand")),
  H = canon(c("Spain",       "Uruguay",               "Saudi Arabia",          "Cabo Verde")),
  I = canon(c("France",      "Senegal",               "Norway",                "Iraq")),
  J = canon(c("Argentina",   "Austria",               "Algeria",               "Jordan")),
  K = canon(c("Portugal",    "Colombia",              "Uzbekistan",            "Congo DR")),
  L = canon(c("England",     "Croatia",               "Panama",                "Ghana"))
)


################################################################################
##  BLOCK 1A – National-team match results
################################################################################

all_results_cache <- "wc2026_output/all_results_parsed.rds"

if (TeamStats && run_mode %in% c("all", "fetch")) {

  message("\n── 1A  National-team match results ──")

  if (file.exists(all_results_cache) && !force_refresh) {
    message("  Loading from cache: ", all_results_cache)
    all_results <- readRDS(all_results_cache)
  } else {

    intl_competitions <- c(
      "FIFA World Cup","UEFA Euro","Copa America",
      "Africa Cup of Nations","AFC Asian Cup",
      "CONCACAF Gold Cup","UEFA Nations League"
    )

    ## FBref pull (raw) -----------------------------------------------------------
    raw_cache <- "wc2026_output/all_results_raw.rds"
    if (file.exists(raw_cache) && !force_refresh) {
      intl_raw <- readRDS(raw_cache)
    } else {
      intl_raw <- purrr::map_dfr(intl_competitions, function(comp) {
        message("  FBref: ", comp)
        tryCatch({
          df <- worldfootballR::load_match_comp_results(comp_name = comp)
          df$competition <- comp
          Sys.sleep(3)
          df
        }, error = function(e) {
          warning("FBref failed [", comp, "]: ", e$message)
          NULL
        })
      })
      if (nrow(intl_raw) > 0) saveRDS(intl_raw, raw_cache)
    }

    ## Log column names so we can inspect them
    writeLines(names(intl_raw), "wc2026_output/fbref_columns.txt")

    ## Robust column discovery ------------------------------------------------
    df_names_lc <- tolower(names(intl_raw))
    find_col <- function(candidates) {
      lc_cands <- tolower(candidates)
      idx <- match(lc_cands, df_names_lc)
      hit <- idx[!is.na(idx)][1]
      if (!is.na(hit)) return(names(intl_raw)[hit])
      NA_character_
    }

    date_col  <- find_col(c("Date","date","utcDate","match_date","kickoff"))
    home_col  <- find_col(c("Home","home","home_team","home_team_name","HomeTeam"))
    away_col  <- find_col(c("Away","away","away_team","away_team_name","AwayTeam"))
    score_col <- find_col(c("Score","score","Result","result"))
    hg_col    <- find_col(c("home_goals","HomeGoals","FTHG","hg","home_score"))
    ag_col    <- find_col(c("away_goals","AwayGoals","FTAG","ag","away_score"))

    message("  Detected columns → date:", date_col, " home:", home_col,
            " away:", away_col, " score:", score_col,
            " hg:", hg_col, " ag:", ag_col)

    if (nrow(intl_raw) > 0 && !is.na(home_col)) {
      df <- intl_raw

      ## Date
      df$date <- if (!is.na(date_col)) as.Date(df[[date_col]]) else as.Date(NA)

      ## Teams
      df$home_team <- canon(as.character(df[[home_col]]))
      df$away_team <- canon(as.character(df[[away_col]]))

      ## Goals  [BUG-4 FIX: removed reference to undefined `comp` variable]
      if (!is.na(hg_col) && !is.na(ag_col)) {
        df$home_goals <- suppressWarnings(as.integer(df[[hg_col]]))
        df$away_goals <- suppressWarnings(as.integer(df[[ag_col]]))
      } else if (!is.na(score_col)) {
        sc <- as.character(df[[score_col]])
        ## handle various separators: – — - : (but not negative numbers)
        sc_split <- stringr::str_match(sc, "^(\\d+)\\s*[–—\\-:]\\s*(\\d+)$")
        df$home_goals <- suppressWarnings(as.integer(sc_split[, 2]))
        df$away_goals <- suppressWarnings(as.integer(sc_split[, 3]))
      } else {
        df$home_goals <- NA_integer_
        df$away_goals <- NA_integer_
      }

      ## Keep competition column (already set during fetch loop)
      if (!"competition" %in% names(df)) df$competition <- "Unknown"

      all_results <- df |>
        dplyr::filter(
          !is.na(date),
          date >= as.Date("2022-01-01"),
          !is.na(home_goals), !is.na(away_goals),
          home_team %in% WC_TEAMS | away_team %in% WC_TEAMS
        ) |>
        dplyr::select(date, home_team, away_team,
                      home_goals, away_goals, competition)
    } else {
      message("  WARNING: FBref returned no usable rows; will rely on FIFA ranking only.")
      all_results <- tibble::tibble(
        date=as.Date(character()), home_team=character(), away_team=character(),
        home_goals=integer(), away_goals=integer(), competition=character()
      )
    }

    ## football-data.org WC matches (live tournament) -------------------------
    fetch_fdorg <- function(ep, key = FOOTBALL_DATA_KEY) {
      req <- httr2::request(paste0("https://api.football-data.org/v4/", ep))
      if (nchar(key) > 0) req <- httr2::req_headers(req, `X-Auth-Token` = key)
      tryCatch(
        httr2::req_perform(req) |> httr2::resp_body_json(simplifyVector = TRUE),
        error = function(e) { message("  FDORG API: ", e$message); NULL }
      )
    }

    fdorg_cache <- "wc2026_output/fdorg_matches.rds"
    if (file.exists(fdorg_cache) && !force_refresh) {
      qualifier_results <- readRDS(fdorg_cache)
    } else {
      qj <- fetch_fdorg("competitions/WC/matches?season=2026")
      if (!is.null(qj$matches) && nrow(as.data.frame(qj$matches)) > 0) {
        qualifier_results <- as.data.frame(qj$matches) |>
          dplyr::select(
            date       = utcDate,
            home_team  = homeTeam.name,
            away_team  = awayTeam.name,
            home_goals = score.fullTime.home,
            away_goals = score.fullTime.away,
            status     = status
          ) |>
          dplyr::filter(status == "FINISHED") |>
          dplyr::mutate(
            date       = as.Date(date),
            home_team  = canon(home_team),
            away_team  = canon(away_team),
            home_goals = as.integer(home_goals),
            away_goals = as.integer(away_goals),
            competition = "WC 2026"
          ) |>
          dplyr::select(-status)
        saveRDS(qualifier_results, fdorg_cache)
        message("  football-data.org: ", nrow(qualifier_results), " WC2026 matches.")
      } else {
        message("  football-data.org returned no matches.")
        qualifier_results <- tibble::tibble(
          date=as.Date(character()), home_team=character(), away_team=character(),
          home_goals=integer(), away_goals=integer(), competition=character()
        )
      }
    }

    all_results <- dplyr::bind_rows(all_results, qualifier_results) |>
      dplyr::distinct(date, home_team, away_team, .keep_all = TRUE)

    ## Recency + competition weights
    all_results <- all_results |>
      dplyr::mutate(
        days_ago    = as.numeric(Sys.Date() - date),
        time_weight = exp(-days_ago / 730),
        comp_weight = dplyr::case_when(
          stringr::str_detect(competition, "WC 2026|Qualifier|World Cup") ~ 1.5,
          stringr::str_detect(competition, "Euro|Copa|Nations|Asian Cup|AFCON|Gold Cup") ~ 1.2,
          TRUE ~ 0.7
        ),
        match_weight = time_weight * comp_weight
      )

    saveRDS(all_results, all_results_cache)
    message("  → ", nrow(all_results), " matches after parsing.")
  }

  ## Diagnostics
  diag <- c(
    paste0("all_results rows: ", nrow(all_results)),
    paste0("date range: ", min(all_results$date, na.rm=TRUE),
           " – ", max(all_results$date, na.rm=TRUE)),
    "teams seen:",
    paste(sort(unique(c(all_results$home_team, all_results$away_team))), collapse=", "),
    "missing from WC_TEAMS:",
    paste(setdiff(WC_TEAMS,
                  unique(c(all_results$home_team, all_results$away_team))), collapse=", ")
  )
  writeLines(diag, "wc2026_output/data_diagnostics.txt")

} else {
  all_results <- tibble::tibble(
    date=as.Date(character()), home_team=character(), away_team=character(),
    home_goals=integer(), away_goals=integer(), competition=character(),
    match_weight=numeric()
  )
}


################################################################################
##  BLOCK 1B – Player statistics (FBref scouting reports)
################################################################################

if (PlayerStats) {

  message("\n── 1B  Player statistics ──")

  team_fbref_urls <- list(
    "France"       = "https://fbref.com/en/squads/3306a0c6/France-Stats",
    "Spain"        = "https://fbref.com/en/squads/53a2f082/Spain-Stats",
    "England"      = "https://fbref.com/en/squads/cff3d9bb/England-Stats",
    "Germany"      = "https://fbref.com/en/squads/adccbe41/Germany-Stats",
    "Brazil"       = "https://fbref.com/en/squads/9d012f1e/Brazil-Stats",
    "Argentina"    = "https://fbref.com/en/squads/f9fddd6e/Argentina-Stats",
    "Portugal"     = "https://fbref.com/en/squads/9a818eff/Portugal-Stats",
    "Netherlands"  = "https://fbref.com/en/squads/fc07b09a/Netherlands-Stats",
    "Morocco"      = "https://fbref.com/en/squads/231ed2f2/Morocco-Stats",
    "Japan"        = "https://fbref.com/en/squads/a3bdbfcd/Japan-Stats",
    "United States"= "https://fbref.com/en/squads/7f3b5eda/United-States-Stats",
    "Mexico"       = "https://fbref.com/en/squads/9abc0167/Mexico-Stats",
    "Belgium"      = "https://fbref.com/en/squads/0fc19eba/Belgium-Stats",
    "Croatia"      = "https://fbref.com/en/squads/2b98ca53/Croatia-Stats",
    "Uruguay"      = "https://fbref.com/en/squads/020ca5dc/Uruguay-Stats",
    "Senegal"      = "https://fbref.com/en/squads/6b12e54e/Senegal-Stats",
    "Switzerland"  = "https://fbref.com/en/squads/a9077cb2/Switzerland-Stats",
    "Colombia"     = "https://fbref.com/en/squads/4419abe4/Colombia-Stats",
    "South Korea"  = "https://fbref.com/en/squads/71782d9d/South-Korea-Stats",
    "Australia"    = "https://fbref.com/en/squads/e8c3f88e/Australia-Stats"
    ## TODO: add remaining 28 teams; use fb_teams_urls() to discover automatically
  )

  player_stats_cache <- "wc2026_output/player_stats_raw.rds"
  if (!file.exists(player_stats_cache) || force_refresh) {
    player_urls_df <- purrr::imap_dfr(team_fbref_urls, function(url, team) {
      Sys.sleep(4)
      tryCatch({
        urls <- worldfootballR::fb_player_urls(team_url = url)
        tibble::tibble(team = team, player_url = urls)
      }, error = function(e) { warning("URLs failed [", team, "]: ", e$message); NULL })
    })

    message("  Fetching scouting reports for ", nrow(player_urls_df), " players …")
    player_stats_raw <- purrr::map2_dfr(
      player_urls_df$player_url, player_urls_df$team,
      function(url, team) {
        Sys.sleep(3)
        tryCatch({
          df <- worldfootballR::fb_player_scouting_report(url, pos_versus = "primary")
          df$national_team <- team; df
        }, error = function(e) NULL)
      }, .progress = TRUE
    )
    saveRDS(player_stats_raw, player_stats_cache)
  } else {
    player_stats_raw <- readRDS(player_stats_cache)
    message("  Player stats loaded from cache.")
  }

  ## Club-level stats (Big5, 3 seasons)
  club_stats_cache <- "wc2026_output/club_stats_raw.rds"
  if (!file.exists(club_stats_cache) || force_refresh) {
    club_stats_raw <- purrr::map_dfr(2023:2025, function(yr) {
      purrr::map_dfr(c("standard","shooting","passing","defense","possession","keeper"),
        function(st) {
          message("  Club stats yr=", yr, " type=", st)
          Sys.sleep(4)
          tryCatch(
            worldfootballR::fb_big5_advanced_season_stats(yr, st, "player") |>
              dplyr::mutate(season = yr, stat_group = st),
            error = function(e) NULL
          )
        })
    })
    saveRDS(club_stats_raw, club_stats_cache)
  } else {
    club_stats_raw <- readRDS(club_stats_cache)
    message("  Club stats loaded from cache.")
  }
}


################################################################################
##  BLOCK 1C – FIFA ranking (with safe fallback)
################################################################################

message("\n── 1C  FIFA ranking ──")

fifa_ranking_file <- "data/fifa_ranking_2026.csv"
if (file.exists(fifa_ranking_file)) {
  fifa_ranking <- readr::read_csv(fifa_ranking_file, show_col_types = FALSE) |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::rename(team = dplyr::any_of(c("team","country","nation")),
                  fifa_rank   = dplyr::any_of(c("rank","fifa_rank","position")),
                  fifa_points = dplyr::any_of(c("points","fifa_points","total_points"))) |>
    dplyr::mutate(team = canon(team))
} else {
  message("  Using embedded fallback FIFA ranking (all 48 WC teams).")
  ## Approximate April 2026 ranking for ALL 48 qualified teams
  fifa_ranking <- tibble::tibble(
    team = WC_TEAMS,
    fifa_rank = seq_along(WC_TEAMS),
    fifa_points = seq(1850, by = -(1850 - 1200) / (length(WC_TEAMS) - 1),
                      length.out = length(WC_TEAMS))
  )
  ## Manually adjust approximate rank order for well-known teams
  rank_order <- c(
    "France","Spain","England","Brazil","Argentina","Portugal","Germany",
    "Netherlands","Belgium","Morocco","Japan","Colombia","Switzerland",
    "Uruguay","Croatia","Austria","United States","Mexico","Denmark",
    "South Korea","Iran","Saudi Arabia","Egypt","Senegal","Ghana","Algeria",
    "Ecuador","Paraguay","Serbia","Norway","Sweden","Australia",
    "Côte d'Ivoire","South Africa","Tunisia","Czechia","Cabo Verde",
    "Bosnia and Herzegovina","Uzbekistan","Jordan","Türkiye","Canada",
    "Qatar","Congo DR","Iraq","Panama","Curaçao","Scotland","New Zealand",
    "Haiti"
  )
  rank_order <- intersect(rank_order, WC_TEAMS)
  missing_in_order <- setdiff(WC_TEAMS, rank_order)
  rank_order <- c(rank_order, missing_in_order)
  fifa_ranking <- tibble::tibble(
    team        = rank_order,
    fifa_rank   = seq_along(rank_order),
    fifa_points = seq(1850, by = -(1850 - 1200) / (length(rank_order) - 1),
                      length.out = length(rank_order))
  )
}


################################################################################
##  BLOCK 2A – Team features
################################################################################

message("\n── 2A  Team features ──")

## Try to load cached match / player data if present to avoid re-fetching
if ((!exists("all_results") || (exists("all_results") && nrow(all_results) == 0)) &&
    file.exists("wc2026_output/all_results_parsed.rds")) {
  try({
    all_results <- readRDS("wc2026_output/all_results_parsed.rds")
    message("  Loaded cached parsed all_results (wc2026_output/all_results_parsed.rds) — ", nrow(all_results), " rows")
  }, silent = TRUE)
} else if ((!exists("all_results") || (exists("all_results") && nrow(all_results) == 0)) &&
           file.exists("wc2026_output/all_results.rds")) {
  try({
    all_results <- readRDS("wc2026_output/all_results.rds")
    message("  Loaded cached raw all_results (wc2026_output/all_results.rds) — ", nrow(all_results), " rows")
  }, silent = TRUE)
}

## If PlayerStats requested, try to load cached player stats
if (PlayerStats && !exists("player_stats_raw") && file.exists("wc2026_output/player_stats_raw.rds")) {
  try({
    player_stats_raw <- readRDS("wc2026_output/player_stats_raw.rds")
    message("  Loaded cached player_stats_raw (wc2026_output/player_stats_raw.rds) — ", nrow(player_stats_raw), " rows")
  }, silent = TRUE)
}

has_results <- nrow(all_results) >= 10

if (has_results) {

  results_long <- dplyr::bind_rows(
    all_results |> dplyr::transmute(
      date, match_weight, competition,
      team = home_team, opponent = away_team,
      gf = home_goals, ga = away_goals),
    all_results |> dplyr::transmute(
      date, match_weight, competition,
      team = away_team, opponent = home_team,
      gf = away_goals, ga = home_goals)
  ) |>
    dplyr::filter(team %in% WC_TEAMS) |>
    dplyr::mutate(
      result    = dplyr::case_when(gf > ga ~ "W", gf < ga ~ "L", TRUE ~ "D"),
      points    = dplyr::case_when(result == "W" ~ 3L, result == "D" ~ 1L, TRUE ~ 0L),
      goal_diff = gf - ga
    )

  ## Weighted team-level aggregates
  team_features <- results_long |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      n_matches     = n(),
      weighted_pts  = sum(match_weight * points),
      w_gf          = sum(match_weight * gf) / sum(match_weight),
      w_ga          = sum(match_weight * ga) / sum(match_weight),
      w_gd          = w_gf - w_ga,
      form_last5    = {
        r5 <- tail(dplyr::arrange(dplyr::cur_data(), date), 5)
        mean(r5$points)
      }
    )

  qualifier_features <- results_long |>
    dplyr::filter(stringr::str_detect(competition, "Qualifier|World Cup")) |>
    dplyr::group_by(team) |>
    dplyr::summarise(q_n = n(), q_pts_per = mean(points), q_gd_per = mean(goal_diff))

  ## Scaffold: ensure every WC team has a row even if data-poor
  team_features <- tibble::tibble(team = WC_TEAMS) |>
    dplyr::left_join(team_features,     by = "team") |>
    dplyr::left_join(qualifier_features, by = "team") |>
    dplyr::left_join(
      fifa_ranking |> dplyr::select(team, fifa_rank, fifa_points),
      by = "team"
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)),
      ## If still no FIFA points after join, use a rank-based proxy
      fifa_points = dplyr::if_else(fifa_points == 0 & !is.na(fifa_rank),
                                   1850 - fifa_rank * 15, fifa_points),
      fifa_strength = scales::rescale(fifa_points, to = c(0.2, 1.0)),
      attack_str    = scales::rescale(w_gf,         to = c(0, 1)),
      defence_str   = scales::rescale(-w_ga,         to = c(0, 1)),
      form_last5_sc = scales::rescale(form_last5,    to = c(0, 1)),
      q_pts_sc      = scales::rescale(q_pts_per,     to = c(0, 1)),
      team_strength = 0.40 * fifa_strength +
                      0.25 * attack_str    +
                      0.20 * defence_str   +
                      0.15 * form_last5_sc
    )

} else {
  message("  No match results – using FIFA ranking proxy only.")
  team_features <- tibble::tibble(team = WC_TEAMS) |>
    dplyr::left_join(fifa_ranking |> dplyr::select(team, fifa_rank, fifa_points),
                     by = "team") |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)),
      fifa_points   = dplyr::if_else(fifa_points == 0, 1850 - fifa_rank * 15, fifa_points),
      fifa_strength = scales::rescale(fifa_points, to = c(0.2, 1.0)),
      attack_str    = fifa_strength,
      defence_str   = fifa_strength,
      form_last5    = 1.5, form_last5_sc = 0.5,
      q_pts_per     = 1.5, q_pts_sc      = 0.5,
      team_strength = fifa_strength,
      w_gf = 1.2, w_ga = 1.0, w_gd = 0.2,
      n_matches = 0L, weighted_pts = 0, q_n = 0L, q_gd_per = 0
    )
  results_long <- tibble::tibble(
    team = character(), date = as.Date(character()),
    match_weight = numeric(), competition = character(),
    opponent = character(), gf = integer(), ga = integer(),
    result = character(), points = integer(), goal_diff = integer()
  )
}

team_features <- team_features |>
  dplyr::mutate(elo_proxy = 1500 + (team_strength - 0.5) * 600)

message("  Team features built for ", nrow(team_features), " teams.")


################################################################################
##  BLOCK 2B – Player aggregate features per squad
################################################################################

if (PlayerStats && exists("player_stats_raw") && nrow(player_stats_raw) > 0) {

  message("\n── 2B  Player aggregates ──")

  player_agg <- player_stats_raw |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(per90)) |>
    dplyr::group_by(national_team, scouting_period) |>
    dplyr::summarise(
      squad_goals_p90    = mean(per90[statistic == "Goals"],         na.rm=TRUE),
      squad_xg_p90       = mean(per90[statistic == "xG"],            na.rm=TRUE),
      squad_assists_p90  = mean(per90[statistic == "Assists"],       na.rm=TRUE),
      squad_xa_p90       = mean(per90[statistic == "xAG"],           na.rm=TRUE),
      squad_prgpass_p90  = mean(per90[statistic == "Prog Passes"],   na.rm=TRUE),
      squad_dribbles_p90 = mean(per90[statistic == "Take-Ons Att"],  na.rm=TRUE),
      squad_tackles_p90  = mean(per90[statistic == "Tackles"],       na.rm=TRUE),
      squad_cards_p90    = mean(per90[statistic %in%
                             c("Yellow Cards","Red Cards")],          na.rm=TRUE),
      n_players = n()
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(national_team) |>
    dplyr::slice_max(scouting_period, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  team_features <- team_features |>
    dplyr::left_join(player_agg |> dplyr::rename(team = national_team), by = "team")
}


################################################################################
##  BLOCK 2C – Head-to-head network  +  world map
################################################################################

message("\n── 2C  H2H network & map ──")

if (has_results && nrow(all_results) > 0) {

  h2h <- all_results |>
    dplyr::filter(home_team %in% WC_TEAMS, away_team %in% WC_TEAMS) |>
    dplyr::mutate(winner = dplyr::case_when(
      home_goals > away_goals ~ home_team,
      away_goals > home_goals ~ away_team,
      TRUE ~ NA_character_)) |>
    dplyr::group_by(home_team, away_team) |>
    dplyr::summarise(
      n_matches    = n(),
      home_wins    = sum(winner == home_team, na.rm=TRUE),
      away_wins    = sum(winner == away_team, na.rm=TRUE),
      draws        = sum(is.na(winner)),
      home_win_pct = home_wins / n_matches
    ) |>
    dplyr::ungroup()

  edges <- h2h |> dplyr::filter(n_matches >= 2) |>
    dplyr::select(from = home_team, to = away_team, weight = home_win_pct)

  if (nrow(edges) >= 3) {
    g <- igraph::graph_from_data_frame(d = edges,
                                       vertices = unique(c(edges$from, edges$to)),
                                       directed = TRUE)
    igraph::V(g)$strength <- team_features$team_strength[
      match(igraph::V(g)$name, team_features$team)]

    set.seed(42)
    p_net <- ggraph::ggraph(g, layout = "fr") +
      ggraph::geom_edge_arc(aes(alpha = weight, width = weight),
                            arrow = grid::arrow(length = unit(3,"mm")),
                            colour = "#4a90d9", end_cap = ggraph::circle(4,"mm")) +
      ggraph::geom_node_point(aes(size = strength), colour = "#e84393", alpha = 0.85) +
      ggraph::geom_node_label(aes(label = name), size = 2.4, repel = TRUE,
                              label.padding = unit(0.12,"lines")) +
      ggraph::scale_edge_width(range = c(0.3, 1.8)) +
      ggraph::scale_edge_alpha(range = c(0.2, 0.9)) +
      ggplot2::labs(title = "Head-to-Head Network – WC 2026",
                    subtitle = "Edge opacity ∝ win rate; node size ∝ team strength") +
      ggraph::theme_graph()
    ggplot2::ggsave("wc2026_output/h2h_network.png", p_net, width=16, height=12, dpi=150)
    message("  Saved: h2h_network.png")
  } else {
    message("  Skipping H2H network: too few cross-WC matches (need ≥3 edges).")
  }
}

## World map ------------------------------------------------------------------
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

country_iso <- tibble::tibble(
  team   = canon(c(
    "France","Spain","England","Germany","Brazil","Argentina","Portugal",
    "Netherlands","Belgium","Morocco","Japan","South Korea","Colombia",
    "Switzerland","Uruguay","Croatia","Austria","United States","Mexico",
    "Canada","Denmark","Iran","Saudi Arabia","Egypt","Senegal","Ghana",
    "Algeria","Cabo Verde","South Africa","Tunisia","Côte d'Ivoire",
    "Norway","Sweden","Ecuador","Paraguay","Australia","Bosnia and Herzegovina",
    "Czechia","Panama","Türkiye","Jordan","Uzbekistan","Iraq","Qatar",
    "Congo DR","Curaçao","Scotland","Haiti","Serbia","New Zealand")),
  iso_a3 = c(
    "FRA","ESP","GBR","DEU","BRA","ARG","PRT","NLD","BEL","MAR","JPN",
    "KOR","COL","CHE","URY","HRV","AUT","USA","MEX","CAN","DNK","IRN",
    "SAU","EGY","SEN","GHA","DZA","CPV","ZAF","TUN","CIV","NOR","SWE",
    "ECU","PRY","AUS","BIH","CZE","PAN","TUR","JOR","UZB","IRQ","QAT",
    "COD","CUW","GBR","HTI","SRB","NZL")
)

map_sf <- world_sf |>
  dplyr::left_join(
    country_iso |>
      dplyr::left_join(team_features |> dplyr::select(team, team_strength), by="team"),
    by = "iso_a3"
  )

p_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world_sf, fill = "grey92", colour = "white", linewidth = 0.15) +
  ggplot2::geom_sf(data = map_sf |> dplyr::filter(!is.na(team_strength)),
                   aes(fill = team_strength), colour = "white", linewidth = 0.2) +
  ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "grey92",
                                 name = "Strength", limits = c(0, 1)) +
  ggplot2::coord_sf(crs = sf::st_crs("ESRI:54030")) +
  ggplot2::labs(title   = "2026 FIFA World Cup – Team Strength",
                subtitle= "FIFA ranking + form + qualifier results (2022-2026)",
                caption = "Sources: FBref, FIFA, worldfootballR") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom",
                 plot.title = ggplot2::element_text(face = "bold"))
ggplot2::ggsave("wc2026_output/team_strength_map.png", p_map,
                width=18, height=10, dpi=150)
message("  Saved: team_strength_map.png")


################################################################################
##  BLOCK 3A – Bradley-Terry model                                    [BUG-7 FIX]
################################################################################

## If we're running in fetch-only mode, stop here (after preprocessing)
if (exists("run_mode") && identical(run_mode, "fetch")) {
  summary_file <- "wc2026_output/data_fetch_summary.txt"
  s_lines <- c(
    paste0("run_mode: fetch"),
    paste0("timestamp: ", Sys.time()),
    paste0("all_results_rows: ", if (exists("all_results")) nrow(all_results) else 0),
    paste0("player_stats_rows: ", if (exists("player_stats_raw")) nrow(player_stats_raw) else 0),
    paste0("club_stats_sections: ", if (exists("club_stats_raw")) length(club_stats_raw) else 0),
    paste0("teams_in_WC_LIST: ", length(WC_TEAMS))
  )
  writeLines(s_lines, summary_file)
  message("\nFetch-only mode: preprocessing complete. Summary written to ", summary_file)
  quit(save = "no", status = 0)
}

message("\n── 3A  Bradley-Terry model ──")

USE_BT_DATA <- has_results && exists("results_long") && nrow(results_long) > 10

if (USE_BT_DATA) {

  bt_matches <- all_results |>
    dplyr::filter(home_team %in% WC_TEAMS, away_team %in% WC_TEAMS,
                  !is.na(home_goals), !is.na(away_goals))

  teams_bt <- unique(c(bt_matches$home_team, bt_matches$away_team))
  n_bt <- length(teams_bt)
  W <- matrix(0, n_bt, n_bt, dimnames = list(teams_bt, teams_bt))

  for (i in seq_len(nrow(bt_matches))) {
    r  <- bt_matches[i, ]
    wt <- r$match_weight
    hm <- r$home_team; aw <- r$away_team
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
    BradleyTerry2::BTm(outcome = W,
                       player1 = BradleyTerry2::row.player(W),
                       player2 = BradleyTerry2::col.player(W)),
    error = function(e) { warning("BT failed: ", e$message); NULL }
  )

  if (!is.null(bt_fit)) {
    bt_ab <- BradleyTerry2::BTabilities(bt_fit)
    bt_df <- tibble::tibble(
      team       = rownames(bt_ab),
      bt_ability = bt_ab[, "ability"],
      bt_se      = bt_ab[, "s.e."]
    )
    message("  BT model converged on ", nrow(bt_df), " teams.")
  } else {
    bt_df <- NULL
  }
} else {
  bt_df <- NULL
}

## Always build a complete bt_df covering ALL WC teams [BUG-7 FIX]
## Teams not in the BT data get an ability derived from their FIFA-based strength.
bt_df_full <- team_features |>
  dplyr::select(team, team_strength) |>
  dplyr::mutate(bt_ability_proxy = team_strength * 3 - 1.5,
                bt_se_proxy      = 0.35)

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

team_features <- team_features |>
  dplyr::left_join(bt_df_full, by = "team")

## Win probability from BT abilities
bt_win_prob <- function(team_a, team_b) {
  la <- bt_df_full$bt_ability[bt_df_full$team == team_a]
  lb <- bt_df_full$bt_ability[bt_df_full$team == team_b]
  if (length(la) == 0 || length(lb) == 0 || is.na(la) || is.na(lb)) return(0.5)
  plogis(la - lb)
}


################################################################################
##  BLOCK 3B – Dixon-Coles Poisson model
################################################################################

message("\n── 3B  Poisson model ──")

if (has_results && nrow(all_results) >= 30) {

  goals_df <- dplyr::bind_rows(
    all_results |> dplyr::transmute(
      scored=home_goals, attack=home_team, defence=away_team, home_adv=1L, weight=match_weight),
    all_results |> dplyr::transmute(
      scored=away_goals, attack=away_team, defence=home_team, home_adv=0L, weight=match_weight)
  ) |>
    dplyr::filter(!is.na(scored), attack %in% WC_TEAMS, defence %in% WC_TEAMS)

  dc_fit <- tryCatch(
    lme4::glmer(
      scored ~ home_adv + (1 | attack) + (1 | defence),
      data = goals_df, family = poisson(), weights = goals_df$weight
    ),
    error = function(e) { warning("glmer failed: ", e$message); NULL }
  )

  if (!is.null(dc_fit)) {
    re        <- lme4::ranef(dc_fit)
    attack_re <- re$attack  |> tibble::rownames_to_column("team") |>
      dplyr::rename(attack_re  = `(Intercept)`)
    defence_re <- re$defence |> tibble::rownames_to_column("team") |>
      dplyr::rename(defence_re = `(Intercept)`)

    team_features <- team_features |>
      dplyr::left_join(attack_re,  by="team") |>
      dplyr::left_join(defence_re, by="team") |>
      dplyr::mutate(
        attack_re  = tidyr::replace_na(attack_re,  0),
        defence_re = tidyr::replace_na(defence_re, 0)
      )

    mu0     <- lme4::fixef(dc_fit)["(Intercept)"]
    ha_coef <- lme4::fixef(dc_fit)["home_adv"]

    ## [BUG-6 FIX] safe fallback when a team is missing from team_features
    predict_goals <- function(ta, tb, neutral = TRUE) {
      a_off <- team_features$attack_re[team_features$team == ta]
      b_def <- team_features$defence_re[team_features$team == tb]
      b_off <- team_features$attack_re[team_features$team == tb]
      a_def <- team_features$defence_re[team_features$team == ta]
      ## default to 0 random effect if team not found
      if (length(a_off) == 0 || is.na(a_off)) a_off <- 0
      if (length(b_def) == 0 || is.na(b_def)) b_def <- 0
      if (length(b_off) == 0 || is.na(b_off)) b_off <- 0
      if (length(a_def) == 0 || is.na(a_def)) a_def <- 0
      ha <- if (neutral) 0 else ha_coef
      list(
        lambda_a = exp(mu0 + a_off - b_def + ha),
        lambda_b = exp(mu0 + b_off - a_def)
      )
    }

    score_win_prob <- function(ta, tb, max_goals = 10L) {
      L <- predict_goals(ta, tb)
      pm <- outer(dpois(0:max_goals, L$lambda_a),
                  dpois(0:max_goals, L$lambda_b))
      c(win_a = sum(pm[upper.tri(pm, diag=FALSE)]),
        draw  = sum(diag(pm)),
        win_b = sum(pm[lower.tri(pm, diag=FALSE)]))
    }
    message("  Poisson model fitted.")
  } else {
    predict_goals <- function(ta, tb, neutral=TRUE) list(lambda_a=1.3, lambda_b=1.1)
    score_win_prob <- function(ta, tb, ...) {
      p <- bt_win_prob(ta, tb)
      c(win_a = p * 0.75, draw = 0.25, win_b = (1-p) * 0.75)
    }
    message("  Poisson fallback (BT-derived).")
  }
} else {
  predict_goals <- function(ta, tb, neutral=TRUE) list(lambda_a=1.3, lambda_b=1.1)
  score_win_prob <- function(ta, tb, ...) {
    p <- bt_win_prob(ta, tb)
    c(win_a = p * 0.75, draw = 0.25, win_b = (1-p) * 0.75)
  }
  message("  Poisson skipped (<30 matches) – using BT fallback.")
}

## Ensemble (BT 50% + Poisson 50%)
ensemble_win_prob <- function(ta, tb, extra_time = FALSE) {
  bt_p <- bt_win_prob(ta, tb)
  dc   <- score_win_prob(ta, tb)
  dc_p <- unname(dc["win_a"]) / (unname(dc["win_a"]) + unname(dc["win_b"]))
  pa   <- 0.5 * bt_p + 0.5 * dc_p
  if (extra_time) pa <- 0.5 + (pa - 0.5) * 0.6   # regression toward 50/50
  c(p_a = as.numeric(pa), p_b = as.numeric(1 - pa))
}


################################################################################
##  BLOCK 4 – Monte Carlo tournament simulation           [BUG-5, BUG-6 FIXES]
################################################################################

message("\n── 4  Monte Carlo simulation (10 000 runs) ──")

## validate all group teams are in WC_TEAMS
all_group_teams <- unique(unlist(WC_GROUPS))
unknown <- setdiff(all_group_teams, WC_TEAMS)
if (length(unknown) > 0) warning("Unknown group teams: ", paste(unknown, collapse=", "))

simulate_group <- function(group_teams) {
  matchups  <- combn(group_teams, 2, simplify = FALSE)
  standings <- tibble::tibble(team = group_teams, pts = 0L, gd = 0L, gf = 0L)

  for (mu in matchups) {
    ta <- mu[[1]]; tb <- mu[[2]]
    dc <- score_win_prob(ta, tb)
    r  <- sample(c("a_win","draw","b_win"), 1,
                 prob = c(unname(dc["win_a"]),
                          unname(dc["draw"]),
                          unname(dc["win_b"])))

    ## Simulate scoreline
    L  <- tryCatch(predict_goals(ta, tb),
                   error = function(e) list(lambda_a=1.3, lambda_b=1.1))
    ga <- rpois(1, max(0.01, L$lambda_a))
    gb <- rpois(1, max(0.01, L$lambda_b))

    ## Force consistency between sampled result and score
    if (r == "a_win" && ga <= gb) ga <- gb + 1L
    if (r == "b_win" && gb <= ga) gb <- ga + 1L
    if (r == "draw"  && ga != gb) gb <- ga

    ## Update standings
    standings$gf[standings$team == ta] <- standings$gf[standings$team == ta] + ga
    standings$gf[standings$team == tb] <- standings$gf[standings$team == tb] + gb
    standings$gd[standings$team == ta] <- standings$gd[standings$team == ta] + (ga - gb)
    standings$gd[standings$team == tb] <- standings$gd[standings$team == tb] + (gb - ga)

    ## [BUG-5 FIX] clean points assignment
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
  grp <- lapply(WC_GROUPS, simulate_group)

  top2 <- unlist(lapply(grp, function(g) g$team[g$rank <= 2]))
  third <- lapply(grp, function(g) g[g$rank == 3, ]) |>
    dplyr::bind_rows() |>
    dplyr::arrange(dplyr::desc(pts), dplyr::desc(gd)) |>
    dplyr::slice_head(n = 8) |>
    dplyr::pull(team)

  bracket <- c(top2, third)   # 32 teams
  while (length(bracket) > 1) {
    bracket <- vapply(
      seq(1, length(bracket), by = 2),
      function(i) simulate_knockout(bracket[i], bracket[i+1]),
      FUN.VALUE = character(1)
    )
  }
  bracket
}

set.seed(2026)
n_sim   <- 10000L
winners <- character(n_sim)

progressr::with_progress({
  pg <- progressr::progressor(n_sim)
  for (i in seq_len(n_sim)) {
    winners[i] <- tryCatch(simulate_tournament(),
                           error = function(e) NA_character_)
    if (i %% 500 == 0) pg(amount = 500)
  }
})

win_probs <- table(winners[!is.na(winners)]) |>
  sort(decreasing = TRUE) |>
  as.data.frame() |>
  dplyr::rename(team = winners, n_wins = Freq) |>
  dplyr::mutate(
    win_pct   = n_wins / sum(n_wins),
    win_pct_l = qbeta(0.025, n_wins + 1, n_sim - n_wins + 1),
    win_pct_u = qbeta(0.975, n_wins + 1, n_sim - n_wins + 1)
  )

saveRDS(win_probs, "wc2026_output/win_probabilities.rds")
message("  Simulation complete. Top 5:")
print(head(win_probs, 5))


################################################################################
##  BLOCK 5 – Visualisations
################################################################################

message("\n── 5  Visualisations ──")

## 5A  Win-probability bar chart ----------------------------------------------
# Show top teams up to the full 48 after simulation
top_n_bar <- min(48L, nrow(win_probs))
top_bar   <- win_probs |> head(top_n_bar)

p_probs <- ggplot2::ggplot(top_bar,
  ggplot2::aes(x = reorder(team, win_pct), y = win_pct, fill = win_pct)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = win_pct_l, ymax = win_pct_u),
                         width = 0.25, colour = "grey30", linewidth = 0.6) +
  ggplot2::geom_text(ggplot2::aes(label = scales::percent(win_pct, accuracy = 0.1)),
                     hjust = -0.15, size = 3.2) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, max(top_bar$win_pct_u, na.rm=TRUE) * 1.22),
    expand = c(0, 0)) +
  ggplot2::scale_fill_viridis_c(option = "plasma", guide = "none") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = "2026 FIFA World Cup – Predicted Win Probabilities",
    subtitle = glue::glue("Monte Carlo: {scales::comma(n_sim)} simulations | 95% CI shown"),
    x = NULL, y = "Tournament Win Probability",
    caption  = "Models: Bradley-Terry + Dixon-Coles (50/50 ensemble)\nSources: FBref · FIFA ranking · worldfootballR"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title          = ggplot2::element_text(face = "bold", size = 17),
    plot.subtitle       = ggplot2::element_text(colour = "grey40"),
    panel.grid.major.y  = ggplot2::element_blank()
  )

ggplot2::ggsave("wc2026_output/win_probabilities.png", p_probs,
                width = 18, height = 12, dpi = 180)
message("  Saved: win_probabilities.png")


## 5B  Radar chart (top 8)  [BUG-3 FIX] -------------------------------------
## Ensure all required columns exist (impute 0 if absent)
radar_required_cols <- c("attack_str","defence_str","form_last5_sc",
                         "q_pts_sc","fifa_strength")
for (col in radar_required_cols) {
  if (!col %in% names(team_features))
    team_features[[col]] <- 0
}

radar_teams <- head(win_probs$team, 8)
## Guard: only keep teams that appear in team_features
radar_teams <- intersect(radar_teams, team_features$team)

if (length(radar_teams) >= 2) {
  rf <- team_features |>
    dplyr::filter(team %in% radar_teams) |>
    dplyr::select(team, dplyr::all_of(radar_required_cols)) |>
    dplyr::mutate(dplyr::across(-team, function(x) {
      r <- range(x, na.rm = TRUE)
      if (r[1] == r[2]) return(rep(0.5, length(x)))   # avoid rescale error
      scales::rescale(x, to = c(0, 1))
    }))

  mat_data <- as.data.frame(rf[, -1])
  rownames(mat_data) <- rf$team
  max_row  <- setNames(rep(1, ncol(mat_data)), names(mat_data))
  min_row  <- setNames(rep(0, ncol(mat_data)), names(mat_data))
  radar_df <- rbind(max_row, min_row, mat_data)

  png("wc2026_output/strength_radar.png", width = 1400, height = 1000, res = 130)
  n_r <- min(2, ceiling(length(radar_teams) / 4))
  n_c <- ceiling(length(radar_teams) / n_r)
  par(mfrow = c(n_r, n_c), mar = c(0.5, 0.5, 2, 0.5))
  colours <- RColorBrewer::brewer.pal(max(3, length(radar_teams)), "Set1")
  axis_labels <- c("Attack","Defence","Form","Qual Pts","FIFA")
  for (i in seq_len(length(radar_teams))) {
    fmsb::radarchart(
      radar_df[c(1, 2, i + 2), ],
      axistype = 1, seg = 4,
      pcol  = colours[i],
      pfcol = adjustcolor(colours[i], 0.25),
      plwd  = 2, cglcol = "grey70", cglty = 1,
      axislabcol = "grey50", caxislabels = seq(0, 1, 0.25),
      vlabels = axis_labels, vlcex = 0.8,
      title = rownames(mat_data)[i]
    )
  }
  dev.off()
  message("  Saved: strength_radar.png")
} else {
  message("  Radar skipped: fewer than 2 teams with win probabilities.")
}


## 5C  H2H probability heat-map -----------------------------------------------
# Heatmap for the top teams (up to 48)
top_n_heat <- min(48L, nrow(win_probs))
topN_teams <- head(win_probs$team, top_n_heat)
heat_mat <- expand.grid(team_a = topN_teams, team_b = topN_teams,
                        stringsAsFactors = FALSE) |>
  dplyr::mutate(p_a = mapply(function(a, b) {
    if (a == b) NA_real_ else ensemble_win_prob(a, b)["p_a"]
  }, team_a, team_b))

p_heat <- ggplot2::ggplot(heat_mat,
  ggplot2::aes(x = team_b, y = team_a, fill = p_a)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
  ggplot2::geom_text(
    ggplot2::aes(label = ifelse(is.na(p_a), "",
                                scales::percent(p_a, accuracy = 1))),
    size = 2.4, colour = "white", fontface = "bold") +
  ggplot2::scale_fill_gradient2(low="#1B2A4A", mid="grey92", high="#E63946",
                                 midpoint=0.5, na.value="white",
                                 name="Win Prob\n(row team)") +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::labs(title    = "Head-to-Head Win Probability Matrix",
                subtitle = "Row team's probability of beating column team",
                x = "Opponent", y = "Team") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x     = ggplot2::element_text(angle=45, hjust=0, size=8),
    axis.text.y     = ggplot2::element_text(size=8),
    panel.grid      = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face="bold")
  )
ggplot2::ggsave("wc2026_output/h2h_heatmap.png", p_heat,
                width = max(14, ceiling(top_n_heat / 2)), height = max(13, ceiling(top_n_heat / 2)), dpi = 160)
message("  Saved: h2h_heatmap.png")


## 5D  Player stats facet (if available) -------------------------------------
if (PlayerStats && exists("player_agg") && nrow(player_agg) > 0) {
  top10 <- head(win_probs$team, 10)
  pa_long <- player_agg |>
    dplyr::filter(national_team %in% top10) |>
    dplyr::select(national_team, squad_goals_p90, squad_xg_p90, squad_assists_p90) |>
    tidyr::pivot_longer(-national_team, names_to="metric", values_to="value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(metric = stringr::str_replace_all(metric,"_"," ") |>
                    stringr::str_to_title())

  if (nrow(pa_long) > 0) {
    p_pl <- ggplot2::ggplot(pa_long,
      ggplot2::aes(x=reorder(national_team, value), y=value, fill=national_team)) +
      ggplot2::geom_col(width=0.6) +
      ggplot2::facet_wrap(~metric, scales="free_x") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_viridis_d(guide="none") +
      ggplot2::labs(title="Player Stats – Top 10 Teams",
                    subtitle="Squad per-90 averages (FBref)", x=NULL, y="Per 90") +
      ggplot2::theme_minimal(base_size=12) +
      ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))
    ggplot2::ggsave("wc2026_output/player_stats_top10.png", p_pl,
                    width=14, height=7, dpi=160)
    message("  Saved: player_stats_top10.png")
  }
}


################################################################################
##  BLOCK 6 – Final output table                                   [BUG-2 FIX]
################################################################################

message("\n── 6  Final output ──")

## Use any_of() so missing columns don't crash dplyr::select()
optional_cols <- c("attack_re","defence_re","q_pts_per","q_pts_sc",
                   "form_last5","attack_str","defence_str","fifa_strength")

final_table <- win_probs |>
  dplyr::left_join(
    team_features |>
      dplyr::select(team,
                    dplyr::any_of(c("fifa_rank","team_strength","bt_ability")),
                    dplyr::any_of(optional_cols)),
    by = "team"
  ) |>
  dplyr::arrange(dplyr::desc(win_pct)) |>
  dplyr::mutate(
    pred_rank   = dplyr::row_number(),
    win_pct_fmt = scales::percent(win_pct, accuracy = 0.1),
    ci_95       = glue::glue(
      "[{scales::percent(win_pct_l, accuracy=0.1)}, {scales::percent(win_pct_u, accuracy=0.1)}]")
  )

## Select only columns that actually exist
keep_cols <- intersect(
  c("pred_rank","team","win_pct_fmt","ci_95","n_wins",
    "fifa_rank","team_strength","bt_ability",
    optional_cols),
  names(final_table)
)
final_table <- dplyr::select(final_table, dplyr::all_of(keep_cols))

readr::write_csv(final_table, "wc2026_output/final_predictions.csv")

message("\n══ FINAL WC 2026 PREDICTIONS (top 16) ══")
print(head(final_table, 16))
message("\n✓  All outputs written to: wc2026_output/")
message("   Files: win_probabilities.png, h2h_heatmap.png,")
message("          team_strength_map.png, strength_radar.png,")
message("          final_predictions.csv, data_diagnostics.txt")


################################################################################
##  SUGGESTIONS & EXTENSIONS
##─────────────────────────────────────────────────────────────────────────────
##  1.  BAYESIAN UPGRADE (Block 3C)
##      Replace the BT / Poisson combo with a full Stan model via brms:
##        brm(goals | weights(w) ~ (1|attack) + (1|defence) + home_adv,
##            data = goals_df, family = poisson(), chains = 4, iter = 2000)
##      Gives proper posterior distributions for each team's attack/defence.
##
##  2.  PLAYER QUALITY FEATURE
##      Add average squad market value (Transfermarkt) as a predictor.
##      Weighted by whether the player actually appeared in qualifiers.
##
##  3.  INJURY / SUSPENSION UPDATES
##      Before the tournament starts, adjust team_strength by removing key
##      players who are injured (e.g. deduct 0.05-0.15 from team_strength
##      per top-tier missing player, weighted by their FBref scouting score).
##
##  4.  LIVE UPDATING
##      Once the tournament starts, update bt_ability after each group match
##      using a sequential Bayesian update and re-run Block 4.
##
##  5.  INTERACTIVE SHINY APP
##      Wrap Block 4 in a Shiny app where users can:
##        (a) select tournament parameters,
##        (b) "lock in" results as games finish,
##        (c) see real-time win probability updates.
##
##  6.  EXPECTED GOALS (xG) LAYER
##      Pull Understat xG per team (for leagues with coverage) and use xG
##      instead of actual goals in the Poisson model to reduce noise.
##
##  7.  ODDS CALIBRATION
##      Compare model output to bookmaker odds (free at football-data.co.uk).
##      Fit a Platt scaling calibration to ensure probabilities are well-calibrated.
##
##  8.  CONFEDERATION STRENGTH ADJUSTMENT
##      Add a confederation-level random effect: UEFA / CONMEBOL teams have
##      harder qualifier schedules than OFC / some AFC members.
################################################################################