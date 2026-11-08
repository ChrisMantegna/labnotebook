# Daily dashboard — run from the Quarto project root.
# Requires install.packages("yaml").
# Historical daily writing counts: content/writing-log.csv
# Monthly goal baselines: content/goals-log.csv
# Current month's daily post YAML may override CSV counts for the same day.

lab_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
lab_number <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)

# Accept ISO dates, m/d/yy, and m/d/yyyy, without ambiguous automatic guessing.
lab_parse_dates <- function(x) {
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  us2 <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$", x)
  us4 <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
  if (any(iso)) out[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  if (any(us2)) out[us2] <- as.Date(x[us2], format = "%m/%d/%y")
  if (any(us4)) out[us4] <- as.Date(x[us4], format = "%m/%d/%Y")
  out
}

lab_post_metadata <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 3L || trimws(lines[1]) != "---") return(NULL)
  closing <- which(trimws(lines) == "---")
  if (length(closing) < 2L || closing[2] <= 2L) return(NULL)
  meta <- tryCatch(
    yaml::yaml.load(paste(lines[2:(closing[2] - 1L)], collapse = "\n")),
    error = function(e) {
      warning(sprintf("Bad YAML in %s: %s", file, e$message))
      NULL
    }
  )
  if (is.null(meta) || is.null(meta$date)) return(NULL)
  date <- lab_parse_dates(as.character(meta$date)[1L])
  if (is.na(date)) {
    warning("Skipping post with invalid YAML date: ", file)
    return(NULL)
  }
  words <- if (is.null(meta$writing_words)) NA_real_ else
    suppressWarnings(as.numeric(meta$writing_words))
  if (length(words) != 1L || (!is.na(words) &&
      (!is.finite(words) || words < 0 || words != floor(words)))) {
    stop("Invalid writing_words in ", file)
  }
  progress <- meta$goal_progress
  if (is.null(progress) || !is.list(progress) || is.null(names(progress))) {
    progress <- list()
  }
  list(date = date, words = words, progress = progress, path = file)
}

render_daily_dashboard <- function(writing_csv = "content/writing-log.csv",
                                   goals_csv = "content/goals-log.csv",
                                   posts_dir = "daily_log",
                                   as_of = Sys.Date(), daily_target = 500L) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Install the yaml package: install.packages('yaml')")
  }
  as_of <- lab_parse_dates(as.character(as_of)[1L])
  if (is.na(as_of)) stop("as_of must be a valid date")
  if (!is.numeric(daily_target) || length(daily_target) != 1L ||
      is.na(daily_target) || daily_target <= 0) stop("daily_target must be positive")
  if (!file.exists(writing_csv)) stop("Missing writing CSV: ", writing_csv)
  if (!file.exists(goals_csv)) stop("Missing goals CSV: ", goals_csv)
  if (!dir.exists(posts_dir)) stop("Missing posts directory: ", posts_dir)

  historical <- read.csv(writing_csv, stringsAsFactors = FALSE,
                         colClasses = "character", check.names = FALSE)
  if (!all(c("date", "words") %in% names(historical))) {
    stop("writing-log.csv must have date,words columns")
  }
  historical <- historical[nzchar(trimws(historical$date)), , drop = FALSE]
  if (nrow(historical)) {
    historical$date <- lab_parse_dates(historical$date)
    historical$words <- suppressWarnings(as.numeric(trimws(historical$words)))
    bad <- which(is.na(historical$date) | is.na(historical$words) |
                 !is.finite(historical$words) | historical$words < 0 |
                 historical$words != floor(historical$words))
    if (length(bad)) stop("Invalid writing-log.csv record(s) at data row(s): ",
                          paste(bad, collapse = ", "))
    if (anyDuplicated(historical$date)) {
      stop("writing-log.csv must contain at most one record per date")
    }
  }
  historical <- historical[, c("date", "words"), drop = FALSE]

  # Only daily YYYY_MM_DD folders count. Monthly archive folders such as
  # daily_log/2026_10/ are deliberately excluded, even if their files have YAML.
  paths <- list.files(posts_dir, pattern = "\\.(qmd|Rmd|rmd|md)$",
                      full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  normalized <- gsub("\\\\", "/", paths)
  daily_path <- grepl("(^|/)[0-9]{4}[_-][0-9]{2}[_-][0-9]{2}(/|$)", normalized)
  daily_paths <- paths[daily_path]
  posts <- Filter(Negate(is.null), lapply(daily_paths, lab_post_metadata))

  # Use YAML updates from the ACTIVE month only. Finalized history stays in CSV.
  month_start <- as.Date(format(as_of, "%Y-%m-01"))
  active_posts <- Filter(function(p) p$date >= month_start && p$date <= as_of, posts)
  new_counts <- Filter(function(p) !is.na(p$words), active_posts)
  if (length(new_counts)) {
    counts <- data.frame(
      date = as.Date(vapply(new_counts, function(p) as.character(p$date), character(1))),
      words = vapply(new_counts, function(p) p$words, numeric(1))
    )
    if (anyDuplicated(counts$date)) {
      stop("More than one active daily post has writing_words for the same date")
    }
    historical <- historical[!historical$date %in% counts$date, , drop = FALSE]
    records <- rbind(historical, counts)
  } else {
    records <- historical
  }
  records <- records[records$date <= as_of, , drop = FALSE]
  year_start <- as.Date(format(as_of, "%Y-01-01"))
  sum_period <- function(start, end) {
    sum(records$words[records$date >= start & records$date <= end], na.rm = TRUE)
  }
  today_words <- sum_period(as_of, as_of)
  month_words <- sum_period(month_start, as_of)
  year_words <- sum_period(year_start, as_of)
  expected <- daily_target * (as.integer(as_of - year_start) + 1L)
  year_rows <- records$date >= year_start
  days_met <- sum(year_rows & records$words >= daily_target)
  logged_days <- sum(year_rows)
  pct <- min(100, 100 * today_words / daily_target)
  today_status <- if (today_words >= daily_target) "Daily goal met" else if
    (today_words > 0) "Daily goal in progress" else "No writing logged today"

  goals <- read.csv(goals_csv, stringsAsFactors = FALSE, check.names = FALSE,
                    colClasses = "character")
  required <- c("month", "goal_id", "goal", "target", "completed")
  if (!all(required %in% names(goals))) {
    stop("goals-log.csv needs: ", paste(required, collapse = ", "))
  }
  monthly <- goals[!is.na(goals$month) &
                    trimws(goals$month) == format(as_of, "%Y-%m"), , drop = FALSE]
  if (nrow(monthly)) {
    monthly$target <- suppressWarnings(as.numeric(monthly$target))
    monthly$completed <- suppressWarnings(as.numeric(monthly$completed))
    if (anyDuplicated(monthly$goal_id) || anyNA(monthly$target) ||
        any(monthly$target <= 0) || anyNA(monthly$completed) ||
        any(monthly$completed < 0)) {
      stop("Current monthly goals: IDs must be unique, targets positive, completed nonnegative")
    }
  }
  goal_cards <- character()
  for (i in seq_len(nrow(monthly))) {
    g <- monthly[i, , drop = FALSE]
    added <- 0
    for (p in active_posts) {
      v <- p$progress[[as.character(g$goal_id)]]
      if (!is.null(v)) {
        n <- suppressWarnings(as.numeric(v))
        if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 0) {
          stop("Invalid goal_progress in ", p$path)
        }
        added <- added + n
      }
    }
    # 'completed' is the baseline, and YAML entries are additions (not snapshots).
    done <- g$completed + added
    completion <- min(100, 100 * done / g$target)
    unit <- if ("unit" %in% names(g) && !is.na(g$unit) && nzchar(g$unit))
      as.character(g$unit) else "units"
    goal_cards <- c(goal_cards, sprintf(
      '<div class="lab-goal"><div class="lab-goal-name">%s</div><div class="lab-track"><div class="lab-fill" style="width:%.1f%%"></div></div><div class="lab-goal-footer"><span>%s / %s %s</span><span>%.0f%%</span></div></div>',
      lab_escape(g$goal), completion, lab_number(done), lab_number(g$target),
      lab_escape(unit), completion))
  }
  if (!length(goal_cards)) {
    goal_cards <- '<p>No goals recorded for this month in content/goals-log.csv.</p>'
  }
  card <- paste0(
    '<div class="lab-dashboard">',
    '<section class="lab-card"><h3>', toupper(format(as_of, "%B")), ' GOALS</h3>',
    paste(goal_cards, collapse = "\n"), '</section>',
    '<section class="lab-card"><h3>WRITING PROGRESS</h3>',
    '<div class="lab-stat"><span>Today</span><strong>', lab_number(today_words),
    ' / ', lab_number(daily_target), ' words</strong></div>',
    '<div class="lab-track" style="margin-top:.65rem"><div class="lab-fill lab-fill-writing" style="width:',
    sprintf("%.1f", pct), '%"></div></div>',
    '<div class="lab-status">', today_status, '</div>',
    '<div class="lab-stat"><span>Monthly total to date</span><strong>',
    lab_number(month_words), ' words</strong></div>',
    '<div class="lab-stat"><span>Annual total to date</span><strong>',
    lab_number(year_words), ' words</strong></div>',
    '<div class="lab-stat"><span>Annual target to date</span><strong>',
    lab_number(expected), ' words</strong></div>',
    '<div class="lab-small">Daily commitment met on ', days_met, ' of ',
    logged_days, ' logged days this year. Annual target uses ', daily_target,
    ' × elapsed calendar days.</div>',
    '</section></div>'
  )
  cat(card, "\n")
  invisible(list(today = today_words, month = month_words, year = year_words,
                 target = expected, met_days = days_met, logged_days = logged_days))
}
