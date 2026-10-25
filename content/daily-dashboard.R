# Run from project root during rendering of dailylog.qmd.
# Requires: install.packages("yaml")

lab_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
lab_number <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)

lab_post_metadata <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 3 || trimws(lines[1]) != "---") return(NULL)
  closing <- which(trimws(lines) == "---")
  if (length(closing) < 2 || closing[2] <= 2) return(NULL)
  meta <- tryCatch(yaml::yaml.load(paste(lines[2:(closing[2]-1)], collapse="\n")),
                   error = function(e) {warning(sprintf("Bad YAML in %s: %s", file, e$message)); NULL})
  if (is.null(meta) || is.null(meta$date)) return(NULL)
  date <- tryCatch(as.Date(as.character(meta$date)), error=function(e) as.Date(NA))
  if (is.na(date)) return(NULL)
  # Only writing_words entries join the writing history; older posts without count are skipped.
  words <- if (is.null(meta$writing_words)) NA_real_ else suppressWarnings(as.numeric(meta$writing_words))
  if (!is.na(words) && (words < 0 || words != floor(words))) stop("Invalid writing_words in ", file)
  progress <- meta$goal_progress
  if (is.null(progress)) progress <- list()
  if (!is.list(progress) || is.null(names(progress))) progress <- list()
  list(date=date, words=words, progress=progress, path=file)
}

render_daily_dashboard <- function(writing_csv="content/writing-log.csv",
                                   goals_csv="content/goals-log.csv",
                                   posts_dir="daily_log", as_of=Sys.Date(), daily_target=500L) {
  if (!requireNamespace("yaml", quietly=TRUE)) stop("Install the yaml package: install.packages('yaml')")
  if (!file.exists(writing_csv) || !file.exists(goals_csv)) stop("Missing a historical CSV in content/")
  historical <- read.csv(writing_csv, stringsAsFactors=FALSE, colClasses="character")
  if (!all(c("date","words") %in% names(historical))) stop("writing-log.csv needs date,words columns")
  historical <- historical[nzchar(trimws(historical$date)),,drop=FALSE]
  if (nrow(historical)) {
    historical$date <- as.Date(historical$date)
    historical$words <- suppressWarnings(as.numeric(historical$words))
    if (anyNA(historical$date) || anyNA(historical$words) || any(historical$words < 0) || any(historical$words != floor(historical$words)))
      stop("Invalid date or nonnegative integer word count in writing-log.csv")
    if (anyDuplicated(historical$date)) stop("Keep exactly one imported writing record per date")
  }
  paths <- list.files(posts_dir, pattern="\\.(qmd|Rmd|rmd)$", full.names=TRUE, recursive=TRUE)
  posts <- Filter(Negate(is.null), lapply(paths, lab_post_metadata))
  new_counts <- Filter(function(x) !is.na(x$words), posts)
  # If date occurs in both sources, the newer daily-post YAML is authoritative.
  if (length(new_counts)) {
    counts <- data.frame(date=as.Date(vapply(new_counts, function(p) as.character(p$date), character(1))),
                         words=vapply(new_counts, `[[`, numeric(1), "words"))
    if (anyDuplicated(counts$date)) stop("More than one daily post has writing_words for the same date")
    historical <- historical[!historical$date %in% counts$date,,drop=FALSE]
    records <- rbind(data.frame(date=historical$date, words=historical$words), counts)
  } else {
    records <- data.frame(date=as.Date(historical$date), words=as.numeric(historical$words))
  }
  records <- records[!is.na(records$date) & records$date <= as_of,,drop=FALSE]
  year_start <- as.Date(format(as_of,"%Y-01-01"))
  month_start <- as.Date(format(as_of,"%Y-%m-01"))
  sum_period <- function(start,end) sum(records$words[records$date >= start & records$date <= end],na.rm=TRUE)
  today_words <- sum_period(as_of,as_of)
  month_words <- sum_period(month_start,as_of)
  year_words <- sum_period(year_start,as_of)
  elapsed <- as.integer(as_of-year_start)+1L
  expected <- daily_target*elapsed
  days_met <- sum(records$words >= daily_target & records$date >= year_start)
  logged_days <- sum(records$date >= year_start)
  pct <- min(100,100*today_words/daily_target)
  today_status <- if (today_words >= daily_target) "Daily goal met" else if (today_words > 0) "Daily goal in progress" else "No writing logged today"

  goals <- read.csv(goals_csv, stringsAsFactors=FALSE, check.names=FALSE)
  required <- c("month","goal_id","goal","target","completed")
  if (!all(required %in% names(goals))) stop("goals-log.csv needs: ",paste(required, collapse=", "))
  monthly <- goals[goals$month == format(as_of,"%Y-%m"),,drop=FALSE]
  if (nrow(monthly) > 0 && (anyDuplicated(monthly$goal_id) || anyNA(monthly$target) || any(monthly$target <= 0) || anyNA(monthly$completed)))
    stop("Monthly goal IDs must be unique; targets must be positive; completed must be numeric")
  goal_cards <- character()
  for (i in seq_len(nrow(monthly))) {
    g <- monthly[i,,drop=FALSE]
    added <- 0
    for (p in posts) {
      if (p$date <= as_of && format(p$date,"%Y-%m") == g$month && !is.null(p$progress[[g$goal_id]])) {
        n <- suppressWarnings(as.numeric(p$progress[[g$goal_id]]))
        if (length(n)!=1 || is.na(n) || n<0) stop("Invalid goal_progress in ",p$path)
        added <- added + n
      }
    }
    done <- as.numeric(g$completed)+added
    completion <- min(100,100*done/as.numeric(g$target))
    unit <- if ("unit" %in% names(g)) as.character(g$unit) else "units"
    goal_cards <- c(goal_cards,sprintf(
      '<div class="lab-goal"><div class="lab-goal-name">%s</div><div class="lab-track"><div class="lab-fill" style="width:%.1f%%"></div></div><div class="lab-goal-footer"><span>%s / %s %s</span><span>%.0f%%</span></div></div>',
      lab_escape(g$goal), completion, lab_number(done), lab_number(g$target), lab_escape(unit), completion))
  }
  if (!length(goal_cards)) goal_cards <- '<p>No goals recorded for this month in content/goals-log.csv.</p>'
  card <- paste0(
    '<div class="lab-dashboard">',
    '<section class="lab-card"><h3>', toupper(format(as_of,"%B")), ' GOALS</h3>',
    paste(goal_cards,collapse="\n"), '</section>',
    '<section class="lab-card"><h3>WRITING PROGRESS</h3>',
    '<div class="lab-stat"><span>Today</span><strong>',lab_number(today_words),' / ',lab_number(daily_target),' words</strong></div>',
    '<div class="lab-track" style="margin-top:.65rem"><div class="lab-fill lab-fill-writing" style="width:',sprintf("%.1f",pct),'%"></div></div>',
    '<div class="lab-status">',today_status,'</div>',
    '<div class="lab-stat"><span>Monthly total to date</span><strong>',lab_number(month_words),' words</strong></div>',
    '<div class="lab-stat"><span>Annual total to date</span><strong>',lab_number(year_words),' words</strong></div>',
    '<div class="lab-stat"><span>Annual target to date</span><strong>',lab_number(expected),' words</strong></div>',
    '<div class="lab-small">Daily commitment met on ',days_met,' of ',logged_days,' logged days this year. Annual target uses ', daily_target,' × elapsed calendar days.</div>',
    '</section></div>'
  )
  cat(card,"\n")
  invisible(list(today=today_words, month=month_words, year=year_words, target=expected, met_days=days_met))
}
