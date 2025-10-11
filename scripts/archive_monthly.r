#!/usr/bin/env Rscript

# Usage:
#   Rscript scripts/archive_month.R         # archives last month by default
#   Rscript scripts/archive_month.R 2025-09 # or pass YYYY-MM explicitly

suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)

# --- pick target month (YYYY-MM) ---
target_month <- if (length(args) >= 1) {
  args[1]
} else {
  # default last month from today
  as.character(format(seq(as.Date(Sys.Date()), length = 2, by = "-1 month")[2], "%Y-%m"))
}

posts_dir   <- "daily_log"
archive_dir <- file.path(posts_dir, "archive")
dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)

# --- discover daily posts: folder pattern YYYY_MMDD with index.qmd inside ---
# e.g., daily_log/2025_0928/index.qmd
daily_dirs <- list.files(posts_dir, pattern = "^\\d{4}_\\d{4}$", full.names = TRUE)
daily_index <- file.path(daily_dirs, "index.qmd")
daily_index <- daily_index[file.exists(daily_index)]

# keep only those in target_month (YYYY-MM). From "YYYY_MMDD" → year=1:4, mo=6:7, day=8:9
dir_names <- basename(dirname(daily_index))
get_month_key <- function(dn) sprintf("%s-%s", substr(dn, 1, 4), substr(dn, 6, 7))
keep <- vapply(dir_names, function(dn) get_month_key(dn) == target_month, logical(1))
daily_index <- daily_index[keep]
dir_names   <- dir_names[keep]

if (length(daily_index) == 0) {
  message(sprintf("No daily posts found for %s. Nothing to do.", target_month))
  quit(save = "no", status = 0)
}

# --- helper: read qmd splitting YAML/front matter & body ---
read_qmd <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) >= 3 && lines[1] == "---") {
    end <- which(lines[-1] == "---")[1] + 1
    if (is.na(end)) end <- 0
  } else end <- 0
  
  if (end > 0) {
    fm_text <- paste(lines[2:(end - 1)], collapse = "\n")
    meta <- tryCatch(yaml::yaml.load(fm_text), error = function(e) list())
    body <- paste(lines[(end + 1):length(lines)], collapse = "\n")
  } else {
    meta <- list()
    body <- paste(lines, collapse = "\n")
  }
  list(meta = meta, body = body)
}

# --- build entries and sort by actual date ---
dates <- as.Date(sprintf("%s-%s-%s",
                         substr(dir_names,1,4),
                         substr(dir_names,6,7),
                         substr(dir_names,8,9)))

ord <- order(dates)
daily_index <- daily_index[ord]
dir_names   <- dir_names[ord]

entries <- lapply(seq_along(daily_index), function(i) {
  p  <- daily_index[[i]]
  dn <- dir_names[[i]]            # e.g., 2025_0928
  d  <- as.character(dates[[i]])  # 2025-09-28
  
  x <- read_qmd(p)
  title <- x$meta$title %||% dn
  # link points to where the daily will live after moving:
  rel_link <- file.path("archive", target_month, dn, "index.qmd")
  
  header <- sprintf("## %s — %s  \n[View original daily](%s)\n", d, title, rel_link)
  paste0(header, "\n", x$body, "\n")
})

# --- write monthly file: daily_log/YYYY-MM.qmd ---
monthly_title <- sprintf("%s — Daily Posts", target_month)
monthly_path  <- file.path(posts_dir, paste0(target_month, ".qmd"))

monthly_yaml <- paste0(
  "---\n",
  "title: \"", monthly_title, "\"\n",
  "toc: true\n",
  "page-layout: article\n",
  "format:\n",
  "  html:\n",
  "    df-print: paged\n",
  "---\n\n",
  "# ", monthly_title, "\n\n",
  "> This page compiles all daily posts for ", target_month, ".\n\n"
)

writeLines(c(monthly_yaml, unlist(entries)), con = monthly_path)

message(sprintf("Wrote monthly file: %s", monthly_path))
message(sprintf("Next: move daily folders for %s into %s/%s/ so they stop rendering.",
                target_month, archive_dir, target_month))

