#!/usr/bin/env Rscript

# this script compiles the posts from the previous month into one doc to preserve space and readability on the main page
# usage: Rscript archive_monthly_updated.r [YYYY-MM]

suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)

# pick target month (YYYY-MM)
target_month <- if (length(args) >= 1) {
  args[1]
} else {
  # default last month from today
  as.character(format(seq(as.Date(Sys.Date()), length = 2, by = "-1 month")[2], "%Y-%m"))
}

if (!grepl("^\\d{4}-\\d{2}$", target_month)) {
  stop("target_month must be in YYYY-MM format.")
}

posts_dir   <- "daily_log"
archive_dir <- file.path(posts_dir, "archive")

# create monthly archive folder using YYYY_MM
month_folder_name <- gsub("-", "_", target_month)
monthly_folder    <- file.path(archive_dir, month_folder_name)
dir.create(monthly_folder, recursive = TRUE, showWarnings = FALSE)

# find daily posts with folder pattern YYYY_MM_DD that contain an index.qmd inside
daily_dirs <- list.files(
  posts_dir,
  pattern = "^\\d{4}_\\d{2}_\\d{2}$",
  full.names = TRUE
)

daily_index <- file.path(daily_dirs, "index.qmd")
daily_index <- daily_index[file.exists(daily_index)]

# keep only those in target_month
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
    body_lines <- if (end < length(lines)) lines[(end + 1):length(lines)] else character(0)
  } else {
    meta <- list()
    body_lines <- lines
  }

  list(meta = meta, body_lines = body_lines)
}

# helper: copy each daily img folder into a month-scoped assets directory inside the monthly archive folder
copy_daily_img_assets <- function(posts_dir, target_month, dir_names, monthly_folder) {
  assets_root <- file.path(monthly_folder, "monthly_img", gsub("-", "_", target_month))

  for (dn in dir_names) {
    src_img <- file.path(posts_dir, dn, "img")
    if (!dir.exists(src_img)) next

    dest_img <- file.path(assets_root, dn, "img")
    dir.create(dest_img, recursive = TRUE, showWarnings = FALSE)

    src_files <- list.files(src_img, full.names = TRUE)
    if (length(src_files) == 0) next

    file.copy(src_files, dest_img, recursive = TRUE)
  }

  invisible(assets_root)
}

# helper: strip an initial top-level title/header from each daily post body
strip_first_header <- function(lines) {
  idx <- which(trimws(lines) != "")
  if (length(idx) == 0) return(lines)

  first <- idx[1]
  if (grepl("^#\\s+", lines[first])) {
    lines <- lines[-seq_len(first)]
    # remove one extra blank line after header block if present
    while (length(lines) > 0 && trimws(lines[1]) == "") {
      lines <- lines[-1]
    }
  }

  lines
}

# helper: demote heading sizes slightly to make the page less visually loud
soften_headings <- function(body) {
  body <- gsub("(?m)^##\\s+", "### ", body, perl = TRUE)
  body <- gsub("(?m)^###\\s+", "#### ", body, perl = TRUE)
  body <- gsub("(?m)^####\\s+", "##### ", body, perl = TRUE)
  body
}

# helper: rewrite img paths inside the daily body to point at copied assets
rewrite_img_paths <- function(body, target_month, dn) {
  new_prefix <- file.path("monthly_img", gsub("-", "_", target_month), dn, "img")

  # Markdown links/images like (img/foo.png) or ![](img/foo.png)
  body2 <- gsub(
    pattern = "(?<=\\()img/",
    replacement = paste0(new_prefix, "/"),
    body,
    perl = TRUE
  )

  # Quoted paths 'img/...' or "img/..."
  body2 <- gsub(
    pattern = "(?<=\\')img/|(?<=\\\")img/",
    replacement = paste0(new_prefix, "/"),
    body2,
    perl = TRUE
  )

  # HTML img tags: src="img/..." or src='img/...'
  body2 <- gsub(
    pattern = "src=[\"']img/",
    replacement = paste0("src=\"", new_prefix, "/"),
    body2
  )

  body2
}

# build entries and sort by actual date
dates <- as.Date(sprintf(
  "%s-%s-%s",
  substr(dir_names, 1, 4),
  substr(dir_names, 6, 7),
  substr(dir_names, 9, 10)
))

ord <- order(dates)
daily_index <- daily_index[ord]
dir_names   <- dir_names[ord]
dates       <- dates[ord]

# copy any daily img assets if present
assets_root <- copy_daily_img_assets(posts_dir, target_month, dir_names, monthly_folder)

entries <- lapply(seq_along(daily_index), function(i) {
  p  <- daily_index[[i]]
  dn <- dir_names[[i]]            # e.g., 2025_03_28
  d  <- as.character(dates[[i]])  # e.g., 2025-03-28

  x <- read_qmd(p)
  title <- x$meta$title %||% dn

  body_lines <- strip_first_header(x$body_lines)
  body <- paste(body_lines, collapse = "\n")
  body <- rewrite_img_paths(body, target_month, dn)
  body <- soften_headings(body)

  # lighter per-entry header
  header <- sprintf("### %s — %s\n", d, title)
  paste0(header, "\n", body, "\n")
})

# monthly output lives inside archive/YYYY_MM/index.qmd
monthly_date  <- paste0(target_month, "-01")
monthly_title <- format(as.Date(monthly_date), "%Y %B Daily Posts")
monthly_path  <- file.path(monthly_folder, "index.qmd")
callout_month <- format(as.Date(monthly_date), "%B %Y")

monthly_yaml <- paste0(
  "---\n",
  "title: \"", monthly_title, "\"\n",
  "date: \"", monthly_date, "\"\n",
  "toc: true\n",
  "page-layout: article\n",
  "format:\n",
  "  html:\n",
  "    df-print: paged\n",
  "---\n\n",
  "> This page compiles **_all_** daily posts for ", callout_month, ".\n\n"
)

writeLines(c(monthly_yaml, unlist(entries)), con = monthly_path)

message(sprintf("Wrote monthly file: %s", monthly_path))
message(sprintf("Created monthly folder: %s", monthly_folder))
message(sprintf("Copied daily img assets (if any) into: %s", assets_root))
message(sprintf(
  "Next: move daily folders for %s into %s so they stop rendering on the main daily page.",
  target_month,
  monthly_folder
))
