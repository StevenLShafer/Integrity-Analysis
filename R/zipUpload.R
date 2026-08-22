# zipUpload.R - accept a .zip in the upload box and expand it into the
# ordinary multi-file upload.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-20,
# at Steve Shafer's request: an investigator reproducing an analysis
# like Carlisle's 2012 review of Fujii (PMID 22404311, 168 trials)
# should be able to zip every trial file - spreadsheets, article PDFs,
# any mix - and upload ONE archive. Each entry then flows through the
# same per-file pipeline as a direct multi-file selection: every file
# becomes a frame, frames concatenate into one table distinguished by
# TRIAL, and a file without a TRIAL column is named after its own file
# name - so a zip of 30 PDFs becomes 30 trials with sensible names.
#
# SECURITY (AGENTS.md "Security"; uploads are hostile input): archives
# get the full defensive treatment -
#   - entries are extracted ONE AT A TIME with junkpaths = TRUE into a
#     fresh per-entry directory, so an entry named "../../evil.pdf" or
#     "C:/autoexec.bat" cannot write outside the extraction root (the
#     path part is discarded entirely; the name filter below refuses
#     traversal names anyway, belt and braces);
#   - nested archives are NOT recursed into (a zip inside a zip is
#     skipped with a message) - archive recursion is how decompression
#     bombs hide;
#   - entry count and total UNCOMPRESSED size are capped before any
#     byte is extracted (the listing reports sizes), and each extracted
#     file is verified to be no larger than the listing promised;
#   - only the extensions the upload box accepts are taken; macOS
#     resource-fork junk (__MACOSX/, .DS_Store, ._* files) is dropped
#     silently.
# Extracted files land under tempdir(), so the purge-on-exit guarantee
# covers them like any direct upload.

# Caps, chosen generously above any legitimate use (Carlisle's Fujii
# analysis is 168 trials) but far below bomb territory:
.zipMaxEntries <- 300L         # supported files per archive
.zipMaxTotalBytes <- 300 * 1024^2   # total uncompressed
.zipMaxEntryBytes <- 50 * 1024^2    # any single file

#' Classify the entries of a zip listing
#'
#' Pure function over `utils::unzip(list = TRUE)` output, so the
#' hostile-name handling is unit-testable without crafting hostile
#' archives.
#'
#' @param entries data.frame with columns Name and Length.
#' @return the same rows plus `take` (logical) and `why` (skip reason,
#'   "" for taken rows and silently-dropped junk).
#' @noRd
.zipEntryPlan <- function(entries) {
  name <- entries$Name
  len  <- entries$Length
  take <- rep(TRUE, length(name))
  why  <- rep("", length(name))
  drop <- function(i, reason = "") {
    take[i] <<- FALSE
    why[i]  <<- reason
  }

  base <- basename(gsub("\\\\", "/", name))
  ext  <- tolower(tools::file_ext(base))

  # directories (Length 0 and trailing slash) and macOS junk: silent
  isDir  <- grepl("[/\\\\]$", name) | (len == 0 & ext == "")
  isJunk <- grepl("(^|/)__MACOSX(/|$)", name) | base == ".DS_Store" |
    startsWith(base, "._")
  drop(which(isDir | isJunk))

  # traversal and absolute names: refused loudly even though junkpaths
  # would neuter them - a crafted archive deserves a visible refusal
  hostile <- grepl("^([/\\\\]|[A-Za-z]:)", name) |
    grepl("(^|[/\\\\])[.][.]([/\\\\]|$)", name)
  drop(which(hostile & take), "unsafe path in archive")

  # A .docx is itself a zip container, but it is matched by EXTENSION in
  # the allowlist below, and this nested-archive check runs first - so
  # "docx" must never be added to this list, or Word uploads inside an
  # archive would be refused as nested archives (issue 19).
  nested <- ext %in% c("zip", "gz", "tgz", "tar", "7z", "rar")
  drop(which(nested & take), "nested archive (not expanded)")

  unsupported <- !ext %in% c("csv", "xlsx", "xls", "pdf", "docx")
  drop(which(unsupported & take & !isDir & !isJunk & !hostile & !nested),
       "not a supported file type")

  tooBig <- len > .zipMaxEntryBytes
  drop(which(tooBig & take),
       paste0("larger than ", .zipMaxEntryBytes / 1024^2, " MB"))

  data.frame(Name = name, Length = len, take = take, why = why,
             stringsAsFactors = FALSE)
}

#' Expand any .zip rows of an upload into their usable entries
#'
#' @param files the `input$upload` data.frame (`name`, `datapath`) with
#'   `ext` and `stem` columns already added by the caller.
#' @param say logging function (defaults to [outputComments()]).
#' @return `files` with each zip row replaced by one row per extracted
#'   entry (`name` shows "archive.zip: entry.pdf" so log messages stay
#'   attributable); non-zip rows pass through untouched. The caller must
#'   add the returned `datapath`s to the purge list exactly as it does
#'   for direct uploads.
#' @noRd
expandZipUploads <- function(files, say = outputComments) {
  zipIdx <- which(files$ext == "zip")
  if (length(zipIdx) == 0) return(files)

  keep <- files[-zipIdx, , drop = FALSE]

  for (i in zipIdx) {
    zipName <- files$name[i]
    entries <- tryCatch(utils::unzip(files$datapath[i], list = TRUE),
                        error = function(e) NULL)
    if (is.null(entries) || nrow(entries) == 0) {
      say(paste0(zipName, " could not be read as a zip archive."))
      next
    }
    plan <- .zipEntryPlan(entries)
    for (k in which(!plan$take & nzchar(plan$why)))
      say(paste0(zipName, ": skipped ", plan$Name[k], " (", plan$why[k], ")."))
    plan <- plan[plan$take, , drop = FALSE]

    if (nrow(plan) == 0) {
      say(paste0(zipName,
                 " contains no usable files (csv, xls, xlsx, pdf, docx)."))
      next
    }
    if (nrow(plan) > .zipMaxEntries) {
      say(paste0(zipName, " holds ", nrow(plan), " usable files; only the ",
                 "first ", .zipMaxEntries, " are taken."))
      plan <- plan[seq_len(.zipMaxEntries), , drop = FALSE]
    }
    if (sum(plan$Length) > .zipMaxTotalBytes) {
      say(paste0(zipName, " would expand to ",
                 round(sum(plan$Length) / 1024^2), " MB - over the ",
                 .zipMaxTotalBytes / 1024^2, " MB limit. Archive refused."))
      next
    }

    # one directory per entry, junkpaths always: no archive-controlled
    # path ever touches the filesystem
    exRoot <- file.path(tempdir(),
                        paste0("zip", i, "_", format(Sys.time(), "%H%M%OS3")))
    got <- 0L
    for (k in seq_len(nrow(plan))) {
      exd <- file.path(exRoot, k)
      dir.create(exd, recursive = TRUE, showWarnings = FALSE)
      path <- tryCatch({
        utils::unzip(files$datapath[i], files = plan$Name[k],
                     exdir = exd, junkpaths = TRUE)
      }, error = function(e) character(0), warning = function(w) character(0))
      if (length(path) != 1 || !file.exists(path[1])) {
        say(paste0(zipName, ": could not extract ", plan$Name[k], "."))
        next
      }
      # a lying central directory (bomb tactic): listed size small,
      # actual inflation large. Verify what landed.
      if (file.size(path[1]) > max(plan$Length[k], 1) * 1.01 + 1024) {
        say(paste0(zipName, ": ", plan$Name[k],
                   " inflated beyond its declared size; discarded."))
        unlink(path[1])
        next
      }
      base <- basename(path[1])
      keep <- rbind(keep, data.frame(
        name = paste0(zipName, ": ", base),
        size = file.size(path[1]),
        type = "",
        datapath = path[1],
        ext = tolower(tools::file_ext(base)),
        stem = tools::file_path_sans_ext(base),
        stringsAsFactors = FALSE)[, names(keep), drop = FALSE])
      got <- got + 1L
    }
    say(paste0("Unpacked ", zipName, ": ", got, " file(s)."))
  }
  keep
}
