# securityCheck.R - static security tripwire, run by the GitHub Actions
# checks before anything deploys.
#
# PROVENANCE: written by Claude Code (model Claude Opus 5), 2026-08-20,
# from the full-repository security review Steve requested. The review's
# conclusions live in AGENTS.md ("Security"); this script mechanises the
# handful of properties that a one-line diff could silently break. It is
# a TRIPWIRE, not a substitute for reviewing new code: it catches the
# known-dangerous patterns recurring, nothing more.
#
# THE PROPERTIES IT PINS (each verified by hand in the review):
#  1. Nothing in R/ evaluates constructed code or shells out, except the
#     one reviewed subprocess launcher (parseBaselineTableFiles.R, which
#     shQuote()s every argument and runs Rscript --vanilla).
#  2. The comments log stays HTML-escaped at its single entry point
#     (outputComments.R) - it is rendered with HTML(), and file names
#     from uploads flow into it.
#  3. No workflow uses pull_request_target, which would hand repository
#     secrets (the shinyapps tokens) to code from forked PRs.
#  4. No credential material is committed.
#
# Usage:  Rscript tools/securityCheck.R     (exit 0 = pass, 1 = fail)

fail <- character(0)
note <- function(msg) fail <<- c(fail, msg)

rFiles <- list.files("R", pattern = "[.]R$", full.names = TRUE)
srcOf <- function(f) readLines(f, warn = FALSE)

## 1 - code execution primitives -----------------------------------------
# system2 is allowed ONLY in the reviewed subprocess launcher; everything
# else on this list is banned outright in R/ (corpus/ and tools/ are
# local tooling, reviewed but not deployed - the app is what ships).
banned <- c("\\bsystem\\s*\\(",
            "\\bshell\\s*\\(",
            "\\bshell.exec\\s*\\(",
            "\\beval\\s*\\(",
            "\\bparse\\s*\\(\\s*text",
            "\\bsource\\s*\\(",
            "\\bReduce\\s*\\(\\s*get\\b")
for (f in rFiles) {
  src <- srcOf(f)
  code <- sub("#.*$", "", src)          # comments may NAME the patterns
  for (pat in banned) {
    hit <- grep(pat, code)
    if (length(hit))
      note(sprintf("%s:%d: banned pattern %s", f, hit[1], pat))
  }
  hit <- grep("\\bsystem2\\s*\\(", code)
  if (length(hit) && basename(f) != "parseBaselineTableFiles.R")
    note(sprintf("%s:%d: system2() outside the reviewed launcher", f, hit[1]))
}

## 2 - the comments log stays escaped ------------------------------------
oc <- srcOf("R/outputComments.R")
if (!any(grepl("\\.escapeHtml\\(text\\)", oc)) ||
    !any(grepl("\\.escapeHtml\\(line\\)", oc)))
  note(paste("R/outputComments.R no longer escapes messages -",
             "the log renders as HTML and carries uploaded file names"))

## 3 - workflow triggers --------------------------------------------------
for (wf in list.files(".github/workflows", pattern = "[.]ya?ml$",
                      full.names = TRUE)) {
  if (any(grepl("pull_request_target", srcOf(wf))))
    note(paste0(wf, ": pull_request_target exposes deploy secrets to forks"))
}

## 4 - committed credentials ----------------------------------------------
# Tracked text files only; the corpus xlsx and PDFs are gitignored.
tracked <- system2("git", c("ls-files"), stdout = TRUE)
tracked <- tracked[grepl("[.](R|r|yaml|yml|md|Rmd|html|css|json|txt|csv)$",
                         tracked)]
secretPat <- c("sk-ant-[A-Za-z0-9-]{10,}",
               "ANTHROPIC_API_KEY\\s*[=:]\\s*[A-Za-z0-9_-]{12,}",
               "SHINY_(TOKEN|SECRET)\\s*[=:]\\s*[A-Za-z0-9_-]{12,}")
for (f in setdiff(tracked, "tools/securityCheck.R")) {
  if (!file.exists(f)) next
  src <- srcOf(f)
  for (pat in secretPat) {
    hit <- grep(pat, src)
    if (length(hit))
      note(sprintf("%s:%d: looks like a committed credential (%s)",
                   f, hit[1], pat))
  }
}

## ------------------------------------------------------------------------
if (length(fail)) {
  cat("SECURITY CHECK FAILED:
")
  for (m in fail) cat("  -", m, "
")
  quit(status = 1)
}
cat("Security check passed:", length(rFiles), "R/ files,",
    "4 property groups.
")
