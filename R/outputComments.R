# outputComments.R — session-aware logging to the app's comments panel.
#
# PROVENANCE: moved from app_globals.R (originally global.R) to its own file
# in phase 2 of the package restructure (Claude Code, model Claude Fable 5,
# 2026-08-16). Body untouched.

#' Write a message to the console and to the session's comments log
#'
#' When called anywhere inside a running Shiny session (including from
#' [P_Calc()] and [validateData()], which are ordinary functions), the
#' active session is recovered with `getDefaultReactiveDomain()` and the
#' message is appended to the `commentsLog` reactive that `app_server`
#' registered in `session$userData`. Outside Shiny it just prints.
#' Data frames are captured line by line via `print(digits = 3)`.
#'
#' @param ... message parts, pasted with `sep`; or a single data frame.
#' @param echo also `cat()` to the console (default from option
#'   `ECHO_OUTPUT_COMMENTS`, `TRUE` if unset; `NA` suppresses everything).
#' @param sep separator used when pasting multiple arguments.
#' @return invisibly `NULL`; called for its side effects.
#' @noRd
outputComments <- function(
    ...,
    echo = getOption("ECHO_OUTPUT_COMMENTS", TRUE),
    sep = " ")
{
  isolate({
    argslist <- list(...)
    if (length(argslist) == 1) {
      text <- argslist[[1]]
    } else {
      text <- paste(argslist, collapse = sep)
    }

    # If this is called within a shiny app, try to get the active session
    # and write to the session's logger
    commentsLog <- function(x) invisible(NULL)
    session <- getDefaultReactiveDomain()
    if (!is.null(session) &&
        is.environment(session$userData) &&
        is.reactive(session$userData$commentsLog))
    {
      commentsLog <- session$userData$commentsLog
    }

    if (is.na(echo)) return()
    if (is.data.frame((text)))
    {
      con <- textConnection("outputString","w",local=TRUE)
      capture.output(print(text, digits = 3), file = con, type="output", split = FALSE)
      close(con)
      if (echo)
      {
        for (line in outputString) cat(line, "\n")
      }
      for (line in outputString)
        commentsLog(paste0(commentsLog(), "<br>", .escapeHtml(line)))
    } else {
      if (echo)
      {
        cat(text, "\n")
      }
      commentsLog(paste0(commentsLog(), "<br>", .escapeHtml(text)))
    }
  })
}

#' Escape text for the HTML comments panel
#'
#' SECURITY (2026-08-20, security review): the comments log is rendered
#' with `HTML()` in app_server (the log's own line breaks are `<br>`
#' tags), and messages routinely embed USER-CONTROLLED strings - most
#' directly the names of uploaded files. The threat model here is not
#' hypothetical: the adversary for this app is the AUTHOR of a manuscript
#' under investigation, and an editor can be induced to upload a file the
#' author supplied. Without escaping, a file named
#' `<img src=x onerror=...>.pdf` would execute script in the editor's
#' session. Every message is therefore escaped at the single point where
#' it enters the log; no caller passes intentional markup (verified over
#' all 25 call sites). Local rather than htmltools::htmlEscape to keep
#' the security property visible in this file (same reasoning as the
#' local sumz()). tools/securityCheck.R asserts this function stays in
#' use here.
#'
#' @param x character vector.
#' @return `x` with HTML metacharacters escaped.
#' @noRd
.escapeHtml <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
