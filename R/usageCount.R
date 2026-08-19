# usageCount.R - anonymous usage counting (Steve's request, 2026-08-19).
#
# PROVENANCE: written by Claude Code (model Claude Fable 5), 2026-08-19.
# Steve: "I am tabulating the number of times it is called, but no other
# information, to see if the program is being used. No point in
# maintaining a program that nobody uses."
#
# HOW THE PRIVACY PROPERTY HOLDS BY CONSTRUCTION: the ping is sent by
# the R SERVER process, not the user's browser, so the user's IP address
# never reaches the counter - the counter sees only the hosting
# service's address. The payload is one event name ("session" or
# "analyze"). No cookies, no fingerprint, no content. Disclosed in the
# sidebar, the user guide, and the landing page, in Steve's words.
#
# Counting is OFF unless run_app() turns it on, which it does only for
# the production deployment (a testNote - every PR test app - disables
# it, and the test suite never calls run_app), so the counts mean what
# Steve wants them to mean: real use.

#' Count one usage event, anonymously and non-blockingly
#'
#' Fire-and-forget GET to Steve's GoatCounter site. Any failure - DNS,
#' timeout, the counter being down - is swallowed: counting must never
#' break or delay the app for a user.
#'
#' @param event short path-safe event name ("session", "analyze").
#' @noRd
countUsage <- function(event) {
  if (!isTRUE(getOption("IntegrityAnalysis.countUsage"))) {
    return(invisible(FALSE))
  }
  try(silent = TRUE, {
    # base URL overridable so tests can point at an unreachable address
    # instead of polluting the real counts
    base <- getOption("IntegrityAnalysis.countUrl",
                      "https://integrityanalysis.goatcounter.com/count")
    req <- httr2::request(paste0(
      base, "?p=/", utils::URLencode(event, reserved = TRUE)))
    req <- httr2::req_timeout(req, 3)
    # an honest identity; GoatCounter discards tool-default agents
    req <- httr2::req_user_agent(
      req, "IntegrityAnalysis/1.0 (server-side usage counter)")
    httr2::req_perform(req)
  })
  invisible(TRUE)
}
