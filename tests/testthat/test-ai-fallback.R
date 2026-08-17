# Tests for the AI engine that do not touch the network: request shape,
# response decoding, and the mapping from the model's JSON to template rows.

test_that("the request body carries the schema with arrays intact", {
  body <- .ppClaudeRequestBody("Table 1. Baseline characteristics ...")
  expect_equal(body$model, "claude-opus-5")
  expect_equal(body$output_config$format$type, "json_schema")

  # httr2 serialises with auto_unbox = TRUE; single-element JSON arrays must
  # survive that rather than collapsing to scalars.
  js <- jsonlite::toJSON(body, auto_unbox = TRUE)
  expect_match(js, '"required":\\["found"', fixed = FALSE)
  expect_match(js, '"additionalProperties":false', fixed = TRUE)
  expect_match(js, '"type":\\["integer","null"\\]')
  # The page text reaches the user turn
  expect_match(js, "BEGIN TEXT", fixed = TRUE)
})

test_that("the prose request asks for baseline data in running text", {
  body <- .ppClaudeRequestBody("Patients were aged 45 (12) and 47 (11) years.",
                               source = "prose")
  expect_match(body$system, "running text|narrative")
  expect_match(body$system, "before or at\\s+randomization|at baseline")
  expect_match(body$messages[[1]]$content, "stated in the narrative")
  # It must not tell the model to read a table that is not there
  expect_false(grepl("column alignment", body$messages[[1]]$content))

  tbl <- .ppClaudeRequestBody("x", source = "table")
  expect_match(tbl$messages[[1]]$content, "column alignment")
  expect_false(grepl("stated in the narrative", tbl$messages[[1]]$content))
  # Both sources share one schema, so both map through .ppAiToTemplate()
  expect_identical(body$output_config$format$schema,
                   tbl$output_config$format$schema)
})

test_that("effort and model are passed through", {
  body <- .ppClaudeRequestBody("x", model = "claude-sonnet-5", effort = "low")
  expect_equal(body$model, "claude-sonnet-5")
  expect_equal(body$output_config$effort, "low")
})

test_that("a hint from the deterministic pass is included when supplied", {
  expect_match(.ppUserPrompt("page", hint = "Arms already identified: A; B"),
               "Arms already identified", fixed = TRUE)
  expect_false(grepl("deterministic parser already established",
                     .ppUserPrompt("page"), fixed = TRUE))
})

test_that("structured output is pulled out of a normal response", {
  resp <- list(stop_reason = "end_turn",
               content = list(list(type = "text",
                                   text = '{"found":true,"notes":"","arms":[]}')))
  out <- .ppClaudeStructuredOutput(resp)
  expect_true(out$found)
})

test_that("a refusal is reported as a refusal, not as malformed JSON", {
  resp <- list(stop_reason = "refusal",
               stop_details = list(category = "cyber"),
               content = list())
  expect_error(.ppClaudeStructuredOutput(resp), "declined this request")
})

test_that("a truncated reply is reported rather than silently parsed", {
  resp <- list(stop_reason = "max_tokens",
               content = list(list(type = "text", text = '{"found":tr')))
  expect_error(.ppClaudeStructuredOutput(resp), "max_tokens")
})

# A canned reply in the shape the schema requires.
cannedReply <- function() jsonlite::fromJSON('{
  "found": true,
  "notes": "Duration of surgery reported as median [range]; omitted.",
  "arms": [{"name": "Control", "n": 15}, {"name": "Treatment", "n": 17}],
  "continuous": [
    {"label": "Age", "decimalsMean": 1,
     "values": [{"arm": "Control", "n": null, "mean": 45.3, "sd": 12.1},
                {"arm": "Treatment", "n": null, "mean": 46.1, "sd": 11.8}]}
  ],
  "categorical": [
    {"label": "Sex", "categories": ["Male", "Female"],
     "values": [{"arm": "Control", "counts": [10, 5]},
                {"arm": "Treatment", "counts": [12, 5]}]}
  ]
}', simplifyVector = FALSE)

test_that("the model's JSON maps onto the template layout", {
  tbl <- .ppAiToTemplate(cannedReply(), trial = "T", roundObsDelta = 1)
  d   <- tbl$data

  expect_equal(nrow(d), 4)                       # 2 variables x 2 arms
  expect_equal(names(d)[seq_along(.ppBaseColumns())], .ppBaseColumns())  # base columns stay leftmost
  expect_identical(tbl$arms$N, c(15L, 17L))

  age <- d[d$ROW == "Age", ]
  expect_equal(age$MEAN, c(45.3, 46.1))
  expect_equal(age$N, c(15L, 17L))               # arm N used when the cell is null
  expect_equal(age$ROUND_MEAN, c(1L, 1L))
  expect_equal(age$ROUND_OBSERVATION, c(2L, 2L))

  sex <- d[d$ROW == "Sex", ]
  expect_equal(sex$Male, c(10L, 12L))
  expect_equal(sex$Female, c(5L, 5L))
  expect_true(all(is.na(sex$MEAN)))              # the app requires this
  expect_true(all(is.na(sex$N)))
})

test_that("category levels from different variables cannot collide", {
  reply <- jsonlite::fromJSON('{
    "found": true, "notes": "",
    "arms": [{"name": "A", "n": 10}],
    "continuous": [],
    "categorical": [
      {"label": "Smoking", "categories": ["Yes", "No"],
       "values": [{"arm": "A", "counts": [3, 7]}]},
      {"label": "Diabetes", "categories": ["Yes", "No"],
       "values": [{"arm": "A", "counts": [2, 8]}]}
    ]}', simplifyVector = FALSE)
  d <- .ppAiToTemplate(reply, trial = "T")$data
  expect_true(all(c("Yes", "No", "Yes 2", "No 2") %in% names(d)))
  expect_equal(d$Yes[d$ROW == "Smoking"], 3L)
  expect_equal(d$`Yes 2`[d$ROW == "Diabetes"], 2L)
})

test_that("a malformed variable is dropped rather than corrupting the table", {
  reply <- jsonlite::fromJSON('{
    "found": true, "notes": "",
    "arms": [{"name": "A", "n": 10}],
    "continuous": [
      {"label": "Age", "decimalsMean": 0,
       "values": [{"arm": "A", "n": null, "mean": 45, "sd": null}]}
    ],
    "categorical": [
      {"label": "Sex", "categories": ["Male", "Female"],
       "values": [{"arm": "A", "counts": [6]}]}
    ]}', simplifyVector = FALSE)
  d <- .ppAiToTemplate(reply, trial = "T")$data
  expect_equal(nrow(d), 0)   # SD missing, and counts do not match the levels
})

test_that("claudeAvailable reflects the environment without calling out", {
  old <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("ANTHROPIC_API_KEY")
          else Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)

  Sys.unsetenv("ANTHROPIC_API_KEY")
  expect_false(claudeAvailable())
  expect_error(.ppApiKey(), "ANTHROPIC_API_KEY")

  Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-test")
  expect_true(claudeAvailable())
  expect_equal(.ppApiKey(), "sk-ant-test")
  expect_equal(.ppApiKey("explicit"), "explicit")
})
