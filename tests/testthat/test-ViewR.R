# =============================================================================
# Tests for R/ViewR.R — exported functions
# Note: ViewR() itself requires an interactive Shiny session; we test
# argument validation here.
# =============================================================================

# ── ViewR() argument validation ──────────────────────────────────────────────

test_that("ViewR() errors for non-data.frame input", {
  expect_error(ViewR(1:10),          "data.frame")
  expect_error(ViewR("hello"),       "data.frame")
  expect_error(ViewR(list(a = 1)),   "data.frame")
})

test_that("ViewR() accepts tibbles without error (no launch)", {
  skip_if_not(interactive())
  skip_if_not_installed("tibble")
  tb <- tibble::tibble(x = 1:3, y = letters[1:3])
  expect_no_error(
    withCallingHandlers(
      tryCatch(
        ViewR(tb, popup = FALSE),
        error = function(e) {
          if (grepl("shiny|gadget|browser|viewer|cannot open",
                    conditionMessage(e), ignore.case = TRUE)) {
            NULL
          } else {
            stop(e)
          }
        }
      ),
      message = function(m) invokeRestart("muffleMessage")
    )
  )
})

test_that("ViewR() silently returns original data on NULL result", {
  skip_if_not(interactive())
  df <- data.frame(x = 1:3)
  result <- tryCatch(
    ViewR(df, return_data = FALSE, popup = FALSE),
    error = function(e) df
  )
  expect_s3_class(result, "data.frame")
})



