## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  echo     = TRUE,
  message  = FALSE,
  warning  = FALSE,
  screenshot.force = FALSE
)
library(ViewR)

## -----------------------------------------------------------------------------
library(ViewR)
viewdt(mtcars)

## -----------------------------------------------------------------------------
viewdt(
  iris,
  options = viewdt_options(
    theme          = "dark",
    hidden_columns = "Species",
    na_string      = "—"
  )
)

## -----------------------------------------------------------------------------
df <- data.frame(
  AGE = c(54, 61, 47, NA, 72),
  SEX = c("M", "F", "F", "M", "F"),
  BMI = c(24.1, 28.7, 22.3, 31.0, 26.4)
)
attr(df$AGE, "label") <- "Age (years)"
attr(df$SEX, "label") <- "Sex"
attr(df$BMI, "label") <- "Body Mass Index"

viewdt(df, options = viewdt_options(theme = "light"))

## -----------------------------------------------------------------------------
viewdt(
  mtcars,
  options = viewdt_options(
    query_builder = FALSE,
    code_export   = FALSE,
    insights      = FALSE,
    global_search = TRUE
  )
)

## ----eval = FALSE-------------------------------------------------------------
# # Single self-contained file (needs pandoc)
# save_viewdt(mtcars, "mtcars.html", open = TRUE)
# 
# # Lightweight file + companion _files/ dir (best for large data)
# save_viewdt(iris, "iris.html", selfcontained = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# library(shiny)
# ui <- fluidPage(viewdtOutput("grid", height = "640px"))
# server <- function(input, output, session) {
#   output$grid <- renderViewdt(viewdt(mtcars))
# }
# shinyApp(ui, server)

