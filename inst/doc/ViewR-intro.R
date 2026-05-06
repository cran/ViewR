## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE   # examples require an interactive Shiny session
)

## ----eval=FALSE---------------------------------------------------------------
# library(ViewR)
# ViewR(mtcars)

## ----signature----------------------------------------------------------------
# ViewR(
#   data,
#   edit          = FALSE,   # enable Excel-like editing
#   popup         = TRUE,    # open as modal dialog
#   labels        = NULL,    # named vector of variable labels
#   title         = NULL,    # custom window title
#   viewer        = "dialog",# "dialog", "browser", or "pane"
#   generate_code = TRUE,    # show R Code tab
#   theme         = "flatly",# Bootstrap theme
#   max_display   = 50000L,  # row cap for rendering
#   return_data   = TRUE     # return (possibly edited) data on close
# )

## ----pipe---------------------------------------------------------------------
# library(dplyr)
# 
# clean_data <- mtcars |>
#   filter(cyl >= 4) |>
#   ViewR(edit = TRUE)   # open popup; result assigned on Done

## ----labels-auto--------------------------------------------------------------
# # df <- haven::read_sav("my_survey.sav")
# # ViewR(df)   # labels appear as tooltips automatically

## ----labels-manual------------------------------------------------------------
# ViewR(mtcars,
#       labels = c(
#         mpg  = "Miles per Gallon",
#         cyl  = "Number of Cylinders",
#         disp = "Displacement (cu.in.)",
#         hp   = "Gross Horsepower",
#         wt   = "Weight (1000 lbs)"
#       ))

## ----themes-------------------------------------------------------------------
# ViewR(iris, theme = "darkly")    # dark
# ViewR(iris, theme = "cerulean")  # light blue
# ViewR(iris, theme = "cosmo")     # clean & minimal
# ViewR(iris, theme = "sandstone") # warm beige

## ----viewer-------------------------------------------------------------------
# # Open in the system browser (useful for large datasets)
# ViewR(iris, viewer = "browser")
# 
# # Open in the RStudio Viewer pane
# ViewR(iris, viewer = "pane")
# 
# # Or equivalently, set popup = FALSE (defaults to browser)
# ViewR(iris, popup = FALSE)

## ----editing------------------------------------------------------------------
# # Open the editor; assign the result
# corrected <- ViewR(survey_data, edit = TRUE)
# 
# # 'corrected' now contains all inline edits + any find-replace operations
# head(corrected)

## ----survey-workflow----------------------------------------------------------
# library(ViewR)
# 
# # Load a dataset (here we use the built-in survey proxy)
# data(Titanic)
# titanic_df <- as.data.frame(Titanic)
# 
# # Step 1 — Explore: inspect variable info and browse the data
# ViewR(titanic_df,
#       labels = c(Class    = "Passenger Class",
#                  Sex      = "Passenger Sex",
#                  Age      = "Age Group",
#                  Survived = "Survival Status",
#                  Freq     = "Count"),
#       theme  = "flatly")
# 
# # Step 2 — Clean: use Find & Replace inside the popup to
# #   standardise "Male" -> "M", "Female" -> "F"
# 
# # Step 3 — Filter & export: the R Code tab will have generated:
# #   titanic_df_result <- titanic_df |>
# #     filter(`Survived` == "Yes") |>
# #     arrange(desc(`Freq`))
# # Copy, paste, and run!
# 
# # Step 4 — Edit specific cells if needed
# titanic_clean <- ViewR(titanic_df, edit = TRUE)

## ----session, eval=TRUE-------------------------------------------------------
sessionInfo()

