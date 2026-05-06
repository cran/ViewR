# =============================================================================
# ViewR -- ViewR.R
# Main exported function and dependency installer.
# =============================================================================


#' ViewR: Interactive Data Viewer, Filter, and Editor
#'
#' Opens a feature-rich, popup-based Shiny interface for viewing, exploring,
#' filtering, sorting, editing, and analysing R data frames. All operations
#' are reflected in real-time as copy-pasteable \pkg{dplyr} code.
#'
#' @section Features:
#' \itemize{
#'   \item \strong{Data View} - searchable, paginated DT table with optional
#'         variable-label tooltips and column-level search filters.
#'   \item \strong{Sidebar Filters} - add unlimited filter conditions with
#'         operators \code{==}, \code{!=}, \code{>}, \code{>=}, \code{<},
#'         \code{<=}, \emph{contains}, \emph{starts with}, \emph{ends with},
#'         \emph{is NA}, \emph{is not NA}; combine with AND / OR logic.
#'   \item \strong{Multi-column Sort} - add multiple sort levels; choose
#'         ascending or descending per column.
#'   \item \strong{Column Visibility} - show/hide any columns via checkboxes.
#'   \item \strong{Excel-like Editor} (\code{edit = TRUE}) - powered by
#'         \pkg{rhandsontable}; supports in-cell editing, adding rows, and
#'         unlimited undo/redo.
#'   \item \strong{Find & Replace} - find literal text or regex across one or
#'         all columns with case-sensitivity and exact-match options; preview
#'         changes before applying.
#'   \item \strong{Variable Info} - data type, N, missing %, unique count,
#'         min, max, and sample values for every column.
#'   \item \strong{R Code Generation} - the \emph{R Code} tab always shows
#'         the complete, runnable \pkg{dplyr} pipeline for the current state.
#'         One click copies it to the clipboard.
#' }
#'
#' @param data          A \code{data.frame} or \code{tibble} to view / edit.
#' @param edit          Logical. Enable the Excel-like editing tab.
#'                      Default \code{FALSE}.
#' @param popup         Logical. If \code{TRUE} (default), open in a popup
#'                      dialog; if \code{FALSE}, open in the system browser.
#' @param labels        Optional named character vector of variable labels.
#'                      Names must match column names of \code{data}. If
#'                      \code{NULL}, labels are read from column attributes
#'                      (e.g.\ as set by \pkg{haven}).
#' @param title         Window title. Defaults to the name of \code{data}.
#' @param viewer        Where to display the interface:
#'   \describe{
#'     \item{\code{"dialog"}}{Popup dialog (default when \code{popup = TRUE}).}
#'     \item{\code{"browser"}}{System web browser.}
#'     \item{\code{"pane"}}{RStudio Viewer pane.}
#'   }
#' @param generate_code Logical. Show the R Code tab. Default \code{TRUE}.
#' @param theme         Bootstrap theme for the 'UI'.  One of
#'                      \code{"flatly"} (default), \code{"cerulean"},
#'                      \code{"cosmo"}, \code{"darkly"}, \code{"lumen"},
#'                      \code{"paper"}, \code{"readable"}, \code{"sandstone"},
#'                      \code{"simplex"}, \code{"spacelab"}, \code{"united"},
#'                      \code{"yeti"}.
#' @param max_display   Integer. Maximum rows rendered in the Data View table
#'                      (for performance). Default \code{50000}.
#' @param return_data   Logical. When the user clicks \emph{Done}, return the
#'                      (possibly edited) data frame.  Default \code{TRUE}.
#' @param ...           Reserved for future arguments; currently ignored.
#'
#' @return If \code{return_data = TRUE} and the user clicked \emph{Done},
#'   returns the modified data frame invisibly.  Otherwise returns the
#'   original data frame invisibly.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ViewR(mtcars)
#'
#'   new_iris <- ViewR(iris, edit = TRUE)
#'
#'   ViewR(mtcars,
#'         labels = c(mpg = "Miles per Gallon",
#'                    cyl = "Number of Cylinders",
#'                    hp  = "Gross Horsepower"),
#'         theme  = "darkly")
#'
#'   ViewR(iris, viewer = "browser", generate_code = TRUE)
#' }
ViewR <- function(data,
                  edit          = FALSE,
                  popup         = TRUE,
                  labels        = NULL,
                  title         = NULL,
                  viewer        = c("dialog", "browser", "pane"),
                  generate_code = TRUE,
                  theme         = c("flatly", "cerulean", "cosmo", "darkly",
                                    "lumen", "paper", "readable", "sandstone",
                                    "simplex", "spacelab", "united", "yeti"),
                  max_display   = 50000L,
                  return_data   = TRUE,
                  ...) {

  # -- Input validation --------------------------------------------------------
  if (!is.data.frame(data))
    stop("'data' must be a data.frame or tibble.", call. = FALSE)

  data_name <- deparse(substitute(data))
  # Shorten long names (e.g. from pipes / complex expressions)
  if (nchar(data_name) > 40)
    data_name <- substr(data_name, 1, 37) |> paste0("...")

  if (is.null(title))
    title <- paste0("ViewR \u2014 ", data_name)

  viewer <- match.arg(viewer)
  theme  <- match.arg(theme)

  # Override viewer if popup = FALSE
  if (!isTRUE(popup) && viewer == "dialog")
    viewer <- "browser"

  # -- Coerce to plain data.frame ----------------------------------------------
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # -- Resolve variable labels -------------------------------------------------
  resolved_labels <- .vr_resolve_labels(data, labels)

  # -- Build Shiny app ---------------------------------------------------------
  app <- shiny::shinyApp(
    ui     = .vr_ui(data, title, theme, isTRUE(edit), isTRUE(generate_code),
                    n_rows = nrow(data)),
    server = .vr_server(data, data_name, resolved_labels,
                        isTRUE(edit), isTRUE(generate_code),
                        as.integer(max_display))
  )

  # -- Select Shiny viewer -----------------------------------------------------
  shiny_viewer <- switch(
    viewer,
    "dialog"  = shiny::dialogViewer(title, width = 1200, height = 860),
    "browser" = shiny::browserViewer(),
    "pane"    = shiny::paneViewer(minHeight = "maximize")
  )

  # -- Launch ------------------------------------------------------------------
  result <- tryCatch(
    shiny::runGadget(app, viewer = shiny_viewer, stopOnCancel = FALSE),
    error = function(e) {
      # Graceful degradation if gadget is interrupted
      NULL
    }
  )

  # -- Return value -------------------------------------------------------------
  if (isTRUE(return_data) && !is.null(result)) {
    invisible(result)
  } else {
    invisible(data)
  }
}
