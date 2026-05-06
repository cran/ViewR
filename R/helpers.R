# =============================================================================
# ViewR -- helpers.R
# Utility functions: label resolution, filter engine, sort engine,
# find-and-replace, variable-info table, UI row builders, CSS
# =============================================================================

# -- Null-coalescing operator --------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x


# -- Label resolution ----------------------------------------------------------

#' Resolve variable labels from column attributes and/or user-supplied vector.
#' @noRd
.vr_resolve_labels <- function(data, labels) {
  result <- setNames(rep("", ncol(data)), names(data))

  # Pull labels already stored as column attributes (haven / labelled pkgs)
  for (col in names(data)) {
    lbl <- attr(data[[col]], "label")
    if (!is.null(lbl) && nzchar(as.character(lbl))) {
      result[col] <- as.character(lbl)
    }
  }

  # Overlay user-supplied labels (takes precedence)
  if (!is.null(labels) && length(labels) > 0) {
    valid <- intersect(names(labels), names(data))
    if (length(valid) > 0) result[valid] <- as.character(labels[valid])
  }

  result
}


# -- Filter engine -------------------------------------------------------------

#' Apply a list of filter conditions to a data frame.
#'
#' @param data     data.frame
#' @param filters  list of filter specs, each with:
#'   \describe{
#'     \item{id}{unique integer id}
#'     \item{column}{column name}
#'     \item{operator}{one of ==, !=, >, >=, <, <=, contains, starts with,
#'                     ends with, is NA, is not NA}
#'     \item{value}{character value to compare against}
#'     \item{logic}{"AND" or "OR" (how to combine with previous filter)}
#'   }
#' @noRd
.vr_apply_filters <- function(data, filters) {
  if (length(filters) == 0) return(data)

  compute_mask <- function(f) {
    col <- data[[f$column]]
    val <- f$value %||% ""

    switch(f$operator,
      "=="          = !is.na(col) & (as.character(col) == val),
      "!="          = !is.na(col) & (as.character(col) != val),
      ">"           = suppressWarnings(!is.na(col) &
                        (as.numeric(col) > as.numeric(val))),
      ">="          = suppressWarnings(!is.na(col) &
                        (as.numeric(col) >= as.numeric(val))),
      "<"           = suppressWarnings(!is.na(col) &
                        (as.numeric(col) < as.numeric(val))),
      "<="          = suppressWarnings(!is.na(col) &
                        (as.numeric(col) <= as.numeric(val))),
      "contains"    = !is.na(col) & grepl(tolower(val),
                        tolower(as.character(col)), fixed = TRUE),
      "starts with" = !is.na(col) & grepl(paste0("^", .vr_re_escape(val)),
                        as.character(col), ignore.case = TRUE),
      "ends with"   = !is.na(col) & grepl(paste0(.vr_re_escape(val), "$"),
                        as.character(col), ignore.case = TRUE),
      "is NA"       = is.na(col),
      "is not NA"   = !is.na(col),
      rep(TRUE, nrow(data))   # fallback
    )
  }

  combined <- compute_mask(filters[[1]])

  if (length(filters) > 1) {
    for (i in seq(2, length(filters))) {
      m <- compute_mask(filters[[i]])
      if (identical(filters[[i]]$logic, "OR")) {
        combined <- combined | m
      } else {
        combined <- combined & m
      }
    }
  }

  data[combined & !is.na(combined), , drop = FALSE]
}


# -- Sort engine ---------------------------------------------------------------

#' Apply a list of sort specs to a data frame.
#'
#' @param data   data.frame
#' @param sorts  list of sort specs, each with column and direction ("asc"|"desc")
#' @noRd
.vr_apply_sorts <- function(data, sorts) {
  if (length(sorts) == 0) return(data)

  order_args <- lapply(sorts, function(s) {
    col <- data[[s$column]]
    if (identical(s$direction, "desc")) {
      if (is.numeric(col) || is.logical(col)) return(-col)
      return(-xtfrm(col))
    }
    col
  })

  ord <- do.call(order, c(order_args, list(na.last = TRUE)))
  data[ord, , drop = FALSE]
}


# -- Find & Replace ------------------------------------------------------------

#' Find-and-replace across one or all character/factor columns.
#'
#' @param data       data.frame
#' @param col_spec   column name or "__all__"
#' @param find       string to find
#' @param replace    replacement string
#' @param case_sens  logical
#' @param use_regex  logical
#' @param exact      logical - match the entire cell value
#' @param preview    logical - if TRUE return a diff preview, not the new data
#' @return list(data, preview, n_replaced, code)
#' @noRd
.vr_find_replace <- function(data, col_spec, find, replace,
                              case_sens, use_regex, exact, preview) {
  if (!nzchar(find)) {
    return(list(data = data, preview = NULL, n_replaced = 0L, code = NULL))
  }

  # Determine target columns (character or factor only for safety)
  if (identical(col_spec, "__all__")) {
    cols <- names(data)[vapply(data, function(x)
      is.character(x) || is.factor(x) || is.numeric(x), logical(1))]
  } else {
    cols <- intersect(col_spec, names(data))
  }
  if (length(cols) == 0) cols <- names(data)

  n_replaced  <- 0L
  new_data    <- data
  code_parts  <- character(0)
  preview_rows <- list()

  for (col in cols) {
    orig_vec <- as.character(data[[col]])

    if (exact) {
      if (case_sens) {
        hits     <- orig_vec == find
        new_vals <- ifelse(hits, replace, orig_vec)
      } else {
        hits     <- tolower(orig_vec) == tolower(find)
        new_vals <- ifelse(hits, replace, orig_vec)
      }
      code_expr <- if (case_sens) {
        paste0('ifelse(`', col, '` == "', find, '", "', replace, '", `', col, '`)')
      } else {
        paste0('ifelse(tolower(`', col, '`) == "', tolower(find),
               '", "', replace, '", `', col, '`)')
      }
    } else {
      pattern <- if (use_regex) find else .vr_re_escape(find)
      new_vals <- gsub(pattern, replace, orig_vec,
                       ignore.case = !case_sens, perl = use_regex)
      hits     <- orig_vec != new_vals & !is.na(orig_vec)
      ic_flag  <- if (!case_sens) ", ignore.case = TRUE" else ""
      perl_flag <- if (use_regex) ", perl = TRUE" else ""
      pat_str  <- if (use_regex) find else .vr_re_escape(find)
      code_expr <- paste0('gsub("', pat_str, '", "', replace,
                          '", `', col, '`', ic_flag, perl_flag, ')')
    }

    n_hits <- sum(hits, na.rm = TRUE)
    if (n_hits > 0L) {
      n_replaced <- n_replaced + n_hits

      if (preview) {
        idx  <- which(hits)
        tmp  <- data.frame(
          Row          = idx,
          Column       = col,
          Original     = orig_vec[idx],
          Replacement  = new_vals[idx],
          stringsAsFactors = FALSE
        )
        preview_rows[[length(preview_rows) + 1L]] <- tmp
      } else {
        # Preserve original class where possible
        new_col <- tryCatch({
          cls <- class(data[[col]])[1L]
          switch(cls,
            numeric   = suppressWarnings(as.numeric(new_vals)),
            integer   = suppressWarnings(as.integer(new_vals)),
            logical   = as.logical(new_vals),
            factor    = factor(new_vals, levels = levels(data[[col]])),
            new_vals  # default: keep as character
          )
        }, error = function(e) new_vals)
        new_data[[col]] <- new_col
      }

      code_parts <- c(code_parts, paste0("  `", col, "` = ", code_expr))
    }
  }

  preview_df <- if (length(preview_rows) > 0) do.call(rbind, preview_rows)
                else data.frame(Row = integer(), Column = character(),
                                Original = character(), Replacement = character(),
                                stringsAsFactors = FALSE)

  code <- if (length(code_parts) > 0) {
    paste0("mutate(\n", paste(code_parts, collapse = ",\n"), "\n)")
  } else NULL

  list(data = new_data, preview = preview_df,
       n_replaced = n_replaced, code = code)
}


# -- Variable info table -------------------------------------------------------

#' Build a data frame summarising each column of \code{data}.
#' @noRd
.vr_var_info <- function(data, labels) {
  nms <- names(data)
  data.frame(
    `#`          = seq_along(nms),
    Column       = nms,
    Label        = ifelse(nzchar(labels[nms]), labels[nms], "\u2014"),
    Type         = vapply(data, function(x) paste(class(x), collapse = "/"),
                          character(1)),
    N            = vapply(data, function(x) sum(!is.na(x)), integer(1)),
    Missing      = vapply(data, function(x) sum(is.na(x)),  integer(1)),
    `Missing %`  = vapply(data, function(x) round(100 * mean(is.na(x)), 1),
                          numeric(1)),
    Unique       = vapply(data, function(x) length(unique(x)), integer(1)),
    Min          = vapply(data, function(x) {
                     if (is.numeric(x)) format(min(x, na.rm = TRUE))
                     else ""
                   }, character(1)),
    Max          = vapply(data, function(x) {
                     if (is.numeric(x)) format(max(x, na.rm = TRUE))
                     else ""
                   }, character(1)),
    `Sample values` = vapply(data, function(x) {
                       v <- x[!is.na(x)]
                       if (length(v) == 0) return("")
                       paste(head(unique(as.character(v)), 4), collapse = " | ")
                     }, character(1)),
    check.names  = FALSE,
    stringsAsFactors = FALSE
  )
}


# -- Dynamic UI row builders ---------------------------------------------------

#' Build one filter row for the sidebar (two-row compact layout).
#' @noRd
.vr_filter_row_ui <- function(fid, col_names, is_first) {
  ops <- c("==" , "!=" , ">" , ">=" , "<" , "<=" ,
           "contains", "starts with", "ends with", "is NA", "is not NA")

  shiny::div(
    id    = paste0("filter_row_", fid),
    class = "vr-filter-row",

    # Top row: [AND/OR (if not first)] + column + remove button
    shiny::div(
      class = "vr-filter-top",
      if (!is_first) shiny::div(
        style = "width:58px; flex-shrink:0;",
        shiny::selectInput(paste0("f_logic_", fid), NULL,
                           choices  = c("AND", "OR"),
                           selected = "AND",
                           width    = "100%")
      ),
      shiny::div(
        style = "flex:1; min-width:0;",
        shiny::selectInput(paste0("f_col_", fid), NULL,
                           choices = col_names,
                           width   = "100%")
      ),
      shiny::div(
        style = "flex-shrink:0; padding-top:1px;",
        shiny::actionButton(paste0("f_remove_", fid), NULL,
                            icon  = shiny::icon("times"),
                            class = "btn-xs btn-danger vr-icon-btn")
      )
    ),

    # Bottom row: operator + value (value auto-hides for is NA / is not NA)
    shiny::div(
      class = "vr-filter-bottom",
      shiny::div(
        style = "flex:1; min-width:0;",
        shiny::selectInput(paste0("f_op_", fid), NULL,
                           choices = ops,
                           width   = "100%")
      ),
      shiny::div(
        class = "vr-filter-val",
        style = "flex:1; min-width:0;",
        shiny::textInput(paste0("f_val_", fid), NULL,
                         placeholder = "value\u2026",
                         width       = "100%")
      )
    )
  )
}


#' Build one sort row for the sidebar.
#' @noRd
.vr_sort_row_ui <- function(sid, col_names) {
  shiny::div(
    id    = paste0("sort_row_", sid),
    class = "vr-sort-row",

    shiny::div(
      style = "flex:3;",
      shiny::selectInput(paste0("s_col_", sid), NULL,
                         choices = col_names,
                         width   = "100%")
    ),

    shiny::div(
      style = "flex:2;",
      shiny::selectInput(paste0("s_dir_", sid), NULL,
                         choices  = c("\u2191 Ascending"  = "asc",
                                      "\u2193 Descending" = "desc"),
                         width    = "100%")
    ),

    shiny::div(
      style = "flex-shrink:0; padding-top:1px;",
      shiny::actionButton(paste0("s_remove_", sid), NULL,
                          icon  = shiny::icon("times"),
                          class = "btn-xs btn-danger vr-icon-btn")
    )
  )
}


# -- Regex escape helper -------------------------------------------------------
.vr_re_escape <- function(x) {
  # Escape POSIX ERE metacharacters using fixed=TRUE substitutions so there
  # is no ambiguity in how the regex engine parses the character class.
  # Backslash must be processed first to avoid double-escaping.
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  for (ch in c(".", "+", "*", "?", "^", "$", "{", "}", "(", ")", "|", "[", "]")) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x
}


# -- Plot renderer -------------------------------------------------------------

#' Render a column-level plot (histogram, bar chart, or boxplot).
#'
#' @param d         data.frame (displayed / filtered)
#' @param col_name  character column to plot
#' @param plot_type one of "auto", "hist", "bar", "box"
#' @param bins      number of histogram bins
#' @noRd
.vr_plot_col <- function(d, col_name, plot_type = "auto", bins = 30L) {
  x       <- d[[col_name]]
  n_na    <- sum(is.na(x))
  x_clean <- x[!is.na(x)]

  main_lbl <- if (n_na > 0L)
    paste0(col_name, "  [", n_na, " NA]")
  else
    col_name

  if (length(x_clean) == 0L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "All values are NA", cex = 1.3, col = "gray60")
    return(invisible(NULL))
  }

  is_num  <- is.numeric(x_clean)
  n_uniq  <- length(unique(x_clean))

  eff_type <- if (plot_type == "auto") {
    if (is_num && n_uniq > 10L) "hist" else "bar"
  } else {
    plot_type
  }

  if (eff_type == "hist" && is_num) {
    graphics::hist(
      as.numeric(x_clean),
      breaks = max(2L, as.integer(bins)),
      col    = "#3498db",
      border = "white",
      main   = main_lbl,
      xlab   = col_name,
      las    = 1L
    )
    graphics::rug(as.numeric(x_clean), col = "#1a252f60")

  } else if (eff_type == "box" && is_num) {
    graphics::boxplot(
      as.numeric(x_clean),
      horizontal = TRUE,
      col        = "#3498db",
      border     = "#2c3e50",
      main       = main_lbl,
      xlab       = col_name
    )

  } else {
    tab     <- sort(table(as.character(x)), decreasing = TRUE)
    tab     <- utils::head(tab, 20L)
    max_lbl <- max(nchar(names(tab)), 1L)
    old_par <- graphics::par(
      mar = c(3, min(max_lbl * 0.55, 14), 2.5, 0.5)
    )
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::barplot(
      rev(tab),
      horiz  = TRUE,
      las    = 1L,
      col    = "#3498db",
      border = "white",
      xlab   = "Count",
      main   = main_lbl
    )
  }

  invisible(NULL)
}


# -- R literal formatter -------------------------------------------------------

#' Convert a scalar R value to its source-code literal representation.
#' @noRd
.vr_r_literal <- function(v) {
  if (is.na(v)) {
    if (is.integer(v))  return("NA_integer_")
    if (is.numeric(v))  return("NA_real_")
    if (is.logical(v))  return("NA")
    return("NA_character_")
  }
  if (is.logical(v))  return(if (v) "TRUE" else "FALSE")
  if (is.integer(v))  return(paste0(as.character(v), "L"))
  if (is.numeric(v))  return(as.character(v))
  paste0('"', gsub('"', '\\"', as.character(v), fixed = TRUE), '"')
}


# -- Edit code generator -------------------------------------------------------

#' Diff orig_data against current_data and emit R statements that reproduce
#' the changes (cell edits and appended rows).
#'
#' @param orig_data    original data.frame (never mutated by the app)
#' @param current_data current s$data_work
#' @param result_name  name of the result variable used in the code output
#' @return character vector of R statements (empty if no changes)
#' @noRd
.vr_gen_edit_code <- function(orig_data, current_data, result_name) {
  lines <- character(0)

  n_orig  <- nrow(orig_data)
  n_cur   <- nrow(current_data)
  n_cmp   <- min(n_orig, n_cur)
  shared  <- intersect(names(orig_data), names(current_data))

  # Cell edits on pre-existing rows
  if (n_cmp > 0L && length(shared) > 0L) {
    for (col in shared) {
      ov <- orig_data[[col]]
      cv <- current_data[[col]]
      for (i in seq_len(n_cmp)) {
        o  <- ov[i]
        cv_ <- cv[i]
        changed <- if (is.na(o) && is.na(cv_)) FALSE
                   else if (is.na(o) || is.na(cv_)) TRUE
                   else !identical(o, cv_)
        if (!changed) next
        lines <- c(lines,
          paste0(result_name, "[", i, ', "', col, '"] <- ',
                 .vr_r_literal(cv_)))
      }
    }
  }

  # Appended rows
  if (n_cur > n_orig) {
    for (i in seq(n_orig + 1L, n_cur)) {
      args <- vapply(names(current_data), function(col) {
        paste0("  `", col, "` = ", .vr_r_literal(current_data[[col]][i]))
      }, character(1))
      lines <- c(lines, paste0(
        result_name, " <- rbind(", result_name, ",\n",
        "  data.frame(\n",
        paste(args, collapse = ",\n"), ",\n",
        "  stringsAsFactors = FALSE, check.names = FALSE\n",
        "  ))"
      ))
    }
  }

  lines
}


# -- CSS -----------------------------------------------------------------------
.vr_css <- function(theme = "flatly") {
  dk <- theme %in% c("darkly", "cyborg", "slate", "solar", "superhero")

  panel_bg     <- if (dk) "#293540" else "#f8f9fa"
  panel_border <- if (dk) "#3d5165" else "#dee2e6"
  panel_hdr_c  <- if (dk) "#95a5a6" else "#6c757d"
  row_bg       <- if (dk) "#344552" else "#ffffff"
  row_border   <- if (dk) "#4a6070" else "#ced4da"
  muted_c      <- if (dk) "#8a9ba8" else "#6c757d"

  paste0("
/* -- Reset & base -- */
body { font-size: 13px !important; }
.container-fluid { padding: 0 10px !important; }

/* -- Top bar -- */
.vr-topbar {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 8px 14px;
  background: linear-gradient(135deg, #1a252f 0%, #2c3e50 100%);
  color: #ecf0f1;
  border-radius: 5px;
  margin-bottom: 12px;
  box-shadow: 0 2px 6px rgba(0,0,0,.25);
}
.vr-title {
  font-size: 15px;
  font-weight: 700;
  letter-spacing: .3px;
  flex: 1 1 auto;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.vr-status {
  font-size: 11.5px;
  opacity: .85;
  flex: 1 1 auto;
  text-align: center;
  white-space: nowrap;
}
.vr-actions { display: flex; gap: 6px; flex-shrink: 0; }

/* -- Sidebar panels -- */
.vr-sidebar { padding-right: 6px; }
.vr-panel {
  background: ", panel_bg, ";
  border: 1px solid ", panel_border, ";
  border-radius: 5px;
  padding: 8px 10px;
  margin-bottom: 10px;
}
.vr-panel-header {
  font-size: 11px;
  font-weight: 700;
  color: ", panel_hdr_c, ";
  text-transform: uppercase;
  letter-spacing: .7px;
  margin-bottom: 7px;
  display: flex;
  align-items: center;
  gap: 5px;
}
.vr-panel-header .pull-right { margin-left: auto; display:flex; gap:3px; }

/* -- Filter rows (two-row compact layout) -- */
.vr-filter-row {
  display: flex;
  flex-direction: column;
  gap: 3px;
  margin-bottom: 6px;
  padding: 6px 7px;
  background: ", row_bg, ";
  border: 1px solid ", row_border, ";
  border-radius: 4px;
}
.vr-filter-top,
.vr-filter-bottom {
  display: flex;
  flex-wrap: nowrap;
  gap: 3px;
  align-items: flex-start;
}
.vr-filter-row .form-group,
.vr-sort-row   .form-group { margin-bottom: 0 !important; }
.vr-filter-row select,
.vr-filter-row input[type=text],
.vr-sort-row   select {
  font-size: 12px !important;
  height: 28px !important;
  padding: 3px 6px !important;
}

/* -- Sort rows -- */
.vr-sort-row {
  display: flex;
  flex-wrap: nowrap;
  gap: 3px;
  margin-bottom: 5px;
  align-items: flex-start;
  padding: 5px 6px;
  background: ", row_bg, ";
  border: 1px solid ", row_border, ";
  border-radius: 4px;
}

/* -- Icon buttons -- */
.vr-icon-btn { padding: 3px 7px !important; margin-top: 1px; }

/* -- Column search input -- */
.vr-col-search {
  font-size: 12px !important;
  height: 28px !important;
  padding: 3px 8px !important;
  margin-bottom: 5px;
}

/* -- Column checkbox list -- */
.vr-col-list {
  max-height: 200px;
  overflow-y: auto;
  padding: 2px 4px;
}
.vr-col-list .checkbox { margin: 2px 0 !important; }
.vr-col-list label { font-weight: 400 !important; font-size: 12px; }

/* -- Edit toolbar -- */
.vr-edit-toolbar { display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 8px; }

/* -- Code output -- */
.vr-code-toolbar { display: flex; gap: 6px; margin-bottom: 8px; }
#vr_code_output {
  background: #282c34;
  color: #abb2bf;
  font-family: 'Fira Mono', 'Cascadia Code', 'Consolas', monospace;
  font-size: 12.5px;
  line-height: 1.6;
  min-height: 320px;
  border-radius: 5px;
  padding: 14px 16px;
  white-space: pre;
  overflow: auto;
}

/* -- Download button in topbar -- */
.vr-dl-btn {
  padding: 5px 10px !important;
  line-height: 1.4 !important;
}

/* -- Plots tab -- */
.vr-plot-summary pre {
  font-size: 11.5px;
  background: ", panel_bg, ";
  border: 1px solid ", panel_border, ";
  border-radius: 4px;
  padding: 8px 10px;
  margin-top: 8px;
}

/* -- Small helpers -- */
.mt-2  { margin-top: 6px  !important; }
.mb-0  { margin-bottom: 0 !important; }
.text-muted-sm { font-size: 11px; color: ", muted_c, "; }
.tab-content { padding-top: 4px; }
  ")
}
