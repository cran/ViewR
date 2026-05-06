# ViewR 1.0.0

## New features

* **Plots tab**: auto-detected visualisations for every column — histograms
  (with rug) for continuous numerics, bar charts for categorical and
  low-cardinality variables, and boxplots on demand. Configurable bin count
  and a printed `summary()` panel accompany each plot.

* **Drag-and-drop column reordering**: columns in the Data View table can be
  rearranged by dragging their headers (powered by the DataTables 'ColReorder'
  extension).

* **Copy Code button in the top bar**: one-click clipboard copy of the
  generated 'dplyr' pipeline, placed alongside Done / Cancel for quick access.

* **Download CSV button in the top bar**: exports the currently displayed
  (filtered, sorted, and column-selected) data as a timestamped CSV file.

* **Automatic Find & Replace preview**: the preview table now updates
  automatically as you type — the separate Preview button has been removed.

* **Dynamic rows-per-page slider**: the maximum value now matches the actual
  number of rows in the data rather than a fixed cap of 500.

* **Column search in the sidebar**: a search box above the column checkbox
  list lets you quickly locate columns in wide data frames.

* **Smart filter value input**: the value field is hidden automatically when
  the selected operator is *is NA* or *is not NA*, reducing visual clutter.

* **Improved filter row layout**: each filter condition now uses a compact
  two-row layout (column + logic connector on top; operator + value below)
  that fits comfortably in the narrow sidebar.

## Internal changes

* Removed `install_viewr_deps()` (dependencies are declared in `DESCRIPTION`
  and installed automatically by R).
* `Language: en-GB` declared in `DESCRIPTION`; `inst/WORDLIST` added for
  technical terms.
* `.Rbuildignore` updated to exclude `.claude/` from the source tarball.

---

# ViewR 0.2.0

## New features

* **Excel-like editor** (`edit = TRUE`): powered by `rhandsontable`, supports
  inline cell editing, row addition via right-click context menu or the
  *Add Row* button, and unlimited undo / redo.

* **Find & Replace tab**: search for a literal string, regex pattern, or
  exact cell value across one or all columns. A live *Preview* diff table
  shows matched cells before changes are committed.

* **Variable Info tab**: one-row-per-column summary of data type, N,
  missing count, missing %, unique count, min, max, and sample values.

* **R Code tab** (`generate_code = TRUE`): live `dplyr` pipeline that
  reflects all current filters, sorts, column selections, and
  find-and-replace operations. *Copy to clipboard* button included.

* **Multi-condition Filters** with AND / OR logic and eleven operators:
  `==`, `!=`, `>`, `>=`, `<`, `<=`, *contains*, *starts with*,
  *ends with*, *is NA*, *is not NA*.

* **Multi-column Sort**: add as many ordered sort levels as needed, each
  independently ascending or descending.

* **Column visibility panel**: show / hide any column; bulk *All* / *None*
  toggles.

* **Variable labels**: automatically read from column `"label"` attributes
  (e.g. from `haven::read_spss()`); displayed as hoverable tooltips on
  column headers in the Data View table.

* **Twelve Bootstrap themes** via `shinythemes`: `"flatly"`, `"cerulean"`,
  `"cosmo"`, `"darkly"`, `"lumen"`, `"paper"`, `"readable"`,
  `"sandstone"`, `"simplex"`, `"spacelab"`, `"united"`, `"yeti"`.

* `install_viewr_deps()`: helper to detect and install all required
  packages in one call.

* Pipe-friendly: `ViewR()` accepts piped input and returns the working
  data frame invisibly when the user clicks *Done*.

## Bug fixes

* Type coercion after find-and-replace no longer depends on `methods::as()`;
  replaced with an explicit `switch()` over the original column class.

## Internal changes

* Separated UI, server, helpers, and code-generation concerns into
  dedicated source files (`R/ui.R`, `R/server.R`, `R/helpers.R`,
  `R/code_gen.R`).
* Full `testthat` (edition 3) test suite covering filter engine, sort
  engine, find-and-replace, variable info, and code generation.
* Added `vignettes/ViewR-intro.Rmd` introduction vignette.
* Added `_pkgdown.yml` for the package website.
* Hex sticker added to `inst/sticker/`.

---

# ViewR 0.1.0

* Initial release: basic `ViewR()` stub.
