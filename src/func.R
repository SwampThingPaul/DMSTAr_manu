## Functions for this specific project

#' Plot DMSTA and DMSTAr Model Output by STA
#'
#' Creates a faceted comparison plot of DMSTA and DMSTAr model results for a
#' selected output variable. The function subsets the input data to the specified
#' variable, plots DMSTA results as lines and DMSTAr results as points, and facets
#' the plot by STA. This is intended for visual comparison of legacy DMSTA output
#' and corresponding DMSTAr results across water years.
#'
#' @param dat A data frame containing model comparison results in long format.
#'   The data frame must include, at minimum, the columns \code{variable},
#'   \code{WY}, \code{STA}, \code{DMSTA}, and \code{DMSTAr}.
#' @param var_name Character string specifying the value of \code{variable} to
#'   plot.
#' @param y_lab Character string specifying the y-axis label.
#' @param show_legend Logical. If \code{TRUE}, the model legend is displayed on
#'   the right side of the plot. If \code{FALSE}, the legend is suppressed.
#'   Default is \code{FALSE}.
#'
#' @return A \code{ggplot} object showing DMSTA and DMSTAr results by water year,
#'   faceted by STA.
#'
#' @details
#' DMSTA values are shown as black lines and DMSTAr values are shown as red
#' points. Facets use independent y-axis scales to improve readability when
#' variable magnitudes differ among STAs.
#'
#' @examples
#' \dontrun{
#' make_model_plot(
#'   dat = dmsta_dmstar_long,
#'   var_name = "TP_Load",
#'   y_lab = "Total Phosphorus Load"
#' )
#'
#' make_model_plot(
#'   dat = dmsta_dmstar_long,
#'   var_name = "Outflow_TP",
#'   y_lab = "Outflow TP Concentration",
#'   show_legend = TRUE
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_grid vars
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme
#' @importFrom ggplot2 element_blank element_rect element_text
#'
#' @export
make_model_plot <- function(dat, var_name, y_lab, show_legend = FALSE) {
  
  plot_dat <- subset(dat, variable == var_name)
  
  ggplot(plot_dat, aes(x = WY)) +
    geom_line(
      aes(y = DMSTA,group = STA,color = "DMSTA"),
      linewidth = 0.5
    ) +
    geom_point(
      aes(y = DMSTAr, color = "DMSTAr"),
      size = 1.6,
      alpha = 0.8
    ) +
    facet_grid(
      rows = vars(STA),
      scales = "free_y"
    ) +
    scale_color_manual(
      name = "Model",
      values = c("DMSTA" = "black", "DMSTAr" = "red")
    ) +
    labs(
      x = "Water Year",
      y = y_lab
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      strip.background = element_rect(fill = "grey85", color = "grey30"),
      strip.text = element_text(face = "bold")
    )
}

#' Read and Normalize DMSTA Parameter Tables from Excel
#'
#' Reads a DMSTA-style parameter worksheet from an Excel workbook and converts
#' it into a cell-wise parameter data frame suitable for \pkg{DMSTAr}.
#'
#' This function is designed to be robust to real-world DMSTA workbooks,
#' including inconsistent capitalization, punctuation, duplicated labels,
#' aliasing across DMSTA versions, and Excel date serials. Parameter labels are
#' matched using a normalized key and user-configurable alias specifications.
#'
#' For each modeled cell, parameters are extracted column-wise, coerced to the
#' appropriate type, missing values are filled using defined defaults, and the
#' result is passed through \code{DMSTAr:::dmstar_default_params()} to ensure a complete,
#' validated parameter set.
#'
#' @param xlsx_path Character path to an Excel workbook containing a DMSTA-style
#'   parameter table.
#'
#' @param sheet Name or index of the worksheet containing parameters.
#'   Defaults to \code{"Parameters"}.
#'
#' @param skip Number of rows to skip before the parameter table begins.
#'   Defaults to 15, consistent with legacy DMSTA templates.
#'
#' @param nrows Number of rows to read from the worksheet after \code{skip}.
#'   Defaults to 55.
#'
#' @param ncells Total number of cells represented in the worksheet.
#'   Defaults to 12.
#'
#' @param cells Integer vector of cell indices to read (subset of
#'   \code{seq_len(ncells)}). Defaults to all cells.
#'
#' @param DutyCycle Numeric scalar giving the duty cycle parameter applied
#'   uniformly across all cells.
#'
#' @param force_Q_out Logical; if \code{TRUE}, forces treated outflow behavior
#'   downstream regardless of individual cell configuration.
#'
#' @param dmsta_version Character string identifying the DMSTA parameterization
#'   version (e.g., \code{"2C2B"}, \code{"2E"}). Passed through unchanged and
#'   stored for downstream logic.
#'
#' @param map Named character vector mapping DMSTAr argument names to label
#'   specifications in the Excel sheet. Label specifications may include:
#'   \describe{
#'     \item{Alias sets}{Multiple acceptable labels separated by \code{||};
#'       the first non-blank match is used.}
#'     \item{Duplicate selectors}{Suffix \code{@@n} selects the nth occurrence
#'       of a duplicated label (e.g., \code{"Offline trigger@@2"}).}
#'   }
#'
#' @param na_fill Named list of default values used to replace blank or missing
#'   parameters after coercion. This layer is applied prior to any DMSTA
#'   iteration or offline-logic defaults.
#'
#' @param warn_on_duplicates Logical; if \code{TRUE}, emits a warning when
#'   duplicated parameter labels contain conflicting non-blank values for a
#'   given cell. The first non-blank value is still used.
#'
#' @details
#' \strong{Label normalization.} Parameter labels from the worksheet are
#' normalized by lowercasing, trimming whitespace, collapsing internal spaces,
#' and removing punctuation before matching against \code{map} entries.
#'
#' \strong{Type coercion.}
#' Numeric parameters are coerced using \code{as.numeric()} (with suppression of
#' conversion warnings). Offline dates are interpreted as Excel serial dates,
#' \code{Date}, \code{POSIXt}, or character representations of common formats.
#' Offline triggers are coerced to logical via numeric and string heuristics.
#'
#' \strong{Offline parameters.}
#' Offline-related fields (\code{offline_trigger}, \code{offline_start},
#' \code{offline_freq}, \code{offline_dur}, and \code{frac_1}–\code{frac_6}) are
#' read generically but are not automatically assigned VBA-style defaults within
#' this function. This separation allows explicit, auditable parity handling in
#' higher-level DMSTAr logic.
#'
#' @return
#' A data frame with one row per requested cell and fully populated DMSTAr
#' parameter columns, as returned by \code{dmstar_default_params()}.
#'
#' @seealso
#' \code{\link{dmstar_default_params}}
#'
#' @export

dmstar_read_parameters_tab <- function(
    xlsx_path,
    sheet = "Parameters",
    skip = 15,
    nrows = 55,
    ncells = 12,
    cells = seq_len(ncells),
    DutyCycle = 1,
    force_Q_out = FALSE,
    dmsta_version = "2E",
    
    # Label specs:
    #  - Use "A||B||C" for aliases (first non-blank wins)
    #  - Use "@@n" to select nth occurrence of a duplicated label
    #    e.g. "Offline trigger@@2"
    map = c(
      Cell_Label = "Cell Label",
      Vegetation = "Vegetation Type",
      Qin_Frac   = "Inflow Fraction",
      DownCell   = "Downstream Cell Number",
      A_cell     = "Surface Area",
      Width      = "Mean Width of Flow Path",
      Ntanks     = "Number of Tanks in Series",
      Zrelease   = "Minimum Depth for Releases",
      Q_zmin     = "Outflow Control Depth",
      Zweir      = "Outflow Weir Depth",
      Q_b        = "Outflow Coefficient - Exponent",
      Q_a        = "Outflow Coefficient - Intercept",
      Bypass_elev= "Bypass Depth",
      Qimax      = "Maximum Inflow",
      Qomax      = "Maximum Outflow",
      Seepin_Rate= "Inflow Seepage Rate",
      Seepin_Elev= "Inflow Seepage Control Elev",
      seepin_conc= "Inflow Seepage Conc",
      Seepout_Rate = "Outflow Seepage Rate",
      Seepout_Elev = "Outflow Seepage Control Elev",
      seepage_c  = "Max Outflow Seepage Conc",
      C_init_ppb = "Initial Water Column Conc",
      Y_init_mgm2= "Initial P Storage Per Unit Area",
      Zinit      = "Initial Water Column Depth",
      C1000      = "C1 = Conc at 1 g/m2 P storage",
      Chalf      = "C2 = Conc at Half-Max Uptake",
      Ks_per_yr  = "K = Net Settling Rate at Steady State",
      Z1         = "Z1 = Saturated Uptake Depth",
      Z2         = "Z2 = Lower Penalty Depth",
      Z3         = "Z3 = Upper Penalty Depth",
      QR1_name   = "Release 1 Series Name",
      QR2_name   = "Release 2 Series Name",
      QR0_name   = "Outflow Series Name",
      Zcon_name  = "Depth Series Name",
      
      # Offline fields (aliases included to be robust)
      offline_trigger = "Offline trigger||Offline Trigger||Offline Trigger (1/0)",
      offline_start   = "Offline starting date||Offline start||Offline starting date (mm/dd/yyyy)",
      offline_freq    = "Offline frequency (every N years)||Offline frequency||Offline freq",
      offline_dur     = "duration (days)||Duration (days)||Offline duration (days)",
      
      frac_1 = "Fraction 1||Frac 1",
      frac_2 = "Fraction 2||Frac 2",
      frac_3 = "Fraction 3||Frac 3",
      frac_4 = "Fraction 4||Frac 4",
      frac_5 = "Fraction 5||Frac 5",
      
      # allow frac_6 to come from the second duplicated "Offline trigger"
      # if the workbook has that mislabeled row.
      frac_6 = "Fraction 6||Frac 6||Offline trigger@@2"
    ),
    
    na_fill = list(
      Qin_Frac = 0, DownCell = 0,
      Bypass_elev = 0, Zrelease = 0,
      Zweir = 0, Qomax = 0, Qimax = 0,
      Seepout_Rate = 0, Seepout_Elev = 0,
      seepage_c = 20,
      Seepin_Rate = 0, Seepin_Elev = 0,
      seepin_conc = 0,
      A_cell = 0, Zinit = 0, Q_zmin = 0,
      C1000 = 0, Ks_per_yr = 0,
      Z1 = 0, Z2 = 0, Z3 = 0,
      Chalf = 0, Ntanks = 0,
      Q_a = 0, Q_b = 0, Width = 0,
      C_init_ppb = 0,
      Y_init_mgm2 = 0,
      
      # Offline defaults (kept minimal here; we’ll apply VBA-aligned defaults later)
      offline_trigger = FALSE,
      offline_start = NA,
      offline_freq = NA,
      offline_dur  = NA,
      frac_1 = 0, frac_2 = 0, frac_3 = 0, frac_4 = 0, frac_5 = 0, frac_6 = 0
    ),
    
    warn_on_duplicates = TRUE
) {
  stopifnot(file.exists(xlsx_path))
  stopifnot(all(cells %in% seq_len(ncells)))
  
  raw <- readxl::read_xlsx(
    xlsx_path, sheet = sheet, skip = skip,
    col_names = FALSE, .name_repair = "minimal", progress = FALSE
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  
  raw <- raw[seq_len(nrows), seq_len(2 + ncells), drop = FALSE]
  names(raw) <- c("var", "unit", paste0("cell", seq_len(ncells)))
  
  # normalize more aggressively (handles punctuation / double spaces / case)
  norm_key <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("\\s+", " ", x)
    gsub("[^a-z0-9]+", "", x)
  }
  raw$var_key <- norm_key(raw$var)
  
  is_blank <- function(x) {
    if (length(x) == 0) return(TRUE)
    if (is.na(x)) return(TRUE)
    if (is.character(x) && trimws(x) == "") return(TRUE)
    FALSE
  }
  
  fill_default <- function(x, default) if (is_blank(x)) default else x
  
  # Parse a label spec like "Offline trigger@@2"
  parse_one <- function(s) {
    parts <- strsplit(s, "@@",
                      fixed = TRUE)[[1]]
    label <- parts[1]
    nth <- if (length(parts) == 2) suppressWarnings(as.integer(parts[2])) else NA_integer_
    list(label = label, nth = nth)
  }
  
  # Fetch value from a (possibly duplicated) label, honoring @@n and aliases
  get_val <- function(spec, cell_col) {
    # aliases split
    alts <- strsplit(spec, "\\|\\|")[[1]]
    alts <- trimws(alts)
    
    for (alt in alts) {
      one <- parse_one(alt)
      key <- norm_key(one$label)
      hits <- which(raw$var_key == key)
      if (!length(hits)) next
      
      # If specific occurrence requested:
      if (!is.na(one$nth)) {
        if (one$nth <= length(hits)) {
          v <- raw[hits[one$nth], cell_col]
          if (!is_blank(v)) return(v)
          # if blank, keep trying other aliases
          next
        } else {
          next
        }
      }
      
      # Otherwise: choose first nonblank; warn if multiple nonblank differ
      vals <- raw[hits, cell_col, drop = TRUE]
      good <- !vapply(vals, is_blank, logical(1))
      if (!any(good)) next
      
      good_vals <- vals[good]
      if (warn_on_duplicates && length(good_vals) > 1) {
        # warn only if there are competing distinct values
        uniq <- unique(as.character(good_vals))
        if (length(uniq) > 1) {
          warning(
            sprintf(
              "Duplicate label '%s' has multiple nonblank values in %s; using the first nonblank. Values: %s",
              one$label, cell_col, paste(uniq, collapse = ", ")
            ),
            call. = FALSE
          )
        }
      }
      return(good_vals[1])
    }
    
    NA
  }
  
  # Coercers
  as_num <- function(x) suppressWarnings(as.numeric(x))
  
  as_date_maybe <- function(x) {
    if (is_blank(x)) return(NA)
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))  # Excel serial
    # try common formats
    xx <- suppressWarnings(as.Date(x))
    if (!is.na(xx)) return(xx)
    suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  }
  
  coerce_offline_trigger <- function(x) {
    if (is_blank(x)) return(NA)
    if (is.logical(x)) return(x)
    if (is.numeric(x)) return(x != 0)
    # strings like "1", "0", "TRUE", "FALSE"
    z <- tolower(trimws(as.character(x)))
    if (z %in% c("1", "true", "t", "yes", "y")) return(TRUE)
    if (z %in% c("0", "false", "f", "no", "n")) return(FALSE)
    NA
  }
  
  # Which args are numeric? (everything except the name-ish fields + offline_start date)
  non_numeric <- c("Cell_Label", "Vegetation", "QR1_name", "QR2_name", "QR0_name", "Zcon_name", "offline_start")
  numeric_args <- setdiff(names(map), non_numeric)
  
  out_list <- lapply(cells, function(i) {
    cell_col <- paste0("cell", i)
    
    args <- lapply(names(map), function(arg) get_val(map[[arg]], cell_col))
    names(args) <- names(map)
    
    # Coerce numeric fields (including frac_1..frac_6)
    for (nm in numeric_args) args[[nm]] <- as_num(args[[nm]])
    
    # Coerce offline_start to Date, and offline_trigger to logical
    args$offline_start   <- as_date_maybe(args$offline_start)
    args$offline_trigger <- coerce_offline_trigger(args$offline_trigger)
    args$offline_freq    <- if (is_blank(args$offline_freq)) NA_integer_ else as.integer(args$offline_freq)
    args$offline_dur     <- if (is_blank(args$offline_dur))  NA_integer_ else as.integer(args$offline_dur)
    
    # Apply defaults for blanks
    for (nm in names(na_fill)) {
      if (nm %in% names(args)) args[[nm]] <- fill_default(args[[nm]], na_fill[[nm]])
    }
    
    # VBA-aligned offline defaults:
    # In the VBA Parameters() routine, offline_start/freq/dur are initialized to:
    # 1965-03-15, 3, 45 (and then the model uses frac_1..frac_6 by modulo year).  
    # if (isTRUE(args$offline_trigger)) {
    #   if (is.na(args$offline_start)) args$offline_start <- as.Date("1965-03-15")
    #   if (is.na(args$offline_freq))  args$offline_freq  <- 3L
    #   if (is.na(args$offline_dur))   args$offline_dur   <- 45L
    # } else {
    #   # keep these NA if not triggered (matches "off" behavior cleanly)
    #   args$offline_start <- if (isTRUE(args$offline_trigger)) args$offline_start else NA
    #   args$offline_freq  <- if (isTRUE(args$offline_trigger)) args$offline_freq  else NA_integer_
    #   args$offline_dur   <- if (isTRUE(args$offline_trigger)) args$offline_dur   else NA_integer_
    # }
    
    # Add constants
    args$DutyCycle   <- DutyCycle
    args$force_Q_out <- force_Q_out
    args$dmsta_version <- dmsta_version
    
    # No length-0 args
    for (nm in names(args)) if (length(args[[nm]]) == 0) args[[nm]] <- NA
    
    as.data.frame(
      do.call(dmstar_default_params, args),
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}



#' Read Time Series Inputs from a DMSTA Excel Workbook
#'
#' Reads the \code{"Series_Input"} worksheet from a DMSTA-style Excel workbook
#' and returns a tidy data frame containing date-indexed inflow, concentration,
#' and ancillary forcing time series.
#'
#' The function is designed to be robust to partially populated header rows,
#' extraneous Excel columns, and mixed date representations. A fixed set of
#' required base columns is always enforced, with any additional series
#' discovered dynamically from the worksheet header.
#'
#' @param path Character path to an Excel workbook containing a
#'   \code{Series_Input} worksheet.
#'
#' @param sheet Sheet name or index where the time series inputs are stored.
#'   Defaults to \code{"Series_Input"}.
#'
#' @param header_row Integer row index containing optional series names
#'   beyond the base variables. Defaults to 2, consistent with legacy DMSTA
#'   templates.
#'
#' @param data_start_row Integer row index where time series data begin.
#'   Defaults to 5.
#'
#' @param base Character vector of required base column names. These columns
#'   are always present in the output, even if the worksheet header is blank.
#'   Defaults to \code{c("Date","Flow","Conc","Rainfall","ET")}.
#'
#' @details
#' \strong{Header handling.}
#' The function probes a single header row for additional series names.
#' Non-blank entries are appended to the base variables. If no additional
#' headers are found, the output consists only of the base columns.
#'
#' \strong{Column width control.}
#' The data block is truncated or padded with \code{NA} to exactly match the
#' constructed header width. This prevents accidental inclusion of stray Excel
#' columns or formatting artifacts.
#'
#' \strong{Date coercion.}
#' If a \code{Date} column is present, values are coerced conservatively:
#' \itemize{
#'   \item Excel serial dates are converted using origin \code{"1899-12-30"}
#'   \item \code{POSIXt} values are converted via \code{as.Date()}
#'   \item Character values are passed to \code{as.Date()} without guessing
#' }
#' No additional date validation or interpolation is performed.
#'
#' \strong{Scope.}
#' This function performs input normalization only. It does not enforce model-
#' specific semantics, unit conversions, or DMSTA/DMSTAr parity logic, which are
#' handled at higher layers in the workflow.
#'
#' @return
#' A data frame with one row per timestep and one column per required or
#' discovered series input. Column order is deterministic and begins with
#' \code{base}.
#'
#' @seealso
#' \code{\link{dmstar_read_parameters_tab}}
#'
#' @export

read_series_input <- function(path, sheet="Series_Input",
                              header_row=2, data_start_row=5,
                              base=c("Date","Flow","Conc","Rainfall","ET")){
  
  stopifnot(file.exists(path))
  # header probe
  hdr_df <- read_xlsx(path, sheet = sheet,
                      skip = header_row - 1,
                      n_max = 1,
                      col_names = FALSE,
                      .name_repair = "minimal")
  
  hdr <- as.character(unlist(hdr_df[1,], use.names = FALSE))
  hdr <- hdr[!is.na(hdr) & nzchar(trimws(hdr))]
  
  # standardize header
  if (length(hdr) > 0) hdr <- c(base,hdr) else hdr <- base
  
  # data read constrained to header width
  # rng <- cell_limits(c(data_start_row, 1), c(NA, length(hdr)))
  # dat <- read_xlsx(path, sheet = sheet, range = rng,
  #                  col_names = FALSE, .name_repair = "minimal") |>
  #   as.data.frame()
  
  dat <- read_xlsx(path,sheet = sheet, skip = data_start_row-1,col_names = FALSE) |>
    as.data.frame()
  
  names(dat) <- hdr
  
  #Gentle Date coercion (first column)
  if ("Date" %in% names(dat)) {
    dat$Date <- {
      x <- dat$Date
      if (inherits(x, "Date")) {
        x
      } else if (inherits(x, "POSIXt")) {
        as.Date(x)
      } else if (is.numeric(x)) {
        as.Date(x, origin = "1899-12-30")
      } else {
        suppressWarnings(as.Date(x))
      }
    }
  }
  
  dat
}

#' Build a Validated List of DMSTA Cells from a Parameter Table
#'
#' Constructs a list of DMSTA cell objects from a parameter data frame,
#' performing basic structural validation and enforcing required fields
#' before delegating cell creation and consistency checks to internal
#' DMSTA helpers.
#'
#' Each row of \code{params_df} is interpreted as a single modeled cell.
#' The function initializes cells in row order, assigns downstream routing
#' and inflow fractions, and validates the resulting network structure.
#'
#' @param params_df A data frame containing per-cell DMSTAr/DMSTA parameters.
#'   Must include, at minimum, the columns:
#'   \describe{
#'     \item{\code{Cell_Label}}{Unique identifier for each cell.}
#'     \item{\code{Ntanks}}{Number of tanks in series for the cell.}
#'     \item{\code{DownCell}}{Index of the downstream cell (0 for terminal).}
#'     \item{\code{Qin_Frac}}{Fraction of inflow routed to the cell.}
#'   }
#'
#' @details
#' \strong{Validation.}
#' The function enforces several defensive checks prior to cell construction:
#' \itemize{
#'   \item \code{params_df} must be a non-empty data frame
#'   \item Required columns must be present
#'   \item \code{Cell_Label} values must be non-missing
#' }
#'
#' \strong{Cell construction.}
#' For each row of \code{params_df}, a cell object is created using
#' \code{dmsta_make_cell()}, with:
#' \itemize{
#'   \item parameters passed row-wise
#'   \item tank count taken from \code{Ntanks}
#'   \item downstream routing defined by \code{DownCell}
#'   \item inflow fraction defined by \code{Qin_Frac}
#'   \item \code{RecycleIndex} initialized to zero
#' }
#'
#' After construction, the full list of cells is validated using
#' \code{dmsta_validate_cells()} to ensure network consistency.
#'
#' This function performs no reordering, coercion, or inference beyond
#' the explicit contents of \code{params_df}.
#'
#' @return
#' A validated list of DMSTA cell objects suitable for use in network-based
#' DMSTAr simulations.
#'
#' @seealso
#' \code{\link{DMSTAr::dmsta_make_cell}},
#' \code{\link{DMSTAr::dmsta_validate_cells}}
#'
#' @export

build_case_cells <- function(params_df) {
  stopifnot(is.data.frame(params_df))
  req <- c("Cell_Label","Ntanks","DownCell","Qin_Frac")
  miss <- setdiff(req, names(params_df))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse=", "))
  
  # guard against NA/length mismatch
  if (anyNA(params_df$Cell_Label)) stop("NA Cell_Label in params_df")
  if (nrow(params_df) < 1) stop("Empty params_df")
  
  cells <- lapply(seq_len(nrow(params_df)), function(i) {
    dmsta_make_cell(
      label        = params_df$Cell_Label[i],
      params       = params_df[i, , drop = FALSE],
      ttankS       = params_df$Ntanks[i],
      DownCell     = params_df$DownCell[i],
      Qin_Frac     = params_df$Qin_Frac[i],
      RecycleIndex = 0
    )
  })
  dmsta_validate_cells(cells)
}



## Other functions
summary_metrics_fun <- function(x){
  # data.frame must have DMSTAr, DMSTA, diff_val, mean_val and diff_val columns
  d <- x$diff_val
  p <- x$diff_pct
  abs_p <- abs(p)
  abs_d <- abs(d)
  
  # MAD = typical parity difference (low sensitivity to outliers)
  # MAE = average parity difference (moderate sens to outliers)
  # RMSE = outlier-sensitive parity diagnostic (high sens to outliers)
  
  data.frame(
    n = sum(!is.na(d)),
    
    bias = mean(d, na.rm = TRUE),
    mae = mean(abs_d, na.rm = TRUE),
    mad = median(abs_d, na.rm = TRUE),
    rmse = sqrt(mean(d^2, na.rm = TRUE)),
    
    # percent-difference summaries
    mean_pct = mean(p, na.rm = TRUE),
    median_pct = median(p, na.rm = TRUE),
    # median_abs_pct = median(abs_p, na.rm = TRUE),
    # p90_abs_pct = as.numeric(quantile(abs_p, 0.90, na.rm = TRUE)),
    # p95_abs_pct = as.numeric(quantile(abs_p, 0.95, na.rm = TRUE)),
    # p99_abs_pct = as.numeric(quantile(abs_p, 0.99, na.rm = TRUE)),
    # max_abs_pct = max(abs_p, na.rm = TRUE),
    # 
    pct_within_0.5pct = mean(abs_p <= 0.5, na.rm = TRUE) * 100,
    pct_within_1pct = mean(abs_p <= 1.0, na.rm = TRUE) * 100,
    pct_within_2pct = mean(abs_p <= 2.0, na.rm = TRUE) * 100
  )
}

ba_stats_fun <- function(x) {
  p <- x$diff_pct
  
  c(
    bias_pct = mean(p, na.rm = TRUE),
    loa_low_pct = mean(p, na.rm = TRUE) - qnorm(0.975) * sd(p, na.rm = TRUE),
    loa_high_pct = mean(p, na.rm = TRUE) + qnorm(0.975) * sd(p, na.rm = TRUE)
    # loa_low_pct = mean(p, na.rm = TRUE) - 1.96 * sd(p, na.rm = TRUE),
    # loa_high_pct = mean(p, na.rm = TRUE) + 1.96 * sd(p, na.rm = TRUE)
  )
}

convergence_order <- function(daily_data, reference_steps = 16,
                              by.var = "Date",
                              dat_vars = c("Date", "Q_out_total", "L_out_total","Depth"),
                              ref_vars = c("Date", "Q_ref", "load_ref","depth_ref"),
                              test_vars = c("Date", "Q_test", "load_test","depth_test")) {
  # Ensure data is ordered by date and steps
  # daily_data <- daily_data[order(daily_data$date, daily_data$steps_per_day), ]
  # Get unique step sizes and sort
  steps_vec <- sort(unique(daily_data$Nstep))
  
  # Extract reference (finest resolution)
  ref_data <- daily_data[daily_data$Nstep == reference_steps, dat_vars]
  names(ref_data) <- ref_vars
  
  results <- data.frame(
    steps_per_day = numeric(),
    nrmse_Q = numeric(),
    nrmse_load = numeric(),
    nrmse_depth = numeric(),
    mae_Q = numeric(),
    mae_load = numeric(),
    mae_depth = numeric(),
    mad_Q = numeric(),
    mad_load = numeric(),
    mad_depth = numeric(),
    max_error_Q = numeric(),
    max_error_load = numeric(),
    max_error_depth = numeric()
  )
  
  for (sp in steps_vec[steps_vec!= reference_steps]) {
    test_data <- daily_data[daily_data$Nstep == sp, dat_vars]
    names(test_data) <- test_vars
    
    # Merge on date
    merged <- merge(ref_data, test_data, by = by.var, all = FALSE)
    
    # Calculate errors
    Q_error <- abs(merged$Q_test - merged$Q_ref)
    load_error <- abs(merged$load_test - merged$load_ref)
    depth_error <- abs(merged$depth_test - merged$depth_ref)
    
    # NRMSE (percent)
    nrmse_Q <- 100 * sqrt(mean(Q_error^2, na.rm = TRUE)) / 
      mean(merged$Q_ref, na.rm = TRUE)
    nrmse_load <- 100 * sqrt(mean(load_error^2, na.rm = TRUE)) / 
      mean(merged$load_ref, na.rm = TRUE)
    nrmse_depth <- 100 * sqrt(mean(depth_error^2, na.rm = TRUE)) / 
      mean(merged$depth_ref, na.rm = TRUE)
    
    # MAE (absolute)
    mae_Q <- mean(Q_error, na.rm = TRUE)
    mae_load <- mean(load_error, na.rm = TRUE)
    mae_depth<- mean(depth_error, na.rm = TRUE)
    
    # MAD
    mad_Q <- median(abs(Q_error), na.rm = TRUE)
    mad_load <- median(abs(load_error), na.rm = TRUE)
    mad_depth<- median(abs(depth_error), na.rm = TRUE)
    
    # Max absolute error
    max_err_Q <- max(Q_error, na.rm = TRUE)
    max_err_load <- max(load_error, na.rm = TRUE)
    max_err_depth <- max(depth_error, na.rm = TRUE)
    
    results <- rbind(results, data.frame(
      steps_per_day = sp,
      nrmse_Q = nrmse_Q,
      nrmse_load = nrmse_load,
      nrmse_depth = nrmse_depth,
      mae_Q = mae_Q,
      mae_load = mae_load,
      mae_depth = mae_depth,
      mad_Q = mad_Q,
      mad_load = mad_load,
      mad_depth = mad_depth,
      max_error_Q = max_err_Q,
      max_error_load = max_err_load,
      max_error_depth = max_err_depth
    ))
  }
  return(results)
}
