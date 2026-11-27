#' DreamAI Ensemble Imputation
#'
#' Impute missing abundances with the mean of the selected
#' [DreamAI](https://github.com/WangLab-MSSM/DreamAI) methods. The default
#' uses all six methods. Select a single method to use its estimates directly.
#'
#' @inheritParams impute_bpca
#' @param by Optional column name in `colData` (`sample_info` for legacy
#'   experiments), or a grouping vector with one value per sample. Each
#'   group is imputed separately.
#' @param method Character vector of DreamAI methods to include in the ensemble.
#'   Choose from `"KNN"`, `"MissForest"`, `"ADMIN"`, `"Birnn"`, `"SpectroFM"`,
#'   and `"RegImpute"`.
#' @param ... Named arguments passed to DreamAI, such as `k` for the number
#'   of neighbors, `ntree` for the number of MissForest trees, or
#'   `iter_SpectroFM` for the number of SpectroFM iterations.
#' @param seed Non-negative integer seed for each fit. Defaults to `123`.
#'
#' @details DreamAI requires `devtools` and Bioconductor's `impute` at run time.
#'   Install `impute` with `BiocManager::install("impute")`, then install
#'   DreamAI with `devtools::install_github("WangLab-MSSM/DreamAI/Code")`.
#'   glyclean calls the installed package in a separate R session when a group
#'   has missing values.
#'
#'   DreamAI fits features in rows and samples in columns, on the log2
#'   abundance scale. Observed raw abundances must be positive. For `logx`
#'   input it uses the supplied coordinates directly. For log-ratios, pass
#'   `gamma_ADMIN = 0` when using ADMIN. Each feature and sample needs at
#'   least one observation within each group. KNN standardizes features by
#'   their within-group standard deviation. Use [remove_rare()] or
#'   [remove_constant()] to filter sparse or constant features before fitting.
#'   For KNN and ADMIN, `k` must be smaller than the number of features.
#'   RegImpute needs at least three samples per fit. SpectroFM requires a
#'   C++ compiler, at least 101 observed entries and 11 missing entries per fit.
#'   During fitting, Birnn treats zeros as missing and ADMIN truncates negative
#'   regression estimates to zero.
#'
#' @references Ma W, Kim S, Chowdhury S, et al. (2021).
#'   *DreamAI: algorithm for the imputation of proteomics data*. bioRxiv
#'   preprint, revised May 19, 2021. \doi{10.1101/2020.07.21.214205}.
#'
#'   The authors maintain the [original source code](https://github.com/WangLab-MSSM/DreamAI).
#'
#' @return A container of the same class and abundance scale as `x`, with
#'   estimates written only to the missing entries.
#' @examplesIf rlang::is_installed(c("DreamAI", "callr", "devtools", "impute"))
#' abundance <- matrix(c(2, 3, 5, 8, 4, 6, NA, 16, 8, 12, 20, 32), nrow = 4)
#' x <- SummarizedExperiment::SummarizedExperiment(assays = list(abundance = abundance))
#' filled <- impute_dreamai(x, method = "KNN", k = 2)
#' SummarizedExperiment::assay(filled)
#' @seealso [data_scales], [glyclean_pipeline()]
#' @export
impute_dreamai <- function(
  x,
  by = NULL,
  method = c("KNN", "MissForest", "ADMIN", "Birnn", "SpectroFM", "RegImpute"),
  ...,
  seed = 123
) {
  record <- .pipeline_recorder(x)
  checkmate::assert_int(seed, lower = 0)
  checkmate::assert_character(method, min.len = 1L)
  method <- rlang::arg_match(method, multiple = TRUE)
  args <- list(...)
  if (length(args) && !rlang::is_named(args)) {
    cli::cli_abort(
      "Supply DreamAI tuning arguments by name, for example {.code k = 5}."
    )
  }
  if (any(c("data", "out") %in% names(args))) {
    cli::cli_abort(
      "Use {.arg x} for the input and {.arg method} to select the DreamAI estimates."
    )
  }
  record({
    mat <- .apply_to_expr_mat(
      x,
      .impute_dreamai,
      by = by,
      scale = .get_scale(x),
      method = method,
      seed = seed,
      args = args
    )
    if (inherits(x, "glyexp_experiment")) {
      x$expr_mat <- mat
    } else {
      x <- .ensure_scale_metadata(x)
      SummarizedExperiment::assay(x) <- mat
    }
    x
  })
}

.impute_dreamai <- function(mat, scale, method, seed, args) {
  work <- .to_work_scale(as.matrix(mat), scale, "log2", allow_logx = TRUE)
  .assert_finite_observed(work)
  missing <- is.na(work)
  if (!any(missing)) {
    return(mat)
  }
  imputed <- .dreamai_ensemble(work, method, seed, args)
  checkmate::assert_matrix(
    imputed,
    mode = "numeric",
    nrows = nrow(work),
    ncols = ncol(work)
  )
  estimates <- .from_work_scale(imputed[missing], scale, "log2")
  if (any(!is.finite(estimates))) {
    cli::cli_abort(
      "DreamAI could not produce finite estimates for all missing values."
    )
  }
  mat[missing] <- estimates
  mat
}

.dreamai_ensemble <- function(data, method, seed, args) {
  package <- "DreamAI"
  if (!rlang::is_installed(package)) {
    cli::cli_abort(
      c(
        "{.fn impute_dreamai} requires a separately installed {.pkg DreamAI} package.",
        "i" = 'Install it with {.code devtools::install_github("WangLab-MSSM/DreamAI/Code")}.',
        "i" = 'Install the Bioconductor package {.pkg impute} with {.code BiocManager::install("impute")}.'
      ),
      class = "glyclean_dreamai_not_installed"
    )
  }
  .check_dreamai_input(data, method, args)
  rlang::check_installed(
    "callr",
    reason = "to run DreamAI in a separate R session"
  )

  # DreamAI uses feature and sample names in formulas and in text files.
  dimnames(data) <- list(
    paste0("F", seq_len(nrow(data))),
    paste0("S", seq_len(ncol(data)))
  )
  if ("SpectroFM" %in% method) {
    # DreamAI 0.1.0 forwards this argument to libFM, whose own RNG uses a
    # wall-clock seed by default. Pass its native seed along with the iteration count.
    iter <- args[["iter_SpectroFM"]]
    if (is.null(iter)) {
      iter <- 40L
    }
    checkmate::assert_int(iter, lower = 1L, .var.name = "iter_SpectroFM")
    args$iter_SpectroFM <- paste(as.integer(iter), "-seed", as.integer(seed))
  }

  # DreamAI attaches packages, changes options and opens sinks in its working directory.
  callr::r(
    function(package, args, seed) {
      set.seed(seed)
      fit <- getExportedValue(package, "DreamAI")
      do.call(fit, args)[["Ensemble"]]
    },
    args = list(
      package = package,
      args = c(list(data = data, method = method, out = "Ensemble"), args),
      seed = seed
    ),
    wd = withr::local_tempdir(),
    user_profile = FALSE
  )
}

.check_dreamai_input <- function(data, method, args) {
  observed <- !is.na(data)
  per_feature <- rowSums(observed)
  if (any(per_feature == 0L) || any(colSums(observed) == 0L)) {
    cli::cli_abort(
      "DreamAI requires at least one observed value per feature and per sample within each group."
    )
  }
  if ("KNN" %in% method && any(per_feature == 1L)) {
    cli::cli_abort(c(
      "DreamAI's KNN component needs at least two observed values per feature.",
      "x" = "Row {match(1L, per_feature)} has only one observation in this fit.",
      "i" = "Filter sparse features with {.fn remove_rare} or select other components with {.arg method}."
    ))
  }
  if ("RegImpute" %in% method && ncol(data) < 3L) {
    cli::cli_abort(
      "RegImpute needs at least three samples per fit. Select other components with {.arg method} for smaller groups."
    )
  }
  # DreamAI's native SpectroFM code indexes these entries even on small inputs.
  if (
    "SpectroFM" %in% method && (sum(observed) < 101L || sum(!observed) < 11L)
  ) {
    cli::cli_abort(
      "SpectroFM needs at least 101 observed and 11 missing entries per fit. Select other components with {.arg method} for smaller inputs."
    )
  }
  if (any(c("KNN", "ADMIN") %in% method)) {
    k <- args[["k"]]
    if (is.null(k)) {
      k <- 10L
    }
    checkmate::assert_int(k, lower = 1L, upper = nrow(data) - 1L)
  }
  invisible(NULL)
}
