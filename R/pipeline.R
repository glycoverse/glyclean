#' Reproducible preprocessing pipelines
#'
#' Build an unevaluated sequence of glyclean processing calls, apply it to one
#' experiment, or retrieve the processing history of an experiment.
#'
#' @param ... Calls to glyclean processing functions. Omit the experiment and
#'   name every argument explicitly; positional and partially matched arguments
#'   are not accepted. Parameter expressions are evaluated once when building
#'   the pipeline, including default values, but processing functions are not run.
#' @param exp A supported glyclean experiment.
#' @param pipeline A `glyclean_pipeline` list.
#' @details
#' A pipeline is a list of `glyclean_step` lists. Each step has exactly two
#' fields: `function` (the function object) and `parameters` (a named list).
#' Use `step[["function"]]` to inspect the function. Pipelines are ordinary
#' lists, not callable functions: use `apply_pipeline(exp, pipeline)`.
#'
#' Successful processing calls append to `metadata(exp)$pipeline` (or
#' `exp$meta_data$pipeline` for legacy experiments). Automatic operations record
#' the concrete operations they execute. QC summaries and plots do not modify
#' history. The experiment still contains only one abundance assay; history
#' contains parameters, not copies of intermediate assays.
#'
#' Replaying a history requires the same original input and compatible package
#' versions and RNG configuration. Each stochastic operation records its explicit `seed` in
#' `parameters`, including the default when omitted. Replay passes these
#' parameters directly to the operation. Steps carry package-version information
#' as an attribute; no random-generator state is stored. Callback functions and parameter
#' values are captured at construction/recording time; external files, network
#' side effects and mutable references in arbitrary callbacks are not supported.
#' Site-sequence annotation records the actual sequences. CoDA records the
#' selected top-level CLR or ALR function and its parameters, including seed.
#' Replaying ALR repeats its reference evaluation and any CLR fallback using
#' that seed; no per-site plan or reference selection is stored in history.
#'
#' @return `glyclean_pipeline()` and `get_pipeline()` return a
#'   `glyclean_pipeline` list. `apply_pipeline()` returns a processed experiment.
#' @examples
#' pipeline <- glyclean_pipeline(normalize_total_area(), impute_min_prob(seed = 42))
#' cleaned <- apply_pipeline(glyexp::real_experiment, pipeline)
#' history <- get_pipeline(cleaned)
#' @export
glyclean_pipeline <- function(...) {
  calls <- as.list(substitute(list(...)))[-1L]
  caller <- parent.frame()
  steps <- lapply(calls, function(expr) {
    if (!is.call(expr)) {
      stop("Each pipeline step must be a function call.", call. = FALSE)
    }
    fun <- eval(expr[[1L]], caller)
    if (!is.function(fun) || !.is_pipeline_function(fun)) {
      stop("Pipeline calls must use glyclean processing functions.", call. = FALSE)
    }
    args <- as.list(expr)[-1L]
    .check_pipeline_names(args)
    fml <- names(formals(fun))
    if (length(args) && fml[[1L]] %in% names(args)) {
      stop("Omit the experiment argument from pipeline steps.", call. = FALSE)
    }
    if (!"..." %in% fml && any(!names(args) %in% fml[-1L])) {
      stop("Unknown or partially matched pipeline argument.", call. = FALSE)
    }
    # Even with ..., partial matching of a formal is ambiguous and forbidden.
    for (arg in names(args)) {
      if (!arg %in% fml && any(startsWith(setdiff(fml, "..."), arg))) {
        stop("Use complete argument names in pipeline steps.", call. = FALSE)
      }
    }
    parameters <- lapply(args, eval, envir = caller)
    defaults <- formals(fun)
    bindings <- list2env(parameters, parent = environment(fun))
    for (nm in setdiff(names(defaults)[-1L], "...")) {
      if (!nm %in% names(parameters)) {
        if (identical(defaults[[nm]], quote(expr = ))) {
          stop(sprintf("Missing required pipeline argument: %s.", nm), call. = FALSE)
        }
        value <- eval(defaults[[nm]], bindings)
        parameters[nm] <- list(value)
        assign(nm, value, bindings)
      }
    }
    .new_pipeline_step(fun, parameters)
  })
  structure(steps, class = c("glyclean_pipeline", "list"))
}

#' @rdname glyclean_pipeline
#' @export
get_pipeline <- function(exp) {
  .assert_glyclean_container(exp)
  pipeline <- .get_container_metadata(exp)$pipeline
  if (is.null(pipeline)) {
    return(structure(list(), class = c("glyclean_pipeline", "list")))
  }
  .validate_pipeline(pipeline)
  pipeline
}

#' @rdname glyclean_pipeline
#' @export
apply_pipeline <- function(exp, pipeline) {
  .assert_glyclean_container(exp)
  .validate_pipeline(pipeline)
  result <- exp
  for (step in pipeline) {
    before <- get_pipeline(result)
    result <- .replay_pipeline_step(result, step)
    .assert_glyclean_container(result)
    # A concrete replay helper may not itself append history.
    if (identical(get_pipeline(result), before) && !.is_auto_pipeline_function(step[["function"]])) {
      result <- .append_pipeline_step(result, step, previous = before)
    }
  }
  result
}

.pipeline_function_names <- function() {
  c("normalize_median", "normalize_median_abs", "normalize_total_area",
    "normalize_quantile", "normalize_loessf", "normalize_loesscyc",
    "normalize_rlr", "normalize_rlrma", "normalize_rlrmacyc",
    "normalize_vsn", "normalize_median_quotient", "impute_zero",
    "impute_sample_min", "impute_half_sample_min", "impute_sw_knn",
    "impute_fw_knn", "impute_bpca", "impute_ppca", "impute_svd",
    "impute_min_prob", "impute_miss_forest", "remove_rare", "remove_low_var",
    "remove_low_cv", "remove_constant", "remove_low_expr", "aggregate",
    "adjust_protein", "add_site_seq", "correct_batch_effect",
    "transform_clr", "transform_alr", "auto_normalize", "auto_impute",
    "auto_remove", "auto_aggregate", "auto_clean", "auto_coda",
    "auto_correct_batch_effect")
}

.is_auto_pipeline_function <- function(fun) {
  any(vapply(grep("^auto_", .pipeline_function_names(), value = TRUE), function(nm) {
    identical(fun, get(nm, envir = asNamespace("glyclean")))
  }, logical(1)))
}

.is_pipeline_function <- function(fun) {
  any(vapply(.pipeline_function_names(), function(nm) {
    identical(fun, get(nm, envir = asNamespace("glyclean")))
  }, logical(1)))
}

.check_pipeline_names <- function(parameters) {
  if (length(parameters) && (is.null(names(parameters)) ||
      anyNA(names(parameters)) || any(!nzchar(names(parameters))) ||
      anyDuplicated(names(parameters)))) {
    stop("Pipeline parameters must have unique, nonempty keyword names.", call. = FALSE)
  }
}

# Snapshot only referenced lexical bindings, never an entire caller frame.
.freeze_pipeline_value <- function(value, depth = 0L) {
  if (depth > 30L) stop("Recursive callback environments are not supported.", call. = FALSE)
  if (is.environment(value) || typeof(value) %in% c("externalptr", "weakref")) {
    stop("Mutable reference parameters cannot be recorded reproducibly.", call. = FALSE)
  }
  if (is.function(value) && !is.primitive(value)) {
    env <- environment(value)
    if (!isNamespace(env) && !identical(env, baseenv()) && !identical(env, emptyenv())) {
      bindings <- codetools::findGlobals(value, merge = TRUE)
      dynamic <- c("get", "get0", "mget", "assign", "eval", "evalq", "source",
                   "parent.frame", "sys.frame", "globalenv", "<<-")
      if (any(bindings %in% dynamic)) {
        stop("Callbacks must use static lexical bindings, not dynamic environment access.", call. = FALSE)
      }
      frozen <- new.env(parent = baseenv())
      for (nm in bindings) {
        if (exists(nm, env, inherits = TRUE)) {
          binding <- get(nm, env, inherits = TRUE)
          if (identical(binding, value)) {
            stop("Self-referencing callbacks are not supported.", call. = FALSE)
          }
          assign(nm, .freeze_pipeline_value(binding, depth + 1L), frozen)
        }
      }
      environment(value) <- frozen
    }
    return(value)
  }
  if (is.list(value)) {
    value[] <- lapply(value, .freeze_pipeline_value, depth = depth + 1L)
  }
  value
}

.new_pipeline_step <- function(fun, parameters) {
  stopifnot(is.function(fun), is.list(parameters))
  .check_pipeline_names(parameters)
  structure(list(`function` = fun,
                 parameters = .freeze_pipeline_value(parameters)),
            class = c("glyclean_step", "list"),
            package_version = as.character(utils::packageVersion("glyclean")))
}

.validate_pipeline <- function(pipeline) {
  if (!inherits(pipeline, "glyclean_pipeline") || !is.list(pipeline)) {
    stop("Expected a glyclean_pipeline list.", call. = FALSE)
  }
  for (step in pipeline) {
    if (!inherits(step, "glyclean_step") || !is.list(step) ||
        !identical(names(step), c("function", "parameters")) ||
        !is.function(step[["function"]]) || !is.list(step$parameters)) {
      stop("Invalid glyclean_step: expected function and parameters fields.", call. = FALSE)
    }
    .check_pipeline_names(step$parameters)
  }
  invisible(pipeline)
}

.name_pipeline_dots <- function(fun, dots) {
  if (!length(dots)) return(dots)
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  if (any(!nzchar(nms))) {
    matches <- function(nms) any(vapply(nms, function(nm) {
      identical(fun, get(nm, asNamespace("glyclean")))
    }, logical(1)))
    available <- if (matches(c("impute_bpca", "impute_ppca", "impute_svd"))) {
      setdiff(names(formals(pcaMethods::pca)), c("object", "method", "nPcs", "..."))
    } else if (matches(c("impute_sw_knn", "impute_fw_knn"))) {
      setdiff(names(formals(impute::impute.knn)), c("data", "k", "..."))
    } else if (matches("impute_miss_forest")) {
      setdiff(names(formals(missForest::missForest)), c("xmis", "..."))
    } else if (matches("normalize_vsn")) {
      setdiff(names(formals(vsn::vsnMatrix)), c("x", "..."))
    } else if (matches("normalize_quantile")) {
      setdiff(names(formals(limma::normalizeQuantiles)), "A")
    } else if (matches(c("normalize_loessf", "normalize_loesscyc"))) {
      setdiff(names(formals(limma::normalizeCyclicLoess)), c("x", "method", "..."))
    } else character()
    available <- setdiff(available, nms[nzchar(nms)])
    if (sum(!nzchar(nms)) > length(available)) {
      stop("Name forwarded arguments explicitly so they can be recorded.", call. = FALSE)
    }
    nms[!nzchar(nms)] <- utils::head(available, sum(!nzchar(nms)))
    names(dots) <- nms
  }
  .check_pipeline_names(dots)
  dots
}

.capture_pipeline_step <- function(fun, frame) {
  parameters <- list()
  for (nm in names(formals(fun))[-1L]) {
    if (nm == "...") {
      dots <- eval(quote(list(...)), frame)
      dots <- .name_pipeline_dots(fun, dots)
      parameters <- c(parameters, dots)
    } else {
      parameters[nm] <- list(get(nm, frame, inherits = FALSE))
    }
  }
  if ("by" %in% names(parameters) && !is.null(parameters$by)) {
    input <- get(names(formals(fun))[[1L]], frame, inherits = FALSE)
    parameters["by"] <- list(.resolve_column_param(
      parameters$by, sample_info = .get_sample_info(input),
      param_name = "by", n_samples = ncol(.get_expr_mat(input))
    ))
    # A single character value is otherwise parsed as a column name on replay.
    if (is.character(parameters$by) && length(parameters$by) == 1L) {
      parameters$by <- factor(parameters$by)
    }
  }
  .new_pipeline_step(fun, parameters)
}

.append_pipeline_step <- function(result, step, previous = get_pipeline(result)) {
  .validate_pipeline(previous)
  history <- structure(c(unclass(previous), list(step)),
                       class = c("glyclean_pipeline", "list"))
  metadata <- .get_container_metadata(result)
  metadata$pipeline <- history
  if (inherits(result, "glyexp_experiment")) {
    result$meta_data <- metadata
  } else {
    S4Vectors::metadata(result) <- metadata
  }
  result
}

.replay_pipeline_step <- function(exp, step) {
  do.call(step[["function"]], c(list(exp), step$parameters))
}

#' @export
print.glyclean_pipeline <- function(x, ...) {
  cat(sprintf("<glyclean_pipeline> %d step(s)\n", length(x)))
  for (i in seq_along(x)) {
    step <- x[[i]]
    fun <- step[["function"]]
    candidates <- .pipeline_function_names()
    hit <- candidates[vapply(candidates, function(nm) {
      identical(fun, get(nm, envir = asNamespace("glyclean")))
    }, logical(1))]
    label <- if (length(hit)) hit[[1L]] else "recorded operation"
    cat(sprintf("  %d. %s\n", i, label))
  }
  invisible(x)
}
