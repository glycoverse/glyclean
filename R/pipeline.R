#' Reproducible preprocessing pipelines
#'
#' Build an unevaluated sequence of glyclean processing calls, apply it to one
#' experiment, or retrieve the processing history of an experiment.
#'
#' @param ... Calls to glyclean processing functions. Omit the experiment and
#'   name every argument explicitly; positional and partially matched arguments
#'   are not accepted. Parameter expressions are evaluated once when building
#'   the pipeline, including default values, but processing functions are not run.
#'   Calls and parameter expressions support tidy-evaluation injection with
#'   `!!` and `!!!`.
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
#' Grouping values are frozen for replay. When `by` is supplied as a column
#' name, that original name is retained for printing the recorded step.
#'
#' Replaying a history requires the same original input and compatible package
#' versions and RNG configuration. Each stochastic operation records its explicit `seed` in
#' `parameters`, including the default when omitted. Replay passes these
#' parameters directly to the operation. Steps carry package-version information
#' as an attribute; no random-generator state is stored. Callback functions and parameter
#' values are captured at construction/recording time; external files, network
#' side effects and mutable references in arbitrary callbacks are not supported.
#' Site-sequence annotation records the actual sequences.
#'
#' @return `glyclean_pipeline()` and `get_pipeline()` return a
#'   `glyclean_pipeline` list. `apply_pipeline()` returns a processed experiment.
#' @examples
#' pipeline <- glyclean_pipeline(normalize_total_area(), impute_min_prob(seed = 42))
#' cleaned <- apply_pipeline(glyexp::real_experiment, pipeline)
#' history <- get_pipeline(cleaned)
#' @export
glyclean_pipeline <- function(...) {
  calls <- rlang::enquos(..., .named = NULL, .ignore_empty = "none")
  steps <- lapply(calls, function(call) {
    expr <- rlang::quo_get_expr(call)
    caller <- rlang::quo_get_env(call)
    if (!rlang::is_call(expr)) {
      stop("Each pipeline step must be a function call.", call. = FALSE)
    }
    fun <- .eval_pipeline_expr(expr[[1L]], caller)
    if (!is.function(fun) || !.is_pipeline_function(fun)) {
      stop("Pipeline calls must use glyclean processing functions.", call. = FALSE)
    }
    args <- rlang::call_args(expr)
    .check_pipeline_names(args)
    defaults <- rlang::fn_fmls(fun)
    fml <- names(defaults)
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
    parameters <- lapply(args, .eval_pipeline_expr, env = caller)
    bindings <- rlang::env(rlang::fn_env(fun), !!!parameters)
    for (nm in setdiff(names(defaults)[-1L], "...")) {
      if (!nm %in% names(parameters)) {
        if (rlang::is_missing(defaults[[nm]])) {
          stop(sprintf("Missing required pipeline argument: %s.", nm), call. = FALSE)
        }
        value <- rlang::eval_bare(defaults[[nm]], bindings)
        parameters[nm] <- list(value)
        assign(nm, value, bindings)
      }
    }
    .new_pipeline_step(fun, parameters)
  })
  structure(steps, class = c("glyclean_pipeline", "list"))
}

# Ordinary expressions retain their caller-scope side effects. Quosures need
# tidy evaluation, including when injected inside a larger parameter expression.
.eval_pipeline_expr <- function(expr, env) {
  if (.has_pipeline_quosure(expr)) {
    rlang::eval_tidy(expr, env = env)
  } else {
    rlang::eval_bare(expr, env = env)
  }
}

.has_pipeline_quosure <- function(expr) {
  # Empty subscript positions are missing arguments, not quosures.
  if (rlang::is_missing(expr)) return(FALSE)
  rlang::is_quosure(expr) ||
    (rlang::is_call(expr) &&
       any(vapply(as.list(expr), .has_pipeline_quosure, logical(1))))
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
    "normalize_vsn", "normalize_median_quotient", "zero_to_na", "impute_zero",
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

# Capture the input history once, then record only successful results. Keeping
# capture inside the returned recorder lets callers validate first, or supply
# an explicit step after resolving effective parameters (for example FASTA data).
.pipeline_recorder <- function(exp) {
  previous <- get_pipeline(exp)
  fun <- sys.function(-1L)
  frame <- parent.frame()
  function(result, step = .capture_pipeline_step(fun, frame)) {
    force(step)
    result <- force(result)
    .append_pipeline_step(result, step, previous = previous)
  }
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
  by_column <- NULL
  if ("by" %in% names(parameters) && !is.null(parameters$by)) {
    if (is.character(parameters$by) && length(parameters$by) == 1L) {
      by_column <- parameters$by
    }
    input <- get(names(formals(fun))[[1L]], frame, inherits = FALSE)
    parameters["by"] <- list(.resolve_column_param(
      parameters$by,
      sample_info = .get_sample_info(input),
      param_name = "by",
      n_samples = ncol(.get_expr_mat(input))
    ))
    # A single character value is otherwise parsed as a column name on replay.
    if (is.character(parameters$by) && length(parameters$by) == 1L) {
      parameters$by <- factor(parameters$by)
    }
  }
  step <- .new_pipeline_step(fun, parameters)
  if (!is.null(by_column)) {
    attr(step, "by_column") <- by_column
  }
  step
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
  result <- do.call(step[["function"]], c(list(exp), step$parameters))
  by_column <- attr(step, "by_column", exact = TRUE)
  if (!is.null(by_column)) {
    # Replay supplies frozen group values, so the recorder cannot recover the
    # original column name. Carry that display attribute to its new record.
    previous <- get_pipeline(exp)
    history <- get_pipeline(result)
    if (length(history) == length(previous) + 1L) {
      recorded <- history[[length(history)]]
      attr(recorded, "by_column") <- by_column
      result <- .append_pipeline_step(result, recorded, previous = previous)
    }
  }
  result
}

.pipeline_operation_name <- function(fun) {
  candidates <- .pipeline_function_names()
  # Stored closures can come from an earlier namespace instance or an installed
  # build without source references. This comparison is only for display;
  # replay still uses the original function, environment and parameters.
  fun <- rlang::zap_srcref(fun)
  matches <- candidates[vapply(
    candidates,
    \(name) {
      current <- get(name, envir = asNamespace("glyclean"))
      identical(fun, rlang::zap_srcref(current), ignore.environment = TRUE)
    },
    logical(1)
  )]
  if (length(matches) == 1L) matches[[1L]] else "recorded operation"
}

.pipeline_callback_summary <- function(fun) {
  env <- if (is.primitive(fun)) baseenv() else environment(fun)
  if (identical(env, baseenv()) || isNamespace(env)) {
    candidates <- ls(env, all.names = TRUE)
    for (name in candidates) {
      if (
        !bindingIsActive(name, env) &&
          identical(fun, get(name, envir = env, inherits = FALSE))
      ) {
        package <- if (identical(env, baseenv())) {
          "base"
        } else {
          getNamespaceName(env)
        }
        separator <- if (name %in% getNamespaceExports(package)) "::" else ":::"
        name <- if (make.names(name) == name) {
          name
        } else {
          encodeString(name, quote = "`")
        }
        return(paste0(package, separator, name))
      }
    }
  }
  paste0("function(", paste(names(formals(fun)), collapse = ", "), ")")
}

.pipeline_factor_summary <- function(value) {
  codes <- unclass(value)
  attributes(codes) <- NULL
  labels <- attr(value, "levels", exact = TRUE)
  counts <- tabulate(codes, nbins = length(labels))
  used <- which(counts > 0L)
  shown <- utils::head(used, 6L)
  groups <- if (length(shown)) {
    paste0(
      vapply(labels[shown], .pipeline_parameter_summary, character(1)),
      " (",
      counts[shown],
      ")"
    )
  } else {
    character()
  }
  if (length(used) > length(shown)) {
    groups <- c(groups, "...")
  }
  missing <- sum(is.na(codes))
  if (missing) {
    groups <- c(groups, paste0("NA (", missing, ")"))
  }
  paste0(
    "<factor [",
    length(codes),
    "]",
    if (length(groups)) paste0(": ", paste(groups, collapse = ", ")),
    ">"
  )
}

# Compact, non-dispatching summaries: printing history must never run callbacks
# or user-defined print/format methods, or expose large stored data.
.pipeline_parameter_summary <- function(value) {
  if (is.null(value)) return("NULL")
  if (is.function(value)) return(.pipeline_callback_summary(value))
  dimensions <- attr(value, "dim", exact = TRUE)
  if (!is.null(dimensions)) {
    kind <- if (length(dimensions) == 2L) "matrix" else "array"
    return(sprintf("<%s [%s]>", kind, paste(dimensions, collapse = " x ")))
  }
  if (is.factor(value)) return(.pipeline_factor_summary(value))
  if (is.object(value)) {
    return(paste0("<", paste(attr(value, "class", exact = TRUE),
                            collapse = "/"), ">"))
  }
  kind <- typeof(value)
  size <- length(value)
  if (kind %in% c("logical", "integer", "double", "complex", "character") &&
      size == 1L) {
    if (kind == "character") {
      if (is.na(value)) return("NA_character_")
      # Long strings may be biological sequences or other stored input data.
      if (nchar(value, type = "width") > 40L) return("<character [1]>")
      return(encodeString(value, quote = '"'))
    }
    return(as.character(value))
  }
  sprintf("<%s [%s]>", kind, size)
}

#' @export
print.glyclean_pipeline <- function(x, ...) {
  if (!interactive()) {
    old <- options(cli.num_colors = 1L)
    on.exit(options(old), add = TRUE)
  }
  count <- length(x)
  cli::cli_h1("<glyclean_pipeline> {count} step{?s}")
  if (!count) {
    cli::cli_text("(empty)")
    return(invisible(x))
  }
  cli::cli_ol()
  for (step in x) {
    label <- .pipeline_operation_name(step[["function"]])
    cli::cli_li("{.strong {label}}")
    parameter_block <- cli::cli_div(theme = list(div = list("margin-left" = 3)))
    parameters <- step[["parameters"]]
    by_column <- attr(step, "by_column", exact = TRUE)
    if (!length(parameters)) {
      cli::cli_text("(no parameters)")
      cli::cli_end(parameter_block)
      next
    }
    tokens <- vapply(
      seq_along(parameters),
      function(j) {
        name <- names(parameters)[[j]]
        summary <- if (name == "by" && !is.null(by_column)) {
          encodeString(by_column, quote = '"')
        } else {
          .pipeline_parameter_summary(parameters[[j]])
        }
        paste0(
          encodeString(name),
          " = ",
          summary
        )
      },
      character(1)
    )
    parameter_text <- paste(tokens, collapse = ", ")
    cli::cli_text("{parameter_text}")
    cli::cli_end(parameter_block)
  }
  invisible(x)
}
