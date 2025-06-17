#' Remove Variables with Missing Values
#'
#' @param exp An experiment object.
#' @param prop The proportion of missing values to use as a threshold.
#' Variables with missing values above this threshold will be removed.
#' Defaults to 0.5.
#' @param n The number of missing values to use as a threshold.
#' An alternative to `prop`.
#' @param by A string specifying a sample information column to stratify by.
#' Missing value counts or proportions will be calculated within each group.
#' @param strict Works with `by`. If `FALSE`, remove a variable only if
#' it passes the missing threshold in all groups.
#' If `TRUE`, remove a variable if it passes the missing threshold in any group.
#' See examples for more details.
#' @param min_n The minimum number of non-missing values required for a variable
#' to be kept. If `NULL` (default), it is calculated dynamically:
#' - For datasets with 1, 2, or 3 samples: min_n equals the sample count
#' - For datasets with >3 samples: min_n = 3
#' - When using `by`, the rule is applied within each group
#'
#' @examples
#' exp <- glyexp::toy_experiment()
#' exp$expr_mat[1, 1] <- NA    # V1: 1/6 missing
#' exp$expr_mat[2, 1:3] <- NA  # V2: 3/6 missing
#' exp$expr_mat[3, 1:5] <- NA  # V3: 5/6 missing
#' exp$expr_mat[4, 1:6] <- NA  # V4: 6/6 missing
#' exp$expr_mat
#'
#' # Remove variables with more than 50% missing values.
#' remove_missing_variables(exp, prop = 0.5)$expr_mat
#'
#' # Remove variables with more than 2 missing values.
#' remove_missing_variables(exp, n = 2)$expr_mat
#'
#' # Remove variables if they have more than 1 missing value in all groups.
#' # In another word, keep variables as long as they have 1 or 0 missing value
#' # in any group.
#' remove_missing_variables(exp, by = "group", strict = FALSE)$expr_mat
#'
#' # Keep only variables with no missing values.
#' remove_missing_variables(exp, prop = 0)$expr_mat
#'
#' # Use custom min_n to require at least 4 non-missing values
#' remove_missing_variables(exp, min_n = 4)$expr_mat
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return An experiment object with the variables removed.
#' @export
remove_missing_variables <- function(exp, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL) {
  # Validate and standardize parameters
  params <- validate_filter_params(prop, n, min_n)
  
  # Calculate min_n if not provided
  min_n_values <- calculate_min_n(exp, by, params$min_n)
  
  # Validate min_n against sample sizes
  validate_min_n(exp, by, min_n_values)
  
  # Filter variables based on missing values
  if (is.null(by)) {
    vars_to_remove <- filter_missing_global(exp$expr_mat, params$prop, params$n, min_n_values$global)
  } else {
    vars_to_remove <- filter_missing_by_group(exp, by, params$prop, params$n, min_n_values, strict)
  }
  
  # Apply filtering
  exp$expr_mat <- exp$expr_mat[!vars_to_remove, , drop = FALSE]
  exp$var_info <- exp$var_info %>%
    dplyr::filter(.data$variable %in% rownames(exp$expr_mat))
  exp
}


validate_filter_params <- function(prop, n, min_n) {
  # Set default prop if both prop and n are NULL
  if (is.null(n) && is.null(prop)) {
    prop <- 0.5
  }
  
  # Check mutual exclusivity
  if (!is.null(n) && !is.null(prop)) {
    rlang::abort("Only one of `prop` or `n` can be provided.")
  }
  
  # Validate prop
  if (!is.null(prop)) {
    checkmate::assert_number(prop, lower = 0, upper = 1)
  }
  
  # Validate n
  if (!is.null(n)) {
    checkmate::assert_number(n, lower = 0)
  }
  
  # Validate min_n
  if (!is.null(min_n)) {
    checkmate::assert_number(min_n, lower = 1)
  }
  
  list(prop = prop, n = n, min_n = min_n)
}


calculate_min_n <- function(exp, by, min_n) {
  if (!is.null(min_n)) {
    # User provided min_n, use it for all
    return(list(global = min_n, by_group = NULL))
  }
  
  if (is.null(by)) {
    # Global min_n calculation
    n_samples <- ncol(exp$expr_mat)
    global_min_n <- ifelse(n_samples <= 3, n_samples, 3)
    return(list(global = global_min_n, by_group = NULL))
  } else {
    # Group-wise min_n calculation
    groups <- split(seq_len(ncol(exp$expr_mat)), exp$sample_info[[by]])
    group_sizes <- sapply(groups, length)
    group_min_n <- ifelse(group_sizes <= 3, group_sizes, 3)
    return(list(global = NULL, by_group = group_min_n))
  }
}


validate_min_n <- function(exp, by, min_n_values) {
  if (is.null(by)) {
    # Global validation
    if (min_n_values$global > ncol(exp$expr_mat)) {
      rlang::abort(paste0("min_n (", min_n_values$global, ") cannot be greater than the number of samples (", ncol(exp$expr_mat), ")."))
    }
  } else {
    # Group-wise validation
    groups <- split(seq_len(ncol(exp$expr_mat)), exp$sample_info[[by]])
    group_sizes <- sapply(groups, length)
    
    if (is.null(min_n_values$global)) {
      # Using group-specific min_n
      for (i in seq_along(groups)) {
        if (min_n_values$by_group[i] > group_sizes[i]) {
          rlang::abort(paste0("min_n (", min_n_values$by_group[i], ") cannot be greater than the number of samples in group ", names(groups)[i], " (", group_sizes[i], ")."))
        }
      }
    } else {
      # Using global min_n for all groups
      if (any(min_n_values$global > group_sizes)) {
        problematic_groups <- names(groups)[min_n_values$global > group_sizes]
        rlang::abort(paste0("min_n (", min_n_values$global, ") cannot be greater than the number of samples in group(s): ", paste(problematic_groups, collapse = ", "), "."))
      }
    }
  }
}


filter_missing_global <- function(expr_mat, prop, n, min_n) {
  # Calculate threshold-based removal
  if (is.null(n)) {
    vars_to_remove <- rowMeans(is.na(expr_mat)) > prop
  } else {
    vars_to_remove <- rowSums(is.na(expr_mat)) > n
  }
  
  # Apply min_n constraint
  non_missing_counts <- rowSums(!is.na(expr_mat))
  vars_to_remove <- vars_to_remove | (non_missing_counts < min_n)
  
  vars_to_remove
}


filter_missing_by_group <- function(exp, by, prop, n, min_n_values, strict) {
  groups <- split(seq_len(ncol(exp$expr_mat)), exp$sample_info[[by]])
  
  group_results <- lapply(names(groups), function(group_name) {
    idx <- groups[[group_name]]
    group_mat <- exp$expr_mat[, idx, drop = FALSE]
    
    # Calculate missing threshold result
    if (is.null(n)) {
      threshold_result <- rowMeans(is.na(group_mat)) > prop
    } else {
      threshold_result <- rowSums(is.na(group_mat)) > n
    }
    
    # Apply min_n constraint for this group
    non_missing_counts <- rowSums(!is.na(group_mat))
    current_min_n <- if (is.null(min_n_values$global)) {
      min_n_values$by_group[[group_name]]
    } else {
      min_n_values$global
    }
    min_n_result <- non_missing_counts < current_min_n
    
    # Combine threshold and min_n constraints
    threshold_result | min_n_result
  })
  names(group_results) <- names(groups)
  
  # Combine results based on strict parameter
  if (strict) {
    # Remove if exceeds threshold in any group
    vars_to_remove <- Reduce(`|`, group_results)
  } else {
    # Remove only if exceeds threshold in all groups
    vars_to_remove <- Reduce(`&`, group_results)
  }
  
  vars_to_remove
}
