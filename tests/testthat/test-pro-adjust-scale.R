protein_scale_fixture <- function(
  scale = "raw",
  legacy = FALSE,
  gp = c(0.2, 0.5, 2, 4, NA, 7),
  pro = c(0.1, 2, 3, 0.4, 8, 6)
) {
  x <- simple_glycoproteomic_exp(2, length(gp))
  gp <- rbind(gp, rev(gp) * 2)
  dimnames(gp) <- dimnames(SummarizedExperiment::assay(x))
  if (legacy) {
    x <- withr::with_options(
      list(lifecycle_verbosity = "quiet"),
      glyexp::experiment(
        gp,
        sample_info = .get_sample_info(x),
        var_info = .get_var_info(x),
        exp_type = "glycoproteomics",
        glycan_type = "N"
      )
    )
  }
  metadata <- .get_container_metadata(x)
  metadata$scale <- scale
  metadata$custom <- "preserved"
  x <- .rebuild_container(
    x,
    expr_mat = protein_encode(gp, scale),
    metadata = metadata
  )
  pro <- rbind(pro, rev(pro) * 0.5)
  dimnames(pro) <- list(c("P1", "P2"), colnames(gp))
  list(exp = x, pro = pro, gp = gp)
}

protein_encode <- function(x, scale) {
  switch(scale, raw = x, log = log(x), log2 = log2(x), log10 = log10(x))
}

protein_decode <- function(x, scale) {
  switch(scale, raw = x, log = exp(x), log2 = 2^x, log10 = 10^x)
}

test_that("raw defaults retain historical formulas and positional methods", {
  d <- protein_scale_fixture()
  expected_ratio <- t(vapply(
    seq_len(2),
    function(i) {
      (d$gp[i, ] / d$pro[i, ]) /
        (median(d$gp[i, ], na.rm = TRUE) / median(d$pro[i, ], na.rm = TRUE))
    },
    numeric(6)
  ))
  expected_reg <- t(vapply(
    seq_len(2),
    function(i) {
      valid <- !is.na(d$gp[i, ]) & !is.na(d$pro[i, ])
      result <- rep(NA_real_, 6)
      result[valid] <- 2^residuals(lm(
        log2(d$gp[i, valid] + 1) ~
          log2(d$pro[i, valid] + 1)
      ))
      result
    },
    numeric(6)
  ))
  dimnames(expected_ratio) <- dimnames(expected_reg) <- dimnames(d$gp)
  expect_equal(.get_expr_mat(adjust_protein(d$exp, d$pro)), expected_ratio)
  expect_equal(.get_expr_mat(adjust_protein(d$exp, d$pro, "reg")), expected_reg)
  expect_identical(
    get_pipeline(adjust_protein(d$exp, d$pro))[[1]]$parameters$pro_scale,
    "raw"
  )
})

test_that("ordinary input scales agree independently for SE and legacy inputs", {
  scales <- c("raw", "log", "log2", "log10")
  for (legacy in c(FALSE, TRUE)) {
    for (method in c("ratio", "reg")) {
      raw <- protein_scale_fixture(legacy = legacy)
      expected <- .get_expr_mat(adjust_protein(raw$exp, raw$pro, method))
      for (scale in scales) {
        for (pro_scale in scales) {
          d <- protein_scale_fixture(scale, legacy)
          pro <- protein_encode(d$pro, pro_scale)
          # Reordered protein rows and samples must still align by name.
          pro <- pro[2:1, 6:1, drop = FALSE]
          result <- adjust_protein(d$exp, pro, method, pro_scale)
          expect_equal(
            protein_decode(.get_expr_mat(result), scale),
            expected,
            tolerance = 1e-10
          )
          expect_identical(.get_container_metadata(result)$scale, scale)
          expect_identical(.get_container_metadata(result)$custom, "preserved")
          expect_identical(class(result), class(d$exp))
          expect_equal(.get_var_info(result), .get_var_info(d$exp))
          expect_equal(.get_sample_info(result), .get_sample_info(d$exp))
        }
      }
    }
  }
})

test_that("absent experiment scale defaults to raw in protein adjustment", {
  for (legacy in c(FALSE, TRUE)) {
    d <- protein_scale_fixture(legacy = legacy)
    x <- d$exp
    metadata <- .get_container_metadata(x)
    metadata$scale <- NULL
    if (legacy) {
      x$meta_data <- metadata
    } else {
      S4Vectors::metadata(x) <- metadata
    }
    x <- unserialize(serialize(x, NULL))
    for (method in c("ratio", "reg")) {
      for (pro_scale in c("raw", "log", "log2", "log10")) {
        pro <- protein_encode(d$pro, pro_scale)
        expected <- adjust_protein(d$exp, pro, method, pro_scale)
        result <- adjust_protein(x, pro, method, pro_scale)
        expect_equal(.get_expr_mat(result), .get_expr_mat(expected))
        expect_identical(class(result), class(x))
        expect_identical(.get_scale(result), "raw")
        if (legacy) {
          expect_false("scale" %in% names(.get_container_metadata(result)))
        } else {
          expect_identical(.get_container_metadata(result)$scale, "raw")
          expect_true(methods::validObject(result))
        }
        expect_identical(.get_container_metadata(result)$custom, "preserved")
        expect_identical(apply_pipeline(x, get_pipeline(result)), result)
      }
    }
    expect_identical(.get_container_metadata(x), metadata)
  }
})

test_that("protein adjustment rejects explicit invalid experiment scales", {
  for (legacy in c(FALSE, TRUE)) {
    d <- protein_scale_fixture(legacy = legacy)
    for (scale in list(
      NULL,
      character(),
      NA_character_,
      "invalid",
      2,
      c("raw", "log2"),
      list("raw")
    )) {
      x <- d$exp
      metadata <- .get_container_metadata(x)
      # List subassignment retains the field even when its value is NULL.
      metadata["scale"] <- list(scale)
      if (legacy) {
        x$meta_data <- metadata
      } else {
        S4Vectors::metadata(x) <- metadata
      }
      for (method in c("ratio", "reg")) {
        expect_error(adjust_protein(x, d$pro, method), "scale|choices|element")
      }
    }
  }
})

test_that("pipeline stores the supplied protein representation and replays it", {
  for (legacy in c(FALSE, TRUE)) {
    for (method in c("ratio", "reg")) {
      d <- protein_scale_fixture("log10", legacy)
      pro <- log2(d$pro)
      manual <- adjust_protein(d$exp, pro, method, "log2")
      pipeline <- unserialize(serialize(get_pipeline(manual), NULL))
      expect_identical(pipeline[[1]]$parameters$pro_expr_mat, pro)
      expect_identical(pipeline[[1]]$parameters$pro_scale, "log2")
      replay <- apply_pipeline(d$exp, pipeline)
      recipe <- apply_pipeline(
        d$exp,
        glyclean_pipeline(
          adjust_protein(
            pro_expr_mat = pro,
            method = method,
            pro_scale = "log2"
          )
        )
      )
      expect_equal(.get_expr_mat(replay), .get_expr_mat(manual))
      expect_equal(.get_expr_mat(recipe), .get_expr_mat(manual))
      expect_identical(.get_container_metadata(replay)$scale, "log10")
    }
  }
})

test_that("zero and missing values keep historical behavior across scales", {
  for (method in c("ratio", "reg")) {
    raw <- protein_scale_fixture(
      gp = c(0, 0.2, 1, NA, 2, 3),
      pro = c(0.1, 0, 2, 1, NA, 3)
    )
    expected <- .get_expr_mat(adjust_protein(raw$exp, raw$pro, method))
    for (scale in c("log", "log2", "log10")) {
      metadata <- .get_container_metadata(raw$exp)
      metadata$scale <- scale
      x <- .rebuild_container(
        raw$exp,
        protein_encode(raw$gp, scale),
        metadata = metadata
      )
      result <- adjust_protein(x, protein_encode(raw$pro, scale), method, scale)
      expect_equal(protein_decode(.get_expr_mat(result), scale), expected)
    }
  }
  d <- protein_scale_fixture(gp = c(0, 0, 0, NA), pro = c(0, 0, 0, 1))
  expect_warning(result <- adjust_protein(d$exp, d$pro), "no non-missing")
  expect_true(all(is.na(.get_expr_mat(result))))
})

test_that("regression fallbacks preserve exact input representation", {
  for (legacy in c(FALSE, TRUE)) {
    for (scale in c("raw", "log", "log2", "log10")) {
      d <- protein_scale_fixture(
        scale,
        legacy,
        gp = c(0, 0.2, NA),
        pro = c(1, NA, 3)
      )
      warnings <- character()
      result <- withCallingHandlers(
        adjust_protein(d$exp, log2(d$pro), "reg", "log2"),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_length(warnings, 2L)
      expect_true(all(grepl("fewer than 3", warnings)))
      expect_identical(.get_expr_mat(result), .get_expr_mat(d$exp))
    }
  }
  d <- protein_scale_fixture("log")
  d$exp <- .subset_container(d$exp, rows = 1)
  d$pro <- d$pro[1, , drop = FALSE]
  local_mocked_bindings(
    lm = function(...) stop("fit failed"),
    .package = "stats"
  )
  expect_warning(
    result <- adjust_protein(d$exp, d$pro, "reg"),
    "Regression failed"
  )
  expect_identical(.get_expr_mat(result), .get_expr_mat(d$exp))
})

test_that("unsupported scales and invalid protein matrices fail clearly", {
  d <- protein_scale_fixture()
  for (scale in list("logx", "ln", NA_character_, NULL, 2, c("raw", "log2"))) {
    expect_error(
      adjust_protein(d$exp, d$pro, pro_scale = scale),
      class = "rlang_error"
    )
  }
  for (legacy in c(FALSE, TRUE)) {
    d <- protein_scale_fixture(legacy = legacy)
    metadata <- .get_container_metadata(d$exp)
    metadata$scale <- "logx"
    # Assign metadata directly so rejection also works with older glyexp versions
    # whose constructors predate the logx label.
    x <- d$exp
    if (legacy) {
      x$meta_data <- metadata
    } else {
      S4Vectors::metadata(x) <- metadata
    }
    expect_error(
      adjust_protein(x, d$pro),
      "cannot be.*logx",
      class = "rlang_error"
    )
  }
  d <- protein_scale_fixture()
  expect_error(adjust_protein(d$exp, as.data.frame(d$pro)))
  expect_error(adjust_protein(d$exp, unname(d$pro)))
  rownames(d$pro) <- c("P1", "P1")
  expect_error(adjust_protein(d$exp, d$pro), "duplicated")
})

test_that("abundance validation catches invalid raw values and decoding overflow", {
  d <- protein_scale_fixture()
  for (method in c("ratio", "reg")) {
    for (bad in c(-1, Inf, -Inf)) {
      pro <- d$pro
      pro[1, 1] <- bad
      expect_error(adjust_protein(d$exp, pro, method), "finite, nonnegative")
    }
    for (scale in c("log", "log2", "log10")) {
      pro <- protein_encode(d$pro, scale)
      pro[1, 1] <- 1e6
      expect_error(
        adjust_protein(d$exp, pro, method, scale),
        "finite, nonnegative"
      )
      pro[1, 1] <- -1e6
      expect_error(adjust_protein(d$exp, pro, method, scale), "underflow")
      metadata <- .get_container_metadata(d$exp)
      metadata$scale <- scale
      gp <- protein_encode(d$gp, scale)
      gp[1, 1] <- 1e6
      x <- .rebuild_container(d$exp, gp, metadata = metadata)
      expect_error(adjust_protein(x, d$pro, method), "finite, nonnegative")
    }
  }
})

test_that("matching and replay preserve scale after protein and sample filtering", {
  d <- protein_scale_fixture("log2")
  pro <- log10(d$pro[1, c(6, 2, 1, 4), drop = FALSE])
  for (method in c("ratio", "reg")) {
    expect_message(
      result <- adjust_protein(d$exp, pro, method, "log10"),
      "Dropped 1 glycopeptides"
    )
    expect_identical(rownames(.get_expr_mat(result)), "V1")
    expect_identical(colnames(.get_expr_mat(result)), c("S1", "S2", "S4", "S6"))
    expect_identical(.get_container_metadata(result)$scale, "log2")
    replay <- suppressMessages(apply_pipeline(d$exp, get_pipeline(result)))
    expect_equal(.get_expr_mat(replay), .get_expr_mat(result))
    expect_identical(get_pipeline(result)[[1]]$parameters$pro_expr_mat, pro)
  }
})

test_that("ratio uses raw arithmetic medians for even sample counts", {
  raw <- protein_scale_fixture(gp = c(0.2, 2, 9, 40), pro = c(5, 0.3, 2, 15))
  for (scale in c("log", "log2", "log10")) {
    d <- protein_scale_fixture(
      scale,
      gp = c(0.2, 2, 9, 40),
      pro = c(5, 0.3, 2, 15)
    )
    expected <- (raw$gp[1, ] / raw$pro[1, ]) /
      (median(raw$gp[1, ]) / median(raw$pro[1, ]))
    result <- adjust_protein(d$exp, log10(d$pro), pro_scale = "log10")
    expect_equal(protein_decode(.get_expr_mat(result)[1, ], scale), expected)
  }
})

test_that("ignored proteins and samples are excluded before abundance decoding", {
  d <- protein_scale_fixture("log")
  pro <- log2(d$pro)
  pro <- rbind(pro, ignored = rep(1e6, ncol(pro)))
  pro <- cbind(pro, extra_sample = rep(-1e6, nrow(pro)))
  for (method in c("ratio", "reg")) {
    expected <- adjust_protein(d$exp, d$pro, method)
    result <- adjust_protein(d$exp, pro, method, "log2")
    expect_equal(.get_expr_mat(result), .get_expr_mat(expected))
    expect_equal(
      .get_expr_mat(apply_pipeline(d$exp, get_pipeline(result))),
      .get_expr_mat(expected)
    )
  }
})
