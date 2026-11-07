scale_reader_inputs <- function() {
  list(
    simple_exp(4, 6),
    simple_glycoproteomic_exp(4, 6),
    plain_se(simple_exp(4, 6)),
    legacy_exp(4, 6)
  )
}

scale_reader_metadata <- function(x, metadata) {
  if (inherits(x, "glyexp_experiment")) {
    x$meta_data <- metadata
  } else {
    S4Vectors::metadata(x) <- metadata
  }
  x
}

test_that("scale reader defaults only an absent field to raw without mutation", {
  for (input in scale_reader_inputs()) {
    metadata <- .get_container_metadata(input)
    metadata$scale <- NULL
    metadata$scale_hint <- "log2"
    x <- scale_reader_metadata(input, metadata)
    x <- unserialize(serialize(x, NULL))
    expect_false("scale" %in% names(.get_container_metadata(x)))
    expect_identical(.get_scale(x), "raw")
    expect_identical(.get_container_metadata(x), metadata)
    result <- normalize_total_area(x)
    expected <- normalize_total_area(input)
    expect_equal(.get_expr_mat(result), .get_expr_mat(expected))
    expect_identical(class(result), class(x))
    expect_identical(apply_pipeline(x, get_pipeline(result)), result)
    expect_identical(.get_container_metadata(x), metadata)
  }
})

test_that("scale reader retains every valid explicit scale", {
  for (input in scale_reader_inputs()) {
    for (scale in c("raw", "log", "log2", "log10", "logx")) {
      metadata <- .get_container_metadata(input)
      metadata$scale <- scale
      x <- scale_reader_metadata(input, metadata)
      expect_identical(.get_scale(x), scale)
      expect_identical(.get_container_metadata(x), metadata)
    }
  }
})

test_that("scale reader rejects explicit invalid values in every container", {
  for (input in scale_reader_inputs()) {
    for (scale in list(
      NULL,
      character(),
      NA_character_,
      "",
      "invalid",
      2,
      TRUE,
      c("raw", "log2"),
      list("raw")
    )) {
      metadata <- .get_container_metadata(input)
      metadata["scale"] <- list(scale)
      x <- scale_reader_metadata(input, metadata)
      expect_true("scale" %in% names(.get_container_metadata(x)))
      expect_error(.get_scale(x), "scale|choices|element")
      expect_error(normalize_total_area(x), "scale|choices|element")
    }
  }
})
