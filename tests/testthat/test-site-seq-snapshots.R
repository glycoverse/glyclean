test_that("empty sequence histories replay without UniProt lookup", {
  x <- test_glycoproteomic_se(matrix(
    1:4,
    2,
    dimnames = list(c("V1", "V2"), c("S1", "S2"))
  ))
  SummarizedExperiment::rowData(x)$protein <- rep(NA_character_, 2)
  local_mocked_bindings(.query_uniprot = function(...) {
    stop("Unexpected lookup")
  })
  before <- x
  result <- suppressMessages(add_site_seq(x))
  expect_identical(
    SummarizedExperiment::rowData(result)$site_sequence,
    rep(NA_character_, 2)
  )
  history <- get_pipeline(result)
  expect_identical(
    history[[1]]$parameters$fasta,
    stats::setNames(character(), character())
  )
  expect_identical(suppressMessages(apply_pipeline(x, history)), result)
  # Old histories stored an unnamed empty vector; keep them replayable too.
  history[[1]]$parameters$fasta <- character()
  expect_identical(suppressMessages(apply_pipeline(x, history)), result)
  expect_identical(
    suppressMessages(add_site_seq(x, fasta = character())),
    result
  )
  expect_identical(x, before)
})

test_that("normalized sequences replay and blanks fail before recording", {
  x <- test_glycoproteomic_se(matrix(
    1:4,
    2,
    dimnames = list(c("V1", "V2"), c("S1", "S2"))
  ))
  sequences <- c(P1 = "a c\nd", P2 = " e f g ")
  result <- suppressMessages(add_site_seq(x, fasta = sequences))
  expect_identical(
    get_pipeline(result)[[1]]$parameters$fasta,
    c(P1 = "ACD", P2 = "EFG")
  )
  expect_identical(
    suppressMessages(apply_pipeline(x, get_pipeline(result))),
    result
  )
  for (blank in c("", " \t\n", NA_character_)) {
    before <- x
    expect_error(add_site_seq(x, fasta = c(P1 = blank)), "non-empty")
    expect_identical(x, before)
    expect_length(get_pipeline(x), 0L)
  }
  local_mocked_bindings(.fetch_uniprot_sequences = function(...) c(P1 = " \t "))
  expect_error(suppressMessages(add_site_seq(x)), "non-empty")
})

test_that("fetched sequences are normalized before snapshotting", {
  x <- test_glycoproteomic_se(matrix(
    1:4,
    2,
    dimnames = list(c("V1", "V2"), c("S1", "S2"))
  ))
  local_mocked_bindings(.fetch_uniprot_sequences = function(...) {
    c(P1 = " a c d ", P2 = "efg")
  })
  result <- suppressMessages(add_site_seq(x))
  expect_identical(
    get_pipeline(result)[[1]]$parameters$fasta,
    c(P1 = "ACD", P2 = "EFG")
  )
  expect_identical(
    suppressMessages(apply_pipeline(x, get_pipeline(result))),
    result
  )
})
