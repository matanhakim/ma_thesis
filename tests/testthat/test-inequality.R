# The weighted measures must equal the naive "one row per resident" versions
# that the original analysis used.

naive_top_share <- function(x, w, prop) {
  expanded <- rep(x, times = w)
  top <- sort(expanded, decreasing = TRUE)[seq_len(floor(prop * length(expanded)))]
  sum(top) / sum(expanded)
}

set.seed(2024)
x <- c(0, runif(40, 0, 200))
w <- c(sample(50:2000, 41, replace = TRUE))

test_that("top_share reproduces the expanded-population calculation", {
  for (prop in c(0.1, 0.5, 0.9)) {
    expect_equal(top_share(x, w, prop), naive_top_share(x, w, prop))
  }
})

test_that("top_share handles boundary cases", {
  expect_equal(top_share(x, w, 1), 1)
  expect_equal(top_share(c(10, 10, 10), c(1, 1, 1), 1 / 3), 1 / 3)
  # a single authority holding everything: the top 10% of residents hold it all
  expect_equal(top_share(c(0, 0, 100), c(1000, 1000, 10), 0.1), 1)
})

# Textbook definition: mean absolute difference between all pairs of residents,
# divided by twice the mean.
naive_gini <- function(x, w) {
  expanded <- rep(x, times = w)
  n <- length(expanded)
  sum(abs(outer(expanded, expanded, "-"))) / (2 * n^2 * mean(expanded))
}

test_that("gini_weighted matches the Gini of the expanded population", {
  small_w <- pmax(1, round(w / 50))
  expect_equal(gini_weighted(x, small_w), naive_gini(x, small_w))
  expect_equal(gini_weighted(c(5, 5, 5), c(1, 2, 3)), 0)
  expect_equal(gini_weighted(c(0, 0, 0, 100), c(1, 1, 1, 1)), 0.75)
})

test_that("collapse_clusters maps 1-10 into three ordered groups", {
  out <- collapse_clusters(c(1, 3, 4, 6, 7, 10))
  expect_equal(
    as.character(out),
    c("אשכולות 1-3", "אשכולות 1-3", "אשכולות 4-6", "אשכולות 4-6", "אשכולות 7-10", "אשכולות 7-10")
  )
  expect_equal(levels(out), c("אשכולות 1-3", "אשכולות 4-6", "אשכולות 7-10"))
})
