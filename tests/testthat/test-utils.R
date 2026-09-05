test_that("clean_yishuv_name strips punctuation and digits but keeps name characters", {
  expect_equal(clean_yishuv_name("  תל אביב - יפו  "), "תל אביב - יפו")
  expect_equal(clean_yishuv_name("כפר חב\"ד"), "כפר חב\"ד")
  expect_equal(clean_yishuv_name("ג'ש (גוש חלב)"), "ג'ש (גוש חלב)")
  expect_equal(clean_yishuv_name("ירושלים 91000, ת.ד. 5"), "ירושלים תד")
  expect_equal(clean_yishuv_name("חיפה/קריות"), "חיפהקריות")
})

test_that("fmt_sigfig keeps integer digits and rounds small numbers to significant figures", {
  expect_equal(fmt_sigfig(325902.3, 2), "325,902")
  expect_equal(fmt_sigfig(0.22113, 2), "0.22")
  expect_equal(fmt_sigfig(0.07742, 2), "0.077")
  expect_equal(fmt_sigfig(-23733.6, 2), "-23,734")
  expect_equal(fmt_sigfig(19.206, 3), "19.2")
  expect_equal(fmt_sigfig(-1.5791, 3), "-1.58")
  expect_equal(fmt_sigfig(0.35317, 3), "0.353")
})

test_that("fmt_pvalue follows the thesis conventions", {
  expect_equal(fmt_pvalue(c(0.0004, 0.0046, 0.1159, 0.4), bold_below = NULL), c("<0.001", "0.005", "0.12", "0.4"))
  expect_equal(fmt_pvalue(0.024), "**0.024**")
  expect_equal(fmt_pvalue(0.15), "0.15")
})
