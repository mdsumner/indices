# common data
DMT_threshold <- 19.5
temps <- c(20.48, 17.81, 19.06, 16.40, 16.34, 20.79, 18.82, 16.28, 18.25, 19.56,
           15.57, 18.60, 17.22, 19.12, 19.02, 15.58, 20.88, 15.62, 15.27, 20.52,
           19.13, 18.46, 16.80, 15.26, 20.71, 20.74, 15.74, 16.63, 18.88, 18.67,
           15.32, 16.83, 17.86, 19.04, 19.62, 19.14, 17.99, 19.36, 16.04, 18.41,
           19.02, 20.77, 17.99, 20.74, 20.41, 20.80, 20.40, 18.06, 16.32, 18.25)

# pre calculated answers from spreadsheet
# just summed the rows, instead of inserting complete lists
pre_calc_EHI_sig <- -58.6166666667
pre_calc_EHI_accl <- 13.8603333333
pre_calc_EHF <- -8.6549955556


# let
test_EHI_sig <- calc_EHI_sig(temps, DMT_threshold)

test_EHI_accl <- calc_EHI_accl(temps)


test_that("EHI_sig is computed correctly", {
  expect_equal(sum(test_EHI_sig, na.rm = TRUE), pre_calc_EHI_sig)
})


test_that("EHI_accl is computed correctly", {
  expect_equal(sum(test_EHI_accl, na.rm = TRUE), pre_calc_EHI_accl)
})


test_that("EHF is computed correctly", {
  test_EHF <- calc_EHF(test_EHI_sig, test_EHI_accl)

  expect_equal(sum(test_EHF, na.rm = TRUE), pre_calc_EHF)
})



test_that("EHF events are detected at the start of a series", {
  result <- heatwave_detect_EHF_with_lengths(EHF_values = c(1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
                                             min_duration = 3)

  expect_equal(result$detect,   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(result$duration, c(4,    0,    0,    0,    0,     0,     0,     0,     0,     0,     0,     0))
})

test_that("EHF events are detected at the end of a series", {
  result <- heatwave_detect_EHF_with_lengths(EHF_values = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1),
                                             min_duration = 3)

  expect_equal(result$detect,   c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(result$duration, c(0,     0,     0,     0,    0,     0,     0,     0,     4,     0,     0,     0))
})


test_that("EHF events are detected correctly", {

  values <- c(0, rep(1, 1),
              0, rep(1, 2),
              0, rep(1, 3),
              0, rep(1, 4),
              0, rep(1, 5),
              0, rep(1, 6),
              0, rep(1, 5),
              0, rep(1, 4),
              0, rep(1, 3),
              0, rep(1, 2),
              0, rep(1, 1))

  result <- heatwave_detect_EHF_with_lengths(EHF_values = values, min_duration = 3)

  expect_equal(result$detect,
               c(FALSE, rep(FALSE, 1),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 6),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(FALSE, 1)))

  # only the first TRUE has the duration, the rest of the values are zero
  expect_equal(result$duration,
               c(0, 0, rep(0, 1-1),
                 0, 0, rep(0, 2-1),
                 0, 3, rep(0, 3-1),
                 0, 4, rep(0, 4-1),
                 0, 5, rep(0, 5-1),
                 0, 6, rep(0, 6-1),
                 0, 5, rep(0, 5-1),
                 0, 4, rep(0, 4-1),
                 0, 3, rep(0, 3-1),
                 0, 0, rep(0, 2-1),
                 0, 0, rep(0, 1-1)))
})


test_that("min_duration parameter works for edge cases", {

  values <- c(0, rep(1, 1),
              0, rep(1, 2),
              0, rep(1, 3),
              0, rep(1, 4),
              0, rep(1, 5),
              0, rep(1, 6),
              0, rep(1, 5),
              0, rep(1, 4),
              0, rep(1, 3),
              0, rep(1, 2),
              0, rep(1, 1))

  expect_error(heatwave_detect_EHF(values, min_duration = 0),
               "min_duration")

  expect_equal(heatwave_detect_EHF(values, min_duration = 1),
               c(FALSE, rep(TRUE, 1),
                 FALSE, rep(TRUE, 2),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 6),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(TRUE, 2),
                 FALSE, rep(TRUE, 1)))

  expect_equal(heatwave_detect_EHF(values, min_duration = 2),
               c(FALSE, rep(FALSE, 1),
                 FALSE, rep(TRUE, 2),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 6),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(TRUE, 2),
                 FALSE, rep(FALSE, 1)))


  expect_equal(heatwave_detect_EHF(values, min_duration = 3),
               c(FALSE, rep(FALSE, 1),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 6),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 4),
                 FALSE, rep(TRUE, 3),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(FALSE, 1)))

  expect_equal(heatwave_detect_EHF(values, min_duration = 5),
               c(FALSE, rep(FALSE, 1),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(FALSE, 3),
                 FALSE, rep(FALSE, 4),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(TRUE, 6),
                 FALSE, rep(TRUE, 5),
                 FALSE, rep(FALSE, 4),
                 FALSE, rep(FALSE, 3),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(FALSE, 1)))

  expect_equal(heatwave_detect_EHF(values, min_duration = 10),
               c(FALSE, rep(FALSE, 1),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(FALSE, 3),
                 FALSE, rep(FALSE, 4),
                 FALSE, rep(FALSE, 5),
                 FALSE, rep(FALSE, 6),
                 FALSE, rep(FALSE, 5),
                 FALSE, rep(FALSE, 4),
                 FALSE, rep(FALSE, 3),
                 FALSE, rep(FALSE, 2),
                 FALSE, rep(FALSE, 1)))
})


test_that("calc_heatwave_EHF_severity produces correct factor", {
  # input and output
  baseline_EHF_85th <- 2
  data <- tibble::tribble(
    ~EHF, ~is_heatwave, ~EHF_severity,
    -1,   FALSE,        NA,
    -1,   FALSE,        NA,
    -1,   FALSE,        NA,
     0,   FALSE,        NA,
     1,   TRUE,         "High",
     1,   TRUE,         "High",
     1,   TRUE,         "High",
     0,   FALSE,        NA,
     1,   FALSE,        NA,
     0,   FALSE,        NA,
     0,   FALSE,        NA,
     1,   TRUE,         "High",
     2,   TRUE,         "Severe",
     3,   TRUE,         "Severe",
     4,   TRUE,         "Severe",
     0,   FALSE,        NA,
     0,   FALSE,        NA,
     1,   TRUE,         "High",
     2,   TRUE,         "Severe",
     6,   TRUE,         "Extreme",
     7,   TRUE,         "Extreme",
     8,   TRUE,         "Extreme",
     9,   TRUE,         "Extreme",
     5,   TRUE,         "Severe",
     1,   TRUE,         "High",
     0,   FALSE,        NA,
    )

  # output
  EHF_severity <- factor(x = data$EHF_severity, levels = c("High", "Severe", "Extreme"), ordered = TRUE)

  expect_equal(calc_heatwave_EHF_severity(data$EHF, baseline_EHF_85th, data$is_heatwave), EHF_severity)
})
