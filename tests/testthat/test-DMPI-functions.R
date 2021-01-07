
test_that("Downy Mildew Pressure Index is calculated correctly", {

  ## context: test rainfall -- duration definitely longer than 10 hours
  #
  expect_equal(downy_mildew_pressure_index(rain = 9,  T_max = 20, T_min = 8, T_thresh = 10), FALSE)
  expect_equal(downy_mildew_pressure_index(rain = 10, T_max = 20, T_min = 8, T_thresh = 10), TRUE)

  ## context: test duration -- all cases where rainfall criteria is met
  # examples are pre calculated manually (Pythagoras...)
  #
  expect_equal(downy_mildew_pressure_index(rain = 11, T_max = 20,   T_min = 10,   T_thresh = 10), TRUE)   # 24
  expect_equal(downy_mildew_pressure_index(rain = 11, T_max = 22,   T_min = 6,    T_thresh = 10), TRUE)   # 18
  expect_equal(downy_mildew_pressure_index(rain = 11, T_max = 22,   T_min = -6.8, T_thresh = 10), TRUE)   # 10
  expect_equal(downy_mildew_pressure_index(rain = 11, T_max = 22.5, T_min = -6.8, T_thresh = 10), TRUE)   # >10 just (nudge T_max higher)
  expect_equal(downy_mildew_pressure_index(rain = 11, T_max = 21.5, T_min = -6.8, T_thresh = 10), FALSE)  # <10 just (nudge T_max lower)
  expect_equal(downy_mildew_pressure_index(rain = 11, T_max = 13,   T_min = 4,    T_thresh = 10), FALSE)  # 8
})
