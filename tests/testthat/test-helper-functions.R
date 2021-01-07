#' context("Helper Functions")
#'
#'
#' #' Austral Season Months:
#' #' 01 Jul  02 Aug  03 Sep  04 Oct  05 Nov  06 Dec
#' #' 07 Jan  08 Feb  09 Mar  10 Apr  11 May  12 Jun
#' all_months <- 1:12
#'
#' test_that("is_Oct_to_Apr is computed correctly", {
#'   expected_OA <- c(
#'     FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
#'     TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
#'   )
#'
#'   expect_equal(is_Oct_to_Apr(all_months), expected_OA)
#' })
#'
#'
#' test_that("is_Sep_to_Apr is computed correctly", {
#'   expected_SA <- c(
#'     FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
#'     TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
#'   )
#'
#'   expect_equal(is_Sep_to_Apr(all_months), expected_SA)
#' })
#'
#'
#' test_that("is_Mar_to_Apr is computed correctly", {
#'   expected_MA <- c(
#'     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
#'     FALSE, FALSE, TRUE, TRUE, FALSE, FALSE
#'   )
#'
#'   expect_equal(is_Mar_to_Apr(all_months), expected_MA)
#' })
