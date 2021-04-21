test_that("locale is set to English", {
  skip_on_cran()  # We can't be sure which locales are installed on CRAN
  set_time_locale("eng")
  expect_match(Sys.getlocale("LC_TIME"), "(English|en)")
})

test_that("locale is set to German", {
  skip_on_cran()  # We can't be sure which locales are installed on CRAN
  set_time_locale("deu")
  expect_match(Sys.getlocale("LC_TIME"), "(German|de)")
})
