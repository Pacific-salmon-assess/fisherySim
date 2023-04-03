test_that("SIT Method", {
  #Results based on the Excel spreadsheet simulation
  expected_result <-
    data.frame(post_fishery_mark_cohort = 30.400000,
               pre_fishery_mark_cohort = 57.664,
               mark_cohort_mortalities = 57.664 - 30.400000,
               post_fishery_unmark_cohort = 38.7033,
               pre_fishery_unmark_cohort = 48.1987392,
               unmark_cohort_mortalities = 48.1987392 - 38.7033)

  sit_result <-
    sitMethod(kept_mark = 80,
              legal_release_unmark = 200,
              kept_mark_cohort = 22.4,
              legal_release_mort_rate = 0.2,
              nonlegal_release_mort_rate = 0.34,
              legal_drop_off_mort_rate = 0.05,
              nonlegal_drop_off_mort_rate = 0.05,
              cohort_prop_nonlegal = 0.3,
              escapement_mark_cohort = 30.400000,
              escapement_unmark_cohort = 38.7033)

  expect_equal(sit_result, expected_result)
})
