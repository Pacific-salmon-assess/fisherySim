test_that("SIT Method", {
  #Results based on the Excel spreadsheet simulation
  expected_result <-
    data.frame(post_fishery_mark_cohort = 30.400000,
               pre_fishery_mark_cohort = 57.664,
               post_fishery_unmark_cohort = 38.7033,
               pre_fishery_unmark_cohort = 48.1987392)

  sit_result <-
    sitMethod(kept_mark = 80,
              legal_release_unmark = 200,
              kept_mark_cohort = 22.4,
              escapement_mark_cohort = 30.400000,
              legal_release_mort_rate = 0.2,
              nonlegal_release_mort_rate = 0.34,
              cohort_prop_nonlegal = 0.3,
              drop_off_rate = 0.05,
              escapement_unmark_cohort = 38.7033)

  expect_equal(sit_result, expected_result)
})
