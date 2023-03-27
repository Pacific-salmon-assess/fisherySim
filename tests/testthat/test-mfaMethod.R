test_that("MFA Method", {
  #Results based on the Excel spreadsheet simulation
  expected_result <-
    data.frame(post_fishery_mark_cohort = 30.400000,
               pre_fishery_mark_cohort = 60,
               post_fishery_unmark_cohort = 38.7033,
               pre_fishery_unmark_cohort = 60.082421)

  mfa_result <-
    mfaMethod(kept_mark = 80,
              legal_release_mark = 20,
              kept_unmark = 100,
              legal_release_unmark = 200,
              kept_mark_cohort = 22.4,
              escapement_mark_cohort = 30.400000,
              legal_release_mort_rate = 0.2,
              nonlegal_release_mort_rate = 0.34,
              cohort_prop_nonlegal = 0.3,
              drop_off_rate = 0.05,
              escapement_unmark_cohort = 38.7033)

  expect_equal(mfa_result, expected_result)
})
