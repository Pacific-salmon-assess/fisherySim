#' Single Index Tag Method
#'
#'
#' @export
#'
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom future plan multisession
sitMfaComparison <- function(iterations = 500) {
  run_fishery <- function(catch, fishery_params) {
    fishery_params$catch <- catch

    sim_result <-
      createCohort(cohort_size = 50000L,
                   adclip_rate = 0.5,
                   pns = 0.5) |>
      sequencialFisherySim(cohort_encounter_rate = 0.3,
                           fishery_params = fishery_params)

    fishery_summary <- summarizeFishery(sim_result$fishery_df)

    kept_mark <-
      subset(fishery_summary,
             is_clipped == TRUE & outcome == OutcomeKept,
             select = total) |>
      sum()

    kept_mark_cohort <-
      subset(fishery_summary,
             is_clipped == TRUE & outcome == OutcomeKept & is_cohort == TRUE,
             select = total) |>
      sum()

    legal_release_mark <-
      subset(fishery_summary,
             is_clipped == TRUE & outcome == OutcomeReleased & is_legal_size == TRUE,
             select = total) |>
      sum()

    kept_unmark <-
      subset(fishery_summary,
             is_clipped == FALSE & outcome == OutcomeKept & is_legal_size == TRUE,
             select = total) |>
      sum()

    legal_release_unmark <-
      subset(fishery_summary,
             is_clipped == FALSE & outcome == OutcomeReleased & is_legal_size == TRUE,
             select = total) |>
      sum()

    post_unmark_cohort <-
      subset(sim_result$cohort_df,
             is_dead == FALSE & is_clipped == FALSE) |>
      nrow()

    sim_pre_fishery_mark_cohort <-
      subset(sim_result$cohort_df, is_clipped == TRUE) |>
      nrow()

    sim_pre_fishery_unmark_cohort <-
      subset(sim_result$cohort_df, is_clipped == FALSE) |>
      nrow()

    sim_post_fishery_mark_cohort <-
      subset(sim_result$cohort_df, is_clipped == TRUE & is_dead == FALSE) |>
      nrow()

    sim_post_fishery_unmark_cohort <-
      subset(sim_result$cohort_df, is_clipped == FALSE & is_dead == FALSE) |>
      nrow()

    sit_est_fishery <-
      sitMethod(kept_mark,
                legal_release_unmark,
                kept_mark_cohort,
                sim_post_fishery_mark_cohort,
                fishery_params$legal_release_mort_rate,
                fishery_params$non_legal_release_mort_rate,
                attr(sim_result$cohort_df, "pns"),
                fishery_params$drop_off_rate,
                escapement_unmark_cohort = sim_post_fishery_unmark_cohort)

    colnames(sit_est_fishery) <- paste0("sit_", colnames(sit_est_fishery))

    mfa_est_fishery <-
      mfaMethod(kept_mark,
                legal_release_mark,
                kept_unmark,
                legal_release_unmark,
                kept_mark_cohort,
                sim_post_fishery_mark_cohort,
                fishery_params$legal_release_mort_rate,
                fishery_params$non_legal_release_mort_rate,
                attr(sim_result$cohort_df, "pns"),
                fishery_params$drop_off_rate,
                escapement_unmark_cohort = sim_post_fishery_unmark_cohort)

    colnames(mfa_est_fishery) <- paste0("mfa_", colnames(mfa_est_fishery))

    method_compare_df <-
      data.frame(total_catch = catch,
                 sim_pre_fishery_mark_cohort = sim_pre_fishery_mark_cohort,
                 sim_pre_fishery_unmark_cohort = sim_pre_fishery_unmark_cohort,
                 sim_post_fishery_mark_cohort = sim_post_fishery_mark_cohort,
                 sim_post_fishery_unmark_cohort = sim_post_fishery_unmark_cohort) |>
      cbind(sit_est_fishery) |>
      cbind(mfa_est_fishery)

    return(method_compare_df)
  }

  future::plan(future::multisession, workers = 5)

   method_compare_df <-
     furrr::future_map_dfr(as.integer(runif(iterations, 100, 5000)),
                           run_fishery,
                           AdclipSelectiveFishery,
                           .options = furrr::furrr_options(seed = T))

  return(method_compare_df)
}
