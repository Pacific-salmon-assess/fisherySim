#' Run Simulation with results Compared to SIT and MFA methods
#'
#' @param iterations Number of independent simulations to run
#' @param fishery_params Fishery parameters used for the simulations
#' @param cohort_encounter_rate Cohort encounter rate in the fishery
#' @param cohort)size Cohort initial size
#' @param cohort_pns Cohort Proportion Non-Legal Size
#'
#' @export
#'
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom future plan multisession
sitMfaComparison <- function(iterations = 500,
                             fishery_params = AdclipSelectiveFishery,
                             cohort_encounter_rate = 0.6,
                             cohort_size = 200000L,
                             cohort_pns = 0.2) {
  run_fishery <- function(catch, fishery_params) {
    fishery_params$catch <- catch

    sim_result <-
      createCohort(cohort_size = cohort_size,
                   adclip_rate = 0.5,
                   pns = cohort_pns) |>
      sequencialFisherySim(cohort_encounter_rate = cohort_encounter_rate,
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
                fishery_params$legal_release_mort_rate,
                fishery_params$nonlegal_release_mort_rate,
                fishery_params$legal_drop_off_mort_rate,
                fishery_params$nonlegal_drop_off_mort_rate,
                attr(sim_result$cohort_df, "pns"),
                escapement_mark_cohort = sim_post_fishery_mark_cohort,
                escapement_unmark_cohort = sim_post_fishery_unmark_cohort)

    colnames(sit_est_fishery) <- paste0("sit_", colnames(sit_est_fishery))

    mfa_est_fishery <-
      mfaMethod(kept_mark = kept_mark,
                legal_release_mark = legal_release_mark,
                kept_unmark = kept_unmark,
                legal_release_unmark = legal_release_unmark,
                kept_mark_cohort = kept_mark_cohort,
                legal_release_mort_rate = fishery_params$legal_release_mort_rate,
                nonlegal_release_mort_rate = fishery_params$nonlegal_release_mort_rate,
                fishery_params$legal_drop_off_mort_rate,
                fishery_params$nonlegal_drop_off_mort_rate,
                cohort_prop_nonlegal = attr(sim_result$cohort_df, "pns"),
                escapement_mark_cohort = sim_post_fishery_mark_cohort,
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
                          fishery_params = fishery_params,
                          .options = furrr::furrr_options(seed = T))

  #method_compare_df <-
  #  purrr::map_dfr(as.integer(runif(iterations, 100, 5000)),
  #                 run_fishery,
  #                 fishery_params = fishery_params)
  return(method_compare_df)
}
