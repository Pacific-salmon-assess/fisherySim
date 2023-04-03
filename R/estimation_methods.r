#' Mixed Fishery Adjustment Method
#'
#' @param kept_mark Total kept marked catch in the fishery
#' @param legal_release_mark Legal released marked catch in the fishery
#' @param kept_unmark Total kept unmarked catch in the fishery
#' @param legal_release_unmark Legal released unmarked catch in the fishery
#' @param kept_mark_cohort Kept mark catch of the cohort (e.g. CWT estimate)
#' @param legal_release_mort_rate Mortality rate of legal size releases
#' @param nonlegal_release_mort_rate Mortality rate of non-legal size releases
#' @param legal_drop_off_mort_rate Drop-off and mortality rate of legal-size caught fish
#' @param nonlegal_drop_off_mort_rate Drop-off and mortality rate of non-legal-size caught fish
#' @param cohort_prop_nonlegal Proportion of cohort that is not legal size
#' @param escapement_mark_cohort Abundance of marked cohort after the fishery
#' @param escapement_unmark_cohort Unmarked cohort size after the fishery
#' @param terminal_mark_cohort Marked cohort size before the fishery
#' @param terminal_unmark_cohort Unmarked cohort size before the fishery#'
#'
#' @return A data.frame with the cohort size before and after the fisher based on adclip status
#'
#' @export
#'
mfaMethod <- function(kept_mark,
                      legal_release_mark,
                      kept_unmark,
                      legal_release_unmark,
                      kept_mark_cohort,
                      legal_release_mort_rate,
                      nonlegal_release_mort_rate,
                      legal_drop_off_mort_rate,
                      nonlegal_drop_off_mort_rate,
                      cohort_prop_nonlegal,
                      escapement_mark_cohort = NULL,
                      escapement_unmark_cohort = NULL,
                      terminal_mark_cohort = NULL,
                      terminal_unmark_cohort = NULL) {
  if ((!is.null(escapement_mark_cohort) && !is.null(escapement_unmark_cohort) )  ||
      (!is.null(terminal_mark_cohort) && !is.null(terminal_unmark_cohort))) {
    if (all(!is.null(escapement_mark_cohort), !is.null(escapement_unmark_cohort),
            !is.null(terminal_mark_cohort), !is.null(terminal_unmark_cohort))) {
      stop("You can only provide the terminal or the escapement cohort size, not both")
    }
  } else {
    stop("Either the terminal or escapement marked and unmarked cohort must be provided.")
  }
  legal_catch_mark <- kept_mark + legal_release_mark
  legal_catch_unmark <- kept_unmark + legal_release_unmark

  legal_release_mark_cohort <- legal_release_mark * kept_mark_cohort / kept_mark
  legal_catch_mark_cohort <- legal_release_mark_cohort + kept_mark_cohort

  nonlegal_rel_mark_cohort <- legal_catch_mark_cohort * cohort_prop_nonlegal / (1-cohort_prop_nonlegal)

  legal_drop_off_mort_ratio <- legal_drop_off_mort_rate / (1 - legal_drop_off_mort_rate)
  legal_drop_mark_cohort <- legal_catch_mark_cohort * legal_drop_off_mort_ratio


  nonlegal_drop_off_mort_ratio <- nonlegal_drop_off_mort_rate / (1 - nonlegal_drop_off_mort_rate)
  nonlegal_drop_mark_cohort <- nonlegal_rel_mark_cohort * nonlegal_drop_off_mort_ratio

  if(is.null(terminal_mark_cohort)) {
    terminal_mark_cohort <-
      kept_mark_cohort +
      legal_release_mort_rate * legal_release_mark_cohort +
      nonlegal_release_mort_rate * nonlegal_rel_mark_cohort +
      escapement_mark_cohort + legal_drop_mark_cohort + nonlegal_drop_mark_cohort
  } else {
    escapement_mark_cohort <-
      terminal_mark_cohort - kept_mark_cohort -
      legal_release_mort_rate * legal_release_mark_cohort -
      nonlegal_release_mort_rate * nonlegal_rel_mark_cohort -
      legal_drop_mark_cohort - nonlegal_drop_mark_cohort
  }


  A_m <- 1 - escapement_mark_cohort / terminal_mark_cohort

  Z_m <- -1 * log(1 - A_m)

  mu_u_k <- kept_unmark / legal_catch_unmark * legal_catch_mark_cohort / terminal_mark_cohort

  F_u_k <- mu_u_k * Z_m / A_m

  #Legal Release of Unmarked Cohort
  mu_u_lr <- legal_release_mort_rate * legal_release_unmark / legal_catch_unmark *
    legal_catch_mark_cohort / terminal_mark_cohort

  F_u_lr <- mu_u_lr * Z_m / A_m

  #Non-Legal Release of Unmarked Cohort
  mu_u_nlr <- nonlegal_release_mort_rate * legal_catch_unmark * cohort_prop_nonlegal / (1- cohort_prop_nonlegal) / legal_catch_unmark *
    legal_catch_mark_cohort / terminal_mark_cohort


  mu_m_nlr <- nonlegal_release_mort_rate * nonlegal_rel_mark_cohort / terminal_mark_cohort

  F_u_nlr <- mu_m_nlr * Z_m / A_m

  #Legal Dropoff Mortality Rate of Cohort
  mu_m_ld <- legal_drop_mark_cohort / terminal_mark_cohort

  F_u_ld <- mu_m_ld * Z_m / A_m

  #Non-Legal Dropoff Mortality Rate of Cohort
  mu_m_nld <- nonlegal_drop_mark_cohort / terminal_mark_cohort

  F_u_nld <- mu_m_nld * Z_m / A_m

  #Unmarked Instantaneous Mortality
  Z_u <- F_u_nlr + F_u_lr + F_u_k + F_u_ld + F_u_nld

  if(is.null(terminal_unmark_cohort)) {
    terminal_unmark_cohort <- escapement_unmark_cohort / exp(-Z_u)
  } else {
    escapement_unmark_cohort <- terminal_unmark_cohort * exp(-Z_u)
  }
  return(data.frame(post_fishery_mark_cohort = escapement_mark_cohort,
                    pre_fishery_mark_cohort = terminal_mark_cohort,
                    mark_cohort_mortalities = terminal_mark_cohort - escapement_mark_cohort,
                    post_fishery_unmark_cohort = escapement_unmark_cohort,
                    pre_fishery_unmark_cohort = terminal_unmark_cohort,
                    unmark_cohort_mortalities = terminal_unmark_cohort - escapement_unmark_cohort))
}


#' Single Index Tag Method
#'
#' @param kept_mark Total kept marked catch in the fishery
#' @param legal_release_unmark Legal released unmarked catch in the fishery
#' @param kept_mark_cohort Kept mark catch of the cohort (e.g. CWT estimate)
#' @param legal_release_mort_rate Mortality rate of legal size releases
#' @param nonlegal_release_mort_rate Mortality rate of non-legal size releases
#' @param legal_drop_off_mort_rate Drop-off and mortality rate of legal caught fish
#' @param nonlegal_drop_off_mort_rate Drop-off and mortality rate of non-legal caught fish
#' @param cohort_prop_nonlegal Proportion of cohort that is not legal size
#' @param escapement_mark_cohort Marked size cohort after the fishery
#' @param escapement_unmark_cohort Unmarked cohort size after the fishery
#' @param terminal_mark_cohort Marked cohort size before the fishery
#' @param terminal_unmark_cohort Unmarked cohort size before the fishery
#'
#' @return A data.frame with the cohort size before and after the fisher based on adclip status
#'
#' @export
#'
sitMethod <- function(kept_mark,
                      legal_release_unmark,
                      kept_mark_cohort,
                      legal_release_mort_rate,
                      nonlegal_release_mort_rate,
                      legal_drop_off_mort_rate,
                      nonlegal_drop_off_mort_rate,
                      cohort_prop_nonlegal,
                      escapement_mark_cohort = NULL,
                      escapement_unmark_cohort = NULL,
                      terminal_mark_cohort = NULL,
                      terminal_unmark_cohort = NULL) {
  if ((!is.null(escapement_mark_cohort) && !is.null(escapement_unmark_cohort) )  ||
      (!is.null(terminal_mark_cohort) && !is.null(terminal_unmark_cohort))) {
    if (all(!is.null(escapement_mark_cohort), !is.null(escapement_unmark_cohort),
            !is.null(terminal_mark_cohort), !is.null(terminal_unmark_cohort))) {
      stop("You can only provide the terminal or the escapement cohort size, not both")
    }
  } else {
    stop("Either the terminal or escapement marked and unmarked cohort must be provided.")
  }

  nonlegal_rel_mark_cohort <- kept_mark_cohort * cohort_prop_nonlegal / (1-cohort_prop_nonlegal)

  legal_drop_off_ratio <- legal_drop_off_mort_rate / (1 - legal_drop_off_mort_rate)
  legal_drop_mark_cohort <- kept_mark_cohort  * legal_drop_off_ratio

  nonlegal_drop_off_ratio <- nonlegal_drop_off_mort_rate / (1 - nonlegal_drop_off_mort_rate)
  nonlegal_drop_mark_cohort <- nonlegal_rel_mark_cohort * nonlegal_drop_off_mort_rate

  if(is.null(terminal_mark_cohort)) {
    terminal_mark_cohort <-
      kept_mark_cohort + nonlegal_release_mort_rate * nonlegal_rel_mark_cohort +
      escapement_mark_cohort + legal_drop_mark_cohort + nonlegal_drop_mark_cohort
  } else {
    escapement_mark_cohort <-
      terminal_mark_cohort - kept_mark_cohort -
      nonlegal_release_mort_rate * nonlegal_rel_mark_cohort -
      legal_drop_mark_cohort - nonlegal_drop_mark_cohort
  }

  A_m <- 1 - escapement_mark_cohort / terminal_mark_cohort

  Z_m <- -1 * log(1 - A_m)

  #Legal Release of Unmarked Cohort
  mu_u_lr <- legal_release_mort_rate * kept_mark_cohort / terminal_mark_cohort

  F_u_lr <- mu_u_lr * Z_m / A_m

  #Non-Legal Release of Unmarked Cohort
  mu_m_nlr <- nonlegal_release_mort_rate * nonlegal_rel_mark_cohort / terminal_mark_cohort

  F_u_nlr <- mu_m_nlr * Z_m / A_m

  #Legal Dropoff Mortality Rate of Cohort
  mu_m_ld <- legal_drop_mark_cohort / terminal_mark_cohort

  F_u_ld <- mu_m_ld * Z_m / A_m

  #Non-Legal Dropoff Mortality Rate of Cohort
  mu_m_nld <- nonlegal_drop_mark_cohort / terminal_mark_cohort

  F_u_nld <- mu_m_nld * Z_m / A_m

  #Unmarked Instantaneous Mortality
  Z_u <- F_u_nlr + F_u_lr + F_u_ld + F_u_nld

  if(is.null(terminal_unmark_cohort)) {
    terminal_unmark_cohort <- escapement_unmark_cohort / exp(-Z_u)
  } else {
    escapement_unmark_cohort <- terminal_unmark_cohort * exp(-Z_u)
  }
  return(data.frame(post_fishery_mark_cohort = escapement_mark_cohort,
                    pre_fishery_mark_cohort = terminal_mark_cohort,
                    mark_cohort_mortalities = terminal_mark_cohort - escapement_mark_cohort,
                    post_fishery_unmark_cohort = escapement_unmark_cohort,
                    pre_fishery_unmark_cohort = terminal_unmark_cohort,
                    unmark_cohort_mortalities = terminal_unmark_cohort - escapement_unmark_cohort))
}
