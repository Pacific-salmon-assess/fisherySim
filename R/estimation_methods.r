#' Mixed Fishery Adjustment Method
#'
#' @param kept_mark Total kept marked catch in the fishery
#' @param legal_release_mark Legal released marked catch in the fishery
#' @param kept_unmark Total kept unmarked catch in the fishery
#' @param legal_release_unmark Legal released unmarked catch in the fishery
#' @param kept_mark_cohort Kept mark catch of the cohort (e.g. CWT estimate)
#' @param escapement_mark_cohort Abundance of marked cohort after the fishery
#' @param legal_release_mort_rate Mortality rate of legal size releases
#' @param nonlegal_release_mort_rate Mortality rate of non-legal size releases
#' @param cohort_prop_nonlegal Proportion of cohort that is not legal size
#' @param drop_off_rate Mortality rate of non-legal size releases
#' @param escapement_unmark_cohort Mortality rate of non-legal size releases
#' @param terminal_unmark_cohort Mortality rate of non-legal size releases
#'
#' @return Fish/row number of a fish that is encountered in a fishery
#'
#' @export
#'
mfaMethod <- function(kept_mark,
                      legal_release_mark,
                      kept_unmark,
                      legal_release_unmark,
                      kept_mark_cohort,
                      escapement_mark_cohort,
                      legal_release_mort_rate,
                      nonlegal_release_mort_rate,
                      cohort_prop_nonlegal,
                      drop_off_rate,
                      escapement_unmark_cohort = NULL,
                      terminal_unmark_cohort = NULL) {

  if(is.null(escapement_unmark_cohort) && is.null(terminal_unmark_cohort) ) {
    stop("Either the unmarked cohort escapement or terminal unmarked cohort must be provided")
  }

  if(!is.null(escapement_unmark_cohort) && !is.null(terminal_unmark_cohort) ) {
    stop("Only the the unmarked cohort escapement or terminal unmarked cohort can be provided")
  }

  #drop_off_ratio <- drop_off_rate / (1-drop_off_rate)
  drop_off_ratio <- drop_off_rate

  legal_catch_mark <- kept_mark + legal_release_mark
  legal_catch_unmark <- kept_unmark + legal_release_unmark

  legal_release_mark_cohort <- legal_release_mark * kept_mark_cohort / kept_mark
  legal_catch_mark_cohort <- legal_release_mark_cohort + kept_mark_cohort

  nonlegal_rel_mark_cohort <- legal_catch_mark_cohort * cohort_prop_nonlegal / (1-cohort_prop_nonlegal)

  drop_mark_cohort <- (legal_catch_mark_cohort +
                         nonlegal_rel_mark_cohort) * drop_off_ratio

  terminal_mark_cohort <- kept_mark_cohort +
    legal_release_mort_rate * legal_release_mark_cohort +
    nonlegal_release_mort_rate * nonlegal_rel_mark_cohort +
    escapement_mark_cohort + drop_mark_cohort

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

  #Dropoff Mortality Rate of Cohort
  mu_m_d <- drop_mark_cohort / terminal_mark_cohort

  F_u_d <- mu_m_d * Z_m / A_m

  Z_u <- F_u_nlr + F_u_lr + F_u_k + F_u_d

  if(is.null(terminal_unmark_cohort)) {
    terminal_unmark_cohort <- escapement_unmark_cohort / exp(-Z_u)
  } else {
    escapement_unmark_cohort <- terminal_unmark_cohort * exp(-Z_u)
  }
  return(data.frame(post_fishery_mark_cohort = escapement_mark_cohort,
                    pre_fishery_mark_cohort = terminal_mark_cohort,
                    post_fishery_unmark_cohort = escapement_unmark_cohort,
                    pre_fishery_unmark_cohort = terminal_unmark_cohort))
}


#' Single Index Tag Method
#'
#' @param kept_mark Total kept marked catch in the fishery
#' @param legal_release_unmark Legal released unmarked catch in the fishery
#' @param kept_mark_cohort Kept mark catch of the cohort (e.g. CWT estimate)
#' @param escapement_mark_cohort Abundance of marked cohort after the fishery
#' @param legal_release_mort_rate Mortality rate of legal size releases
#' @param nonlegal_release_mort_rate Mortality rate of non-legal size releases
#' @param cohort_prop_nonlegal Proportion of cohort that is not legal size
#' @param drop_off_rate Mortality rate of non-legal size releases
#' @param escapement_unmark_cohort Mortality rate of non-legal size releases
#' @param terminal_unmark_cohort Mortality rate of non-legal size releases
#'
#' @return Fish/row number of a fish that is encountered in a fishery
#'
#' @export
#'
sitMethod <- function(kept_mark,
                      legal_release_unmark,
                      kept_mark_cohort,
                      escapement_mark_cohort,
                      legal_release_mort_rate,
                      nonlegal_release_mort_rate,
                      cohort_prop_nonlegal,
                      drop_off_rate,
                      escapement_unmark_cohort = NULL,
                      terminal_unmark_cohort = NULL) {

  if(is.null(escapement_unmark_cohort) && is.null(terminal_unmark_cohort) ) {
    stop("Either the unmarked cohort escapement or terminal unmarked cohort must be provided")
  }

  if(!is.null(escapement_unmark_cohort) && !is.null(terminal_unmark_cohort) ) {
    stop("Only the the unmarked cohort escapement or terminal unmarked cohort can be provided")
  }

  drop_off_ratio <- drop_off_rate

  nonlegal_rel_mark_cohort <- kept_mark_cohort * cohort_prop_nonlegal / (1-cohort_prop_nonlegal)

  drop_mark_cohort <- (kept_mark_cohort + nonlegal_rel_mark_cohort) * drop_off_ratio

  terminal_mark_cohort <- kept_mark_cohort +
    nonlegal_release_mort_rate * nonlegal_rel_mark_cohort +
    escapement_mark_cohort + drop_mark_cohort

  A_m <- 1 - escapement_mark_cohort / terminal_mark_cohort

  Z_m <- -1 * log(1 - A_m)

  #Legal Release of Unmarked Cohort
  mu_u_lr <- legal_release_mort_rate * kept_mark_cohort / terminal_mark_cohort

  F_u_lr <- mu_u_lr * Z_m / A_m

  #Non-Legal Release of Unmarked Cohort
  mu_m_nlr <- nonlegal_release_mort_rate * nonlegal_rel_mark_cohort / terminal_mark_cohort

  F_u_nlr <- mu_m_nlr * Z_m / A_m

  #Dropoff Mortality Rate of Cohort
  mu_m_d <- drop_mark_cohort / terminal_mark_cohort

  F_u_d <- mu_m_d * Z_m / A_m

  Z_u <- F_u_nlr + F_u_lr + F_u_d

  if(is.null(terminal_unmark_cohort)) {
    terminal_unmark_cohort <- escapement_unmark_cohort / exp(-Z_u)
  } else {
    escapement_unmark_cohort <- terminal_unmark_cohort * exp(-Z_u)
  }
  return(data.frame(post_fishery_mark_cohort = escapement_mark_cohort,
                    pre_fishery_mark_cohort = terminal_mark_cohort,
                    post_fishery_unmark_cohort = escapement_unmark_cohort,
                    pre_fishery_unmark_cohort = terminal_unmark_cohort))
}
