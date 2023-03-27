
#' @export
NonSelectiveFishery <-
  list(catch = 1000,
       adclip_rate  = 0.4,
       legal_release_mort_rate = 0.15,
       non_legal_release_mort_rate = 0.15,
       drop_off_rate = 0.05,
       drop_off_mort_rate = 1.0,
       unclip_release_rate = 0.1,
       clip_release_rate = 0.1,
       prop_non_legal_size = 0.2)

#' @export
MixedSelectiveFishery <-
  list(catch = 1000,
       adclip_rate  = 0.4,
       legal_release_mort_rate = 0.15,
       non_legal_release_mort_rate = 0.15,
       drop_off_rate = 0.05,
       drop_off_mort_rate = 1.0,
       unclip_release_rate = 0.7,
       clip_release_rate = 0.3,
       prop_non_legal_size = 0.1)


#' @export
AdclipSelectiveFishery <-
  list(catch = 1000,
       adclip_rate  = 0.4,
       legal_release_mort_rate = 0.15,
       non_legal_release_mort_rate = 0.15,
       drop_off_rate = 0.05,
       drop_off_mort_rate = 1.0,
       unclip_release_rate = 0.9,
       clip_release_rate = 0.1,
       prop_non_legal_size = 0.1)


#' Setup Fishery Parameters
#'
#' Combine the fishery parameters into a list for use by the fishery
#' simulation
#'
#' @param catch Total kept catch that the fishery runs until achieving
#' @param adclip_rate Adipose-clip rate of non-cohort fish in the fishery
#' @param legal_release_mort_rate Mortality rate of legal size releases
#' @param non_legal_release_mort_rate Mortality rate of non-legal size releases
#' @param drop_off_rate Drop-off rate of fish
#' @param drop_off_mort_rate Mortality rate of drop-off fish
#' @param unclip_release_rate Release rate of non-adipose-clipped fish
#' @param clip_release_rate Release rate of adipose-clipped fish
#' @param prop_non_legal_size Proportion of non-cohort fish not legal size
#'
#' @return A list with the fishery parameters for use by a fishery simulation
#'
#'
#' @export
#'
setupFishery <- function (catch,
                          adclip_rate,
                          legal_release_mort_rate,
                          non_legal_release_mort_rate,
                          drop_off_rate,
                          drop_off_mort_rate,
                          unclip_release_rate,
                          clip_release_rate,
                          prop_non_legal_size) {
  fishery_param <-
    list(catch = catch,
         adclip_rate  = adclip_rate,
         legal_release_mort_rate = legal_release_mort_rate,
         non_legal_release_mort_rate = non_legal_release_mort_rate,
         drop_off_rate = drop_off_rate,
         drop_off_mort_rate = drop_off_mort_rate,
         unclip_release_rate = unclip_release_rate,
         clip_release_rate = clip_release_rate,
         prop_non_legal_size = prop_non_legal_size)

  return(fishery_param)
}

