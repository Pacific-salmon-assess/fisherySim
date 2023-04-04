
#' Non-Selective Fishery with no disproportionate release based on adclip status
#'
#' @export
NonSelectiveFishery <-
  list(catch = 1000,
       adclip_rate  = 0.4,
       legal_release_mort_rate = 0.15,
       nonlegal_release_mort_rate = 0.15,
       legal_drop_off_mort_rate = 0.35,
       nonlegal_drop_off_mort_rate = 0.35,
       unclip_release_rate = 0.1,
       clip_release_rate = 0.1,
       prop_non_legal_size = 0.2)

#' Mixed Adipose Clipped Selective Fishery with higher URE and MRE
#'
#' @export
MixedSelectiveFishery <-
  list(catch = 1000,
       adclip_rate  = 0.4,
       legal_release_mort_rate = 0.15,
       nonlegal_release_mort_rate = 0.15,
       legal_drop_off_mort_rate = 0.35,
       nonlegal_drop_off_mort_rate = 0.35,
       unclip_release_rate = 0.7,
       clip_release_rate = 0.3,
       prop_non_legal_size = 0.1)


#' Adipose Clip Selective Fishery with some URE and MRE
#'
#' @export
AdclipSelectiveFishery <-
  list(catch = 1000,
       adclip_rate  = 0.4,
       legal_release_mort_rate = 0.15,
       nonlegal_release_mort_rate = 0.15,
       legal_drop_off_mort_rate = 0.35,
       nonlegal_drop_off_mort_rate = 0.35,
       unclip_release_rate = 0.9,
       clip_release_rate = 0.1,
       prop_non_legal_size = 0.1)



#' DGM Preterminal Fishery Parameters used for the CYER report
#'
#' @export
DgmPreTerminal <-
  list(catch = 1000,
       adclip_rate  = 0.5,
       legal_release_mort_rate = 0.25,
       nonlegal_release_mort_rate = 0.35,
       legal_drop_off_mort_rate = 0.05,
       nonlegal_drop_off_mort_rate = 0.05,
       unclip_release_rate = 0.9,
       clip_release_rate = 0.1,
       prop_non_legal_size = 0.1)


#' DGM Terminal Fishery Parameters used for the CYER report
#'
#' @export
DgmTerminal <-
  list(catch = 1000,
       adclip_rate  = 0.5,
       legal_release_mort_rate = 0.20,
       nonlegal_release_mort_rate = 0.5,
       legal_drop_off_mort_rate = 0.05,
       nonlegal_drop_off_mort_rate = 0.05,
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
#' @param nonlegal_release_mort_rate Mortality rate of non-legal size releases
#' @param legal_drop_off_mort_rate Drop-off mortality rate of legal size catch
#' @param nonlegal_drop_off_mort_rate Drop-off mortality rate of non-legal size catch
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
                          nonlegal_release_mort_rate,
                          legal_drop_off_mort_rate,
                          nonlegal_drop_off_mort_rate,
                          unclip_release_rate,
                          clip_release_rate,
                          prop_non_legal_size) {
  fishery_param <-
    list(catch = catch,
         adclip_rate  = adclip_rate,
         legal_release_mort_rate = legal_release_mort_rate,
         nonlegal_release_mort_rate = nonlegal_release_mort_rate,
         legal_drop_off_mort_rate = legal_drop_off_mort_rate,
         nonlegal_drop_off_mort_rate = nonlegal_drop_off_mort_rate,
         unclip_release_rate = unclip_release_rate,
         clip_release_rate = clip_release_rate,
         prop_non_legal_size = prop_non_legal_size)

  return(fishery_param)
}


