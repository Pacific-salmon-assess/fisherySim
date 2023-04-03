#' Outcome of the fishing event was No Encounter
#' @export
OutcomeNoEncounter <- "NO ENCOUNTER"

#' Outcome of the fishing event was Drop-Off
#' @export
OutcomeDropOff <- "DROP-OFF"

#' Outcome of the fishing event was Released fish
#' @export
OutcomeReleased <- "RELEASED"

#' Outcome of the fishing event was Kept Fish
#' @export
OutcomeKept <- "KEPT"

#' Create Cohort Data Frame
#'
#' Create a data frame of random fish based on the
#' cohort size and the adipose clip rate
#'
#' @param adclip_rate Adipose clip rate of the cohort (related to lambda)
#' @param pns Proportion not legal size
#' @param cohort_size Size of the cohort
#'
#' @return A data frame with `cohort_size` rows
#'
#' @export
#'
#' @importFrom stats runif
#'
createCohort <- function(adclip_rate = 0.5,
                         pns = 0.2,
                         cohort_size = 10000L) {
  cohort_df <-
    data.frame(fish_number = seq_len(cohort_size),
               is_clipped = runif(cohort_size) < adclip_rate,
               is_legal_size = runif(cohort_size) > pns,
               is_dead = FALSE)

  attr(cohort_df, "pns") <- pns
  attr(cohort_df, "adclip_rate") <- adclip_rate

  return(cohort_df)
}

#' (Re-)Select a random cohort fish
#'
#' Select a random cohort fish, if it is dead select a different one
#' Do this until you find a fish or you have made as many tries as there are
#' fish in the cohort.
#'
#' @param cohort_df Cohort data frame
#' @param potential_fish A random number to try as first fish
#'
#' @return Fish/row number of a fish that is encountered in a fishery
#'
selectFish <- function(cohort_df, potential_fish) {
  find <- 1
  fish_number <- potential_fish

  while(cohort_df$is_dead[fish_number] == TRUE) {
    #Keep looking for an alive fish.
    #Used a random number instead of randomly selecting from alive fix
    # is assumed to be faster.  We save all the time of filtering the cohort
    # and randomly selecting from non-dead fish.  We should typically on fish a
    # small component of the overall cohort and random fish will likely be alive
    fish_number <- as.integer(runif(1, min = 1, max = nrow(cohort_df)))

    if(find > nrow(cohort_df)) {
      #if we search randomly for a fish as many times as there are fish
      #in the cohort, this is likely very few or no fish left
      stop("Ran out of fish")
    }
    find <- find + 1
  }
  return(fish_number)
}

#' Sequential Fishery Simulation
#'
#' Sequentially simulate a fishery up to a total kept catch based on individual fishing
#' (i.e. kept or released) events.  Events may be slightly more or less to accommodate
#' the random probability of drop-off events.  If a fish from the cohort dies, it is
#' no longer available for selection in the fishery
#'
#' @param cohort_df Cohort data frame
#' @param cohort_encounter_rate Encounter rate of the cohort
#' @param fishery_params Fishery parameters
#'
#' @return Fish/row number of a fish that is encountered in a fishery
#'
#' @export
#'
sequencialFisherySim <- function(cohort_df,
                                 cohort_encounter_rate = 0.7,
                                 fishery_params = NULL) {

  if(is.null(fishery_params)) {
    stop("No Fishery Parameters provided for the fishery simulation")
  }
  event_seq <- seq_len(fishery_params$catch * 10)
  event_len <- length(event_seq)

  total_kept_catch <- 0L
  outcome <- rep(NA_character_, event_len)
  mortality <- rep(NA, event_len)
  fish_number <- rep(NA_integer_, event_len)

  potential_fish <- as.integer(runif(event_len, min = 1, max = nrow(cohort_df)))
  encounter_event <- runif(event_len)
  drop_event <- runif(event_len)
  rel_event <- runif(event_len)
  mort_event <- runif(event_len)
  clip_event <- runif(event_len)
  legal_size_event <- runif(event_len)
  is_clipped <- rep(NA, event_len)
  is_legal_size <- rep(NA, event_len)

  for(event_id in event_seq) {
    if(total_kept_catch  >= fishery_params$catch) {
      #Total allowable catch is reach, so no more fishing events to process
      break
    }

    if(encounter_event[event_id] <= cohort_encounter_rate) {
      #Encountered fish is from the provided cohort
      cohort_fish_number <- selectFish(cohort_df,
                                       potential_fish[event_id])

      if(cohort_fish_number > nrow(cohort_df)) {
        stop("Bad fish number")
      }

    } else {
      #Encountered fish is not from the provided cohort
      cohort_fish_number <- NA_integer_
    }

    fish_number[event_id] <- cohort_fish_number

    #The fish was caught, we need to figure out if it was kept or released
    if(!is.na(cohort_fish_number)) {
      #If the fish was from our model cohort, use the fish adclip status
      is_clipped[event_id] <- cohort_df$is_clipped[cohort_fish_number]

      #If the fish was from our model cohort, use the fish legal status
      is_legal_size[event_id] <- cohort_df$is_legal_size[cohort_fish_number]
    } else {
      #If the fish was NOT from our model cohort, use randomly assign clip
      # based on the default fishery adclip rate
      is_clipped[event_id] <- clip_event[event_id] <= fishery_params$adclip_rate


      #If the fish was NOT from our model cohort, use randomly assign
      # vulnerability status based on the default fishery vulnerability rate
      is_legal_size[event_id] <- legal_size_event[event_id] >= fishery_params$prop_non_legal_size
    }

    if(is_legal_size[event_id] == FALSE) {
      #Fish caught is non-legal size
      if(drop_event[event_id] <= fishery_params$nonlegal_drop_off_mort_rate) {
        #Fish died after dropping off
        outcome[event_id] <- OutcomeDropOff
        mortality[event_id] <- TRUE
        if(!is.na(cohort_fish_number)) {
          #Kill the fish in the cohort data frame
          cohort_df$is_dead[cohort_fish_number] <- TRUE
        }
      } else {
        #If the fish is not legal, then 100% chance of release
        outcome[event_id] <- OutcomeReleased

        if(mort_event[event_id] <= fishery_params$nonlegal_release_mort_rate) {
          #The fish died immediately after being released
          mortality[event_id] <- TRUE
          if(!is.na(cohort_fish_number)) {
            #Kill the fish in the cohort data frame
            cohort_df$is_dead[cohort_fish_number] <- TRUE
          }
        } else {
          mortality[event_id] <- FALSE
        }
      }
    } else {
      #Fish caught is legal size
      if(drop_event[event_id] <= fishery_params$legal_drop_off_mort_rate) {
        #Fish died after dropping off
        outcome[event_id] <- OutcomeDropOff
        mortality[event_id] <- TRUE
        if(!is.na(cohort_fish_number)) {
          #Kill the fish in the cohort data frame
          cohort_df$is_dead[cohort_fish_number] <- TRUE
        }
      } else {
        #For legal fish, select the release rate based on clip status
        fish_release_rate <- ifelse(is_clipped[event_id],
                                    fishery_params$clip_release_rate,
                                    fishery_params$unclip_release_rate)

        if(rel_event[event_id] <= fish_release_rate) {
          #The fish was released
          outcome[event_id] <- OutcomeReleased
          if(mort_event[event_id] <= fishery_params$legal_release_mort_rate) {
            #The fish died immediately after being released
            mortality[event_id] <- TRUE
            if(!is.na(cohort_fish_number)) {
              #Kill the fish in the cohort data frame
              cohort_df$is_dead[cohort_fish_number] <- TRUE
            }
          } else {
            mortality[event_id] <- FALSE
          }
        } else {
          #The fish was kept and died
          total_kept_catch <- total_kept_catch + 1L
          outcome[event_id] <- OutcomeKept
          mortality[event_id] <- TRUE
          if(!is.na(cohort_fish_number)) {
            #Kill the fish in the cohort data frame
            cohort_df$is_dead[cohort_fish_number] <- TRUE
          }
        }
      }
    }
  }

  if(!is.na(outcome[length(outcome)])) {
    stop("Ran out of fishing events :-(")
  }

  fishery_df <-
    data.frame(event_id = event_seq,
               is_clipped  = is_clipped,
               is_legal_size = is_legal_size,
               outcome = outcome,
               mortality = mortality,
               fish_number = fish_number)

  #remove empty fishing event rows that had no outcome
  fishery_df <- fishery_df[!is.na(fishery_df$outcome),]

  return(list(fishery_df = fishery_df, cohort_df = cohort_df))
}

#' Summarize Fishery Simulation
#'
#' Summarize the individual fishing events from a simulated fishery
#'
#' @param fishery_df Data frame of all fishing events from a simulation
#'
#' @return Fish/row number of a fish that is encountered in a fishery
#'
#' @export
#'
#' @importFrom stats aggregate
#'
summarizeFishery <- function(fishery_df) {
  fishery_df$is_cohort <- !is.na(fishery_df$fish_number)
  fishery_df$events <- 1L
  total_catch <-
    aggregate(fishery_df[,"events"],
              list(outcome = fishery_df$outcome,
                   is_cohort = fishery_df$is_cohort,
                   is_clipped = fishery_df$is_clipped,
                   is_legal_size = fishery_df$is_legal_size),
              FUN = "sum")

  colnames(total_catch)[colnames(total_catch) == "x"] <- "total"

  return(total_catch)
}

