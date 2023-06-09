% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishery_simulate.R
\name{sequencialFisherySim}
\alias{sequencialFisherySim}
\title{Sequential Fishery Simulation}
\usage{
sequencialFisherySim(
  cohort_df,
  cohort_encounter_rate = 0.7,
  fishery_params = NULL
)
}
\arguments{
\item{cohort_df}{Cohort data frame}

\item{cohort_encounter_rate}{Encounter rate of the cohort}

\item{fishery_params}{Fishery parameters}
}
\value{
Fish/row number of a fish that is encountered in a fishery
}
\description{
Sequentially simulate a fishery up to a total kept catch based on individual fishing
(i.e. kept or released) events.  Events may be slightly more or less to accommodate
the random probability of drop-off events.  If a fish from the cohort dies, it is
no longer available for selection in the fishery
}
