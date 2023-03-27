
# fisherySim

<!-- badges: start -->
<!-- badges: end -->

The goal of fisherySim is to provide a simulator that passes a finite size cohort
through a fisher with adipose and size selective removal.  The package can 
stochastically create a simulated cohort based on a probability of being adipose-clipped and
a probability of being non-legal size.

The simulated cohort can then be passed through a fishery that stochastically 
simulates individual fishing events based on probability of the fishing dropping-off, 
released by fisher, or kept by fisher based on adipose clip status and size category.  
Each of these event outcomes will then have a probability of a mortality based on the outcome.
Releases of legal-sized fish can have a release mortality rate different from releases
of non-legal-sized fish.

The mortality of a fish from a previous can affect the availability of a fish in a subsequent 
fishing event.  

## Installation

You can install the development version of fisherySim like so:

``` r
remotes::install_git("...")

```

## Example

This is a basic example that compares the SIT and MFA methods in backwards direction:

``` r

library(fisherySim)
library(ggplot2)
library(patchwork)

compare_sit_mfa_df <- sitMfaComparison(500)

compare_sit_mfa_df$sim_mark_mort <-
  with(compare_sit_mfa_df,
       sim_pre_fishery_mark_cohort - sim_post_fishery_mark_cohort)

compare_sit_mfa_df$sit_mark_mort <-
  with(compare_sit_mfa_df,
       sit_pre_fishery_mark_cohort - sit_post_fishery_mark_cohort)

compare_sit_mfa_df$mfa_mark_mort <-
  with(compare_sit_mfa_df,
       mfa_pre_fishery_mark_cohort - mfa_post_fishery_mark_cohort)

compare_sit_mfa_df$sim_unmark_mort <-
  with(compare_sit_mfa_df,
       sim_pre_fishery_unmark_cohort - sim_post_fishery_unmark_cohort)

compare_sit_mfa_df$sit_unmark_mort <-
  with(compare_sit_mfa_df,
       sit_pre_fishery_unmark_cohort - sit_post_fishery_unmark_cohort)

compare_sit_mfa_df$mfa_unmark_mort <-
  with(compare_sit_mfa_df,
       mfa_pre_fishery_unmark_cohort - mfa_post_fishery_unmark_cohort)


sit_mark_plot <-
  ggplot(compare_sit_mfa_df,
         aes(sim_mark_mort,
             sit_mark_mort)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    x = "Simulated Marked Cohort Mortalities",
    y = "SIT Marked Cohort Mortalities",
    title = "SIT Method Marked Mortalities"
  )



sit_unmark_plot <-
  ggplot(compare_sit_mfa_df, aes(sim_unmark_mort,
                                 sit_unmark_mort)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    x = "Simulated Unmarked Cohort Mortalities",
    y = "MFA Unmarked Cohort Mortalities",
    title = "SIT Method Unmarked Mortalities"
  )

mfa_mark_plot <-
  ggplot(compare_sit_mfa_df, aes(sim_mark_mort,
                                 mfa_mark_mort)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    x = "Simulated Marked Cohort Mortalities",
    y = "MFA Marked Cohort Mortalities",
    title = "MFA Method Marked Mortalities"
  )

mfa_unmark_plot <-
  ggplot(compare_sit_mfa_df,
         aes(sim_unmark_mort,
             mfa_unmark_mort)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    x = "Simulated Unmarked Cohort Mortalities",
    y = "MFA Unmarked Cohort Mortalities",
    title = "MFA Method Unmarked Mortalities"
  )

(sit_mark_plot + sit_unmark_plot) / (mfa_mark_plot + mfa_unmark_plot)
```

