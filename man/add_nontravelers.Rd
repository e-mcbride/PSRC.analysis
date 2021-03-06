% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_nontravelers.R
\name{add_nontravelers}
\alias{add_nontravelers}
\title{Add non-travelers to the travelers data set for Mplus models}
\usage{
add_nontravelers(dumdat, prraw, notr_pids)
}
\arguments{
\item{dumdat}{Travel-level data frame, which will be modified and used}

\item{prraw}{Person-level data frame, uncleaned, to get the full list of person IDs}

\item{notr_pids}{List of person ids of people who were not included in travel data.}
}
\value{
Returns an object that has all people we want for analysis, whether or not they traveled, with the analysis variables ready in columns for Mplus.
}
\description{
This function takes the people who did not travel and adds them to the travel dataset with 0's for all travel variables that will be used in the Mplus analysis.
}
