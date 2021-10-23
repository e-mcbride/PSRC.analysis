#' Add non-travelers to the travelers data set for Mplus models
#'
#' This function takes the people who did not travel and adds them to the travel dataset with 0's for all travel variables that will be used in the Mplus analysis.
#' @param prraw Person-level data frame, uncleaned, to get the full list of person IDs
#' @param dumdat Travel-level data frame, which will be modified and used
#' @param notr_pids List of person ids of people who were not included in travel data.
#' @return Returns an object that has all people we want for analysis, whether or not they traveled, with the analysis variables ready in columns for Mplus.
#' @export
add_nontravelers <- function(dumdat, prraw, notr_pids) {

  # create df containing pids who didn't travel
  notrav <- prraw %>%
    dplyr::filter(personid %in% notr_pids) %>%
    dplyr::select(personid)

  # pull the dummy variable column names
  usevar_names <- dumdat %>%
    names() %>%
    setdiff("personid")

  # add empty analysis columns, clean names
  notrav[,usevar_names] <- 0

  notrav <- notrav %>%
    janitor::clean_names()

  # Adding people who did not travel:
  if(!(identical(names(notrav), names(dumdat)))) {
    warning("there was an issue with column matching")
    warning(all.equal(names(notrav), names(dumdat)), call. = FALSE)
  }

  readydat <- dumdat %>%
    dplyr::bind_rows(notrav)
  return(readydat)

}

