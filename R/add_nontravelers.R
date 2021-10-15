#' Add non-travelers to the travelers data set for Mplus models
#'
#' This function takes the people who did not travel and adds them to the travel dataset with 0's for all travel variables that will be used in the Mplus analysis.
#' @param prdat Person-level data frame to get the full list of person IDs
#' @param trdat Travel-level data frame, which will be modified and used
#' @param usevar Variable from `trdat` whose unique values will be used to create the analysis variables
#' @return Returns an object that has all people, whether or not they traveled, with the analysis variables ready in columns for Mplus.
#' @export
add_nontravelers <- function(trdat, prdat, usevar) {
  # pull the ids of the people that made trips
  trpid <- trdat %>%
    dplyr::pull(personid) %>%
    unique()

  # pull the analysis variable unique values from a single column
  usevar_names <- trdat %>%
    dplyr::filter(!is.na({{ usevar }})) %>%
    dplyr::pull({{ usevar }}) %>%
    unique()

  # create df containing pids who didn't travel
  notrav <- prdat %>%
    dplyr::filter(!(personid %in% trpid)) %>%
    dplyr::select(personid)

  # add empty analysis columns, clean names
  notrav[,usevar_names] <- 0

  notrav <- notrav %>%
    janitor::clean_names()


  trav <- trdat %>%
    dplyr::select(personid, {{ usevar }}) %>%
    dplyr::filter(!is.na({{ usevar }})) %>%
    dplyr::mutate(vals = 1) %>%
    dplyr::distinct() %>%
    # group_by(personid) %>%
    tidyr::pivot_wider(id_cols = personid, names_from = {{ usevar }}, values_from = vals, values_fill = 0) %>%
    janitor::clean_names()
    # dplyr::select(sov, walk, everything())

  # Adding people who did not travel:
  if(!(identical(names(notrav), names(trav)))) {
    warning("there was an issue with column matching")
    warning(all.equal(names(notrav), names(trav)), call. = FALSE)
  }

  readydat <- trav %>%
    dplyr::bind_rows(notrav)
  return(readydat)

}

