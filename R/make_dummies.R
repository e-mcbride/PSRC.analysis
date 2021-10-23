#' Make analysis variables
#'
#' This function takes a single column and creates a set of dummy columns from it
#' @param dat Data frame. The output will return a set of dummies for each observation in this data frame
#' @param usevar Variable from `dat` whose unique values will be used to create the dummy variables
#' @return Datset with the same observations as is input, but with only dummy variables and personid as output
#' @export
make_dummies <- function(dat, usevar) {

  # pull the analysis variable unique values from a single column
  usevar_names <- dat %>%
    dplyr::filter(!is.na({{ usevar }})) %>%
    dplyr::pull({{ usevar }}) %>%
    unique()

  trav <- dat %>%
    dplyr::select(personid, {{ usevar }}) %>%
    dplyr::filter(!is.na({{ usevar }})) %>%
    dplyr::mutate(vals = 1) %>%
    dplyr::distinct() %>%
    # group_by(personid) %>%
    tidyr::pivot_wider(id_cols = personid, names_from = {{ usevar }}, values_from = vals, values_fill = 0) %>%
    janitor::clean_names()

  return(trav)
}
