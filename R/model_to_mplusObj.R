#' Extract input from mplus.model into list format suitable for \code{MplusAutomation::mplusObject()}
#'
#' This function uses `tidyverse` functions to extract input from 1 mplus.model so that after running \code{MplusAutomation::mplusObject()} I can use it to modify input using `MplusAutomation::update.mplusObject()`
#'
#' @param mplusmodelinp A single `mplus.inp` object
model_to_mplusObjList <- function(mplusmodelinp) {
  mplusmodelinp %>%
    purrr::set_names(toupper) %>%
    purrr::map_at(vars(-TITLE),
           ~ paste0(stringr::str_to_upper(names(.x)), " = ", .x, ";\n") %>%
             stringr::str_c(collapse = " ")
           )
}

#' Extract input from \code{mplus.inp}, convert into mplusObject
#'
#' This function uses `tidyverse` functions to extract input from 1 object of type \code{mplus.model}, then it converts the input into object type \code{mplusObject}. This means I can use this extracted input to update the model specifications using `MplusAutomation::update.mplusObject()`.
#'
#' @param mplusmodelinp A single `mplus.inp` object
#' @export
model_to_mplusObj <- function(mplusmodelinp) {
  mplus_objList <- model_to_mplusObjList(mplusmodelinp)

  do.call(MplusAutomation::mplusObject, mplus_objList)
}
