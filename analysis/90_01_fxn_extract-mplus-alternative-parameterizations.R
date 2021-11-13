library(dplyr)
library(stringr)
#' Extract Mplus alternative parameterization model results
#'
#' Takes Mplus model results imported by `MplusAutomation`, extracts one alternative parameterization model results table
#' @param mplusModel A single mplus.model object (from `MplusAutomation`)
#' @param refClass Integer of the reference class for which you want the alternative parameterization results
#' @param modelID A string with the unique ID code you want for this model when you paste it into excel sheet
#'
alt_param <- function(mplusModel, refClass, modelID){
  out <- mplusModel$output

  altparam.start <- which(out == "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION")
  altparam.end <- (which(out == "ODDS RATIO FOR THE ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION")-1)

  altparam <- out[altparam.start:altparam.end]

  ref.indx <- altparam %>%
    str_which("Parameterization using Reference Class |Intercepts")


  ap <- divide_text(altparam, ref.indx)

  is_ap_int <- ap %>%
    names() %>%
    str_detect("Intercept")

  ap_aux <- ap[!is_ap_int]
  ap_int <- ap[is_ap_int]

  ap_auxtabs <- ap_aux %>%
    map(~ divide_text(.x, str_which(.x, " C#\\d      ON")) %>% set_names(~ str_replace(., "#", ""))
    ) %>%
    modify_depth(1, ~.x %>%
                   map(~.x %>% str_trim() %>% strsplit("\\s+")
                   ) %>%
                   map(~ .x %>%  transpose() %>% set_names(c("VarID", "Estimate", "S.E.", "Est./S.E.", "P-Value")) %>%
                         as_tibble()
                   ) %>%
                   enframe(name = "ClassID") %>%
                   unnest(cols = c(value)) %>%
                   unnest(cols = c(VarID, Estimate, S.E., `Est./S.E.`, `P-Value`)) %>%
                   janitor::clean_names()
    )

  ap_inttabs <- ap_int %>%
    map( ~ .x %>% str_trim() %>% strsplit("\\s+") %>%
           transpose() %>%
           set_names(c("VarID", "Estimate", "S.E.", "Est./S.E.", "P-Value")) %>%
           map(unlist) %>%
           as.data.frame() %>%
           janitor::clean_names()
    )

  intparam_ref <- ap_inttabs[[refClass]] %>%
    mutate(class_id = "Intercept") %>%
    select(-var_id, everything(), var_id)

  aparam_ref <- ap_auxtabs[[refClass]] %>%
    select(-class_id, -var_id, everything(), class_id, var_id) %>%
    bind_rows(intparam_ref) %>%
    mutate(model.ID = modelID,
           class.name = "",
           var.name = "") %>%
    select(model.ID, class.name, var.name, everything())

  return(aparam_ref)

}

#' Helper function to divide character vector
#'
#' Uses specified indices to divide a character vector into a list of smaller character vectors
#' @param text.inp The text file that is being split up
#' @param ref.index Vector of indices of `text.inp` at which the cut points should be made. The string at `ref.index` also becomes the name of the list item that contains the text below that index.
#' @return A list of named character vectors. The value at `ref.index` becomes the name of the list item.
divide_text <- function(text.inp, ref.index) {
  ap_comp <- list()
  ap_return <- list()
  for (i in 1:length(ref.index)) {
    start <- (ref.index[i])+1
    if(i == length(ref.index)) {
      end <- length(text.inp)
    } else {end <- (ref.index[i+1])-1}

    ap_comp[i] <- list(text.inp[start:end])

    ap_return[i] <- list(ap_comp[[i]][(which(ap_comp[[i]] != ""))])
    names(ap_return)[i] <- text.inp[start-1]
  }
  return(ap_return)
}
