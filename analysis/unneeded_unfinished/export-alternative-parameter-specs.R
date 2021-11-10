# model with auxiliary: figures
library(tidyverse)
library(MplusAutomation)


c6_models <-
  readModels(here::here("analysis/Mplus/mode_cleaned_aux/final-model-work-c6/"),
             recursive = FALSE
  ) %>%
  purrr::set_names(~ .x %>%
                     str_replace(".out", replacement = "") %>%
                     str_split("_cleaned_") %>%
                     map(2) %>%
                     str_replace("aux_03_", "m")
  )


# Model 3A ##########################################################

m3a <- c6_models$m03a_inc.seq.interactions

# params <- m3a$parameters$unstandardized

out <- m3a$output

altparam.start <- which(out == "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION")
altparam.end <- (which(out == "ODDS RATIO FOR THE ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION")-1)

altparam <- out[altparam.start:altparam.end]
#
# altparam %>%
#   str_replace()

ref.indx <- altparam %>%
  str_which("Parameterization using Reference Class |Intercepts")

altparam[ref.indx]

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


ap <- divide_text(altparam, ref.indx)


# divide by " C#\\d      ON" ==============================================


is_ap_int <- ap %>%
  names() %>%
  str_detect("Intercept")

ap_aux <- ap[!is_ap_int]
ap_int <- ap[is_ap_int]

# ref.c <- ap_aux %>%
#   map(~str_which(.x, " C#\\d      ON"))

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

refclass <- 4

intparam_4 <- ap_inttabs[[refclass]] %>%
  mutate(class_id = "Intercept") %>%
  select(-var_id, everything(), var_id)

aparam_4 <- ap_auxtabs[[refclass]] %>%
  select(-class_id, -var_id, everything(), class_id, var_id) %>%
  bind_rows(intparam_4)

aparam_4


aparam_4 %>%
  write.table("clipboard", sep = "\t", row.names = FALSE)

