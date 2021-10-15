# model_path <- paste0("analysis/03_Mplus/", model_name, "/")

# assign(paste0("allOut_", model_name),
#        value = readModels(
#          model_path,
#          recursive = FALSE
#        )
# )
# Create a list object named allOut, with an item named "mode"
# if the list named allOut already exists, then check if item named "mode" exists.
# if it does, then update it
# if it does not, append "mode" or "attitude" or whatever to the list.
#
# allOut <- list()
# allOut[[model_name]] <- readModels(here(model_path),
#   # here("analysis/03_Mplus/attitudes/"),
#   recursive = FALSE)




# attempt to create a new list level with the name of each model -----
# shorten %>%
#   str_extract("[^.]+\\.(?=.)")
#
# shorten %>%
#   str_extract("[^.]+(?=\\.)") # this means extract the string before the first `.`
#
# IF name before first period starts with "mode",
# THEN this model goes into the list called "mode"


