# automating creation of the template
library(tidyverse)
library(here)

model_name <- "TEST"

model_path <- paste0("analysis/Mplus/", model_name, "/")
model_template <-  paste0(model_path, "template/")

templatefile_path <- paste0(model_template, model_name, "_template.txt")

string2add <- c(a = "what if there are two strings", b = "testing adding a string to the file")

test <- write_lines(x = string2add, file = here(templatefile_path))


read_lines(file = here(templatefile_path))

# so we could create a function that helps us write parts of the template

