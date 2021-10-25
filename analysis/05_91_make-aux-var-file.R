
filelist <- list.files(here::here("analysis/"), pattern = "05_0", full.names = TRUE)
i <- 0

for(i in 1:length(filelist)) {
  source(here::here(filelist[i]))
  rm(list = dplyr::setdiff(ls(), c("filelist", "i")))
  message(paste0("File ", i, " successfully run"))
}



