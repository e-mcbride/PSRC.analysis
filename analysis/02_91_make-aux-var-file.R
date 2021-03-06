
filelist <- list.files(here::here("analysis/"), pattern = "02_0", full.names = TRUE)
i <- 0

for(i in 1:length(filelist)) {
  message(paste0("Running File ", i, " of ", length(filelist)))
  source(here::here(filelist[i]))
  rm(list = dplyr::setdiff(ls(), c("filelist", "i")))
  message(paste0("File ", i, " successfully run"))
}

