#' Creates Mplus model directory structure
#'
#' Uses a string to create a directory structure for running Mplus models.
#' @param model_name A string that will be used to name the analysis folder where all model
#' @return This function creates folders in the right places, or lets you know if the directory structure already exists. This includes creating a blank template file in the correct place if it does not already exist.
#' @export
create_model_dirs <- function(model_name) {
  analysis_relPath <- paste0("analysis/Mplus/", model_name, "/")
  template_relPath <- paste0(analysis_relPath, "template/")

  analysis_path <- here::here(analysis_relPath)
  template_path <- here::here(template_relPath)
  if(dir.exists(analysis_path)) {
    message("Model root folder already exists")
  }
  dir.create(analysis_path)

  if(dir.exists(template_path)) {
    message("Template folder already exists within model root folder")
  }
  dir.create(template_path)

  # Create the blank template file
  templatefile_relpath <- paste0(template_relPath, model_name, "_template.txt")
  templatefile_path <- here::here(templatefile_relpath)

  if(file.exists(templatefile_path)) {
    message("Template file already exists")
  } else {
    file.create(templatefile_path)
  }

}
