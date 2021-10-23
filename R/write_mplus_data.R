#' Write data file for Mplus, including setting working directory
#'
#' This function modifies \code{prepareMplusData} from the \code{MplusAutomation} package. It changes the working directory to allow you to get a shortened file name in the pasted syntax.
#'
#'@param df Argument used by \code{MplusAutomation::prepareMplusData}. From the \code{prepareMplusData} documentation: The \code{R} data.frame to be prepared for Mplus.
#'@param wd_for_analysis Working directory where you want your .dat file to be saved. This is where your .inp file should also be saved.
#'@param filename Argument used by \code{MplusAutomation::prepareMplusData}. From the \code{prepareMplusData} documentation: The filename for the tab-delimited data file for use with Mplus.
#'@param writeData Argument used by \code{MplusAutomation::prepareMplusData}. From the \code{prepareMplusData} documentation: A character vector, one of 'always', 'ifmissing', 'never' indicating whether the data files (*.dat) should be written to disk. Defaults to ‘always’ for consistency with previous behavior. See details for further information.
#'@param hashfilename Argument used by \code{MplusAutomation::prepareMplusData}. From the \code{prepareMplusData} documentation: A logical whether or not to add a hash of the raw data to the data file name. Defaults to \code{FALSE} for consistency with previous behavior where this feature was not available.
#'@param ... Optional arguments passed on to `MplusAutomation::prepareMplusData()`
#'@return Writes .dat file formatted for Mplus to the proper working directory. It also returns the Mplus syntax to copy/paste into a .inp file that goes into the same working directory.
#'@export
write_mplus_data <- function(df,
                             wd_for_analysis,
                             filename,
                             writeData,
                             hashfilename,
                             ...) {
  if (getwd() != wd_for_analysis) {
    setwd(wd_for_analysis)
  }

  try({
    data.frame(df) %>%
      MplusAutomation::prepareMplusData(
        filename = filename,
        writeData = writeData,
        hashfilename = hashfilename,
        ...);
    message(paste0("Mplus data has been written to ", wd_for_analysis, "/", filename))
  })
  setwd(here())
}
