# Create item probability plots for all LPA models
# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

model_name <- "mode_cleaned_aux"

# Item Probability Plots (IPPs)==================
## Fn: Plot mixtures for LCA probability scale --------------------------

# This fn taken and edited from source code for MplusAutomation::plotMixtures().
# I had to do this to get it to plot the line graphs for LCA probability scale


plotMixtures.probscale <- function(modelList,
                                   coefficients,
                                   paramCat,
                                   paramOrder = c("bik", "wlk", "trn", "dal", "dot", "pas", "oth", "wsfh")) {
  # select the category to plot (cat 2)
  plotdat <-
    lapply(modelList, function(x) {
      subset(x$parameters[[coefficients]], x$parameters[[coefficients]]$category == paramCat)
    })

  # Bind into one df with identifying variable

  plotdat <- do.call(rbind, lapply(names(modelList), function(x) {
    data.frame(Title = modelList[[x]]$input$title, plotdat[[x]])
  }))

  # Drop useless stuff
  plotdat <- plotdat %>% select(-est_se, -pval, -category)

  # Get some classy names
  names(plotdat)[which(names(plotdat) %in% c("param", "est", "LatentClass"))] <-
    c("Variable", "Value", "Class")

  plotdat$Variable <- factor(plotdat$Variable, levels = toupper(paramOrder))

  levels(plotdat$Variable) <- paste0(toupper(substring(levels(plotdat$Variable), 1, 1)), tolower(substring(levels(plotdat$Variable), 2)))


  classplotdat <-
    ggplot(
      NULL,
      aes_string(
        x = "Variable",
        y = "Value",
        group = "Class",
        linetype = "Class",
        shape = "Class",
        colour = "Class"
      )
    )

  classplot <- classplotdat + geom_point(data = plotdat, size = 0.7) +
    geom_line(data = plotdat, size = 0.4) +
    theme_bw()

  if (length(modelList) > 1) {
    classplot <- classplot + facet_wrap(~ Title)
  }
  return(classplot)
}

# aesthetic modifications to `plotMixtures.probscale`:
create_ipps.probscale <- function(outfile,
                                  coefficients,
                                  paramCat,
                                  paramOrder = c("bik", "wlk", "trn", "dal", "dot", "pas", "oth", "wsfh")) {
  plotMixtures.probscale(modelList = outfile, coefficients, paramCat, paramOrder) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title,
               # scales = "free_y",
               ncol = 2) +
    theme(strip.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          text = element_text(size = 8, family = "serif"))
  # +
  #   theme_dark(base_size = 10, base_family = "serif")
    # theme(strip.text = element_text(size = 8),
    #       legend.title = element_text(size = 8),
    #       text = element_text(size = 7, family = "serif"))
}

## Execute IPP creation -------------------------------

# parses model results (from .out files) from a specific directory (model_name)
allOut_mode <- readModels(here(paste0("analysis/Mplus/", model_name)), recursive = FALSE)

# shorten long model names auto-generated from file names:
shorten <- allOut_mode %>%
  names() %>%
  str_split("X") %>%
  map(~.x[2]) %>%
  str_split("_mode") %>%
  map(~.x[1])
# shorten

names(allOut_mode) <- shorten


### Travel Mode -------------------------------------------------------------

# ordering of variables in plot:
modeOrder <- c('sov' = 'Single Occupancy Vehicle',
               'dr_oth' = 'Drive Others',
               'pass' = 'Passenger',
               'transit' = 'Transit',
               'walk' = 'Walk',
               'bike' = 'Bike',
               'other' = 'Other Modes')

# create plots for all models in one image:
ipps_mode <- create_ipps.probscale(outfile = allOut_mode,
                                   coefficients = "probability.scale",
                                   paramCat = 2,
                                   paramOrder = names(modeOrder)) +
  scale_x_discrete(labels = str_wrap(unname(modeOrder), width = 10)) +
  ylab("Estimated Probabilities")

ipps_mode

ggsave(plot = ipps_mode,here::here("analysis/figures/all_ipps_mode-cl-aux.png"), width = 6.5, height = 4.5)



# IPP for selected number of classes ===================================================

# for 5 class Un-comment this and run instead of 6 class below
# c <- 5 # final n classes selected
# mode_className <- c('Carpool Drivers',
#                     'Diverse Mode Users',
#                     'Transit and Walk',
#                     'Car Passengers',
#                     'Solitary Drivers')

# for 6 class. Un-comment this and run instead of 5 class above
nclasses <- 6 # final n classes selected

# names for mode classes:
mode_className <- c('Transit Users',
                    'Car Passengers',
                    'Diverse Mode Users',
                    'Solitary Drivers',
                    'Walkers',
                    'Non-solitary Drivers')

## create plot class labels: ---------------------------------------------------
# proportion of observations by class, turn into percentages (for labels on plot)
classProp <- (allOut_mode[[nclasses]]$class_counts$mostLikely$proportion) * 100 %>%
  round(digits = 1)

# Create strings to be used as class labels
classname_prop_labels <- paste0(mode_className, " (", classProp, "%)")

## create final plot with labels ---------------------------------------------------
ipp <- allOut_mode[nclasses] %>%
  create_ipps.probscale(coefficients = "probability.scale",
                        paramCat = 2,
                        paramOrder = names(modeOrder)) +
  scale_x_discrete(labels = str_wrap(unname(modeOrder), width = 11)) +
  ylab("Estimated Probabilities") +
  scale_colour_discrete(#type = RColorBrewer::brewer.pal(n = 7, name = "Set1"),
                        labels = str_wrap(classname_prop_labels, width = 16)) +
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "gray80") +
  geom_hline(yintercept = 0.30, linetype = "dashed", color = "gray80") +
  theme(strip.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        text = element_text(size = 10, family = "serif"))


ipp

ggsave(plot = ipp, here::here(paste0("analysis/figures/ipp_mode-clean-aux_final_c", nclasses, ".png")), width = 6.5, height = 3)






