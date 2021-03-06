# Create item probability plots for all LPA models
# Imports ===========================
library(tidyverse)
library(here)
library(MplusAutomation)
library(ggrepel)
devtools::load_all()

model_name <- "mode_cleaned"

# Item Probability Plots (IPPs)==================
## Fn: Plot mixtures for LCA probability scale --------------------------

# This fn taken and edited from source code for MplusAutomation::plotMixtures().
# I had to do this to get it to plot the line graphs for LCA probability scale


# # TEMP create_ipps.probscale inputs:
# modelList = allOut_mode
# coefficients = "probability.scale"
# paramCat = 2
# paramOrder = names(modeOrder)
#
# x = 6
# data.frame(Title = modelList[[x]]$input$title, plotdat[[x]])


plotMixtures.probscale <- function(modelList,
                                   coefficients,
                                   paramCat,
                                   paramOrder = c("bik", "wlk", "trn", "dal", "dot", "pas", "oth", "wsfh")) {
  # select the category to plot (cat 2),
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

  classplot <- classplotdat + geom_point(data = plotdat) +
    geom_line(data = plotdat) +
    theme_bw()

  if (length(modelList) > 1) {
    classplot <- classplot + facet_wrap(~ Title)
  }
  return(classplot)
}


## Fn: Create IPPs ------------------------------------------
create_ipps <- function(outfile, parameter = "Means") {
  outfile %>%
    plotMixtures(parameter = parameter, ci = NULL) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title, scales = "free_y", ncol = 2) +
    theme(strip.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          text = element_text(size = 8, family = "serif"))
}



create_ipps.probscale <- function(outfile,
                                  coefficients,
                                  paramCat,
                                  paramOrder = c("bik", "wlk", "trn", "dal", "dot", "pas", "oth", "wsfh")) {
  plotMixtures.probscale(modelList = outfile, coefficients, paramCat, paramOrder) + #parameter = parameter, ci = NULL) +

    # ggplot specifications
    aes(linetype = "solid", shape = "circle") +
    guides(linetype = 'none', shape = "none") +
    facet_wrap(~ Title, scales = "free_y", ncol = 2) +
    theme(strip.text = element_text(size = 11),
          legend.title = element_text(size = 10),
          text = element_text(size = 10, family = "serif"))
}

## Fn: add counts of estimates ---------------------------------
add_estcount <- function(outfile) {
  combo <- outfile %>%
    enframe() %>%
    transmute(name,
              est_count = map(value,
                              ~ .x$class_counts$modelEstimated %>%
                                mutate(class = as.character(class))
              ),
              label_time = map(value,
                               ~ .x$parameters$unstandardized %>%
                                 filter(paramHeader == "Means") %>%
                                 filter(!str_detect(param, "C#")) %>%
                                 group_by(LatentClass) %>%
                                 mutate(est_se = as.numeric(est_se),
                                        est = as.numeric(est),
                                        maxest = est == max(est)) %>%
                                 filter(maxest)
              ),

    ) %>%
    unnest(cols = c(est_count, label_time)) %>%
    rename(Class = LatentClass,
           Variable = param,
           Value = est)

  if(any(combo$class != combo$LatentClass)){
    warning("Something went wrong")
  }
  return(combo)# message("Count Added successfully. NAs introduced by coercion are to be expected.")
}


## Exec IPP creation -------------------------------

## Import allOut ------
allOut <- readModels(here("analysis/Mplus/"), recursive = TRUE)

shorten <- allOut %>%
  names() %>%
  str_split(".Mplus.") %>%
  map(~.x[2])
names(allOut) <- shorten



### Travel Mode -------------------------------------------------------------



allOut_mode <- allOut %>%
  keep(str_detect(names(.), pattern = paste0(model_name, "\\.(?=[:digit:])")))

modeOrder <- c('sov' = "Single Occupancy Vehicle",
               'hov' = "High Occupancy Vehicle",
               'transit' = "Taking Transit",
               'walk' = "Walking",
               'bike' = "Biking",
               'other' = "Using Other Modes")





ipps_mode <- create_ipps.probscale(outfile = allOut_mode,
                                   coefficients = "probability.scale",
                                   paramCat = 2,
                                   paramOrder = names(modeOrder)) +
  scale_x_discrete(labels = str_wrap(unname(modeOrder), width = 11))

ipps_mode
ggsave(plot = ipps_mode,"analysis/figures/all_ipps_mode-cleaned.png", width = 6.5, height = 4.5)



# IPP for selected number of classes ===================================================

# for 5 class
c <- 5 # final n classes selected
mode_className <- c('Diverse Mode Users',
                    'Carpoolers',
                    'Solitary Drivers',
                    'Non-Driver, Transit users',
                    'Non-Driver, Walkers')

# # for 6 class. Un-comment this and run instead of 5 class above
# c <- 6 # final n classes selected
# mode_className <- c('Non-Driver, Transit users',
#                     'Non-Driver, Walkers',
#                     'Diverse Mode Users',
#                     'Carpoolers',
#                     'Bicycle Users',
#                     'Solitary Drivers')

classCounts <- allOut_mode[[c]]$class_counts$mostLikely

classProp <- round(classCounts$proportion * 100, digits = 1)

classname_count_labels <- paste0(mode_className, " (n=", classCounts$count, ")")

classname_prop_labels <- paste0(mode_className, " (", classProp, "%)")

ipp <- allOut_mode[c] %>%
  create_ipps.probscale(coefficients = "probability.scale",
                        paramCat = 2,
                        paramOrder = names(modeOrder)) +
  scale_x_discrete(labels = str_wrap(unname(modeOrder), width = 11)) +
  ylab("Estimated Probabilities") +
  scale_colour_discrete(labels = str_wrap(classname_prop_labels, width = 16)) +
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "gray47") +
  geom_hline(yintercept = 0.30, linetype = "dashed", color = "gray47")

ipp

ggsave(plot = ipp, paste0("analysis/figures/ipp_mode-clean_c", c, ".png"), width = 6.5, height = 3)






