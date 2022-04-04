# 03_03 Analyze model results
##    This is also when updates to .inp syntax and model re-runs would occur
# Imports =====
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

model_name <- "mode_cleaned_aux"


# Get the table of values =====

# TO DO: make into fxn in "/R/"?

# Takes Mplus model results list object (created by `MplusAutomation::readModels()`),
#     extracts parameters of interest into a table:
fitind <- function(outfiles) {
  outfiles %>%
    enframe() %>%
    transmute(name,
              LLRepTbl = map(value, LLrep_to_table),
              summaries = map(value, ~ .x$summaries),
              nclasses = map(summaries, "NLatentClasses"),
              Loglikelihood = map(summaries, "LL"),
              BIC = map(summaries, "BIC"),
              ABIC = map(summaries, "aBIC"),
              t11_km1ll = map(summaries, "T11_KM1LL"),
              # t11_km1ll = map_dbl(t11_km1ll, ~ as.numeric(.x)),
              # # km1ll_correct = (as.numeric(t11_km1ll) == lag(as.numeric(Loglikelihood))),
              BLRT_pval = map(summaries, "BLRT_PValue"),
              VLMRT_pval = map(summaries, "T11_VLMR_PValue"),
              Entropy = map(summaries, "Entropy"),
              llnreps = map(value, LLreplication),
              optseed = map(LLRepTbl,
                            ~ .x %>% slice(1) %>% pull(seed)),
              seedused = map(value, ~ .x$input$analysis$optseed)
    )
}

# parses model results (from .out files) from a specific directory (model_name)
allOut_mode <- readModels(here(paste0("analysis/Mplus/", model_name)), recursive = FALSE)

# shorten long model names auto-generated from file names:
shorten <- allOut_mode %>%
  names() %>%
  str_split("X") %>%
  map(~.x[2]) %>%
  str_split(".out") %>%
  map(~.x[1])
shorten

names(allOut_mode) <- shorten

# # does not do anything now, but would be useful if more directories were extracted from:
# allOut <- allOut_mode %>%
#   keep(str_detect(names(.), pattern = model_name))

# create fit indices table from function defined earlier:
fitind_mode <- fitind(allOut_mode)


# elbow plot of BIC and ABIC results:
fitind_mode %>% ggplot(aes(x = as.numeric(nclasses))) +
  geom_line(aes(y = as.numeric(ABIC), color = "red")) +
  geom_line(aes(y = as.numeric(BIC), color = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("ABIC", "BIC"))

# condense fit stats table for use in paper/presentation:
fitstats_tbl_mode <- fitind_mode %>%
  select(-name, -LLRepTbl, -summaries, -llnreps, -optseed, -seedused, -t11_km1ll) %>%
  unnest(cols = c(nclasses, Loglikelihood, BIC, ABIC, BLRT_pval, VLMRT_pval,
                  Entropy))

# Copy to clipboard for pasting into Excel for beautification
fitstats_tbl_mode %>% write.table("clipboard", sep = "\t")

