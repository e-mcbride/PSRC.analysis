# Analyze attitude, update and re-run
# Imports =====
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

model_name <- "mode"


# Get the table of values =====

#make into fn
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
              BLRT_pval = map(summaries, "BLRT_PValue"),
              VLMRT_pval = map(summaries, "T11_VLMR_PValue"),
              Entropy = map(summaries, "Entropy"),
              llnreps = map(value, LLreplication),
              optseed = map(LLRepTbl,
                            ~ .x %>% slice(1) %>% pull(seed)),
              seedused = map(value, ~ .x$input$analysis$optseed),
              t11_km1ll = map(summaries, "T11_KM1LL")
    )
}


allOut <- readModels(here("analysis/03_Mplus/"), recursive = TRUE)

shorten <- allOut %>%
  names() %>%
  str_split(".03_Mplus.") %>%
  map(~.x[2])

names(allOut) <- shorten

allOut_mode <- allOut %>%
  keep(str_detect(names(.), pattern = model_name))


fitind_mode <- fitind(allOut_mode)


fitind_mode %>% ggplot(aes(x = as.numeric(nclasses))) +
  geom_line(aes(y = as.numeric(ABIC), color = "red")) +
  geom_line(aes(y = as.numeric(BIC), color = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("ABIC", "BIC"))

# Very clearly from the elbow plot of ABIC and BIC, the 3-class model is where the elbow is


fitstats_tbl_mode <- fitind_mode %>%
  select(-name, -LLRepTbl, -summaries, -llnreps, -optseed, -seedused, -t11_km1ll) %>%
  unnest(cols = c(nclasses, Loglikelihood, BIC, ABIC, BLRT_pval, VLMRT_pval,
                  Entropy)) %>%
  filter(nclasses < 7)
