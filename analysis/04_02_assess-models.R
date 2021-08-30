# Analyze attitude, update and re-run
# Imports =====
library(tidyverse)
library(here)
library(MplusAutomation)
devtools::load_all()

model_name <- "mode"
model_path <- paste0("analysis/03_Mplus/", model_name, "/")

assign(paste0("allOut_", model_name),
       value = readModels(
         model_path,
         recursive = FALSE
       )
)
# Create a list object named allOout, with an item named "mode"
# if the list named allOut already exists, then check if item named "mode" exists.
# if it does, then update it
# if it does not, append "mode" or "attitude" or whatever to the list.


# allOut_mod <- readModels(
#   model_path,
#   # here("analysis/03_Mplus/attitudes/"),
#   recursive = FALSE)

# Get the table of values =====
tt_outs_att <- allOut_att %>%
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

ggplot(tt_outs_att, aes(x = as.numeric(nclasses))) +
  geom_line(aes(y = as.numeric(ABIC), color = "red")) +
  geom_line(aes(y = as.numeric(BIC), color = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("ABIC", "BIC"))

fitstats_att <- tt_outs_att %>%
  select(-name, -LLRepTbl, -summaries, -llnreps, -optseed, -seedused, -t11_km1ll) %>%
  unnest(cols = c(nclasses, Loglikelihood, BIC, ABIC, BLRT_pval, VLMRT_pval,
                  Entropy)) %>%
  filter(nclasses < 7)
