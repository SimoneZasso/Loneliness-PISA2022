
# Load libraries ----------------------------------------------------------

library(psych)
library(tidyverse)
library(haven)
library(writexl)
library(apaTables)
library(lavaan)
library(magrittr)
library(summarytools)
library(corrplot)
library(MplusAutomation)
library(multilevelTools)
library(devtools)
library(intsvy)
library(ggplot2)
library(httr)
library(telegram.bot)
library(forestplot)

# Load function -----------------------------------------------------------

lapply(
  list.files(path = "./function_R", pattern = "\\.R$", full.names = TRUE),
  source
)


