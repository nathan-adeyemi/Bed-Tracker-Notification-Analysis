# rm(list = ls())
source(file.path(Sys.getenv(
   if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
 ), ".vscode-R", "init.R"))

options(scipen = 999,
        digits = 6,
        error = recover)

# Utilities ----------------------------
if(!interactive()){
  library(jsonlite)
}
library(data.table)
library(openxlsx)
library(writexl)
library(tictoc)
library(gtools)
library(simmer)
library(lubridate)
library(tidyverse)
library(stringdist)
library(tidytext)
library(queueing)

# Packages for Statistics/Bootstrapping/etc. ------------------------------
library(fitdistrplus)
library(boot)
library(simpleboot)
library(EnvStats)

# Packages for Parallel Processing ----------------------------------------
library(doParallel)
library(parallelly)


invisible(lapply(X = file.path('functions',list.files(path = file.path('.','Functions')))
, FUN = source))

invisible(lapply(X = file.path('functions',list.files(path = file.path('.','Functions')))
                 , FUN = source))
list2env(readRDS(file.path(
  "..",
  'Policy_Interventions_to_Improve_Mental_Healthcare_Access',
  "simulations",
  "function_requirements",
  "MH_Network_sim_input_list.rds"
)), .GlobalEnv)
