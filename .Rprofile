# rm(list = ls())
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}

options(scipen = 999,
        digits = 6)

# Utilities ----------------------------
suppressPackageStartupMessages({
  if(!interactive()){
    library(jsonlite)
    # library(tidyverse)
  }
  library(data.table)
  library(openxlsx)
  library(writexl)
  library(tictoc)
  library(gtools)
  library(simmer)
  library(lubridate)
  library(ggplot2)
  library(queueing)
  library(yaml)
  library(zoo)

  # Packages for Statistics/Bootstrapping/etc. ------------------------------
  library(fitdistrplus)
  library(boot)
  library(simpleboot)

  # Packages for Parallel Processing ----------------------------------------
  library(doParallel)
  library(parallelly)
})


source("src/functions.R")
source("src/repair_problem_dyn_prog.R")
hccis <- readRDS("Data/hccis.rds")
siteInfo <- readRDS("Data/siteInfo.rds")
