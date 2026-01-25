# Purpose: Load packages
# Notes: 1) checks if installed. 2) if not, then installs, then 3) calls library
# Usage: 
## Add to the pkgs list as needed
## source("PackageLoads.R") within other files as needed. 
# Authors
## Mike Aguilar (mike.aguilar@duke.edu) & Ziming Huang



# Define the packages you need
pkgs <- c(
  "tidyverse", "data.table", "lubridate",
  "dplyr", "tidyr", "ggplot2",
  "zoo", "PerformanceAnalytics", "gridExtra",
  "reshape2", "corrplot", "scales",
  "moments", "quantmod",
  "tidyquant", "here", "slider",
  "purrr", "pROC", "quadprog","sandwich", "lmtest","boot", 
  "plotly", "tibble", "mclust", "ggforce", 
  "factoextra", "cluster", "NbClust", "e1071", "ClusterR", 
  "proxy", "igraph","ggraph", "ggrepel", "Silhouette", "caret", 
  "keras3", "torch", "tensorflow",
  "class", "FNN", "e1071", "randomForest", "xgboost", "gbm", "C50", "nnet", 
  "rpart", "rpart.plot", "partykit"
)


failed_pkgs <- character()

for (p in pkgs) {

  # 1) Install if missing
  if (!requireNamespace(p, quietly = TRUE)) {
    ok_install <- tryCatch(
      {
        install.packages(p, dependencies = TRUE)
        TRUE
      },
      warning = function(w) {
        warning(sprintf("Package '%s' produced a warning during install: %s", p, w$message))
        TRUE  # warning != failure
      },
      error = function(e) {
        warning(sprintf("Package '%s' failed to install: %s", p, e$message))
        FALSE
      }
    )
    if (!ok_install) {
      failed_pkgs <- c(failed_pkgs, p)
      next
    }
  }

  # 2) Load package
  ok_load <- tryCatch(
    {
      suppressPackageStartupMessages(library(p, character.only = TRUE))
      message(sprintf("Loaded package: %s", p))
      TRUE
    },
    error = function(e) {
      warning(sprintf("Package '%s' is installed but failed to load: %s", p, e$message))
      FALSE
    }
  )

  if (!ok_load) failed_pkgs <- c(failed_pkgs, p)
}

failed_pkgs <- unique(failed_pkgs)

if (length(failed_pkgs) > 0) {
  message("The following packages failed: ", paste(failed_pkgs, collapse = ", "))
} else {
  message("All packages loaded successfully.")
}


# Load other helper functions
source(here("Supporting","Signal_Evaluation_Static.R"))
source(here("Supporting","Signal_Evaluation_Dynamic.R"))
source(here("Supporting","ConstructPortfolio.R"))
source(here("Supporting","DailyPerformanceStats.R"))
