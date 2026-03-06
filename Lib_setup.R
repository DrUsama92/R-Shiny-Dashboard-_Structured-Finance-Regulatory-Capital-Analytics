# ------------------------------------------------------------
# LIB SETUP SCRIPT
# Purpose:
# This file prepares your computer to run the SRT Shiny App.
# It checks system tools, installs required packages,
# and verifies that all required files exist.
#
# This script should be run ONCE (or when errors occur)
# before running the main app file (Mod90.R / App.R).
# ------------------------------------------------------------

# ------------------------------------------------------------
# 1) Check system build tools (mainly for Windows users)
# ------------------------------------------------------------
# This checks whether system tools like 'make' are available.
# These are sometimes required to install certain R packages.
Sys.which("make")

# Checks whether R can build packages from source if needed.
pkgbuild::has_build_tools(debug = TRUE)


# ------------------------------------------------------------
# 2) Set CRAN repository (where R downloads packages from)
# ------------------------------------------------------------
# Ensures packages are downloaded from a reliable official source.
options(repos = c(CRAN = "https://cloud.r-project.org"))


# ------------------------------------------------------------
# 3) Install 'fs' package (used for file/path handling)
# ------------------------------------------------------------
install.packages("fs")

# Re-confirm CRAN repository
options(repos = c(CRAN = "https://cloud.r-project.org"))


# ------------------------------------------------------------
# 4) Install core required packages (only if missing)
# ------------------------------------------------------------
# List of required packages for the SRT app
pkgs <- c(
  "shiny",
  "shinyjs",
  "openxlsx",
  "shinyWidgets",
  "dplyr",
  "ggplot2",
  "scales",
  "fs"
)

# Identify which packages are not yet installed
need <- setdiff(pkgs, rownames(installed.packages()))

# Install missing packages only
if (length(need)) install.packages(need)


# ------------------------------------------------------------
# 5) Helper function: install + load packages safely
# ------------------------------------------------------------
# This function:
# - checks if a package is installed
# - installs it if missing
# - then loads it into the session
safe_lib <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}


# ------------------------------------------------------------
# 6) Load required packages used by the app
# ------------------------------------------------------------
# These packages are critical for data handling and UI behavior
safe_lib("openxlsx")
safe_lib("rhandsontable")
safe_lib("dplyr")      # data manipulation
safe_lib("shiny")      # Shiny framework
safe_lib("shinyjs")    # JavaScript helpers for Shiny

# Explicitly load rhandsontable
library("rhandsontable")


# ------------------------------------------------------------
# 7) Ensure correct repository for rhandsontable
# ------------------------------------------------------------
# rhandsontable is sometimes hosted on R-universe,
# so we include both R-universe and CRAN.
options(repos = c(
  jrowen = "https://jrowen.r-universe.dev",
  CRAN   = "https://cloud.r-project.org"
))

# Reinstall rhandsontable to avoid version issues
#install.packages("rhandsontable")

# Load the package (should load without errors)
library(rhandsontable)


# ------------------------------------------------------------
# 8) Working directory checks
# ------------------------------------------------------------
# Shows the current working directory
getwd()

# Sets the working directory to the SRT app folder
# Use setwd(), if you need to set the working directory from here.
#setwd()


# ------------------------------------------------------------
# 9) File existence checks
# ------------------------------------------------------------
# These checks confirm that all required files exist
# in the correct folders. All of them should return TRUE.

file.exists("App.R")
file.exists("data/Base.xlsx")
file.exists("data/Stress.xlsx")
file.exists("data/Stress_backloaded.xlsx")
file.exists("www/mod90.css")

