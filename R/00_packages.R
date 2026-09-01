# About -------------------------------------------------------------------
# HOSPITAL SURGE CAPACITY MODEL
# PAHO/WHO
# Script: Packages
# Last-mod-date: August 2026
# R 4.5.2

# Library -----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(markdown)
library(thematic)
library(rintrojs)
library(simmer)
library(future.apply)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(openxlsx)
# pkgs <- c(
#   # Shiny interface
#   "shiny",
#   "shinydashboard",
#   "rintrojs",
# 
#   # Discrete-event simulation
#   "simmer",
# 
#   # Parallel simulation
#   "future.apply",
# 
#   # Data processing and visualisation
#   "dplyr",
#   "ggplot2",
#   "plotly",
# 
#   # Excel import and export
#   "readxl",
#   "openxlsx",
# 
#   # PDF report tables and graphics
#   "grid",
#   "gridExtra"
# )

# Install if missing (no loading) -----------------------------------------
# installed <- rownames(installed.packages())
# missing <- setdiff(pkgs, installed)
# if (length(missing)) install.packages(missing)
# rm(installed, missing)
# 
# # Load packages -----------------------------------------------------------
# suppressMessages({
#   suppressWarnings({
#     invisible(lapply(pkgs, library, character.only = TRUE))
#   })
# })
# 
# rm(pkgs)

# Genera manifest.json
# Para publicar una aplicación Shiny en R desde GitHub, Connect Cloud necesita conocer sus dependencias. 
# Desde la raíz del proyecto ejecuta: rsconnect::writeManifest(appDir = ".")