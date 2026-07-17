# Setup script

# CRAN packages to load
packages = list(
  CRAN = c("remotes", "ggplot2", "readr", "tidyr", "rnaturalearth", "robis", 
            "sf", "dplyr", "patchwork", "stars", "tidysdm", "effectplots", "caret", "ranger", "cofbb"),
  GITHUB = list(
    andreas = c(repos = "BigelowLab/andreas", ref = "main"),
    copernicus = c(repos = "BigelowLab/copernicus", ref = "main"),
    cofbb = c(repos = "BigelowLab/cofbb", ref = "main"),
    twinkle = c(repos = "BigelowLab/twinkle", ref = "main")
    )
)

# install blocks for setup on new machine
#
# check for prior installations and install locally as needed
#installed = installed.packages() |> rownames()
#if ("CRAN" %in% names(packages)){
#  ix = packages$CRAN %in% installed
#  for (package in packages$CRAN[!ix]) {
#    install.packages(package)
#  }
#}

#if ("GITHUB" %in% names(packages)){
#  ix = names(packages$GITHUB) %in% installed
#  for(package in names(packages$GITHUB)[!ix]) {
#    remotes::install_github(getElement(packages$GITHUB[[package]], "repos"),
#                            ref = getElement(packages$GITHUB[[package]], "ref"))
#  }
#}

# load packages
suppressPackageStartupMessages({
  for (package in packages$CRAN) library(package, character.only = TRUE)
  for (package in names(packages$GITHUB)) library(package, character.only = TRUE)
})

# Next we check the 'functions' directory for ".R" files and source those
for (f in list.files("functions", pattern = glob2rx("*.R"), full.names = TRUE)) {
  source(f, echo = FALSE)
}

# Finally set path to the data hopefully as a sibling to the project directory
# The data directory has top level subdirectories ("buoys", "coast", "brickman")
# that contain data used by all, and to which you wil add your own data.

ROOT_DATA_PATH = "/mnt/s1/projects/ecocast/projectdata/fishkillhabs"

if (!dir.exists(ROOT_DATA_PATH)) {
  ok = dir.create(ROOT_DATA_PATH, recursive = TRUE)
  macosx_junk = file.path(ROOT_DATA_PATH, "__MACOSX") 
  if (dir.exists(macosx_junk)) unlink(macosx_junk, recursive = TRUE, force = TRUE)
}