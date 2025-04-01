# 01-data-download.R
# This script downloads data from the google survey about waste separation,
# which we did in the RBTL course in FS25
# Author: Jan Helfenstein
# Date: 2025-04-01

# libraries
library(googlesheets4)
library(tidyverse)

# import data from google sheet
survey_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1GKnJ1hurZ5QffM6urSp2GiSltdq1ep2GtML-aTGM6v0/edit?usp=sharing")

# store data
write_csv(survey_raw, "data/raw/survey-raw.csv")
