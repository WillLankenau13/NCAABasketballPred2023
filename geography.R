library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")

#Load Files
cities <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/Cities.csv")
m_cities <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MGameCities.csv")
w_cities <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WGameCities.csv")
