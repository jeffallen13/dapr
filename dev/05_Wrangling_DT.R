#-----------------------------------------------------------------------------#
# Wrangling with Data Table
#-----------------------------------------------------------------------------#

library(data.table)

options(scipen = 999)


# Import and Inspect ------------------------------------------------------

## NB: WDI data pulled from WDI package (see appendix)
# setDT or data.table(); former recommended for data frames
wdi_df <- setDT(readRDS("data-raw/wdi.rds"))

# Prints first and last 5 rows
wdi_df[,1:7]

# Prefers characters as headers
groups_df <- fread("data-raw/OGHIST.csv")

groups_df[,1:7]

groups_df <- fread("data-raw/OGHIST.csv", 
                   # Specify header
                   header = TRUE,
                   # Drop country
                   drop = "country",
                   # Character vector of column names
                   col.names = c("code", paste0("Y", 1987:2020)),
                   # Convert ".." to NA
                   na.strings = "..")

groups_df[,1:7]

# Data table inherits from data frame
class(wdi_df)

# Because inherits, can use many of same functions
summary(wdi_df)
str(wdi_df)

# Label attribute is really nice in WDI


# Reshaping ---------------------------------------------------------------

# In previous chapter, one of last topics
# If need to reshape to put together dataset, often one of first tasks
# Why? Inefficient to manipulate until data are in shape 
# May become irrelevant or re-do

# Reshape country groups: longer
# Notice, uses the language of reshape2

groups_l <- melt(groups_df, id.vars = "code", value.name = "income_group")

groups_l

# Base R plotting multiple countries or series -> wide
# Formula interface

gdpgr_w <- dcast(wdi_df, year ~ country, value.var = "gdpgr")

gdpgr_w[,1:4]

rm(groups_df, gdpgr_w)


# Data Frame Manipulation -------------------------------------------------

# Reference semantics
# DT[i, j, by]

# Big differences we'll focus on
# (1) Row orientation
# (2) Reference column names directly
# (3) Updating in place 
# (4) Group by (section x.x)

# Row subsetting

wdi_df[1:5]

wdi_countries <- wdi_df[region != "Aggregates"]

rm(wdi_df)

# Column selection

wdi_countries[, 1:5]

wdi_countries[1:5, 1:5]

wdi_countries[, .(country, iso2c)]

# Drop using - / !
# For non-contiguous, specify with quotes
# For contiguous, can do like so:
wdi_countries <- wdi_countries[, -(status:lastupdated)]
## NB: Gap is selecting multiple subsets of columns 

# Unique about data.table in R is update "by reference" or "in place"
# Careful, the Python-related copy() issues are there but mostly applicable to 
# function writing (see Vignette 2, 7:10)
# Explain why you don't need these 
# Use :=

wdi_countries[, c("region", "income", "lending") := NULL]
## When you modify in place, dim changes take time to show in viewer
dim(wdi_countries)

# NAs 
summary(wdi_countries)

apply(wdi_countries, 2, function(x) mean(is.na(x)))

# Drop gdebt_gdp

wdi_countries[, c("gdebt_gdp") := NULL]
dim(wdi_countries)

# Sorting

groups_l <- groups_l[order(code, variable)]

wdi_countries <- wdi_countries[order(country, year)]

# Rename by reference

setnames(groups_l, "variable", "year")

setnames(wdi_countries, "iso3c", "code")


# Variable modification and creation
# This is all done by reference/in-place
# Here we'll mostly do modification, but principles apply to creation

# Use :=
# For one var, basic syntax is DT[, column := value]
groups_l[, year := gsub("Y", "", year)]

# Two different ways to modify multiple 
# First way:
groups_l[, `:=` (year = as.integer(year),
                 income_group = factor(income_group, 
                                       levels = c("L", "LM", "UM", "H")))]

groups_l
summary(groups_l)

# Second way:
# Two variables, GDP and population, not normalized
# Big numbers can be tough to work with; convert to millions
wdi_countries[, c("gdp", "population") := list(gdp / 1000000000,      # billions
                                               population / 1000000)] # millions


# Separate: add to attributes (not a DT thing)
attributes(wdi_countries$population)
attributes(wdi_countries$population)$label
# We can overwrite this

attributes(wdi_countries$population)$label <- 
  paste0(attributes(wdi_countries$population)$label, " (M)")

attributes(wdi_countries$population)$label

# Verbose, and this is where would be good to write function 
attributes(wdi_countries$gdp)$label <- 
  paste0(
    substr(attributes(wdi_countries$gdp)$label, start = 1, 
           stop = nchar(attributes(wdi_countries$gdp)$label) - 1),
    "B)"
  )
  
attributes(wdi_countries$gdp)$label


# Merging -----------------------------------------------------------------

# Very similar to data frame 

wdi_20 <- merge(wdi_countries, groups_l, 
                by = c("code", "year"), all.x = TRUE)

vdem_20 <- as.data.table(readRDS("data/vdem_20.rds"))

ipe_df <- merge(vdem_20, 
                wdi_20[, -c("country")], 
                by = c("code", "year"), all.x = TRUE)


# Organization --------------------------------------------------------

# Re-arrange columns
id_vars <- c("country", "code", "iso2c", "capital", 
             "region", "region_abb", "longitude", "latitude", 
             "year", "income_group")

# Notice selection methods
# Alternative is to use setcolorder
ipe_df <- cbind(ipe_df[, ..id_vars], 
                ipe_df[, v2x_polyarchy:v2x_regime],
                ipe_df[, gdp:internet])

# SAVE
saveRDS(ipe_df, "data/ipe.rds")
saveRDS(wdi_20, "data/wdi_20.rds")

rm(list=ls())

# Grouping, summarizing --------------------------------------------------

# Vignette 1: 6-8, 10-19
# Vignette 2 (Reference semantics): 6-7 (for sub-query)

ipe_df <- readRDS("data/ipe.rds")

# Working with j directly

# How many country-years 50% or below internet?
ipe_df[, sum(internet < 50, na.rm = TRUE)]

# Mean of gdppc and gdpgr for countries in EAP
## See Vignette 1, pg. 7 for explanation about why this is so fast
ipe_df[region_abb == "EAP", .(gdppc = mean(gdppc, na.rm = TRUE), 
                              gdpgr = mean(gdpgr, na.rm = TRUE), 
                              count = .N)] # Notice .N (always important)

# Using by

## Number of observations for intersection of region and income group across the panel
ipe_df[, .N, by = .(region_abb, income_group)]

## How about just for 2019?
ipe_df[year == 2019, .N, by = .(region_abb, income_group)]

## The original ordering of vars is preserved

## Again, all of this is fast and efficient

## Mean of gdppc and v2x_libdem by region AND SORT using keyby
ipe_df[, .(v2x_libdem = mean(v2x_libdem, na.rm = TRUE), 
           gdppc = mean(gdppc, na.rm = TRUE), 
           count = .N), keyby = region]
## Sorts in alphabetical 

## Or, by chaining 
ipe_df[, .(v2x_libdem = mean(v2x_libdem, na.rm = TRUE), 
           gdppc = mean(gdppc, na.rm = TRUE), 
           count = .N), by = region][order(region)]

# By accepts expressions
# How many liberal democracies 
# ipe_df[, .N, by = .(v2x_regime, milex_gdp > 2)] # ehhh...

# Using .SD (subset of data) + lapply 
# Here, extension: .SDcols
ipe_df[, lapply(.SD, median), by = region, .SDcols = gdp:inflation]

# Need anonymous
ipe_df[, lapply(.SD, function(x) round(median(x, na.rm = TRUE), 2)), 
       by = region, .SDcols = gdp:inflation]

# Sub-query: add group GDP: year, region, income_group
# Can use this to compare, impute 
# We have done this by reference 
ipe_df[, group_gdpgr := mean(gdpgr, na.rm = TRUE), 
       by = .(year, region, income_group)]

# Use this for imputation
# Could imagine taking the sum of GDP and creating a new var that reps share of 
# group GDP


# Setting Keys ------------------------------------------------------------

# Different framework for conducting many of same operations
# Faster when working with big datasets (will illustrate; binary search)
# Most directly related to subsetting rows
# But works in tandem with column selection, group, aggregation, etc.

# Set
setkey(ipe_df, year, v2x_regime)

# Check check
key(ipe_df)

view.vars <- c("country", "year", "v2x_regime")

# setkey sorts the data table
ipe_df[, ..view.vars]

##--Subsetting (core)--##

# Subset Liberal democracies in 2019
ipe_df[.(2019, "Liberal democracy"), ..view.vars]

# Differs from original 
# ipe_df[year == 2019 & v2x_regime == "Liberal democracy]

# Just subset first var
ipe_df[.(2019), ..view.vars]

# Just subset based on second var
ipe_df[.(unique(year), "Closed autocracy"), ..view.vars]

# Find biggest GDP in 2019
ipe_df[.(2019), .(country, year, gdp)][order(-gdp)]

# Aggregate
ipe_df[.(2019), .(gdp = sum(gdp, na.rm = TRUE),
                  count = .N), keyby = v2x_regime]

# Finally, remove key
setkey(ipe_df, NULL)
key(ipe_df)

# Binary search 
# Fun explanation: https://www.youtube.com/watch?v=YzT8zDPihmc
# Vector scan: search everything
# What enables? Sorting...


# WDI Pull Appendix -------------------------------------------------------

# See data-raw/wdi_data_pull.R
