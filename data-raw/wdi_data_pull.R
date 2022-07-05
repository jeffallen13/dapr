#-----------------------------------------------------------------------------#
# WDI Data Pull
#-----------------------------------------------------------------------------#

library(WDI)

wdi_codes <- read.csv("data-raw/wdi_codes.csv")

wdi.codes <- wdi_codes$Code

wdi.names <- c("gdp", "gdpgr", "gdppc", "population", "inflation",
               "trade_gdp", "debt_gdp", "fdi_gdp", "investment_gdp",
               "consumption_gdp", "savings_gdp", "gdebt_gdp", "govex_gdp",
               "milex_gdp", "milex_govex", "internet")

names(wdi.codes) <- wdi.names

wdi_df <- WDI(indicator = wdi.codes, start = 2000, extra = TRUE)

class(wdi_df)

str(wdi_df)
summary(wdi_df)

saveRDS(wdi_df, "data-raw/wdi.rds")

# The labels are really nice 
# Best of both worlds 
# You can use this label later for plotting 
attr(wdi_df$gdp, 'label')

# You still have to do almost all data wrangling tasks
# Column selection, row filtering, reshape, join, etc. 