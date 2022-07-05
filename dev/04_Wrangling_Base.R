#----------------------------------------------------------------------------#
# Wrangling with Base R
#----------------------------------------------------------------------------#


# Importing and inspecting ------------------------------------------------

class_df <- read.csv("data-raw/CLASS.csv")

class(class_df)

vdem_df <- readRDS("data-raw/V-Dem-CY-Core-v12.rds")

dim(vdem_df)

head(class_df)

head(vdem_df, c(10, 6))

str(class_df)

summary(class_df)

str(vdem_df[c("country_text_id", "year", "v2x_polyarchy", "v2x_libdem")])

summary(vdem_df[c("country_text_id", "year", "v2x_polyarchy", "v2x_libdem")])



# Data Frame Manipulation -------------------------------------------------

# Renaming

colnames(class_df) <- tolower(colnames(class_df))

colnames(vdem_df)[1:2] <- c("country", "code")

# Column selection

# Using column indices
region_df <- class_df[2:3]

# Using negative indexing
region_df <- class_df[-c(1,4)]

# Using column names 
region_df <- class_df[c("code", "region")]

# Row subsetting

region_df[region_df$code == "IND",]

vdem_20 <- vdem_df[vdem_df$year > 1999, c("country", 
                                            "code", 
                                            "year", 
                                            "v2x_polyarchy", 
                                            "v2x_libdem",
                                            "v2x_regime")]

vdem_20 <- subset(vdem_df, year > 1999, 
                    select = c(country, 
                               code, 
                               year, 
                               v2x_polyarchy, 
                               v2x_libdem,
                               v2x_regime))

# Sorting

index <- order(vdem_20$country, vdem_20$year)

vdem_20 <- vdem_20[index,]

head(vdem_20)

row.names(vdem_20) <- 1:nrow(vdem_20)

# Variable modification and creation

region_df$region <- factor(region_df$region)

summary(region_df)

subset(region_df, region == "North America")

summary(vdem_20)

which(is.na(vdem_20$v2x_libdem))

vdem_20[which(is.na(vdem_20$v2x_libdem)), c("country", "year", "v2x_libdem")]

mean(vdem_20$v2x_libdem)

mean(vdem_20$v2x_libdem, na.rm = TRUE)

(bahrain.libdem <- vdem_20$v2x_libdem[vdem_20$country == "Bahrain"])

names(bahrain.libdem) <- 2000:2021

bahrain.libdem

vdem_20[is.na(vdem_20$v2x_libdem),]$v2x_libdem <- bahrain.libdem[3]

vdem_20$v2x_libdem[vdem_20$country == "Bahrain"]

levels(region_df$region)

region_df$region_abb <- factor(region_df$region, 
                               labels = c("EAP", 
                                          "ECA", 
                                          "LAC", 
                                          "MENA", 
                                          "NAM", 
                                          "SAS", 
                                          "SSA"))

summary(region_df)

str(vdem_20$v2x_regime)

summary(vdem_20$v2x_regime)

vdem_20$v2x_regime <- factor(vdem_20$v2x_regime,
                               labels = c("Closed autocracy",
                                          "Electoral autocracy",
                                          "Electoral democracy",
                                          "Liberal democracy"))

summary(vdem_20)



# Merging -----------------------------------------------------------------

subset(class_df, code == "EGY", select = economy)

subset(vdem_df, code == "EGY" & year == 2020, select = country)

# In

5 %in% c(3, 5, 7)

c(5, 2, 9) %in% c(3, 5, 7)

unique(vdem_20[vdem_20$code %in% region_df$code == FALSE, c("country", "code")])

vdem_20[vdem_20$code == "PSG",]$code <- "PSE"

vdem_20 <- subset(vdem_20, code != "SML" & code != "ZZB")

# Merge
vdem_20 <- merge(vdem_20, region_df, by = "code", all.x = TRUE)

summary(vdem_20)


# Reshaping ---------------------------------------------------------------

# Reshaping: pivoting data longer or wider 
# Here we didn't need to put together dataset
# However, also critical for plotting groups
# Best to start with example

# Reshape and reshape2

nam_df <- subset(vdem_20, region_abb == "NAM",
                 select = c(country, year, v2x_libdem))

# Wider

nam_w <- reshape(nam_libdem, direction = "wide", 
                 idvar = "year", timevar = "country")

nam_w2 <- reshape2::dcast(nam_libdem, year ~ country)

# What happened? Spread out countries
# Tradeoff between wide and dimensions; hard to store multiple dims

# Longer

## If data come like this...

nam_l <- reshape(nam_w, direction = "long", 
                 varying = 2:3,
                 timevar = "country",
                 idvar = "year",
                 new.row.names = 1:(22*2))

nam_l2 <- reshape2::melt(nam_w2, id.vars = "year",
                         value.name = "v2x_libdem",
                         variable.name = "country")

## More complex
## Need to make two dimensions longer (ggplot use case)
## AND, have multiple dimensions need to freeze
vdem_20_w <- reshape2::melt(vdem_20[2:5], 
                            id.vars = c("country", "year"))


# Easier to see if ordered by country
vdem_20_w <- vdem_20_w[order(vdem_20_w$country),]


# Grouping and summarizing ------------------------------------------------

tapply(vdem_20$v2x_polyarchy, vdem_20$region, mean)

(libdem.region <- round(with(vdem_20, tapply(v2x_libdem, region, mean)), 2))

polyarchy.region <- round(with(vdem_20, tapply(v2x_polyarchy, region, mean)), 2)

polyarchy.region - libdem.region

aggregate(v2x_libdem ~ region_abb, data = vdem_20, mean)

polyarchy_region <- 
  aggregate(v2x_polyarchy ~ year + region_abb, data = vdem_20, mean)

head(polyarchy_region, 10)

class(polyarchy_region)

(regime.reg <- with(vdem_20, 
                    table("Region" = region_abb, 
                          "Regime Type" = v2x_regime)))

addmargins(regime.reg)

(regime.reg.prop <- round(prop.table(regime.reg, margin = 1), digits = 2))


# Finishing touches -------------------------------------------------------

vdem_20 <- vdem_20[c("code", "country", "year", "region", 
                         "region_abb","v2x_polyarchy", "v2x_libdem", 
                         "v2x_regime")]

summary(vdem_20)


# Save --------------------------------------------------------------------

saveRDS(vdem_20, "data/vdem_20.rds")
