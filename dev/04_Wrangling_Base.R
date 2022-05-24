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

vdem_0021 <- vdem_df[vdem_df$year > 1999, c("country", 
                                            "code", 
                                            "year", 
                                            "v2x_polyarchy", 
                                            "v2x_libdem",
                                            "v2x_regime")]

vdem_0021 <- subset(vdem_df, year > 1999, 
                    select = c(country, 
                               code, 
                               year, 
                               v2x_polyarchy, 
                               v2x_libdem,
                               v2x_regime))

# Sorting

index <- order(vdem_0021$country, vdem_0021$year)

vdem_0021 <- vdem_0021[index,]

head(vdem_0021)

row.names(vdem_0021) <- 1:nrow(vdem_0021)

# Variable modification and creation

region_df$region <- factor(region_df$region)

summary(region_df)

subset(region_df, region == "North America")

summary(vdem_0021)

which(is.na(vdem_0021$v2x_libdem))

vdem_0021[which(is.na(vdem_0021$v2x_libdem)), c("country", "year", "v2x_libdem")]

mean(vdem_0021$v2x_libdem)

mean(vdem_0021$v2x_libdem, na.rm = TRUE)

(bahrain.libdem <- vdem_0021$v2x_libdem[vdem_0021$country == "Bahrain"])

names(bahrain.libdem) <- 2000:2021

bahrain.libdem

vdem_0021[is.na(vdem_0021$v2x_libdem),]$v2x_libdem <- bahrain.libdem[3]

vdem_0021$v2x_libdem[vdem_0021$country == "Bahrain"]

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

str(vdem_0021$v2x_regime)

summary(vdem_0021$v2x_regime)

vdem_0021$v2x_regime <- factor(vdem_0021$v2x_regime,
                               labels = c("Closed autocracy",
                                          "Electoral autocracy",
                                          "Electoral democracy",
                                          "Liberal democracy"))

summary(vdem_0021)



# Merging -----------------------------------------------------------------

subset(class_df, code == "EGY", select = economy)

subset(vdem_df, code == "EGY" & year == 2020, select = country)

# In

5 %in% c(3, 5, 7)

c(5, 2, 9) %in% c(3, 5, 7)

unique(vdem_0021[vdem_0021$code %in% region_df$code == FALSE, c("country", "code")])

vdem_0021[vdem_0021$code == "PSG",]$code <- "PSE"

vdem_0021 <- subset(vdem_0021, code != "SML" & code != "ZZB")

# Merge
vdem_0021 <- merge(vdem_0021, region_df, by = "code", all.x = TRUE)

summary(vdem_0021)


# Grouping and summarizing ------------------------------------------------

tapply(vdem_0021$v2x_polyarchy, vdem_0021$region, mean)

(libdem.region <- round(with(vdem_0021, tapply(v2x_libdem, region, mean)), 2))

polyarchy.region <- round(with(vdem_0021, tapply(v2x_polyarchy, region, mean)), 2)

polyarchy.region - libdem.region

aggregate(v2x_libdem ~ region_abb, data = vdem_0021, mean)

polyarchy_region <- 
  aggregate(v2x_polyarchy ~ year + region_abb, data = vdem_0021, mean)

head(polyarchy_region, 10)

class(polyarchy_region)

(regime.reg <- with(vdem_0021, 
                    table("Region" = region_abb, 
                          "Regime Type" = v2x_regime)))

addmargins(regime.reg)

(regime.reg.prop <- round(prop.table(regime.reg, margin = 1), digits = 2))

vdem_0021 <- vdem_0021[c("code", "country", "year", "region", 
                         "region_abb","v2x_polyarchy", "v2x_libdem", 
                         "v2x_regime")]

summary(vdem_0021)


# Save --------------------------------------------------------------------

saveRDS(vdem_0021, "data/vdem_0021.rds")
