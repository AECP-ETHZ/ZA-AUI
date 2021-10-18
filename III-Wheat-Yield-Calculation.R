################################################################################
# Wheat yield computation from ZA-AUI data                                     #
# date: 18/10/2021                                                             #
# generated file: III-WW_Revenue.csv                                           #
# Hervé D., Philippe M, Niklas M.                                              #
################################################################################

setwd("Y:/Papers and Data/Datasets/ZA AUI Data/data_transformation_HD_PM_NM")

library(tidyverse)
library(DescTools)

#Read-in data for the years 2009 to 2018
#External data from ZA-AUI database (protected data)

d9 <- read.csv2("./Feldkalender/Feldkalender_2009.csv", header = T, stringsAsFactors = F)
d10 <- read.csv2("./Feldkalender/Feldkalender_2010.csv", header = T, stringsAsFactors = F)
d11 <- read.csv2("./Feldkalender/Feldkalender_2011.csv", header = T, stringsAsFactors = F)
d12 <- read.csv2("./Feldkalender/Feldkalender_2012.csv", header = T, stringsAsFactors = F)
d13 <- read.csv2("./Feldkalender/Feldkalender_2013.csv", header = T, stringsAsFactors = F)
d14 <- read.csv2("./Feldkalender/Feldkalender_2014.csv", header = T, stringsAsFactors = F)
d15 <- read.csv2("./Feldkalender/Feldkalender_2015.csv", header = T, stringsAsFactors = F)
d16 <- read.csv2("./Feldkalender/Feldkalender_2016.csv", header = T, stringsAsFactors = F)
d17 <- read.csv2("./Feldkalender/Feldkalender_2017.csv", header = T, stringsAsFactors = F)
d18 <- read.csv2("./Feldkalender/Feldkalender_2018.csv", header = T, stringsAsFactors = F)

ds <- list(d9, d10, d11, d12, d13, d14, d15, d16, d17, d18)

years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

# Loading the file with correspondent AUI.ID for AUI.Betriebsnr.

ID <- read.csv2("Zuweisung_BetrNr_AUI_ID.csv", header = T, stringsAsFactors = F)

# Replace ZA.Betriebnsr. identification variable by the unique AUI.ID identification variable

IDConverter <- cbind.data.frame(ID$AUI.ID, ID$AUI.Betriebsnummer)
colnames(IDConverter) <- c("AUI.ID", "AUI.Betriebsnummer")
IDConverter <- with(IDConverter, setNames(AUI.ID, AUI.Betriebsnummer))

dfs <- lapply(years, function(y) {
  df <- read.csv2(paste0("./Feldkalender/Feldkalender_", y, ".csv"), header = T, stringsAsFactors = F)
  within(df, ZA.Betriebsnr. <- unname(IDConverter[as.character(ZA.Betriebsnr.)]))
})

for (i in 1:length(years)) {
  ds[[i]] <- dfs[[i]]
  colnames(ds[[i]])[colnames(ds[[i]]) == "ZA.Betriebsnr."] <- "AUI.ID"
}

# Use dataset with converted information but no aggregation

dat <- read.csv2("Pesticide_cleaned_nobook_noagg_ind_definiert_09_18.csv")

nec <- c(
  "SIdent", "Schlag.ID", "KID", "AUI.ID", "Schlagflaeche", "Flaeche", "PSMBez", "PSMIdent", "Menge.Produkt", "Menge.Produkt.ha", "Menge.Produkt.ha2",
  "Massnahmeflaeche2", "Herbizid", "Insektizid", "Fungizid", "Rest", "AIag", "AIA", "AIAha", "FL", "HL", "TL", "STIQuotient", "Kultur", "Datum", "Kulturertrag", "Jahr"
) # Use only neccesary information

table(nec %in% names(dat)) # spelling mistakes?

dat <- dat[nec[nec %in% names(dat)]]

### Check for WW Ext if they use "gebeiztes Saatgut"

# Provide years and AUI.ID and plots with Extenso WW
dat_ids <- dat[, c("AUI.ID", "Schlag.ID", "Jahr", "Kultur")]
Jahre <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

ext_list <-
  dat_ids %>%
  filter(Kultur == "WinterweizenExt") %>%
  select(Schlag.ID, Jahr) %>%
  distinct(Schlag.ID, Jahr)


# From original ZA-AUI data: Massnahmentypnr. == 2 (Saat/Pflanzung) und == 5,6 (Ernte) are interesting

# First start with Saat (2)

test <- as.data.frame(cbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c(
  "gebeizt", "Gebeizt",
  "gebeizt Gebeizt", "hallo",
  "Gr?tze ungebeizt",
  "unGebeizt", "Galle geb.",
  "gebeizte Gr?tze", "Gr?tze gebeizt ist lecker",
  "Gr?tze non trait."
))) # Check how to use the string recognition
int1 <- "gebeizt"
int2 <- "geb[:punct:]"
int3 <- "(Gebeizt|gebeizt|geb\\.|trait?es|trait\\.)"
int4 <- "(non|ungebeizt)"
int5 <- "(Winterweizen|Bl? d'automne)"
int6 <- "(combin?|comb\\.|komb\\.|kobiniert)"
checks <- rbind(test[str_detect(test$V2, int1), ], test[str_detect(test$V2, int2), ])
checks2 <- test[str_detect(test$V2, int3), ]
checks3 <- checks2[!(str_detect(checks2$V2, regex(int4, ignore_case = T))), ]

# First use table(o3$Produktbezeichnung) to find all the possible ways of naming a product gebeizt (gebeizt,Gebeizt,geb.,traite?s.....) ... than use str_detect or tidyr (14.4.4 Grouped matches ...on bookmark) to extract those plots
ext_check <- list()

for (i in 1:length(years)) {
  ext_nec <- ext_list[ext_list$Jahr == Jahre[[i]], ]

  o <- ds[[i]]

  o2 <- o[o$Schlag.ID %in% ext_nec$Schlag.ID, ]

  o2 <- o2[, c("AUI.ID", "Schlag.ID", "Massnahmentypnr.", "Massnahmentyp", "Massnahme", "Produktbezeichnung")]

  o3 <- o2[o2$Massnahmentypnr. %in% c(2), ]

  o4 <- o3[str_detect(o3$Produktbezeichnung, int3), ] # Filter those with variations of gebeizt

  o5 <- o4[!(str_detect(o4$Produktbezeichnung, regex(int4, ignore_case = T))), ] # Kick those out with non traitees / ungebeizt

  o6 <- o5[str_detect(o5$Produktbezeichnung, int5), ] # Only use those which have Winterweizen/Ble de automne in their name

  o7 <- o6[str_detect(o6$Produktbezeichnung, regex(int6, ignore_case = T)), ] # Only those which have combined seed treatment are forbidden under Extenso

  o8 <- unique(o7$Schlag.ID) # Extract IDs of those which cannot be Extenso

  ext_check[[i]] <- o8
}

# Change those plots from WWExt to WW

for (i in 1:length(years)) {
  dat$Kultur[dat$Jahr == Jahre[[i]] & dat$Kultur == "WinterweizenExt" & dat$Schlag.ID %in% ext_check[[i]]] <- "Winterweizen"
}

### Identify those which grow WW and WW Ext in the same year
dat_ids <- dat[, c("AUI.ID", "Schlag.ID", "Jahr", "Kultur")]

rel_ids <-
  dat_ids %>%
  filter(Kultur %in% c("Winterweizen", "WinterweizenExt")) %>%
  mutate(ind_cult = ifelse(Kultur == "Winterweizen", 1,
    ifelse(Kultur == "WinterweizenExt", 2, 0)
  )) %>%
  distinct(AUI.ID, Jahr, ind_cult)


rel_ids2 <-
  rel_ids %>%
  select(AUI.ID, Jahr, ind_cult) %>%
  group_by(AUI.ID, Jahr) %>%
  summarize(rel = sum(ind_cult, na.rm = T))

table(rel_ids2$rel) # rel=1 WW, rel=2 WWExt, rel=3 both   - 3 is only possible if WWExt was breadwheat and WW conv fodderwheat

# Extract those plots where WW conv is cultivated and the farmer grows WW Ext in the same year
WWfodder_ids <-
  merge(dat, rel_ids2, by = c("AUI.ID", "Jahr")) %>%
  select(AUI.ID, Schlag.ID, Jahr, Kultur, rel) %>%
  filter(rel == 3, Kultur == "Winterweizen")

# First look at information regarding harvest: If they are Weizen Top, I,II,II --> cannot be fooder wheat
WWnotfodder_plots <- list()

test <- as.data.frame(cbind(c(1, 2, 3, 4, 5, 6), c("I", "II", "III", "Top", "Weizen Top", "Weizen II"))) # Check how to use the string recognition
int1 <- "(I|Top)"
checks <- test[str_detect(test$V2, int1), ]

for (i in 1:length(years)) {
  o <- ds[[i]]
  o2 <- o[, c("AUI.ID", "Schlag.ID", "Massnahmentypnr.", "Massnahmentyp", "Massnahme", "Produktbezeichnung")]

  o3 <- WWfodder_ids[WWfodder_ids$Jahr == Jahre[[i]], ] # select the right year

  o4 <- o2[o2$Schlag.ID %in% o3$Schlag.ID, ] # select those plots where they grow WW conv and farmer grows both in a given year

  o5 <- o4[o4$Massnahmentypnr. %in% c(5, 6), ] # select harvest measures

  o6 <- o5[str_detect(o5$Produktbezeichnung, int1), ] # select those with Weizen I, II, III or Top

  o7 <- ds[[i]]
  o8 <- o7[, c("AUI.ID", "Schlag.ID", "Massnahmentypnr.", "Massnahmentyp", "Massnahme", "Produktbezeichnung", "Datum")]

  o9 <- o8[o8$Schlag.ID %in% o6$Schlag.ID, ] # to make sure no mistake is done look at events in this plot as a whole again

  WWnotfodder_plots[[i]] <- unique(o9$AUI.ID)
}

# The above identified farmers (WWnotfodder_plots) are not allowed to grow extensive breadwheat according to IP-SUisse because they grow intensive breadwheat already --> change those plots from WW Ext to WW int

WWfodderext_ids <- list() # Extenso plots that are in question

for (i in 1:length(years)) {
  WWfodderext_ids[[i]] <-
    dat %>%
    select(AUI.ID, Schlag.ID, Jahr, Kultur) %>%
    filter(Jahr == Jahre[[i]], Kultur == "WinterweizenExt", AUI.ID %in% WWnotfodder_plots[[i]])
}

WWnotfodderext_plots <- list() # And those of these Extenso plots where breadwheat is grown

test <- as.data.frame(cbind(c(1, 2, 3, 4, 5, 6), c("I", "II", "III", "Top", "Weizen Top", "Weizen II"))) # Check how to use the string recognition
int1 <- "(I|Top)"
checks <- test[str_detect(test$V2, int1), ]

for (i in 1:length(years)) {
  o <- ds[[i]]
  o2 <- o[, c("AUI.ID", "Schlag.ID", "Massnahmentypnr.", "Massnahmentyp", "Massnahme", "Produktbezeichnung")]

  o3 <- WWfodderext_ids[[i]] # select the right year

  o4 <- o2[o2$Schlag.ID %in% o3$Schlag.ID, ] # select those plots where they grow WW conv and farmer grows both in a given year

  o5 <- o4[o4$Massnahmentypnr. %in% c(5, 6), ] # select harvest measures

  o6 <- o5[str_detect(o5$Produktbezeichnung, int1), ] # select those with Weizen I, II, III or Top

  WWnotfodderext_plots[[i]] <- unique(o6$Schlag.ID) # Change those plots from Ext to WW
}

# Change those plots from WWExt to WW

for (i in 1:length(years)) {
  dat$Kultur[dat$Jahr == Jahre[[i]] & dat$Kultur == "WinterweizenExt" & dat$Schlag.ID %in% WWnotfodderext_plots[[i]]] <- "Winterweizen"
}

dat$Kultur[dat$Jahr == 2011 & dat$Schlag.ID %in% c("77356E0D-C1C3-4593-9DD2-79B05D", "71F8AABD-2057-469F-B55F-93BD21")] <- "WinterweizenExt" # No indications of intensive were found


# delete plots with yield == 0 in 2013

noyield <-
  ds[[5]] %>%
  filter(Kultur == "Winterweizen", Kulturertrag == 0) %>%
  select(Schlag.ID) %>%
  distinct(Schlag.ID) # identify IDs with yield ==0 in 2013 and no hail damage

dat <- dat[!(dat$Jahr == 2013 & dat$Schlag.ID %in% noyield$Schlag.ID), ] # delete the plots


dat <- dat[dat$Kultur %in% c("Kartoffeln", "Winterweizen"), ] # fulfill requirement 1a): cultures

table(dat$Kultur) # check cultures left

#############  2.) Add prices and yields= revenues ##################

# a) Create a Value Variable (yield*price) ####

# Results in avg rev per ha over all years per AUI.ID are saved in revenue_Pot_all and revenue_WW_all

# a - i) Winterwheat ####

cat_WW <- list()
count_WW_prod <- list()
double_WW_prod <- list()
no_WW_prod <- list()

test <- as.data.frame(cbind(c(1, 2, 3, 4, 5, 6), c("I", "II", "III", "Top", "Weizen Top", "Weizen II"))) # Check how to use the string recognition
int1 <- "(I|Top|Weizen|Biscuits)" # wanted expressions
int2 <- "(Weizenstroh)" # unwanted expressions
int3 <- "Arina"
checks <- test[str_detect(test$V2, int1), ]

for (i in 1:length(years)) {
  o <- ds[[i]] # raw data

  o2 <- o[, c("AUI.ID", "Schlag.ID", "Massnahmentypnr.", "Massnahmentyp", "Massnahme", "Produktbezeichnung")]

  o3 <- dat[dat$Kultur == "Winterweizen" & dat$Jahr == Jahre[[i]], ] # plots used for analysis

  o4 <- o2[o2$Schlag.ID %in% o3$Schlag.ID & o2$Massnahmentypnr. %in% c(5, 6), ] # only harvest

  o5 <- o4[str_detect(o4$Produktbezeichnung, int1), ] # those which have Weizen in their name

  o6 <- o5[!(str_detect(o5$Produktbezeichnung, int2)), ] # take those with straw in their name out

  cat_WW[[i]] <- o6 # list with relevant products harvested per plot

  o7 <- # count if there are multiple products indicated on some plots (indicates mistake)
    unique(o6) %>%
    group_by(AUI.ID, Schlag.ID, Produktbezeichnung) %>%
    summarise(classification = n())

  o7b <- # 2 stands for 2 product names per plot
    o7 %>%
    filter(classification == 2)

  double_WW_prod[[i]] <- # identify plots with two product indications --> why? -> hay is indicated differently here: no problem because classes are consistent
    o6 %>%
    filter(Schlag.ID %in% o7b$Schlag.ID)

  o8 <- o7[, c("AUI.ID", "Schlag.ID", "classification")]

  o9 <- # join information from counting with all used plots: see which plots do not have product information yet
    merge(o3, o8, by = c("AUI.ID", "Schlag.ID"), all.x = T) %>% # plots with no info will receive an NA in classification variable
    select(AUI.ID, Schlag.ID, classification) %>%
    distinct(AUI.ID, Schlag.ID, classification)

  count_WW_prod[[i]] <- o9 # save this information to get an overview

  o10 <- # those plots which have no indication of products
    o9 %>%
    filter(is.na(classification)) %>%
    select(Schlag.ID)

  o11 <- o2[o2$Schlag.ID %in% o10$Schlag.ID & o2$Massnahmentypnr. %in% c(2), ] # those which have no product indication, sewing information

  no_WW_prod[[i]] <- unique(o11) # those with Arina are Weizen I ... the rest not identified

  o12 <- o11[str_detect(o11$Produktbezeichnung, int3), ] # those are the unnamed ones that can be identified as Arina

  # create dataset
  o13 <- unique(o3[, c("AUI.ID", "Schlag.ID")]) # alle IDs used

  o14 <- merge(o13, o6, by = c("AUI.ID", "Schlag.ID"), all.x = T) # merge with information on products

  o14$Produktbezeichnung <- as.character(o14$Produktbezeichnung)
  o14$Produktbezeichnung[o13$Schlag.ID %in% o12$Schlag.ID] <- "Weizen I" # Arina plots are Weizen I
  o14$Produktbezeichnung[is.na(o14$Produktbezeichnung)] <- "Average" # fill empty fields with word Average

  o15 <- o14[, c("AUI.ID", "Schlag.ID", "Produktbezeichnung")]

  cat_WW[[i]] <- unique(o15) # list with relevant products harvested per plot revised: no double entries ... only empty fields have to be filled with averages
}

# Price information from Agridea

Years <- as.numeric(c(
  rep(2009, 7), rep(2010, 7), rep(2011, 7), rep(2012, 7), rep(2013, 7),
  rep(2014, 7), rep(2015, 7), rep(2016, 7), rep(2017, 7), rep(2018, 7)
))
Klasse <- rep(c("Weizen Top", "Weizen I", "Weizen II", "Weizen III", "Weizen Biscuits", "Weizen", "Average"), 5)
Price <- as.numeric(c(
  51, 48, 43, 40, 44.5, 36, 44.5,
  51, 48, 43, 40, 44.5, 36, 44.5,
  53, 50, 47, 44, 48, 36.5, 48,
  53, 50, 47.5, 44, 48, 36.5, 48,
  52, 50.5, 49.5, 44.5, 48.5, 36.5, 44.5,
  52, 50, 49, 45, 49, 36.5, 49, 
  52, 50, 49, 45, 49, 36.5, 49, 
  52, 50, 49, 45, 49, 36.5, 49, 
  52, 50, 49, 45, 49, 36.5, 49, 
  52, 50, 49, 45, 49, 36.5, 49
)) # Prices per dt (100kg)

prices_WW <- as.data.frame(cbind(Years, Klasse, Price))
prices_WW$Price <- as.numeric(prices_WW$Price)#as.numeric(levels(prices_WW$Price))[prices_WW$Price]

# Join information: classification, prices per class, yields, plotsize

revenue_WW_yearly <- list()
checks_revenue <- list()
checks_yield <- list()

for (i in 1:length(years)) {
  o <- ds[[i]] # Raw data

  o2 <- dat[dat$Kultur == "Winterweizen" & dat$Jahr == Jahre[[i]], ] # plots used for analysis

  o3 <- o2[o2$Schlag.ID %in% o2$Schlag.ID, ] # plots used in the analysis

  o4 <- unique(o3[, c("AUI.ID", "Schlag.ID", "Schlagflaeche", "Kulturertrag")]) # IDs, acreage per plot, yield in 100kg/ha
  
  o4$Kulturertrag <- as.numeric(o4$Kulturertrag)

  checks_revenue[[i]] <- dim(o4)[1] - length(unique(o4$Schlag.ID)) # if zero no doubble entries: Yes!!!

  o5 <- merge(o4, cat_WW[[i]], by = c("AUI.ID", "Schlag.ID"))

  o5$Kulturertrag[o5$Kulturertrag == 980] <- 98 # correct typo in AUI.ID 10

  o6 <- prices_WW[prices_WW$Years == Jahre[[i]], ] # Prices in this year
  
  
  o7 <-
    merge(o5, o6, by.x = c("Produktbezeichnung"), by.y = c("Klasse")) %>%
    mutate(Rev = Kulturertrag * Price) # Rev per ha

  check_ID <- o7$Schlag.ID[o7$Kulturertrag < 40]

  checks_yield[[i]] <- o[o$Schlag.ID %in% check_ID, ]

  revenue_WW_yearly[[i]] <- o7
}

# Correct Outliers (very obvious)

o <- revenue_WW_yearly[[1]]

o$Kulturertrag[o$AUI.ID == 278] <- o$Kulturertrag[o$AUI.ID == 278] * 10
o$Rev[o$AUI.ID == 278] <- o$Rev[o$AUI.ID == 278] * 10

revenue_WW_yearly[[1]] <- o

##################### 3. Save Files #######################################################

for (i in 1:length(years)) {
  revenue_WW_yearly[[i]]$Jahr <- rep(Jahre[[i]], times = dim(revenue_WW_yearly[[i]])[1])
}

revenue_WW <- rbind(
  revenue_WW_yearly[[1]], revenue_WW_yearly[[2]], revenue_WW_yearly[[3]], revenue_WW_yearly[[4]], revenue_WW_yearly[[5]],
  revenue_WW_yearly[[6]], revenue_WW_yearly[[7]], revenue_WW_yearly[[8]], revenue_WW_yearly[[9]],
  revenue_WW_yearly[[10]]
)

summary(revenue_WW)

write.csv2(revenue_WW, "III-WW_Revenue.csv", row.names = F)
