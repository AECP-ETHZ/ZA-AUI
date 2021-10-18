################################################################################
# Fertilizer conversion from ZA-AUI data                                       #
# date: 18/10/2021                                                             #
# generated file: I-Fert_N_per_plot.csv                                        #
# Hervé D., Philippe M, Niklas M.                                              #
################################################################################


setwd("Y:/Papers and Data/Datasets/ZA AUI Data/data_transformation_HD_PM_NM")

library(tidyverse)
library(DescTools)

#Steps to convert raw ZA-AUI data on fertilizer in comparable fertilizer units per plot/farmer/year

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

#Creating ds list with data from 2009 to 2018

ds <- list(d9, d10, d11, d12, d13, d14, d15, d16, d17, d18)

#Creating a vector for the years 2009 to 2018

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

# Create variable: Menge.Produkt.ha = Menge.Produkt/Massnahmeflaeche

for (i in 1:length(years)) {
  o <- ds[[i]]
  Menge.Produkt.ha <- c()
  Menge.Produkt.ha <- (as.numeric(o$Menge.Produkt) / as.numeric(o$Massnahmeflaeche))
  Menge.Produkt.ha[is.na(Menge.Produkt.ha)] <- 0
  o2 <- cbind(o, as.numeric(Menge.Produkt.ha))
  colnames(o2)[colnames(o2) == "as.numeric(Menge.Produkt.ha)"] <- "Menge.Produkt.ha"
  ds[[i]] <- o2
}

# Loading the products data and kick out the fruits

Produkte <- read.csv2("Produkte.csv", header = T, stringsAsFactors = F)

Notinteresting <- Produkte$Produkte[c(6:17, 23, 25, 27, 28, 31, 33, 38, 39, 40, 47:60, 43, 63:67)]
interesting <- Produkte[!(Produkte$Produkte %in% Notinteresting), ]

# Loading fertilizer conversion table

fert_conv <- read.csv2("Fertilizer_conversion_new2.csv", header = T, stringsAsFactors = F)

# Fields that don't have Einheiten (units) are empty == "", for example:

View(fert_conv[1:10, c(1, 2, 10)])

# Number of lines without Units:

dim(fert_conv[fert_conv$Einheiten == "", ])

# List of units and number of line for each unit:

table(fert_conv$Einheiten[fert_conv$Einheiten != ""])

# 1. Relevant part of ZA-AUI data
# a) 
#     i)"dat" contains the cleaned data
#     ii)"o" contains the raw data of each year
#     iii)"o3" contains the raw data only for relevant crops (can be extended)
#     iv)"o4" has cleaned double entries
#     v)Save "clean" data in dat list

dat <- list()

for (i in 1:length(years)) {
  o <- ds[[i]]
  o3 <- o[(o$Kultur %in% interesting), ] 
  o3$Produktbezeichnung[(!(is.na(o3$orig..Produktbezeichnung))) & o3$orig..Produktbezeichnung != ""] <- o3$orig..Produktbezeichnung[(!(is.na(o3$orig..Produktbezeichnung))) & o3$orig..Produktbezeichnung != ""] # manual changes to the names of products are misleading
  o4 <- o3 %>%
    distinct(AUI.ID, Schlag.ID, Schlagflaeche, Kultur, Datum, Massnahme, Massnahmedetail.ID,
             Massnahmentyp, Massnahmentypnr., Massnahmeflaeche, Produktbezeichnung, 
             Menge.Produkt.ha, Einheit.Produkt, keep_all = T) 
  dat[[i]] <- o4
}

# b) Delete data which contains Outliers (fond through aggregates, inspection)

# For 2009

o <- dat[[1]]
o <- o[o$AUI.ID != 101, ]
o <- o[o$AUI.ID != 190 & o$Kultur != "Silomais", ]
o <- o[o$AUI.ID != 267 & o$Kultur != "Silomais", ]
o <- o[o$AUI.ID != 222 & o$Kultur != "Zuckerrüben", ]
o <- o[o$Massnahmedetail.ID != "9DFDA6A8-BC81-44BB-8EC7-FB3FB8", ]
dat[[1]] <- o

# For 2010

o <- dat[[2]]

o <- o[o$Massnahmedetail.ID != "E23BD655-8511-47F2-871F-7B20BE", ]
o <- o[o$Massnahmedetail.ID != "02CB608F-B02F-4F16-99EE-1AA801", ]
o <- o[o$Massnahmedetail.ID != "07493E6C-001F-485D-B5B0-625569", ]
o <- o[o$Massnahmedetail.ID != "1BE5A602-091E-4A31-85AF-0F5929", ]
o <- o[o$Massnahmedetail.ID != "4395E33B-A1B1-4863-9BA1-184FAD", ]
dat[[2]] <- o

# For 2011

o <- dat[[3]]
o <- o[o$Massnahmedetail.ID != "EF16C506-92F3-4A7F-A49A-F97DB7", ]
o <- o[o$Massnahmedetail.ID != "DBB47076-C724-4433-BC80-B21AD3", ]
o <- o[o$Massnahmedetail.ID != "81564FB2-3FD7-4EB8-982E-82EED6", ]
dat[[3]] <- o

# For 2012

o <- dat[[4]]
o <- o[o$Massnahmedetail.ID != "F5848AC8-46A1-4A8F-9EBF-B3A8FD", ]
o <- o[o$Massnahmedetail.ID != "9C2284D3-AAF9-4BB6-A441-D46193", ]
o <- o[o$Massnahmedetail.ID != "9B460057-4D68-4E96-8A12-E41A59", ]
o <- o[o$Massnahmedetail.ID != "F5848AC8-46A1-4A8F-9EBF-B3A8FD", ]
dat[[4]] <- o

# For 2013

o <- dat[[5]]
o <- o[!(o$AUI.ID == 279 & o$Kultur == "Winterweizen"), ]
o <- o[o$Massnahmedetail.ID != "FBECAC5B-EB31-4B21-BEAD-AF74B2", ]
dat[[5]] <- o

# For 2014 (to be checked)

o <- dat[[6]]
dat[[6]] <- o

# For 2015 (to be checked)

o <- dat[[7]]
dat[[7]] <- o

# For 2016 (to be checked)

o <- dat[[8]]
dat[[8]] <- o

# For 2017 (to be checked)

o <- dat[[9]]
dat[[9]] <- o

# For 2018 (to be checked)

o <- dat[[10]]
dat[[10]] <- o

# c) Delete plots which do not contain at least one seeding and one harvesting activity (uncomplete data)

dims <- matrix(nrow = 10, ncol = 2) # Matrix to compare the dimension before and after deletion
colnames(dims) <- c("Old", "New")

dat_corr <- list()

for (i in 1:length(years)) {
  o <- dat[[i]] # basic data
  dims[i, 1] <- length(unique(o$Schlag.ID)) # the number of plots before cleaning

  seeding <- o %>% # seedingevents
    filter(Massnahmentypnr. == 2) %>%
    group_by(Schlag.ID) %>%
    tally() %>%
    transmute(Schlag.ID = Schlag.ID, seedevents = n)

  harvesting <- o %>% # harvestingevents
    filter(Massnahmentypnr. %in% c(5, 6)) %>%
    group_by(Schlag.ID) %>%
    tally() %>%
    transmute(Schlag.ID = Schlag.ID, harvestevents = n)

  Plots <- as.data.frame(o$Schlag.ID) # Plots as indicated before
  colnames(Plots) <- "Schlag.ID"
  Plots <- unique(Plots)

  Plots1 <- merge(Plots, seeding, by = "Schlag.ID", all.x = T) # Merge numbers on events with original plots
  Plots2 <- merge(Plots1, harvesting, by = "Schlag.ID", all.x = T)
  Plots2$seedevents[is.na(Plots2$seedevents)] <- 0
  Plots2$harvestevents[is.na(Plots2$harvestevents)] <- 0

  Plots3 <- Plots2 %>% # delete those where either no seeding or no harvesting is indicated
    filter(harvestevents != 0) %>%
    filter(seedevents != 0)

  dims[i, 2] <- length(unique(Plots3$Schlag.ID)) # save the new number of plots

  dat_corr[[i]] <- o[o$Schlag.ID %in% Plots3$Schlag.ID, ] # save only the plots with at least one seeding and one harvesting activity
}

# d) Define a start and an end date for each culture/plot
# The start date is the day of seeding - 1 month , the end day is the day of the last harvest activity

for (i in c(1:5, 7:9)) {
  o <- dat_corr[[i]]

  o2 <- o %>% # seedingevents
    filter(Massnahmentypnr. == 2) %>%
    filter(Massnahme != "Aussaat Zwischenkultur") %>%
    group_by(Schlag.ID) %>%
    summarize(earliestseeding = min(as.Date(Datum, format = "%d.%m.%Y"))) # The earliest seeding event on the plot

  o2$startdate <- AddMonths(o2$earliestseeding, -2) # substratc another 2 month as a reasonable time period for activities to prepare

  o3 <- o %>% # harvestingevents
    filter(Massnahmentypnr. %in% c(5, 6)) %>%
    group_by(Schlag.ID) %>%
    summarize(enddate = max(as.Date(Datum, format = "%d.%m.%Y"))) # The latest harvest day

  o4 <- merge(o, o2, by = "Schlag.ID")
  o5 <- merge(o4, o3, by = "Schlag.ID")

  dat_corr[[i]] <- o5
}

for (i in c(6, 10)) {
  o <- dat_corr[[i]]
  
  o2 <- o %>% # seedingevents
    filter(Massnahmentypnr. == 2) %>%
    filter(Massnahme != "Aussaat Zwischenkultur") %>%
    group_by(Schlag.ID) %>%
    summarize(earliestseeding = min(as.Date(Datum, format = "%d/%m/%Y"))) # The earliest seeding event on the plot
  
  o2$startdate <- AddMonths(o2$earliestseeding, -2) # substratc another 2 month as a reasonable time period for activities to prepare
  
  o3 <- o %>% # harvestingevents
    filter(Massnahmentypnr. %in% c(5, 6)) %>%
    group_by(Schlag.ID) %>%
    summarize(enddate = max(as.Date(Datum, format = "%d/%m/%Y"))) # The latest harvest day
  
  o4 <- merge(o, o2, by = "Schlag.ID")
  o5 <- merge(o4, o3, by = "Schlag.ID")
  
  dat_corr[[i]] <- o5
}


# e) Delete all activities before startdate and after enddate

for (i in c(1:5, 7:9)) {
  o <- dat_corr[[i]]
  o$Datum <- as.Date(o$Datum, format = "%d.%m.%Y")

  o2 <- o[as.Date(o$Datum) >= as.Date(o$startdate), ]
  o3 <- o2[as.Date(o2$Datum) <= as.Date(o2$enddate), ]

  o4 <- o3[as.Date(o3$earliestseeding) <= as.Date(o3$enddate), ]

  dat_corr[[i]] <- o4
}

for (i in c(6, 10)) {
  o <- dat_corr[[i]]
  o$Datum <- as.Date(o$Datum, format = "%d/%m/%Y")
  
  o2 <- o[as.Date(o$Datum) >= as.Date(o$startdate), ]
  o3 <- o2[as.Date(o2$Datum) <= as.Date(o2$enddate), ]
  
  o4 <- o3[as.Date(o3$earliestseeding) <= as.Date(o3$enddate), ]
  
  dat_corr[[i]] <- o4
}

# f) Correct cases where Application Surface is greater than Plotsize
# Then correct the amount of product applied by the quota of both

for (i in 1:10) {
  o <- dat_corr[[i]]

  o <- o[!is.na(o$Massnahmeflaeche), ]
  o <- o[!is.na(o$Schlagflaeche), ]

  o$plotquota <- as.numeric(o$Schlagflaeche) / as.numeric(o$Massnahmeflaeche)

  o$Menge.Produkt[o$Massnahmeflaeche > o$Schlagflaeche] <- o$Menge.Produkt[o$Massnahmeflaeche > o$Schlagflaeche] * o$plotquota[o$Massnahmeflaeche > o$Schlagflaeche]

  dat_corr[[i]] <- o
}

# g) Only fertilization

dat_fert <- list()

for (i in 1:length(years)) {
  o2 <- dat_corr[[i]] # the corrected data

  o3 <- o2[o2$Massnahmentyp == "Düngung", ] # only fertilization measures relevant

  dat_fert[[i]] <- o3 # save "clean" data in dat list
}

## 2. Merge data and conversion table

# Does conversion table match data?:

# d<-rbind(dat_fert[[1]],dat_fert[[2]],dat_fert[[3]],dat_fert[[4]],dat_fert[[5]],dat_fert[[6]],dat_fert[[7]],dat_fert[[8]],dat_fert[[9]],dat_fert[[10]])
# table((paste(d$Produktbezeichnung,d$Einheit.Produkt)) %in% paste(fert_conv$Produktbezeichnung,fert_conv$Einheit.Produkt))
# Add <-table((paste(d$Produktbezeichnung,d$Einheit.Produkt))[(!(paste(d$Produktbezeichnung,d$Einheit.Produkt)) %in% paste(fert_conv$Produktbezeichnung,fert_conv$Einheit.Produkt))])
# Why do we not have Ammonsalpeter 24 % N 5% Mg 7% S dt   and  Mg-Ammonsalpeter 23% + 7 S dt and Nitrate magnésien 27 % + 2.5 % Mg + 9 % Ca dt and Ricokalk  (Abholung Fabrik) dt and Sulfate d'ammoniaque Mg plus dt Ammonsalpeter 27 % dt ?

# Does conversion table match data?: especially for Zuckerrüben (low aggregate values)

# d<- rbind(dat_fert[[1]],dat_fert[[2]],dat_fert[[3]],dat_fert[[4]],dat_fert[[5]],dat_fert[[6]],dat_fert[[7]],dat_fert[[8]],dat_fert[[9]],dat_fert[[10]]) %>% filter(Kultur=="Zuckerrüben")

# Add <-table((paste(d$Produktbezeichnung,d$Einheit.Produkt))[(!(paste(d$Produktbezeichnung,d$Einheit.Produkt)) %in% paste(fert_conv$Produktbezeichnung,fert_conv$Einheit.Produkt))])

dat_m <- list() # will contain the merged data

for (i in 1:length(years)) {
  o <- dat_fert[[i]] # the cleaned data

  o2 <- merge(o, fert_conv, by = c("Produktbezeichnung", "Einheit.Produkt")) # merge data and table by the columns Produktbezeichung and Einheit

  o3 <- o2[!(is.na(o2$Menge.Produkt)), ] # Kick out Nas

  o3$Faktor_N <- as.numeric(o3$Faktor_N)
  o3$Faktor_N[is.na(o3$Faktor_N)] <- 0

  o3$Faktor_P <- as.numeric(o3$Faktor_P)
  o3$Faktor_P[is.na(o3$Faktor_P)] <- 0

  o3$Faktor_K <- as.numeric(o3$Faktor_K)
  o3$Faktor_K[is.na(o3$Faktor_K)] <- 0

  dat_m[[i]] <- o3
}

# 3. Outlier in Fertilizer applications

# outs1<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]],dat_m[[6]],dat_m[[7]],dat_m[[8]],dat_m[[9]],dat_m[[10]])%>%filter(Produktbezeichnung %in% c("Fosse à purin ouverte","Mistplatz Junghennen","Schweinegülle Swissag","Brinamon flüssig") & Einheiten=="kg/m3")
# outs2<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]],dat_m[[6]],dat_m[[7]],dat_m[[8]],dat_m[[9]],dat_m[[10]])%>%filter(Produktbezeichnung %in% c("Fosse à purin ouverte","Mistplatz Junghennen","Schweinegülle Swissag","Brinamon flüssig") & Einheiten=="kg/t")
# View(outs1) View(outs2)

# a) correct very strong outliers manually

M2013.10 <- c("459E31BC-B445-49C7-B558-4A9D82", "A8F363DB-6C55-48CD-B8A3-A71DDC", "3A383A05-66FB-4906-A088-4FD8AB", "53FEF481-A83C-48BB-95FB-6A271B")

o <- dat_m[[5]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] / 1000
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] / 1000

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2013.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2013.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2013.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2013.10] / 10

dat_m[[5]] <- o

M2010.10 <- c("F7A0FFA8-BEBB-4DA4-A78F-0E9AC2", "F2CC467E-27CC-4333-8971-8AE23D", "6CC766D8-613C-4CE0-B704-B3DD80", "9C3A779D-A07C-461D-85C8-CBC6C2", "7FFA97A8-B162-4ECD-8274-95C5FE", "A340F008-5129-4D41-8949-41739E", "ED010F6F-1CF9-4618-BB11-FA3A1C")

o <- dat_m[[2]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] / 2
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] / 2

o$Menge.Produkt[o$Massnahmedetail.ID %in% c("EA6798C7-2CC5-4BE0-A03B-CE36F2", "EC715ADF-4BF2-4FB5-98E6-1CC488")] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% c("EA6798C7-2CC5-4BE0-A03B-CE36F2", "EC715ADF-4BF2-4FB5-98E6-1CC488")] / 100
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("EA6798C7-2CC5-4BE0-A03B-CE36F2", "EC715ADF-4BF2-4FB5-98E6-1CC488")] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("EA6798C7-2CC5-4BE0-A03B-CE36F2", "EC715ADF-4BF2-4FB5-98E6-1CC488")] / 100

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2010.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2010.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2010.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2010.10] / 10

dat_m[[2]] <- o

M2009.10 <- c("CD3A0F47-EC48-4F8B-96E9-C4DECE", "DB47BEB1-A35C-4EA6-8676-5BABAC", "628FDE0F-3046-421A-AF0D-244255", "AF139021-9689-4817-9520-B64F9E", "62C3E8A2-F80D-4762-A472-1BAB5E", "8BB95009-3295-4F7F-881E-D8169C")

o <- dat_m[[1]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2009.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2009.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2009.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2009.10] / 10

dat_m[[1]] <- o

M2011.10 <- c("BC985B24-F862-4B9F-A858-A35CB2,7927EC80-CBDB-48FC-8FEA-D27681")
o <- dat_m[[3]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2011.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2011.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2011.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2011.10] / 10

dat_m[[3]] <- o

M2012.10 <- c("2DA2FE9B-3CD9-4ADB-8F88-090FD1", "F8E157B0-1A50-457E-A99E-F21D68", "07B4406E-3E9E-4F19-B85C-4D27A0", "2322DFFD-52F7-4024-AF69-614708", "E38619FB-CADE-4F59-A808-16BF5E", "C4B4DD7E-5554-416F-882A-D15C12")

o <- dat_m[[4]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2012.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2012.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2012.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2012.10] / 10

dat_m[[4]] <- o

# b) Find outliers with rules on distribution
# Look at all top ten entries  for each combination of Produktbezeichnung und Menge.Produkt

for (i in 1:10) {
  o <- dat_m[[i]]

  o$Jahr <- rep(years[[i]], times = (dim(o)[1])) # add years to the values

  o <- o[o$Menge.Produkt != 0, ]

  dat_m[[i]] <- o
}

# d_borders<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]],dat_m[[6]],dat_m[[7]],dat_m[[8]],dat_m[[9]],dat_m[[10]])%>% # some summary stats
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   summarize(mean=mean(Menge.Produkt.ha,na.rm=T),sd=sd(Menge.Produkt.ha,na.rm=T),n=n(),min=min(Menge.Produkt.ha,na.rm=T),max=max(Menge.Produkt.ha,na.rm=T))%>%
#   filter(!(is.na(sd)))
#
# d_ranks<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]],dat_m[[6]],dat_m[[7]],dat_m[[8]],dat_m[[9]],dat_m[[10]])%>% # the ranking of values
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   arrange(Produktbezeichnung,Einheit.Produkt, -Menge.Produkt.ha) %>%
#   mutate(rank = rank(-Menge.Produkt.ha))
#
#
# d_outs<-merge(d_ranks,d_borders,by=c("Produktbezeichnung","Einheit.Produkt") )%>%  #connect and filter
#   filter(n.y>10 & rank<=7 & (Menge.Produkt.ha>(2*mean)) | n.y %in% (3:9) & rank<=3 & (Menge.Produkt.ha>(2*mean)))%>%
#   select(n.y,Produktbezeichnung,Einheit.Produkt,rank,Menge.Produkt.ha,mean,Massnahmedetail.ID,Jahr,AUI.ID,Kultur)%>%
#   arrange(desc(n.y),rank,Produktbezeichnung,Einheit.Produkt,Menge.Produkt.ha)

# Only WW in 2013
# d_borders<-dat_m[[5]]%>%
#   filter(Kultur=="Winterweizen") %>%# some summary stats
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   summarize(mean=mean(Menge.Produkt.ha,na.rm=T),sd=sd(Menge.Produkt.ha,na.rm=T),n=n(),min=min(Menge.Produkt.ha,na.rm=T),max=max(Menge.Produkt.ha,na.rm=T))%>%
#   filter(!(is.na(sd)))
#
# d_ranks<-dat_m[[5]]%>%
#   filter(Kultur=="Winterweizen") %>%
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   arrange(Produktbezeichnung,Einheit.Produkt, -Menge.Produkt.ha) %>%
#   mutate(rank = rank(-Menge.Produkt.ha))
#
#
# d_outs<-merge(d_ranks,d_borders,by=c("Produktbezeichnung","Einheit.Produkt") )%>%  #connect and filter
#   filter(n.y>10 & rank<=7 & (Menge.Produkt.ha>(2*mean)) | n.y %in% (3:9) & rank<=3 & (Menge.Produkt.ha>(2*mean)))%>%
#   select(n.y,Produktbezeichnung,Einheit.Produkt,rank,Menge.Produkt.ha,mean,Massnahmedetail.ID,Jahr,AUI.ID,Kultur)%>%
#   arrange(desc(n.y),rank,Produktbezeichnung,Einheit.Produkt,Menge.Produkt.ha)

## 4. Convert units

# What when dt is the unit? How high is Menge.Produkt?

# d<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]],dat_m[[6]],dat_m[[7]],dat_m[[8]],dat_m[[9]],dat_m[[10]])
# table(d$Menge.Produkt[d$Einheiten=="Annahmen"])
# What are the units when "Einheiten" is empty ?
# table(d$Einheit.Produkt[d$Einheiten==""])


# Creation of Lists

dat_conv <- list() # will contain converted units
dat_check_a <- list() # will contain graphs to check if there are strong outliers in the one without Einheiten
dat_check_b <- list() # will contain graphs to check if there are strong outliers in the one with Einheiten
dat_tab_a <- list() # will contain tables to check if there are strong outliers in the one without Einheiten
dat_tab_b <- list() # will contain tables to check if there are strong outliers in the one with Einheiten

# Correcting outlier in Einheit.Produkt and making histograms to see if there are strong outliers

for (i in 1:length(years)) {
  o <- dat_m[[i]] # the merged data

  # correct outlier in Einheit.Produkt

  o$Einheit.Produkt[o$Produktbezeichnung == "Ammonsalpeter 27.5 %" & o$Einheit.Produkt == "kg"] <- "dt"


  o.a <- o[o$Einheiten == "", ] # two different conversion rules for those where Einheiten does not have a unit

  o.a <- o.a[!(o.a$Einheit.Produkt %in% c("Am", "l", "hl", "Wagen", "mg")), ] # exclude all rows where there is not enough information to use the entry

  o.a$N <- o.a$Menge.Produkt * o.a$Faktor_N # N in kg

  o.a$N[o.a$Einheit.Produkt == "dt"] <- o.a$N[o.a$Einheit.Produkt == "dt"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg
  o.a$N[o.a$Einheit.Produkt == "t"] <- o.a$N[o.a$Einheit.Produkt == "t"] * 1000 # the standard unit should be kg: convert cases where t is the outcome to kg

  o.a$P <- o.a$Menge.Produkt * o.a$Faktor_P # P in kg

  o.a$P[o.a$Einheit.Produkt == "dt"] <- o.a$P[o.a$Einheit.Produkt == "dt"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg
  o.a$P[o.a$Einheit.Produkt == "t"] <- o.a$P[o.a$Einheit.Produkt == "t"] * 1000 # the standard unit should be kg: convert cases where t is the outcome to kg

  o.a$K <- o.a$Menge.Produkt * o.a$Faktor_K # K in kg

  o.a$K[o.a$Einheit.Produkt == "dt"] <- o.a$K[o.a$Einheit.Produkt == "dt"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg
  o.a$K[o.a$Einheit.Produkt == "t"] <- o.a$K[o.a$Einheit.Produkt == "t"] * 1000 # the standard unit should be kg: convert cases where t is the outcome to kg


  o.a$N_ha <- o.a$Menge.Produkt.ha * o.a$Faktor_N # N in kg/ha

  o.a$N_ha[o.a$Einheit.Produkt == "dt"] <- o.a$N_ha[o.a$Einheit.Produkt == "dt"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg
  o.a$N_ha[o.a$Einheit.Produkt == "t"] <- o.a$N_ha[o.a$Einheit.Produkt == "t"] * 1000 # the standard unit should be kg: convert cases where t is the outcome to kg

  o.a$P_ha <- o.a$Menge.Produkt.ha * o.a$Faktor_P # P in kg/ha

  o.a$P_ha[o.a$Einheit.Produkt == "dt"] <- o.a$P_ha[o.a$Einheit.Produkt == "dt"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg
  o.a$P_ha[o.a$Einheit.Produkt == "t"] <- o.a$P_ha[o.a$Einheit.Produkt == "t"] * 1000 # the standard unit should be kg: convert cases where t is the outcome to kg

  o.a$K_ha <- o.a$Menge.Produkt.ha * o.a$Faktor_K # K in kg/ha

  o.a$K_ha[o.a$Einheit.Produkt == "dt"] <- o.a$K_ha[o.a$Einheit.Produkt == "dt"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg
  o.a$K_ha[o.a$Einheit.Produkt == "t"] <- o.a$K_ha[o.a$Einheit.Produkt == "t"] * 1000 # the standard unit should be kg: convert cases where t is the outcome to kg


  o.b <- o[o$Einheiten != "", ] # ... and where it has a unit

  o.b <- o.b[!(o.b$Einheiten %in% c("Landi- Dünger", "in %", "0")), ] # exclude all rows where there is not enough information to use the entry:   What about the 0 ????

  o.b$N <- o.b$Menge.Produkt * o.b$Faktor_N # N in kg

  o.b$N[o.b$Einheiten == "g/l"] <- o.b$N[o.b$Einheiten == "g/l"] / 1000 # the standard unit should be kg: convert cases where g is the outcome to kg
  o.b$N[o.b$Einheiten == "Annahmen"] <- o.b$N[o.b$Einheiten == "Annahmen"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg

  o.b$P <- o.b$Menge.Produkt * o.b$Faktor_P # P in kg

  o.b$P[o.b$Einheiten == "g/l"] <- o.b$P[o.b$Einheiten == "g/l"] / 1000 # the standard unit should be kg: convert cases where g is the outcome to kg
  o.b$P[o.b$Einheiten == "Annahmen"] <- o.b$P[o.b$Einheiten == "Annahmen"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg

  o.b$K <- o.b$Menge.Produkt * o.b$Faktor_K # K in kg

  o.b$K[o.b$Einheiten == "g/l"] <- o.b$K[o.b$Einheiten == "g/l"] / 1000 # the standard unit should be kg: convert cases where g is the outcome to kg
  o.b$K[o.b$Einheiten == "Annahmen"] <- o.b$K[o.b$Einheiten == "Annahmen"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg


  o.b$N_ha <- o.b$Menge.Produkt.ha * o.b$Faktor_N # N in kg

  o.b$N_ha[o.b$Einheiten == "g/l"] <- o.b$N_ha[o.b$Einheiten == "g/l"] / 1000 # the standard unit should be kg: convert cases where g is the outcome to kg
  o.b$N_ha[o.b$Einheiten == "Annahmen"] <- o.b$N_ha[o.b$Einheiten == "Annahmen"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg

  o.b$P_ha <- o.b$Menge.Produkt.ha * o.b$Faktor_P # P in kg

  o.b$P_ha[o.b$Einheiten == "g/l"] <- o.b$P_ha[o.b$Einheiten == "g/l"] / 1000 # the standard unit should be kg: convert cases where g is the outcome to kg
  o.b$P_ha[o.b$Einheiten == "Annahmen"] <- o.b$P_ha[o.b$Einheiten == "Annahmen"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg

  o.b$K_ha <- o.b$Menge.Produkt.ha * o.b$Faktor_K # K in kg

  o.b$K_ha[o.b$Einheiten == "g/l"] <- o.b$K_ha[o.b$Einheiten == "g/l"] / 1000 # the standard unit should be kg: convert cases where g is the outcome to kg
  o.b$K_ha[o.b$Einheiten == "Annahmen"] <- o.b$K_ha[o.b$Einheiten == "Annahmen"] * 100 # the standard unit should be kg: convert cases where dt is the outcome to kg

  o2 <- rbind(o.a, o.b) %>% # connect the two again, select only relevant columns and apply unique
    select(AUI.ID, Schlag.ID, Kultur, Massnahmedetail.ID, Massnahmeflaeche, Schlagflaeche, Menge.Produkt, Einheit.Produkt, N, N_ha) %>%
    distinct(AUI.ID, Schlag.ID, Kultur, Massnahmedetail.ID, Massnahmeflaeche, Schlagflaeche, Menge.Produkt, Einheit.Produkt, N, N_ha)

  dat_conv[[i]] <- o2 # save the converted dataset

  o_check_a <- # make histograms to see if there are strong outliers
    o.a %>%
    select(N, P, K, Einheit.Produkt) %>%
    mutate(Einheit.Produkt = as.factor(Einheit.Produkt))

  gg_a <- ggplot(o_check_a, aes(x = N))
  dat_check_a[[i]] <- gg_a + geom_histogram() + xlim(c(0, 20)) + ggtitle(years[[i]])

  dat_tab_a[[i]] <-
    table(o.a$N)

  o_check_b <-
    o.b %>%
    select(N, P, K, Einheit.Produkt) %>%
    mutate(Einheit.Produkt = as.factor(Einheit.Produkt))

  gg_b <- ggplot(o_check_b, aes(x = N))
  dat_check_b[[i]] <- gg_b + geom_histogram() + ggtitle(years[[i]])

  dat_tab_b[[i]] <-
    table(o.b$N)
}

# d<-rbind(dat_conv[[1]],dat_conv[[2]],dat_conv[[3]],dat_conv[[4]],dat_conv[[5]],dat_conv[[6]],dat_conv[[7]],dat_conv[[8]],dat_conv[[9]],dat_conv[[10]])
# two<-(d%>%select(Einheiten,Einheit.Produkt,Menge.Produkt.ha,Produktbezeichnung,N_ha,K_ha,P_ha)%>%group_by(Produktbezeichnung,Einheit.Produkt,Einheiten)%>%summarize(mean_Menge.Produkt.ha=mean(Menge.Produkt.ha,na.rm=T),mean_N_ha=mean(N_ha,na.rm=T),mean_K_ha=mean(K_ha,na.rm=T),mean_P_ha=mean(P_ha,na.rm=T),n=n())%>%arrange(desc(mean_N_ha)))

# d<-rbind(dat_conv[[1]],dat_conv[[2]],dat_conv[[3]],dat_conv[[4]],dat_conv[[5]],dat_conv[[6]],dat_conv[[7]],dat_conv[[8]],dat_conv[[9]],dat_conv[[10]])
# table(duplicated(d$Massnahmedetail.ID))

## 5. Aggregate fertilizer per plot + culture

dat_ag <- list()
dat_ag_kult_farm <- list()

for (i in 1:10) {
  o <- dat_conv[[i]]
  p <- dat_corr[[i]]

  o2 <- o %>% # aggregate N and N_ha on a plot+culture level per year
    group_by(AUI.ID, Kultur, Schlag.ID) %>%
    summarize(N_total = sum(N, na.rm = T), N_ha_total = sum(N_ha, na.rm = T), flaeche = mean(as.numeric(Schlagflaeche), na.rm = T))

  o3 <- o %>%
    group_by(AUI.ID, Kultur) %>%
    summarize(N_tot_kult = sum(N))

  o4 <- p %>%
    select(AUI.ID, Kultur, Schlag.ID, Schlagflaeche) %>%
    distinct(AUI.ID, Kultur, Schlag.ID, .keep_all = T) %>%
    group_by(AUI.ID, Kultur) %>%
    summarize(surface = sum(as.numeric(Schlagflaeche)))

  o5 <- merge(o3, o4, by = c("AUI.ID", "Kultur")) %>%
    group_by(AUI.ID, Kultur) %>%
    summarize(N_ha = N_tot_kult / surface, surface = surface, Jahr = years[[i]])

  dat_ag[[i]] <- o2
  dat_ag_kult_farm[[i]] <- o5
}

# d2<-rbind(dat_ag_kult_farm[[1]],dat_ag_kult_farm[[2]],dat_ag_kult_farm[[3]],dat_ag_kult_farm[[4]],dat_ag_kult_farm[[5]])
# View(d2)

d <- rbind(dat_ag[[1]], dat_ag[[2]], dat_ag[[3]], dat_ag[[4]], dat_ag[[5]], dat_ag[[6]], dat_ag[[7]], dat_ag[[8]], dat_ag[[9]], dat_ag[[10]])
dat_ag_kult_all <- d %>%
  group_by(Kultur) %>%
  summarize(N_tot_kult = sum(N_total), Surface = sum(flaeche)) %>%
  mutate(N_ha_kult = N_tot_kult / Surface)

# View(dat_ag_kult_all)

# Again for Zuckerrüben

for (i in 1:10) {
  o <- dat_ag[[i]]

  o$Jahr <- rep(years[[i]], times = dim(o)[1])

  dat_ag[[i]] <- o
}

# d<-rbind(dat_ag[[1]],dat_ag[[2]],dat_ag[[3]],dat_ag[[4]],dat_ag[[5]],dat_ag[[6]],dat_ag[[7]],dat_ag[[8]],dat_ag[[9]],dat_ag[[10]])
# dat_ag_kult_z<-d%>%
#   filter(Kultur=="Zuckerrüben")%>%
#   group_by(Jahr,AUI.ID)%>%
#   summarize(N_tot_kult=sum(N_total),Surface=sum(flaeche))%>%
#   mutate(N_ha_kult=N_tot_kult/Surface)%>%
#   arrange(Jahr,N_ha_kult)

# View(dat_ag_kult_z)  --> 80 kg N is actually ok --> recoomended max is 100

## 6. Save outcomes ####

# All dat_ag in one dataframe and then save

dat_ag <- rbind(dat_ag[[1]], dat_ag[[2]], dat_ag[[3]], dat_ag[[4]], dat_ag[[5]], dat_ag[[6]], dat_ag[[7]], dat_ag[[8]], dat_ag[[9]], dat_ag[[10]])

write.csv2(dat_ag, "I-Fert_N_per_plot.csv", row.names = F)


# Was needed to correct the conversion tables ...
# write.csv2(Add,"Conversion of units/Bearbeitet Gregor/Add.csv",row.names = F)
# write.csv2(two,"Conversion of units/Bearbeitet Gregor/Outlier_fert.csv",row.names = F)
```