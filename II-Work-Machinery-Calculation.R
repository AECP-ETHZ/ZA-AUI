################################################################################
# Labor and machinery conversion from ZA-AUI data                              #
# date: 18/10/2021                                                             # 
# generated file: II-dat_ag_workmach_total.csv                                 #
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

# Creating a list with ZA-AUI data

ds <- list(d9, d10, d11, d12, d13, d14, d15, d16, d17, d18)

# Produkte data + excluding products that do not interest use (Fruits)

Produkte <- read.csv2("Produkte.csv", header = T, stringsAsFactors = F)

Notinteresting <- Produkte$Produkte[c(6:17, 23, 25, 27, 28, 31, 33, 38, 39, 40, 47:60, 43, 63:67)]
interesting <- Produkte[!(Produkte$Produkte %in% Notinteresting), ]

# Creating a vector of the length of the number of years in data

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

# Loading Work-machinery conversion table

workmach_conv <- read.csv2("workmach_conversion_17Jan.csv", header = T, stringsAsFactors = F)

# Loading data for amount N fertilizer per plot

fert <- read.csv2("I-Fert_N_per_plot.csv", header = T, stringsAsFactors = F)

# Loading data for plots + Massnahmendetail.ID that were used to compute pesticide use

pest <- read.csv2("Pestoutlier.csv", header = T, stringsAsFactors = F) # no clue on this one HD

# Loading the file with correspondent AUI.ID for AUI.Betriebsnr.

IDConverter <- read.csv2("Zuweisung_BetrNr_AUI_ID.csv", header = T, stringsAsFactors = F)

## 1. Relevant part of ZA-AUI data

# a) Clean the data into new dat matrix
# i)o3-> only relevant crops (can be extended)
# ii)o3-> only measures that have a non-zero surface
# iii)o4-> clean double entries (very general)

dat <- list()

for (i in 1:length(years)) {
  o <- ds[[i]] # the raw data

  o3 <- o[(o$Kultur %in% interesting), ]

  o3 <- o3[!(is.na(o3$Massnahmeflaeche)), ]

  o4 <- o3 %>%
    distinct(AUI.ID, Schlag.ID, Schlagflaeche, Kultur, Datum, Massnahme, Massnahmeflaeche, Massnahmedetail.ID, Massnahmentypnr., Produktbezeichnung, Menge.Produkt.ha, Einheit.Produkt)

  dat[[i]] <- o4
}

# b) Delete data which contains Outliers: found during the work on the pesticide and fertilizer files
# Delete single measures ... plots or even farmers which were deleted in other steps will be dropped when merging files

# From the pesticide file:

for (i in 1:10) {
  o <- dat[[i]] # the partly cleaned data
  p <- pest[pest$Jahr == years[[i]], ] # the measures which where deleted before pesticide aggregation

  o <- o[!(o$Massnahmedetail.ID %in% p$Massnahmedetail.ID), ]

  dat[[i]] <- o
}  #no clue on this one so not checked HD

# From the fertilizer file:

o <- dat[[1]]

o <- o[o$AUI.ID != 101, ]
o <- o[o$AUI.ID != 190 & o$Kultur != "Silomais", ]
o <- o[o$AUI.ID != 267 & o$Kultur != "Silomais", ]
o <- o[o$AUI.ID != 222 & o$Kultur != "Zuckerrüben", ]
o <- o[o$Massnahmedetail.ID != "9DFDA6A8-BC81-44BB-8EC7-FB3FB8", ]

dat[[1]] <- o

o <- dat[[2]]

o <- o[o$Massnahmedetail.ID != "E23BD655-8511-47F2-871F-7B20BE", ]
o <- o[o$Massnahmedetail.ID != "02CB608F-B02F-4F16-99EE-1AA801", ]
o <- o[o$Massnahmedetail.ID != "07493E6C-001F-485D-B5B0-625569", ]
o <- o[o$Massnahmedetail.ID != "1BE5A602-091E-4A31-85AF-0F5929", ]
o <- o[o$Massnahmedetail.ID != "4395E33B-A1B1-4863-9BA1-184FAD", ]

dat[[2]] <- o

o <- dat[[3]]

o <- o[o$Massnahmedetail.ID != "EF16C506-92F3-4A7F-A49A-F97DB7", ]
o <- o[o$Massnahmedetail.ID != "DBB47076-C724-4433-BC80-B21AD3", ]
o <- o[o$Massnahmedetail.ID != "81564FB2-3FD7-4EB8-982E-82EED6", ]

dat[[3]] <- o

o <- dat[[4]]

o <- o[o$Massnahmedetail.ID != "F5848AC8-46A1-4A8F-9EBF-B3A8FD", ]
o <- o[o$Massnahmedetail.ID != "9C2284D3-AAF9-4BB6-A441-D46193", ]
o <- o[o$Massnahmedetail.ID != "9B460057-4D68-4E96-8A12-E41A59", ]
o <- o[o$Massnahmedetail.ID != "F5848AC8-46A1-4A8F-9EBF-B3A8FD", ]

dat[[4]] <- o

o <- dat[[5]]

o <- o[!(o$AUI.ID == 279 & o$Kultur == "Winterweizen"), ]
o <- o[o$Massnahmedetail.ID != "FBECAC5B-EB31-4B21-BEAD-AF74B2", ]

dat[[5]] <- o

o <- dat[[6]]

dat[[6]] <- o

o <- dat[[7]]

dat[[7]] <- o

o <- dat[[8]]

dat[[8]] <- o

o <- dat[[9]]

dat[[9]] <- o

o <- dat[[10]]

dat[[10]] <- o

# c) Delete plots which not contain at least one seeding and one harvesting activity (uncomplete data)

# Matrix to compare the dimension before and after deletion

dims <- matrix(nrow = 10, ncol = 2)
colnames(dims) <- c("Old", "New")

# Save only the plots with at least one seeding and one harvesting activity in dat_corr

dat_corr <- list()

for (i in 1:10) {
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

  o2$startdate <- AddMonths(o2$earliestseeding, -2) # substract another 2 month as a reasonable time period for activities to prepare ground and fertilize

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
  
  o2$startdate <- AddMonths(o2$earliestseeding, -2) # substract another 2 month as a reasonable time period for activities to prepare ground and fertilize
  
  o3 <- o %>% # harvestingevents
    filter(Massnahmentypnr. %in% c(5, 6)) %>%
    group_by(Schlag.ID) %>%
    summarize(enddate = max(as.Date(Datum, format = "%d/%m/%Y"))) # The latest harvest day
  
  o4 <- merge(o, o2, by = "Schlag.ID")
  o5 <- merge(o4, o3, by = "Schlag.ID")
  
  dat_corr[[i]] <- o5
}

# e) Delete all activities before startdate and after enddate, and delete those where harvesting takes place before seeding

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

for (i in 1:10) {
  o <- dat_corr[[i]]

  o <- o[!is.na(o$Massnahmeflaeche), ]
  o <- o[!is.na(o$Schlagflaeche), ]

  o$plotquota <- as.numeric(o$Schlagflaeche) / as.numeric(o$Massnahmeflaeche)
  o$plotquota[is.na(o$plotquota)] <- 0

  o$Massnahmeflaeche[o$plotquota < 1] <- o$Schlagflaeche[o$plotquota < 1]

  dat_corr[[i]] <- o
}

# g) correct cases where work/machinery has a typo or other obvious wrong/missing declarations

o <- dat_corr[[1]]

o$Massnahme[o$Schlag.ID == "63E64ED5-F9DD-48C7-A61A-75079C" & o$Massnahme == "Mähdrusch"] <- "Ernten"

dat_corr[[1]] <- o

for (i in 1:10) {
  o <- dat_corr[[i]]

  o$Massnahme[o$Massnahme == "Bekämpf. Ausfallgetreide" & o$Produktbezeichnung %in% c("Glyfos", "Touchdown", "Touchdown System 4")] <- "Spritzen"

  o$Massnahme[o$Massnahme == "Bekämpf. Ausfallraps" & o$Produktbezeichnung %in% c("Banvel 4 S", "Touchdown System 4")] <- "Spritzen"

  o$Massnahme[o$Massnahme == "Abschleppen" & o$Produktbezeichnung %in% c("Spotlight", "CCC", "Input", "Moddus")] <- "Spritzen"

  o$Massnahme[o$Massnahme == "Bewässern" & o$Produktbezeichnung %in% c("Acrobat MZ WG", "Captan  80 WDG", "Flint", "Forum")] <- "Spritzen"

  o$Massnahme[o$Massnahme == "Bio-Dyn. Präparate ausbringen" & o$Produktbezeichnung %in% c("Isomate-C Plus", "Karate")] <- "Spritzen"

  o$Massnahme[o$Massnahme == "Blacken stechen" & o$Produktbezeichnung %in% c("Plenum WG", "Asulox")] <- "Spritzen"

  o$Massnahme[o$Massnahme == "Mähdrusch" & o$Kultur %in% c("Industriekartoffeln", "Futterkartoffeln", "Futterrüben", "Zuckerrüben")] <- "Ernten"

  o$Produktbezeichnung[o$Massnahmentypnr. == 2 & o$Produktbezeichnung == ""] <- o$Kultur[o$Massnahmentypnr. == 2 & o$Produktbezeichnung == ""] # When a harvesting or seeding activity is indicated, but no productname indentification is not possible --> take culture name
  o$Einheit.Produkt[o$Massnahmentypnr. == 2 & o$Produktbezeichnung == ""] <- o$Einheit.Ertrag.ha[o$Massnahmentypnr. == 2 & o$Produktbezeichnung == ""]

  o$check <- ifelse(o$Massnahmentyp == "Ernte übrige Kulturen" & o$Produktbezeichnung == "", 1, 0)
  o$Produktbezeichnung[o$Massnahmentyp == "Ernte übrige Kulturen" & o$Produktbezeichnung == ""] <- o$Kultur[o$Massnahmentyp == "Ernte übrige Kulturen" & o$Produktbezeichnung == ""]
  # o$Einheit.Produkt[o$check==1] <-  o$Einheit.Ertrag.ha[o$check==1]

  o$check2 <- ifelse(o$Massnahmentyp == "Ernte Futterbau" & o$Produktbezeichnung == "", 1, 0)
  o$Produktbezeichnung[o$Massnahmentyp == "Ernte Futterbau" & o$Produktbezeichnung == ""] <- o$Kultur[o$Massnahmentyp == "Ernte Futterbau" & o$Produktbezeichnung == ""]
  # o$Einheit.Produkt[o$check2==1] <-  o$Einheit.Ertrag.ha[o$check2==1]

  dat_corr[[i]] <- o
}

# Correct wrong entries from work/machinery

# For 2009:

wrongs09 <- c(
  "5BC22A59-7031-49B7-AFE3-FB1443", "62B1F070-3434-4FD8-80E7-D40864", "6E2D492A-EF95-47F5-AF1D-FC1F49", "BA438B63-1D2A-4232-B436-3C638D", "EAE357D2-79F8-4008-BF92-839C20", "79BECAC7-2ED5-43FD-969B-FE774A",
  "E5F3E6EE-EB31-4AC3-B588-AB7B0D", "FA41625D-0A87-46CD-BE4A-5BCAA4", "8B11AEA7-1F33-44E7-8D5C-F5180B", "9EC5FB5C-8327-491B-8442-DE006F", "5ECEE84A-4A53-4A25-9F54-223757", "253EF731-23D4-4D47-938C-866586",
  "BCC0F9C2-BA38-41C1-BABB-0AC9C0", "925BD171-9567-4D5F-B887-D76110", "A71BAC00-6AB7-4657-AB92-B4AA1E", "45D01BBA-99C7-42C3-B86E-4A4D86", "EF45B3D8-C3A3-4FCC-828D-0730FC", "CBF267D8-F975-4186-94A5-5470E5",
  "BE54EBFD-B4CE-42DC-93B2-A38DDB", "71E11E07-DB51-4269-A9ED-867062", "0223BEB1-610B-4032-8AFB-EAEAD9", "D44CC85D-87E4-43CC-B524-3A8AED", "0EE6FAC1-767F-48AC-A4A3-E365B9", "C3CA3E84-F12E-4B71-8109-4CC954",
  "C88DCB5C-A390-4AED-AF1E-4C5F86", "8DFFCAB3-BD2C-4153-9BCF-4D60B9", "5AB8C6B4-698A-4C2F-8180-B7B9F9", "BEF1C411-3F24-49EA-96CD-F22201", "8BF202F1-5739-4107-A176-29A513", "C7C774A7-920F-48EB-B102-C4EB68",
  "D2740F75-E54B-49BB-B772-61A94A", "6FFB98C8-0778-4DDD-988C-FAF41A", "91389F66-24CD-4239-A114-F125E5", "EFABAE63-F8AD-48E1-BD0D-633E2E", "F441A582-C52F-4999-9E05-CCBFF1", "F77C0580-CFBA-49EA-B3A8-A9F71F"
)

o <- dat_corr[[1]]

o <- o[!(o$Massnahmedetail.ID %in% wrongs09), ]

o$Menge.Produkt[o$Massnahmedetail.ID == "FFF262A9-705C-40B6-9A57-D5B697"] <- as.numeric(o$Menge.Produkt.ha[o$Massnahmedetail.ID == "FFF262A9-705C-40B6-9A57-D5B697"]) * as.numeric(o$Massnahmeflaeche[o$Massnahmedetail.ID == "FFF262A9-705C-40B6-9A57-D5B697"])

o$Kultur[o$Schlag.ID == "30A703D3-EB37-40D8-B3C6-8F1BAB"] <- "Sommerweizen"

dat_corr[[1]] <- o

# For 2010:

wrongs10 <- c(
  "32B32C8F-E27C-4B39-BBAC-14243D", "20A33D80-DD48-4328-AF19-E2FE77", "E615E3C0-86D6-4DFE-9AAA-EDE934", "97856DDE-99D0-421A-BCD9-3F55A6", "7705B8BF-7FC1-415A-B1EE-960CFE", "10AE44DD-3988-4E49-8250-604545",
  "98E1F3F9-5DF2-408A-BF17-B40A48", "C5AA48A6-E363-4672-8A74-7FF2E9", "DF043CBD-63BD-4C2D-BC02-6E9DA5", "E145A4BA-ED6B-42BD-AAC8-7CCD1C", "7ED99CA6-B354-4B8F-9551-D6DF3F", "27DF1A23-ED46-476D-BE1D-11334F",
  "5FE439C5-6815-4E91-B0A8-EB77AF", "A285FB48-24A3-4E09-8D82-8224BF", "3F6B757B-EDF1-496D-8A11-DDF1E6", "7AE98DEC-E06C-4D0F-A720-D7E171", "F1488296-B41B-41A7-BDDB-A1637A", "75F090EB-6853-4A10-9EFD-149F3D",
  "76033DC0-0342-4972-A0C6-AA77DF", "B4B4CEF0-AD95-4A53-B09F-FA8CF6", "003C3B2E-5821-4885-9995-7C06C6", "0C221BA7-90A3-4091-9C6A-4CF804", "7A7951BC-9B4A-4BCF-AA52-A7F322", "50B6001D-57FF-44E4-BAD7-6BDE52"
)

o <- dat_corr[[2]]

o <- o[!(o$Massnahmedetail.ID %in% wrongs10), ]

dat_corr[[2]] <- o

# For 2011:

wrongs11 <- c(
  "BF27D8C5-3C79-41F9-9C57-9CCAD1", "93380D19-F708-4D93-9C34-1C02CD", "57327852-5241-49F2-9E6B-E9A1CB", "CB04FBEE-A50C-4E36-B318-0D4B6F", "87556929-1D74-4552-AEE8-0EDAAA", "54A2FF4C-2589-4BE2-993A-689419",
  "7D2D4482-C5BC-425F-ADC1-893C83", "DCE62237-F8F8-48A9-9CA0-B7F9A7", "CF259137-F8BC-463E-9F94-EB6524", "561F0D4F-A66E-4067-931A-E6B28E", "309C34B1-875F-447D-A805-EF474F", "0D238A14-1DD7-40F6-BE29-98E757",
  "EEF1D3F3-8E03-4804-A950-CB962E", "FC94B09E-6738-4EF2-911B-BAD95F", "FCE9184D-AF8A-46F3-9119-477387", "A229B601-A316-4B85-9570-61DC32", "23201DB9-FF24-4C0E-AA76-439976", "962F3226-C53D-4968-B984-2AD15A",
  "6B5768CC-48CD-4508-BA17-D00604", "5E131CB8-6BDC-4ED9-8362-F65873", "0FEBC37B-A9D3-433D-8D64-3CE541", "F79F88D1-605F-4117-A09D-5B7E7A", "C51BB619-5852-462A-A4F9-A652B1", "810E9918-1468-4635-9A70-21B6AA",
  "30F60D0C-88FA-4668-8951-87D12A", "8FF3B8EA-DDC4-462A-BC9C-25FA4D"
)

o <- dat_corr[[3]]

o <- o[!(o$Massnahmedetail.ID %in% wrongs11), ]

dat_corr[[3]] <- o

# For 2012:

wrongs12 <- c("DF6FAE37-717D-4FDF-A368-1CCA74", "07C8C341-9987-4ECA-8F07-BBCF56", "A1F2C45A-FE27-4BA7-9D5D-7882A6", "13EA2D69-358F-4818-8100-9C4CE1")

o <- dat_corr[[4]]

o <- o[!(o$Massnahmedetail.ID %in% wrongs12), ]

dat_corr[[4]] <- o

# For 2013:

wrongs13 <- c(
  "46F23541-DCBB-4C67-B4FD-F94001", "CEC7CF8B-27EF-45F2-93BA-23E226", "A5A473BC-7861-4E4E-ACCE-8CFDC7", "FC6E4BDA-4D7D-47E5-8368-151E75", "5E92F458-4044-41EA-A828-BA5229", "7FBC2591-F6A3-4BD6-90B9-EA9C2B",
  "C0BFC388-0607-48F9-A82C-7F4820", "8B788C5F-71B4-4814-B2F8-E4F287"
)

o <- dat_corr[[5]]

o <- o[!(o$Massnahmedetail.ID %in% wrongs13), ]

dat_corr[[5]] <- o

# For 2014:

wrongs14 <- c()
o <- dat_corr[[6]]
dat_corr[[6]] <- o

# For 2015:

wrongs15 <- c()
o <- dat_corr[[7]]
dat_corr[[7]] <- o

# For 2016:

wrongs16 <- c()
o <- dat_corr[[8]]
dat_corr[[8]] <- o

# For 2017:

wrongs17 <- c()
o <- dat_corr[[9]]
dat_corr[[9]] <- o

# For 2018:

wrongs18 <- c()
o <- dat_corr[[10]]
dat_corr[[10]] <- o

# 2. Merge data and conversion table ####

# does conversion table match data?:

# d<-rbind(dat_corr[[1]],dat_corr[[2]],dat_corr[[3]],dat_corr[[4]],dat_corr[[5]])
# table((paste(d$Massnahme,d$Produktbezeichnung,d$Einheit.Produkt)) %in% paste(workmach_conv$Massnahme,workmach_conv$Produktbezeichnung,workmach_conv$Einheit.Produkt))
# Add <-table( (paste(d$Massnahme,d$Produktbezeichnung,d$Einheit.Produkt)) [!((paste(d$Massnahme,d$Produktbezeichnung,d$Einheit.Produkt)) %in% paste(workmach_conv$Massnahme,workmach_conv$Produktbezeichnung,workmach_conv$Einheit.Produkt))] )
# Add <-table( (paste(d$Massnahme,d$Produktbezeichnung)) [!((paste(d$Massnahme,d$Produktbezeichnung)) %in% paste(workmach_conv$Massnahme,workmach_conv$Produktbezeichnung))] )
# write.csv2(Add,"Conversion of units/Bearbeitet Gregor/Add_to_workmach_6Jan2017.csv",row.names = F)

dat_m <- list() # will contain the merged data

for (i in 1:length(years)) {
  o <- dat_corr[[i]] # the cleaned data

  o2 <- merge(o, workmach_conv, by = c("Massnahme", "Produktbezeichnung", "Einheit.Produkt")) # merge data and table by the columns Massnahme, Produktbezeichung and Einheit

  colnames(o2)[colnames(o2) == "Lohnkosten.Fr..AE"] <- "workmach_cost"

  o2$workmach_cost <- as.numeric(o2$workmach_cost)

  o2$Jahr <- rep(years[[i]], times = (dim(o2)[1])) # add years to the values

  o3 <- o2[!(is.na(o2$Menge.Produkt) & o2$Spezial.ja.nein == 1), ] # Kick out Nas when Spezial = yes

  o4 <- o3[!(o3$Menge.Produkt == 0 & o3$Spezial.ja.nein == 1), ] # Kick out 0's when Spezial = yes

  dat_m[[i]] <- o4
}

## 3. Outlier in Fertilizer applications: still valid here because some measures are counted in m3, dt or kg

# outs1<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]])%>%filter(Produktbezeichnung %in% c("Fosse à purin ouverte","Mistplatz Junghennen","Schweinegülle Swissag","Brinamon flüssig") & Einheiten=="kg/m3")
# outs2<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]])%>%filter(Produktbezeichnung %in% c("Fosse à purin ouverte","Mistplatz Junghennen","Schweinegülle Swissag","Brinamon flüssig") & Einheiten=="kg/t")
# View(outs1) View(outs2)
# Plus outliers in other measures when Spezial = 1 are added

# a) correct very strong outliers manually


# For 2009:

M2009.10 <- c(
  "CD3A0F47-EC48-4F8B-96E9-C4DECE", "DB47BEB1-A35C-4EA6-8676-5BABAC", "628FDE0F-3046-421A-AF0D-244255", "AF139021-9689-4817-9520-B64F9E", "62C3E8A2-F80D-4762-A472-1BAB5E", "8BB95009-3295-4F7F-881E-D8169C",
  "A0538100-1108-4CB3-8D49-7F0048", "CFBA9EDC-AAE3-4CF4-B35C-84D352", "BCC4431E-8896-435B-A67E-1F4C3E", "C61E63A8-4352-40A1-B33D-3C833C", "DF46F1B6-6E45-47F1-99AD-7A36FD"
)
M2009.100 <- c("E277D06C-CD27-4102-B6D5-894544", "4729ECB7-0B00-482D-A868-FB6E50")

o <- dat_m[[1]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2009.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2009.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2009.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2009.10] / 10

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2009.100] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2009.100] / 100
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2009.100] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2009.100] / 100

dat_m[[1]] <- o

# For 2010:

M2010.10 <- c(
  "F7A0FFA8-BEBB-4DA4-A78F-0E9AC2", "F2CC467E-27CC-4333-8971-8AE23D", "6CC766D8-613C-4CE0-B704-B3DD80", "9C3A779D-A07C-461D-85C8-CBC6C2", "7FFA97A8-B162-4ECD-8274-95C5FE", "A340F008-5129-4D41-8949-41739E",
  "ED010F6F-1CF9-4618-BB11-FA3A1C", "13950B80-9C08-4E89-8C3E-46DF89", "F9F44221-7A76-4F98-BDF0-C604F1", "B81F5282-76FC-4AAC-8041-5FF736"
)

M2010.100 <- c("EA6798C7-2CC5-4BE0-A03B-CE36F2", "EC715ADF-4BF2-4FB5-98E6-1CC488", "35CEA8A8-1659-4108-BE26-E9EE1E", "807F7AAE-A2FE-44D1-B5B2-71B61D")

o <- dat_m[[2]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] / 2
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("172B38B1-43AD-475B-8409-949410", "CE3FC4D5-828D-4351-8411-DCAC81")] / 2

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2010.100] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2010.100] / 100
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2010.100] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2010.100] / 100

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2010.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2010.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2010.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2010.10] / 10

dat_m[[2]] <- o

# For 2011:

M2011.10 <- c(
  "BC985B24-F862-4B9F-A858-A35CB2", "7927EC80-CBDB-48FC-8FEA-D27681", "8ADADFEF-EDD8-4FD4-B233-1C3E98", "90645D4D-B865-4AE9-A3F3-8922B8", "3D07C819-E91A-47D4-A62D-F7BE41", "1D231924-E951-4523-BBE9-D4519F",
  "8F4EB4E8-35D9-4FEA-863F-09C0F3", "4AA56535-6BF0-4CC6-9D0F-EC5458", "3A0E243C-57B2-4DBF-B7EB-9A747C"
)
M2011.100 <- c("561B555C-B99A-4A12-8EC6-0D38A7", "28BA54FA-9F60-4440-8F77-59AC02", "99F2732F-6401-4491-A090-C6621A", "B5EF5A6A-9FA8-4CA0-84E7-F7A03A")

o <- dat_m[[3]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2011.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2011.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2011.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2011.10] / 10

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2011.100] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2011.100] / 100
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2011.100] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2011.100] / 100

dat_m[[3]] <- o

# For 2012:

M2012.10 <- c(
  "2DA2FE9B-3CD9-4ADB-8F88-090FD1", "F8E157B0-1A50-457E-A99E-F21D68", "07B4406E-3E9E-4F19-B85C-4D27A0", "2322DFFD-52F7-4024-AF69-614708", "E38619FB-CADE-4F59-A808-16BF5E", "C4B4DD7E-5554-416F-882A-D15C12",
  "09413BA5-4162-4B56-B055-F5A59A", "F649B337-B0B9-419B-B721-E263BC", "D2321585-4B5E-40C3-9F87-797417"
)
M2012.100 <- c("D943EA2D-BE14-4906-A9CF-ACDE2D")

o <- dat_m[[4]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2012.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2012.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2012.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2012.10] / 10

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2012.100] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2012.100] / 100
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2012.100] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2012.100] / 100

o$Massnahme[o$Massnahmedetail.ID == "D943EA2D-BE14-4906-A9CF-ACDE2D"] <- "Mähdrusch"

dat_m[[4]] <- o

# For 2013:

M2013.10 <- c(
  "459E31BC-B445-49C7-B558-4A9D82", "A8F363DB-6C55-48CD-B8A3-A71DDC", "0EC98E8D-7FA6-4F81-9B48-6EE6E6", "3A383A05-66FB-4906-A088-4FD8AB", "41DC0151-5283-4A0F-910A-9291CE", "34A95D03-DB64-4FD4-A020-63E786",
  "B8225D74-8F5F-4BFE-8A47-7F70EB", "FD74C1D7-7792-488D-A7E9-947143"
)

o <- dat_m[[5]]

o$Menge.Produkt[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] / 1000
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% c("3F18F771-5E04-4E7A-9965-FDABB2", "245365B3-9699-4064-B031-B7A789")] / 1000

o$Menge.Produkt[o$Massnahmedetail.ID %in% M2013.10] <- o$Menge.Produkt[o$Massnahmedetail.ID %in% M2013.10] / 10
o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2013.10] <- o$Menge.Produkt.ha[o$Massnahmedetail.ID %in% M2013.10] / 10

dat_m[[5]] <- o

# b) Find outliers with rules on distribution

# Look at all top ten entries  for each combination of Produktbezeichnung und Menge.Produkt
# Fertilizer has already been cleaned: only look at those entries where Spezial = yes and not a fertilization measure

# rel_meas<-c("Pressen (Gross-, Rundballen)","Pressen (Kleinballen)","Ballen presen/ernten")
#
#
# d_borders<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]])%>% # some summary stats
#   filter(Massnahme %in% rel_meas)%>%
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   summarize(mean=mean(Menge.Produkt.ha,na.rm=T),sd=sd(Menge.Produkt.ha,na.rm=T),n.y=n(),min=min(Menge.Produkt.ha,na.rm=T),max=max(Menge.Produkt.ha,na.rm=T))%>%
#   filter(!(is.na(sd)))
#
# d_ranks<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]])%>% # the ranking of values
#   filter(Massnahme %in% rel_meas)%>%
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   arrange(Produktbezeichnung,Einheit.Produkt, -Menge.Produkt.ha) %>%
#   mutate(rank = rank(-Menge.Produkt.ha))
#
#
# d_outs<-merge(d_ranks,d_borders,by=c("Produktbezeichnung","Einheit.Produkt") )%>%  #connect and filter
#   filter(n.y>10 & rank<=15 & (Menge.Produkt.ha>(2*mean)) | n.y %in% (3:9) & rank<=5 & (Menge.Produkt.ha>(2*mean)))%>%
#   select(n.y,Produktbezeichnung,Einheit.Produkt,rank,Menge.Produkt.ha,mean,Massnahmedetail.ID,Jahr,AUI.ID,Kultur)%>%
#   arrange(desc(n.y),rank,Produktbezeichnung,Einheit.Produkt,Menge.Produkt.ha)
#
# # Only WW in 2013
#
# rel_meas<-c("Pressen (Gross-, Rundballen)","Pressen (Kleinballen)","Ballen presen/ernten")
#
#
# d_borders<-dat_m[[5]]%>%
#   filter(Massnahme %in% rel_meas & Kultur == "Winterweizen")%>%
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   summarize(mean=mean(Menge.Produkt.ha,na.rm=T),sd=sd(Menge.Produkt.ha,na.rm=T),n.y=n(),min=min(Menge.Produkt.ha,na.rm=T),max=max(Menge.Produkt.ha,na.rm=T))%>%
#   filter(!(is.na(sd)))
#
# d_ranks<-dat_m[[5]]%>% # the ranking of values
#   filter(Massnahme %in% rel_meas & Kultur == "Winterweizen")%>%
#   group_by(Produktbezeichnung,Einheit.Produkt)%>%
#   arrange(Produktbezeichnung,Einheit.Produkt, -Menge.Produkt.ha) %>%
#   mutate(rank = rank(-Menge.Produkt.ha))
#
#
# d_outs<-merge(d_ranks,d_borders,by=c("Produktbezeichnung","Einheit.Produkt") )%>%  #connect and filter
#   filter(n.y>10 & rank<=15 & (Menge.Produkt.ha>(2*mean)) | n.y %in% (3:9) & rank<=5 & (Menge.Produkt.ha>(2*mean)))%>%
#   select(n.y,Produktbezeichnung,Einheit.Produkt,rank,Menge.Produkt.ha,mean,Massnahmedetail.ID,Jahr,AUI.ID,Kultur)%>%
#   arrange(desc(n.y),rank,Produktbezeichnung,Einheit.Produkt,Menge.Produkt.ha)



# c) Special rule for potatoes: Harvesting is often indicated double on the same day if two different kind of potatoes, although the same culture is indicated. If both entries are for the whole surface this is misleading --> delete those entries

for (i in 1:10) {
  o <- dat_m[[i]]

  o$Produktbezeichnung[o$Kultur %in% c("Industriekartoffeln", "Futterkartoffeln", "Speisekartoffeln") & o$Massnahmentypnr. == 6 & o$Produktbezeichnung %in% c("Industriekartoffeln", "Futterkartoffeln", "Speisekartoffeln")] <- "Kartoffeln"

  o2 <- o %>%
    filter(o$Kultur %in% c("Industriekartoffeln", "Futterkartoffeln", "Speisekartoffeln") & Massnahmentypnr. == 6 & Produktbezeichnung == "Kartoffeln") %>%
    distinct(AUI.ID, Schlag.ID, Kultur, Datum, Massnahme, Massnahmeflaeche, Produktbezeichnung, Menge.Produkt.ha, .keep_all = T) # clean double entries (very general)

  o3 <- o[!(o$Kultur %in% c("Industriekartoffeln", "Futterkartoffeln", "Speisekartoffeln") & o$Massnahmentypnr. == 6 & o$Produktbezeichnung == "Kartoffeln"), ] # and the rest of the data

  o4 <- rbind(o2, o3) # and back together

  dat_m[[i]] <- o4
}


## 4. Convert units

# There are four possible cases
# 1. There is no information --> kick measure out
# 2. There is no commentary and costs are blank --> correct in list: Gregor ?
#   3. Spezial = 1: The transformation unit is not ha but something else: case based transformation
# 4. Spezial = 0 and costs are non-empty : standard case - multiply measure with surface (Massnahmenflaeche)

# d<-rbind(dat_m[[1]],dat_m[[2]],dat_m[[3]],dat_m[[4]],dat_m[[5]],dat_m[[6]],dat_m[[7]],dat_m[[8]],dat_m[[9]],dat_m[[10]])
# table(is.na(d$Massnahmeflaeche)) table(d$Massnahmeflaeche=="") table(is.na(d$Spezial.ja.nein))
# utils::View(d[is.na(d$Massnahmeflaeche),]) View(d[is.na(d$Spezial.ja.nein),])
# table(d$Bemerkungen[d$Spezial.ja.nein==1])

# a)

dat_conv <- list() # will contain converted units

for (i in 1:length(years)) {
  o <- dat_m[[i]] # the merged data

  o <- o[!(is.na(o$Massnahmeflaeche)), ]

  # case 1: no information - kick measures out

  o2 <- o[!(o$Bemerkungen %in% c("Keine Infos", "keine Info")), ]

  # case 2:

  o3 <- o2[o2$workmach_cost != "", ]
  o3 <- o3[!(is.na(o3$workmach_cost)), ]
  o3 <- o3[!(is.na(o3$Spezial.ja.nein)), ]

  # case 3:

  o3$workmach[o3$Spezial.ja.nein == 1 & o3$Bemerkungen %in% c("pro dt", "pro m3", "pro t", "pro dt")] <-
    o3$workmach_cost[o3$Spezial.ja.nein == 1 & o3$Bemerkungen %in% c("pro dt", "pro m3", "pro t", "pro dt")] * o3$Menge.Produkt[o3$Spezial.ja.nein == 1 & o3$Bemerkungen %in% c("pro dt", "pro m3", "pro t", "pro dt")]

  # case 4:

  o3$workmach[o3$Spezial.ja.nein == 0] <-
    o3$workmach_cost[o3$Spezial.ja.nein == 0] * as.numeric(o3$Massnahmeflaeche[o3$Spezial.ja.nein == 0])


  dat_conv[[i]] <- o3 # save the converted dataset
}

# b) Kick out those harvest activities where you have more than one per year by taking the mean

check <- list()

for (i in 1:length(years)) {
  p <- dat_conv[[i]]

  p$h_check <- ifelse(p$Massnahmentypnr. %in% c(5, 6) & p$Kultur == "Winterweizen", 1, 0)
  p <- p %>%
    filter(Kultur == "Winterweizen") %>%
    group_by(Schlag.ID, Jahr) %>%
    summarize(n_harvests = sum(h_check, na.rm = T))
  check[[i]] <- p

  o <- dat_conv[[i]]

  o2 <- o %>%
    filter(Massnahmentypnr. %in% c(5, 6)) %>%
    group_by(Schlag.ID, Jahr) %>%
    summarize(workmach_harvest = mean(workmach, na.rm = T), n_harvest = n()) # the mean of harvest activities

  o3 <- o %>%
    filter(!(Massnahmentypnr. %in% c(5, 6))) # kick harvesting out

  o4 <- merge(o3, o2, by = c("Schlag.ID", "Jahr"), all.x = T) # after aggregation it has to be added again

  o4$n_harvest[is.na(o4$n_harvest)] <- 0 # replace Nas (no harvest entry) by zeros

  dat_conv[[i]] <- o4
}

# d<-rbind(check[[1]],check[[2]],check[[3]],check[[4]],check[[5]])
# d<-rbind(dat_conv[[1]],dat_conv[[2]],dat_conv[[3]],dat_conv[[4]],dat_conv[[5]])

## 5. Indicate seperately how high workmach for mechanic pest control was respectively

# Mechanic pest control measures are indicated in the following lists

herbnr <- read.csv2("Herb.csv", header = T)
fungnr <- read.csv2("Fung.csv", header = T)
insektnr <- read.csv2("Insekt.csv", header = T)

m_pest <- rbind(herbnr, fungnr, insektnr) # a list with all mechanic pest control measures

# dv<-rbind(dat_conv[[1]],dat_conv[[2]],dat_conv[[3]],dat_conv[[4]],dat_conv[[5]],dat_conv[[6]],dat_conv[[7]],dat_conv[[8]],dat_conv[[9]],dat_conv[[10]])
# table(m_pest$Massnahme %in% dv$Massnahme) # control if measures are found in data: which not? m_pest[!(m_pest$Massnahme %in% dv$Massnahme),]

for (i in 1:10) {
  o <- dat_conv[[i]] %>%
    mutate(workmach_mech_pest_ind = ifelse(Massnahme %in% m_pest$Massnahme, 1, 0))

  o$workmach_mech_pest <- o$workmach_mech_pest_ind * o$workmach

  dat_conv[[i]] <- o
}

# dv<-rbind(dat_conv[[1]],dat_conv[[2]],dat_conv[[3]],dat_conv[[4]],dat_conv[[5]],dat_conv[[6]],dat_conv[[7]],dat_conv[[8]],dat_conv[[9]],dat_conv[[10]])
# table(dv$workmach_mech_pest_ind) table(dv$workmach_mech_pest)

## 6. Aggregate workmach per plot + culture

dat_ag <- list()
dat_ag_kult_farm <- list()

for (i in 1:10) {
  o <- dat_conv[[i]]
  p <- dat_corr[[i]]

  o$Schlagflaeche <- as.numeric(o$Schlagflaeche)
  o$workmach <- as.numeric(o$workmach)

  p$Schlagflaeche <- as.numeric(p$Schlagflaeche)

  o2 <- o %>% # aggregate workmach on a plot+culture level per year
    group_by(AUI.ID, Kultur, Schlag.ID) %>%
    summarize(workmach_total = sum(workmach, na.rm = T), flaeche = mean(Schlagflaeche, na.rm = T), mech_pest_total = sum(workmach_mech_pest, na.rm = T), workmach_harvest = mean(workmach_harvest, na.rm = T), n_harvest = mean(n_harvest, na.rm = T))
  o2$workmach_total[o2$n_harvest > 0] <- o2$workmach_total[o2$n_harvest > 0] + o2$workmach_harvest[o2$n_harvest > 0]

  o2 <- o2 %>%
    mutate(workmach_total_ha = workmach_total / flaeche, mech_pest_total_ha = mech_pest_total / flaeche)

  o3 <- o %>%
    group_by(AUI.ID, Kultur) %>%
    summarize(workmach_tot_kult = sum(workmach))

  o4 <- p %>%
    select(AUI.ID, Kultur, Schlag.ID, Schlagflaeche) %>%
    distinct(AUI.ID, Kultur, Schlag.ID, .keep_all = T) %>%
    group_by(AUI.ID, Kultur) %>%
    summarize(surface = sum(Schlagflaeche))

  o5 <- merge(o3, o4, by = c("AUI.ID", "Kultur")) %>%
    group_by(AUI.ID, Kultur) %>%
    summarize(workmach_ha_kult = workmach_tot_kult / surface, surface = surface, Jahr = years[[i]])

  dat_ag[[i]] <- o2
  dat_ag_kult_farm[[i]] <- o5
}

# d2<-rbind(dat_ag_kult_farm[[1]],dat_ag_kult_farm[[2]],dat_ag_kult_farm[[3]],dat_ag_kult_farm[[4]],dat_ag_kult_farm[[5]],dat_ag_kult_farm[[6]],dat_ag_kult_farm[[7]],dat_ag_kult_farm[[8]],dat_ag_kult_farm[[9]],dat_ag_kult_farm[[10]])
# View(d2)

d <- rbind(dat_ag[[1]], dat_ag[[2]], dat_ag[[3]], dat_ag[[4]], dat_ag[[5]], dat_ag[[6]], dat_ag[[7]], dat_ag[[8]], dat_ag[[9]], dat_ag[[10]])
dat_ag_kult_all <- d %>%
  group_by(Kultur) %>%
  summarize(workmach_tot_kult = sum(workmach_total), Surface = sum(flaeche)) %>%
  mutate(workmach_ha_kult = workmach_tot_kult / Surface)

# View(dat_ag_kult_all)

## 7. check for outliers

# Per plot and culture

# Look at all top ten entries for each culture

for (i in 1:10) { # add years

  o <- dat_ag[[i]]

  o$Jahr <- rep(years[[i]], times = dim(o)[1])

  dat_ag[[i]] <- o
}

# d_borders<-rbind(dat_ag[[1]],dat_ag[[2]],dat_ag[[3]],dat_ag[[4]],dat_ag[[5]])%>% # some summary stats
#   group_by(Kultur)%>%
#   summarize(mean=mean(workmach_total_ha,na.rm=T),sd=sd(workmach_total_ha,na.rm=T),n.y=n(),min=min(workmach_total_ha,na.rm=T),max=max(workmach_total_ha,na.rm=T))%>%
#   filter(!(is.na(sd)))
#
# d_ranks<-rbind(dat_ag[[1]],dat_ag[[2]],dat_ag[[3]],dat_ag[[4]],dat_ag[[5]])%>% # the ranking of values
#   group_by(Kultur)%>%
#   arrange(Kultur, -workmach_total_ha) %>%
#   mutate(rank = rank(-workmach_total_ha))
#
#
# d_outs<-merge(d_ranks,d_borders,by=c("Kultur") )%>%  #connect and filter
#   filter(n.y>10 & rank<=15 & (workmach_total_ha>(2*mean)) | n.y %in% (3:9) & rank<=5 & (workmach_total_ha>(2*mean)))%>%
#   select(n.y,Kultur,rank,workmach_total_ha,mean,Schlag.ID,Jahr,AUI.ID)%>%
#   arrange(desc(n.y),rank,Kultur,workmach_total_ha)

# Only WW in 2013
# d_borders<- dat_ag[[5]]%>%
#   filter(Kultur=="Winterweizen")%>%
#   group_by(Kultur)%>%
#   summarize(mean=mean(workmach_total_ha,na.rm=T),sd=sd(workmach_total_ha,na.rm=T),n.y=n(),min=min(workmach_total_ha,na.rm=T),max=max(workmach_total_ha,na.rm=T))%>%
#   filter(!(is.na(sd)))
#
# d_ranks<- dat_ag[[5]]%>%
#   filter(Kultur=="Winterweizen")%>%
#   group_by(Kultur)%>%
#   arrange(Kultur, -workmach_total_ha) %>%
#   mutate(rank = rank(-workmach_total_ha))
#
#
# d_outs<-merge(d_ranks,d_borders,by=c("Kultur") )%>%  #connect and filter
#   filter(n.y>10 & rank<=15 & (workmach_total_ha>(2*mean)) | n.y %in% (3:9) & rank<=5 & (workmach_total_ha>(2*mean)))%>%
#   select(n.y,Kultur,rank,workmach_total_ha,mean,Schlag.ID,Jahr,AUI.ID)%>%
#   arrange(desc(n.y),rank,Kultur,workmach_total_ha)


#
# #And from below ( outliers smaller than 25% of mean)
#
# d_ranks_low<-rbind(dat_ag[[1]],dat_ag[[2]],dat_ag[[3]],dat_ag[[4]],dat_ag[[5]])%>% # the ranking of values
#   group_by(Kultur)%>%
#   arrange(Kultur, workmach_total_ha) %>%
#   mutate(rank = rank(workmach_total_ha))
#
#
# d_outs_low<-merge(d_ranks_low,d_borders,by=c("Kultur") )%>%  #connect and filter
#   filter(n.y>10 & rank<=15 & (workmach_total_ha<(0.25*mean)) | n.y %in% (3:9) & rank<=5 & (workmach_total_ha<(0.25*mean)))%>%
#   select(n.y,Kultur,rank,workmach_total_ha,mean,Schlag.ID,Jahr,AUI.ID)%>%
#   arrange(desc(n.y),rank,Kultur,workmach_total_ha)


# # Only WW in 2013
# d_ranks_low<- dat_ag[[5]]%>%
#   filter(Kultur=="Winterweizen")%>%# the ranking of values
#   group_by(Kultur)%>%
#   arrange(Kultur, workmach_total_ha) %>%
#   mutate(rank = rank(workmach_total_ha))
#
#
# d_outs_low<-merge(d_ranks_low,d_borders,by=c("Kultur") )%>%  #connect and filter
#   filter(n.y>10 & rank<=15 & (workmach_total_ha<(0.25*mean)) | n.y %in% (3:9) & rank<=5 & (workmach_total_ha<(0.25*mean)))%>%
#   select(n.y,Kultur,rank,workmach_total_ha,mean,Schlag.ID,Jahr,AUI.ID)%>%
#   arrange(desc(n.y),rank,Kultur,workmach_total_ha)
#
#
# #
# l<-dat_conv[[5]]
# View(l[l$Schlag.ID=="026AD18C-7C2A-4304-994D-DB12C5",])
# #
# l<-dat_corr[[5]]
# View(l[l$Schlag.ID=="E96A7521-CBD5-4E62-8786-44BB75",])

## 8. Save outcomes

# All dat_ag in one dataframe and then save

dat_ag <- rbind(dat_ag[[1]], dat_ag[[2]], dat_ag[[3]], dat_ag[[4]], dat_ag[[5]], dat_ag[[6]], dat_ag[[7]], dat_ag[[8]], dat_ag[[9]], dat_ag[[10]])


write.csv2(dat_ag, "II-dat_ag_workmach_total.csv", row.names = F)

# table(dat_ag$workmach_total -dat_ag$mech_pest_total)
