################################################################################
# Pesticides load indices computation from ZA-AUI data                         #
# date: 18/10/2021                                                             # 
# generated file: IV-dat_wheat_pesti.csv                                       #
# Hervé D., Philippe M, Niklas M.                                              #
################################################################################

setwd("Y:/Papers and Data/Datasets/ZA AUI Data/data_transformation_HD_PM_NM")

library(tidyverse)

# data <- readxl::read_xlsx("D:/N5QOJBL/ONGOING_WORK/Pesticides_Spillovers/Daten_ZA_09-18/Daten_pesti_all/Pesticide_cleaned_nobook_noagg_ind_definiert_09_18.xlsx")

data <- readr::read_csv2("Pesticide_cleaned_nobook_noagg_ind_definiert_09_18.csv")

## Different production ----
unique(data$Kultur)

## retain wheat and potatoes for analysis ----

# data2 <- data %>% filter(Kultur %in% c("Winterweizen", "Industriekartoffeln",
#                                        "Speisekartoffeln", "Saatkartoffeln", "Fr\u0081hkartoffeln", "Futterkartoffeln"))
#
## Grouping variables at the farm level ----

# AI (Active Ingredients in kg)

dat_AI_wheat <- data %>%
  filter(Kultur == "Winterweizen") %>%
  select(
    Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID,
    AIA, HerbAI, FungAI, InsAI
  ) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, AIA, HerbAI, FungAI, InsAI) %>%
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(
    AIplot = sum(AIA, na.rm = T),
    HerbAIplot = sum(HerbAI, na.rm = T),
    FungAIplot = sum(FungAI, na.rm = T),
    InsAIplot = sum(InsAI, na.rm = T),
    Schlagflaeche = mean(Schlagflaeche, na.rm = T)
  ) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(
    AI = sum(AIplot, na.rm = T),
    HerbAI = sum(HerbAIplot, na.rm = T),
    FungAI = sum(FungAIplot, na.rm = T),
    InsAI = sum(InsAIplot, na.rm = T),
    surface = sum(Schlagflaeche, na.rm = T)
  ) %>%
  select(Jahr, AUI.ID, AI, HerbAI, FungAI, InsAI, surface)

# PQ (Volume of pesticides kg)

dat_PQ_wheat <- data %>%
  filter(Kultur == "Winterweizen") %>%
  select(
    Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID,
    PQ, HerbPQ, FungPQ, InsPQ
  ) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, PQ, HerbPQ, FungPQ, InsPQ) %>%
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(
    PQplot = sum(PQ, na.rm = T),
    HerbPQplot = sum(HerbPQ, na.rm = T),
    FungPQplot = sum(FungPQ, na.rm = T),
    InsPQplot = sum(InsPQ, na.rm = T),
    Schlagflaeche = mean(Schlagflaeche, na.rm = T)
  ) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(
    PQ = sum(PQplot, na.rm = T),
    HerbPQ = sum(HerbPQplot, na.rm = T),
    FungPQ = sum(FungPQplot, na.rm = T),
    InsPQ = sum(InsPQplot, na.rm = T)
  ) %>%
  select(Jahr, AUI.ID, PQ, HerbPQ, FungPQ, InsPQ)

# TFI (Treatment Frequency Index)
# TFI is a measure that is already expressed in per ha units ---> to weight it a) multiply by plotsize, b) sum all measures per farm, c) divide by total acreage

dat_TFI_wheat <- data %>%
  filter(Kultur == "Winterweizen") %>%
  select(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, STI, HerbSTI, FungSTI, InsSTI) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, STI, HerbSTI, FungSTI, InsSTI) %>%
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(
    STI = sum(STI, na.rm = T),
    HerbSTI = sum(HerbSTI, na.rm = T),
    FungSTI = sum(FungSTI, na.rm = T),
    InsSTI = sum(InsSTI, na.rm = T),
    Schlagflaeche = mean(Schlagflaeche, na.rm = T)
  ) %>%
  mutate(
    TFItimesha = STI * Schlagflaeche, HerbTFItimesha = HerbSTI * Schlagflaeche, # TFI is unitless so we can either use it as it is or use a weighted average
    FungTFItimesha = FungSTI * Schlagflaeche, InsTFItimesha = InsSTI * Schlagflaeche
  ) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(
    TFI = sum(TFItimesha, na.rm = T) / sum(Schlagflaeche),
    HerbTFI = sum(HerbTFItimesha, na.rm = T) / sum(Schlagflaeche),
    FungTFI = sum(FungTFItimesha, na.rm = T) / sum(Schlagflaeche),
    InsTFI = sum(InsTFItimesha, na.rm = T) / sum(Schlagflaeche)
  ) %>%
  select(Jahr, AUI.ID, TFI, HerbTFI, FungTFI, InsTFI)

# LI (Load Index)
# LI is a measure that is already expressed in per ha units ---> to weight it a) multiply by plotsize, b) sum all measures per farm, c) divide by total acreage

dat_LI_wheat <- data %>%
  filter(Kultur == "Winterweizen") %>%
  select(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, LI, HerbLI, FungLI, InsLI) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, LI, HerbLI, FungLI, InsLI) %>%
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(
    LIplot = sum(LI, na.rm = T),
    HerbLIplot = sum(HerbLI, na.rm = T),
    FungLIplot = sum(FungLI, na.rm = T),
    InsLIplot = sum(InsLI, na.rm = T),
    Schlagflaeche = mean(Schlagflaeche, na.rm = T)
  ) %>%
  mutate(
    LItimesha = LIplot * Schlagflaeche, HerbLItimesha = HerbLIplot * Schlagflaeche,
    FungLItimesha = FungLIplot * Schlagflaeche, InsLItimesha = InsLIplot * Schlagflaeche
  ) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(
    LI = sum(LItimesha, na.rm = T), # area load
    HerbLI = sum(HerbLItimesha, na.rm = T),
    FungLI = sum(FungLItimesha, na.rm = T),
    InsLI = sum(InsLItimesha, na.rm = T)
  ) %>%
  select(Jahr, AUI.ID, LI, HerbLI, FungLI, InsLI)

# To get the partial Load Indicators (Toxicity Load Index, Fate Load Index and Health Load Index) compute them as follows and then follow the same computation procedure as for the Load Index (LI) above:

# Toxicity Load Index = TLI, Fate Load Index = FLI, Health Load Index = HLI,

dat_tox_wheat <- data %>%
  filter(Kultur == "Winterweizen") %>%
  mutate(TLI = TL * STI, FLI = FL * STI, HLI = HL * STI) %>%
  select(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, TLI, FLI, HLI) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, TLI, FLI, HLI) %>%
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(
    TLIplot = sum(TLI, na.rm = T),
    FLIplot = sum(FLI, na.rm = T),
    HLIplot = sum(HLI, na.rm = T),
    Schlagflaeche = mean(Schlagflaeche, na.rm = T)
  ) %>%
  mutate(
    TLIha = TLIplot * Schlagflaeche, FLIha = FLIplot * Schlagflaeche,
    HLIha = HLIplot * Schlagflaeche
  ) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(
    TLI = sum(TLIha, na.rm = T),
    FLI = sum(FLIha, na.rm = T),
    HLI = sum(HLIha, na.rm = T)
  ) %>%
  select(Jahr, AUI.ID, TLI, FLI, HLI)

### some other data

data_other_wheat <- data %>%
  filter(Kultur == "Winterweizen") %>%
  select(
    Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, Hoehe.ueber.Meer,
    Kanton, Zone.Nr
  ) %>%
  distinct(
    Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Massnahmedetail.ID, Hoehe.ueber.Meer,
    Kanton, Zone.Nr
  ) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(
    Altitude = mean(Hoehe.ueber.Meer),
    Kanton = unique(Kanton), ZoneAgri = unique(Zone.Nr)
  ) %>%
  select(Jahr, AUI.ID, Altitude, Kanton, ZoneAgri)

## Merge data into one

dat_wheat <- left_join(dat_AI_wheat, dat_LI_wheat) %>%
  left_join(., dat_PQ_wheat) %>%
  left_join(., dat_TFI_wheat) %>%
  left_join(., dat_tox_wheat) %>%
  left_join(., data_other_wheat)

# save(dat_wheat, file = "dat_wheat_pesti.RData")

write.csv(dat_wheat, "IV-dat_wheat_pesti.csv")
