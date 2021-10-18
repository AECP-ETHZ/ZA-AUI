################################################################################
# Merging all data: values at the farm level                                   #
# date: 18/10/2021                                                             # 
# generated file: V-pesti_spill_dat.csv                                        #
# Hervé D., Philippe M, Niklas M.                                              #
################################################################################

setwd("Y:/Papers and Data/Datasets/ZA AUI Data/data_transformation_HD_PM_NM")

library(tidyverse)

dat_wheat_pesti <- read.csv("IV-dat_wheat_pesti.csv")

Fert_N_per_plot <- read.csv2("I-Fert_N_per_plot.csv")

ag_workmach_total <- read.csv2("II-dat_ag_workmach_total.csv")

dat_yield <- read.csv2("III-WW_Revenue.csv")

## N 

N_wheat <- Fert_N_per_plot %>% 
  filter(Kultur == "Winterweizen") %>%
  select(Jahr, AUI.ID, Schlag.ID, flaeche, N_ha_total) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, flaeche, N_ha_total) %>% 
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(Nplot_ha = sum(N_ha_total * flaeche, na.rm = T)/
              sum(flaeche, na.rm = T), 
            flaeche = mean(flaeche, na.rm = T)) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(Nha = sum(Nplot_ha * flaeche, na.rm = T)/
              sum(flaeche, na.rm = T),
            surface = sum(flaeche,na.rm = T)) %>%
  select(Jahr, AUI.ID, Nha, surface)

## potential outliers

plot(N_wheat$Nha)
plot(N_wheat$surface)

N_wheat <- N_wheat %>% 
  filter(surface < 30, Nha < 400)

## work and machinery

work_wheat <- ag_workmach_total %>% 
  filter(Kultur == "Winterweizen") %>%
  select(Jahr, AUI.ID, Schlag.ID, flaeche, workmach_total, mech_pest_total) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, flaeche, workmach_total, mech_pest_total) %>% 
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(workplot = sum(workmach_total, na.rm = T), 
            mechworkplot = sum(mech_pest_total, na.rm = T),
            flaeche = mean(flaeche, na.rm = T)) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(work_tot = sum(workplot, na.rm = T),
            mechwork_tot = sum(mechworkplot, na.rm = T),
            surface = sum(flaeche,na.rm = T), 
            WKha = work_tot/surface, 
            MWKha = mechwork_tot/surface) %>%
  select(Jahr, AUI.ID, work_tot, WKha, mechwork_tot, MWKha, surface)

## potential outliers

plot(work_wheat$WKha)
plot(work_wheat$surface)

work_wheat <- work_wheat %>% 
  filter(WKha < 3000, surface < 30)

## yield

dat_yield2 <- dat_yield %>%
  select(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Kulturertrag, Rev) %>%
  distinct(Jahr, AUI.ID, Schlag.ID, Schlagflaeche, Kulturertrag, Rev) %>%
  mutate(totprod = Schlagflaeche * Kulturertrag, 
         revtot = Schlagflaeche * Rev) %>%
  group_by(Jahr, AUI.ID, Schlag.ID) %>%
  summarize(totprodplot = sum(totprod, na.rm = T), 
            revtotplot = sum(revtot, na.rm = T),
            Schlagflaeche = mean(Schlagflaeche, na.rm = T)) %>%
  group_by(Jahr, AUI.ID) %>%
  summarize(wheat_tot = sum(totprodplot, na.rm = T),
            rev_tot = sum(revtotplot, na.rm = T),
            surface = sum(Schlagflaeche,na.rm = T), 
            Yield = wheat_tot/surface/10, 
            Revha = rev_tot/surface) %>%
  select(Jahr, AUI.ID, wheat_tot, rev_tot, Yield, Revha, surface)

summary(dat_yield2)

plot(dat_yield2$Yield)
plot(dat_yield2$Revha)
plot(dat_yield2$surface)

## pesticides

dat_wheat_pesti <- dat_wheat_pesti %>% 
  mutate(AI_ha = AI/surface, 
         HerbAI_ha = HerbAI/surface, 
         FungAI_ha = FungAI/surface, 
         InsAI_ha = InsAI/surface, 
         LI_ha = LI/surface, 
         HerbLI_ha = HerbLI/surface, 
         FungLI_ha = FungLI/surface, 
         InsLI_ha = InsLI/surface,
         TLI_ha = TLI/surface,
         FLI_ha = FLI/surface,
         HLI_ha = HLI/surface,
         PQ_ha = PQ/surface, 
         HerbPQ_ha = HerbPQ/surface, 
         FungPQ_ha = FungPQ/surface, 
         InsPQ_ha = InsPQ/surface)

## potential outliers

plot(dat_wheat_pesti$LI_ha)
plot(dat_wheat_pesti$AI_ha)
plot(dat_wheat_pesti$PQ_ha)
plot(dat_wheat_pesti$TFI)
plot(dat_wheat_pesti$TLI_ha)
plot(dat_wheat_pesti$FLI_ha)
plot(dat_wheat_pesti$HLI_ha)


dat_wheat_pesti <- dat_wheat_pesti %>% 
  filter(AI_ha < 7, LI_ha < 20)

# ## create id for yield data set
# 
# ID <- read.csv2("D:/N5QOJBL/ONGOING_WORK/Pesticides_Spillovers/Daten_ZA_09-18/Daten_ZA-AUI/Zuweisung_BetrNr_AUI_ID.csv", header = T, stringsAsFactors = F)
# 
# IDConverter <- cbind.data.frame(ID$AUI.ID, ID$AUI.Betriebsnummer)
# colnames(IDConverter) <- c("AUI.ID", "AUI.Betriebsnummer")
# IDConverter <- with(IDConverter, setNames(AUI.ID, AUI.Betriebsnummer))
# 
# dyield <- merge(dat_yield, ID, by.x = "BETRIEB", by.y = "AUI.Betriebsnummer")
# dyield <- dyield[order(dyield$JAHR), ]

## merge all data to one

# names(dat_wheat)[1] <- "Jahr"
# dyield <- dyield %>% select(JAHR, AUI.ID, weiz_Anbauflaeche, weiz_Natertrag)
# names(dyield)[1:2] <- c("Jahr", "AUI.ID")

pesti_spill_dat <- left_join(dat_wheat_pesti, N_wheat[, -4], by = c("Jahr", "AUI.ID")) %>%
  left_join(., work_wheat[, -7], by = c("Jahr", "AUI.ID")) %>%
  left_join(., dat_yield2[, -7], by = c("Jahr", "AUI.ID"))

# pesti_spill_dat <- pesti_spill_dat %>%
#   mutate(N_total = Nha * surface, wheatProd = wheat_tot/10)

pesti_spill_dat <- pesti_spill_dat %>%
  filter(!is.na(Yield), !is.na(WKha ), !is.na(Nha))

summary(pesti_spill_dat)
table(pesti_spill_dat$Jahr)

write.csv2(pesti_spill_dat, "V-pesti_spill_dat.csv")
