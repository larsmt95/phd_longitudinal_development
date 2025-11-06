library(readxl)
library(writexl)
library(tidyverse)
library(lme4)
library(boot.pval)
library(lmerTest)
library(openxlsx)
library(dplyr)
library(tidyr)
library(emmeans)

paper3_alle <- read_excel("~/Library/CloudStorage/OneDrive-nih.no/PhD/Artikler/Data/all_results.xlsx", sheet = "paper3")
paper3_alle <- read_excel("C:/Users/larsmt/OneDrive - nih.no/PhD/Artikler/Data/all_results.xlsx", sheet = "paper3")
paper3 <- read_excel("C:/Users/larsmt/OneDrive - nih.no/PhD/Artikler/Data/all_results.xlsx", sheet = "paper3")


wtf <- read_excel("~/Library/CloudStorage/OneDrive-nih.no/PhD/Artikler/Data/all_results.xlsx", sheet = "paper3")

paper3_korr2  %>% 
  select(id, FP2, group, time, sprint30, `30m1`, `30m2`, `30m3`, bodymass, relforce) %>% 
  filter(id %in% c(643)) %>% 
  arrange(id)

# korrigering av cod, sprint og cmj for UIA -> runde2 hC%ndball & runde3 fotball
paper3_korr2 <- paper3_alle %>% 
  mutate(
    sprint10 = ifelse(FP2 == "uia" & sport == "handball" & time == "round2", sprint10 + 0.21, sprint10), # definere hvem som skal korrigeres
    sprint30 = ifelse(FP2 == "uia" & sport == "handball" & time == "round2", sprint30 + 0.21, sprint30),
    cod = ifelse(FP2 == "uia" & sport == "handball" & time == "round2", cod + 0.68, cod),
    cmj = ifelse(FP2 == "uia" & sport == "handball" & time == "round2", cmj + 2.96, cmj),
    sprint10 = ifelse(FP2 == "uia" & sport == "handball" & time == "round3", sprint10 + 0.21, sprint10), # definere hvem som skal korrigeres
    sprint30 = ifelse(FP2 == "uia" & sport == "handball" & time == "round3", sprint30 + 0.21, sprint30),
    cod = ifelse(FP2 == "uia" & sport == "handball" & time == "round3", cod + 0.68, cod),
    cmj = ifelse(FP2 == "uia" & sport == "handball" & time == "round3", cmj + 2.96, cmj),
    sprint10 = ifelse(FP2 == "uia" & sport == "football" & time == "round3", sprint10 + 0.21, sprint10), # definere hvem som skal korrigeres
    sprint30 = ifelse(FP2 == "uia" & sport == "football" & time == "round3", sprint30 + 0.21, sprint30),
    cod = ifelse(FP2 == "uia" & sport == "football" & time == "round3", cod + 0.68, cod),
    cmj = ifelse(FP2 == "uia" & sport == "football" & time == "round3", cmj + 2.96, cmj)
  )

paper3_selected <- paper3_korr2 %>% 
  select(id, FP2, time, sex, group, sport, ageg, age, height, leglength, sittingheight, bodymass, phv, sprint10, sprint30, cmj, relabd, reladd, 
         cod, yyir, totforce, relforce, relpwr, totpwr, meanhams, meanadd, meanabd, relhams, ir, erot, relir, reler) %>%
  filter(group != "boysU18") %>% 
  mutate(
    time = factor(time, levels = c("round1", "round2", "round3")),
    sex = factor(sex, levels = c("boys", "girls")),
    group = factor(group, levels = c("boysU14", "boysU16", "girlsU14", "girlsU16")),
    APHV = age - phv) %>%  # Beregner alder ved PHV
  print()

# inkluderer alle med min. 2 testidspunkt
paper3_longer_vol2 <- paper3_selected %>% 
  select(id, FP2, time, sex, group, sport, ageg, APHV, age, height, bodymass, phv, sprint10, sprint30, cmj, relabd, reladd, 
         cod, yyir, totforce, relforce, relpwr, totpwr, meanhams, meanadd, meanabd, relhams) %>% 
  pivot_longer(
    cols = !(id:APHV),
    names_to = c("test")) %>% 
  filter(!is.na(value)) %>% 
  distinct(id, time, test, value, .keep_all = TRUE) %>%   ## fjerner  duplikat rader
  group_by(id, test) %>%
  filter(n_distinct(time) >= 2) %>%
  ungroup()

# Lagre ferdig dataframe
saveRDS(paper3_longer_vol2, here("paper_3/data/processed/paper3_longer_vol2.rds"))
