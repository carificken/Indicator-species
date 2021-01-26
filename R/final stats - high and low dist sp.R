# this script runs the stats  for low and high dist sp in separate .R files, combines them, and outputs full results

library(tidyverse)
library(lme4)
library(lmerTest)
library(vegan)
library(broom.mixed)

rm(list=ls())

# 1. Stats for high dist IS
source("R/stats - high dist sp.R")
high_out

# 2. Stats for low dist IS
source("R/stats - low dist sp.R")
low_out

# clean up environment
rm(list=setdiff(ls(), c("high_out", "low_out")))

# combine output
all_out <- bind_rows(high_out, low_out) 
head(all_out)

# write.csv(all_out, file="results/Model output - all models.csv", row.names = F)

# how many sig tests
all_out %>% 
  filter(effect=="fixed" & term!="(Intercept)") %>% 
  group_by(IS_Type) %>% 
  filter(`Pr(>F)` <= 0.050) %>% 
  tally() # num of sig tests out of 66

# are full or top5 models better?
all_out %>% 
  filter(term=="HighDistSp_N") %>% 
  select(WetlandType, response_var, IS_Type, AIC) %>% 
  spread(., key=IS_Type, value=AIC) %>% 
  mutate(BestModel = ifelse(`Full - High` < `Top 5 - High`, "Full", "Top 5")) %>% 
  group_by(BestModel) %>% 
  tally()

all_out %>% 
  filter(term=="LowDistSp_N") %>% 
  select(WetlandType, response_var, IS_Type, AIC) %>% 
  spread(., key=IS_Type, value=AIC) %>% 
  mutate(BestModel = ifelse(`Full - Low` < `Top 5 - Low`, "Full", "Top 5")) %>% 
  group_by(BestModel) %>% 
  tally()

# clean output for presentation
# high dist IS
high_out_clean <- high_out %>% 
  filter(effect=="fixed" & term!="(Intercept)") %>% 
  select(-effect, -group, -term) %>% 
  arrange(WetlandType, response_var, IS_Type) %>% 
  mutate(estimate=round(estimate, 2),
         std.error=round(std.error, 2),
         t.value=round(t.value, 2),
         p.value.variable=round(p.value, 3),
         `Mean Sq`=round(`Mean Sq`, 2),
         DenDF =round(DenDF, 2),
         `F value`=round(`F value`, 2),
         p.value.model=round(`Pr(>F)`, 3),
         AIC  =round(AIC , 2),
         Marginal_R2 =round(Marginal_R2, 2),
         Conditional_R2  =round(Conditional_R2 , 2) ) %>% 
  select(-p.value, -`Pr(>F)`) %>%   # renamed these variables for clarity
  select(WetlandType, 
         response_var,
         IS_Type,
         estimate, std.error, t.value, p.value.variable,
         `Mean Sq`, NumDF, DenDF, `F value`, p.value.model, AIC, Marginal_R2, Conditional_R2)

write.csv(x=high_out_clean,
          file="results/model output - clean - high dist sp.csv", row.names = F)

low_out_clean <- low_out %>% 
  filter(effect=="fixed" & term!="(Intercept)") %>% 
  select(-effect, -group, -term) %>% 
  arrange(WetlandType, response_var, IS_Type) %>% 
  mutate(estimate=round(estimate, 2),
         std.error=round(std.error, 2),
         t.value=round(t.value, 2),
         p.value.variable=round(p.value, 3),
         `Mean Sq`=round(`Mean Sq`, 2),
         DenDF =round(DenDF, 2),
         `F value`=round(`F value`, 2),
         p.value.model=round(`Pr(>F)`, 3),
         AIC  =round(AIC , 2),
         Marginal_R2 =round(Marginal_R2, 2),
         Conditional_R2  =round(Conditional_R2 , 2) ) %>% 
  select(-p.value, -`Pr(>F)`) %>%  # renamed these variables for clarity
  select(WetlandType, 
         response_var,
         IS_Type,
         estimate, std.error, t.value, p.value.variable,
         `Mean Sq`, NumDF, DenDF, `F value`, p.value.model, AIC, Marginal_R2, Conditional_R2)

write.csv(x=low_out_clean,
          file="results/model output - clean - low dist sp.csv", row.names = F)
