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
