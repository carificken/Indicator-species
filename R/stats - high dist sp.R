# this file run regressions to predict wetland characteristics with HIGH dist indicator species
# see "stats - low dist sp.R" for code to run analyses for low dist indicator species 

# High dist sp are sp associated with most dist; low dist species are sp associated with least dist

library(tidyverse)
library(lme4)
library(lmerTest)
library(vegan)
library(broom.mixed)

# load files - top 5 is
{
  # site-level disturbance weights, plus the wetland and env characteristics
  chars <- read.csv("data/clean/Site Dist Status - Wet and Env Chars - top 5 IS.csv")
  
  # site-level disturbance weights plus veg Presence-Absence
  rich <- read.csv("data/clean/Site Dist Status - Veg PA - top 5 IS.csv")
  
  # this file has site-level disturbance weights plus veg abundance - not using
  abund <- read.csv("data/clean/Site Dist Status - Veg Abund - top 5 IS.csv")
  
}

# load files - full is list
{
  # site-level disturbance weights, plus the wetland and env characteristics
  chars_full <- read.csv("data/clean/Site Dist Status - Wet and Env Chars - all IS.csv") 
  
  # site-level disturbance weights plus veg Presence-Absence
  rich_full <- read.csv("data/clean/Site Dist Status - Veg PA - all IS.csv") 
  
  # this file has site-level disturbance weights plus veg abundance
  abund_full <- read.csv("data/clean/Site Dist Status - Veg Abund - all IS.csv") 
  
}

# convert Year variable to factor
{
  chars$Year <- as.factor(chars$Year)
  rich$Year <- as.factor(rich$Year)
  chars_full$Year <- as.factor(chars_full$Year)
  rich_full$Year <- as.factor(rich_full$Year)
}

# standardize wetland characteristic response variables to compare IS effect sizes across characteristics
{
  # Standardize all variables with Zscores
  chars[,9:ncol(chars)] <- data.frame(scale(chars[,9:ncol(chars)], center=T, scale=T))
  rich[,9:ncol(rich)] <- data.frame(scale(rich[,9:ncol(rich)], center=T, scale=T))

  chars_full[,9:ncol(chars_full)] <- data.frame(scale(chars_full[,9:ncol(chars_full)], center=T, scale=T))
  rich_full[,9:ncol(rich_full)] <- data.frame(scale(rich_full[,9:ncol(rich_full)], center=T, scale=T))
  
}

# scope ####
{
  # num samples
  chars %>% 
    distinct(WetlandType, uniqueID) %>%
    nrow() # 1255
  chars_full %>% 
    distinct(WetlandType, uniqueID) %>%
    nrow() # 1255
  
  rich %>% 
    distinct(WetlandType, uniqueID) %>%
    nrow()# 1522
  rich_full %>% 
    distinct(WetlandType, uniqueID) %>%
    nrow() # 1522
  
  # num wetlands per wetland class (same for rich, chars, rich_full, chars_full)
  chars %>% 
    group_by(WetlandType) %>% 
    tally() 
  
  # range of years sampled - took one year per wetland
  summary(chars$Year) 
  chars %>% 
    group_by(WetlandType, uniqueID) %>% 
    tally() %>% 
    filter(n>1)
  
  # average is richness 
  head(rich_full)
  rich_full %>% 
    group_by(WetlandType) %>% 
    summarize(medHighDistSp = median(HighDistSp_N),
              minHighDistSp = min(HighDistSp_N),
              maxHighDistSp = max(HighDistSp_N),
              
              medLowDistSp = median(LowDistSp_N),
              minLowDistSp = min(LowDistSp_N),
              maxLowDistSp = max(LowDistSp_N) )
  
  # num of wetlands per env var
  head(chars_full)
  chars_full %>% gather(., key=env_var, value=value, 10:ncol(.)) %>% 
    filter(!is.na(value)) %>% 
    group_by(env_var) %>% 
    tally() %>% 
    arrange(n)
    
  chars_full %>% select(WetlandType, uniqueID, Year, CulImp_N) %>% 
    filter(is.na(CulImp_N))
  
}

#  check normality - ok 
{
  # # from The R Book p 636
  # checkdat <- filter(chars, WetlandType=="Bog")
  # checkmod <- lmer(CulImp_N ~ HighDistSp_N + (1|Protocol) + (1|Year), data=checkdat)
  # # response shows relatively linear relationship to fitted values
  # plot(checkmod, CulImp_N ~ fitted(.))
  # # variances are homogeneous across groups
  # plot(checkmod) # even spread around 0 line
  # # residuals are normally distributed
  # lattice::qqmath(checkmod) # most lie on line
  # # residuals normally distributed across groups too
  # checkmod1 <- nlme::lme(CulImp_N ~ HighDistSp_N,
  #                        random = ~1|Year/Protocol, data=checkdat)
  # qqnorm(checkmod1, ~resid(.)|Protocol) # straight line
  # qqnorm(checkmod1, ~resid(.)|Year) # straight line
  # 
  # # using total richness
  # checkdatr <- filter(rich, WetlandType=="Bog")
  # checkmodr <- lmer(TotRichness ~ HighDistSp_N + (1|Protocol) + (1|Year), data=checkdatr)
  # # response shows relatively linear relationship to fitted values
  # plot(checkmodr, TotRichness ~ fitted(.))
  # # variances are homogeneous across groups
  # plot(checkmodr) # even spread around 0 line
  # # residuals are normally distributed
  # lattice::qqmath(checkmodr) # most lie on line
  # # residuals normally distributed across groups too
  # checkmod1r <- nlme::lme(TotRichness ~ HighDistSp_N,
  #                        random = ~1|Year/Protocol, data=checkdatr)
  # qqnorm(checkmod1r, ~resid(.)|Protocol)
  # qqnorm(checkmod1r, ~resid(.)|Year)
  # 
  
}

# Analyses with: Top 5 IS ####
{
  # regressions:
  # Num of HighDistSp as fixed eff
  # Protocol and Year ran effs (for vars collected on both protocols)
  # unique ID not included b/c only 1 sample per site
  
  # top 5 - env vars, mostly #####
  {
    # 1. perform the LMERs, tidied summary stats, and anova model fits
    # # vars only meausred on terrestrial protocol sites PLUS culimp sp for non-SL wetlands
    results_chars_5ter <- chars %>%
      filter(WetlandType!="Shallow Lake") %>% 
      nest(data=-WetlandType) %>% 
      mutate(      
        culimp_fit = map(data, ~lmer(CulImp_N ~ HighDistSp_N + (1|Protocol) + (1|Year), data=.x )),
        culimp_tidied = map(culimp_fit, tidy),
        culimp_anova = map(culimp_fit, anova, type = 2),
        culimp_aic = map(culimp_fit, glance),
        culimp_Marginal_R2 = piecewiseSEM::rsquared(culimp_fit)$Marginal,
        culimp_Conditional_R2 = piecewiseSEM::rsquared(culimp_fit)$Conditional,
        
        CanOpen_fit = map(data, ~lmer(CanopyOpenness ~ HighDistSp_N + (1|Year), data=.x )),
        CanOpen_tidied = map(CanOpen_fit, tidy),
        CanOpen_anova = map(CanOpen_fit, anova, type = 2),
        CanOpen_aic = map(CanOpen_fit, glance),
        CanOpen_Marginal_R2 = piecewiseSEM::rsquared(CanOpen_fit)$Marginal,
        CanOpen_Conditional_R2 = piecewiseSEM::rsquared(CanOpen_fit)$Conditional,
        
        OM_fit = map(data, ~lmer(OMDepth_cm ~ HighDistSp_N + (1|Year), data=.x )),
        OM_tidied = map(OM_fit, tidy),
        OM_anova = map(OM_fit, anova, type = 2),
        OM_aic = map(OM_fit, glance),
        OM_Marginal_R2 = piecewiseSEM::rsquared(OM_fit)$Marginal,
        OM_Conditional_R2 = piecewiseSEM::rsquared(OM_fit)$Conditional,
        
        TC_soil_fit = map(data, ~lmer(TC_soil ~ HighDistSp_N + (1|Year), data=.x )),
        TC_soil_tidied = map(TC_soil_fit, tidy),
        TC_soil_anova = map(TC_soil_fit, anova, type = 2),
        TC_soil_aic = map(TC_soil_fit, glance),
        TC_soil_Marginal_R2 = piecewiseSEM::rsquared(TC_soil_fit)$Marginal,
        TC_soil_Conditional_R2 = piecewiseSEM::rsquared(TC_soil_fit)$Conditional,
        
        pH_soil_fit = map(data, ~lmer(pH_soil ~ HighDistSp_N + (1|Year), data=.x )),
        pH_soil_tidied = map(pH_soil_fit, tidy),
        pH_soil_anova = map(pH_soil_fit, anova, type = 2),
        pH_soil_aic = map(pH_soil_fit, glance),
        pH_soil_Marginal_R2 = piecewiseSEM::rsquared(pH_soil_fit)$Marginal,
        pH_soil_Conditional_R2 = piecewiseSEM::rsquared(pH_soil_fit)$Conditional)
    
    # vars only measured on wetland protocol
    results_chars_5wet <- chars %>%
      nest(data=-WetlandType) %>% 
      mutate(
        DO_fit = map(data, ~lmer(DO_water ~ HighDistSp_N + (1|Year), data=.x )),
        DO_tidied = map(DO_fit, tidy),
        DO_anova = map(DO_fit, anova, type = 2),
        DO_aic = map(DO_fit, glance),
        DO_Marginal_R2 = piecewiseSEM::rsquared(DO_fit)$Marginal,
        DO_Conditional_R2 = piecewiseSEM::rsquared(DO_fit)$Conditional,
        
        DOC_fit = map(data, ~lmer(DOC_water ~ HighDistSp_N + (1|Year), data=.x )),
        DOC_tidied = map(DOC_fit, tidy),
        DOC_anova = map(DOC_fit, anova, type = 2),
        DOC_aic = map(DOC_fit, glance),
        DOC_Marginal_R2 = piecewiseSEM::rsquared(DOC_fit)$Marginal,
        DOC_Conditional_R2 = piecewiseSEM::rsquared(DOC_fit)$Conditional,
        
        EC_fit = map(data, ~lmer(EC_water ~ HighDistSp_N + (1|Year), data=.x )),
        EC_tidied = map(EC_fit, tidy),
        EC_anova = map(EC_fit, anova, type = 2),
        EC_aic = map(EC_fit, glance),
        EC_Marginal_R2 = piecewiseSEM::rsquared(EC_fit)$Marginal,
        EC_Conditional_R2 = piecewiseSEM::rsquared(EC_fit)$Conditional,
        
        pH_water_fit = map(data, ~lmer(pH_water ~ HighDistSp_N + (1|Year), data=.x )),
        pH_water_tidied = map(pH_water_fit, tidy),
        pH_water_anova = map(pH_water_fit, anova, type = 2),
        pH_water_aic = map(pH_water_fit, glance),
        pH_water_Marginal_R2 = piecewiseSEM::rsquared(pH_water_fit)$Marginal,
        pH_water_Conditional_R2 = piecewiseSEM::rsquared(pH_water_fit)$Conditional,
        
        salinity_fit = map(data, ~lmer(Salinity_water ~ HighDistSp_N + (1|Year), data=.x )),
        salinity_tidied = map(salinity_fit, tidy),
        salinity_anova = map(salinity_fit, anova, type = 2),
        salinity_aic = map(salinity_fit, glance),
        salinity_Marginal_R2 = piecewiseSEM::rsquared(salinity_fit)$Marginal,
        salinity_Conditional_R2 = piecewiseSEM::rsquared(salinity_fit)$Conditional,
        
        TN_fit = map(data, ~lmer(TN_water ~ HighDistSp_N + (1|Year), data=.x )),
        TN_tidied = map(TN_fit, tidy),
        TN_anova = map(TN_fit, anova, type = 2),
        TN_aic = map(TN_fit, glance),
        TN_Marginal_R2 = piecewiseSEM::rsquared(TN_fit)$Marginal,
        TN_Conditional_R2 = piecewiseSEM::rsquared(TN_fit)$Conditional,
        
        TP_fit = map(data, ~lmer(TP_water ~ HighDistSp_N + (1|Year), data=.x )),
        TP_tidied = map(TP_fit, tidy),
        TP_anova = map(TP_fit, anova, type = 2),
        TP_aic = map(TP_fit, glance),
        TP_Marginal_R2 = piecewiseSEM::rsquared(TP_fit)$Marginal,
        TP_Conditional_R2 = piecewiseSEM::rsquared(TP_fit)$Conditional) 
    
    # 2. now separate the terrestrial and wetland model outputs into fits and summary dfs
    # # summary stats (tidied dfs) - these have stats for individual predictors (fixed, random, intercept...)
    ter_summary_stats <- results_chars_5ter %>% 
      select(WetlandType, culimp_tidied, CanOpen_tidied, OM_tidied, TC_soil_tidied, pH_soil_tidied) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    wet_summary_stats <- results_chars_5wet %>% 
      select(WetlandType, DO_tidied, DOC_tidied, EC_tidied, pH_water_tidied, salinity_tidied, TN_tidied, TP_tidied) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # model fits (anova dfs) - these have stats for overall model fit
    ter_fits <- results_chars_5ter %>% 
      select(WetlandType, CanOpen_anova, culimp_anova, OM_anova, TC_soil_anova, pH_soil_anova) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)

    wet_fits <- results_chars_5wet %>% 
      select(WetlandType, DO_anova, DOC_anova, EC_anova, pH_water_anova, salinity_anova, TN_anova, TP_anova) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # model aics - these have stats for overall model fit
    ter_aic <- results_chars_5ter %>% 
      select(WetlandType, CanOpen_aic, culimp_aic, OM_aic, TC_soil_aic, pH_soil_aic) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp) %>% 
      select(WetlandType, response_var, AIC) %>% 
      mutate(response_var = str_remove(response_var, "_aic"))
    
    wet_aic <- results_chars_5wet %>% 
      select(WetlandType, DO_aic, DOC_aic, EC_aic, pH_water_aic, salinity_aic, TN_aic, TP_aic) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp) %>% 
      select(WetlandType, response_var, AIC) %>% 
      mutate(response_var = str_remove(response_var, "_aic"))
    
    # 3. perform additional lmers on shallow lake data, separate 
    sl_m <- lmer(CulImp_N ~ HighDistSp_N + (1|Year), data=filter(chars, WetlandType=="Shallow Lake") )
    sl_summary_stats <- tibble(WetlandType="Shallow Lake", response_var="culimp", tidy(sl_m))
    sl_fits <- tibble(WetlandType="Shallow Lake", response_var="culimp", anova(sl_m,  type=2))
    sl_aic <- tibble(WetlandType="Shallow Lake", response_var="culimp", AIC=AIC(sl_m))
    
    # combine terrestrial, wetland, and shallow lake summary stats and fit dfs
    m_summary_top5 <- bind_rows(ter_summary_stats, wet_summary_stats, sl_summary_stats)
    m_summary_top5 <- m_summary_top5 %>% 
      mutate(response_var=str_remove(response_var, "_tidied")) %>% 
      arrange(response_var, WetlandType)
    
    m_fits_top5 <- bind_rows(ter_fits, wet_fits, sl_fits)
    m_fits_top5 <- m_fits_top5 %>% 
      mutate(response_var=str_remove(response_var, "_anova")) %>% 
      arrange(response_var, WetlandType)
    aics <- bind_rows(ter_aic, wet_aic, sl_aic) # combine aic values and add to fits df
    m_fits_top5 <- left_join(m_fits_top5, aics)
    
    # 4. gather all the R2 values for terrestrial and wetland models; add sl culimp r2 vals
    ter_margr <- results_chars_5ter %>% 
      select(WetlandType, culimp_Marginal_R2, CanOpen_Marginal_R2, OM_Marginal_R2, TC_soil_Marginal_R2, pH_soil_Marginal_R2) %>% 
      gather(., key="response_var", value="Marginal_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Marginal_R2"))
    ter_condr <- results_chars_5ter %>% 
      select(WetlandType, culimp_Conditional_R2, CanOpen_Conditional_R2, OM_Conditional_R2, TC_soil_Conditional_R2, pH_soil_Conditional_R2) %>% 
      gather(., key="response_var", value="Conditional_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Conditional_R2"))
    rvals_top5_ter <- full_join(ter_margr, ter_condr, by=c("WetlandType", "response_var"))
    
    sl_r2 <- data.frame(WetlandType = "Shallow Lake",
                        response_var = "culimp",
                        Marginal_R2 = piecewiseSEM::rsquared(sl_m)$Marginal,
                        Conditional_R2 = piecewiseSEM::rsquared(sl_m)$Conditional)
    rvals_top5 <- bind_rows(rvals_top5_ter, sl_r2) %>% arrange(response_var, WetlandType)
    
    wet_margr <- results_chars_5wet %>% 
      select(WetlandType, DO_Marginal_R2, DOC_Marginal_R2, EC_Marginal_R2, pH_water_Marginal_R2, salinity_Marginal_R2, TN_Marginal_R2, TP_Marginal_R2) %>% 
      gather(., key="response_var", value="Marginal_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Marginal_R2"))
    wet_condr <- results_chars_5wet %>% 
      select(WetlandType, DO_Conditional_R2, DOC_Conditional_R2, EC_Conditional_R2, pH_water_Conditional_R2, salinity_Conditional_R2, TN_Conditional_R2, TP_Conditional_R2) %>% 
      gather(., key="response_var", value="Conditional_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Conditional_R2"))
    rvals_top5_wet <- full_join(wet_condr, wet_margr)
    
    # finaly join the wetland r2 vals with the terrestrial + sl cul imp 
    rvals_top5 <- bind_rows(rvals_top5, rvals_top5_wet)
    
    # 5. add r2 vals to fits df
    m_fits_top5 <- full_join(m_fits_top5, rvals_top5, by=c("WetlandType", "response_var"))
    
    # 6. remove unwanted columns and rename columns for clarity 
    m_fits_top5 <- m_fits_top5 %>% select(-`Sum Sq`)
    m_summary_top5 <- m_summary_top5 %>% 
      select(WetlandType, response_var, effect, group, term, estimate, std.error, "t.value" = statistic, p.value)
    
  }
  

  # top 5 plant diversity vars ####
  {
    # 1. perform the LMERs, tidied summary stats, and anova model fits
    results_rich_top5 <- rich %>% 
      filter(WetlandType!="Shallow Lake") %>% 
      nest(data=-WetlandType) %>% 
      mutate(
        rich_fit = map(data, ~lmer(TotRichness ~ HighDistSp_N + (1|Protocol) + (1|Year), data=.x )),
        rich_tidied = map(rich_fit, tidy),
        rich_anova = map(rich_fit, anova, type = 2),
        rich_aic = map(rich_fit, glance),
        rich_Marginal_R2 = piecewiseSEM::rsquared(rich_fit)$Marginal,
        rich_Conditional_R2 = piecewiseSEM::rsquared(rich_fit)$Conditional,
        
        propexot_fit = map(data, ~lmer(PropExot ~ HighDistSp_N + (1|Protocol) + (1|Year), data=.x )),
        propexot_tidied = map(propexot_fit, tidy),
        propexot_anova = map(propexot_fit, anova, type = 2),
        propexot_aic = map(propexot_fit, glance),
        propexot_Marginal_R2 = piecewiseSEM::rsquared(propexot_fit)$Marginal,
        propexot_Conditional_R2 = piecewiseSEM::rsquared(propexot_fit)$Conditional      )
    
    # 2. now separate the terrestrial and wetland model outputs into fits and summary dfs
    # # summary stats (tidied dfs)
    rich_summary_stats_top5 <- results_rich_top5 %>% 
      select(WetlandType, rich_tidied, propexot_tidied) %>%  
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # model fits (anova dfs)
    rich_fits_top5 <- results_rich_top5 %>% 
      select(WetlandType, rich_anova, propexot_anova) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # aics
    rich_aic_top5 <- results_rich_top5 %>% 
      select(WetlandType, rich_aic, propexot_aic) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp) %>% 
      select(WetlandType, response_var, AIC) %>% 
      mutate(response_var = str_remove(response_var, "_aic"))
    
    # 3. now perform additional lmers on shallow lake data, separate 
    sl_rich_m <- lmer(TotRichness ~ HighDistSp_N + (1|Year), data=filter(rich, WetlandType=="Shallow Lake") )
    sl_rich_summary_stats <- tibble(WetlandType="Shallow Lake", response_var="rich", tidy(sl_rich_m))
    sl_rich_fits <- tibble(WetlandType="Shallow Lake", response_var="rich", anova(sl_rich_m, type=2))
    sl_rich_aic <- tibble(WetlandType="Shallow Lake", response_var="rich", AIC=AIC(sl_rich_m))
    
    sl_propexot_m <- lmer(PropExot ~ HighDistSp_N + (1|Year), data=filter(rich, WetlandType=="Shallow Lake") )
    sl_propexot_summary_stats <- tibble(WetlandType="Shallow Lake", response_var="propexot", tidy(sl_propexot_m))
    sl_propexot_fits <- tibble(WetlandType="Shallow Lake", response_var="propexot", anova(sl_propexot_m, type=2))
    sl_propexot_aic <- tibble(WetlandType="Shallow Lake", response_var="propexot", AIC=AIC(sl_propexot_m))
    
    sl_summary_stats2 <- bind_rows(sl_rich_summary_stats, sl_propexot_summary_stats)
    sl_fits2 <- bind_rows(sl_rich_fits, sl_propexot_fits)
    
    # combine full df with shallow lake summary stats and fit dfs
    m_summary_top5_rich <- bind_rows(rich_summary_stats_top5, sl_summary_stats2)
    m_summary_top5_rich <- m_summary_top5_rich %>% 
      mutate(response_var=str_remove(response_var, "_tidied")) %>% 
      arrange(response_var, WetlandType)
    
    m_fits_top5_rich <- bind_rows(rich_fits_top5, sl_fits2)
    m_fits_top5_rich <- m_fits_top5_rich %>% 
      mutate(response_var=str_remove(response_var, "_anova")) %>% 
      arrange(response_var, WetlandType)
    
    aics <- bind_rows(rich_aic_top5, sl_rich_aic, sl_propexot_aic) # combine aic values and add to fits df
    m_fits_top5_rich <- left_join(m_fits_top5_rich, aics, by=c("WetlandType", "response_var"))
    
    # 4. gather all the R2 values for terrestrial and wetland models; add sl culimp r2 vals
    rich_margr <- results_rich_top5 %>% 
      select(WetlandType, rich_Marginal_R2, propexot_Marginal_R2) %>% 
      gather(., key="response_var", value="Marginal_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Marginal_R2"))
    
    rich_condr <- results_rich_top5 %>% 
      select(WetlandType, rich_Conditional_R2, propexot_Conditional_R2) %>% 
      gather(., key="response_var", value="Conditional_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Conditional_R2"))
    rvals_top5_rich <- full_join(rich_margr, rich_condr, by=c("WetlandType", "response_var"))
    
    sl_rich_r2_top5 <- data.frame(WetlandType = c("Shallow Lake","Shallow Lake"),
                        response_var = c("rich","propexot"),
                        Marginal_R2 = c(piecewiseSEM::rsquared(sl_rich_m)$Marginal,
                                        piecewiseSEM::rsquared(sl_propexot_m)$Marginal),
                        Conditional_R2 = c(piecewiseSEM::rsquared(sl_rich_m)$Conditional,
                                           piecewiseSEM::rsquared(sl_propexot_m)$Conditional))
    
    rvals_top5_rich <- bind_rows(rvals_top5_rich, sl_rich_r2_top5) %>% 
      arrange(response_var, WetlandType)
    
    # 5. add r2 vals to fits df
    m_fits_top5_rich <- full_join(m_fits_top5_rich, rvals_top5_rich, by=c("WetlandType", "response_var"))
    
    # 6. remove unwanted columns and rename columns for clarity 
    m_fits_top5_rich <- m_fits_top5_rich %>% select(-`Sum Sq`)
    m_summary_top5_rich <- m_summary_top5_rich %>% select(WetlandType, response_var, effect, group, term, estimate, std.error, "t.value" = statistic, p.value)
  }
  
}

# Analyses with: Full IS list #### 
{
  # regressions:
  # Num of HighDistSp as fixed eff
  # Protocol and Year ran effs (for vars collected on both protocols)
  # unique ID not included b/c only 1 sample per site
  
  # full is list - env vars ####
  {
    # 1. perform the LMERs, tidied summary stats, and anova model fits
    # # vars only meausred on terrestrial protocol sites PLUS culimp sp for non-SL wetlands
    results_chars_fullter <- chars_full %>%
      filter(WetlandType!="Shallow Lake") %>% 
      nest(data=-WetlandType) %>% 
      mutate(      
        culimp_fit = map(data, ~lmer(CulImp_N ~ HighDistSp_N + (1|Protocol) + (1|Year), data=.x )),
        culimp_tidied = map(culimp_fit, tidy),
        culimp_anova = map(culimp_fit, anova, type = 2),
        culimp_aic = map(culimp_fit, glance),
        culimp_Marginal_R2 = piecewiseSEM::rsquared(culimp_fit)$Marginal,
        culimp_Conditional_R2 = piecewiseSEM::rsquared(culimp_fit)$Conditional,
        
        CanOpen_fit = map(data, ~lmer(CanopyOpenness ~ HighDistSp_N + (1|Year), data=.x )),
        CanOpen_tidied = map(CanOpen_fit, tidy),
        CanOpen_anova = map(CanOpen_fit, anova, type = 2),
        CanOpen_aic = map(CanOpen_fit, glance),
        CanOpen_Marginal_R2 = piecewiseSEM::rsquared(CanOpen_fit)$Marginal,
        CanOpen_Conditional_R2 = piecewiseSEM::rsquared(CanOpen_fit)$Conditional,
        
        OM_fit = map(data, ~lmer(OMDepth_cm ~ HighDistSp_N + (1|Year), data=.x )),
        OM_tidied = map(OM_fit, tidy),
        OM_anova = map(OM_fit, anova, type = 2),
        OM_aic = map(OM_fit, glance),
        OM_Marginal_R2 = piecewiseSEM::rsquared(OM_fit)$Marginal,
        OM_Conditional_R2 = piecewiseSEM::rsquared(OM_fit)$Conditional,
        
        TC_soil_fit = map(data, ~lmer(TC_soil ~ HighDistSp_N + (1|Year), data=.x )),
        TC_soil_tidied = map(TC_soil_fit, tidy),
        TC_soil_anova = map(TC_soil_fit, anova, type = 2),
        TC_soil_aic = map(TC_soil_fit, glance),
        TC_soil_Marginal_R2 = piecewiseSEM::rsquared(TC_soil_fit)$Marginal,
        TC_soil_Conditional_R2 = piecewiseSEM::rsquared(TC_soil_fit)$Conditional,
        
        pH_soil_fit = map(data, ~lmer(pH_soil ~ HighDistSp_N + (1|Year), data=.x )),
        pH_soil_tidied = map(pH_soil_fit, tidy),
        pH_soil_anova = map(pH_soil_fit, anova, type = 2),
        pH_soil_aic = map(pH_soil_fit, glance),
        pH_soil_Marginal_R2 = piecewiseSEM::rsquared(pH_soil_fit)$Marginal,
        pH_soil_Conditional_R2 = piecewiseSEM::rsquared(pH_soil_fit)$Conditional)
    
    # vars only measured on wetland protocol
    results_chars_fullwet <- chars_full %>%
      nest(data=-WetlandType) %>% 
      mutate(
        DO_fit = map(data, ~lmer(DO_water ~ HighDistSp_N + (1|Year), data=.x )),
        DO_tidied = map(DO_fit, tidy),
        DO_anova = map(DO_fit, anova, type = 2),
        DO_aic = map(DO_fit, glance),
        DO_Marginal_R2 = piecewiseSEM::rsquared(DO_fit)$Marginal,
        DO_Conditional_R2 = piecewiseSEM::rsquared(DO_fit)$Conditional,
        
        DOC_fit = map(data, ~lmer(DOC_water ~ HighDistSp_N + (1|Year), data=.x )),
        DOC_tidied = map(DOC_fit, tidy),
        DOC_anova = map(DOC_fit, anova, type = 2),
        DOC_aic = map(DOC_fit, glance),
        DOC_Marginal_R2 = piecewiseSEM::rsquared(DOC_fit)$Marginal,
        DOC_Conditional_R2 = piecewiseSEM::rsquared(DOC_fit)$Conditional,
        
        EC_fit = map(data, ~lmer(EC_water ~ HighDistSp_N + (1|Year), data=.x )),
        EC_tidied = map(EC_fit, tidy),
        EC_anova = map(EC_fit, anova, type = 2),
        EC_aic = map(EC_fit, glance),
        EC_Marginal_R2 = piecewiseSEM::rsquared(EC_fit)$Marginal,
        EC_Conditional_R2 = piecewiseSEM::rsquared(EC_fit)$Conditional,
        
        pH_water_fit = map(data, ~lmer(pH_water ~ HighDistSp_N + (1|Year), data=.x )),
        pH_water_tidied = map(pH_water_fit, tidy),
        pH_water_anova = map(pH_water_fit, anova, type = 2),
        pH_water_aic = map(pH_water_fit, glance),
        pH_water_Marginal_R2 = piecewiseSEM::rsquared(pH_water_fit)$Marginal,
        pH_water_Conditional_R2 = piecewiseSEM::rsquared(pH_water_fit)$Conditional,
        
        salinity_fit = map(data, ~lmer(Salinity_water ~ HighDistSp_N + (1|Year), data=.x )),
        salinity_tidied = map(salinity_fit, tidy),
        salinity_anova = map(salinity_fit, anova, type = 2),
        salinity_aic = map(salinity_fit, glance),
        salinity_Marginal_R2 = piecewiseSEM::rsquared(salinity_fit)$Marginal,
        salinity_Conditional_R2 = piecewiseSEM::rsquared(salinity_fit)$Conditional,
        
        TN_fit = map(data, ~lmer(TN_water ~ HighDistSp_N + (1|Year), data=.x )),
        TN_tidied = map(TN_fit, tidy),
        TN_anova = map(TN_fit, anova, type = 2),
        TN_aic = map(TN_fit, glance),
        TN_Marginal_R2 = piecewiseSEM::rsquared(TN_fit)$Marginal,
        TN_Conditional_R2 = piecewiseSEM::rsquared(TN_fit)$Conditional,
        
        TP_fit = map(data, ~lmer(TP_water ~ HighDistSp_N + (1|Year), data=.x )),
        TP_tidied = map(TP_fit, tidy),
        TP_anova = map(TP_fit, anova, type = 2),
        TP_aic = map(TP_fit, glance),
        TP_Marginal_R2 = piecewiseSEM::rsquared(TP_fit)$Marginal,
        TP_Conditional_R2 = piecewiseSEM::rsquared(TP_fit)$Conditional) 
    
    # 2. now separate the terrestrial and wetland model outputs into fits and summary dfs
    # # summary stats (tidied dfs) - these have stats for individual predictors (fixed, random, intercept...)
    ter_summary_stats <- results_chars_fullter %>% 
      select(WetlandType, culimp_tidied, CanOpen_tidied, OM_tidied, TC_soil_tidied, pH_soil_tidied) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    wet_summary_stats <- results_chars_fullwet %>% 
      select(WetlandType, DO_tidied, DOC_tidied, EC_tidied, pH_water_tidied, salinity_tidied, TN_tidied, TP_tidied) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # model fits (anova dfs) - these have stats for overall model fit
    ter_fits <- results_chars_fullter %>% 
      select(WetlandType, CanOpen_anova, culimp_anova, OM_anova, TC_soil_anova, pH_soil_anova) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    wet_fits <- results_chars_fullwet %>% 
      select(WetlandType, DO_anova, DOC_anova, EC_anova, pH_water_anova, salinity_anova, TN_anova, TP_anova) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # model aics - these have stats for overall model fit
    ter_aic <- results_chars_fullter %>% 
      select(WetlandType, CanOpen_aic, culimp_aic, OM_aic, TC_soil_aic, pH_soil_aic) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp) %>% 
      select(WetlandType, response_var, AIC) %>% 
      mutate(response_var = str_remove(response_var, "_aic"))
    
    wet_aic <- results_chars_fullwet %>% 
      select(WetlandType, DO_aic, DOC_aic, EC_aic, pH_water_aic, salinity_aic, TN_aic, TP_aic) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp) %>% 
      select(WetlandType, response_var, AIC) %>% 
      mutate(response_var = str_remove(response_var, "_aic"))
    
    # 3. perform additional lmers on shallow lake data, separate 
    sl_m <- lmer(CulImp_N ~ HighDistSp_N + (1|Year), data=filter(chars_full, WetlandType=="Shallow Lake") )
    sl_summary_stats <- tibble(WetlandType="Shallow Lake", response_var="culimp", tidy(sl_m))
    sl_fits <- tibble(WetlandType="Shallow Lake", response_var="culimp", anova(sl_m,  type=2))
    sl_aic <- tibble(WetlandType="Shallow Lake", response_var="culimp", AIC=AIC(sl_m))
    
    # combine terrestrial, wetland, and shallow lake summary stats and fit dfs
    m_summary_full <- bind_rows(ter_summary_stats, wet_summary_stats, sl_summary_stats)
    m_summary_full <- m_summary_full %>% 
      mutate(response_var=str_remove(response_var, "_tidied")) %>% 
      arrange(response_var, WetlandType)
    
    m_fits_full <- bind_rows(ter_fits, wet_fits, sl_fits)
    m_fits_full <- m_fits_full %>% 
      mutate(response_var=str_remove(response_var, "_anova")) %>% 
      arrange(response_var, WetlandType)
    aics <- bind_rows(ter_aic, wet_aic, sl_aic) # combine aic values and add to fits df
    m_fits_full <- left_join(m_fits_full, aics, by=c("WetlandType", "response_var"))
    
    # 4. gather all the R2 values for terrestrial and wetland models; add sl culimp r2 vals
    ter_margr <- results_chars_fullter %>% 
      select(WetlandType, culimp_Marginal_R2, CanOpen_Marginal_R2, OM_Marginal_R2, TC_soil_Marginal_R2, pH_soil_Marginal_R2) %>% 
      gather(., key="response_var", value="Marginal_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Marginal_R2"))
    ter_condr <- results_chars_fullter %>% 
      select(WetlandType, culimp_Conditional_R2, CanOpen_Conditional_R2, OM_Conditional_R2, TC_soil_Conditional_R2, pH_soil_Conditional_R2) %>% 
      gather(., key="response_var", value="Conditional_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Conditional_R2"))
    rvals_full_ter <- full_join(ter_margr, ter_condr, by=c("WetlandType", "response_var"))
    
    sl_r2 <- data.frame(WetlandType = "Shallow Lake",
                        response_var = "culimp",
                        Marginal_R2 = piecewiseSEM::rsquared(sl_m)$Marginal,
                        Conditional_R2 = piecewiseSEM::rsquared(sl_m)$Conditional)
    rvals_full <- bind_rows(rvals_full_ter, sl_r2) %>% arrange(response_var, WetlandType)
    
    wet_margr <- results_chars_fullwet %>% 
      select(WetlandType, DO_Marginal_R2, DOC_Marginal_R2, EC_Marginal_R2, pH_water_Marginal_R2, salinity_Marginal_R2, TN_Marginal_R2, TP_Marginal_R2) %>% 
      gather(., key="response_var", value="Marginal_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Marginal_R2"))
    wet_condr <- results_chars_fullwet %>% 
      select(WetlandType, DO_Conditional_R2, DOC_Conditional_R2, EC_Conditional_R2, pH_water_Conditional_R2, salinity_Conditional_R2, TN_Conditional_R2, TP_Conditional_R2) %>% 
      gather(., key="response_var", value="Conditional_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Conditional_R2"))
    rvals_full_wet <- full_join(wet_condr, wet_margr)
    
    # finaly join the wetland r2 vals with the terrestrial + sl cul imp 
    rvals_full <- bind_rows(rvals_full, rvals_full_wet)
    
    # 5. add r2 vals to fits df
    m_fits_full <- full_join(m_fits_full, rvals_full, by=c("WetlandType", "response_var"))
    
    # 6. remove unwanted columns and rename columns for clarity 
    m_fits_full <- m_fits_full %>% select(-`Sum Sq`)
    m_summary_full <- m_summary_full %>% 
      select(WetlandType, response_var, effect, group, term, estimate, std.error, "t.value" = statistic, p.value)
    
  }
  
  
  # full is list - plant diversity vars ####
  {
    # 1. perform the LMERs, tidied summary stats, and anova model fits
    results_rich_full <- rich_full %>% 
      filter(WetlandType!="Shallow Lake") %>% 
      nest(data=-WetlandType) %>% 
      mutate(
        rich_fit = map(data, ~lmer(TotRichness ~ HighDistSp_N + (1|Protocol) + (1|Year), data=.x )),
        rich_tidied = map(rich_fit, tidy),
        rich_anova = map(rich_fit, anova, type = 2),
        rich_aic = map(rich_fit, glance),
        rich_Marginal_R2 = piecewiseSEM::rsquared(rich_fit)$Marginal,
        rich_Conditional_R2 = piecewiseSEM::rsquared(rich_fit)$Conditional,
        
        propexot_fit = map(data, ~lmer(PropExot ~ HighDistSp_N + (1|Protocol) + (1|Year), data=.x )),
        propexot_tidied = map(propexot_fit, tidy),
        propexot_anova = map(propexot_fit, anova, type = 2),
        propexot_aic = map(propexot_fit, glance),
        propexot_Marginal_R2 = piecewiseSEM::rsquared(propexot_fit)$Marginal,
        propexot_Conditional_R2 = piecewiseSEM::rsquared(propexot_fit)$Conditional      )
    
    # 2. now separate the terrestrial and wetland model outputs into fits and summary dfs
    # # summary stats (tidied dfs)
    rich_summary_stats_full <- results_rich_full %>% 
      select(WetlandType, rich_tidied, propexot_tidied) %>%  
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # model fits (anova dfs)
    rich_fits_full <- results_rich_full %>% 
      select(WetlandType, rich_anova, propexot_anova) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp)
    
    # aics
    rich_aic_full <- results_rich_full %>% 
      select(WetlandType, rich_aic, propexot_aic) %>% 
      gather(., key="response_var", value="tmp", 2:ncol(.)) %>% 
      unnest(tmp) %>% 
      select(WetlandType, response_var, AIC) %>% 
      mutate(response_var = str_remove(response_var, "_aic"))
    
    # 3. now perform additional lmers on shallow lake data, separate 
    sl_rich_m <- lmer(TotRichness ~ HighDistSp_N + (1|Year), data=filter(rich_full, WetlandType=="Shallow Lake") )
    sl_rich_summary_stats <- tibble(WetlandType="Shallow Lake", response_var="rich", tidy(sl_rich_m))
    sl_rich_fits <- tibble(WetlandType="Shallow Lake", response_var="rich", anova(sl_rich_m, type=2))
    sl_rich_aic <- tibble(WetlandType="Shallow Lake", response_var="rich", AIC=AIC(sl_rich_m))
    
    sl_propexot_m <- lmer(PropExot ~ HighDistSp_N + (1|Year), data=filter(rich_full, WetlandType=="Shallow Lake") )
    sl_propexot_summary_stats <- tibble(WetlandType="Shallow Lake", response_var="propexot", tidy(sl_propexot_m))
    sl_propexot_fits <- tibble(WetlandType="Shallow Lake", response_var="propexot", anova(sl_propexot_m, type=2))
    sl_propexot_aic <- tibble(WetlandType="Shallow Lake", response_var="propexot", AIC=AIC(sl_propexot_m))
    
    sl_summary_stats2 <- bind_rows(sl_rich_summary_stats, sl_propexot_summary_stats)
    sl_fits2 <- bind_rows(sl_rich_fits, sl_propexot_fits)
    
    # combine full df with shallow lake summary stats and fit dfs
    m_summary_full_rich <- bind_rows(rich_summary_stats_full, sl_summary_stats2)
    m_summary_full_rich <- m_summary_full_rich %>% 
      mutate(response_var=str_remove(response_var, "_tidied")) %>% 
      arrange(response_var, WetlandType)
    
    m_fits_full_rich <- bind_rows(rich_fits_full, sl_fits2)
    m_fits_full_rich <- m_fits_full_rich %>% 
      mutate(response_var=str_remove(response_var, "_anova")) %>% 
      arrange(response_var, WetlandType)
    
    aics <- bind_rows(rich_aic_full, sl_rich_aic, sl_propexot_aic) # combine aic values and add to fits df
    m_fits_full_rich <- left_join(m_fits_full_rich, aics, by=c("WetlandType", "response_var"))
    
    # 4. gather all the R2 values for terrestrial and wetland models; add sl culimp r2 vals
    rich_margr <- results_rich_full %>% 
      select(WetlandType, rich_Marginal_R2, propexot_Marginal_R2) %>% 
      gather(., key="response_var", value="Marginal_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Marginal_R2"))
    
    rich_condr <- results_rich_full %>% 
      select(WetlandType, rich_Conditional_R2, propexot_Conditional_R2) %>% 
      gather(., key="response_var", value="Conditional_R2", 2:ncol(.)) %>% 
      mutate(response_var = str_remove(response_var, pattern="_Conditional_R2"))
    rvals_full_rich <- full_join(rich_margr, rich_condr, by=c("WetlandType", "response_var"))
    
    sl_rich_r2_full <- data.frame(WetlandType = c("Shallow Lake","Shallow Lake"),
                                  response_var = c("rich","propexot"),
                                  Marginal_R2 = c(piecewiseSEM::rsquared(sl_rich_m)$Marginal,
                                                  piecewiseSEM::rsquared(sl_propexot_m)$Marginal),
                                  Conditional_R2 = c(piecewiseSEM::rsquared(sl_rich_m)$Conditional,
                                                     piecewiseSEM::rsquared(sl_propexot_m)$Conditional))
    
    rvals_full_rich <- bind_rows(rvals_full_rich, sl_rich_r2_full) %>% 
      arrange(response_var, WetlandType)
    
    # 5. add r2 vals to fits df
    m_fits_full_rich <- full_join(m_fits_full_rich, rvals_full_rich, by=c("WetlandType", "response_var"))
    
    # 6. remove unwanted columns and rename columns for clarity 
    m_fits_full_rich <- m_fits_full_rich %>% select(-`Sum Sq`)
    m_summary_full_rich <- m_summary_full_rich %>% select(WetlandType, response_var, effect, group, term, estimate, std.error, "t.value" = statistic, p.value)
  }
  
}

# combine and export results for all models
{
  # join richness and chars output dfs togehter
  
  # m_summary_top5
  # m_fits_top5
  # m_summary_top5_rich
  # m_fits_top5_rich
  # 
  # m_summary_full
  # m_fits_full
  # m_summary_full_rich
  # m_fits_full_rich
  
  m_summary_top5 <- bind_rows(m_summary_top5, m_summary_top5_rich)
  m_fits_top5 <- bind_rows(m_fits_top5, m_fits_top5_rich)
  m_out_top5 <- left_join(m_summary_top5, m_fits_top5,
                          by=c("WetlandType", "response_var"))
  
  m_summary_full <- bind_rows(m_summary_full, m_summary_full_rich)
  m_fits_full <- bind_rows(m_fits_full, m_fits_full_rich)
  m_out_full <- left_join(m_summary_full, m_fits_full,
                          by=c("WetlandType", "response_var"))
  
  # r2 vals of sig models
  m_out_full %>% 
    filter(term=="HighDistSp_N") %>% 
    filter(p.value<=0.050) %>% 
    select(WetlandType, response_var, Marginal_R2) %>% 
    mutate(Marginal_R2= round(Marginal_R2, 2)) %>% 
    arrange(WetlandType) %>% 
    spread(., key=response_var, value=Marginal_R2)
  
  m_out_top5$IS_Type <- "Top 5 - High"
  m_out_full$IS_Type <- "Full - High"
  
  high_out <- bind_rows(m_out_top5, m_out_full) %>% select(IS_Type, everything())
  
  
}
