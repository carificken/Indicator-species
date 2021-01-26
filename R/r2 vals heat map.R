# this file used to create heat map of r2 vals
library(tidyverse)
library(cowplot)

rm(list=ls())
out <- read.csv("results/Model output - all models.csv") 

out <- out %>% 
  mutate(response_var = case_when(response_var == "culimp" ~ "Cul. Sp.",
                                  response_var == "CanOpen" ~ "Can. Open.",
                                  response_var == "pH_soil" ~ "pH (soil)",
                                  response_var == "pH_water" ~ "pH (water)",
                                  response_var == "propexot" ~ "Exot. Sp.",
                                  response_var == "rich" ~ "Rich.",
                                  response_var == "TC_soil" ~ "TC",
                                  response_var == "salinity" ~ "Salinity",
                                  TRUE ~ as.character(response_var))) %>% 
  mutate(response_var = factor(response_var, 
                               levels=c("Can. Open.", # first biotic vars
                                        "Cul. Sp.",
                                        "Exot. Sp.",
                                        "Rich.",
                                        
                                        "OM", # soil vars
                                        "pH (soil)",
                                        "TC",
                                        
                                        "DO",
                                        "DOC",
                                        "EC",
                                        "pH (water)",
                                        "Salinity",
                                        "TN",
                                        "TP")))

# heatmap - colored and text w/ rvals ####
{
  rvals <- out %>% 
    filter(term=="LowDistSp_N" | term=="HighDistSp_N") %>% 
    filter(p.value <= 0.050) %>% # note - this is pval of the predictor only, not full model 
    select(IS_Type, WetlandType, response_var, Marginal_R2)
  
  rvals <- rvals %>% 
    mutate(WetlandType=case_when(WetlandType=="Wet Meadow" ~ "WM",
                                 WetlandType=="Shallow Lake" ~ "SL",
                                 TRUE ~ as.character(WetlandType)))
  rvals <- rvals %>% 
    spread(., key=response_var, value=Marginal_R2) %>% 
    gather(., key=response_var, value=Marginal_R2, 3:ncol(.)) %>% 
    replace_na(list(Marginal_R2 = 0)) %>% 
    mutate(response_var = factor(response_var, 
                                 levels=c("Can. Open.", # first biotic vars
                                          "Cul. Sp.",
                                          "Exot. Sp.",
                                          "Rich.",
                                          
                                          "OM", # soil vars
                                          "pH (soil)",
                                          "TC",
                                          
                                          "DO",
                                          "DOC",
                                          "EC",
                                          "pH (water)",
                                          "Salinity",
                                          "TN",
                                          "TP")))
  
  
  # plot panels
  {
    p1 <- rvals %>% 
      filter(IS_Type=="Top 5 - Low") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="blue", limits=c(0,0.8)) +
      geom_text(data = filter(rvals, IS_Type== "Top 5 - Low" & Marginal_R2>0), aes(label=round(Marginal_R2, 2))) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      ggtitle("Top 5 Low IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    
    p2 <- rvals %>% 
      filter(IS_Type=="Full - Low") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="blue", limits=c(0,0.8)) +
      geom_text(data = filter(rvals, IS_Type== "Full - Low" & Marginal_R2>0), aes(label=round(Marginal_R2, 2))) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      ggtitle("Full Low IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    
    p3 <- rvals %>% 
      filter(IS_Type=="Top 5 - High") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="red", limits=c(0,0.8)) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      geom_text(data = filter(rvals, IS_Type== "Top 5 - High" & Marginal_R2>0), aes(label=round(Marginal_R2, 2))) +
      ggtitle("Top 5 High IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    
    p4 <- rvals %>% 
      filter(IS_Type=="Full - High") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="red", limits=c(0,0.8)) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      geom_text(data = filter(rvals, IS_Type== "Full - High" & Marginal_R2>0), aes(label=round(Marginal_R2, 2))) +
      ggtitle("Full High IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
  }
  
  myleg_low <- get_legend(p1)
  myleg_high <- get_legend(p3)
  bothlegs <- plot_grid(myleg_high, myleg_low)
  
  # plot full IS panels
  myheatmap_full <- plot_grid(
    p4+ theme(legend.position = "none"), 
    p2+ theme(legend.position = "none"), 
    nrow=1, ncol=2)
  
  # plot only top 5 is pannels
  myheatmap_top5 <- plot_grid(
    p3+ theme(legend.position = "none"),  
    p1 + theme(legend.position = "none"), 
    nrow=1, ncol=2)
  
  # full IS pannels with legend
  # this plots the marginal r2 values w/ color AND text
  plot_grid(bothlegs, myheatmap_full, ncol=1, nrow=2, rel_heights = c(0.1,1))
  
  # ggsave("results/heatmap - r2 vals color and text.jpg", width=18, height=18, units="cm",dpi=300)
  
}

# heatmap - colored w/ rvals + text w/ regression coefficient ####
{
  est <- out %>% 
    filter(term=="LowDistSp_N" | term=="HighDistSp_N") %>% 
    filter(p.value <= 0.050) %>% 
    select(IS_Type, WetlandType, response_var, estimate)
  
  est <- est %>% 
    mutate(WetlandType=case_when(WetlandType=="Wet Meadow" ~ "WM",
                                 WetlandType=="Shallow Lake" ~ "SL",
                                 TRUE ~ as.character(WetlandType)))  %>% 
    mutate(response_var = factor(response_var, 
                                 levels=c("Can. Open.", # first biotic vars
                                          "Cul. Sp.",
                                          "Exot. Sp.",
                                          "Rich.",
                                          
                                          "OM", # soil vars
                                          "pH (soil)",
                                          "TC",
                                          
                                          "DO",
                                          "DOC",
                                          "EC",
                                          "pH (water)",
                                          "Salinity",
                                          "TN",
                                          "TP")))
  # plot panels
  {
    p1 <- rvals %>% 
      filter(IS_Type=="Top 5 - Low") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="blue", limits=c(0,0.8)) +
      geom_text(data = filter(est, IS_Type== "Top 5 - Low"),
                aes(x=response_var, y= WetlandType, 
                    label=round(estimate, 2)),
                inherit.aes = F) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      ggtitle("Top 5 Low IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    
    p2 <- rvals %>% 
      filter(IS_Type=="Full - Low") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="blue", limits=c(0,0.8)) +
      geom_text(data = filter(est, IS_Type== "Full - Low"),
                aes(x=response_var, y= WetlandType, 
                    label=round(estimate, 2)),
                inherit.aes = F) +      geom_vline(xintercept = c(4.5, 7.5)) +
      ggtitle("Full Low IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    
    p3 <- rvals %>% 
      filter(IS_Type=="Top 5 - High") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="red", limits=c(0,0.8)) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      geom_text(data = filter(est, IS_Type== "Top 5 - High"),
                aes(x=response_var, y= WetlandType, 
                    label=round(estimate, 2)),
                inherit.aes = F) +      ggtitle("Top 5 High IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    
    p4 <- rvals %>% 
      filter(IS_Type=="Full - High") %>% 
      ggplot(., aes(x=response_var, y=WetlandType, fill=Marginal_R2)) +
      geom_tile() +
      scale_fill_gradient2(low="white", high="red", limits=c(0,0.8)) +
      geom_vline(xintercept = c(4.5, 7.5)) +
      geom_text(data = filter(est, IS_Type== "Full - High"),
                aes(x=response_var, y= WetlandType, 
                    label=round(estimate, 2)),
                inherit.aes = F) +      ggtitle("Full High IS list") +
      labs(x=NULL, y=NULL, fill=expression(paste(R^2, " (marg.)"))) +
      theme_bw() +
      theme(legend.position = "top") +
      coord_flip()
    }
  
  myleg_low <- get_legend(p1)
  myleg_high <- get_legend(p3)
  bothlegs <- plot_grid(myleg_high, myleg_low, labels="auto")
  
  # plot the full is panels
  # recall that cells are colored by marginal r2 value and text shows regression coefficient
  myheatmap_full <- plot_grid(
    p4+ theme(legend.position = "none", title=element_blank()), 
    p2+ theme(legend.position = "none", title=element_blank()), 
    nrow=1, ncol=2)
  
  # myheatmap_top5 <- plot_grid(
  #   p3+ theme(legend.position = "none", title=element_blank()),  
  #   p1 + theme(legend.position = "none", title=element_blank()), 
  #   nrow=1, ncol=2)
  
  plot_grid(bothlegs, 
            myheatmap_full, 
            ncol=1, nrow=2, rel_heights = c(0.1,1))
  
  ggsave("results/heatmap - full is - r2 and ests.jpg",
         width=18, height=10, units="cm",dpi=300)
  
  p <- plot_grid(
    p4+ theme(legend.position = "none", title=element_blank()), 
    p2+ theme(legend.position = "none", title=element_blank()), 

    p3+ theme(legend.position = "none", title=element_blank()),  
    p1 + theme(legend.position = "none", title=element_blank()), 
    nrow=2, ncol=2,
    labels="auto")
  
  bothlegs2 <- plot_grid(myleg_high, myleg_low)
  
  plot_grid(bothlegs2, p, ncol=1, rel_heights = c(0.1,1))
  
  ggsave("results/heatmap - full and top5 - r2 and ests.jpg",
         width=18, height=18, units="cm",dpi=300)
  
}
