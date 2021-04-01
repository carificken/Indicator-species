# this file used to create heat map of r2 vals
library(tidyverse)
library(cowplot)

rm(list=ls())

# load data 
{
  # load df with all model output
  out <- read.csv("results/Model output - all models.csv") 
  # shorten response variable names for clean data vis
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
  # extract the marginal R2 values
  # r2 values tell us about the amount of variability explained - the accuracy of the model
  rvals <- out %>% 
    filter(term=="LowDistSp_N" | term=="HighDistSp_N") %>% 
    filter(p.value <= 0.050) %>% # note - this is pval of the predictor only, not full model 
    select(IS_Type, WetlandType, response_var, Marginal_R2)
  # shorten names for easy data vis
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
  
  # now extract standardized regression coefficients (estimates) 
  # the standardized estimates tells us about the size of the relationship (that is, whether the response changes a lot or a little)
  est <- out %>% 
    filter(term=="LowDistSp_N" | term=="HighDistSp_N") %>% 
    filter(p.value <= 0.050) %>% 
    select(IS_Type, WetlandType, response_var, estimate)
  # clean and shorten names for easy plotting
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
}

# heatmap - colored w/ rvals + text w/ regression coefficient ####
{
  
  # plot and store individual panels - top 5 and full IS list
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
      guides(fill = guide_colourbar(title.vjust = 0.9)) +
      theme(legend.position = "top",
            axis.text = element_text(color=1)) +
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
      guides(fill = guide_colourbar(title.vjust = 0.9)) +
      theme(legend.position = "top",
            axis.text = element_text(color=1)) +
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
      guides(fill = guide_colourbar(title.vjust = 0.9)) +
      theme(legend.position = "top",
            axis.text = element_text(color=1)) +
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
      guides(fill = guide_colourbar(title.vjust = 0.9)) +
      theme(legend.position = "top",
            axis.text = element_text(color=1)) +
      coord_flip()
  }
  
  # create legends
  {
    myleg_low <- get_legend(p1) # legend for low IS
    myleg_high <- get_legend(p3) # legend for high IS
    bothlegs <- plot_grid(myleg_high, myleg_low, labels="auto") # legend for plots with both low and high IS
  }

  # create and save plot with FULL IS panels
  {
    myheatmap_full <- plot_grid(
      p4+ theme(legend.position = "none", title=element_blank()), 
      p2+ theme(legend.position = "none", title=element_blank()), 
      nrow=1, ncol=2)
    
    full_is_heatmap <- plot_grid(bothlegs, 
              myheatmap_full, 
              ncol=1, nrow=2, rel_heights = c(0.15,1))
    full_is_heatmap
    ggsave(full_is_heatmap,
           file="./manuscript/figures/fig1.jpg",
           width=18, height=10, units="cm",dpi=300)
  }
  
  # create and save plot with top 5 IS panels
  {
    myheatmap_top5 <- plot_grid(
      p3+ theme(legend.position = "none", title=element_blank()),
      p1 + theme(legend.position = "none", title=element_blank()),
      nrow=1, ncol=2)
    
    top5_heatmap <- plot_grid(bothlegs, 
              myheatmap_top5, 
              ncol=1, nrow=2, rel_heights = c(0.1,1))
    top5_heatmap
    ggsave(top5_heatmap,
           file="./manuscript/figures/top5_heatmap.jpg",
           width=18, height=10, units="cm",dpi=300)
  }
  
  # create and save plot with full and top 5 IS panels
  {
    combo_heatmap <- plot_grid(
      p4+ theme(legend.position = "none", title=element_blank()),
      p2 + theme(legend.position = "none", title=element_blank()),

      p3+ theme(legend.position = "none", title=element_blank()),
      p1 + theme(legend.position = "none", title=element_blank()),
      nrow=2, ncol=2,
      labels="auto")
    
    combo_heatmap <- plot_grid(plot_grid(myleg_high, myleg_low), # redo legend to remove panel lettering 
                             combo_heatmap, 
                              ncol=1, nrow=2, rel_heights = c(0.1,1.1))
    combo_heatmap
    ggsave(combo_heatmap,
           file="./manuscript/figures/figS1 - full and top5 heatmap.jpg",
           width=18, height=18, units="cm",dpi=300)
  }

  

  

  
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

# heatmap - colored and text w/ rvals - dont use this one #### 
{
  # this version has the cell color and the cell text both depicting the marginal R2 values
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



