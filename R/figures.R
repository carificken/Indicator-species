rm(list=ls())

library(tidyverse)
library(cowplot)
library(colorblindr)
library(grid)
library(gridExtra)

# load files - top 5 is
{
  # site-level disturbance weights, plus the wetland and env characteristics
  chars <- read.csv("data/clean/Site Dist Status - Wet and Env Chars - top 5 IS.csv")
  
  # site-level disturbance weights plus veg Presence-Absence
  rich <- read.csv("data/clean/Site Dist Status - Veg PA - top 5 IS.csv")
  
  # this file has site-level disturbance weights plus veg abundance - not using
  abund <- read.csv("data/clean/Site Dist Status - Veg Abund - top 5 IS.csv")
  
  # convert year to factor
  chars$Year <- as.factor(chars$Year)
  rich$Year <- as.factor(rich$Year)
  
}

# load files - full is list
{
  # site-level disturbance weights, plus the wetland and env characteristics
  chars_full <- read.csv("data/clean/Site Dist Status - Wet and Env Chars - all IS.csv") 
  
  # site-level disturbance weights plus veg Presence-Absence
  rich_full <- read.csv("data/clean/Site Dist Status - Veg PA - all IS.csv") 
  
  # this file has site-level disturbance weights plus veg abundance
  abund_full <- read.csv("data/clean/Site Dist Status - Veg Abund - all IS.csv") 
  
  # convert year to factor
  chars_full$Year <- as.factor(chars_full$Year)
  rich_full$Year <- as.factor(rich_full$Year)
}

# load model output summaries for model comparisons
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
    select(IS_Type, WetlandType, response_var, Marginal_R2, p.value)
  # shorten names for easy data vis
  rvals <- rvals %>% 
    mutate(WetlandType=case_when(WetlandType=="Wet Meadow" ~ "WM",
                                 WetlandType=="Shallow Lake" ~ "SL",
                                 TRUE ~ as.character(WetlandType)))
  rvals <- rvals %>% 
    # relevel response var for clean plotting
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
    select(IS_Type, WetlandType, response_var, estimate, std.error, p.value)
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

# check results for out liers
{
  head(rich)
  ggplot(rich, aes(x=LowDistSp_N, y=PropExot)) +
    geom_jitter(color=1, pch=1, width=0.3) +
    geom_smooth(method="lm") +
    facet_wrap(~WetlandType, scale="free_y")
  
  chars %>% 
    filter(WetlandType=="Wet Meadow") %>% 
    gather(., key="Wet_response", value = Val, 9:ncol(.) ) %>% 
    ggplot(., aes(x=LowDistSp_N, y=Val)) +
    # geom_point(color=1, pch=1) +
    geom_jitter(color=1, pch=1, width=0.3) +
    geom_smooth(method="lm") +
    facet_wrap(~Wet_response, scale="free_y")
}

# fig 1 - heat map -- see `r2 vals heat map.R` ####

# fig 2 - model comparison eg bogs
{
  # r2 vals - model accuracy
  {
    r2_high_bog <- ggplot(filter(rvals, WetlandType=="Bog" & str_detect(IS_Type, "High")), 
                          aes(x=Marginal_R2, y=response_var, fill=IS_Type)) + 
      geom_bar(data=filter(rvals,
                           WetlandType=="Bog" & 
                             IS_Type=="Top 5 - High"), 
               stat="identity", alpha=0.5) + 
      geom_bar(data=filter(rvals,
                           WetlandType=="Bog" & 
                             IS_Type=="Full - High"), 
               stat="identity", alpha=0.5) +
      lims(x=c(0,0.8)) +
      labs(x=expression(paste("Model Accuracy (", R^2, ")")),
           y=NULL) +
      scale_fill_manual( labels=c("HDI","HDI-rapid"),
                         values=c("#009E73", "#D55E00")) +    
      theme_classic() +
      theme(legend.position="top",
            legend.title=element_blank(),
            legend.margin=margin(0,0,0,0),
            panel.grid = element_blank(),
            axis.text = element_text(color=1),

            strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
    
    r2_low_bog <- ggplot(filter(rvals, WetlandType=="Bog" & str_detect(IS_Type, "Low")), 
                         aes(x=Marginal_R2, y=response_var, fill=IS_Type)) + 
      geom_bar(data=filter(rvals,
                           WetlandType=="Bog" & IS_Type=="Top 5 - Low"), 
               stat="identity", alpha=0.5) + 
      geom_bar(data=filter(rvals,
                           WetlandType=="Bog" & IS_Type=="Full - Low"), 
               stat="identity", alpha=0.5) +
      lims(x=c(0,0.8)) +
      labs(x=expression(paste("Model Accuracy (", R^2, ")")),
           y=NULL) +
      scale_fill_manual( labels=c("LDI","LDI-rapid"),
                         values=c("#56B4E9", "#CC79A7")) +
      theme_classic() +
      theme(legend.position="top",
            legend.title=element_blank(),
            legend.margin=margin(0,0,0,0),
            panel.grid = element_blank(),
            axis.text = element_text(color=1),

            strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
  
  }
  
  # standardized reg coefficients - relationship strength 
  {
    
    est_high_bog <-ggplot(filter(est, WetlandType=="Bog" & str_detect(IS_Type, "High")), 
                           aes(x=estimate, y=response_var, color=IS_Type, fill=IS_Type)) + 
      geom_vline(xintercept=0) + 
      geom_errorbarh(aes(xmin=estimate-std.error, xmax=estimate+std.error),
                     position=position_dodge(width=0.1),
                     height=0) +
      geom_point(position=position_dodge(width=0.1), size=2, pch=21, color="white", fill="white") +
      geom_point(position=position_dodge(width=0.1), size=2, pch=21, alpha=0.5) +
      labs(x="Relationship Strength (Std. Reg. Coef.)",
           y=NULL) +
      scale_color_manual( labels=c("HDI","HDI-rapid"),
                          values=c("#009E73", "#D55E00")) +  
      scale_fill_manual( labels=c("HDI","HDI-rapid"),
                          values=c("#009E73", "#D55E00")) +  
      scale_x_continuous(breaks=c(-0.5, 0, 0.5), limits=c(-0.5, 0.65)) +
      theme_classic() +
      theme(legend.position="top",
            legend.title=element_blank(),
            panel.grid.major.y = element_line(color="grey90"),
            legend.margin=margin(0,0,0,0),
            panel.grid.minor=element_blank(), 
            panel.grid = element_blank(),
            axis.text = element_text(color=1),

            strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
    
    est_low_bog <- ggplot(filter(est, WetlandType=="Bog" & str_detect(IS_Type, "Low")), 
                          aes(x=estimate, y=response_var, color=IS_Type, fill=IS_Type)) + 
      geom_vline(xintercept=0) + 
      geom_errorbarh(aes(xmin=estimate-std.error, xmax=estimate+std.error),
                     position=position_dodge(width=0.1),
                     height=0) +
      geom_point(position=position_dodge(width=0.1), size=2, pch=21, color="white", fill="white") +
      geom_point(position=position_dodge(width=0.1), size=2, pch=21, alpha=0.5) +
      labs(x="Relationship Strength (Std. Reg. Coef.)",
           y=NULL) +
      scale_color_manual( labels=c("LDI","LDI-rapid"),
                          values=c("#56B4E9", "#CC79A7")) +
      scale_fill_manual( labels=c("LDI","LDI-rapid"),
                          values=c("#56B4E9", "#CC79A7")) +
      scale_x_continuous(breaks=c(-0.5, 0, 0.5), limits=c(-0.5, 0.65)) +
      theme_classic() +
      theme(legend.position="top",
            legend.title=element_blank(),
            panel.grid.major.y = element_line(color="grey90"),
            legend.margin=margin(0,0,0,0),
            panel.grid.minor=element_blank(), 
            panel.grid = element_blank(),
            axis.text = element_text(color=1),

            strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))

  }
  
  # combine r2 and est plots; export
  {
    # extract labels
    myleg1 <- get_legend(r2_high_bog)
    myleg2 <- get_legend(r2_low_bog)
    bothlegs <- plot_grid(myleg1, myleg2, ncol=2)
    
    r2_comparisons_bog <- plot_grid(r2_high_bog + 
                                      theme(axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    r2_low_bog + 
                                      theme(axis.title.x = element_blank(),
                                            legend.position = "none"),
                                    labels=c("a", "b"))
    # create common X-axis title
    x.grob <- textGrob(expression(paste("        Model Accuracy (", R^2, ")")), just = "bottom")
    #add to plot
    r2_comparisons_bog <- grid.arrange(r2_comparisons_bog, bottom = x.grob)
    
    
    est_comparisons_bog <- plot_grid(est_high_bog + theme(axis.title.x = element_blank(), 
                                                          legend.position = "none"),
                                     est_low_bog + theme(axis.title.x = element_blank(), 
                                                         legend.position="none"),
                                     labels = c("c", "d"))
    # create common X-axis title
    x.grob2 <- textGrob("        Relationship Strength (Std. Reg. Coef.)", just="bottom")
    #add to plot
    est_comparisons_bog <- grid.arrange(est_comparisons_bog, bottom = x.grob2)
    
    tmp_p <- plot_grid(r2_comparisons_bog,
                       est_comparisons_bog,
                       ncol=1)
    bog_ex_fig <- plot_grid(bothlegs, tmp_p, ncol=1, rel_heights = c(0.1,1))
    bog_ex_fig
    ggsave(bog_ex_fig,
           file="./results/fig 2 - full vs rapid model comps - bogs.jpg",
           height=14, width=12, units = "cm", dpi=300)
    
  }
  
}

# figure 3 - important results in bogs and fens ####
{
  # scale_color_manual(values=c("#7570b3", "#d95f02")) +
  # bogs
  {
    rich_bogs <-
      ggplot(filter(rich, WetlandType=="Bog"), 
             aes(x=HighDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
      # geom_smooth(method="lm", show.legend = F, color=1) +
      geom_jitter(width=0.3, alpha=0.5) +
      geom_smooth(method="lm", se=F) +
      lims( y=c(0,120)) +
      scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
      scale_shape_manual(values=c(1,2)) +
      scale_color_manual(values=c("#7570b3", "#d95f02")) +
      labs(x=expression(paste(HDI[rapid], " in Bogs (Num.)")),
           y="Total Rich.\n(Num.)") +
      theme_classic() +
      # facet_wrap(~WetlandType, scale="free") +
      theme(legend.position="none",
            legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color=1))
    
    
    culimp_bogs <- ggplot(filter(chars, WetlandType=="Bog"), 
                          aes(x=HighDistSp_N, y=CulImp_N, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
      geom_smooth(method="lm", se=F) +
      # geom_smooth(method="lm", show.legend = F, color=1) +
      geom_jitter(width=0.3, alpha=0.5) +
      lims( y=c(0,30)) +
      scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +scale_shape_manual(values=c(1, 17)) +  
      scale_color_manual(values=c("#7570b3")) +
      labs(x=expression(paste(HDI[rapid], " in Bogs (Num.)")),
           y="Cul. Imp. Sp.\n(Num.)") +
      theme_classic() +
      # facet_wrap(~WetlandType, scale="free") +
      theme(legend.position="none",
            legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.text=element_text(color=1))
    
    om_bogs <- ggplot(filter(chars, WetlandType=="Bog"), 
                      aes(x=HighDistSp_N, y=OMDepth_cm, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
      # geom_smooth(method="lm", show.legend = F, color=1) +
      geom_jitter(width=0.3, alpha=0.5) +
      geom_smooth(method="lm", se=F) +
      scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
      scale_shape_manual(values=c(1,2)) +
      scale_color_manual(values=c("#7570b3")) +
      labs(x=expression(paste(HDI[rapid], " in Bogs (Num.)")),
           y="OM Depth\n(cm)") +
      theme_classic() +
      # facet_wrap(~WetlandType, scale="free") +
      theme(legend.position="none",
            legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.text=element_text(color=1))
    
    bogfits <- plot_grid(rich_bogs + theme(axis.title.x=element_blank(),
                                           axis.text.x = element_blank()),
                         culimp_bogs + theme(axis.title.x=element_blank(),
                                             axis.text.x = element_blank()),
                         om_bogs , 
                         ncol=1, nrow=3,
                         labels=c("a", "c", "e"), align="v",
                         rel_heights = c(0.9,0.9,1)) 
  }
  
  # fens
  {
    rich_fens <-
      ggplot(filter(rich, WetlandType=="Fen"), 
             aes(x=HighDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
      # geom_smooth(method="lm", show.legend = F, color=1) +
      geom_jitter(width=0.3, alpha=0.5) +
      geom_smooth(method="lm", se=F) +
      lims( y=c(0,120)) +
      scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
      scale_shape_manual(values=c(1,2)) +
      scale_color_manual(values=c("#d95f02")) +
      labs(x=expression(paste(HDI[rapid], " in Fens (Num.)")),
           y="Total Rich.\n(Num.)") +
      theme_classic() +
      # facet_wrap(~WetlandType, scale="free") +
      theme(legend.position="none",
            legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color=1))
    
    
    culimp_fens <- ggplot(filter(chars, WetlandType=="Fen"), 
                          aes(x=HighDistSp_N, y=CulImp_N, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
      geom_smooth(method="lm", se=F) +
      # geom_smooth(method="lm", show.legend = F, color=1) +
      geom_jitter(width=0.3, alpha=0.5) +
      lims( y=c(0,30)) +
      scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +scale_shape_manual(values=c(1, 17)) +  
      scale_color_manual(values=c("#d95f02")) +
      labs(x=expression(paste(HDI[rapid], " in Fens (Num.)")),
           y="Cul. Imp. Sp.\n(Num.)") +
      theme_classic() +
      # facet_wrap(~WetlandType, scale="free") +
      theme(legend.position="none",
            legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.text=element_text(color=1))
    
    om_fens <- ggplot(filter(chars, WetlandType=="Fen"), 
                      aes(x=HighDistSp_N, y=OMDepth_cm, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
      # geom_smooth(method="lm", show.legend = F, color=1) +
      geom_jitter(width=0.3, alpha=0.5) +
      geom_smooth(method="lm", se=F) +
      scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
      scale_shape_manual(values=c(1,2)) +
      scale_color_manual(values=c("#d95f02")) +
      labs(x=expression(paste(HDI[rapid], " in Fens (Num.)")),
           y="OM Depth\n(cm)") +
      theme_classic() +
      # facet_wrap(~WetlandType, scale="free") +
      theme(legend.position="none",
            legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.text=element_text(color=1))
    
    fenfits <- plot_grid(rich_fens + theme(axis.title.x=element_blank(),
                                           axis.text.x = element_blank()),
                         culimp_fens + theme(axis.title.x=element_blank(),
                                             axis.text.x = element_blank()),
                         om_fens , 
                         ncol=1, nrow=3,
                         labels=c("b", "d", "f"), align="v",
                         rel_heights = c(0.9,0.9,1)) 
    

  }
  
  peatland_fits <- plot_grid(bogfits, fenfits, ncol=2, align="h")
  ggsave(peatland_fits,
         file="./results/fig3 - peatfit.jpg",
         dpi=300, width=12, height=12, units="cm")
  
}

# bogs
{
  rich_wms <-
    ggplot(filter(rich, WetlandType=="Wet Meadow"), 
           aes(x=LowDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
    # geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.3, alpha=0.5) +
    geom_smooth(method="lm", se=F) +
    lims( y=c(0,120)) +
    scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
    scale_shape_manual(values=c(1,2)) +
    scale_color_manual(values=c("#1b9e77")) +
    labs(x=expression(paste(LDI[rapid], " in WMs (Num.)")),
         y="Total Rich.\n(Num.)") +
    theme_classic() +
    # facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",
          legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(color=1))
  
  
  culimp_wms <- ggplot(filter(chars, WetlandType=="Wet Meadow"), 
                        aes(x=LowDistSp_N, y=CulImp_N, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
    geom_smooth(method="lm", se=F) +
    # geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.3, alpha=0.5) +
    lims( y=c(0,30)) +
    scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +scale_shape_manual(values=c(1, 17)) +  
    scale_color_manual(values=c("#1b9e77")) +
    labs(x=expression(paste(LDI[rapid], " in WMs (Num.)")),
         y="Cul. Imp. Sp.\n(Num.)") +
    theme_classic() +
    # facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",
          legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text=element_text(color=1))
  
  exot_wms <- ggplot(filter(rich, WetlandType=="Wet Meadow"), 
                    aes(x=LowDistSp_N, y=PropExot, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
    # geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.3, alpha=0.5) +
    geom_smooth(method="lm", se=F) +
    scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
    scale_shape_manual(values=c(1,2)) +
    scale_color_manual(values=c("#1b9e77")) +
    labs(x=expression(paste(LDI[rapid], " in WMs (Num.)")),
         y="Exotic Sp.\n(Proportion)") +
    theme_classic() +
    # facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",
          legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text=element_text(color=1))
  
  
  canopen_wms <- ggplot(filter(chars, WetlandType=="Wet Meadow"), 
         aes(x=LowDistSp_N, y=CanopyOpenness, shape=WetlandType, linetype=WetlandType, color=WetlandType)) +
    # geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.3, alpha=0.5) +
    geom_smooth(method="lm", se=F) +
    scale_x_continuous(breaks=0:5, limits = c(-0.3,5.3)) +
    scale_shape_manual(values=c(1,2)) +
    scale_color_manual(values=c("#1b9e77")) +
    labs(x=expression(paste(LDI[rapid], " in WMs (Num.)")),
         y="Can. Open.\nIndex (N/96)") +
    theme_classic() +
    # facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",
          legend.title=element_blank(),
          panel.grid = element_blank(),
          axis.text=element_text(color=1))
  
  wmfits <- plot_grid(rich_wms + theme(axis.title.x=element_blank(),
                                         axis.text.x = element_blank()),
                       culimp_wms + theme(axis.title.x=element_blank(),
                                           axis.text.x = element_blank()),
                      exot_wms ,
                      canopen_wms,
 
                       ncol=2, nrow=2,
                       labels="auto", align="v",
                       rel_heights = c(0.9,1)) 
  
  wmfits
  ggsave(wmfits,
         file="./results/fig4 - wm fits.jpg",
         dpi=300, width=12, height=9, units="cm")
}

# fig 4 - important results in marsh, sl, wm ####
{
  chars %>% 
    # filter(!is.na(TC_soil)) %>% 
    filter(WetlandType=="Marsh") %>% 
    select(LowDistSp_N, TC_soil) %>% View()
  
  ggplot(filter(chars, WetlandType=="Marsh"), 
         aes(x=LowDistSp_N, y=TC_soil , shape=WetlandType, linetype=WetlandType), color=1) +
    geom_smooth(method="lm", se=F, color=1) +
    geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.1, color="black", alpha=0.5) +
    # lims(x=c(0,5)) +
    scale_shape_manual(values=c(15:17)) +  
    labs(x=expression(paste(LDI[rapid], " in Marshes (Num.)")),
         y="Soil Carbon (% dry weight") +
    theme_bw() + 
    facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",panel.grid = element_blank())
  
  min_totrich <- ggplot(filter(rich, WetlandType!="Bog" & WetlandType!="Fen"), 
                             aes(x=LowDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType), color=1) +
    lims(x=c(0,5)) +
    scale_shape_manual(values=c(15:17)) +
    geom_smooth(method="lm", se=F, color=1) +
    geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.1, color="black", alpha=0.5) +
    labs(x=expression(paste(LDI[rapid], " Richness (Num.)")),
         y="Total Richness (Num.)") +
    theme_bw() + 
    facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",panel.grid = element_blank())
  
  fig3 <- plot_grid(min_culimp + 
              theme(axis.title.x = element_blank(),
                    axis.text.y = element_text(angle=90, hjust = .5)), 
            min_totrich +
              theme(axis.text.y = element_text(angle=90, hjust = .5)),
            ncol=1, nrow=2,
            align = "v",
            rel_heights = c(0.9,1))
  fig3
  
  ggsave(fig3,
         file="./manuscript/figures/fig3.jpg",
         dpi=300, width=15, height=10, units="cm")
  
}

# testing other plot design ####
{
  p1 <- ggplot(filter(chars, WetlandType!="Bog" & WetlandType!="Fen"), 
               aes(x=LowDistSp_N, y=CulImp_N, shape=WetlandType, linetype=WetlandType), color=1) +
    geom_smooth(method="lm", se=F, color=1) +
    geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.1, color="black", alpha=0.5) +
    lims(x=c(0,5)) +
    scale_shape_manual(values=c(15:17)) +  
    labs(x=expression(paste(LDI[rapid], " Richness (Num.)")),
         y="Cul. Important Sp. (Num.)") +
    theme_minimal() + 
    facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none", panel.grid.minor = element_blank())
  
  p2 <- ggplot(filter(rich, WetlandType!="Bog" & WetlandType!="Fen"), 
               aes(x=LowDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType), color=1) +
    lims(x=c(0,5)) +
    scale_shape_manual(values=c(15:17)) +
    geom_smooth(method="lm", se=F, color=1) +
    geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.1, color="black", alpha=0.5) +
    labs(x=expression(paste(LDI[rapid], " Richness (Num.)")),
         y="Total Richness (Num.)") +
    theme_minimal() + 
    facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none", panel.grid.minor = element_blank())
  
  testp <- plot_grid(p1 + theme(axis.title.x = element_blank(),
                                axis.text.y = element_text(angle=90, hjust = .5)), 
                     p2 +
                       theme(axis.text.y = element_text(angle=90, hjust = .5)),
                     ncol=1, nrow=2,
                     align = "v",
                     rel_heights = c(0.9,1))
  testp
  ggsave(testp,
         file="./manuscript/figures/format_test.jpg",
         dpi=300, width=15, height=10, units="cm")
  
}

# figure S1 - comps of relationship strength (i.e. standardized regression estimates) ####
{
  est_high <- ggplot(filter(est, str_detect(IS_Type, "High")), 
                     aes(x=estimate, y=response_var, color=IS_Type)) + 
    geom_vline(xintercept=0) + 
    geom_point(position=position_dodge(width=0.1), size=2) +
    geom_errorbarh(aes(xmin=estimate-std.error, xmax=estimate+std.error),
                   position=position_dodge(width=0.1),
                   height=0) +
    labs(x="Relationship Strength (Std. Coefficients)",
         y=NULL) +
    lims(x=c(-0.7, 0.9)) +
    scale_color_manual( labels=c("HDI","HDI-rapid"),
                        values=c("#009E73", "#D55E00")) +    
    facet_wrap(~WetlandType, scales="fixed", ncol=1) +
    theme_bw() +
    theme(legend.position="top",
          legend.title=element_blank(),
          panel.grid.major.y = element_line(color="grey90"),
          legend.margin=margin(0,0,0,0),
          panel.grid.minor=element_blank(), 
          panel.grid = element_blank(),
          axis.text = element_text(color=1),
          strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
  
  est_low <- ggplot(filter(est, str_detect(IS_Type, "Low")), 
                    aes(x=estimate, y=response_var, color=IS_Type)) + 
    geom_vline(xintercept=0) + 
    geom_point(position=position_dodge(width=0.1), size=2) +
    geom_errorbarh(aes(xmin=estimate-std.error, xmax=estimate+std.error),
                   position=position_dodge(width=0.1),
                   height=0) +
    labs(x="Relationship Strength (Std. Coefficients)",
         y=NULL) +
    lims(x=c(-0.7, 0.9)) +
    scale_color_manual( labels=c("LDI","LDI-rapid"),
                        values=c("#56B4E9", "#CC79A7")) +
    facet_wrap(~WetlandType, scales="fixed", ncol=1) +
    theme_bw() +
    theme(legend.position="top",
          legend.title=element_blank(),
          panel.grid.major.y = element_line(color="grey90"),
          legend.margin=margin(0,0,0,0),
          panel.grid.minor=element_blank(), 
          panel.grid = element_blank(),
          axis.text = element_text(color=1),
          strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
  
  est_comparisons <- plot_grid(est_high + theme(axis.title = element_blank()), 
                               est_low + theme(axis.text.y = element_blank(),
                                               axis.title = element_blank()),  
                               ncol=2, rel_widths = c(1.3,1))
  
  
  # create common X-axis title
  x.grob <- textGrob("    Relationship Strength (Standardized Reg. Coef.)", just = "bottom")
  #add to plot
  est_comparisons <- grid.arrange(est_comparisons, bottom = x.grob)
  
  # ggsave(est_comparisons,
  #        file="./results/est_comparisons.jpg",
  #        dpi=300,
  #        width = 12, height=20, units="cm")
  
  
}

# figure S2 - R2 comps ####
{
  # all wetland types
  r2_high <- ggplot(filter(rvals, str_detect(IS_Type, "High")), 
                    aes(x=Marginal_R2, y=response_var, fill=IS_Type)) + 
    geom_bar(data=filter(rvals,
                         IS_Type=="Top 5 - High"), 
             stat="identity", alpha=0.5) + 
    geom_bar(data=filter(rvals,
                         IS_Type=="Full - High"), 
             stat="identity", alpha=0.5) +
    lims(x=c(0,0.8)) +
    facet_wrap(~WetlandType, scales="fixed", ncol=1) +
    labs(x=expression(paste("Model Accuracy (", R^2, ")")),
         y=NULL) +
    scale_fill_manual( labels=c("HDI","HDI-rapid"),
                       values=c("#009E73", "#D55E00")) +    
    theme_bw() +
    theme(legend.position="top",
          legend.title=element_blank(),
          legend.margin=margin(0,0,0,0),
          panel.grid = element_blank(),
          axis.text = element_text(color=1),
          strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
  
  r2_low <- ggplot(filter(rvals, str_detect(IS_Type, "Low")), 
                   aes(x=Marginal_R2, y=response_var, fill=IS_Type)) + 
    geom_bar(data=filter(rvals,
                         IS_Type=="Top 5 - Low"), 
             stat="identity", alpha=0.5) + 
    geom_bar(data=filter(rvals,
                         IS_Type=="Full - Low"), 
             stat="identity", alpha=0.5) +
    lims(x=c(0,0.8)) +
    facet_wrap(~WetlandType, scales="fixed", ncol=1) +
    labs(x=expression(paste("Model Accuracy (", R^2, ")")),
         y=NULL) +
    scale_fill_manual( labels=c("LDI","LDI-rapid"),
                       values=c("#56B4E9", "#CC79A7")) +
    theme_bw() +
    theme(legend.position="top",
          legend.title=element_blank(),
          legend.margin=margin(0,0,0,0),
          panel.grid = element_blank(),
          axis.text = element_text(color=1),
          strip.text.x = element_text(margin = margin(0,0,0,0, "cm")))
  
  
  r2_comparisons <- plot_grid(r2_high + theme(axis.title = element_blank()), 
                              r2_low + theme(axis.text.y = element_blank(),
                                             axis.title = element_blank()), 
                              ncol=2, rel_widths = c(1.3,1))
  
  
  # create common X-axis title
  x.grob <- textGrob(expression(paste("       Model Accuracy (", R^2, ")")), just = "bottom")
  #add to plot
  r2_comparisons <- grid.arrange(r2_comparisons, bottom = x.grob)
  
  # ggsave(r2_comparisons,
  #        file="./results/r2_comparisons.jpg",
  #        dpi=300,
  #        width = 12, height=20, units="cm")
  
}



