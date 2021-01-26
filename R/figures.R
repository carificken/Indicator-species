rm(list=ls())

library(tidyverse)
library(cowplot)

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

chars$Year <- as.factor(chars$Year)
rich$Year <- as.factor(rich$Year)
chars_full$Year <- as.factor(chars_full$Year)
rich_full$Year <- as.factor(rich_full$Year)

# peatland regressions
{
peatland_culimp <- ggplot(filter(chars_full, WetlandType=="Bog" | WetlandType=="Fen"), 
                            aes(x=HighDistSp_N, y=CulImp_N, shape=WetlandType, linetype=WetlandType), color=1) +
  geom_smooth(method="lm", se=F, color=1) +
  geom_smooth(method="lm", show.legend = F, color=1) +
  geom_jitter(width=0.1, color="black", alpha=0.5) +
  scale_shape_manual(values=c(16, 17)) +  
    labs(x=expression(paste(IS[high], " (Num.)")),
         y="Cul. Important Sp. (Num.)") +
  theme_bw() + 
  facet_wrap(~WetlandType, scale="free") +
  theme(legend.position="none",panel.grid = element_blank())
peatland_culimp

peatland_totrich <- ggplot(filter(rich_full, WetlandType=="Bog" | WetlandType=="Fen"), 
                           aes(x=HighDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType), color=1) +
  geom_smooth(method="lm", se=F, color=1) +
  geom_smooth(method="lm", show.legend = F, color=1) +
  geom_jitter(width=0.1, color="black", alpha=0.5) +
  scale_shape_manual(values=c(16, 17)) +
  labs(x=expression(paste(IS[high], " (Num.)")),
       y="Total Richness (Num.)") +
  theme_bw() + 
  facet_wrap(~WetlandType, scale="free") +
  theme(legend.position="none",panel.grid = element_blank())
peatland_totrich

plot_grid(peatland_culimp + 
            theme(axis.title.x = element_blank(),
                  axis.text.y = element_text(angle=90, hjust = .5)), 
          peatland_totrich +
            theme(axis.text.y = element_text(angle=90, hjust = .5)),
          ncol=1, nrow=2,
          align = "v",
          rel_heights = c(0.9,1))
}


# marsh, sl, wm regressions
{
  min_culimp <- ggplot(filter(chars_full, WetlandType!="Bog" & WetlandType!="Fen"), 
         aes(x=LowDistSp_N, y=CulImp_N, shape=WetlandType, linetype=WetlandType), color=1) +
    geom_smooth(method="lm", se=F, color=1) +
    geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.1, color="black", alpha=0.5) +
    scale_shape_manual(values=c(15:17)) +  
    labs(x=expression(paste(IS[low], " (Num.)")),
         y="Cul. Important Sp. (Num.)") +
    theme_bw() + 
    facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",panel.grid = element_blank())
  
  min_totrich <- ggplot(filter(rich_full, WetlandType!="Bog" & WetlandType!="Fen"), 
                             aes(x=LowDistSp_N, y=TotRichness, shape=WetlandType, linetype=WetlandType), color=1) +
    scale_shape_manual(values=c(15:17)) +
    geom_smooth(method="lm", se=F, color=1) +
    geom_smooth(method="lm", show.legend = F, color=1) +
    geom_jitter(width=0.1, color="black", alpha=0.5) +
    labs(x=expression(paste(IS[low], " (Num.)")),
         y="Total Richness (Num.)") +
    theme_bw() + 
    facet_wrap(~WetlandType, scale="free") +
    theme(legend.position="none",panel.grid = element_blank())
  
  plot_grid(min_culimp + 
              theme(axis.title.x = element_blank(),
                    axis.text.y = element_text(angle=90, hjust = .5)), 
            min_totrich +
              theme(axis.text.y = element_text(angle=90, hjust = .5)),
            ncol=1, nrow=2,
            align = "v",
            rel_heights = c(0.9,1))
}
