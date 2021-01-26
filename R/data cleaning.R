library(tidyverse)

rm(list=ls())

# PA veg terr and wet sites ####
{
  # load plant data
  {
    # vascular plants - terrestrial sites
    {
      vascplant_pa <- read.csv("data/raw/A_T15_Vascular_Plants.csv", row.names = NULL, na.strings = c("NONE", "VNA", "SNI", "DNC"))
      names(vascplant_pa) <- colnames(vascplant_pa[,2:ncol(vascplant_pa)])
      vascplant_pa <- vascplant_pa[2:ncol(vascplant_pa)-1]

      vascplant_pa <- vascplant_pa %>% 
        select(Site="ABMI.Site",Year,Species="Scientific.Name")
      vascplant_pa <- filter(vascplant_pa, Year!=2017)

      vascplant_pa <- droplevels(vascplant_pa)
      vascplant_pa$Protocol <- "Terrestrial"
      vascplant_pa <- vascplant_pa %>% select(Protocol, everything())
      
      vascplant_pa <- vascplant_pa %>% 
        mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
               Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
               Site = str_replace(Site, pattern="-DH-", replacement = "-")) 
    } 
    
    # vascular plants - wetland sites 
    {
      wet_vascplant_pa <- read.csv("data/raw/A_W05_Vascular_Plants.csv", row.names = NULL, na.strings = c("NONE", "VNA", "SNI", "DNC"))
      
      # clean plant data set
      wet_vascplant_pa <- wet_vascplant_pa %>% select(Site=ABMI.Site,
                                                      Year=Year,
                                                      Species=Scientific.Name)
      wet_vascplant_pa$Protocol <- "Wetland"
      wet_vascplant_pa <- wet_vascplant_pa %>% select(Protocol, everything())
      
      wet_vascplant_pa <- wet_vascplant_pa %>% 
        mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
               Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
               Site = str_replace(Site, pattern="-DH-", replacement = "-")) 
      
    }
    
    # combine both veg datasets
    {
      plant_pa <- bind_rows(vascplant_pa,wet_vascplant_pa)
      
      # round every taxa to sp; keep only taxa ID'd to species
      plant_pa$Species <- plant_pa$Species %>% word(start=1, end=2)
      
      # make site x species matrix
      # first create a full sitexspecies matrix for moss dataset
      plant_pa$PA <- 1
      
      # remove species that are duplicated b/c they were present in multiple zones, or b/c there taxa ID'd past species
      plant_pa <- plant_pa %>% distinct()
      plant_pa <- plant_pa %>% filter(!is.na(Species))
      
      # exclude singletons
      sptokeep <- plant_pa %>% group_by(Species) %>% tally() %>% filter(n>1) %>% select(Species)
      plant_pa <- plant_pa %>% filter(Species %in% sptokeep$Species)    
      
    }
    
    # load and clean wetland classification - terrestrial sites
    {
      siteclass <- read.csv("data/raw/A_T01C_Site_Capability.csv", na.strings = c("DNC", "PNA", "VNA"))
      # remove missing classification values, and only retain the dominant site classification
      # retain site classification at location point count station 1 (out of 1-9); this was where veg was sampled
      siteclass <- siteclass %>% filter(Point.Count.Station ==1)
      siteclass <- droplevels(siteclass)
      
      # separate sites by wetland type
      bogsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(08) PD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      poorfensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(09) MD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      richfensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(10) RDp" ) %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      wetmeadowsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(10.5) RDm") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      marshsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(11) VD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      swampsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(12) SD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      alkalifensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(13) AD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
      
      # add wetland type ID and combine datasets
      bogsites$WetlandType <- "Bog"
      poorfensites$WetlandType <- "Fen"
      richfensites$WetlandType <- "Fen"
      alkalifensites$WetlandType <- "Fen"
      wetmeadowsites$WetlandType <- "Wet Meadow"
      marshsites$WetlandType <- "Marsh"
      swampsites$WetlandType <- "Swamp"
      
      wetlandclassification <- rbind(bogsites,
                                     poorfensites,
                                     richfensites,
                                     alkalifensites,
                                     wetmeadowsites,
                                     marshsites,
                                     swampsites)
      
      # how many repeated site classifications are there
      head(wetlandclassification)
      unique(wetlandclassification$Site) %>% View()
      
      wetlandclassification <- wetlandclassification %>% 
        mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
               Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
               Site = str_replace(Site, pattern="-DH-", replacement = "-"),
               Site = str_replace(Site, pattern="-SRD-", replacement = "-")) 
      
      wetlandclassification <- wetlandclassification %>% 
        group_by(Site, WetlandType) %>% 
        summarize(meanCover=mean(Cover, na.rm=T))
      
      wetlandclassification %>% 
        distinct(Site, WetlandType) %>% 
        group_by(Site) %>%
        tally() %>% 
        filter(n>1) %>% 
        arrange(desc(n)) # 39 of 319 have multiple classifications
        
      # take the DOMINANT classification (dom = highest cover)
      wetlandclassification <- wetlandclassification %>% 
        group_by(Site) %>% 
        filter(meanCover==max(meanCover)) %>% 
        select(Site, WetlandType)
      # we still have 7 sites w/ 50:50 split classifications
      wetlandclassification %>% 
        distinct(Site, WetlandType) %>% 
        group_by(Site) %>%
        tally() %>% 
        filter(n>1) %>% 
        arrange(desc(n)) 
      
      # amend manually
      wetlandclassification[wetlandclassification$Site=="1417","WetlandType"] <- "Wet Meadow"
      wetlandclassification[wetlandclassification$Site=="323","WetlandType"] <- "Fen"
      wetlandclassification[wetlandclassification$Site=="327","WetlandType"] <- "Fen"
      wetlandclassification[wetlandclassification$Site=="498","WetlandType"] <- "Fen"
      wetlandclassification[wetlandclassification$Site=="544","WetlandType"] <- "Fen"
      wetlandclassification[wetlandclassification$Site=="561","WetlandType"] <- "Fen"
      wetlandclassification[wetlandclassification$Site=="699","WetlandType"] <- "Fen"
      
      # delete repeated Site classifications
      wetlandclassification <- wetlandclassification %>% distinct(Site, WetlandType)
      
      # check to make sure each site has only 1 wetland classification
      wetlandclassification %>% select(Site) %>% unique() %>% nrow() # 277 unique sites
      wetlandclassification %>% select(Site, WetlandType) %>% unique() %>% nrow() # 277 unique site x wetlandtypes
      wetlandclassification$Protocol <- "Terrestrial"
    }
    
    # load and clean wetland classification - wetland sites
    {
      siteclass_wet <- read.csv("data/raw/A_W02B_Site_Capability.csv", na.strings=c("DNC", "NONE", "VNA", "PNA"))
      # retain the dominant site classification
      siteclass_wet <- droplevels(siteclass_wet)

      # separate sites by wetland type
      bogsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(08) PD") %>% select(Site="ABMI.Site")
      poorfensites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(09) MD") %>% select(Site="ABMI.Site")
      richfensites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(10) RDp" ) %>% select(Site="ABMI.Site")
      wetmeadowsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(10.5) RDm") %>% select(Site="ABMI.Site")
      marshsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(11) VD") %>% select(Site="ABMI.Site")
      swampsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(12) SD") %>% select(Site="ABMI.Site")
      alkalifensites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(13) AD") %>% select(Site="ABMI.Site")
      
      # add wetland type ID and combine datasets
      bogsites_wetland$WetlandType <- "Bog"
      poorfensites_wetland$WetlandType <- "Fen"
      richfensites_wetland$WetlandType <- "Fen"
      alkalifensites_wetland$WetlandType <- "Fen"
      wetmeadowsites_wetland$WetlandType <- "Wet Meadow"
      marshsites_wetland$WetlandType <- "Marsh"
      swampsites_wetland$WetlandType <- "Swamp"
      
      wetlandclassification_wetland <- rbind(bogsites_wetland,
                                             poorfensites_wetland,
                                             richfensites_wetland,
                                             alkalifensites_wetland,
                                             wetmeadowsites_wetland,
                                             marshsites_wetland,
                                             swampsites_wetland)
      
      wetlandclassification_wetland <- wetlandclassification_wetland %>% 
        mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
               Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
               Site = str_replace(Site, pattern="-DH-", replacement = "-"),
               Site = str_replace(Site, pattern="-SRD-", replacement = "-")) 
      
      
      # # extract proper site names add "wet" before site id
      # wetlandclassification_wetland$Site <- paste("wet_",wetlandclassification_wetland$Site, sep="")
      # wetlandclassification_wetland$Site <- wetlandclassification_wetland$Site %>% str_replace(pattern="OG-", replace="OGW-") 
      
      # MANY sites have repeated site classifications; 
      wetlandclassification_wetland %>% 
        distinct(Site, WetlandType) %>% 
        group_by(Site) %>% tally() %>% filter(n>1) %>% arrange(desc(n))
      
      # note: must take the DOMINANT classification (dominant = the mode or the most common)
      wetlandclassification_wetland <- wetlandclassification_wetland %>% 
        group_by(Site,WetlandType) %>% 
        tally() %>% 
        ungroup() %>% 
        group_by(Site) %>% 
        filter(n==max(n)) %>% 
        select(-n)
      
      # we still have 34 sites w/ double split classifications; amend manually
      wetlandclassification_wetland %>% 
        distinct(Site, WetlandType) %>% 
        group_by(Site) %>% tally() %>% filter(n>1) %>% arrange(desc(n)) -> tmp
      tmp$Site
      
      # amend manually
      {
        # marsh  + wm >> wm
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="956","WetlandType"]  <- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1057","WetlandType"] <- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1333","WetlandType"] <- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1363","WetlandType"] <- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1370","WetlandType"] <- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1429","WetlandType"] <- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1435","WetlandType"]<- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1651","WetlandType"]<- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="826","WetlandType"]<- "Wet Meadow"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="OGW-511-21","WetlandType"] <- "Wet Meadow"
        
        # bog/fen/swamp + marsh >> marsh
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="154","WetlandType"] <- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="157","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="1570","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="20","WetlandType"] <- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="207","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="210","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="237","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="247","WetlandType"]      <- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="362","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="416","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="467","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="553","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="851","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="899","WetlandType"]<- "Marsh"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="OGW-469-1","WetlandType"]<- "Marsh"
        
        # bog + fen >> fen
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="16","WetlandType"] <- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="295","WetlandType"]<- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="298","WetlandType"]<- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="417","WetlandType"]<- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="560","WetlandType"]<- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="621","WetlandType"]      <- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="726","WetlandType"]<- "Fen"
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="842","WetlandType"]<- "Fen"
        
        # swamp + wm >> wm
        wetlandclassification_wetland[wetlandclassification_wetland$Site=="924","WetlandType"]<- "Wet Meadow"
        
        
      }

      # delete repeated Site classificaitons
      wetlandclassification_wetland <- wetlandclassification_wetland %>% distinct()
      
      # check to make sure each site has only 1 wetland classification
      wetlandclassification_wetland %>% select(Site) %>% unique() %>% nrow() # 868 unique sites
      wetlandclassification_wetland %>% select(Site, WetlandType) %>% unique() %>% nrow() # 868 unique site x wetlandtypes
      wetlandclassification_wetland$Protocol <- "Wetland"
    }
    
    # combine wetland classifications
    wetlandclassification_all <- bind_rows(wetlandclassification, wetlandclassification_wetland)
    wetlandclassification_all %>% distinct(Site) %>% dim()
    
    # OG wetlands from wetland protocol named as "OGW"
    wetlandclassification_all %>% 
      filter(Protocol=="Wetland") %>% 
      distinct(Site) %>% 
      filter(str_detect(Site, pattern="OG")) # OGW
    
    # OG wetlands from wetland protocol on vascular plant df labeled only with OG
    plant_pa %>% 
      filter(Protocol=="Wetland") %>% 
      distinct(Site) %>% 
      filter(str_detect(Site, pattern="OG")) # OGW-ABMI
    
    # # must change naming in site classification
    wetlandclassification_all <- wetlandclassification_all %>%
      mutate(Site = str_replace(string=Site, pattern="OGW", replacement="OG"))
    
    # add wetland class to plant veg df
    {
      plant_pa <- left_join(plant_pa, wetlandclassification_all, by=c("Protocol", "Site")) %>% 
        select(Protocol, WetlandType,Site,Year,everything())
      tmp <- filter(plant_pa, is.na(WetlandType) & Protocol=="Wetland") # wetland sites w/o classification are SOWWs
      
      unique(tmp$Site) # "shallow lake" isnt a classification option, and really all these have open water wetlands.
      
      tmp$WetlandType <- "Shallow Lake"
      plant_pa <- plant_pa %>% filter(!is.na(WetlandType)) # excludes wetland and terrestrial sites without classification
      # exclude species which only occur 1x
      plant_pa <- plant_pa %>% group_by(Species) %>% mutate(n=sum(PA)) %>% filter(n>1) %>% select(-n) %>% ungroup()
      
      plant_pa2 <- bind_rows(plant_pa,tmp) # adds SOWWs back to dataset
      plant_pa2 <- plant_pa2 %>% group_by(Species) %>% mutate(n=sum(PA)) %>% filter(n>1) %>% select(-n) %>% ungroup()
    }
    
    veg_pa <- plant_pa2 # plant_pa excludes SOWWs; plant_pa2 includes SOWWs
    veg_pa %>% ungroup() %>% distinct(Protocol,WetlandType,Site) %>% dim() #1303 unique sites; 1675 site x yrs
    
    #  load natural region site designation and add to veg_pa
    {
      nr <- read.csv("data/raw/Terrestrial_Wetland_Sites_all_NRs.csv", )
      nr %>% distinct(Protocol, ABMI.Site) %>% tail(50) %>% data.frame() # note: must exclude the "W-" in wetland sites
      nr$ABMI.Site <- nr$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
      
      veg_pa$tmpSiteMatch <- paste(str_extract(string=veg_pa$Site, pattern="OG-"), str_extract(string=veg_pa$Site, pattern="\\d+"), sep="") 
      veg_pa$tmpSiteMatch <- veg_pa$tmpSiteMatch %>% str_remove(pattern="NA")
      
      veg_pa <- left_join(veg_pa, select(nr, Protocol, ABMI.Site, NSRNAME, NRNAME), by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch) %>% distinct()
      
      veg_pa %>% filter(is.na(NRNAME)) %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) #19 sites w/o NR
      
      # veg_pa <- veg_pa %>% filter(WetlandType!="Alkali Fen") %>% droplevels()
      veg_pa <- veg_pa %>% filter(!is.na(NRNAME)) %>% droplevels()
      }
  }
  
  # keep taxa id'd to species
  {
    veg_pa$Species <- veg_pa$Species %>% word(start=1, end=2)
    veg_pa$Species <- veg_pa$Species %>% recode("Betula X" = "Betula X sargentii")
    veg_pa <- veg_pa %>% filter(!is.na(Species))
    # veg_pa %>% filter(Species=="Hesperostipa comata ssp. comata")
    
  }
  
  veg_pa <- veg_pa %>% select(NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
  

  # add the SOWW designations to the wetland classification df to pair with other future dfs
  soww <- distinct(tmp, Site, WetlandType, Protocol)# tmp is from above
  wetlandclassification_all <- wetlandclassification_all %>% 
    select(Protocol, WetlandType, Site)
  
  wetlandclassification_all <- bind_rows(wetlandclassification_all, soww) 
  
  # add in nativity status
  exot <- read.csv("data/raw/exotic_plants_ab.csv", sep=";")
  veg_pa <- left_join(veg_pa, 
            select(exot, -TYPE),
            by=c("Species" = "SPECIES"))
  
  # fix some species
  veg_pa <- veg_pa %>% 
    mutate(Species = case_when(Species=="Pinus X" ~ "Pinus X murraybanksiana",
                                TRUE ~ as.character(Species)),
           ORIGIN = case_when(Species=="Pinus X murraybanksiana" ~ "Native",
                               Species=="Polygonum aviculare" ~ "Exotic",
                               Species=="Amaranthus blitoides" ~ "Exotic",
                               TRUE ~ as.character(ORIGIN)))
}

# abundance (terr sites only) ####
{
  abund <- read.csv("data/raw/A_T13_%_Cover_Low_Vegetation_Species.csv", row.names = NULL, na.strings=c("VNA","DNC","PNA","NONE", "SNI"))
  abund <- abund %>% 
    select(Site="ABMI.Site",Plot="Plot.Number",Year,Species="Scientific.Name", Cover="Total.Cover..Percent.")
  # remove missing values and convert to numeric
  abund <- abund %>% 
     mutate(Cover = case_when(Cover == "<1" ~ "0.5",
                             TRUE ~ as.character(Cover)),
           Cover = as.numeric(as.character(Cover))) 

  # round every taxa to sp; keep only taxa ID'd to species
  abund$Species <- abund$Species %>% word(start=1, end=2)
  
  abund <- abund %>% 
    mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
           Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
           Site = str_replace(Site, pattern="-DH-", replacement = "-"),
           Site = str_replace(Site, pattern="-SRD-", replacement = "-")) 
    
  # add wetland classification (terr protocol only)
  abund <- left_join(abund, select(wetlandclassification, -Protocol), by="Site") %>% filter(!is.na(Species))
  
  abund %>% 
    distinct(Site, WetlandType) %>% 
    group_by(WetlandType) %>% tally() # NAs are likely terrestrial sites
  
  abund %>% 
    filter(!is.na(WetlandType)) %>% 
    distinct(Site, Year) %>% 
    tally() # 125 sites with abundance; only 66 peatlands though
  
  abund <- abund %>% filter(!is.na(WetlandType))
  
  # add protocol type 
  abund$Protocol <- "Terrestrial"
  
  # calculate plot-level total and rel abund
  abund <- abund %>% 
    group_by(Protocol, WetlandType, Site, Year, Plot) %>% 
    mutate(TotalCover = sum(Cover),
           RelAbund = Cover/TotalCover) %>% 
    group_by(Protocol, WetlandType, Site, Year, Species) %>% 
    summarize(TotalSiteCover = mean(Cover),
              RelAbund = mean(RelAbund)) 
  
  # 41 site x yr with abund
  abund %>% distinct(WetlandType, Site, Year) %>% nrow()
}

# top 5 indicator sp list ####
{
  is <- read.csv("data/clean/STOTEN ISA results top 5.csv")
  
  # Cluster 1 is highest disturbance grouping; highest cluster num (differs by wetland class) is lowest
  is <- is %>% 
    mutate(Disturbance = case_when(Cluster=="Cluster1" ~ "High",
                                   WetlandType=="Bog" & Cluster == "Cluster4" ~ "Low",
                                   WetlandType=="Fen" & Cluster == "Cluster2" ~ "Low",
                                   WetlandType=="Marsh" & Cluster == "Cluster3" ~ "Low",
                                   WetlandType=="Shallow Lake" & Cluster == "Cluster4" ~ "Low",
                                   WetlandType=="Wet Meadow" & Cluster == "Cluster3" ~ "Low",
                                   
                                   TRUE ~ NA_character_))
  
  is <- is %>% filter(!is.na(Disturbance))
  
}

# other biotic characteristics ####
{
  # canopy openness
  canopy <- read.csv("data/raw/A_T11_Canopy_Closure.csv", na.strings = c("DNC", "VNA", "PNA", "NONE")) %>% 
    select("Site" = ABMI.Site, 
           Year, 
           "Transect" = Sub.ordinal.transect, 
           "SamplePosition" = Canopy.Cover.Location,
           "CanopyOpenness" = Canopy.Closure..Open.dots.    )
  canopy$Protocol <- "Terrestrial"
  canopy <- canopy %>% 
    group_by(Protocol, Site, Year) %>% 
    summarize(CanopyOpenness = mean(CanopyOpenness))
  canopy <- canopy %>% 
    mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
           Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
           Site = str_replace(Site, pattern="-DH-", replacement = "-"),
           Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
  
  
  # culturally important species
  # sp list taken from Dyck and Geribaldi report to AEP
  culimp <- read.csv("data/raw/Culturally Important Species.csv", fileEncoding = "UTF-8-BOM") %>% select(-Family, -CommonName)
  culimp$CulImp <- "Yes"

  # PA
  culimp_pa <- left_join(veg_pa, culimp, by=c("Species")) 

  # add equisetum in manually
  culimp_pa <- culimp_pa %>% 
    mutate(Type=case_when(str_detect(Species, "Equisetum")~"Household",
                          TRUE~as.character(Type)),
           CulImp = case_when(str_detect(Species, "Equisetum")~"Yes",
                              TRUE ~ as.character(CulImp)))
  culimp_rich <- culimp_pa %>% 
    filter(CulImp=="Yes") %>% 
    group_by(Protocol, WetlandType, Site, Year) %>% 
    tally() %>% 
    select(everything(), 
           "CulImp_N" = n)

  # ALL veg_pa sites should have culimp value; add back in the sites with 0 cul imp sp
  culimp_rich <- left_join(distinct(veg_pa, Protocol, WetlandType, Site, Year), 
            culimp_rich,
            by=c("Protocol", "WetlandType", "Site", "Year") ) %>% 
    replace_na(., list(CulImp_N = 0)) 
  
  # abundance
  culimp_abund <- left_join(abund, culimp, by=c("Species")) 
  # add equisetum manually
  culimp_abund <- culimp_abund %>% 
    mutate(Type=case_when(str_detect(Species, "Equisetum")~"Household",
                          TRUE~as.character(Type)),
           CulImp = case_when(str_detect(Species, "Equisetum")~"Yes",
                              TRUE ~ as.character(CulImp)))
  
  culimp_abund <- culimp_abund %>%
    filter(CulImp == "Yes") %>% 
    group_by(Protocol, WetlandType, Site, Year) %>% 
    summarize(CulImp_RelAbund = sum(RelAbund)) 
  
  culimp_div <- full_join(culimp_abund, culimp_rich, by=c("Protocol", "WetlandType", "Site", "Year"))
}

# abiotic characteristics ####
{
  # org matter depth & buried wood depth
  om <- read.csv("data/raw/A_T02A_Surface_Substrate.csv", na.strings=c("VNA", "DNC", "PNA", "NONE")) %>% 
    select("Site" = ABMI.Site, 
           Year, 
           "Transect" = Transect..North.South., 
           "SamplePosition" = Sample.Position..metres.,
           "OMDepth_cm" = Organic.Depth..centimetres.,
           "BuriedWoodDepth_cm" = Buried.Wood.Depth..centimetres.)
  om <- om %>% 
    mutate(OMDepth_cm = case_when(OMDepth_cm == "<1" ~ "0.5",
                                  TRUE ~ as.character(OMDepth_cm)),
           OMDepth_cm = as.numeric(as.character(OMDepth_cm)))

  om$Protocol <- "Terrestrial"  
  om <- om %>% 
    filter(!is.na(OMDepth_cm) | !is.na(BuriedWoodDepth_cm)) # remove rows where BOTH are NA
  om <- om %>% 
    group_by(Protocol, Site, Year) %>% 
    summarize(OMDepth_cm = mean(OMDepth_cm, na.rm=T),
              BuriedWoodDepth_cm = mean(BuriedWoodDepth_cm, na.rm = T))
  om <- om %>% 
    mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
           Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
           Site = str_replace(Site, pattern="-DH-", replacement = "-"),
           Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
  
  # soil carbon and pH
  soil <- read.csv("data/raw/A_T25_Mineral_Soil.csv", na.strings=c("VNA", "DNC", "PNA", "NONE")) %>% 
    select(RowNum,
           "Site" = ABMI.Site, 
           Year, 
           Quadrant, 
           "TC_pdw" = Total.Carbon..Percent.of.Dry.Weight.,
           "pH" = Soil.Acidity..pH. )
  soil$Protocol <- "Terrestrial"
  soil <- soil %>% 
    mutate(TC_pdw = case_when(TC_pdw == "<0.04" ~ "0.02",

                              TRUE ~ as.character(TC_pdw)),
           TC_pdw = as.numeric(as.character(TC_pdw))) 
  soil <- soil %>% 
    filter(!is.na(TC_pdw) | !is.na(pH)) # remove rows where BOTH are NA
  soil <- soil %>% 
    group_by(Protocol, Site, Year) %>% 
    summarize(TC_soil = mean(TC_pdw, na.rm = T),
              pH_soil = mean(pH, na.rm=T))
  
  soil <- soil %>% 
    mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
           Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
           Site = str_replace(Site, pattern="-DH-", replacement = "-"),
           Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
  
  # water physicochem
  # specifying NA strings during load means they load in as numeric vars
  water <- read.csv("data/raw/A_W04_Water_Physiochemistry.csv", 
                    na.strings = c("DNC", "VNA")) %>% 
    select("Site" = ABMI.Site, 
           Year, 
           "SampleLocation" = Location, 
           "SampleDepth_m" = Depth..metres.,
           "pH_water" = pH,
           "DO_water" = Dissolved.Oxygen..milligrams.Litre.,
           "EC_water" = Conductivity..mSiemens.cm.,
           "Salinity_water" = Salinity..parts.per.thousand.,
           "TN_water" = Total.Nitrogen..micrograms.Litre.,
           "TP_water" = Total.Phosphorous..micrograms.Litre.,
           "DOC_water" = Dissolved.Organic.Carbon..milligrams.Litre.)  
  
  water <- water %>% 
    gather(key="Var", value="Val", 5:ncol(.)) %>% 
    group_by(Site, Year, Var) %>% 
    summarize(meanVal = mean(Val, na.rm=T)) %>% 
    spread(., key=Var, value=meanVal) 
  
  water$Protocol <- "Wetland"
  water <- water %>% 
    mutate(Site = str_replace(Site, "OGW", "OG"))
  
  water <- water %>% 
    mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-"),
           Site = str_replace(Site, pattern="-ALPAC-", replacement = "-"),
           Site = str_replace(Site, pattern="-DH-", replacement = "-"),
           Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
}

# combine various files for wetland characteristics ####
{
  chars <- full_join(canopy, culimp_div, by=c("Protocol", "Site", "Year")) %>% select(-WetlandType) 
  chars <- full_join(chars, om, by=c("Protocol", "Site", "Year"))
  chars <- full_join(chars, soil, by=c("Protocol", "Site", "Year"))
  chars <- full_join(chars, water, by=c("Protocol", "Site", "Year"))
  
  # add wetland classification
  chars <- left_join(chars, wetlandclassification_all,
                     by=c("Protocol", "Site")) %>% 
    select(Protocol, WetlandType, Site, Year, everything())
  
  chars %>% 
    distinct(Protocol, Site, Year, WetlandType) %>% 
    group_by(Protocol, Site, Year) %>% 
    tally() %>% 
    arrange(desc(n)) # each site has 1 wetland classification
  
  head(chars)

  
  # 2 wetland sites w/o classification; 1222 terr sites w/o classification
  chars %>% 
    filter(is.na(WetlandType))
  
  chars %>% 
    filter(is.na(WetlandType)) %>% 
    ungroup() %>% 
    distinct(Protocol, Site, Year) %>% 
    group_by(Protocol) %>% 
    tally() 
  
  # 453 ter + 1299 wetland sites
  chars %>% 
    filter(!is.na(WetlandType)) %>% 
    ungroup() %>% 
    distinct(Protocol, Site, Year) %>% 
    group_by(Protocol) %>%
    tally() 
  
  chars <- chars %>% 
    filter(!is.na(WetlandType)) # remove the sites w/o wetland classification (these are mostly uplands)

}

# create df with each site's disturbance status - top 5 IS ####
{
  
  # 1. num of high/low dist sp
  sitediststatus <- left_join(veg_pa, is, by=c("WetlandType", "Species")) %>% 
    select(-Cluster, -IV, -p.value, -Fidelity, -Sensitivity, -ORIGIN)
  
  sitediststatus <- sitediststatus %>% 
    filter(!is.na(Disturbance)) %>% 
    group_by(Protocol, WetlandType, Site, Year, Disturbance) %>% 
    tally() 
  
  sitediststatus$Disturbance <- recode(sitediststatus$Disturbance, High="HighDistSp_N", Low = "LowDistSp_N")
  sitediststatus <- sitediststatus %>% 
    spread(key=Disturbance, value=n) %>% 
    replace_na(list(HighDistSp_N = 0, LowDistSp_N=0))

  # a number of sites have 0 high dist and 0 low dist species. these were dropped from df so must re-add them.
  sitediststatus <- left_join(distinct(veg_pa, Protocol, WetlandType, Site, Year), 
            sitediststatus, by=c("Protocol", "WetlandType", "Site", "Year")) 
  sitediststatus <- sitediststatus %>% 
    replace_na(., list(HighDistSp_N=0, LowDistSp_N=0))
  
  # 2. abundance of high/low dist sp
  tmp <- left_join(abund, is, by=c("WetlandType", "Species")) %>% 
    select(-Cluster, -IV, -p.value, -Fidelity, -Sensitivity) %>% 
    filter(!is.na(Disturbance)) %>% 
    group_by(Protocol, WetlandType, Site, Year, Disturbance) %>% 
    summarize(TotalRelAbund = sum(RelAbund))
  
  # NO low dist sp in the appropriate wetland classes sampled under terr protocol
  tmp %>% filter(Disturbance=="Low")
  
  tmp$Disturbance <- recode(tmp$Disturbance, High="HighDistSp_Abund", 
                            Low = "LowDistSp_Abund")
  tmp <- tmp %>% 
    spread(key=Disturbance, value=TotalRelAbund) %>% 
    replace_na(list(HighDistSp_Abund = 0, LowDistSp_Abund=0))

  # note: not all sites were sampled for both abund and PA
  sitediststatus <- full_join(sitediststatus, tmp, by=c("Protocol", "WetlandType", "Site", "Year"))
  
  sitediststatus %>% 
    filter(!is.na(HighDistSp_Abund)) %>% 
    filter(is.na(HighDistSp_N)) # no sites where there is abundance but not PA
  
}

abund # Rel abundance (0-1.00) of species at terrestrial protocol sites only (few peatlands)
veg_pa # presence/absence of sp at wetland + terrestrial protocol sites

is # list of top 5 indicator species (highest IV) and their associations with high vs low dist

sitediststatus # richness and cover of (all) high and low dist sp at each site

chars # env and wetland characteristics  


# num of sites in each df ####
{
  abund %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 41 sites with abundance
  
  veg_pa %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 1656 sites with veg pa
  
  sitediststatus %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 1656 sites with some species on IS list; 1650
  
  chars %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 1752 sites with various env characteristics
}

# check overlapping coverage w/ site dist status ####
{
  anti_join(sitediststatus, veg_pa, 
            by=c("Protocol", "WetlandType", "Site", "Year")) %>% 
    nrow() # all sites with dist status also have veg_pa
  
  anti_join(sitediststatus, chars, 
            by=c("Protocol", "WetlandType", "Site", "Year")) %>% 
    nrow() # all sites with dist status also have chars
  
  anti_join(chars, sitediststatus, 
            by=c("Protocol", "WetlandType", "Site", "Year")) %>% 
    nrow() # 96 rows in chars have no match in sitediststatus 
  
  semi_join( sitediststatus, chars,
             by=c("Protocol", "WetlandType", "Site", "Year")) %>% 
    nrow() # 1656 rows in sitediststatus have a match in chars 

  }

# join dfs for export - dist status based on top 5 IS only ####
{
  # this file has the site-level disturbance weights, plus the wetland and env characteristics
  chars1 <- inner_join(sitediststatus, chars, by=c("Protocol", "WetlandType", "Site", "Year"))
  write.csv(x=chars1,
            file = "data/clean/Site Dist Status - Wet and Env Chars - top 5 IS.csv", row.names = F)
  chars1 %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 1656
  
  # this file has site-level disturbance weights plus veg Presence-Absence
  veg_rich <- inner_join(sitediststatus, veg_pa, by=c("Protocol", "WetlandType", "Site", "Year"))
  write.csv(x=veg_rich,
            file = "data/clean/Site Dist Status - Veg PA - top 5 IS.csv", row.names = F)
  veg_rich %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 1656
  
  # this file has site-level disturbance weights plus veg abundance
  veg_abund <- inner_join(sitediststatus, abund, by=c("Protocol", "WetlandType", "Site", "Year"))
  write.csv(x=veg_abund,
            file = "data/clean/Site Dist Status - Veg Abund - top 5 IS.csv", row.names = F)
  veg_abund %>% 
    distinct(Protocol, WetlandType, Site, Year) %>% 
    nrow() # 41
}

# calculate impactedness and naturalness indices for each site ####
{
  # II and NI calculated based on the relative abundance of each ISA sp
  # 164 sites with abundance; only terrestrial protocol
  # this can really only work with marsh and maybe wet meadows
  
  # firt re-load abund b/c need total abundance, not relabund
  {
    rawabund <- read.csv("data/raw/A_T13_%_Cover_Low_Vegetation_Species.csv", row.names = NULL)
    rawabund <- rawabund %>% 
      select(Site="ABMI.Site",Plot="Plot.Number",Year,Species="Scientific.Name", Cover="Total.Cover..Percent.")
    # remove missing values and convert to numeric
    rawabund <- rawabund %>% 
      filter(Cover!="VNA" &
               Cover!="DNC" &
               Cover!="PNA") %>% 
      mutate(Cover = case_when(Cover == "<1" ~ "0.5",
                               TRUE ~ as.character(Cover)),
             Cover = as.numeric(as.character(Cover))) 
    # remove missing species values
    rawabund <- rawabund %>% 
      filter(Species!="NONE" &
               Species!="SNI" &
               Species!="VNA" &
               Species!="DNC") 
    # round every taxa to sp; keep only taxa ID'd to species
    rawabund$Species <- rawabund$Species %>% word(start=1, end=2)
    
    # add wetland classification (terr protocol only)
    rawabund <- left_join(rawabund, select(wetlandclassification, -Protocol), by="Site") 
    
    rawabund %>% 
      distinct(Site, WetlandType) %>% 
      group_by(WetlandType) %>% tally() # NAs are likely terrestrial sites
    
    rawabund %>% 
      filter(!is.na(WetlandType)) %>% 
      distinct(Site, Year) %>% 
      tally() # 164 sites with abundance; only 5 peatlands though
    
    rawabund <- rawabund %>% filter(!is.na(WetlandType))
    
    # add protocol type 
    rawabund$Protocol <- "Terrestrial"
    
    rawabund$WetlandType <- recode(rawabund$WetlandType, 
                                "Poor Fen" = "Fen")
    
    rawabund 
  }
  
  rawabund %>% distinct(WetlandType, Site, Year) %>% group_by(WetlandType) %>% tally()
  
  isa <- read.csv("data/raw/Table S2 - ISA Output - all wetland classes - dist sites only.csv")
  
  is_abund <- left_join(rawabund, 
                       select(isa, -Fidelity, -Sensitivity, -p.value),
                       by=c("WetlandType" = "Wetland.Class", "Species")) %>% 
    mutate(Group=as.character(Group)) %>% 
    mutate(Group=replace_na(Group,"Other")) # some sp are not IS
  
  # calculate relative abund of IS groups 
  indices_relabund <- is_abund %>% 
    filter(Group!="Other") %>% 
    group_by(WetlandType,Site,Year, Group) %>% 
    summarize(AbsAbund=sum(Cover)) %>% # abs abund of group 1 sp, group 2 sp etc w/in each site
    group_by(WetlandType,Site,Year) %>% 
    mutate(TotAbund=sum(AbsAbund)) %>% # total abund (of all groups) at each site
    ungroup() %>% 
    mutate(RelAbund=AbsAbund/TotAbund) # rel abund based on only indicator sp
  
  # not a great distribution across all wetland types and groups
  # this will really only work for marshes
  indices_relabund %>% group_by(WetlandType, Group) %>% tally()
  
  # calculate an impactedness index based on relabund of IS groups
  # rel abund of each cluster 
  indices_relabund <- indices_relabund %>% 
    select(-AbsAbund, -TotAbund) %>% 
    group_by(WetlandType, Site, Year) %>% 
    spread(key=Group, value=RelAbund)
  
  # use only marshes
  indices_relabund <- indices_relabund %>% filter(WetlandType=="Marsh")
  
  # weight relabund by cluster number
  
  # impactedness index
  # Cluster 1 is x100; cluster 2 is 50 and cluster 3 is x0
  impact_index <- indices_relabund %>% 
    mutate(WCluster1=`1`*100,
           WCluster2=`2`*50,
           WCluster3=`3`*0) 
  # sum weighted relative abundances
  impact_index$II <- apply(impact_index[,7:9], 1, sum, na.rm=T)
  
  # for naturalness index weight in the opposite way
  impact_index <- impact_index %>% 
    mutate(WCluster4=`1`*0,
           WCluster5=`2`*50,
           WCluster6=`3`*100) 
  impact_index$NI <- apply(impact_index[,11:13], 1, sum, na.rm=T)
  impact_index <- impact_index %>% select(WetlandType, Site, Year, II, NI)

  # now must divided by the total number of IS present at site
  nsp <- is_abund %>% 
    filter(Cover>0) %>% 
    filter(Group!="Other") %>% 
    distinct(WetlandType, Site, Year, Species) %>% 
    group_by(WetlandType, Site, Year) %>% 
    tally()
  
  # add num of IS to weighted impactedness index
  impact_index <- left_join(impact_index, nsp, by=c("WetlandType", "Site", "Year"))
  impact_index <-  impact_index %>% 
    mutate(II=II/n,
           NI=NI/n)
  
  colnames(impact_index) <- c("WetlandType", "Site", "Year", "II", "NI", "Num_IS")
  
  impact_index <- impact_index %>% 
    mutate(Site = str_replace(Site, "-ABMI-", "-"))
  
  # write.csv(x=impact_index,
  #           file="data/clean/Impactedness Indices - marshes.csv", row.names = F)
    
}

# use ALL indicator species and re-calculate sitediststatus; combine dfs and export ####
{
  isa <- read.csv("data/raw/Table S2 - ISA Output - all wetland classes - dist sites only.csv") %>% 
    select("WetlandType"= Wetland.Class, "Cluster"=Group, Species)
  isa$WetlandType <- recode(isa$WetlandType, SOWW="Shallow Lake", WM="Wet Meadow")
  
  # use most and least disturbed sites
  isa <- isa %>% 
    mutate(Disturbance = case_when(Cluster==1 ~ "High",
                                   WetlandType=="Bog" & Cluster == 4 ~ "Low",
                                   WetlandType=="Fen" & Cluster == 2 ~ "Low",
                                   WetlandType=="Marsh" & Cluster == 3 ~ "Low",
                                   WetlandType=="Shallow Lake" & Cluster == 4 ~ "Low",
                                   WetlandType=="Wet Meadow" & Cluster == 3 ~ "Low",
                                   TRUE ~ NA_character_)) %>% 
    filter(!is.na(Disturbance)) %>% select(-Cluster)
  
  
  
  # 1. num of high/low dist sp
  sitediststatus_full <- left_join(veg_pa, isa, by=c("WetlandType", "Species")) %>% select(-ORIGIN)
  
  sitediststatus_full <- sitediststatus_full %>% 
    filter(!is.na(Disturbance)) %>% 
    group_by(Protocol, WetlandType, Site, Year, Disturbance) %>% 
    tally() 
  
  sitediststatus_full$Disturbance <- recode(sitediststatus_full$Disturbance, High="HighDistSp_N", Low = "LowDistSp_N")
  sitediststatus_full <- sitediststatus_full %>% 
    spread(key=Disturbance, value=n) %>% 
    replace_na(list(HighDistSp_N = 0, LowDistSp_N=0))
  head(sitediststatus_full)
  
  # re-add sites which were dropped b/c they had 0 high dist and 0 low dist species
  sitediststatus_full <- left_join(distinct(veg_pa, Protocol, WetlandType, Site, Year), 
                              sitediststatus_full, by=c("Protocol", "WetlandType", "Site", "Year")) 
  sitediststatus_full <- sitediststatus_full %>% 
    replace_na(., list(HighDistSp_N=0, LowDistSp_N=0))
  
  # 2. abundance of high/low dist sp
  tmp <- left_join(abund, isa, by=c("WetlandType", "Species")) %>% 
    filter(!is.na(Disturbance)) %>% 
    group_by(Protocol, WetlandType, Site, Year, Disturbance) %>% 
    summarize(TotalRelAbund = sum(RelAbund))
  
  # few low dist sp in the appropriate wetland classes sampled under terr protocol
  tmp %>% filter(Disturbance=="Low")
  
  tmp$Disturbance <- recode(tmp$Disturbance, High="HighDistSp_Abund", 
                            Low = "LowDistSp_Abund")
  tmp <- tmp %>% 
    spread(key=Disturbance, value=TotalRelAbund) %>% 
    replace_na(list(HighDistSp_Abund = 0, LowDistSp_Abund=0))
  
  sitediststatus_full <- full_join(sitediststatus_full, 
                                   tmp, 
                                   by=c("Protocol", "WetlandType", "Site", "Year"))

  
  # this file has the site-level disturbance weights, plus the wetland and env characteristics
  chars1_full <- inner_join(sitediststatus_full, chars, by=c("Protocol", "WetlandType", "Site", "Year"))
  # write.csv(x=chars1_full,
  #           file = "data/clean/Site Dist Status - Wet and Env Chars - all IS.csv", row.names = F)
  
  # this file has site-level disturbance weights plus veg Presence-Absence
  veg_rich_full <- inner_join(sitediststatus_full, veg_pa, by=c("Protocol", "WetlandType", "Site", "Year"))
  # write.csv(x=veg_rich_full,
  #           file = "data/clean/Site Dist Status - Veg PA - all IS.csv", row.names = F)
  
  # this file has site-level disturbance weights plus veg abundance
  veg_abund_full <- inner_join(sitediststatus_full, abund, by=c("Protocol", "WetlandType", "Site", "Year"))
  # write.csv(x=veg_abund_full,
  #           file = "data/clean/Site Dist Status - Veg Abund - all IS.csv", row.names = F)

}


# sample only one year for each site to eliminate the need for uniqueID (and year?) as random effects
# try taking the site with the year closest to the median year, so there will be less effect of year
{
  # full IS chars df - this has one more site than the top 5 IS df
  {
    chars_full <- read.csv("data/clean/Site Dist Status - Wet and Env Chars - all IS.csv") %>% 
      filter(WetlandType!="Swamp") %>%
      droplevels()
    chars_full$uniqueID <- paste(chars_full$Protocol, chars_full$Site, sep="_")
    
    median(chars_full$Year) # 2013
    
    chars_full <- chars_full %>% 
      mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
      group_by(Protocol, WetlandType, uniqueID) %>% 
      slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
      select(-Years_from_med) %>% 
      select(uniqueID, everything())
    
    chars_full %>% 
      distinct(Protocol, WetlandType, uniqueID, Year) %>% 
      nrow() # 1255 sites now
    
    write.csv(x=chars_full,
              file = "data/clean/Site Dist Status - Wet and Env Chars - all IS.csv", row.names = F)
  }
  
  # full IS rich df
  {
    rich_full <- read.csv("data/clean/Site Dist Status - Veg PA - all IS.csv") %>% 
      filter(WetlandType!="Swamp")
    
    # summarize and add variable for proportion of exotic sp
    rich_full <- rich_full %>% 
      mutate(ORIGIN=case_when(ORIGIN=="Unknown/Undetermined" ~ "Unknown",
                              TRUE ~ as.character(ORIGIN))) %>% 
      spread(., key=ORIGIN, value=PA) %>%  # first must expand native status for all levels and sites
      replace_na(list(Exotic=0, Unknown=0, Native=0)) %>% 
      gather(., key=Origin, value=PA,11:13) %>% 
      group_by(Protocol, WetlandType, Site, Year, HighDistSp_N, LowDistSp_N, Origin) %>% 
      summarize(Richness = sum(PA)) %>% # sum the num sp of each nativity status for each site 
      group_by(Protocol, WetlandType, Site, Year, HighDistSp_N, LowDistSp_N) %>% 
      mutate(TotRichness=sum(Richness),
             PropExot = Richness/TotRichness) %>% # calculate total rich (of all nativity status) and Prop Exotic
      arrange(Protocol, WetlandType, Site, Year) %>% 
      filter(Origin=="Exotic") %>% 
      select(-Richness)
    rich_full$uniqueID <- paste(rich_full$Protocol, rich_full$Site, sep="_")
    
    median(rich_full$Year) # 2013
    
    rich_full <- rich_full %>% 
      mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
      group_by(Protocol, WetlandType, uniqueID) %>% 
      slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
      select(-Years_from_med) %>% 
      select(uniqueID, everything())
    
    rich_full %>% 
      distinct(Protocol, WetlandType, uniqueID, Year) %>% 
      nrow() # 1255 sites now
    
    write.csv(x=rich_full,
              file = "data/clean/Site Dist Status - Veg PA - all IS.csv", row.names = F)
  }
  
  # full IS abund df - not using
  {
    abund_full <- read.csv("data/clean/Site Dist Status - Veg Abund - all IS.csv") %>% 
      filter(WetlandType!="Swamp")
    abund_full$uniqueID <- paste(abund_full$Protocol, abund_full$Site, sep="_")
    
    median(abund_full$Year) # 2013
    
    abund_full <- abund_full %>% 
      mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
      group_by(Protocol, WetlandType, uniqueID) %>% 
      slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
      select(-Years_from_med) %>% 
      select(uniqueID, everything())
    
    abund_full %>% 
      distinct(Protocol, WetlandType, uniqueID, Year) %>% 
      nrow() # 25 sites now
    
    write.csv(x=abund_full,
              file = "data/clean/Site Dist Status - Veg Abund - all IS.csv", row.names = F)
  }
  
  ####################################
  
  # top 5 IS chars df
  {
    chars <- read.csv("data/clean/Site Dist Status - Wet and Env Chars - top 5 IS.csv") %>% 
      filter(WetlandType!="Swamp") %>% droplevels()
  
    chars$uniqueID <- paste(chars$Protocol, chars$Site, sep="_")
    
    median(chars$Year) # 2013
    
    chars <- chars %>% 
      mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
      group_by(Protocol, WetlandType, uniqueID) %>% 
      slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
      select(-Years_from_med) %>% 
      select(uniqueID, everything())
    
    chars %>% 
      distinct(Protocol, WetlandType, uniqueID, Year) %>% 
      nrow() # 1255 sites now
    
    write.csv(x=chars,
              file = "data/clean/Site Dist Status - Wet and Env Chars - top 5 IS.csv", row.names = F)
  }
  
  # top 5 IS rich df
  {
    rich <- read.csv("data/clean/Site Dist Status - Veg PA - top 5 IS.csv") %>% 
      filter(WetlandType!="Swamp")
    
    # summarize and add variable for proportion of exotic sp
    rich <- rich %>% 
      mutate(ORIGIN=case_when(ORIGIN=="Unknown/Undetermined" ~ "Unknown",
                              TRUE ~ as.character(ORIGIN))) %>% 
      spread(., key=ORIGIN, value=PA) %>%  # first must expand native status for all levels and sites
      replace_na(list(Exotic=0, Unknown=0, Native=0)) %>% 
      gather(., key=Origin, value=PA,10:12) %>% 
      group_by(Protocol, WetlandType, Site, Year, HighDistSp_N, LowDistSp_N, Origin) %>% 
      summarize(Richness = sum(PA)) %>% # sum the num sp of each nativitiy status for each site 
      group_by(Protocol, WetlandType, Site, Year, HighDistSp_N, LowDistSp_N) %>% 
      mutate(TotRichness=sum(Richness),
             PropExot = Richness/TotRichness) %>% # caluclate total rich (of all nativitiy status) and Prop Exotic
      arrange(Protocol, WetlandType, Site, Year) %>% 
      filter(Origin=="Exotic") %>% 
      select(-Richness)
    rich$uniqueID <- paste(rich$Protocol, rich$Site, sep="_")
    
    median(rich$Year) # 2013
    
    rich <- rich %>% 
      mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
      group_by(Protocol, WetlandType, uniqueID) %>% 
      slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
      select(-Years_from_med) %>% 
      select(uniqueID, everything())
    
    rich %>% 
      distinct(Protocol, WetlandType, uniqueID, Year) %>% 
      nrow() # 1255 sites now
    
    write.csv(x=rich,
              file = "data/clean/Site Dist Status - Veg PA - top 5 IS.csv", row.names = F)
  }
  
  # top 5 IS abund df - not using
  {
    # this file has site-level disturbance weights plus veg abundance
    abund <- read.csv("data/clean/Site Dist Status - Veg Abund - top 5 IS.csv") %>% 
      filter(WetlandType!="Swamp")
    abund$uniqueID <- paste(abund$Protocol, abund$Site, sep="_")
    
    median(abund$Year) # 2013
    
    abund <- abund %>% 
      mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
      group_by(Protocol, WetlandType, uniqueID) %>% 
      slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
      select(-Years_from_med) %>% 
      select(uniqueID, everything())
    
    abund %>% 
      distinct(Protocol, WetlandType, uniqueID, Year) %>% 
      nrow() # 25 sites now
    
    write.csv(x=abund,
              file = "data/clean/Site Dist Status - Veg Abund - top 5 IS.csv", row.names = F)
  }

}