rm(list=ls())    # clear the workspace

############ Temur Gugushvili ###########
############ CEES fellowship ###########


#devtools::install_github("mattflor/chorddiag")


# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag) 
library(igraph)





RoadMIPs <- readr::read_csv("data/ongoingMIPs2018_2024.csv")



RoadMIPs_en <- RoadMIPs |>
  mutate(funding_fullname_en  = recode(funding_abbreviation,
                                       "ADB" = "Asian Development Bank",
                                       "AIIB" = "Asian Infrastructure Investment Bank",
                                       "EIB" = "European Investment Bank",
                                       "GOG" = "Government of Georgia",
                                       "KFAED" = "Kuwait Fund for Arab Economic Development",
                                       "WB" = "World Bank Group",
                                       "EBRD" = "European Bank for Reconstruction and Development")) |> 
  mutate(companyBYcountry  = recode(company, 
                                    "Hunan Road & Bridge Construction Group Co., LTD" = "China",
                                    "China Road and Bridge Corporation" = "China",
                                    "JV POLAT YOL-MAPA" = "Turkey",
                                    "China Railway Tunnel Group Co., LTD" = "China",
                                    "Sinohydro Corporation Limited" = "China",
                                    "Road Maintenance and Building Company Serpantini Ltd." = "Georgia",
                                    "China Railway 23rd Bureau Group" = "China",
                                    "China Railway 23rd Bureau Group Co., Ltd" = "China",
                                    "China State Construction Engineering Corporation Limited" = "China",
                                    "Guizhou Highwat Engineering Group Co Ltd and China National Technical Import & Export Corporation JV" = "China",
                                    "Polatyol MAPA Joint Venture" = "Turkey",
                                    "JV MIRBUD-CS"="Poland",
                                    "Company Black Sea Group LLC" = "Georgia",
                                    "JV Company Black Sea Group and Vahagn & Samvel"="Armenia & Georgia",
                                    "AGT Menegment, Meslehet,Tikinti Xidmetleri MMC" = "Azerbaijan",
                                    "Akkord Georgia" = "Georgia",
                                    "New Road" = "Georgia",
                                    "Arali" = "Georgia",
                                    "Peri" = "Georgia",
                                    "Mamisoni" = "Georgia",
                                    "Jeu Group" = "Georgia",
                                    "Sakmilsadenmsheni" =	"Georgia",
                                    "New Road" = "Georgia",
                                    "Dagi+" = "Georgia",
                                    "RTD LLC" = "Georgia")) 


implementWho <- RoadMIPs_en %>% 
  select(funding_abbreviation, companyBYcountry, GEL, funding_fullname_en) %>% 
  group_by(companyBYcountry, funding_abbreviation) %>% 
  summarise(BilionsUSD = round(sum(GEL)/2.6/1000000000, 2)) %>% 
  rename("unite_dest" = "companyBYcountry", "unite_orig"="funding_abbreviation", "value" = "BilionsUSD") 




png("Visualization/chordDiagramMIPs.png",
    width = 680, height = 480,
    pointsize = 12,
    bg = "white") 

# parameters
circos.clear()
circos.par(start.degree = 180, gap.degree = 6, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(13, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:13)]

# Base plot
chordDiagram(
  x = implementWho, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = FALSE, 
  link.largest.ontop = TRUE)

# Add text and axis


circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 4, 
      labels = sector.index, 
      facing = "downward", 
      cex = 0.9
    )
    
    #    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
    #               "downward", "bending", "bending.inside", "bending.outside"),
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      minor.ticks = 6, 
      major.tick.length = 0.5,
      labels.niceFacing = FALSE)
  }
)

dev.off()
