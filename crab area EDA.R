####Erin Fedewa crab area boundary issues####
#Matt Callahan 
#matt.callahan@noaa.gov
#12/14/21

require(tidyverse)
require(sf)
require(ggplot2)
require(rnaturalearth)
require(lubridate)


#bring in world data 
AK <- ne_countries(scale = "medium", returnclass = "sf")%>%
  st_make_valid()%>%
  filter(name=="United States")

#examine crab fgdb
st_layers("Data/fedewa_EBS_NBS_grid.gdb")

#load regions. This file geodatabase has ESR regions, NMFS areas, BSIERP areas, andADFG stat areas.
Management <- st_read("Data/Alaska_Marine_Management_Areas.gdb") 
ADFG<-Management%>%filter(Area_Type=="ADFG Stat Area")
NMFS<-Management%>%filter(Area_Type=="NMFS Reporting Area")

#examine crab fgdb
st_layers("Data/fedewa_EBS_NBS_grid.gdb")

#import each layer as a separate object
NBS<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="NBS_grid")
EBSc<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_grid_with_corners")
EBSnc<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_grid_no_corners")
NBS_2017<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="NBS_HAUL_2017")
IMM_FEM_2017<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="IMM_FEM_2017")
EBS_IMM_FEM_2017<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_IMM_FEM_2017")
EBS_IMM_MALE<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_IMM_MALE")
EBS_MALE_80TO100<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_MALE_80TO100")
ECO_SC_stockstations<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="ECO_SC_stockstations")
EBS_REGIONS<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_REGIONS")
NBS_REGIONS<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="NBS_REGIONS")

#load station points
stations<-read.csv("Data/Stock Station Centers.csv")%>%
  st_as_sf(coords=c("CLON","CLAT"), crs=st_crs(NBS))

#load old shapefiles
BB<-st_read("Data/BristolBay.shp")
SM<-st_read("Data/St_Matthew_District.shp")


#Define extent
xmin <- -180
xmax <- -155
ymin <- 50
ymax <- 67

win.graph()

#plot areas and stations
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  # geom_sf(data=NBS, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBSc, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBSnc, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=NBS_2017, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=IMM_FEM_2017, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_IMM_MALE, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_IMM_MALE, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_MALE_80TO100, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=ECO_SC_stockstations, fill=NA, aes(), size=1, color="black")+
  geom_sf(data=EBS_REGIONS, aes(fill=as.factor(REGION)), alpha=0.5, size=1, color="red")+
  geom_sf(data=NBS_REGIONS, aes(fill=as.factor(REGION)), alpha=0.5, size=1, color="black")+
  geom_sf(data=stations)+
  geom_sf(data=BB, fill=NA, aes(), size=2, color="blue")+
  geom_sf(data=SM, fill=NA, aes(), size=2, color="blue")+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()

unique(stations$STOCK_REGION)

#plot stations by stock region
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=stations)+
  geom_sf(data=EBS_REGIONS, fill=NA,   color="red")+
  geom_sf(data=NBS_REGIONS, fill=NA,   color="black")+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  facet_wrap(~STOCK_REGION)+
  theme_bw()


####QA on existing shapefiles####
#plot existing shapefiles
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=EBS_REGIONS, fill=NA,  size=1, color="red")+
  geom_sf(data=BB, fill=NA, aes(), size=1, color="black")+
  geom_sf(data=SM, fill=NA, aes(), size=1, color="black")+
  geom_sf(data=stations%>%filter(STOCK_REGION %in% c("BBRKC", "StMattBKC")))+
  facet_wrap(~STOCK_REGION)+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()

#zoom in on St Matt
#change extent to zoom in
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=EBS_REGIONS, fill=NA,  size=1, color="red")+
  geom_sf(data=SM, fill=NA, aes(), size=1, color="black")+
  geom_sf(data=stations%>%filter(STOCK_REGION %in% c("BBRKC", "StMattBKC")))+
  #coord_sf(xlim=c(-177, -171), ylim=c(58,62))+
  coord_sf(xlim=c(-173, -172.5), ylim=c(58.8,59))+
  theme_bw()

#check distances from grid to point

#distance to grid
#let's start with a singe point... say P-29
ggplot()+
  geom_sf(data=EBS_REGIONS, fill=NA,  size=1, color="red")+
  geom_sf_text(data=EBS_REGIONS, aes(label=STATION_ID), color="red")+
  geom_sf(data=stations, size=2)+
  # geom_sf_text(data=stations, aes(label=STATION_ID))+
  coord_sf(xlim=c(-178.5, -175.5), ylim=c(59.5,60.5))+
  theme_bw()

ggplot()+
  geom_sf(data=EBS_REGIONS, fill=NA,  size=1, color="red")+
  geom_sf_text(data=EBS_REGIONS, aes(label=STATION_ID), color="red")+
  geom_sf(data=stations, size=2)+
  # geom_sf_text(data=stations, aes(label=STATION_ID))+
  coord_sf(xlim=c(-168.5, -165.5), ylim=c(55.5,56.5))+
  theme_bw()

#plot just one point and line from polygon
ggplot()+
  geom_sf(data=EBS_REGIONS%>%filter(STATION_ID=="P-29")%>%st_cast(to="MULTILINESTRING"), color="red")+
  geom_sf(data=stations%>%filter(STATION_ID=="P-29"), size=2)+
  coord_sf(xlim=c(-177, -175), ylim=c(59.6,60.4))+
  theme_bw()

#plot projected
ggplot()+
  geom_sf(data=EBS_REGIONS%>%filter(STATION_ID=="P-29")%>%st_cast(to="MULTILINESTRING"), color="red")+
  geom_sf(data=stations%>%filter(STATION_ID=="P-29"), size=2)+
  coord_sf(crs=3338)+
  theme_bw()

ggplot()+
  geom_sf(data=EBS_REGIONS, color="red")+
  geom_sf(data=stations, size=2)+
  # coord_sf(crs=3338)+
  theme_bw()

#check distances from points to the edges
#I thought they should be about 20nm or a little less since the points aren't centered
#huh, this gives me about 8m, which is shorter than expected
#It looks like the grid is ~10nm around each point (~20 nm for each edge of the cell)
#This makes sense looking at the images, there are about 6 stations for every two degrees latitude
#At 60nm/degree that comes out to about 20 nm between stations
st_distance(stations%>%filter(STATION_ID=="P-29"),EBS_REGIONS%>%filter(STATION_ID=="P-29")%>%st_cast(to="MULTILINESTRING"))*0.000539957
st_distance(stations%>%filter(STATION_ID=="P-30"),EBS_REGIONS%>%filter(STATION_ID=="P-30")%>%st_cast(to="MULTILINESTRING"))*0.000539957
#


#illustrate points aren't perfectly in the center of the grid
ggplot()+
  geom_sf(data=EBS_REGIONS, fill=NA,  size=1, color="red")+
  geom_sf(data=stations, size=2)+
  coord_sf(xlim=c(-178.5, -176), ylim=c(59.5,60.5))+
  theme_bw()

####Plot regions together #####
#NBS and EBS are offset

ggplot()+
  geom_sf(data=NBS, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBSc, fill=NA, aes(), size=1, color="green")+
  geom_sf(data=EBSnc, fill=NA, aes(), size=1, color="red")+
  # geom_sf(data=NBS_2017, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=IMM_FEM_2017, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_IMM_MALE, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_IMM_MALE, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_MALE_80TO100, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=ECO_SC_stockstations, fill=NA, aes(), size=1, color="black")+
  # geom_sf(data=EBS_REGIONS,  fill=NA, size=1, color="red")+
  geom_sf(data=NBS_REGIONS,  fill=NA, size=1, color="black")+
  #geom_sf(data=stations)+
  #geom_sf(data=BB, fill=NA, aes(), size=2, color="blue")+
  #geom_sf(data=SM, fill=NA, aes(), size=2, color="blue")+
  geom_sf(data=AK, fill="gray60")+
  # coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  # coord_sf(xlim=c(-170, -169), ylim=c(57.5,58))+
  coord_sf(xlim=c(-173, -171), ylim=c(60.5,61.5))+
  # coord_sf(crs=3338,xlim=c(-985938, -796517), ylim=c(1200000,1376986))+
  theme_bw()


#same projection
crs(NBS_REGIONS)
crs(NBS)
crs(EBSnc)

ggplot()+
  geom_sf(data=NBS%>%filter(LAT>60.3 & LAT<62 & LON>-174 & LON<=-170), 
          aes(fill=STATION_ID), size=1, color="red")+
  geom_sf(data=EBSnc, fill=NA, size=1, color="black")+
  # geom_sf(data=NBS_REGIONS,  fill=NA, size=1, color="blue")+
  geom_sf(data=AK, fill="gray60")+
  # coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  coord_sf(xlim=c(-173, -171), ylim=c(60.5,61.5))+
  theme_bw()

hist(NBS$Shape_Area)
min(NBS$Shape_Area)
#There are a bunch of little polygons in the NBSS edges

#plot the small polygons that don't have a station in them.
ggplot()+  
            geom_sf(data=EBSnc, fill=NA, aes(), size=1, color="red")+
            geom_sf(data=AK, fill="gray60")+
            geom_sf(data=NBS%>%filter(Shape_Area<=0.07), fill=NA, aes(), size=1, color="black")+
            coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
            theme_bw()

