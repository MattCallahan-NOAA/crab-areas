####Erin Fedewa crab areas####
#Matt Callahan 
#matt.callahan@noaa.gov
#12/3/21

require(tidyverse)
require(sf)
require(ggplot2)
require(rnaturalearth)
require(httr)
require(curl)
require(cowplot)
require(lubridate)

#bring in world data 
AK <- ne_countries(scale = "medium", returnclass = "sf")%>%
  st_make_valid()%>%
  filter(name=="United States")

#examine crab fgdb
st_layers("Data/fedewa_EBS_NBS_grid.gdb")

#import NBS and EBS layers
NBS<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="NBS_grid")
EBSnc<-st_read(dsn="Data/fedewa_EBS_NBS_grid.gdb", layer="EBS_grid_no_corners")


#load station points
stations<-read.csv("Data/Stock Station Centers.csv")%>%
  st_as_sf(coords=c("CLON","CLAT"), crs=st_crs(NBS))

#Define extent
xmin <- -180
xmax <- -155
ymin <- 50
ymax <- 67

win.graph()

#plot stations by stock region
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=stations)+
  geom_sf(data=EBSnc, fill=NA,   color="red")+
  geom_sf(data=NBS, fill=NA,   color="black")+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  facet_wrap(~STOCK_REGION)+
  theme_bw()

####break out 3 sets of files. 
#one for pribilof, st matt, and bristol bay
#one for east and west tanner
#NBS

#NBS
NBS$NBS_CRAB<-"NBS"
NBS_b<-NBS%>%
  group_by(NBS_CRAB)%>%
  summarise()

#looks right (as long as we're ok with the hole around St. Laurence)
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=NBS_b, fill=NA)+
  geom_sf_label(data=TC, aes(label=NBS))+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()

#King crab 
#RKC and BKC pribs are the same. Could separate later if desired
KC<-st_join(EBSnc, stations%>%
              filter(STOCK_REGION %in% c("BBRKC", "StMattBKC", "PribRKC")), left=FALSE)%>%
  mutate(BS_KING_CRAB=STOCK_REGION)%>%
  group_by(BS_KING_CRAB)%>%
  summarise()

ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=KC, fill=NA)+
  geom_sf_label(data=KC, aes(label=BS_KING_CRAB))+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()


#isolate tanner crab
TC<-st_join(EBSnc, stations%>%
              filter(STOCK_REGION %in% c("TannerE", "TannerW")), left=FALSE)%>%
  mutate(BS_TANNER=STOCK_REGION)%>%
  group_by(BS_TANNER)%>%
  summarise() %>%
  rename
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=TC, fill=NA)+
  geom_sf_label(data=TC, aes(label=BS_TANNER))+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()

#now plot all of them


ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=NBS_b, fill=NA, color="red")+
  geom_sf_label(data=NBS_b, aes(label=NBS_CRAB), color="red")+
  geom_sf(data=KC, fill=NA, color="black")+
  geom_sf_label(data=KC, aes(label=BS_KING_CRAB))+
  geom_sf(data=TC, fill=NA,color="blue")+
  geom_sf_label(data=TC, aes(label=BS_TANNER), color="blue")+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()



#Lookup table
#load station points
lkp<-read.csv("Data/crw_lkp.csv")%>%
  mutate(LAT=LATITUDE, LON=LONGITUDE)%>%
  st_as_sf(coords=c("LON","LAT"), crs=st_crs(NBS_b))

#slow...
ggplot()+
  geom_sf(data=AK, fill="gray60")+
  geom_sf(data=lkp, fill=NA, color="black")+
  coord_sf(xlim=c(-170, -169), ylim=c(57,58))+
  theme_bw()


#Add NBS
lkp_c<-st_join(lkp, NBS_b, join=st_within)
lkp_c<-st_join(lkp_c, KC, join=st_within)
lkp_c<-st_join(lkp_c, TC, join=st_within)


####Pull in SST####
#2.7 billion records, 1.6 GB... 
#If you actually want these data I can send them to you 
#Otherwise just use the pre-aggregated data (attached)

sst<-read.csv("bs_2020_sst.csv")

#isolate one day
sst2<-filter(sst, READ_DATE=="04-JUL-19 12:00:00")
#plot
ggplot()+
  geom_tile(data=sst2, mapping=aes(x=LONGITUDE, y=LATITUDE, fill=TEMP))+
  geom_sf(data=NBS_b, fill=NA, size=1, color="orange")+
  geom_sf(data=KC, fill=NA, size=1, lty=2, color= "red")+
  geom_sf(data=TC, fill=NA, size=1, lty=4, color="black")+
  geom_sf_label(data=NBS_b, aes(label=NBS_CRAB))+
  geom_sf_label(data=TC, aes(label=BS_TANNER))+
  geom_sf_label(data=KC, aes(label=BS_KING_CRAB))+
  geom_sf(data=AK, fill="gray60")+
  scale_fill_viridis_c()+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()

#since this is such a large dataset I will remove some fields to reduce object size
sst<-sst%>%dplyr::select(CRW_ID, READ_DATE, TEMP)
lkp_c<-lkp_c%>%dplyr::select(ID, LONGITUDE,LATITUDE,NBS_CRAB, BS_KING_CRAB,BS_TANNER)
  #join
sst_crab<-sst%>%
  left_join(lkp_c, by=c("CRW_ID"="ID"))
#summarize
#NBS
sst_NBS<-sst_crab%>%filter(NBS_CRAB>0)%>%
  group_by(READ_DATE, NBS_CRAB)%>%
    summarize(meansst=mean(TEMP))
sst_NBS<-sst_NBS%>%
  mutate(date=as_date(dmy_hms(READ_DATE)))%>%
  arrange(date)
#King
sst_KC<-sst_crab%>%filter(BS_KING_CRAB>0)%>%
  group_by(READ_DATE, BS_KING_CRAB)%>%
  summarize(meansst=mean(TEMP))
sst_KC<-sst_KC%>%
  mutate(date=as_date(dmy_hms(READ_DATE)))%>%
  arrange(date)
#Tanner
sst_TC<-sst_crab%>%filter(BS_TANNER>0)%>%
  group_by(READ_DATE, BS_TANNER)%>%
  summarize(meansst=mean(TEMP))
sst_TC<-sst_TC%>%
  mutate(date=as_date(dmy_hms(READ_DATE)))%>%
  arrange(date)
#write aggregates to rds for Erin
saveRDS(sst_NBS, "Data/sst_NBS.rds")
saveRDS(sst_KC, "Data/sst_king.rds")
saveRDS(sst_TC, "Data/sst_tanner.rds")

####Start here with aggregated SST csvs####

sst_NBS<-readRDS("Data/sst_NBS.rds")
sst_KC<-readRDS("Data/sst_king.rds")
sst_TC<-readRDS("Data/sst_tanner.rds")
#Plot
max(sst$TEMP);min(sst$TEMP)
#NBS
pNBS<-ggplot()+
  geom_line(data=sst_NBS, aes(x=date, y=meansst), color="blue")+
  ylim(c(-2, 15))+
  scale_x_date(date_breaks="1 month",
               date_labels = "%b",
               expand = c(0.025,0.025))+
  facet_wrap(~NBS_CRAB)+
  theme_bw()
pNBS


#KC
pKC<-ggplot()+
  geom_line(data=sst_KC, aes(x=date, y=meansst), color="blue")+
  ylim(c(-2, 15))+
  scale_x_date(date_breaks="1 month", date_labels = "%b", expand = c(0.025,0.025))+
  facet_wrap(~BS_KING_CRAB)+
  theme_bw()
pKC

#TC
pTC<-ggplot()+
  geom_line(data=sst_TC, aes(x=date, y=meansst), color="blue")+
  ylim(c(-2, 15))+
  scale_x_date(date_breaks="1 month",
               date_labels = "%b",
               expand = c(0.025,0.025))+
  facet_wrap(~BS_TANNER)+
  theme_bw()
pTC

toprow<-plot_grid(pNBS, pTC, rel_widths = c(1,2))
crab_plot_19<-plot_grid(toprow, pKC, nrow=2)
crab_plot_19
