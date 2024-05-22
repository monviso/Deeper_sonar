
library(sf)
library(mgcv)
library(stars)
library(terra)
library(nngeo)
library(ggplot2)
library(dplyr)

#import boundary
main<- read_sf("E:/OneDrive/Documents/paper_sonar/shp/main.shp")
wt<-read_sf("E:/OneDrive/Documents/paper_sonar/shp/wetted_area.shp")

#load points of known water depth
pt<-read_sf("E:/OneDrive/Documents/paper_sonar/shp/trans_sparkfun.shp")
pt<-st_transform(pt, 25831)
pt$row<-seq(1,nrow(pt))
co<-st_coordinates(pt)
pt$x<- co[,1]
pt$y<-co[,2]

##average point depth within certain resolution function
library(tidyverse)
split_poly <- function(sf_poly, n_areas){
  # create random points
  points_rnd <- st_sample(sf_poly, size = 10000)
  #k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # clip to sf_poly
  crs(voronoi_polys) <- crs(sf_poly)
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly)
  equal_areas$area <- st_area(equal_areas)
  return(equal_areas)
}


main_b<-st_buffer(main,-1)
wt_b<-st_buffer(wt,-0.1)

##main boundary
g_close<-st_coordinates(main_b)
g_close<-as.data.frame(g_close)
bound_close <- list(list(x= g_close[,1], y= g_close[,2]))#, f=rep(0,nrow(g))))## add the f=.... argument if boundary condition are known

#filter boundary
g_wt<-st_coordinates(wt_b)
g_wt<-as.data.frame(g_wt)
bound_wt <- list(list(x= g_wt[,1], y= g_wt[,2]))#, f=rep(0,nrow(g))))## add the f=.... argument if boundary condition are known

ptin_a <- pt[with(pt, inSide(bound_close, x, y)), ]
#s<-split_poly(main,round(st_area(filter)/1,0))
ptin_a<-st_zm(ptin_a)


#knots defined from watered and non waterd point locations
g_close_b<-st_coordinates(main_b)
g_close_b<-as.data.frame(g_close_b)

gx <- seq(min(g_close_b[,1]), max(g_close_b[,1]), len = 7)
gy <- seq(min(g_close_b[,2]), max(g_close_b[,2]), len = 7)
gp <- expand.grid(gx, gy)
names(gp) <- c("x","y")
gp_sf1<-st_as_sf(gp,coords = c("x","y"))
st_crs(gp_sf1)<-25831
gp_sf1$with_close<-st_within(gp_sf1,main_b,sparse = F)
gp_sf1$with_wt<-st_within(gp_sf1,wt_b,sparse = F)
knots_land<-gp_sf1[which(gp_sf1$with_close==T & gp_sf1$with_wt==F),]
kn1<-data.frame(x=st_coordinates(knots_land)[,1],y=st_coordinates(knots_land)[,2])

g_wt_b<-st_coordinates(wt_b)
g_wt_b<-as.data.frame(g_wt_b)

gx <- seq(min(g_wt_b[,1]), max(g_wt_b[,1]), len = 10)
gy <- seq(min(g_wt_b[,2]), max(g_wt_b[,2]), len = 10)
gp <- expand.grid(gx, gy)
names(gp) <- c("x","y")
gp_sf2<-st_as_sf(gp,coords = c("x","y"))
st_crs(gp_sf2)<-25831
gp_sf2$with_wt<-st_within(gp_sf2,wt_b,sparse = F)

knots_water<-gp_sf2[which(gp_sf2$with_wt==T),]
kn2<-data.frame(x=st_coordinates(knots_water)[,1],y=st_coordinates(knots_water)[,2])

##shallow water points

sw<-st_segmentize(wt_b,dfMaxLength = 0.1)%>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  select(X, Y) %>% 
  st_as_sf(coords = c("X", "Y"))

st_crs(sw)<-25831

sw<-sw[round(seq(1,nrow(sw),length.out=300),0),]
L <- st_cast(main_b,"LINESTRING")
sw$dist<-round(as.numeric(st_distance(sw,L)),1)
tbl<-as.data.frame(table(sw$dist))
tbl$dist<-as.numeric(levels(tbl$Var1))

#sw2<-sw[which(sw$dist < tbl$dist[which.max(tbl$Freq)]-0.1 |sw$dist > tbl$dist[which.max(tbl$Freq)] +0.1 ),]
swdf2<-data.frame(x=st_coordinates(sw)[,1],y=st_coordinates(sw)[,2],depth=-0.1,wt=as.factor(1))
ptin_a$wt<-as.factor(1)
ptin<-as.data.frame(ptin_a[,c(18,19,14,20)])
colnames(ptin)[3]<-"depth"
ptin$depth<-ptin$depth*-0.01
swdf2<-st_as_sf(swdf2,coords = c("x","y"),remove=F)
st_crs(swdf2)<-25831
#ptin$wt<-1
ptin2<-rbind(ptin,swdf2)

##values on the river boundary
sw3<-st_segmentize(wt,dfMaxLength = 0.1)%>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  select(X, Y) %>% 
  st_as_sf(coords = c("X", "Y"))
st_crs(sw3)<-25831

sw3<-sw3[round(seq(1,nrow(sw3),length.out=300),0),]


ptin2_sf<-st_as_sf(ptin2,coords=c("x","y"),remove = F)
st_crs(ptin2_sf)<-25831

##zero water depth point
g_close_b<-st_coordinates(wt_buff)
g_close_b<-as.data.frame(g_close_b)

gx <- seq(min(g_close_b[,1]), max(g_close_b[,1]), len =20)
gy <- seq(min(g_close_b[,2]), max(g_close_b[,2]), len = 20)
gp <- expand.grid(gx, gy)
names(gp) <- c("x","y")
gp_sf1<-st_as_sf(gp,coords = c("x","y"))
st_crs(gp_sf1)<-25831
gp_sf1$within<-as.numeric(st_within(gp_sf1,wt))
gp_sf1$within2<-as.numeric(st_within(gp_sf1,wt_buff))

gp_sf1<-gp_sf1[which(is.na(gp_sf1$within) & gp_sf1$within2==1),]

kn<-rbind(kn1,kn2)


##som plot
library(ggplot2)
lng<- read_sf("E:/OneDrive/Documents/paper_sonar/shp/trans_correct.shp")
wt_buff<-st_buffer(wt,5)
knsf<-st_as_sf(kn, coords = c("x","y"))
st_crs(knsf)<-25831

ggplot()+
  geom_sf(data=wt_buff,fill="white",color="black",linewidth=1)+
  geom_sf(data=wt,fill="white",color="orange",linewidth=1)+
  geom_sf(data=lng,color="blue",size=0.5)+
  geom_sf(data=knsf,color= "red",size=2)+
  geom_sf(data=ptin2_sf[which(ptin2_sf$depth==-0.1),],color="darkblue",size=1.5 )+
  geom_sf(data=gp_sf1,color="darkgrey",size=1.5)+
  geom_sf(data=sw3, color="green",size=1.5)+
  xlim(c(299676,299685))+
  ylim(c(4611197,4611202))+
  theme_void()
  
ggsave("E:/OneDrive/Documents/paper_sonar/figures/8c_det.png",dpi = 300, width = 7,height = 5,units = "in")  

#knotsdf<-data.frame(x=st_coordinates(kn)[,1],y=st_coordinates(kn)[,2])#dataframe format
#running model for interpolation with soap film smoothing

kwt<-st_coordinates(wt)
kwt<-as.data.frame(kwt)
bound_wt <- list(list(x= kwt[,1], y= kwt[,2]))#, f=rep(0,nrow(g))))## add the f=.... argument if boundary condition are known


gam2<-gam(depth~s(x,y,bs="so",xt=list(bnd=bound_wt)),knots =kn,data=ptin2_sf,  method = "REML")


dwa<-rast("E:/OneDrive/Documents/paper_sonar/back_ortho/Ortomosaic_1cm.tif")
dwa<-aggregate(dwa, 5)
dwa1<-crop(dwa,wt)
dwa1<-mask(dwa1,wt)
#dwa2<-disagg(dwa1,fact=2)
df<-as.data.frame(dwa1,xy=T,na.rm=F)
#df$layer<-ifelse(is.na(df$layer),0,1)
#dfsf<-st_as_sf(df,coords=c("x","y"),remove = F)
#st_crs(dfsf)<-29874

#dfsf$with<-st_within(dfsf,main1,sparse = F)
#df1<-dfsf[which(dfsf$with==T),]

colnames(df)[c(1:3)]<-c("x","y","wt")
#df$dwt<-predict.gam(gam2, newdata =df,type="response")#prediction
df$d<-predict.gam(gam2, newdata =df,type="response")#prediction

#df$dwt<-as.numeric(df$dwt)
#df$dwt<-ifelse(df$wt==1,df$dwt,NA)
#df$dwt<-ifelse(df$dwt>0,0,df$dwt)

df$d<-as.numeric(df$d)
df$d<-ifelse(df$wt==1,df$d,NA)
df$d<-ifelse(df$d>0,0,df$d)


m_c_d<-matrix(df$d,ncol=ncol(dwa1),byrow = T)
mr<-rast(m_c_d)

ext(mr)<-ext(dwa1)
crs(mr)<-"epsg:25831"

plot(mr)

writeRaster(mr, "E:/OneDrive/Documents/paper_sonar/back_ortho/bathymetry.tif")
