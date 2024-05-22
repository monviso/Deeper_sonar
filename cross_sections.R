pt<-read_sf("E:/OneDrive/Documents/paper_sonar/shp/trans_sparkfun.shp")
a<-pt[which(pt$trans==1),]
a<-a[c(6,1,2,3,4,5,7),]

d<-st_distance(a)

ss<-c()

for (k in 1:ncol(d)-1){
  ss[k]<-d[k+1,k]
}

ss<-c(0,ss)
a$cdist<-ss

##cummulative distance
a$cumdist<-cumsum(a$cdist)
a$depthNeg<-a$depth1004*-0.01
start<-csf[1,]
start$depthNeg<-0

end<-csf[nrow(csf),]
end$depthNeg<-0

csf<-rbind(start,csf,end)


ggplot(data = a, aes(x=cumdist,y=depthNeg))+
  geom_line(aes(x=cumdist,y=depthNeg),linewidth=2)+
  geom_point(aes(x=cumdist,y=depthNeg),size=4, color="blue")+
  geom_point(data = a[which(a$depthNeg==0),], aes(x=cumdist,y=depthNeg),size=5, color="red")+
  
  labs(x="channel width (m)",y="water depth (m)")+
  theme_bw()+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=15))
ggsave("E:/OneDrive/Documents/paper_sonar/figures/11.png",dpi = 300, width = 7,height = 5,units = "in")  


##to poly
a<-st_zm(a)
ap<-a%>%summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
plot(ap)
