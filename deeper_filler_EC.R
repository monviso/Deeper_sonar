###FUNCTION TO GET DEEPR RAW BATHYMETRY FILE AND PRODUCE A COMPLETE LAT-LONG FILE + .SHP######

####HOW TO RUN IT
##required libraries: sf
#execute the function deeper_filler(!)

#run the function with required arguments:

#pth: character path to deeper .csv files
#file_names: character vector of deeper .csv files name(s). From 1 to N. Do not report the .csv extension (see example)
#dest_path: path to folder where the output must be saved
# loc_ref: numeric EPSG code of an additional local reference system

#deeper_filler will produce a .csv file with reconstructed lat-long values for each sonar measure plus a .shp point file for the measures
#default reference system will be WGS84 (epsg:4326)
#by indicating loc_ref and additional .shp file will be produced with the point projected in the indicated reference system.  
#N.B.: the final warnings "In CPL_write_ogr (obj, dsn, ..." it's normal and not problematic. It indicates that the millisecond field has not been written
#to the .shp file.


deeper_filler<-function(pth, file_names,dest_path,loc_ref=NA){
  if(require(sf)==F){stop("install sf!")}else{
    pb <- txtProgressBar(min = 0, max = length(file_names), style = 3)
    
    for (z in 1:length(file_names)) {
      Sys.sleep(0.1)
      # update progress bar
      setTxtProgressBar(pb, z)
      
      sc<-read.csv(paste0(pth,file_names[z],".csv"))
      if(ncol(sc)==5){colnames(sc)<-c("latitude","longitude","depth","temperature","time")}else if(ncol(sc)==4){
        colnames(sc)<-c("latitude","longitude","depth","time") 
      }else{stop(paste0("file ",file_names[z]," corrupted!"))}
      sc$nrow<-seq(1:nrow(sc))
      sc$latitude<-ifelse(sc$latitude==0,NA,sc$latitude)
      sc$longitude<-ifelse(sc$longitude==0,NA,sc$longitude)
      
      comp_co<-sc[-which(is.na(sc$latitude)),]
      #compute effective milliseconds between subsequent coordinates 
      pr<-c()
      for (i in 1:(nrow(comp_co)-1)) {
        pr[i]<-comp_co$time[i+1]-comp_co$time[i]
      }
      comp_co$df_seq<-c(NA,pr)
      df_lat<-c()
      df_long<-c()
      for (i in 1:(nrow(comp_co)-1)){
        df_lat[i]<-(comp_co$latitude[i+1]-comp_co$latitude[i])/comp_co$df_seq[i+1]
        df_long[i]<-(comp_co$longitude[i+1]-comp_co$longitude[i])/comp_co$df_seq[i+1]
      }
      
      comp_co$df_lat<-c(NA,df_lat)
      comp_co$df_long<-c(NA,df_long)
      
      ##calculate for each subset of NA coordinates the degree of lat and long for each millisecond
      
      #library(data.table)
      #scd<-as.data.table(sc)[, count := sequence(.N), by = rleid(latitude)][!is.na(latitude) , count := 0][]
      #s<-c()
      ls<-data.frame()
      for (j in 1: (nrow(comp_co)-1)){
        s<-sc[comp_co$nrow[j]:comp_co$nrow[j+1],]
        for (i in 1:(nrow(s)-1)){
          df<-s$time[i+1]-s$time[i]
          s$latitude[i]<-ifelse(!is.na(s$latitude[i]),s$latitude,s$latitude[i-1]+(df*comp_co$df_lat[j+1]))
          s$longitude[i]<-ifelse(!is.na(s$longitude[i]),s$longitude,s$longitude[i-1]+(df*comp_co$df_long[j+1]))
        }
        ls<-rbind(ls,s)
      }
      write.csv(ls,paste0(dest_path,file_names[z],".csv"))
      defsf<-st_as_sf(ls,coords = c("longitude","latitude"),remove = F)
      st_crs(defsf)<-4326
      write_sf(defsf,paste0(dest_path,file_names[z],"_4326.shp"))
      if(is.na(loc_ref)==F){
        defsf_proj<-st_transform(defsf,crs=loc_ref)
        write_sf(defsf_proj,paste0(dest_path,file_names[z],"_",loc_ref,".shp"))
      }
      
      
      
    }
  }
}


##Example (run_change direction)

deeper_filler(pth="D:/Estrella/4_Data/Deeper_SONAR/", file_names=c("scan_data_2024-02-20_122537"), dest_path="D:/Estrella/4_Data/Deeper_SONAR/output",loc_ref=2062)


