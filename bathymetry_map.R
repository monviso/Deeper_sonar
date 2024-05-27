#obtain full bathymetry map

bathymetry-map<- function(bathymetry_path, boundary,loc_coord=NA,resolution=NA, out_path, overlap_procedure=F, interpolation_spec=NA, val_method="cross-validation", 
                          add_proj=F, DEM=F){
  if(require(sf)==F|require(terra)==F|require(stars)==F){stop("install required packages (terra and/or sf and/or stars)!")}else{
    bp<- read.csv(bathymetry_path)
    #checking format of boundary, needed both sf and rast object
    e<-substr(boundary, nchar(boundary))
    if(e=="p"){
      bd_sf<-read_sf(boundary)
      sfcrs<-st_crs(bd_sf)
      bd_rs<-rasterize(bd_sf, res=resolution, background=0, field=1)##to check raster projection and crs after raterize
      if(sfcrs==4326){bd_sf<-st_transform(bd_sf,loc_coord); crs(bd_rs)<-paste0("epsg:",loc_coord)}
    } else if (e=="f"){
      bd_rs<-rast(boundary)
      rscrs<-crs(bd_rs)
      bd_sf <- st_as_stars(db_rs)%>%st_as_sf(merge = TRUE)
      if(rscrs==4326){bd_rs<-project(bd_rs,loc_coord); bd_sf<-st_crs(bd_sf, loc_coord)}
    } else{stop("the provided boundary format is not supported (need shp or tif extension)")}
    if (overlap_procedure==F){
      
    }
    
  }
}
