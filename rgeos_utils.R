require(rgeos)
spArea <- function(spatialpolygons){
  sapply(seq_len(nrow(spatialpolygons)), function(x)gArea(spatialpolygons[x,]))
}


.spPredictate <- function(spdf1, spdf1.rowid, spdf2, spdf2.rowid, rgeosPredicate){
  spPredicateSingle <- function(spdf1, spdf1idx, spdf2, spdf2idx, rgeosPredicate){
    sp1 <- SpatialPolygons(Srl=spdf1@polygons[spdf1idx], proj4string=CRS(proj4string(spdf1)))
    sp2 <- SpatialPolygons(Srl=spdf2@polygons[spdf2idx], proj4string=CRS(proj4string(spdf2)))
    rgeosPredicate(sp1, sp2)
  }
  
  # Make sure the row ids are unique
  stopifnot(sum(duplicated(spdf1@data[,spdf1.rowid])) == 0)
  stopifnot(sum(duplicated(spdf2@data[,spdf2.rowid])) == 0)
  
  cjoin <- merge(x=data.frame(i1=1:nrow(spdf1),
                              spPredictate.nm1=spdf1@data[,spdf1.rowid]), 
                 y=data.frame(i2=1:nrow(spdf2),
                              spPredictate.nm2=spdf2@data[,spdf2.rowid]), 
                 by=c())
  cjoin$res <- apply(cjoin[,c(1, 3)], MARGIN=1, function(x){
    spPredicateSingle(spdf1, x[1], spdf2, x[2], rgeosPredicate)
  })
  names(cjoin) <- c("i1", spdf1.rowid, "i2", spdf2.rowid, "Result")
  cjoin[,c(2, 4, 5)]
}

# Specific instance of this for gCoveredBy
spCoveredBy <- function(spdf1, spdf1.rowid, spdf2, spdf2.rowid)
  .spPredictate(spdf1, spdf1.rowid, spdf2, spdf2.rowid, gCoveredBy)
spDistance <- function(spdf1, spdf1.rowid, spdf2, spdf2.rowid)
  .spPredictate(spdf1, spdf1.rowid, spdf2, spdf2.rowid, gDistance)
spIntersects <- function(spdf1, spdf1.rowid, spdf2, spdf2.rowid)
  .spPredictate(spdf1, spdf1.rowid, spdf2, spdf2.rowid, gIntersects)

gIntersectionArea <- function(spp1, spp2){
  isect <- gIntersection(spp1, spp2) 
  if (is.null(isect)) 0 else gArea(isect)
}

spIntersectionArea <- function(spdf1, spdf1.rowid, spdf2, spdf2.rowid)
  .spPredictate(spdf1, spdf1.rowid, spdf2, spdf2.rowid, gIntersectionArea)


spDissolve <- function(spdf, dissolvefield=NULL, FUN=function(x)data.frame(id=NA)){
  if(is.null(dissolvefield)){
    polys <- gUnionCascaded(spgeom=SpatialPolygons(spdf@polygons, proj4string=CRS(proj4string(spdf))))
    df <- FUN(spdf@data)
    SpatialPolygonsDataFrame(Sr=polys, data=df, match.ID=F)
  } else {
    # manually emulatie ddply-like behaviour
    uniq.disfields <- unique(spdf@data[,dissolvefield])
    for (i in seq_along(uniq.disfields)){
      if (is.na(uniq.disfields[i])){
        subs <- subset(spdf, is.na(spdf@data[,dissolvefield]))
      } else {
        subs <- subset(spdf, spdf@data[,dissolvefield] == uniq.disfields[i])
      }
      polys <- gUnionCascaded(spgeom=SpatialPolygons(subs@polygons, proj4string=CRS(proj4string(subs))))
      # as(subs, "SpatialPolygons")) - this seems to fill polygon holes!
      polys@polygons[[1]]@ID <- as.character(i)
      df <- FUN(subs@data)
      spdfpart <- SpatialPolygonsDataFrame(Sr=polys, data=df, match.ID=F)
      if (i == 1){
        spdfres <- spdfpart
      } else {
        spdfres <- rbind(spdfres, spdfpart)
      }
    }
    spdfres
  }
}


