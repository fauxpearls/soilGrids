## This code uses XML to access the WCS publication of SoilGrids250m.
## It asks for your variables, depths, and quantiles of interest. 
## You then need to define the bounding boxes for the areas of interest
## SoilGrids250m uses Homolsine projection, which is less common. For ease,
## this code takes bounding box coordinates in lat/long and reprojects them to 
## homolosine. 

library(XML)
library(rgdal)
library(gdalUtils)

## Select variables of interest
voi <- c("bdod", "clay", "nitrogen", "sand", "silt")
## Select desired depths
depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")
## Select desired quantiles
quantiles <- c("mean","Q0.5")

## Pick some names for your regions of interest, and keep them consistent
boxNames<-c("eastBrazil", "guyana", "venezuela", "centralBrazil", "westernBrazil")

##Set bounding boxes for each region, (xmin, xmax, ymin, ymax)
eastBrazilbbox<- extent(-52.5, -45, -5, 2.5)
guyanabbox<- extent(-62.5, -55, 0, 10)
venezuelabbox<- extent(-70, -62.5, 0, 5)
centralBrazilbbox<- extent(-67.5, -52.5, -12.5, -2.5)
westernBrazilbbox<- extent(-80, -67.5, -10, 0)

## Put those together in a list, in the same order as the names list
bboxes<-c(eastBrazilbbox, guyanabbox, venezuelabbox, centralBrazilbbox, westernBrazil)

## proj string for the WGS lat/long projection
## This is needed for reprojecting into homolosine
wgs = '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

# proj string for Homolosine projection, which SoilGrids uses
igh = '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'

## The first part of this loop converts each bounding box to homolosine
for(x in 1:length(bboxes)){
	currentBox<-bboxes[[x]]
	## Convert the extent to a polygon
	currentBoxPoly <- as(currentBox, 'SpatialPolygons')
	## and then to a spatial polygon data frame
	currentBoxSPDF <- as(currentBoxPoly, 'SpatialPolygonsDataFrame')
	## Set the CRS
	crs(currentBoxSPDF) <- wgs
	## Transform to homolosine
	ighCurrentShape<-spTransform(currentBoxSPDF, igh)
	## Pull out the corner coordinates to get the box in homolosine
	ighbbox<- c(xmin(ighCurrentShape), ymax(ighCurrentShape), xmax(ighCurrentShape), ymin(ighCurrentShape))
	
	## Now that we have the bounding box, we stay in the loop and 
	## download the maps for that bounding box
	for(variable in voi){
		for(depth in depths){
			for(quantile in quantiles){
				## This nested loop lets us build the unique filenames
				variableString<-paste(variable,depth,quantile,sep="_")
				# Path to the WCS. See maps.isric.org
				wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/", variable, ".map") 
				wcs_service = "SERVICE=WCS"
				# This works for gdal >=2.3; "VERSION=1.1.1" works with <2.3.
				wcs_version = "VERSION=2.0.1"
				# This works for gdal >= 2.3
				wcs = paste(wcs_path,wcs_service,wcs_version,sep="&")
				l1 <- newXMLNode("WCS_GDAL")
				l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
				l1.l <- newXMLNode("CoverageName", variableString, parent=l1)
				
				# Save to local disk
				xml.out = "./sg.xml"
				saveXML(l1, file = xml.out)
				
				## File path to save the GTIFF
				file.out <- paste0("./yourFolderPath/",boxNames[x],"_",variableString,".tif")
				
				## Translate to GTIFF
				gdal_translate(xml.out, file.out,
				    tr=c(250,250), projwin=ighbbox,
				    projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"),
				    verbose=TRUE
				)
			}
		}
	}
}


