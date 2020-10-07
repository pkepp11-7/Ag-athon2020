#install_github("filipematias23/FIELDimageR", type="source")
#unzip("FIELDimageR-master.zip")
#file.rename("FIELDimageR-master", "FIELDimageR")
#shell("R CMD build FIELDimageR")
#install.packages("FIELDimageR_0.2.1.tar.gz", repos= NULL, type="source")

library(FIELDimageR)
library(raster)
library(agricolae)
library(openxlsx)


########Vegetation Indices########
#Workshop : Normalized Difference Vegetation Index#
#workshop : Normalized Difference Red Edge Index#


files = list.files("../Datasets")
plotImage = stack(paste("../Datasets/", files[1], sep=""))
plotImage.crop = fieldCrop(plotImage)
plotImage.rotate = fieldRotate(plotImage.crop, clockwise = FALSE)

plotImage_custom_mask = plotImage.rotate[[3]] - plotImage.rotate[[2]] < -30
#plotImage_custom_mask = plotImage.rotate[[3]] - plotImage.rotate[[2]] < 0
plotImage.rmsoil = fieldMask(plotImage.rotate, Red = 1, Green = 2, NIR = 3, mask = plotImage_custom_mask, cropValue = 0, cropAbove = TRUE)
plot_mat = read.xlsx(paste("../Hackathon_data/",files[6], sep=""),colNames = FALSE, sheet = 1)
dims = dim(plot_mat)
map = as.matrix(plot_mat)

map2fieldData = function(mat)
{
  
  Column = character(length = nrow(mat) * ncol(mat))
  Row = character(length = nrow(mat) * ncol(mat))
  Plot = character(length = nrow(mat) * ncol(mat))
  for(i in 1:nrow(mat))
  {
    for(j in 1:ncol(mat))
    {
      Column[(i-1) * ncol(mat) + j] = j
      Row[(i-1) * ncol(mat) + j] = i
      Plot[(i-1) * ncol(mat) + j] = mat[i,j]
    }
  }
  df = data.frame(Column, Row, Plot)
  return(df)
}
Data = map2fieldData(map)

rotate = function(x) t(apply(x,2,rev))
map = rotate(rotate(map))

plot_shape = fieldShape(mosaic = plotImage.rmsoil$newMosaic, ncols = dims[2], nrows = dims[1], fieldData =Data, ID="Plot", fieldMap=map)

#View(plot_shape$fieldShape@data)
#plot_shape$fieldShape@data = plot_shape$fieldShape@data[-c(20,42),]

plotImage.indices = fieldIndex(mosaic = plotImage.rmsoil$newMosaic, Red = 1, Green = 2, NIR = 3, index = c("NDVI", "GNDVI"))

plotImage.Info = fieldInfo(mosaic = plotImage.indices[[c("NDVI","GNDVI")]], fieldShape = plot_shape$fieldShape, buffer = -0.10, n.core=4)

export_data = plotImage.Info$fieldShape@data
export_data = export_data[which(export_data$PlotName != "Filler"),]

Panel = character(length=nrow(export_data))
Plot_ID = character(length = nrow(export_data))

for(r in 1:nrow(export_data))
{
  strings = strsplit(export_data$Plot[r], "-")[[1]]
  if(strings[1] == '1')
  {
    Panel[r] = "ADP"
  }
  else
  {
    Panel[r] = "DDP"
  }
  Plot_ID[r] = strings[2]
}

Year = rep("2016", nrow(export_data))
export_data = data.frame(Year, Panel, Plot_ID, export_data$NDVI, export_data$GNDVI)
colnames(export_data) = c("Year", "Panel", "Plot_ID", "NDVI", "GNDVI")
namePrefix = substr(files[1], 1, 10)
write.xlsx(export_data, paste(namePrefix,"-VegIndex.xlsx", sep=""), colNames = TRUE)
