HSImodelPredict <- function(modelHSI,
                            modLayers,
                            currentTime,
                            pathOut){
  
  message(crayon::green(paste0("Predicting IK ranks for year ", currentTime)))
  totalCellsLayers <- raster::ncell(modLayers)
  modelData <- data.table(pixelID = 1:totalCellsLayers, getValues(modLayers))
  modelData <- na.omit(modelData)
  pixelID <- modelData$pixelID
  modelData[, pixelID := NULL]
  ranks <- predict(modelHSI, newdata = modelData) # Prediction for the NWT might take about an hour
  rankValues <- data.table(pixelID = pixelID, ranks = as.numeric(ranks$ypred))
  fullRankValues <- merge(data.table(pixelID = 1:totalCellsLayers), rankValues, on = "pixelID", all.x = TRUE)
  setkey(fullRankValues, "pixelID")
  predictedRankMap <- raster::setValues(modLayers[[1]], values = fullRankValues[["ranks"]])
  writeRaster(predictedRankMap, file.path(pathOut, paste0("IK_basedHSI_year", currentTime)), 
              format = "GTiff", overwrite = TRUE)
  predictedRankMap <- raster::raster(file.path(pathOut, paste0("IK_basedHSI_year", 
                                                               currentTime, ".tif")))
  return(predictedRankMap)
}
