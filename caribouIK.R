## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "caribouIK",
  description = paste0("Module to fit and predict caribou habitat suitability index", 
                       "based on Indigenous Knowledge"),
  keywords = c("Caribou", "population", "Indigenous knowledge"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "Eliot.McIntire@canada.ca", role = c("aut", "cre")),
              person("Frances", "Stewart", email = "frances.stewart@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9004", caribouIK = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "caribouIK.Rmd")),
  reqdPkgs = list("data.table", "ggplot2", "PredictiveEcology/pemisc", "tati-micheletti/usefulFuns", "ordinalForest", "PredictiveEcology/LandR"), 
  parameters = rbind(
    defineParameter("predictLastYear", "logical", TRUE, NA, NA, paste0("If last year of simulation is not multiple of",
                                                                       " predictionInterval, should it predict for the last year too?")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "inital plot time"),
    defineParameter(".plotTimeInterval", "numeric", 10, NA, NA, "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, "Should use dummy data? Automatically set"),
    defineParameter("recoveryTime", "numeric", 40, NA, NA, "Time to recover the forest enough for caribou"),
    defineParameter("predictionInterval", "numeric", 10, NA, NA, "Time between predictions"),
    defineParameter(name = "baseLayer", class = "numeric", default = 2005, min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05 or LCC10?"),
    defineParameter(name = "decidousSp", class = "character", default = c("Betu_Pap", "Popu_Tre", "Popu_Bal"), 
                    min = NA, max = NA, desc = "Deciduous species to be considered for caribou"),
    defineParameter(name = "oldBurnTime", class = "numeric", default = 40, 
                    min = NA, max = NA, desc = "Threshold for oldBurn/newBurn. Max oldburn + 20"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = "Internal. Can be names of events or the whole module name; these will be cached by SpaDES"),
    defineParameter("overwriteResults", "logical", FALSE, NA, NA,
                    desc = "Should available results be overwritten? If FALSE, existing will be returned"),
    defineParameter("subsetForModel", "logical | numeric", 50, NA, NA,
                    desc = "Should data for the model be subsetted? If FALSE, existing will be returned")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer",
                 desc = paste0("Map of groups of pixels that share the same info from cohortData (sp, age, biomass, etc).",
                               "Here is mainly used to determine old and recent burns based on tree age,",
                               " and if deciduous by species")),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = paste0("data.table with information by pixel group of sp, age, biomass, etc")),
    expectsInput(objectName = "roadDensity", objectClass = "RasterLayer",
                 desc = paste0("Layer that maps a 10km buffer on each road.", 
                               "This layer is static if no modules are forecasting anthropogenic disturbances"), 
                 sourceURL = "https://drive.google.com/open?id=1C0Y0z1cgQKwa3_-X2qWrhNIzEHIl9m5e"),
    expectsInput(objectName = "anthropogenicLayer", objectClass = "RasterLayer", 
                 desc = "Raster with road buffered disturbances", 
                 sourceURL = "https://drive.google.com/open?id=1zj7zo8NBNhxxHMUL4ZnKTaP3niuQEI1m"),
    expectsInput(objectName = "Elevation", objectClass = "RasterLayer", 
                 desc = "Raster with elevation values", 
                 sourceURL = "https://drive.google.com/open?id=1SKnXVqUD10_VdemQaPaz9MrWiNZzK7VY"),
    expectsInput(objectName = "Vrug", objectClass = "RasterLayer", 
                 desc = "Raster with elevation values", 
                 sourceURL = "https://drive.google.com/open?id=16u07GpGQbBd5Yh8xPZ_xLiUo31OF0uDP"),
    expectsInput(objectName = "rstLCC", objectClass = "RasterLayer", 
                 desc = "This will give us both shrub and herb layers", 
                 sourceURL = ""),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Study area for the prediction. Currently only available for NWT", 
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it", 
                 sourceURL = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df"),
    expectsInput(objectName = "reclassLCC05", objectClass = "data.table",
                 desc = "Table 41 from ECCC report converting LCC05 classes", 
                 sourceURL = "https://drive.google.com/open?id=1pMfkIoqFoxwICMlend_mNuwNMGA5_6Fr"),
    expectsInput(objectName = "IKLayer", objectClass = "RasterLayer | SpatialPolygonDataFrame",
                 desc = "This can be a raster or a shapefile with rank information on caribou areas", 
                 sourceURL = "")
  ), 
  outputObjects = bindrows(
    createsOutput(objectName = "modelHSI", objectClass = "", # TODO decide the class here  
                  desc = paste0("Caribou habitat suitability model based on ranks defined by ",
                                "Indigenous Knowledge, using static (Elevation, Vrug, Shrub, Herb) ",
                                "and dynamic layers (RoadDensity, Deciduous, Water, RecentBurn, OldBurn)")),
    createsOutput(objectName = "habitatSuitabilityIndex", objectClass = "list", 
                  desc = "List of rasters per year, indicating the probability of presence of Caribous"),
    createsOutput(objectName = "modLayers", objectClass = "RasterStack", 
                  desc = "Stack of all dynamic layers: oldBurn, newBurn, biomassMap, roadDensity, waterRaster"),
    createsOutput(objectName = "listSACaribou", objectClass = "list", 
                  desc = paste0("List of caribou areas to predict for",
                                " Currently the default is 3 shapefiles: Edehzhie, range planning, herds")),
    createsOutput(objectName = "habitatSuitabilityIndex", objectClass = "list", 
                  desc = paste0("List of YearXXXX of caribou HSI predictions"))
  )
))

doEvent.caribouIK = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      Require("magrittr")
      mod$cohortData <- usefulFuns::createModObject(data = "cohortData", sim = sim, 
                                                pathInput = inputPath(sim), currentTime = start(sim))
      mod$pixelGroupMap <- usefulFuns::createModObject(data = "pixelGroupMap", sim = sim, 
                                                   pathInput = inputPath(sim), currentTime = start(sim))
      
      if (any(is.null(mod$pixelGroupMap), is.null(mod$cohortData))) {
        params(sim)$caribouIK$.useDummyData <- TRUE
      }
      
      if (isTRUE(P(sim)$.useDummyData)){
        stop("This module does not work without data. Please provide the necessary layers")
      } else {
        if (is.null(sim$modLayers)){
          sim$modLayers <- list()
        }
        
        sim$modLayers[["Initial"]] <- Cache(usefulFuns::getLayers, currentTime = start(sim),
                                            startTime = start(sim),
                                            endTime = end(sim),
                                            cohortData = mod$cohortData, # Has age info per pixel group
                                            pixelGroupMap = mod$pixelGroupMap,
                                            recoveryTime = P(sim)$recoveryTime,
                                            listSACaribou = sim$listSACaribou,
                                            anthropogenicLayer = sim$anthropogenicLayer,
                                            roadDensity = sim$roadDensity,
                                            waterRaster = sim$waterRaster,
                                            isRSF = TRUE,
                                            decidousSp = P(sim)$decidousSp,
                                            oldBurnTime = P(sim)$oldBurnTime,
                                            elevation = sim$Elevation,
                                            vrug = sim$Vrug,
                                            LCC05 = sim$rstLCC,
                                            reclassLCC05 = sim$reclassLCC05,
                                            rasterToMatch = sim$rasterToMatch,
                                            userTags = c("modLayersInitial", "caribouIK", "getLayers"), 
                                            omitArgs = "useCache")
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouIK", "fittingModel")
      sim <- scheduleEvent(sim, start(sim), "caribouIK", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "caribouIK", "predictingCaribou")
      sim <- scheduleEvent(sim, end(sim), "caribouIK", "plot", eventPriority = .last()) #P(sim)$.plotInitialTime
    },
    fittingModel = {
      sim$modelHSI <- Cache(HSImodelFit, modLayers = 
                              sim$modLayers[["Initial"]][[paste0("Year", time(sim))]], 
                            #TODO this should not be a list of time. Need to modify in prev fun 
                            IKLayer = sim$IKLayer, subsetForModel = P(sim)$subsetForModel,
                            userTags = c("modelHSI", paste0("subset:", P(sim)$subsetForModel), "goal:fitting"))
    },
    gettingData = {
      Require("magrittr")
      mod$cohortData <- usefulFuns::createModObject(data = "cohortData", sim = sim, 
                                                pathInput = inputPath(sim), currentTime = time(sim))
      mod$pixelGroupMap <- usefulFuns::createModObject(data = "pixelGroupMap", sim = sim, 
                                                   pathInput = inputPath(sim), currentTime = time(sim))
      
      if (any(is.null(mod$pixelGroupMap), is.null(mod$cohortData))) {
        params(sim)$caribouIK$.useDummyData <- TRUE
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouIK", "gettingData")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouIK", "gettingData")
      }
    },
    predictingCaribou = {
      if (isTRUE(P(sim)$.useDummyData)){
        stop("This module does not work without data. Please provide the necessary layers")
      } else {
        if (is.null(sim$modLayers)){
          sim$modLayers <- list()
        }

        sim$modLayers <- usefulFuns::getLayers(currentTime = time(sim),
                                           startTime = start(sim),
                                           endTime = end(sim),
                                           cohortData = mod$cohortData, # Has age info per pixel group
                                           pixelGroupMap = mod$pixelGroupMap,
                                           recoveryTime = P(sim)$recoveryTime,
                                           listSACaribou = sim$listSACaribou,
                                           anthropogenicLayer = sim$anthropogenicLayer,
                                           roadDensity = sim$roadDensity,
                                           waterRaster = sim$waterRaster,
                                           isRSF = TRUE,
                                           decidousSp = P(sim)$decidousSp,
                                           oldBurnTime = P(sim)$oldBurnTime,
                                           elevation = sim$Elevation,
                                           vrug = sim$Vrug,
                                           LCC05 = sim$rstLCC,
                                           reclassLCC05 = sim$reclassLCC05,
                                           rasterToMatch = sim$rasterToMatch)
      }
      fls <- tryCatch({usefulFuns::grepMulti(x = list.files(outputPath(sim)), patterns = c("IK", time(sim)))}, error = function(e){
        return(NULL)
      })
      if (all(length(fls) > 0, P(sim)$overwriteResults)) {  # If we have the layers already, we return them if overwriteResults == FALSE
        sim$habitatSuitabilityIndex[[paste0("Year", time(sim))]] <- raster::raster(file.path(outputPath(sim), fls))

      } else {

        sim$habitatSuitabilityIndex[[paste0("Year", time(sim))]] <- HSImodelPredict(modelHSI = sim$modelHSI,
                                                                                    modLayers = sim$modLayers[[paste0("Year", time(sim))]],
                                                                                    currentTime = time(sim),
                                                                                    pathOut = outputPath(sim))
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouIK", "predictingCaribou")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouIK", "predictingCaribou")
      }
    },
    plot = {
      caribouHSI <- sim$habitatSuitabilityIndex[[paste0("Year", time(sim))]]
      Plot(caribouHSI, 
           title = "Caribou Habitat Suitability Index", time(sim))
      
      # schedule future event(s)
      if (time(sim) != end(sim))
        sim <- scheduleEvent(sim, end(sim), "caribouIK", "plot", eventPriority = .last())
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

  .inputObjects <- function(sim) {
    
    #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
    cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
    dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")
    
    cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
   
    if (!suppliedElsewhere(object = "studyArea", sim = sim)){
      sim$studyArea <- Cache(prepInputs,
                             url = extractURL("studyArea"),
                             destinationPath = dataPath(sim),
                             cloudFolderID = sim$cloudFolderID,
                             omitArgs = c("destinationPath", "cloudFolderID"))
    }
    
    if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
      sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"), 
                                 studyArea = sim$studyArea,
                                 targetFile = "RTM.tif", destinationPath = dataPath(sim), 
                                 overwrite = TRUE, filename2 = NULL,
                                 omitArgs = c("destinationPath", "cloudFolderID", "useCloud", "overwrite", "filename2"))
    }

    if (!suppliedElsewhere("waterRaster", sim)){
      sim$waterRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim), 
                               studyArea = sim$studyArea, lccLayer = P(sim)$baseLayer,
                               rasterToMatch = sim$rasterToMatch,
                               userTags = c("objectName:wetLCC"))
      
      waterVals <- raster::getValues(sim$waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
      waterVals[!is.na(waterVals) & waterVals != 1] <- 0
      sim$waterRaster <- raster::setValues(sim$waterRaster, waterVals)
    }
    
    if (!suppliedElsewhere("anthropogenicLayer", sim)){
      sim$anthropogenicLayer <- prepInputs(targetFile = "bufferMap_v0.1.0_m_r500_t0_anthrDisturb.grd",
                                           archive = "bufferMap_v0.1.0_m_r500_t0_anthrDisturb.zip",
                                           alsoExtract = "similar",
                                           url = "https://drive.google.com/open?id=1GhnIjmKsZ3JoxTjefeeBUb02iiEcV_qD",
                                           destinationPath = dataPath(sim), studyArea = sim$studyArea,
                                           overwrite = TRUE, 
                                           rasterToMatch = sim$rasterToMatch)
      
    }
    
    if (!suppliedElsewhere("roadDensity", sim)){
      sim$roadDensity <- prepInputs(targetFile = "roadDensity_BCR6_NWT_t0.tif",
                                    url = extractURL("roadDensity"),
                                    destinationPath = dataPath(sim), 
                                    studyArea = sim$studyArea,
                                    overwrite = TRUE, 
                                    rasterToMatch = sim$rasterToMatch)
    }
    
    if (!suppliedElsewhere("rstLCC", sim)){
      sim$rstLCC <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                        studyArea = sim$studyArea,
                                        rasterToMatch = sim$rasterToMatch)
    }
    if (!suppliedElsewhere("Elevation", sim)){
      sim$Elevation <- prepInputs(targetFile = "nadem100laz_BCR6_NWT.tif", 
                                  url = extractURL("Elevation"),
                                  destinationPath = dataPath(sim), studyArea = sim$studyArea,
                                  overwrite = TRUE, fun = "raster::stack",
                                  rasterToMatch = sim$rasterToMatch)
      
      
    }
    if (!suppliedElsewhere("Vrug", sim)){
      
      sim$Vrug <- prepInputs(archive = "vrug_bcr6.zip",
                             targetFile = "vrug_bcr6.tif",
                             url = extractURL("Vrug"),
                             destinationPath = dataPath(sim), studyArea = sim$studyArea,
                             overwrite = TRUE, 
                             rasterToMatch = sim$rasterToMatch)
      
    }
    if (!suppliedElsewhere("reclassLCC05", sim)){
      
      sim$reclassLCC05 <- prepInputs(targetFile = "Table41_ConvertLCC05.csv",
                                     url = extractURL("reclassLCC05"),
                                     destinationPath = dataPath(sim),
                                     overwrite = TRUE, fun = "data.table::fread")
    }
    
    if (!suppliedElsewhere(object = "listSACaribou", sim = sim)){
      
      caribouArea2 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV",
                            targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                            alsoExtract = "similar", overwrite = TRUE,
                            destinationPath = dataPath(sim), filename2 = "caribouArea2")
      
      Edehzhie <- Cache(prepInputs, targetFile = "Edehzhie.shp",
                        archive = "Edehzhie.zip",
                        alsoExtract = "similar", overwrite = TRUE,
                        url = "https://drive.google.com/open?id=1VP91AyIeGCFwJS9oPSEno4_SbtJQJMh7", 
                        studyArea = sim$studyArea,
                        destinationPath = dataPath(sim), filename2 = NULL,
                        rasterToMatch = sim$rasterToMatch)
      
      caribouArea1 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO",
                            targetFile = "NWT_Regions_2015_LCs_DC_SS_combined_NT1_clip_inc_Yukon.shp",
                            alsoExtract = "similar", overwrite = TRUE,
                            destinationPath = dataPath(sim), filename2 = "caribouArea1")
      
      sim$listSACaribou = list(sim$caribouArea1, sim$caribouArea2, sim$Edehzhie)
      names(sim$listSACaribou) <- c("caribouArea1", "caribouArea2", "Edehzhie")
    }
    
    if (!suppliedElsewhere("forestOnly", sim = sim, where = "sim")){
      
      forestClasses <- c(1:15, 34:35)
      sim$forestOnly <- sim$rasterToMatch
      sim$forestOnly[!sim$rstLCC[] %in% forestClasses] <- NA
    }
    
    if (!suppliedElsewhere("IKLayer", sim = sim, where = "sim")){
      message(crayon::red(paste0("IKLayer was not supplied. Using DUMMY data instead, for testing purposes, for the NWT.",
                                 " If your studyArea is NOT within the BCR6 in the NWT, this will fail.")))
      tryCatch({
        sim$IKLayer <- prepInputs(url = "https://drive.google.com/open?id=10o3r3U9yp26kALofziU9so4w08FzZeWG", 
                                  destinationPath = Paths$inputPath, rasterToMatch = sim$rasterToMatch,
                                  studyArea = sim$studyArea)
        
      }, error = function(e){
        sim$IKLayer <- prepInputs(url = "https://drive.google.com/open?id=10o3r3U9yp26kALofziU9so4w08FzZeWG", 
                                  destinationPath = Paths$inputPath, rasterToMatch = sim$rasterToMatch,
                                  studyArea = sim$studyArea, overwrite = TRUE)
        
      })
      }
    
    
    return(invisible(sim))
  }