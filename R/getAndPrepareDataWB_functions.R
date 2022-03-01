# Taking functions from project linkedModels
# Following directions from https://r-pkgs.org/whole-game.html
# USe

#'Extract data from the PIT tag database
#'
#'@param drainage Which drainage, "west" or "stanley"#'Extract data from the PIT tag database
#'@return a data frame
#'@export

getCoreData <- function(drainage = "west"){

  cdWB <- createCoreData(sampleType = "electrofishing", #"stationaryAntenna","portableAntenna"),
                         whichDrainage = drainage,
                         columnsToAdd=c("sampleNumber","river","riverMeter","survey",'observedLength','observedWeight')) %>%
    addTagProperties( columnsToAdd = c("cohort","species","dateEmigrated","sex","species")) %>%
    dplyr::filter( !is.na(tag), area %in% c("trib","inside","below","above"), !is.na(sampleNumber) ) %>%
    createCmrData( maxAgeInSamples = 20, inside = F, censorDead = F, censorEmigrated = T) %>%
    addSampleProperties() %>%
    addEnvironmental() %>%
    addKnownZ() %>%
    fillSizeLocation(size = F) #assumes fish stay in same location until observed elsewhere
}
