# Taking functions from project linkedModels
# Following directions from https://r-pkgs.org/whole-game.html
# USe

#'Extract data from the PIT tag database
#'
#'@param drainage Which drainage, "west" or "stanley"#'Extract data from the PIT tag database
#'@param addCMR add CMR data or not, default is FALSE. This is a placeholder pending decision on how to deal with CMR data
#'@return a data frame
#'@export

getCoreData <- function(drainage = "west", addCMR = FALSE){

  createCoreData(
      sampleType = "electrofishing",  #"stationaryAntenna","portableAntenna"
      columnsToAdd = c("sampleNumber",
                       "river",
                       "survey",
                       "observedLength",
                       "observedWeight"),
      includeUntagged = TRUE,
      whichDrainage = drainage
    ) %>%
    addTagProperties(
      columnsToAdd = c("cohort",
                       "species",
                       "dateEmigrated",
                       "sex",
                       "species"
      )
    ) %>%
    dplyr::filter(species %in% c( "bkt","bnt","ats"),
                  area %in% c("trib","inside","below","above"),
                  !is.na(sampleNumber)) %>%
    #  createCmrData( maxAgeInSamples=20 ) %>%
    addSampleProperties() %>%
    addEnvironmental()
}

