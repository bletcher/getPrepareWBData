# Taking functions from project linkedModels
# Following directions from https://r-pkgs.org/whole-game.html
# USe

#'from 'getAndPrepareDataWB.R' from project linkedModels
#'Clean data from the PIT tag database
#'

#'@param d Which drainage, "west" or "stanley"
#'@return a data frame
#'@export

testInteger <- function(d){

  return(d)
}

#'from 'getAndPrepareDataWB.R' from project linkedModels
#'Clean data from the PIT tag database
#'
#'@param d dataframe created with getCoreData()
#'@param drainageIn Which drainage, "west" or "stanley"
#'@return a data frame
#'@export

cleanData <- function(d, drainageIn){
  require(lubridate)
  require(getWBData)
  reconnect()
  # some formatting fixes
  d$sectionOriginal <- d$section
  d$section <- as.numeric(d$section)

  if(drainageIn == "west") {
    maxSectionNum <- 47
    d$riverOrdered <- factor(d$river,levels=c('west brook', 'wb jimmy', 'wb mitchell',"wb obear"),labels = c("west brook","wb jimmy","wb mitchell","wb obear"), ordered=T)
    minYear = min(d$year) #1997
  }
  else if(drainageIn == "stanley"){
    maxSectionNum <- 50
    d$riverOrdered <- factor(d$river,levels=c('mainstem', 'west', 'east'),labels = c('mainstem', 'west', 'east'), ordered=T)
    minYear = min(d$year) #2006
  }

  d$inside <- ifelse( d$section %in% 1:maxSectionNum | d$survey == "stationaryAntenna", T, F )

  d$year <- year(d$detectionDate)
  d$yday <- yday(d$detectionDate)

  d$ageInSamples <- (d$year - d$cohort) * 4 + (d$season - 2)
  d$isYOY <- ifelse( d$ageInSamples <= 3, TRUE, FALSE )

  dUntagged <- d %>%
    filter(is.na(tag)) %>%
    mutate(minSample = min(sampleNumber),
           maxSample = max(sampleNumber),
           minYear = minYear,
           moveDir = 0,
           sampleInterval = 0)

  d <- d %>%
    filter(!is.na(tag)) %>%
    group_by(tag) %>%
    # arrange(tag,sampleNumber) %>%
    mutate( lagSection = lead(section),
            distMoved = section - lagSection,
            lagObservedWeight = lead(observedWeight),
            lagObservedLength = lead(observedLength),
            grWeight = exp(lagObservedWeight - observedWeight)/as.numeric((lagDetectionDate - detectionDate)),
            grLength = (lagObservedLength - observedLength)/as.numeric((lagDetectionDate - detectionDate)),
            minSample = min(sampleNumber),
            maxSample = max(sampleNumber),
            minYear = minYear) %>%
    ungroup()

  d$moveDir <- ifelse( d$section == d$lagSection, 0, ifelse( d$section > d$lagSection, 1,-1 ) )
  d$sampleInterval <- as.numeric(d$lagDetectionDate - d$detectionDate)

  d <- bind_rows(d,dUntagged)

  return(d)
}


#'Get data from sites table
#'
#'@param drainage Which drainage, "west" or "stanley"
#'@return a data frame
#'@export

getSites <- function(drainageIn = "west"){
  require(getWBData)
  # get sites table
  sitesIn <- data.frame(tbl(conDplyr,"data_sites") )
  sites <- sitesIn %>%
    filter(is.na(quarter) & !is.na(quarter_length) & drainage == drainageIn) %>%
    dplyr::select(-quarter)
  sites$section <- as.numeric(sites$section)
  return(sites)
}


#'Merge sites table
#'
#'@param d dataframe created with getCoreData()
#'@param drainage Which drainage, "west" or "stanley"
#'@return a data frame
#'@export

mergeSites <- function(d, drainageIn){
  sites <- getSites(drainageIn)
  # merge in riverMeter for sections
  d <- left_join(d, sites, by = c("river","section","area"))
  d$riverMeter <- ifelse(d$survey == "shock" | d$survey == "portableAntenna", d$river_meter, d$riverMeter)
  return(d)
}


#'Get counts and summed mass of all fish, including untagged
#'
#'@param drainage Which drainage, "west" or "stanley"#'Extract data from the PIT tag database
#'@return a data frame
#'@export

getCounts_AllFish <- function(drainage = "west", filteredAreas = c("inside","trib")){
  require(getWBData)

  cdWBAll <- createCoreData(sampleType = "electrofishing", #"stationaryAntenna","portableAntenna"),
                            whichDrainage = drainage,
                            columnsToAdd=c("sampleNumber","river","riverMeter","survey",'observedLength','observedWeight'),
                            includeUntagged = T) %>%
    addTagProperties( columnsToAdd = c("cohort","species","dateEmigrated","sex","species")) %>%
    dplyr::filter( area %in% c("trib","inside","below","above"), !is.na(sampleNumber) ) %>%
    # createCmrData( maxAgeInSamples = 20, inside = F, censorDead = F, censorEmigrated = T) %>%
    addSampleProperties() %>%
    # addEnvironmental() %>%
    # addKnownZ() %>%
    # fillSizeLocation(size = F) #assumes fish stay in same location until observed elsewhere
    filter( species %in% c('bkt','bnt','ats'), observedLength > 60)

  cdWBAll$riverOrdered <- factor(cdWBAll$river,levels=c('west brook', 'wb jimmy', 'wb mitchell',"wb obear"),labels = c("west brook","wb jimmy","wb mitchell","wb obear"), ordered=T)
  cdWBAll$riverN <- as.numeric(cdWBAll$riverOrdered)

  # add in isYOY for all fish
  cdWBAll2 <- cdWBAll %>%
    mutate( ageInSamples = (year-cohort) * 4 + (season - 2) )

  cdWBAll2$isYOY <- ifelse( cdWBAll2$ageInSamples <= 3, 1, 2 )

  counts <- cdWBAll2 %>%
    filter( area %in% filteredAreas ) %>%
    group_by( isYOY,species,river,season,year ) %>%
    summarize( nAllFishBySpeciesYOY = n(),
               massAllFishBySpeciesYOY = sum(observedWeight,na.rm=T))

  ggplot(counts, aes(year,nAllFishBySpeciesYOY,color=species)) +
    # ggplot(counts, aes(year,massAllFishBySpecies,color=species)) +
    geom_point() +
    geom_line() +
    labs( x = "Year", y = "Estimated count") +
    facet_grid( river ~ season+isYOY, scales = "free")

  return(counts)
}

