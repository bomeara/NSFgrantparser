#pull from XML file saved from https://www.nsf.gov/awardsearch/download.jsp

GetFromFile<- function(file) {
  result <- data.frame(t(unlist(XML::xmlToList(XML::xmlParse(file)))), stringsAsFactors=FALSE)
  return(result)
}

#table(unlist(lapply(aa, names))) #aa is list of all returns

# GetAllDataFromYear <- function(year, verbose=TRUE) {
#   tmp <- tempfile()
#   curl::curl_download(paste0("https://www.nsf.gov/awardsearch/download?DownloadFileName=", year,"&All=true"), tmp)
#   all.files <- unzip(tmp, list=FALSE)
#   result <- GetFromFile(all.files[1])
#   if(verbose) {
#     print(paste("File 1 of", length(all.files),"for year", year))
#   }
#   #result <- result[,!is.na(names(result))]
#   for (i in 2:length(all.files)) {
#   #  print(dim(result))
#     y <- GetFromFile(all.files[i])
#   #  y <- y[,!is.na(names(y))]
#   if(verbose & i%%1000==0) {
#     print(paste("File",i,"of", length(all.files),"for year", year))
#   }
#     merge.names <- intersect(names(result), names(y))
#     merge.names <- merge.names[!is.na(merge.names)]
#     if (length(merge.names)>0) {
#       result <- merge(result, y, by=merge.names, all=TRUE)
#     }
#   }
#   return(result)
# }

GetAllDataFromYear <- function(year, verbose=TRUE) {
  original.dir <- getwd()
  setwd(tempdir())
  tmp <- tempfile()
  curl::curl_download(paste0("https://www.nsf.gov/awardsearch/download?DownloadFileName=", year,"&All=true"), tmp)
  if(verbose) {
    print(paste("Starting year", year))
  }
  all.files <- unzip(tmp, list=FALSE)
  bad.files <- c()
  result.list <- list(rep(NA, length(all.files)))
  for (i in sequence(length(all.files))) {
    if(verbose & i%%1000==0) {
      print(paste("File",i,"of", length(all.files),"for year", year))
    }
    local.file.result <- NULL
    try(local.file.result <- GetFromFile(all.files[i]))
    if(!is.null(local.file.result)) {
      result.list[[i]] <- local.file.result
    } else {
      bad.files <- i
    }
  }
  if(length(bad.files)>0) {
    result.list <- result.list[-bad.files]
  }
  result <- plyr::rbind.fill(result.list)
  result <- result[,!grepl("Email", names(result), ignore.case=TRUE)] #prevent spam
  setwd(original.dir)
  return(result)
}

#' Download data from NSF for all grants
#' @param years Numeric vector of years
#' @param save.dir Path to where to save files
#' @return A data.frame with matches.
#' @export
GetAllDataFromMultipleYears <- function(years=seq(from=2017, to=1959, by=-1), save.dir=getwd()) {
  result <- GetAllDataFromYear(years[1])
  write.csv(result, file=paste0(save.dir, "/NSFgrants_",years[1], ".csv"))
  print(paste("There are", nrow(result),"grants recorded for", years[1]))
  Sys.sleep(60)
  if (length(years)>1) {
    for (year.index in 2:length(years)) {
      local.result <- GetAllDataFromYear(years[year.index])
      write.csv(local.result, file=paste0(save.dir, "/NSFgrants_",years[year.index], ".csv"))
      print(paste("There are", nrow(local.result),"grants recorded for", years[year.index]))
      merge.names <- intersect(names(result), names(local.result))
      result <- merge(result, local.result, by=merge.names, all=TRUE)
    }
  }
  write.csv(result, file=paste0(save.dir, "/NSFgrants_AllSearchedYears.csv"))
  return(result)
}

RemoveEmails <- function() {
  files <- system("ls -1 NSFgrants_*", intern=TRUE)
  files <- files[!grepl("NSFgrants_AllSearchedYears.csv", files)]
  for (i in sequence(length(files))) {
    local.file <- read.csv(files[i])
    local.file <- local.file[,!grepl("Email", names(local.file), ignore.case=TRUE)] #prevent spam
    write.csv(local.file, file=files[i])
  }
}

AssignYear <- function(x) {
  x$Year <- as.numeric(t(sapply(strsplit(x$Award.AwardEffectiveDate, "/"), "[", 1:3))[,3])
  return(x)
}


#' Load data saved in inst/extdata
#' @param min.year First year of data to pull in
#' @param max.year Last year of data to to pull in
LoadPackageData <- function(min.year=1959, max.year=2017) {
  result <- data.frame()
  for (year in min.year:max.year) {
    local.result <- read.csv(system.file("extdata", paste0("NSFgrants_", year, ".csv"), package = "NSFgrantparser"))
    if(year==min.year) {
      result <- local.data
    } else {
      merge.names <- intersect(names(result), names(local.result))
      result <- merge(result, local.result, by=merge.names, all=TRUE)
    }
  }
  return(result)
}


#look at plotting like http://www.axismaps.com/blog/2014/10/geography-of-jobs-animated-mapping-with-d3/
