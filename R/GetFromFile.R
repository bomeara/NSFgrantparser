#pull from XML file saved from https://www.nsf.gov/awardsearch/download.jsp

GetFromFile <- function(file) {
  result <- XML::xmlToDataFrame(XML::xmlParse(file), stringsAsFactors = FALSE)
  return(result)
}
