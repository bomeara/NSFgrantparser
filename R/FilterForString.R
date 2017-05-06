#' Search for a single string in award titles and abstract
#' @param string Term to search for
#' @param data Data.frame from GetAllDataFromMultipleYears
#' @return A data.frame with matches.
#' @export
FilterForString <- function(string, data) {
	result <- subset(data, grepl(string, paste(data$Award.AwardTitle, data$Award.AbstractNarration), ignore.case=TRUE))
	return(result)
}
