#' Search for taxon, including its descendent taxa (by default)
#' @param taxon A taxon name
#' @param data Data.frame from GetAllDataFromMultipleYears
#' @param other.names A vector of other names you want to search for, typically common names
#' @param db See ?taxize::downstream
#' @param descendant.ranks Vector of ranks to look for.
#' @return A data.frame with matches.
#' @export
SearchForTaxon <- function(taxon, data, other.names=NULL, db="itis", descendant.ranks = c( "family", "tribe")) {
	all.taxon.names <- c()
	for (i in sequence(length(descendant.ranks))) {
		local.downstream  <- taxize::downstream(taxon, db=db, downto=descendant.ranks[i])
		if (nrow(local.downstream[[1]])>0) {
			all.taxon.names <- c(all.taxon.names, local.downstream[[1]]$childtaxa_name)
		}
	}
	search.string <- paste(all.taxon.names, collapse="|")
	if(!is.null(other.names)) {
		search.string <- paste(search.string, paste(other.names, collapse="|"), collapse="|")
	}
	result <- subset(data, grepl(search.string, paste(data$Award.AwardTitle, data$Award.AbstractNarration), ignore.case=TRUE))
	return(result)
}

#' Search for grants in the area of systematics
#' @param data Data.frame from GetAllDataFromMultipleYears
#' @param terms Vector of terms to look for
#' @return A data.frame with matches.
#' @export
SearchForPhylo <- function(data, terms = c("systematics", "phylogen", "taxonom", "tree of life")) {
	result <- FilterForString(paste(terms, collapse="|"), data)
	return(result)
}

#' Pull out funding data for a set of clades
#' @param clade.names Vector of clade names
#' @param clade.other.names Optional list of vectors with other clade names ("beetle" for Coleoptera for example)
#' @param data Data.frame from GetAllDataFromMultipleYears
#' @return List of data.frames with matches.
#' @export
GetAllGrantDataForClades <- function(clade.names, clade.other.names=NULL, data=LoadPackageData()) {
	result.list <- list(rep(NA, length(clade.names)))
	for (clade.index in sequence(length(clade.names))) {
		clade.result <- SearchForTaxon(clade.names[clade.index], data=data, other.names=clade.other.names[[clade.index]])
		result.list[[clade.index]] <- clade.result
		names(result.list)[clade.index] <- clade.names[clade.index]
	}
	return(result.list)
}

#' Extract taxon names
#' @param data Data.frame from GetAllDataFromMultipleYears
#' @return list of lists of names
#' @export
ExtractTaxonNames <- function(data) {
	taxon.names <- list(rep(NA, nrow(data)))
	for (i in sequence(nrow(data))) {
		try(taxon.names[[i]] <- rphylotastic::GetScientificNamesFromText(paste(as.character(data$Award.AwardTitle[i]), as.character(data$Award.AbstractNarration[i]))))
	}
	return(taxon.names)
}
