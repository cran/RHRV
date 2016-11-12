selectEpisodes <-
function(Episodes, Tags, Indexes) {	
#-----------------------------
# select Episodes from record
#-----------------------------
#	Tags -> Vector containing types of episodes
  #	Indexes -> Vector containing indexes of episodes
#    It adds a new column Episodes$selected
#    After using remove the column with HRVData$Episodes$selected <- NULL

	if (!is.null(Tags)) {
		if (Tags[1]=="all") {
			Episodes$selected <- TRUE
			return(Episodes)
		} 
	}

	if (!is.null(Indexes)) {
		if (Indexes[1]=="all") {
			Episodes$selected <- TRUE
			return(Episodes)
		} 
	}


   	Episodes$selected <- FALSE

   	if (!is.null(Tags)) {
   		Tags <- Tags[is.element(Tags,Episodes$Type)]
   		if (length(Tags) != 0) {
   			Episodes[is.element(Episodes$Type,Tags),]$selected <- TRUE
   		}
   	}
   	if (!is.null(Indexes)) {
   		Indexes <- Indexes[Indexes<=length(Episodes$Type)]
   		if (length(Indexes) != 0) {
   		  Episodes[Indexes,]$selected <- TRUE
   		}
   	}
   	
   	
   	
   	return(Episodes)
}

