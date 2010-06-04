
private <- new.env()

.findExternalHighlight <- function(){
	private[["has_highlight"]] <- Sys.which( "highlight" ) != ""
}

