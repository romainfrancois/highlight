
private <- new.env()

.findExternalHighlight <- function(){
	highlight_cmd <- Sys.which( "highlight" )
	private[["has_highlight"]] <- highlight_cmd != ""
	private[["highlight"]] <- highlight_cmd
}

