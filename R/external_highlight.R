
private <- new.env()

.findExternalHighlight <- function(){
	highlight_cmd <- Sys.which( "highlight" )
	private[["has_highlight"]] <- highlight_cmd != ""
	private[["highlight"]] <- highlight_cmd
}

highlight_supported_languages <- function(){
    files <- list.files( 
        system.file( "highlight", "langDefs", package = "highlight" ),   
        pattern = "lang$" )
    gsub( "[.]lang$", "", files )
}

highlight_themes <- function(){
    files <- list.files( 
        system.file( "highlight", "themes", package = "highlight" ),   
        pattern = "style$" )
    gsub( "[.]style$", "", files )
}
