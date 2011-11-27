
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

highlight_output_types <- function(){
    c("HTML","XHTML","TEX","LATEX","RTF","XML","ANSI","XTERM256",
        "HTML32", "SVG","BBCODE" )    
}

highlight_theme <- function( theme = "emacs" ){
    if( missing(theme) ){
        theme <- highlight_themes()[1L]
    } else {
        theme <- match.arg( theme, highlight_themes() )
    }
    system.file( "highlight", "themes", sprintf( "%s.style", theme ), package = "highlight" )
}

highlight_lang <- function( lang = highlight_supported_languages() ){
    if( missing(lang)){
        stop( "no language" )
    } else {
        lang <- match.arg(lang, highlight_supported_languages() )
    }
    system.file( "highlight", "langDefs", sprintf("%s.lang", lang), package = "highlight" ) 
}

highlight_type <- function(type = highlight_output_types() ){
    if( missing( type ) ){ type <- "HTML" }
    type <- match.arg( type, highlight_output_types() )
    match( type, highlight_output_types() ) - 1L
}

external_highlight <- function( file, 
    outfile = NULL, 
    theme = "kwrite",
    lang  = NULL , 
    type  = "HTML", 
    line_numbers = FALSE, 
    doc = TRUE, 
    code
){
        
    if( !missing(code) ){
        file <- sprintf( "%s.%s", tempfile(), lang )
        writeLines( code, file )    
    }
    type  <- highlight_type(type)
    theme <- highlight_theme(theme) 
    
    lang <- highlight_guess_language(file, lang = lang)
    lang <- highlight_lang(lang)
    
    is_null_outfile <- is.null(outfile)
    if( is_null_outfile ) outfile <- tempfile()
    HighlightMain( file, outfile, type, theme, lang, 
        isTRUE(line_numbers), 
        isTRUE(doc)
        )
    if( is_null_outfile ) readLines(outfile)
}


highlight_extensions <- function(){
    txt <- readLines( system.file( "highlight", "filetypes.conf", package = "highlight" ) )
    
    df <- do.call( rbind, lapply( grep( "^[$]ext" , txt, value = TRUE ), function(x) {
        
        extensions <- strsplit( sub( "^.*=", "",  x), " ")[[1]]
        language   <- sub("^.*[(](.*)[)].*$", "\\1", x  )
        
        data.frame( 
            lang = rep( language, length(extensions)+1L ), 
            ext = c( language, extensions ), 
            stringsAsFactors= FALSE 
       )
    } ) )
    
    
    files <- list.files( system.file( "highlight", "langDefs", package = "highlight" ), pattern = "[.]lang$" )
    languages <- sub( "[.]lang$", "", files )
    
    missings <- setdiff( languages, unique( df$lang ) )
    df <- rbind( df, data.frame( lang = missings, ext = missings, stringsAsFactors = FALSE ) )
    
    df <- df[ order(df$lang), ]
    
    
}

highlight_guess_language <- function(file, lang = NULL){
    if( is.null(lang)) lang <- sub( "^.*[.]([^.]*)$", "\\1", file )
    if( lang == "" ) stop( "no extension" ) 
    
    df <- highlight_extensions()
    id <- match( lang, df$ext )
    if( is.na(id ) ) stop( "unknown extension" )
   
    df[ id, "lang" ]
}

