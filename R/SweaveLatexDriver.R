
#' @importFrom grDevices col2rgb
latex_color <- function( name = col, col  = "white"){
	sprintf( "\\definecolor{%s}{rgb}{%s}", name, paste(as.vector(col2rgb(col))/255, collapse = "," ) )
}

should_use_external_highlight <- function(options){
	any( c("lang", "file" ) %in% names(options) )	
}

#' @importFrom utils RweaveLatexOptions
HWeaveLatexCheckOps <- function(options){
	if( should_use_external_highlight(options) ){
		options
	} else {
	  if( "size" %in% names(options) ) {
			append( RweaveLatexOptions( options[ - which( names(options) == "size" ) ] ), list( size = options$size ) ) 
		} else RweaveLatexOptions( options )
	}
}

#' Sweave driver performing syntax highlighting
#' 
#' Sweave driver using the highlight latex renderer to perform syntax 
#' highlighting of input R code in sweave chunks.
#' 
#' This sweave driver is very similar to standard driver that is 
#' included in \samp{utils}. The difference is that input R code and 
#' verbatim output is rendered using \code{highlight} enabling 
#' syntax highlighting of R code. 
#' 
#' Instead of using \samp{Sinput} and \samp{Soutput} commands, this 
#' driver uses \samp{Hinput} and \samp{Houtput} and defines these commands
#' at the very beginning of the document, letting the user the option 
#' to overwrite them as necessary. 
#' 
#' Latex boxes defined by the latex renderer (\code{\link{renderer_latex}})
#' and style definitions needed are also written at the beginning 
#' of the document.
#' 
#' Because highlight does not use verbatim environments, the user
#' of this driver can freely redefine the \samp{Hinput}, \samp{Houtput}
#' and \samp{Hchunk} environments to achieve greater control
#' of the output latex document than with the standard driver.
#' 
#' @return A sweave driver, suitable for the \samp{driver} argument of
#' \code{\link[utils]{Sweave}} 
#' @examples
#' \dontrun{
#' # using the driver on the grid vignette
#' require( grid )
#' v <- vignette( "grid", package = "grid" )$file
#' file.copy( v, "grid.Snw" )
#' Sweave( "grid.Snw", driver= HWeaveLatex() )
#' }
#' @importFrom utils RweaveLatexSetup RweaveEvalWithOpt RweaveLatexFinish
#' @export
HWeaveLatex <- function( ) {
  # boxes=FALSE, bg = "#F2F2F2", border = "black"
  list(
	  setup      = HweaveLatexSetup,
    runcode    = HWeaveLatexRuncode ,
	  writedoc   = HWeaveLatexWritedoc,
    finish     = RweaveLatexFinish,
    checkopts  = HWeaveLatexCheckOps 
  )
}

HweaveLatexSetup <- function(boxes=FALSE, bg = "#F2F2F2", border = "black", ...){
  out <- RweaveLatexSetup(...)
  out$options <- append( out$options, list(boxes = boxes, bg = bg, border = border))
  out
}

#' remove the first line of the chunk if needed
#' @noRd
handle_first_line <- function(chunk){
  if( grepl( "#line [0-9]", chunk[1L] ) ){
    chunk <- chunk[-1L]
    attr(chunk, "srclines" ) <- attr(chunk, "srclines" )[-1L]
  }
  chunk
}

latex_size <- function(options){
  if( "size" %in% names(options) ) LATEX_SIZES[ pmatch( options$size, LATEX_SIZES) ] else "normalsize"
}

apply_size <- function(tex, size){
  gsub( "hlbox", sprintf( "hl%sbox", size ), tex, fixed = TRUE )   
}

wrap_size_chunk <- function(tex, size){
  tex <- c(
    sprintf( "\\begin{%s}", size ), 
    "\\begin{Hchunk}" , 
    apply_size(tex, size) ,
    "\\end{Hchunk}", 
    sprintf( "\\end{%s}", size )
  )
  tex
}

#' runcode with external highlight (andre version)
#' @noRd
runcode_external <- function(object, chunk, options){
  if( "file" %in% names(options) ){
    chunkfile <- options[["file"]]
  } else {
    lang <- options[["lang"]]
    chunkfile <- tempfile( fileext = paste0(".", lang) )
    chunk <- handle_first_line(chunk)
    writeLines( chunk, chunkfile )   
  }
  tex  <- external_highlight( chunkfile, outfile = NULL, type = "LATEX", doc = FALSE )
  size <- latex_size(options)
  tex  <- wrap_size_chunk(tex, size)
  writeLines( tex, object$output )
  return(object)  
}

#' show some debug information
#' @noRd
runcode_debug <- function(options){
  if( !options$quiet ){
    cat(formatC(options$chunknr, width=2), ":")
    if(options$echo) cat(" echo")
    if(options$keep.source) cat(" keep.source")
    if(options$eval){                                                             
      if(options$print) cat(" print")
      if(options$term) cat(" term")
      cat("", options$results)
      if(options$fig){
        if(options$eps) cat(" eps")
        if(options$pdf) cat(" pdf")
      }
    }
    if(!is.null(options$label))
      cat(" (label=", options$label, ")", sep="")
    cat("\n")  
  }
}

runcode_echo_begin <- function(file, options){
  if( options$echo ) {
    cat("\\begin{Hchunk}\n",file=file, append=TRUE)
    size <- latex_size(options)
    cat( sprintf( "\\begin{%s}\n", size ), file = file, append = TRUE )		
  }
}

runcode_output_strip_spaces <- function(output, strip.white){
  output <- paste(output,collapse="\n")
  if(strip.white %in% c("all", "true")){
    output <- sub("^[[:space:]]*\n", "", output)
    output <- sub("\n[[:space:]]*$", "", output)
    if(strip.white=="all")
      output <- sub("\n[[:space:]]*\n", "\n", output)
  }
  output
}


#' run the code by weaving (similar to standard driver)
#' @noRd
runcode_weave <- function(object, chunk, options){
  chunk <- handle_first_line(chunk)
  
  runcode_debug(options)
  
  chunkprefix <- RweaveChunkPrefix(options)
  if(options$split){
    chunkout <- object$chunkout[chunkprefix][[1L]]
    if(is.null(chunkout)){
      chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
      if(!is.null(options$label))
        object$chunkout[[chunkprefix]] <- chunkout
    }
  } else {
    chunkout <- object$output
  }
  
  append_output <- function(txt){
    lines <- paste(txt, collapse = "\n" )
    cat( lines, file = chunkout, append = TRUE)
  }
  
  saveopts <- options(keep.source = options$keep.source)
  on.exit(options(saveopts))
  
  SweaveHooks(options, run=TRUE)
  
  chunkexps <- try(parse(text=chunk, keep.source = TRUE), silent=TRUE)
  RweaveTryStop(chunkexps, options)
  
  styles   <- simple_detective( chunkexps )
  renderer <- renderer_latex( document = FALSE )
  
  openSinput <- FALSE
  openSchunk <- FALSE
  
  if(length(chunkexps) == 0L)
    return(object)
  
  srclines <- attr(chunk, "srclines")
  linesout <- integer(0L)
  srcline  <- srclines[1L]
  
  srcrefs <- attr(chunkexps, "srcref")
  if (options$expand){
    lastshown <- 0L
  } else {
    lastshown <- srcline - 1L
  }
  
  thisline <- 0
  
  runcode_echo_begin(chunkout, options)
  
  for(nce in 1L:length(chunkexps)) {
    ce <- chunkexps[[nce]]
    if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
      if (options$expand) {
        srcfile  <- attr(srcref, "srcfile")
        showfrom <- srcref[1L]
        showto   <- srcref[3L]
      } else {
        srcfile  <- object$srcfile
        showfrom <- srclines[srcref[1L]]
        showto   <- srclines[srcref[3L]]
      }
      dce       <- getSrcLines(srcfile, lastshown+1, showto)
      leading   <- showfrom - lastshown
      lastshown <- showto
      srcline   <- srclines[srcref[3L]]
      while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
        dce     <- dce[-1L]
        leading <- leading - 1L
      }
    } else {
      dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
      leading <- 1L
    }
    if(object$debug){
      cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
    }
    
    if(options$echo){ 
      if(!openSinput){
        if(!openSchunk){
          linesout[thisline + 1] <- srcline
          thisline <- thisline + 1
          openSchunk <- TRUE
        }
        openSinput <- TRUE
      }
      
      append_output( "\\begin{Hinput}\n"  )
      
      showPrompts <- options$prompt
      size <- latex_size(options)
      highlight( 
        output = chunkout, 
        parse.output = chunkexps, 
        styles = styles, 
        expr = nce, 
        renderer = renderer, 
        final.newline = FALSE, 
        showPrompts = if( !is.null(showPrompts) ) isTRUE(showPrompts) else TRUE , 
        initial.spaces = FALSE, 
        size = size, 
        show_line_numbers = options$show_line_numbers
      )
      append_output("\\end{Hinput}\n\n")
      
      linesout[thisline + 1L:length(dce)] <- srcline
      thisline <- thisline + length(dce)
    }
    
    tmpcon <- file()
    sink(file=tmpcon)
    err <- NULL
    if(options$eval) err <- RweaveEvalWithOpt(ce, options)
    cat("\n") # make sure final line is complete
    sink()
    output <- readLines(tmpcon)
    close(tmpcon)
    
    ## delete empty output
    if(length(output) == 1L & output[1L] == "") output <- NULL
    
    RweaveTryStop(err, options)
    
    
    if(object$debug) cat(paste(output, collapse="\n"))
    
    if(length(output) & (options$results != "hide")){
      
      if(openSinput){
        linesout[thisline + 1L:2L] <- srcline
        thisline <- thisline + 2L
        openSinput <- FALSE
      }
      
      if(options$results=="verbatim"){
        if(!openSchunk){
          linesout[thisline + 1L] <- srcline
          thisline <- thisline + 1L
          openSchunk <- TRUE
        }
        append_output("\\begin{Houtput}\n")
        linesout[thisline + 1L] <- srcline
        thisline <- thisline + 1L
      }
      
      output <- runcode_output_strip_spaces(output, options$strip.white)
      
      if( options$results == "verbatim" ){
        append_output( renderer$header() )
        output. <- strsplit( output, "\n" )[[1]]
        size <- latex_size(options)
        
        tex <- paste( renderer$translator(output., size = size), renderer$newline(), sep = "")
        tex[ length(tex ) ] <- sub( "\\\\\\\\\n\\\\hlstd", "\\\\hlstd", tex[length(tex)] )
        append_output( paste(tex, collapse="") )
        append_output( renderer$footer() )
      } else{
        append_output( output )
      }
      
      count <- sum(strsplit(output, NULL)[[1L]] == "\n")
      if (count > 0L) {
        linesout[thisline + 1L:count] <- srcline
        thisline <- thisline + count
      }
      
      if(options$results=="verbatim"){
        append_output("\\end{Houtput}\n")
        linesout[thisline + 1L:2] <- srcline
        thisline <- thisline + 2L
      }
      
    } 
    if( options$echo ) append_output("\n")
  }
  
  if( options$echo ){
    size <- latex_size(options)
    append_output( sprintf( "\\end{%s}\n", size ) )
    append_output("\\end{Hchunk}\n\n")
  }
  
  if(openSchunk){
    linesout[thisline + 1L] <- srcline
    thisline <- thisline + 1L
  }
  
  if(is.null(options$label) & options$split)
    close(chunkout)
  
  if(options$split & options$include){
    append_output( paste0("\\input{", chunkprefix, "}\n") )
    linesout[thisline + 1L] <- srcline
    thisline <- thisline + 1L
  }
  
  if(options$fig && options$eval){
    
    if(options$eps){
      postscript(
        file=paste(chunkprefix, "eps", sep="."),
        width=options$width, height=options$height,
        paper="special", horizontal=FALSE
      )
      
      err <- try({
        SweaveHooks(options, run=TRUE)
        eval(chunkexps, envir=.GlobalEnv)
      })
      dev.off()
      if(inherits(err, "try-error")) stop(err)
    }
    
    if(options$pdf){
      pdf(
        file=paste(chunkprefix, "pdf", sep="."),
        width=options$width, height=options$height,
        version=options$pdf.version,
        encoding=options$pdf.encoding
      )
      
      err <- try({
        SweaveHooks(options, run=TRUE)
        eval(chunkexps, envir=.GlobalEnv)
      })
      dev.off()
      if(inherits(err, "try-error")) stop(err)
    }
    
    if(options$include) {
      cat("\\includegraphics{", chunkprefix, "}\n", sep="", file=object$output, append=TRUE)
      linesout[thisline + 1L] <- srcline
      thisline <- thisline + 1L
    }
  }
  object$linesout <- c(object$linesout, linesout)
  return(object)
}


#' @importFrom utils RweaveEvalWithOpt RweaveChunkPrefix SweaveHooks RweaveTryStop
HighlightWeaveLatexRuncode <- function(object, chunk, options) {
  if( should_use_external_highlight(options) ){
    object <- runcode_external(object, chunk, options)
  } else if( options$engine %in% c("R", "S") ){
    object <- runcode_weave(object, chunk, options)
  }
  
  object
}

HweaveSyntaxNoweb <- SweaveSyntaxNoweb
HweaveSyntaxNoweb$extension <- "\\.[hHrsRS]?nw$"

#' Weaving and Tangling with syntax highlighting
#' 
#' \code{Hweave} and \code{Htangle} are similar to \code{Sweave} 
#' and \code{Stangle}, but they take advantage of the
#' custom driver shipped with this package 
#'
#'    These functions exist for the purpose of the 
#'    \code{\\VignetteEngine} option in vignette introduced in R 3.0.0
#'    
#'    \code{highlight} loads the \code{highlight} vignette engine 
#'    at load time. Client packages must declare to use it
#'    with the \code{VignetteBuilder} field in their \code{DESCRIPTION}
#'    file
#'    
#'    The vignette engine looks for files matching the 
#'    pattern \code{"[.][hHrRsS]nw$"} although in order to distinguish 
#'    vignettes using this engine and the default
#'    Sweave engine, the recommandation is to use vignette with the \code{".Hnw"}
#'    extension. 
#' 
#' @param file Path to Sweave source file
#' @param driver  The actual workhorse, see the Details section in \code{\link[utils]{Sweave}}
#' @param syntax \code{NULL} or an object of class \code{SweaveSyntax}
#'      or a character string with its name. See the section \code{Syntax Definition}
#'      in \code{\link[utils]{Sweave}}
#' @param encoding  The default encoding to assume for \code{file}
#' @param \dots Further arguments passed to the driver's setup function.
#' 
#' @rdname Hweave
#' @importFrom utils Sweave
#' @export
Hweave <- function (file, driver = HWeaveLatex(), syntax = HweaveSyntaxNoweb, encoding = "", ...){
    Sweave( file, driver = driver, syntax = syntax, encoding = encoding, ... )
}

#' @importFrom utils Rtangle
HighlightTangle <- function(){
	driver <- Rtangle()
	runcode <- driver$runcode
	driver$runcode <- function (object, chunk, options){
		if( "lang" %in% names(options) && ! options$lang %in% c("r", "R" ) ){ 
			object
		} else {	
			runcode(object, chunk, options)
		}
	}
	driver
}

#' @rdname Hweave
#' @importFrom utils Sweave
#' @export
Htangle <- function (file, driver = HighlightTangle(), syntax = HweaveSyntaxNoweb, encoding = "", ...){
	Sweave(file = file, driver = driver, encoding = encoding, ...)
}
