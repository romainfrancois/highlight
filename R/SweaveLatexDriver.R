
latex_color <- function( name = col, col  = "white"){
	sprintf( "\\definecolor{%s}{rgb}{%s}", name, paste(as.vector(col2rgb(col))/255, collapse = "," ) )
}

HighlightWeaveLatexCheckOps <- function(options){
	if( "lang" %in% names(options) ){
		options
	} else {
		if( "size" %in% names(options) ) {
			append( RweaveLatexOptions( options[ - which( names(options) == "size" ) ] ), list( size = options$size ) ) 
		} else RweaveLatexOptions( options )
	}
}

# {{{ HighlightWeaveLatex: driver
HighlightWeaveLatex <- function(boxes=FALSE, bg = rgb( 0.95,0.95,0.95, max = 1 ), border = "black", 
	highlight.options = list( boxes = boxes, bg = bg, border = border )
) {
	list(setup      = RweaveLatexSetup,
         runcode    = makeHighlightWeaveLatexCodeRunner( 
         	evalFunc=RweaveEvalWithOpt, highlight.options = options 
         ) ,
         writedoc   = makeHighlightWeaveLatex_WriteDoc(
         	highlight.options = highlight.options
         ),
         finish     = RweaveLatexFinish,
         checkopts  = HighlightWeaveLatexCheckOps )
}
# }}}

# {{{ makeHighlightWeaveLatexCodeRunner
makeHighlightWeaveLatexCodeRunner <- function(evalFunc=RweaveEvalWithOpt, highlight.options) {
	
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    HighlightWeaveLatexRuncode <- function(object, chunk, options) {
      	 if( grepl( "#line [0-9]", chunk[1L] ) ){
      	  	   chunk <- chunk[-1L]
      	  	   attr(chunk, "srclines" ) <- attr(chunk, "srclines" )[-1L]
      	  }
      	  if( "lang" %in% names(options)){
      	  	 tex <- external_highlight( chunk, lang = options$lang, type = "LATEX" )
      	  	 
      	  	 size <- if( "size" %in% names(options) ) LATEX_SIZES[ pmatch( options$size, LATEX_SIZES) ] else "normalsize"
      	  	 tex <- gsub( "hlbox", sprintf( "hl%sbox", size ), tex, fixed = TRUE ) 
      	  	 keep <- seq( which( tex == "\\noindent" ), which( tex == "\\normalfont" ) )
			 tex <- tex[ keep ]
			 tex[ length(tex) - 2L ] <- sub( "\\\\\\\\$", "", tex[ length(tex) - 2L ] )
      	  	 
			 tex <- c(
			 		sprintf( "\\begin{%s}", size ), 
			 		"\\begin{Hchunk}" , 
			 		tex ,
			 		"\\end{Hchunk}\\vspace{1em}", 
			 		sprintf( "\\end{%s}", size )
			 	)
             writeLines( tex, object$output )
             return(object)
      	  } else { 
      	  
	          if(!(options$engine %in% c("R", "S"))){
	              return(object)
	          }
	
	          if(!object$quiet){
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
	
	          chunkprefix <- RweaveChunkPrefix(options)
	
	          if(options$split){
	              ## [x][[1L]] avoids partial matching of x
	              chunkout <- object$chunkout[chunkprefix][[1L]]
	              if(is.null(chunkout)){
	                  chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
	                  if(!is.null(options$label))
	                    object$chunkout[[chunkprefix]] <- chunkout
	              }
	          }
	          else
	            chunkout <- object$output
	
		  saveopts <- options(keep.source=options$keep.source)
		  on.exit(options(saveopts))
	
	          SweaveHooks(options, run=TRUE)
	
	          chunkexps <- try(parse(text=chunk), silent=TRUE)
			  RweaveTryStop(chunkexps, options)
	          parser.output <- try( parser(text = chunk ), silent = TRUE )
			  
			  styles <- simple_detective( parser.output )
			  renderer <- renderer_latex( document = FALSE )
			  
	          openSinput <- FALSE
	          openSchunk <- FALSE
	
	          if(length(chunkexps) == 0L)
	            return(object)
	
	          srclines <- attr(chunk, "srclines")
	          linesout <- integer(0L)
	          srcline <- srclines[1L]
	
		  srcrefs <- attr(chunkexps, "srcref")
		  if (options$expand)
		    lastshown <- 0L
		  else
		    lastshown <- srcline - 1L
		  thisline <- 0
		  
		  if( options$echo ) {
		  	  cat("\\begin{Hchunk}\n",file=chunkout, append=TRUE)
		  	  size <- if( "size" %in% names(options) ) LATEX_SIZES[ pmatch( options$size, LATEX_SIZES) ] else "normalsize"
      	  	  cat( sprintf( "\\begin{%s}\n", size ), file = chunkout, append = TRUE )		
		  }
	                 
	          for(nce in 1L:length(chunkexps)) {
				     ce <- chunkexps[[nce]]
	                if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
	            	        if (options$expand) {
	      			          	srcfile <- attr(srcref, "srcfile")
	      			          	showfrom <- srcref[1L]
	      			          	showto <- srcref[3L]
	            	        } else {
	            	        	srcfile <- object$srcfile
	            	        	showfrom <- srclines[srcref[1L]]
	            	        	showto <- srclines[srcref[3L]]
	            	        }
	            	        dce <- getSrcLines(srcfile, lastshown+1, showto)
		    		    leading <- showfrom - lastshown
		    		    lastshown <- showto
	            	   srcline <- srclines[srcref[3L]]
	            	   while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
		    				dce <- dce[-1L]
		    				leading <- leading - 1L
		    		    }
		    		} else {
	                    dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
	                    leading <- 1L
	                }
	                if(object$debug)
	                  cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
	                         
					  if(options$echo){ 
						 if(!openSinput){
	                        if(!openSchunk){
	                            linesout[thisline + 1] <- srcline
	                            thisline <- thisline + 1
	                            openSchunk <- TRUE
	                        }
							openSinput <- TRUE
						 }
						
						cat("\\begin{Hinput}", file=chunkout, append=TRUE)
						cat("\n", file = chunkout, append = TRUE )
						showPrompts <- options$prompt
						size <- if( "size" %in% names(options) ) options$size else "normalsize"
						highlight( output = chunkout, 
		 					parser.output = parser.output, 
		 					styles = styles, 
		 					expr = nce, 
		 					renderer = renderer, 
							final.newline = FALSE, 
							showPrompts = if( !is.null(showPrompts) ) isTRUE(showPrompts) else TRUE , 
							initial.spaces = FALSE, 
							size = size, 
							show_line_numbers = options$show_line_numbers
							)
						cat("\\end{Hinput}\n\n", file=chunkout, append=TRUE)
	                   
						linesout[thisline + 1L:length(dce)] <- srcline
						thisline <- thisline + length(dce)
	                }
	
	                tmpcon <- file()
	                sink(file=tmpcon)
	                err <- NULL
	                if(options$eval) err <- evalFunc(ce, options)
	                cat("\n") # make sure final line is complete
	                sink()
	                output <- readLines(tmpcon)
	                close(tmpcon)
	                ## delete empty output
	                if(length(output) == 1L & output[1L] == "") output <- NULL
	
	                RweaveTryStop(err, options)
	
	                if(object$debug)
	                  cat(paste(output, collapse="\n"))
	
	                if(length(output) & (options$results != "hide")){
	
	                    if(openSinput){
						    linesout[thisline + 1L:2L] <- srcline
	                        thisline <- thisline + 2L
	                        openSinput <- FALSE
	                    }
	                    if(options$results=="verbatim"){
	                        if(!openSchunk){
	                        #    cat("\\begin{Hchunk}\n",
	                        #        file=chunkout, append=TRUE)
	                            linesout[thisline + 1L] <- srcline
	                            thisline <- thisline + 1L
	                            openSchunk <- TRUE
	                        }
	                        cat("\\begin{Houtput}\n",
	                            file=chunkout, append=TRUE)
	                        linesout[thisline + 1L] <- srcline
	                        thisline <- thisline + 1L
	                    }
	
	                    output <- paste(output,collapse="\n")
	                    if(options$strip.white %in% c("all", "true")){
	                        output <- sub("^[[:space:]]*\n", "", output)
	                        output <- sub("\n[[:space:]]*$", "", output)
	                        if(options$strip.white=="all")
	                          output <- sub("\n[[:space:]]*\n", "\n", output)
	                    }
	                    
						if( options$results == "verbatim" ){
							cat( paste( renderer$header(), collapse = "\n" ), file = chunkout, append = TRUE)
							output. <- strsplit( output, "\n" )[[1]]
							size <- if( "size" %in% names(options) ) LATEX_SIZES[ pmatch( options$size, LATEX_SIZES) ] else "normalsize"
      	  	  				tex <- paste( renderer$translator(output., size = size), renderer$newline(), sep = "")
      	  	  				tex[ length(tex ) ] <- sub( "\\\\\\\\\n\\\\hlstd", "\\\\hlstd", tex[length(tex)] )
      	  	  				cat( paste(tex, collapse="") , file=chunkout, append=TRUE )
							remove( output.) 
							cat( paste( renderer$footer(), collapse = "\n" ), file = chunkout, append = TRUE )
						 } else{
							cat( output, file=chunkout, append=TRUE)
						}
						 count <- sum(strsplit(output, NULL)[[1L]] == "\n")
	                    if (count > 0L) {
	                    	linesout[thisline + 1L:count] <- srcline
	                    	thisline <- thisline + count
	                    }
	
	                    remove(output)
	
	                    if(options$results=="verbatim"){
	                        cat("\\end{Houtput}\n", file=chunkout, append=TRUE)
	                        linesout[thisline + 1L:2] <- srcline
	                        thisline <- thisline + 2L
	                    }
	                } 
					if( options$echo ) cat("\n", file = chunkout, append = TRUE)
	            }
	
	            if( options$echo ){
	            	size <- if( "size" %in% names(options) ) LATEX_SIZES[ pmatch( options$size, LATEX_SIZES) ] else "normalsize"
	            	cat( sprintf( "\\end{%s}\n", size ), file = chunkout, append = TRUE )
	            	cat("\\end{Hchunk}\n\n", file=chunkout, append=TRUE)
	            }
	          
	#          if(openSinput){
	#			  cat("\n\\end{Hinput}\n", file=chunkout, append=TRUE)
	#              linesout[thisline + 1L:2L] <- srcline
	#              thisline <- thisline + 2L
	#          }
	
	          
			  if(openSchunk){
	              linesout[thisline + 1L] <- srcline
	              thisline <- thisline + 1L
	          }
	
	          if(is.null(options$label) & options$split)
	            close(chunkout)
	
	          if(options$split & options$include){
	              cat("\\input{", chunkprefix, "}\n", sep="",
	                file=object$output, append=TRUE)
	              linesout[thisline + 1L] <- srcline
	              thisline <- thisline + 1L
	          }
	
	          if(options$fig && options$eval){
	              if(options$eps){
	                  grDevices::postscript(file=paste(chunkprefix, "eps", sep="."),
	                                        width=options$width, height=options$height,
	                                        paper="special", horizontal=FALSE)
	
	                  err <- try({SweaveHooks(options, run=TRUE)
	                              eval(chunkexps, envir=.GlobalEnv)})
	                  grDevices::dev.off()
	                  if(inherits(err, "try-error")) stop(err)
	              }
	              if(options$pdf){
	                  grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
	                                 width=options$width, height=options$height,
	                                 version=options$pdf.version,
	                                 encoding=options$pdf.encoding)
	
	                  err <- try({SweaveHooks(options, run=TRUE)
	                              eval(chunkexps, envir=.GlobalEnv)})
	                  grDevices::dev.off()
	                  if(inherits(err, "try-error")) stop(err)
	              }
	              if(options$include) {
	                  cat("\\includegraphics{", chunkprefix, "}\n", sep="",
	                      file=object$output, append=TRUE)
	                  linesout[thisline + 1L] <- srcline
	                  thisline <- thisline + 1L
	              }
	          }
	          object$linesout <- c(object$linesout, linesout)
	          return(object)
	    }
      }
    HighlightWeaveLatexRuncode
}
# }}} 

# {{{ HighlightWeaveLatexWritedoc
makeHighlightWeaveLatex_WriteDoc <- function( highlight.options ){
	
HighlightWeaveLatexWritedoc <- function(object, chunk) {
	
	linesout <- attr(chunk, "srclines")
	renderer <- renderer_latex( )

    if(length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk))){
        object$havesty <- TRUE
	}

    if(!object$havesty){
 		sweave <- paste( "\\usepackage{", object$styfile , "}" )
    } else{
		where.sweave <- grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)[1]
		sweave <- chunk[ where.sweave ]
		chunk[ where.sweave ] <- paste( "" )
	}
	
	environments <- if( highlight.options[["boxes"]] ){                                     
sprintf( 
'
\\usepackage{color}%%
%s
%s
\\newenvironment{Hinput}%%
{}%%
{}%%
\\newenvironment{Houtput}%%
{}%%
{}%%
\\newsavebox{\\highlightbox}%%
\\newenvironment{Hchunk}%%
{%%
\\vspace{0.5em}\\noindent\\begin{lrbox}{\\highlightbox}%%
\\begin{minipage}[b]{.9\\textwidth}%%
}%%
{%%
\\end{minipage}%%
\\end{lrbox}%%
\\fcolorbox{highlightBorder}{highlightBg}{\\usebox{\\highlightbox}}%%
\\vspace{0.5em}}%%
', 
latex_color("highlightBg", highlight.options$bg ), 
latex_color("highlightBorder", highlight.options$border )
)
	} else {
'\\newenvironment{Hinput}%
{}%
{}%
\\newenvironment{Houtput}%
{}%
{}%
\\newenvironment{Hchunk}%
{\\vspace{0.5em}\\par\\begin{flushleft}}%
{\\end{flushleft}}%'
	}

	documentclass <- "^[[:space:]]*\\\\documentclass.*$"
 	which <- grep( documentclass, chunk )
	
	if( length( which ) ){
		replacement <- paste(
				chunk[which], 
				sweave , 
				environments, 
				paste( renderer$boxes , collapse = "\n"),  
				paste( renderer$styles, collapse = "\n"), 
				sep = "\n" )
		chunk[which] <- replacement
	}
	
	while(length(pos <- grep(object$syntax$docexpr, chunk))){
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0L), because sub() will fail
            if(length(val) == 0L) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
	
    while(length(pos <- grep(object$syntax$docopt, chunk))){
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                             HighlightWeaveLatexCheckOps )
        if (isTRUE(object$options$concordance)
              && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep=".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                 paste("\\\\input{", prefix, "}", sep=""),
                                 chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep="\n", file=object$output, append=TRUE)
    object$linesout <- c(object$linesout, linesout)

    return(object)
}
HighlightWeaveLatexWritedoc
}
# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

