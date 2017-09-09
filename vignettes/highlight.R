## ---- echo = FALSE-------------------------------------------------------
library(highlight)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
knitr::knit_hooks$set( 
  source = hl_hook_source, 
  document = hl_hook_css
)

## ----gh-installation, eval = FALSE---------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("romainfrancois/highlight")

## ----eval=FALSE----------------------------------------------------------
#  highlight( file = "css_file.R", detective = lestrade )

## ---- detective = lestrade-----------------------------------------------
css_file <- function( filename = "default.css" ){
  if( file.exists(filename) ){
    return(normalizePath(filename))
  }
	
  f <- file.path( Sys.getenv("HOME"), ".R", "highlight", filename )
  if( file.exists(f) ){
    return(f)
  }

  f <- system.file( "stylesheet", filename , package = "highlight" )
  if( file.exists( f )){
    return( f) 
  }
	
  stop( glue("file not found: '{filename}'") )
}

## ----eval=FALSE----------------------------------------------------------
#  highlight( file = "css_file.R", detective = sherlock )

## ---- detective=sherlock-------------------------------------------------
css_file <- function( filename = "default.css" ){
  if( file.exists(filename) ){
    return(normalizePath(filename))
  }
	
  f <- file.path( Sys.getenv("HOME"), ".R", "highlight", filename )
  if( file.exists(f) ){
    return(f)
  }

  f <- system.file( "stylesheet", filename , package = "highlight" )
  if( file.exists( f )){
    return( f) 
  }
	
  stop( glue("file not found: '{filename}'") )
}

## ----eval=FALSE----------------------------------------------------------
#  knitr::knit_hooks$set(
#    source = hl_hook_source,
#    document = hl_hook_css
#  )

