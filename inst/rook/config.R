require(highlight)
root <- "/Users/romain/svn/highlight/pkg/highlight/inst/rook"
# root <- system.file( "rook", package = "highlight" ) 
app <- Builder$new(
    Static$new(
	    urls = c('/css','/images','/javascript'),
        root = root
	),
    Brewery$new(            
	    url='/brew',
	    root=root
    ),
    Redirect$new('/brew/index.rhtml')
)

