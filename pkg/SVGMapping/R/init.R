## Package Initialization
.svgmapping.env <- new.env()

.onLoad <- function(libname, pkgname) {

  ## internals
  .checkCairo <- function() {

    ## 0 - builtin SVG Cairo
    gr.cap <- capabilities()
    if(gr.cap["cairo"])
      return("builtin")
  
    ## eop
    return(NULL)
  }

  ## init. java env.
##  options("java.parameters" = c("-Djava.awt.headless=true", getOption("java.parameters")))
##   .jpackage(pkgname, lib.loc=libname)
  
  ## init. package env.
  .set("Templates.MetaData", list())
  .set("Active.Devices", list())

  ## init. check CAIRO support
  cairo <- .checkCairo()
  if(is.null(cairo))
    warning("No SVG Cairo engine available.. (pseudo-device disabled)")
  .set(".cairo", cairo)

}
