## Copyright (c) 2011, CEA DSV/iBiTecS
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without modification,
## are permitted provided that the following conditions are met:
## 
## * Redistributions of source code must retain the above copyright notice, this list
##   of conditions and the following disclaimer.
## 
## * Redistributions in binary form must reproduce the above copyright notice, this
##   list of conditions and the following disclaimer in the documentation and/or
##   other materials provided with the distribution.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
## ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
## ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## ---------------------------------------------
## P S E U D O   G R A P H I C S    D E V I C E
## ---------------------------------------------

dev.off <- function(which=dev.cur()) {

  ## close device
  grDevices::dev.off(which)
  
  ## init.
  devinfo <- .getDeviceInfo(which)
  
  ## check if we are plotting whithin a template?
  if(!is.null(devinfo)) {
    rplot <- SVG.factory(devinfo$rplot)

    ## FIX ISSUE WITH THE CAIRO DEVICE (see the svgDEVICE methods)
    if(.get(".cairo") == "builtin") {
      rplot["xpath::/svg:svg","width"] <- as.character(.toUserUnit(rplot["xpath::/svg:svg","width"])/1.25)
      rplot["xpath::/svg:svg","height"] <- as.character(.toUserUnit(rplot["xpath::/svg:svg","height"])/1.25)
    }
    merge.SVG(devinfo$SVG,devinfo$target.node,prefix=devinfo$prefix) <- rplot
    
    ## free & unlink stuff
    unlink(devinfo$rplot)
    .addDeviceInfo(which, NULL)
  }
}
