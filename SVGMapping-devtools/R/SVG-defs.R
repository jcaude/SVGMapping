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

## S V G  -- DEFINITIONS
## --------------------------------------------------

## - internal stuff..
svgNS <- "http://www.w3.org/2000/svg"
SVG.VALUE <- factor("@VALUE@")

.completeNamespaces <- function(svgdata) {
  NS <- xmlNamespaceDefinitions(svgdata, simplify=TRUE)
  NS[["svg"]] <- svgNS
  return(NS)
}

#' SVG class
#' 
#' SVG is the main classs of SVGMapping. It is used to load, save and render 
#' SVG files. Futhermore, all mappings, layout or pseudo-devices operations are
#' applied on SVG objects. 
#' 
#' @name SVG
#' @exportClass "SVG"
#' @aliases SVG-class
setClass("SVG",
         representation(svg="ANY",
                        default_search_attr="character",
                        js.animation="logical",
                        js.inlines="list",
                        js.hrefs="list")
)


setMethod(f="initialize", signature="SVG",
          definition=function(.Object,...)
          {
            ## - locals
            .arg <- function(name,default.value) {
              if(sum(grepl(paste("^",name,"$",sep=""), args.names)) > 0) {
                v <- args[[grep(paste("^",name,"$",sep=""),args.names)]]
                return(v)
              }
              else {
                return(default.value)
              }
            }
            
            ## get args
            args <- list(...)
            args.names <- names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## init. @TODO: To be refactored
#             .Object@svg <- xmlTreeParse(system.file("extdata/blank.svg", package="SVGMapping"),
#                                         useInternalNodes=TRUE,
#                                         addAttributeNamespaces=TRUE,
#                                         fullNamespaceInfo=FALSE)
            
            ## init.
            defaultSearchAttr(.Object) <- "id"
            jsAnimations(.Object) <- FALSE
            jsHRefs(.Object) <- list()
            jsInlines(.Object) <- list()
            
            ## eop
            return(.Object)
          }
)

## STANDARD DOCUMENT DIMENSIONS
## ----------------------------

.SVG.STD.DIMS <-
  list("us.letter" = c("8.5in","11in"),
       "us.legal" = c("8.5in","14in"),
       "us.executive" = c("7.2in","10.5in"),
       "a0" = c("841mm","1189mm"),
       "a1" = c("594mm","841mm"),
       "a2" = c("420mm","594mm"),
       "a3" = c("297mm","420mm"),
       "a4" = c("210mm","297mm"),
       "a5" = c("148mm","210mm"),
       "a6" = c("105mm","148mm"),
       "a7" = c("74mm","105mm"),
       "a8" = c("52mm","74mm"),
       "a9" = c("37mm","52mm"),
       "a10" = c("26mm","37mm"),
       "b0" = c("1000mm","1414mm"),
       "b1" = c("707mm","1000mm"),
       "b2" = c("500mm","707mm"),
       "b3" = c("353mm","500mm"),
       "b4" = c("250mm","353mm"),
       "b5" = c("176mm","250mm"),
       "b6" = c("125mm","176mm"),
       "b7" = c("88mm","125mm"),
       "b8" = c("62mm","88mm"),
       "b9" = c("44mm","62mm"),
       "b10" = c("31mm","44mm"),
       "c0" = c("917mm","1297mm"),
       "c1" = c("648mm","917mm"),
       "c2" = c("458mm","648mm"),
       "c3" = c("324mm","458mm"),
       "c4" = c("229mm","324mm"),
       "c5" = c("162mm","229mm"),
       "c6" = c("114mm","162mm"),
       "c7" = c("81mm","114mm"),
       "c8" = c("57mm","81mm"),
       "c9" = c("40mm","57mm"),
       "c10" = c("28mm","40mm"),
       "d1" = c("545mm","771mm"),
       "d2" = c("385mm","545mm"),
       "d3" = c("272mm","385mm"),
       "d4" = c("192mm","272mm"),
       "d5" = c("136mm","192mm"),
       "d6" = c("96mm","136mm"),
       "d7" = c("68mm","96mm"),
       "e3" = c("400mm","560mm"),
       "e4" = c("280mm","400mm"),
       "e5" = c("200mm","280mm"),
       "e6" = c("140mm","200mm"),
       "us.#10.envelope" = c("41.in","9.5in"),
       "dl.envelop" = c("110mm","220mm"),
       "ledger.tabloid" = c("11in","17in"),
       "card.iso7810" = c("54mm","85.6mm"),
       "card.us" = c("2in","3.5in"),
       "card.eu" = c("55mm","85mm"),
       "arch.a" = c("9in","12in"),
       "arch.b" = c("12in","18in"),
       "arch.c" = c("18in","24in"),
       "arch.d" = c("24in","36in"),
       "arch.e" = c("36in","48in"),
       "arch.e1" = c("30in","42in")
  )
