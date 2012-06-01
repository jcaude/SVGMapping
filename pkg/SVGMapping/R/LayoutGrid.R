## Copyright (c) 2012, CEA DSV/iBiTecS
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

## L A Y O U T   G R I D
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("LayoutGrid",
         representation(grid.rows = "vector",
                        grid.columns = "vector"
                        ),
         contains="Layout"
         )

setGenericVerif(name="gridRows", function(object) { standardGeneric("gridRows") })
setGenericVerif(name="gridRows<-", function(.Object,value) { standardGeneric("gridRows<-") })
setGenericVerif(name="nbRows", function(object) { standardGeneric("nbRows") })
setGenericVerif(name="gridColumns", function(object) { standardGeneric("gridColumns") })
setGenericVerif(name="gridColumns<-", function(.Object,value) { standardGeneric("gridColumns<-") })
setGenericVerif(name="nbColumns", function(object) { standardGeneric("nbColumns") })
setGenericVerif(name=".xml", function(object) { standardGeneric(".xml") })

setMethod(f="initialize", signature="LayoutGrid",
          definition=function(.Object,...)
          {
            ## super
            .Object <- callNextMethod(.Object,...)

            ## default
            grid.rows <- vector(0)
            grid.columns <- vector(0)
            
            ## eop
            return(.Object)
          }
          )

setMethod(f="show", signature="LayoutGrid",
          definition=function(object)
          {
          }
          )

setMethod(f="gridRows", signature="LayoutGrid",
          definition=function(object)
          {
            return(object@grid.rows)
          }
          )

setReplaceMethod(f="gridRows", signature="LayoutGrid",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value) && !is.numeric(value))
                     stop("'value' must be a numeric vector")
                   if(min(value) <= 0)
                     stop("'value' must contains only positive values")
                   if(sum(value) != 1)
                     stop("sum of 'value' must be 1.0")

                   ## assign & eop
                   .Object@grid.rows <- value
                   return(.Object)
                 }
                 )

setMethod(f="nbRows", signature="LayoutGrid",
          definition=function(object)
          {
            return(length(object@grid.rows))
          }
          )

setMethod(f="gridColumns", signature="LayoutGrid",
          definition=function(object)
          {
            return(object@grid.columns)
          }
          )

setReplaceMethod(f="gridColumns", signature="LayoutGrid",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.vector(value) && !is.numeric(value))
                     stop("'value' must be a numeric vector")
                   if(min(value) <= 0)
                     stop("'value' must contains only positive values")
                   if(sum(value) != 1)
                     stop("sum of 'value' must be 1.0")

                   ## assign & eop
                   .Object@grid.columns <- value
                   return(.Object)
                 }
                 )

setMethod(f="nbColumns", signature="LayoutGrid",
          definition=function(object)
          {
            return(length(object@grid.columns))
          }
          )

setMethod(f=".xml", signature="LayoutGrid",
          definition=function(object)
          {
            ## check
            if(nbRows(object) || nbColumns(object))
              stop("Can't render a 'LayoutGrid' with no rows and/or columns")

            ## init.
            prefix <- id(object)
            
            ## create encapsulating group
            layout.group <- newXMLNode("g", attrs=c(id = paste(prefix,".group",sep="")))

            ## add rectangles
            for(i in 1:nbRows(object)) {
              for(j in 1:nbColumns(object)) {
              }
            }
          
            ## eop
            return(layout.group)
          }
          )

## F A C T O R Y
##----------------------------------------

