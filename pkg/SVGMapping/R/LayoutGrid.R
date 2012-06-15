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

setGenericVerif(name="print.LayoutGrid", function(x,...) { standardGeneric("print.LayoutGrid") })
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
            ## -- locals
            .arg <- function(name,default.value) {
              if(sum(grepl(paste("^",name,"$",sep=""), args.names)) > 0) {
                v <- args[[grep(paste("^",name,"$",sep=""),args.names)]]
                return(v)
              }
              else {
                return(default.value)
              }
            }
            
            ## super
            .Object <- callNextMethod(.Object,...)

            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()

            ## default
            gridRows(.Object) <- .arg("grid.rows",vector())
            gridColumns(.Object) <- .arg("grid.columns",vector())
            
            ## eop
            return(.Object)
          }
          )

setMethod(f="show", signature="LayoutGrid",
          definition=function(object)
          {
            ## init.
            svg <- SVG.factory(system.file("extdata/layout-sample.svg", package="SVGMapping"))
            layout(svg,svg["layout.target"][[1]]) <- object

            ## add Apples in each cell
            prefix <- id(object)
            for(i in 1:nbRows(object)) {
              for(j in 1:nbColumns(object)) {
                apples <- SVG.factory(system.file("extdata/gnokii-Apple2.svg", package="SVGMapping"))
                id <- paste(prefix,".grid.",i,".",j,sep="")
                apple.id <- paste("apple",i,".",j,sep="")
                merge.SVG(svg,svg[id][[1]],preserve.ratio=TRUE) <- apples
              }
            }
            
            ## eop
            show(svg)
          }
          )

setMethod(f="print.LayoutGrid", signature="LayoutGrid",
          definition=function(x,...)
          {
            cat("GridLayout: #",
                nbRows(x)," rows and #",
                nbColumns(x), " columns\n",sep="")
            grid <- .xml(x)
            print(grid)
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
                   if((min(value) < 0) || (max(value) > 1))
                     stop("'value' must contains only values in the range [0,1]")
                   if(length(value) < 2)
                     stop("'value' must contains at least two values")
                   if(sum(order(value) != 1:length(value)) > 0)
                     stop("'value' must be an increasing order vector")
                   if(length(unique(value)) != length(value))
                     stop("'value' can't contains equal values")

                   ## add bounds (if necessary)
                   if(value[1] != 0) value <- c(0,value)
                   if(value[length(value)] != 1) value <- c(value,1)
                   
                   ## assign & eop
                   .Object@grid.rows <- value
                   return(.Object)
                 }
                 )

setMethod(f="nbRows", signature="LayoutGrid",
          definition=function(object)
          {
            return(max(0,length(object@grid.rows)-1))
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
                   if((min(value) < 0) || (max(value) > 1))
                     stop("'value' must contains only values in the range [0,1]")
                   if(length(value) < 2)
                     stop("'value' must contains at least two values")
                   if(sum(order(value) != 1:length(value)) > 0)
                     stop("'value' must be an increasing order vector")
                   if(length(unique(value)) != length(value))
                     stop("'value' can't contains equal values")

                   ## add bounds (if necessary)
                   if(value[1] != 0) value <- c(0,value)
                   if(value[length(value)] != 1) value <- c(value,1)

                   ## assign & eop
                   .Object@grid.columns <- value
                   return(.Object)
                 }
                 )

setMethod(f="nbColumns", signature="LayoutGrid",
          definition=function(object)
          {
            return(max(0,length(object@grid.columns)-1))
          }
          )

setMethod(f=".xml", signature="LayoutGrid",
          definition=function(object)
          {
            ## check
            if((nbRows(object) == 0) || (nbColumns(object) == 0))
              stop("Can't render a 'LayoutGrid' with no rows and/or columns")

            ## init.
            prefix <- id(object)
            rows <- gridRows(object)
            cols <- gridColumns(object)
            transform <- svg.transform(object)
            dims <- dimensions(object)
            
            ## create encapsulating group
            opacity <- opacity(object)
            layout.attr <- list()
            if(length(transform) > 0) layout.attr <- c(layout.attr, transform=transform)
            layout.group <- newXMLNode("g", attrs=c(
                                              layout.attr,
                                              id = paste(prefix,".group",sep=""),
                                              style = paste(
                                                "stroke-opacity:",opacity,";",
                                                "fill-opacity:",opacity,sep=""
                                                )
                                              )
                                       )

            ## add rectangles
            for(i in 1:nbRows(object)) {
              for(j in 1:nbColumns(object)) {
                id <- paste(prefix,".grid.",i,".",j,sep="")
                style <- "fill:none;stroke:black;width:1"
                x <- round(dims$width*cols[j],2) + dims$x
                y <- round(dims$height*rows[i],2) + dims$y
                width <- round(dims$width*(cols[j+1]-cols[j]),2)
                height <- round(dims$height*(rows[i+1]-rows[i]),2)
                rect <- SVGRect.factory(x=x,y=y,width=width,height=height,style=style)
                id(rect) <- id
                addChildren(layout.group,.xml(rect))
              }
            }
          
            ## eop
            return(layout.group)
          }
          )

## F A C T O R Y
##----------------------------------------
FixGrid.factory <- function(prefix="fixgrid",nrows=1,ncols=1,opacity) {

  ## init.
  args <- list("LayoutGrid")
  args <- c(args,id=prefix)
  g.rows <- seq(from=0,to=1,length.out=nrows+1)
  g.cols <- seq(from=0,to=1,length.out=ncols+1)
  args <- c(args,grid.rows=list(g.rows))
  args <- c(args,grid.columns=list(g.cols))
  if(!missing(opacity)) args <- c(args,opacity=opacity)
  layout <- do.call(new,args)

  ## eop
  return(layout)
}

VarGrid.factory <- function(prefix="vargrid",rows=c(0,1),cols=c(0,1),opacity) {

  ## init.
  args <- list("LayoutGrid")
  args <- c(args,id=prefix)
  args <- c(args,grid.rows=list(rows))
  args <- c(args,grid.columns=list(cols))
  if(!missing(opacity)) args <- c(args,opacity=opacity)
  layout <- do.call(new,args)

  ## eop
  return(layout)  
}
