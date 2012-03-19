## S V G
## --------------------------------------------------
library(methods)

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

svgNS <- "http://www.w3.org/2000/svg"

.completeNamespaces <- function(svgdata) {
  NS <- xmlNamespaceDefinitions(svgdata, simplify=TRUE)
  NS[["svg"]] <- svgNS
  return(NS)
}

setClass("SVG",
         representation(file="character",                     
                        svg="ANY",
                        .default_search_attr="character",
                        .js_tooltip="logical",
                        .js_animation="logical")
         )

setGenericVerif(name="SVG", function(object) {standardGeneric("SVG")})
setGenericVerif(name="SVG<-", function(.Object,svg) {standardGeneric("SVG<-")})
setGenericVerif(name="dest.file", function(object) {standardGeneric("dest.file")})
setGenericVerif(name="dest.file<-", function(.Object,file) {standardGeneric("dest.file<-")})
setGenericVerif(name="defaultSearchAttr", function(object) {standardGeneric("defaultSearchAttr")})
setGenericVerif(name="defaultSearchAttr<-", function(.Object,attr.name) {standardGeneric("defaultSearchAttr<-")})
setGenericVerif(name="jsTooltip", function(object) {standardGeneric("jsTooltip")})
setGenericVerif(name="jsTooltip<-", function(.Object,flag) {standardGeneric("jsTooltip<-")})
setGenericVerif(name="jsAnimation", function(object) {standardGeneric("jsAnimation")})
setGenericVerif(name="jsAnimation<-", function(.Object,flag) {standardGeneric("jsAnimation<-")})
setGenericVerif(name="read.SVG", function(object,file) {standardGeneric("read.SVG")})
setGenericVerif(name="write.SVG", function(object) {standardGeneric("write.SVG")})

setMethod(f="initialize", signature="SVG",
          definition=function(.Object,...)
          {
            ## init. args
            .Object@file <- character(0)
            .Object@.default_search_attr <- "id"
            .Object@.js_tooltip <- FALSE
            .Object@.js_animation <- FALSE
            
            ## eop
            return(.Object)
          }
          )

setMethod(f="show", signature="SVG",
          definition=function(object)
          {
            cat("SVGMapping::SVG object\n")
          }
          )

setMethod(f="[", signature="SVG",
          definition=function(x,i,j,drop)
          {
            ## !!!!!!  'i' and 'j' CAN BE VECTORS !!!!!!!!
            ## -------------------------------------------
            
            ## - Check
            if(!is.character(i))
              stop("'i' should be a valid node selector")
            
            ## -- Node selection
            if(grepl("^xpath::",i)) {
              xpath <- sub("^xpath::","",i)
            }
            else if(grepl("^id::",i)) {
              value <- sub("^id::","",i)
              xpath <- paste("//*[@id='",value,"']",sep="")
            }
            else if(grepl("::",i)) {
              attname <- sub("::.*","",i)
              value <- sub(".*::","",i)
              xpath <- paste("//*[@",attname,"='",value,"']",sep="")
            }
            else {
              xpath <- paste("//*[@",x@.default_search_attr,"='",i,"']",sep="")
            }

            ## -- Attribute selection
            if(missing(j)) {
              node.set <- getNodeSet(x@svg, xpath, namespaces=.completeNamespaces(x@svg))
              ## -- Check for empty node selection
              if(length(node.set) == 0) return(list())               
              return(node.set)
            }
            else {
              if(is.character(j)) {     # atomic case
                return(xpathSApply(x@svg, xpath,
                                   function(x) {
                                     attrs <- xmlAttrs(x,addNamespacePrefix=TRUE)
                                     if(j %in% names(attrs))
                                       return(attrs[[j]])
                                     else
                                       return(NA)
                                   },
                                   namespaces=.completeNamespaces(x@svg)
                                   )
                       )
              }
              else if (is.vector(j)) {  # vectory case -- very loosy testing here..
                j <- if(is.list(j)) unlist(j) else j
                return(t(xpathSApply(x@svg, xpath,
                                   function(x) {
                                     attrs <- xmlAttrs(x,addNamespacePrefix=TRUE)
                                     jok <- j[j %in% names(attrs)]
                                     jno <- j[!j %in% jok]
                                     res <- vector()
                                     res[jno] <- NA
                                     res[jok] <- attrs[jok]
                                     res
                                   },
                                   namespaces=.completeNamespaces(x@svg)
                                   )
                       ))
              }
            }
          }
          )

setMethod(f="SVG", signature="SVG",
          definition=function(object)
          {
            return(object@svg)
          }
          )
          
setReplaceMethod(f="SVG", signature="SVG",
                 definition=function(.Object,svg)
                 {
                   ## check
                   if(!is(svg,"XMLInternalDocument"))
                     stop("'svg' must be a valid XML document (XMLInternalDocument class)")

                   ## eop
                   .Object@svg <- svg
                   return(.Object)
                 }
                 )

setMethod(f="dest.file", signature="SVG",
          definition=function(object)
          {
            return(object@file)
          }
          )

setReplaceMethod(f="dest.file", signature="SVG",
                 definition=function(.Object,file)
                 {
                   ## check
                   if(!is.character(file))
                     stop("'file' must be a string")

                   ## eop
                   .Object@file <- file
                   return(.Object)
                 }
                 )

setMethod(f="defaultSearchAttr", signature="SVG",
          definition=function(object)
          {
            return(object@.default_search_attr)
          }
          )

setReplaceMethod(f="defaultSearchAttr", signature="SVG",
                 definition=function(.Object, attr.name)
                 {
                   ## check
                   if(!is.character(attr.name))
                     stop("'attr.name' must be a valid character string")

                   ## eop
                   .Object@.default_search_attr <- attr.name
                   return(.Object)
                 }
                 )

setMethod(f="jsTooltip", signature="SVG",
          definition=function(object)
          {
            return(object@.js_tooltip)
          }
          )

setReplaceMethod(f="jsTooltip", signature="SVG",
                 definition=function(.Object, flag)
                 {
                   ## check
                   if(!is.logical(floag))
                     stop("'flag' must be a boolean")

                   ## eop
                   .Object@.js_tooltip <- flag
                   return(.Object)
                 }
                 )

setMethod(f="jsAnimation", signature="SVG",
          definition=function(object)
          {
            return(object@.js_animation)
          }
          )

setReplaceMethod(f="jsAnimation", signature="SVG",
                 definition=function(.Object, flag)
                 {
                   ## check
                   if(!is.logical(floag))
                     stop("'flag' must be a boolean")

                   ## eop
                   .Object@.js_animation <- flag
                   return(.Object)
                 }
                 )

setMethod(f="read.SVG", signature="SVG",
          definition=function(object, file)
          {
            ## init.
            nameObject <- deparse(substitute(object))
            
            ## try to load document.
            object@svg <- xmlTreeParse(file,
                                       useInternalNodes=TRUE,
                                       addAttributeNamespaces=TRUE,
                                       fullNamespaceInfo=FALSE)
            object@file <- file
            assign(nameObject,object,envir=parent.frame())
            return(invisible(object))
          }
          )

setMethod(f="write.SVG", signature="SVG",
          definition=function(object)
          {
            ## - Check
            if(length(file)==0)
              stop("Missing destination 'file' value to save the SVG object")

            ## - Add Javascript
            if(object@.js_tooltip || object@.js_animation) {
              #### THIS IS THE CODE SECTION TO REFACTOR !!!!!!!!!!!!!!!!!
              ## add js init. 
              root <- xmlRoot(svg)
              setAttributeSVG(root, "onload", "init(evt)")
              #### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              ## get js script
              con <- file(system.file("extdata/script.js", package="SVGMapping"), "rb")
              script.lines <- readLines(con)
              close(con)
              script <- paste(script.lines, collapse="\n")
              if(object@.js_tooltip) {
                con <- file(system.file("extdata/tooltip.js", package="SVGMapping"), "rb")
                script.lines <- readLines(con)
                close(con)
                script <- paste(script,script.lines, collapse="\n")
              }
              if(object@.js_animation) {
                con <- file(system.file("extdata/animation.js", package="SVGMapping"), "rb")
                script.lines <- readLines(con)
                close(con)
                script <- paste(script,script.lines, collapse="\n")
              }
              #### THIS IS THE CODE SECTION TO REFACTOR !!!!!!!!!!!!!!!!!
              ## add js to svg
              addScriptSVG(svg, script, id="SVGMapping-script")
              #### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!              
            }

            ## - Produce source XML
            xml <- saveXML(object@svg, indent=FALSE)
            xml <- gsub("\n</text>","</text>", xml)
            xml <- gsub("\n<tspan","<tspan", xml)
  
            ## Write/output the SVG
            cat(xml, file=file)
          }
          )

## F A C T O R Y
## --------------------------------------------------
SVG.factory <- function(file) {

  ## build a new SVG
  svg <- new("SVG")
  if(!missing(file))
    read.SVG(svg,file=file)

  ## eop
  return(svg)
}
