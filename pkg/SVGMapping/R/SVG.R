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
         representation(svg="ANY",
                        .default_search_attr="character",
                        .js_tooltip="logical",
                        .js_animation="logical")
         )

setGenericVerif(name="SVG", function(object) {standardGeneric("SVG")})
setGenericVerif(name="SVG<-", function(.Object,svg) {standardGeneric("SVG<-")})
setGenericVerif(name="defaultSearchAttr", function(object) {standardGeneric("defaultSearchAttr")})
setGenericVerif(name="defaultSearchAttr<-", function(.Object,attr.name) {standardGeneric("defaultSearchAttr<-")})
setGenericVerif(name="jsTooltip", function(object) {standardGeneric("jsTooltip")})
setGenericVerif(name="jsTooltip<-", function(.Object,flag) {standardGeneric("jsTooltip<-")})
setGenericVerif(name="jsAnimation", function(object) {standardGeneric("jsAnimation")})
setGenericVerif(name="jsAnimation<-", function(.Object,flag) {standardGeneric("jsAnimation<-")})
setGenericVerif(name="addScript", function(object,script,id) {standardGeneric("addScript")})
setGenericVerif(name="read.SVG", function(object,file) {standardGeneric("read.SVG")})
setGenericVerif(name="write.SVG", function(object,file) {standardGeneric("write.SVG")})

setMethod(f="initialize", signature="SVG",
          definition=function(.Object,...)
          {
            ## init. args
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
            ## Locals
            ## ----------------
            .row_attnames <- NULL
            
            .get_item <- function(x,item) {
              items <- strsplit(x,";")
              if(max(grepl(":",items[[1]]) == FALSE) > 1) return(NA) 
              items.values <- sub(".*:", "", items[[1]])
              items.names <- sub(":.*", "", items[[1]])
              items <- items.values
              names(items) <- items.names
              if(item %in% items.names)
                return(items[[item]])
              else
                return(NA)
            }
            
            .sub_attribute <- function(x,attname, item) {
              attrs <- xmlAttrs(x,addNamespacePrefix=TRUE)
              if(attname %in% names(attrs)) {
                if(is.na(item))
                  return(attrs[[attname]])
                else
                  return(.get_item(attrs[[attname]],item))
              }
              else
                return(NA)
            }

            .sub_vector_attribute <- function(x,j) {
              ## - init.
              attrs <- xmlAttrs(x,addNamespacePrefix=TRUE)
              j.sub <- j[grepl("::",j)]
              j.nosub <- j[!grepl("::",j)]
              js.names <- sub("::.*","",j.sub)
              js.items <- sub(".*::","",j.sub)
              res <- vector()

              ## - no.sub
              jok <- j.nosub[j.nosub %in% names(attrs)]
              jno <- j.nosub[!j.nosub %in% jok]
              if(length(jno) > 0) res[jno] <- NA
              if(length(jok) > 0) res[jok] <- attrs[jok]

              ## - sub
              check <- js.names %in% names(attrs)
              jcheck <- js.items[check]
              names(jcheck) <- js.names[check]
              if(length(j.sub[!check]) > 0) res[j.sub[!check]] <- NA    # keep original selector
              jok.res <- vector()
              if(length(jcheck) > 0) {
                for(i in 1:length(jcheck)) {
                  attname <- names(jcheck)[i]
                  item <- jcheck[i]
                  jok.res[i] <- .get_item(attrs[[attname]],item)
                }
                res[j.sub[check]] <- jok.res
              }

              ## eop
              .attnames <<- c(jno,jok,j.sub[!check],j.sub[check])
              return(res)
            }

            .atomic_getter <- function(i,x,j) {
              
              ## -- Xpath init.
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

              ## -- Nodes only selection
              if(missing(j)) {
                node.set <- getNodeSet(x@svg, xpath, namespaces=.completeNamespaces(x@svg))               
                if(length(node.set) == 0) return(list())               
                return(node.set)
              }
              
              ## -- Nodes and Attributes selection
              else {
                if(is.character(j) && (length(j) == 1)) {     # atomic case
                  
                  ## init.
                  item <- NA
                  if(grepl("::",j)) {
                    attname <- sub("::.*","",j)
                    item <- sub(".*::","",j)
                  }
                  else
                    attname <- j

                  ## extraction
                  return(xpathSApply(x@svg, xpath, .sub_attribute,                                     
                                     attname, item,
                                     namespaces=.completeNamespaces(x@svg) ) )
              }
              else {  # vectory case -- very loosy testing here..
                j <- if(is.list(j)) unlist(j) else j
                .row_attnames <- NULL
                res <- xpathSApply(x@svg, xpath,
                                   function(x,j) {
                                     res <- .sub_vector_attribute(x,j)
                                     .row_attnames <<- .attnames
                                     return(res)
                                   }, j,
                                   namespaces=.completeNamespaces(x@svg) )
                rownames(res) <- .row_attnames
                return(res)
              }
              }
            }

            ## -- GETTER

            ## - Check
            if(!(is.character(i) || is.list(i)))
              stop("'i' should be a valid node selector")
            
            if(is.list(i) || (length(i) > 1)) {
              res <- lapply(i, .atomic_getter, x, j)
              names(res) <- i
              return(res)
            }
            else
              return(.atomic_getter(i,x,j))
          }
        )

setReplaceMethod(f="[", signature="SVG",
                 definition=function(x,i,j,value)
                 {
                   ## Locals
                   ## ----------------
                   .set_item <- function(x,item,value) {
                     items <- strsplit(x,";")
                     if(max(grepl(":",items[[1]]) == FALSE) > 1) return(x)  
                     items.values <- sub(".*:", "", items[[1]])
                     items.names <- sub(":.*", "", items[[1]])
                     items <- items.values
                     names(items) <- items.names
                     items[[item]] <- value
                     x <- paste(names(items), items,sep=":",collapse=";")
                     return(x)
                   }
            
                   .sub_attribute <- function(x,attname, item, value) {
                     attrs <- xmlAttrs(x,addNamespacePrefix=TRUE)
                     if(attname %in% names(attrs) && !is.na(item)) 
                       attrs[[attname]] <- .set_item(attrs[[attname]],item,value)
                     else
                       attrs[[attname]] <- value
                     ## fix for libxml2 rel. < 2.6.3x
                     if(is.null(xmlParent(x))) removeAttributes(x) 
                     xmlAttrs(x, suppressNamespaceWarning=TRUE) <- attrs
                     return(x)
                   }

                   .atomic_setter <- function(x,xpath,j,value) {
                     ## init.
                     item <- NA
                     if(grepl("::",j)) {
                       attname <- sub("::.*","",j)
                       item <- sub(".*::","",j)
                     }
                     else
                       attname <- j
                     node.set <- getNodeSet(x@svg, xpath, namespaces=.completeNamespaces(x@svg))
                       
                     ## check
                     if(length(node.set) == 0) return(invisible())                       
                     if((length(value) != length(node.set)) && (length(value) != 1))
                       stop("'value' and 'nodes(i)' have different length")
                     if(length(value) == 1) value <- replicate(length(node.set),value)
                     
                     ## loop over nodes
                     for(i in 1:length(node.set)) {
                       node <- node.set[[i]]
                       .sub_attribute(node,attname, item, value[[i]])
                     }                     
                   }

                   .setter <- function(i,x,j,value) {
                     
                     ## - XPath init.
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

                     ## - Attribute selection
                     if(is.character(j) && (length(j) == 1)) {  # atomic case
                       .atomic_setter(x,xpath,j,value)
                     }
                     else {  # vector case

                       ## init. & check
                       j <- if(is.list(j)) unlist(j) else j
                       if(!is.list(value) && (length(value)==length(j)))
                         value <- as.list(value)
                       if(!is.list(value) || (length(value) != length(j)))
                         stop("'value' must be a 'list()' having the same length than 'j'")

                       ## loop over 'j'
                       for(att.id in 1:length(j)) {
                         att <- j[att.id]
                         v <- value[[att.id]]
                         .atomic_setter(x,xpath,att,v)
                       }                       
                     }
                   }

                   ## -- SETTER
                   
                   ## - Check
                   if(missing(j)) stop("Missing attribute specification 'j'")
                   if(!is.character(i)) stop("'i' should be a valid node selector")               

                   ## - Set
                   if(is.list(i) || (length(i) > 1)) {

                     ## init.
                     if(!is.list(value) && length(value) == length(j))
                       value <- replicate(length(i),value,simplify=FALSE)
                     if(!is.list(value) || (length(value) != length(i)))
                       stop("'value' must be a 'list()' having the same length than 'i'")

                     ## proceed..
                     for(idx in 1:length(i)) {
                       node <- i[[idx]]
                       v <- value[[idx]]
                       .setter(node,x,j,v)
                     }
                   }
                   else {
                     .setter(i,x,j,value)
                   }

                   ## - eop
                   return(x)
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

setMethod(f="addScript", signature="SVG",
          definition=function(object,script,id)
          {
            ## Build script node
            cdata <- newXMLCDataNode(paste("\n", script, "\n", sep=""))
            script.attrs <- list(type="text/ecmascript")
            if (!missing(id)) script.attrs[["id"]] <- id
            scriptnode <- newXMLNode("script", attrs=script.attrs, .children=list(cdata))
  
            ## Update/Insert script
            updated <- FALSE
            if (!mising(id)) {
              search.node <- object[paste("id::",id,sep="")]
              if(length(search.node) > 0) {
                replaceNodes(search.node[[1]], scriptnode)
                updated <- TRUE
              }
            }
            if(!updated)
              addChildren(xmlRoot(svg), kids=list(scriptnode))            
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
            assign(nameObject,object,envir=parent.frame())
            return(invisible(object))
          }
          )

setMethod(f="write.SVG", signature="SVG",
          definition=function(object,file)
          {
            ## - Check
            if(length(file)==0)
              stop("Missing destination 'file' value to save the SVG object")

            ## - Add Javascript
            if(object@.js_tooltip || object@.js_animation) {

              ## init.
              object["xpath::/svg:svg","onload"] <- "init(evt)"
              
              ## get js script (init,tooltip, animation)
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
              addScriptSVG(object, script, id="SVGMapping-script")
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
