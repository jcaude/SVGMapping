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

## S V G
## --------------------------------------------------
library(methods)

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

svgNS <- "http://www.w3.org/2000/svg"
SVG.VALUE <- factor("@VALUE@")

.completeNamespaces <- function(svgdata) {
  NS <- xmlNamespaceDefinitions(svgdata, simplify=TRUE)
  NS[["svg"]] <- svgNS
  return(NS)
}

setClass("SVG",
         representation(svg="ANY",
                        .default_search_attr="character",
                        .js_animation="logical",
                        .js_scripts="list",
                        .js_files="list")
         )

setGenericVerif(name="summary", function(object, ...) {standardGeneric("summary")})
setGenericVerif(name="SVG", function(object) {standardGeneric("SVG")})
setGenericVerif(name="SVG<-", function(.Object,value) {standardGeneric("SVG<-")})
setGenericVerif(name="uid", function(object,prefix) {standardGeneric("uid")})
setGenericVerif(name="definitions", function(object) { standardGeneric("definitions")})
setGenericVerif(name="definitions<-", function(.Object,value) { standardGeneric("definitions<-")})
setGenericVerif(name="defaultSearchAttr", function(object) {standardGeneric("defaultSearchAttr")})
setGenericVerif(name="defaultSearchAttr<-", function(.Object,value) {standardGeneric("defaultSearchAttr<-")})
setGenericVerif(name="jsAnimation", function(object) {standardGeneric("jsAnimation")})
setGenericVerif(name="jsAnimation<-", function(.Object,value) {standardGeneric("jsAnimation<-")})
setGenericVerif(name="jsAddScriptFile", function(.Object,name,file) {standardGeneric("jsAddScriptFile")})
setGenericVerif(name="jsAddScriptText", function(.Object,name,script) {standardGeneric("jsAddScriptText")})
setGenericVerif(name="addScript", function(object,script,id) {standardGeneric("addScript")})
setGenericVerif(name="read.SVG", function(object,file) {standardGeneric("read.SVG")})
setGenericVerif(name="write.SVG", function(object,file) {standardGeneric("write.SVG")})
setGenericVerif(name="mapping", function(object,op) {standardGeneric("mapping")})

setMethod(f="initialize", signature="SVG",
          definition=function(.Object,...)
          {
            ## init. args
            .Object@svg <- xmlTreeParse(system.file("extdata/blank.svg", package="SVGMapping"),
                                        useInternalNodes=TRUE,
                                        addAttributeNamespaces=TRUE,
                                        fullNamespaceInfo=FALSE)
            .Object@.default_search_attr <- "id"
            .Object@.js_animation <- FALSE
            .Object@.js_scripts <- list()
            .Object@.js_files <- list()
            
            ## eop
            return(.Object)
          }
          )

setMethod(f="summary", signature="SVG",
          definition=function(object, ...)
          {
            ## init.
            svg.stats <- summary(object@svg)
            svg.stats$jsAnimation <- jsAnimation(object)
            svg.stats$scripts <- length(object@js_scripts) + length(object@js_files)

            ## return stats
            return(svg.stats)
          }
          )

setMethod(f="print", signature="SVG",
          definition=function(x,...)
          {
            ## init.
            svg.stats <- summary(x)

            ## display stats
            cat("SVG Object  :", svg.stats$numNodes, "nodes\n")
            cat("- javascript: animations (", if(jsAnimation(x)) "enable" else "disable", ")\n", sep="")
            cat("- javascript: files+inline=",svg.stats$scripts,"\n")
          }
          )

setMethod(f="show", signature="SVG",
          definition=function(object)
          {
            ## init.
            browser <- getOption("browser")
            path <- tempfile()
            svg.path <- paste(path, ".svg", sep="")            
            html.path <- paste(path, ".html", sep="")

            ## save temp & write HTML page
            write.SVG(object, svg.path)
            con <- file(system.file("extdata/show-template.html", package="SVGMapping"), "rb")
            html.template <- paste(readLines(con),collapse="\n")
            close(con)
            html <- sprintf(html.template,basename(svg.path))
            con <- file(html.path, "w")
            cat(html, file=con)
            close(con)
            browseURL(paste("file:///", html.path, sep=""), browser=browser)
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
                if(is.character(j) && (length(j) == 1)) {     # atomic case (string)
                  
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
                                     namespaces=.completeNamespaces(x@svg)))
                }
                else if((length(j) == 1) && (j == SVG.VALUE)) {  # atomic case (value)
                  ## extraction
                  return(xpathSApply(x@svg, xpath, xmlValue,
                                     recursive=FALSE,
                                     namespaces=.completeNamespaces(x@svg)))
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
              if(missing(j))
                res <- lapply(i, .atomic_getter, x)
              else
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

                     ## eop
                     return(invisible())
                   }

                   .atomic_setter_value <- function(x,xpath,j,value) {
                     ## init
                     node.set <- getNodeSet(x@svg, xpath, namespaces=.completeNamespaces(x@svg))

                     ## check
                     if(length(node.set) == 0) return(invisible())                       
                     if((length(value) != length(node.set)) && (length(value) != 1))
                       stop("'value' and 'nodes(i)' have different length")
                     if(length(value) == 1) value <- replicate(length(node.set),value)

                     ## loop over nodes
                     for(i in 1:length(node.set)) {
                       node <- node.set[[i]]
                       if(is.null(node)) break
                       if(xmlSize(node) > 0)
                         sub.node <- xmlChildren(node)[[1]]
                       if(is(node,"XMLInternalTextNode") || is(sub.node,"XMLInternalTextNode"))
                         xmlValue(node) <- value[[i]]
                       else
                         warning(paste("no text node found to set value=",value,")",sep=""))
                     }

                     ## eop
                     return(invisible())
                   }

                   .setter <- function(i,x,j,value) {
                     
                     ## - XPath init.
                     if(grepl("^xpath::",i)) {
                       xpath <- sub("^xpath::","",i)
                     }
                     else if(grepl("^id::",i)) {
                       attvalue <- sub("^id::","",i)
                       xpath <- paste("//*[@id='",attvalue,"']",sep="")
                     }
                     else if(grepl("::",i)) {
                       attname <- sub("::.*","",i)
                       attvalue <- sub(".*::","",i)
                       xpath <- paste("//*[@",attname,"='",attvalue,"']",sep="")
                     }
                     else {
                       xpath <- paste("//*[@",x@.default_search_attr,"='",i,"']",sep="")
                     }

                     ## - Attribute selection
                     if(is.character(j) && (length(j) == 1)) {  # atomic case (string)
                       .atomic_setter(x,xpath,j,value)
                     }
                     else if((length(j) == 1) && (j == SVG.VALUE)) { # atomic case (value)
                       .atomic_setter_value(x,xpath,j,value)
                     }
                     else {  # vector case

                       ## init.
                       j <- if(is.list(j)) unlist(j) else j
                       value <- if(!is.list(value) && is.vector(value)) as.list(value) else value

                       ## check.
                       if(is.array(value) && (ncol(value) != length(j)))
                         stop("'value' must be an 'array' having the same nb. of columns than 'j'")
                       if(is.list(value) && (length(value) != length(j)))
                         stop("'value' must be a 'list' having the same length than 'j'")

                       ## loop over 'j'
                       for(att.id in 1:length(j)) {
                         att <- j[att.id]
                         v <- if(is.array(value)) value[,att.id] else value[[att.id]]
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
                     if(!is.list(value) && is.vector(value) && length(value) == length(j))
                       value <- replicate(length(i),value,simplify=FALSE)
                     if(is.vector(value) && (length(value) != length(i)))
                       stop("'value' must be a 'list' or 'vector' having the same length than 'svg[i]'")
                     if(is.array(value) && (nrow(value) != length(i)))
                       stop("'value' must be an 'array' having the same nb. of rows than 'svg[i]'")

                     ## proceed..
                     for(idx in 1:length(i)) {
                       node <- i[[idx]]
                       v <- if(is.array(value)) value[idx,] else value[[idx]]
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
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is(value,"XMLInternalDocument"))
                     stop("'value' must be a valid SVG XML document (XMLInternalDocument class)")

                   ## eop
                   .Object@svg <- value
                   return(.Object)
                 }
                 )

setMethod(f="uid", signature="SVG",
          definition=function(object,prefix)
          {
            ## check
            if(missing(prefix)) prefix <- ""

            ## create random uid
            repeat {
              test.id <- paste(prefix,trunc(runif(n=1,min=1,max=999999)),sep="")
              chck=object[paste("id::",test.id,sep="")]
              if(length(chck) == 0) break
            }

            ## eop
            return(test.id)
          }
          )

setMethod(f="definitions", signature="SVG",
          definition=function(object)
          {
            return(object["xpath:://svg:defs"])
          }
          )

setReplaceMethod(f="definitions", signature="SVG",
                 definition=function(.Object,value)
                 {                 
                   ## check
                   if(!is.object(value) &&
                      !(is(value,"XMLInternalNode") || is(value,"SVGNode")))
                     stop("'value' must be a valid 'XMLInternalNode' or 'SVGNode' object")

                   ## create defs if necessary
                   if(length(definitions(.Object)) == 0) {
                     defs.id <- uid(.Object,"defs")
                     defs <- newXMLNode("defs",attrs=list(id=defs.id))
                     addChildren(xmlRoot(.Object@svg, defs))
                   }

                   ## init. (svg node)
                   if(is(value,"SVGNode")) {
                     nameValue <- deparse(substitute(value))
                     uid <- uid(.Object,tolower(class(value)))
                     id(value) <- uid
                     assign(nameValue,value,envir=parent.frame())
                     value <- .xml(value)
                   }
                   
                   ## init.
                   value.attrs <- xmlAttrs(value)
                   if(!"id" %in% names(value.attrs))
                     stop("'id' attribute missing in value")
                   value.id <- value.attrs[["id"]]
                   if(length(value.id)==0)
                     stop("'if' shouldn't be an empty string")
                   chck <- .Object[paste("xpath:://svg:defs/*[@id='",value.id,"']",sep="")]
                   
                   ## case 1 - update
                   if(length(chck) > 0) 
                     replaceNodes(chck[[1]], value)

                   ## case 2 - insert
                   else {
                     defs <- definitions(.Object)
                     addChildren(defs[[1]], value)
                   }                     
                   
                   ## eop
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
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("'attr.name' must be a valid character string")

                   ## eop
                   .Object@.default_search_attr <- value
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
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.logical(value))
                     stop("'flag' must be a boolean")

                   ## eop
                   .Object@.js_animation <- value
                   return(.Object)
                 }
                 )

setMethod(f="jsAddScriptFile", signature="SVG",
          definition=function(.Object,name,file)
          {
             ## init.
            nameObject <- deparse(substitute(.Object))

            ## update js file list
            .Object@js_files[name] <- file
            
            ## eop
            assign(nameObject,.Object,envir=parent.frame())
            return(invisible(.Object))
          }
          )

setMethod(f="jsAddScriptText", signature="SVG",
          definition=function(.Object,script)
          {
             ## init.
            nameObject <- deparse(substitute(.Object))

            ## update js file list
            .Object@js_scripts[name] <- script
            
            ## eop
            assign(nameObject,.Object,envir=parent.frame())
            return(invisible(.Object))
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
              addScriptSVG(object, script, id="SVGMapping-init")
              if(object@.js_animation) {
                con <- file(system.file("extdata/animation.js", package="SVGMapping"), "rb")
                script.lines <- readLines(con)
                close(con)
                script <- paste(script,script.lines, collapse="\n")
              }
              addScriptSVG(object, script, id="SVGMapping-animations")
              if(length(object@js_files) > 0) {
                script_names <- names(object@js_files)
                for(i in 1:length(object@js_files)) {
                  script_file <- object@js_files[[i]]
                  script_name <- script_names[[i]]
                  con <- file(script_file, "rb")
                  script.lines <- readLines(con)
                  close(con)
                  script <- paste(script,script.lines, collapse="\n")
                  addScriptSVG(object, script, id=paste("SVMapping_jsfiles_", script_name, sep=""))
                }
              }

              
            }

            ## - Produce source XML
            xml <- saveXML(object@svg, indent=FALSE)
            xml <- gsub("\n</text>","</text>", xml)
            xml <- gsub("\n<tspan","<tspan", xml)
            
            ## Write/output the SVG
            cat(xml, file=file)
          }
          )

setMethod(f="mapping", signature="SVG",
          definition=function(object,op)
          {
            ## apply mapping on the current svg
            exec(op,svg)
            return(invisible(op))
          }
          )

## F A C T O R Y
## --------------------------------------------------

.SVG.STD.DIMS <-
  list("us.letter" <- c("8.5in","11in"),
       "us.legal" <- c("8.5in","14in"),
       "us.executive" <- c("7.2in","10.5in"),
       "a0" <- c("841mm","1189mm"),
       "a1" <- c("594mm","841mm"),
       "a2" <- c("420mm","594mm"),
       "a3" <- c("297mm","420mm"),
       "a4" <- c("210mm","297mm"),
       "a5" <- c("148mm","210mm"),
       "a6" <- c("105mm","148mm"),
       "a7" <- c("74mm","105mm"),
       "a8" <- c("52mm","74mm"),
       "a9" <- c("37mm","52mm"),
       "a10" <- c("26mm","37mm"),
       "b0" <- c("1000mm","1414mm"),
       "b1" <- c("707mm","1000mm"),
       "b2" <- c("500mm","707mm"),
       "b3" <- c("353mm","500mm"),
       "b4" <- c("250mm","353mm"),
       "b5" <- c("176mm","250mm"),
       "b6" <- c("125mm","176mm"),
       "b7" <- c("88mm","125mm"),
       "b8" <- c("62mm","88mm"),
       "b9" <- c("44mm","62mm"),
       "b10" <- c("31mm","44mm"),
       "c0" <- c("917mm","1297mm"),
       "c1" <- c("648mm","917mm"),
       "c2" <- c("458mm","648mm"),
       "c3" <- c("324mm","458mm"),
       "c4" <- c("229mm","324mm"),
       "c5" <- c("162mm","229mm"),
       "c6" <- c("114mm","162mm"),
       "c7" <- c("81mm","114mm"),
       "c8" <- c("57mm","81mm"),
       "c9" <- c("40mm","57mm"),
       "c10" <- c("28mm","40mm"),
       "d1" <- c("545mm","771mm"),
       "d2" <- c("385mm","545mm"),
       "d3" <- c("272mm","385mm"),
       "d4" <- c("192mm","272mm"),
       "d5" <- c("136mm","192mm"),
       "d6" <- c("96mm","136mm"),
       "d7" <- c("68mm","96mm"),
       "e3" <- c("400mm","560mm"),
       "e4" <- c("280mm","400mm"),
       "e5" <- c("200mm","280mm"),
       "e6" <- c("140mm","200mm"),
       "us.#10.envelope" <- c("41.in","9.5in"),
       "dl.envelop" <- c("110mm","220mm"),
       "ledger.tabloid" <- c("11in","17in"),
       "card.iso7810" <- c("54mm","85.6mm"),
       "card.us" <- c("2in","3.5in"),
       "card.eu" <- c("55mm","85mm"),
       "arch.a" <- c("9in","12in"),
       "arch.b" <- c("12in","18in"),
       "arch.c" <- c("18in","24in"),
       "arch.d" <- c("24in","36in"),
       "arch.e" <- c("36in","48in"),
       "arch.e1" <- c("30in","42in")
       )

SVG.ListDimensions <- function() {
  return(names(.SVG.STD.DIMS))
}

SVG.factory <- function(file,dims,landscape) {

  ## build a new SVG
  svg <- new("SVG")

  ## - from a file
  if(!missing(file))
    read.SVG(svg,file=file)

  ## - blank with given dimensions
  else if(!missing(dims)) {

    ## get orientation
    if(!missing(landscape)) {
      if(!is.logical(landscape))
        stop("'landscape' must be a logical value")
    }
    else
      landscape=FALSE
    
    ## predifined dims 
    if(is.character(dims)) {
      if(!dims %in% names(.SVG.STD.DIMS))
        stop("unknown dimension, use 'SVG.ListDimensions()' to get the list of available values")
      dims <- .SVG.STD.DIMS[[dims]]
    }

    if(is.list(dims) && (length(dims) == 2)) {
      w <- dims[[1]]
      h <- dims[[2]]
      
    }
    else 
      stop("invalid dimensions argument")
    
  }

  ## eop
  return(svg)
}
