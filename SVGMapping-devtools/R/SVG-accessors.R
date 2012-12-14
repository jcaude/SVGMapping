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

## S V G  -- ACCESSORS
## --------------------------------------------------

#' SVG Content Accessors
#' 
#' These methods allows to get/set content of an SVG object. They heavily rely
#' on \emph{XPath} expression (either automatically forged or provided by the
#' user), thus allow very complex queries.
#' 
#' @name [
#' 
#' @param x is the SVG object.
#' @param i the node selector. See details below.
#' @param j the field selector. See details below.
#' @param drop This argument is ignored.
#' 
#' @return This function returns a list of XML Nodes.
#' 
#' @rdname svg.accessors-methods
#' @docType methods
NULL

#' @rdname svg.accessors-methods
#' @aliases [,SVG-method
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

#' @name [<-
#' @rdname svg.accessors-methods
#' @aliases [<-,SVG-method
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
