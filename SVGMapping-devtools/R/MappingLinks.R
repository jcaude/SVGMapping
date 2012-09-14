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

## M A P P I N G   -   L I N K S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Links Mapping Operation
#' 
#' This class allows to associate hyperlinks (as \emph{HTML HREF} references) to
#' the target shapes.
#' 
#' Used of the Mapping Links operation is straightforward. One has onnly to
#' provide html urls as input values. You can also initialize an url pattern (as
#' a standard \code{\link{sprintf}} format expression) that will be used to
#' forge the output urls.
#' 
#' @name MappingLinks
#' @exportClass "MappingLinks"
#' @aliases MappingLinks-class
setClass("MappingLinks",
         representation(url.pattern="character"),
         contains="Mapping"
         )

#' URL Pattern Accessor
#' 
#' URL patterns are used to process input values, using the
#' \code{\link{sprintf}} function, to forge the final URLs. This is very handy
#' (it reduce data duplcation) and less error prone (url pattern is unique for
#' all input values).
#' 
#' The \code{urlPattern(object)} method retrieves the current URL pattern value.
#' 
#' @name urlPattern
#'   
#' @param object the mapping instance object
#'   
#' @return the url pattern value as a character string
#'   
#' @rdname mappinglinks.urlpattern-methods
#' @exportMethod urlPattern
#' @docType methods
setGeneric(name="urlPattern", function(object) { standardGeneric("urlPattern") })

#' <title already defined>
#' 
#' 
#' 
#' The \code{urlPattern(object) <- value} method sets the url pattern value 
#' of the \code{object} mapping instance
#' 
#' @name urlPattern<-
#' @rdname mappinglinks.urlpattern-methods
#' @exportMethod urlPattern<-
#' @docType methods
setGeneric(name="urlPattern<-", function(.Object,value) { standardGeneric("urlPattern<-") })


#' <title already defined>
#' 
#' 
#' 
#'  @name exec
#'  @rdname mapping.exec-methods
#'  @exportMethod exec
#'  @docType methods 
NULL
#setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingLinks",
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
            
            ## super
            .Object <- callNextMethod(.Object,...)
            
            ## get args
            args = list(...)
            args.names = names(args)
            if(is.null(args.names)) args.names <- list()
            
            ## default init.
            urlPattern(.Object) <- .arg("url.pattern",character(0))
            
            ## eop
            return(.Object)
          }
          )

#' @aliases urlPattern,MappingLinks-method
#' @rdname mappinglinks.urlpattern-methods
setMethod(f="urlPattern", signature="MappingLinks",
          definition=function(object)
          {
            return(object@url.pattern)
          }
)

#' @name urlPattern<-
#' @aliases urlPattern<-,MappingLinks-method
#' @rdname mappinglinks.urlpattern-methods
setReplaceMethod(f="urlPattern", signature="MappingLinks",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!(is.atomic(value) && is.character(value)))
                     stop("URL pattern (format string) 'value' must be character strings")
                   
                   ## init.
                   .Object@url.pattern <- value
                   return(.Object)
                 }
)

## This is just a 1D data input checking.. we only forge ONE link per data-row 
#' @name values<-
#' @aliases values<-,MappingLinks-method
#' @rdname mapping.data-methods
setReplaceMethod(f="values", signature="MappingLinks",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(is.null(value)) return(.Object)
                   
                   ## check 1D data
                   if(!is.vector(value) && (ncol(value) > 1))
                     stop("'value' can only be a 'vector/list' or one column 'array/data.frame'")

                   ## super & eop
                   .Object <- callNextMethod(.Object,value)
                   return(.Object)
                 }
                 )

#' @aliases exec,MappingLinks-method
#' @rdname mapping.exec-methods 
setMethod(f="exec", signature="MappingLinks",
          definition=function(.Object,svg)
          {
            ## init.            
            ncond <- ncol(.Object@values)
            if(is.null(ncond)) ncond <- 1
            url.pattern <- urlPattern(.Object)

            ## check
            if(ncond > 1) stop("can't proceed 'urls' with more than one column data arrays")
            
            ## call super
            .Object <- callNextMethod(.Object,svg)
            
            ## insert urls into document
            nset <- svg[.Object@targets]
            for(i in 1:length(nset)) {
              node <- nset[[i]]
              ## WE HAVE TO CHECK THE EXPRESSION BELOW.. here targets=c("x","y")
              ## and get a list of list (nodeset of nodeset)
              if(is(node,"XMLNodeSet")) node <- node[[1]]  
              url <- .Object@.values[i,]
              if(length(url.pattern) > 0) url <- sprintf(url.pattern,url)
              href <- newXMLNode("a", attrs=list("xlink:href"=url, target="_blank"),
                                 namespaceDefinitions=c(xlink="http://www.w3.org/1999/xlink"))
              replaceNodes(oldNode=node,newNode=href)
              addChildren(href,node)
            }

            ## eop
            return(invisible(.Object))
          }
          )

## F A C T O R Y
## ------------------------------------------------------------

#' Mapping Links Factory
#' 
#' This function returns a \code{\link{MappingLinks}} instance that can be used 
#' to associate hyperlinks to a set of \emph{targets} SVG elements. The links
#' URL are forge using input values and if set the url pattern.
#' 
#' @name MappingLinks.factory
#'   
#' @param data is the input dataset to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'      
#' @param url.pattern contains the url pattern format string that is used to
#'   forge url from input values
#'   
#' @param trans.function is the transformation function that is applied onto the
#'   data, prior to the color mapping (see the \code{\link{Mapping}} class
#'   documentation). By default the \emph{identity} function, which do not
#'   transformed the input data, is assigned to the newly created instance.
#'   
#' @param trans.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingLinks}} object
#' 
#' @export MappingLinks.factory
#' 
#' @examples
#' ## load 'basic-sample.svg' a demo SVG template. 
#' ## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))
#' 
#' ## In this demo template, the top six circles are identified with the 
#' ## 'circle.A' ... 'circle.F' svg ID attributes. We will generate a list that
#' ## contains such identifiers..
#' circles <- paste("circle.",LETTERS[1:6],sep="")
#' 
#' ## Then, we will use the following dummy dataset. This data.frame contains 
#' ## one column named x. Row names are the circles identifiers. We will 
#' ## associate a link to the twitter accounts stored in the data-frame
#' dummy <- data.frame(x=c("BarackObama","MittRomney","NYTLive",
#'                         "Zidane_Zinedine","KingJames","IronmanTri"),
#'                    row.names=circles)
#'                    
#' ## Now, let's create a MappingLinks instance using the default factory
#' ## function. We initialize the url pattern to point to twitter 
#' my.map <- MappingLinks.factory(dummy[,"x",drop=FALSE])
#' urlPattern(my.map) <- "http://twitter.com/"
#' 
#' ## Then, we apply this mapping object to the template, and show
#' ## it in the default browser.
#' ## mapping(template,my.map)
#' ## show(template)
MappingLinks.factory <- function(data,targets,
                                 url.pattern,
                                 trans.function, trans.parameters) {

  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")
  
  ## init.
  args <- list("MappingLinks")
  args[["values"]] <- data
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(url.pattern)) arcs <- c(args,url.pattern=url.pattern)
  if(!missing(trans.function)) args <- c(args,trans.function=trans.function)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  mapL <- do.call(new,args)
  
  ## eop
  return(mapL)
}
