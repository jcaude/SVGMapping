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

## M A P P I N G   -   T O O L T I P S
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

#' Tooltips Mapping Operation
#' 
#' This class allows to associate tooltips with any graphical shape. Tooltips 
#' are implemented as javascripts that are embedded with the template. They 
#' react to both  \emph{onmouseover} and \emph{onmouseout} events. Default 
#' tooltip contain only one line that is filled with the content of input
#' values.
#' 
#' @name MappingTooltips
#' @exportClass "MappingTooltips"
#' @aliases MappingTooltips-class
setClass("MappingTooltips",
         contains="Mapping"
         )

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

setMethod(f="initialize", signature="MappingTooltips",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod(.Object,...)

            ## eop
            return(.Object)
          }
          )

#' @aliases exec,MappingTooltips-method
#' @rdname mapping.exec-methods
setMethod(f="exec", signature="MappingTooltips",
          definition=function(.Object,svg)
          {
            ## check
            if(is.null(.Object@values)) {
              warning("Empty 'values', no tooltips will be created")
              return(invisible(.Object))
            }
            if(ncol(.Object@values) != 1)
              stop("In tooltip 'legacy' mode, mapping values array must have only one column")
            
            ## call super
            .Object <- callNextMethod(.Object,svg)

            ## 'legacy' mode
            .Object@.values <- data.frame(onmouseover=paste("legacyTooltip(evt,'",.Object@.values,"')"),
                                          onmouseout="hideUIitems(evt)")              

            ## update targets
            svg[.Object@targets, c("onmouseover","onmouseout")] <- .Object@.values

            ## eop
            return(invisible(.Object))
          }
          )

## F A C T O R Y
## --------------------------------------------------

#' Mapping Tooltips Factory
#' 
#' This function returns a \emph{default} \code{\link{MappingTooltips}} 
#' instance.
#' 
#' Using this factory function, input \emph{values} and \emph{targets} are 
#' initialized in the object. This mapping operation associate default tooltips 
#' to the targets shapes if applied to an SVG template. The tooltip is very
#' simple, with only one line that is filled using input \emph{values}. Thus,
#' the input value data.frame must only contain one column of character strings.
#' 
#' @name MappingTooltips.factory
#'   
#' @param data is the input dataset to use for this mapping
#'   
#' @param targets is the list of template node targets to alter. This can be a 
#'   list of SVG nodes identifiers or any node selection expression. By default 
#'   the targets are the row names of the input data variable.
#'   
#' @param trans.function is the transformation function that is applied onto the
#'   data, prior to the color mapping (see the \code{\link{Mapping}} class 
#'   documentation). By default the \emph{identity} function, which do not 
#'   transformed the input data, is assigned to the newly created instance.
#'   
#' @param trans.parameters is the list of parameters values associated with the 
#'   transformation function.
#'   
#' @return a \code{\link{MappingTooltips}} object
#'   
#' @export MappingTooltips.factory
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
#' ## one column named x. Row names are the circles identifiers.
#' dummy <- data.frame(x=c("This is Circle A",
#'                         "This is Circle B",
#'                         "This is Circle C",
#'                         "This is Circle D",
#'                         "This is Circle E",
#'                         "This is Circle F"),
#'                    row.names=circles)
#'                    
#' ## Now, let's create a MappingTooltips instance using the default factory
#' my.map <- MappingTooltips.factory(dummy)
#' 
#' ## Then, we apply this mapping object to the template, and show
#' ## it in the default browser.
#' ## mapping(template,my.map)
#' ## show(template)
MappingTooltips.factory <- function(data,targets,
                                    trans.function, trans.parameters) {

  ## check.
  if(missing(data))
    stop("'data' argument is absolutely required")
  
  ## init
  args <- list("MappingTooltips")
  args[["values"]] <- data
  
  ## fill mapping structure
  if(!missing(targets)) 
    args[["targets"]] <- targets
  else    
    args[["targets"]] <- row.names(data)
  if(!missing(trans.function)) args <- c(args,trans.function=trans.function)
  if(!missing(trans.parameters)) args[["trans.parameters"]] <- trans.parameters
  mapT <- do.call(new,args)
  
  ## eop
  return(mapT)
}
