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

setClass("MappingTooltips",
         representation(tooltip.style="character"
                        ),
         contains="Mapping"
         )

setGenericVerif(name="tooltipStyle", function(object) { standardGeneric("tooltipStyle") })
setGenericVerif(name="tooltipStyle<-", function(.Object,value) { standardGeneric("tooltipStyle<-") })
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingTooltips",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## default init.
            .Object@tooltip.style <- "legacy"

            ## eop
            return(.Object)
          }
          )

setMethod(f="tooltipStyle", signature="MappingTooltips",
          definition=function(object)
          {
            return(object@tooltip.style)
          }
          )

setReplaceMethod(f="tooltipStyle", signature="MappingTooltips",
                 definition=function(.Object, value)
                 {
                   ## check
                   if(!is.character(value) || !(value %in% c("legacy","bio.array"))) 
                      stop("'value' must be a string equals to 'legacy' or 'bio.array'")
                   
                   ## init.
                   .Object@tooltip.style <- value
                   return(.Object)
                 }
                 )

setMethod(f="exec", signature="MappingTooltips",
          definition=function(.Object,svg)
          {
            ## check
            if(.Object@tooltip.style == "legacy" && ncol(.Object@values) != 1)
              stop("In tooltip 'legacy' mode, mapping values array must have only one column")
            if(.Object@tooltip.style == "bio.array" && ncol(.Object@values) > 3)
              stop("In tooltip 'bio.array' mode, mapping values array must have only ",
                   "three columns (name,description and fold-change)")
            
            ## call super
            callNextMethod()

            ## 'legacy' mode
            if(.Object@tooltip.style == "legacy") {
              .Object@.values <- data.frame(onmouseover=paste("legacyTooltip(evt,'",.Object@.values,"')"),
                                            onmouseout="hideUIitems(evt)")              
            }

            ## 'bio.array' mode
            else if (.Object@tooltip.style == "bio.array") {
            }

            ## update targets
            svg[.Object@targets, c("onmouseover","onmouseout")] <- .Object@.values

            ## eop
            return(invisible(.Object))
          }
          )
