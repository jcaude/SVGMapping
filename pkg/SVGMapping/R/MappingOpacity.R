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

## M A P P I N G   -   O P A C I T Y
## --------------------------------------------------

setGenericVerif <- function(name,y){if(!isGeneric(name)){setGeneric(name,y)}else{}}

setClass("MappingOpacity",
         representation(target.attribute="character"),
         contains="Mapping"
         )

setGenericVerif(name="targetAttribute", function(object) { standardGeneric("targetAttribute") })
setGenericVerif(name="targetAttribute<-", function(.Object,value) { standardGeneric("targetAttribute<-") })
setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingOpacity",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## default init.
            .Object@target.attribute <- "opacity"

            ## eop
            return(.Object)
          }
          )

setMethod(f="targetAttribute", signature="MappingOpacity",
          definition=function(object)
          {
            return(object@target.attribute)
          }
          )

setReplaceMethod(f="targetAttribute", signature="MappingOpacity",
                 definition=function(.Object,value)
                 {
                   ## check
                   if(!is.character(value))
                     stop("Target attribute 'value' must be a string")

                   ## init.
                   .Object@target.attribute <- value
                   return(.Object)
                 }
                 )

setMethod(f="exec", signature="MappingOpacity",
          definition=function(.Object,svg)
          {
            ## init.            
            ncond <- ncol(.Object@values)
            if(is.null(ncond)) ncond <- 1
            
            ## call super
            callNextMethod()

            ## check & bound
            if(ncond < 2) 
              .Object@.values <- sapply(.Object@.values, function(x) {return(min(max(0,x),1))})
            else
              .Object@.values <- apply(.Object@.values, 1, function(x) {return(min(max(0,max(x)),1))})

            ## set opacity values
            svg[.Object@targets, .Object@target.attribute] <- as.character(.Object@.values)

            ## eop
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------
MappingOpacity.factory <- function(data,targets=rownames(data),
                                   target.attribute="opacity",
                                   fn="Identity", fn.parameters=list()) {
  ## init.
  mapO <- new("MappingOpacity")

  ## fill mapping structure
  values(mapO) <- data
  targets(mapO) <- targets
  targetAttribute(mapO) <- target.attribute
  if(missing(fn.parameters))
    mapO <- setFunction(mapO,fn)
  else
    mapO <- setFunction(mapO,fn,fn.parameters)

  ## eop
  return(mapO)
}
