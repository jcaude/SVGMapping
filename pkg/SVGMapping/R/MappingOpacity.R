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
         representation(),
         contains="Mapping"
         )

setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingOpacity",
          definition=function(.Object,...)
          {            
            ## super
            .Object <- callNextMethod()

            ## eop
            return(.Object)
          }
          )


setMethod(f="exec", signature="MappingOpacity",
          definition=function(.Object,svg)
          {
            ## call super
            callNextMethod()

            ## check & bound
            if(ncond < 2) 
              .Object@.values <- sapply(.Object@.values, function(x) {return(min(max(0,x),1))})
            else
              .Object@.values <- sapply(.Object@.values, c(1,2), function(x) {return(min(max(0,x),1))})

            ## set opacity values
            svg[.Object@targets, "style::opacity"] <- as.character(.Object@.values)

            ## eop
            return(invisible(.Object))
          }
          )

## F A C T O R Y
##--------------
MappingOpacity.factory <- function(data,targets=rownames(data),                                   
                                  fn="Identity", fn.parameters=list()) {
  ## init.
  mapO <- new("MappingOpacity")

  ## fill mapping structure
  values(mapO) <- data
  targets(mapO) <- targets
  mapO <- setFunction(mapO,fn,fn.parameters)

  ## eop
  return(mapO)
}
