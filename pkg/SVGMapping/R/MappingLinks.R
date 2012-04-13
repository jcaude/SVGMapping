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

setClass("MappingLinks",
         contains="Mapping"
         )

setGenericVerif(name="exec", function(.Object,svg) { standardGeneric("exec") })

setMethod(f="initialize", signature="MappingLinks",
          definition=function(.Object,...)
          {
            ## super
            .Object <- callNextMethod()

            ## eop
            return(.Object)
          }
          )

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

setMethod(f="exec", signature="MappingLinks",
          definition=function(.Object,svg)
          {
            ## init.            
            ncond <- ncol(.Object@values)
            if(is.null(ncond)) ncond <- 1

            ## check
            if(ncond > 1) stop("can't proceed 'urls' with more than one column data arrays")
            
            ## call super
            callNextMethod()
            
            ## insert urls into document
            nset <- svg[.Object@targets]
            for(i in 1:length(nset)) {
              node <- nset[[i]]
              if(is(node,"XMLNodeSet")) node <- node[[1]]  ### WE HAVE TO CHECK THIS.. here targets=c("x","y") and get a list of list (nodeset of nodeset)
              url <- .Object@.values[i,]
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
##--------------
MappingLinks.factory <- function(data,targets=rownames(data),
                                  fn="None", fn.parameters=list()) {

  ## init.
  mapL <- new("MappingLinks")

  ## fill mapping structure
  values(mapL) <- data
  targets(mapL) <- targets

  ## set transform function
  mapL <- setFunction(mapL,fn,fn.parameters)

  ## eop
  return(mapL)
}
