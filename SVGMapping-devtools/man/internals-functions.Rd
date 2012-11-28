\name{.callMethod}
\alias{.callMethod}
\title{call a class method}
\usage{
  .callMethod(name, signature, ...)
}
\arguments{
  \item{name}{the method to called}

  \item{signature}{the class where the method will be
  searching for}

  \item{...}{method parameters}
}
\value{
  the value returned by the called method
}
\description{
  This function allows to call a method of a given class
  from another class. This method is very usefull in a
  multiple heritage context to call several constructor.
}
\details{
  This function has been develop to handle S4 multiple
  constructor calls in a class that has multiple
  representation (\emph{ie} multiple heritage) in its class
  definition. In this case using the standard
  \link{callNextMethod} function is expected to call the
  parent method of the first herited class. Thus, if one
  wants to also called the parents methods of others
  herited classes it can use this function. This function
  is similar to \code{callNextMethod} except that one has
  to provide the \emph{name} and the \emph{signature}
  (class name) of the parent class. Others arguments are
  the same.
}
\examples{
## Here we define the first parent class
setClass("Parent_1", representation(A="numeric"))
setMethod(f="initialize", signature="Parent_1",
          definition=function(.Object,...)
          {
            .Object@A <- 1
            cat("Parent 1 constructor called..\\n")
            return(.Object)
          })

## Here we define the second parent class
setClass("Parent_2", representation(B="numeric"))
setMethod(f="initialize", signature="Parent_2",
          definition=function(.Object,...)
          {
            .Object@B <- 2
            cat("Parent 2 constructor called..\\n")
            return(.Object)
          })

## Here we create a class that herited from both parent classes
setClass("Son_of_1_2", contains=c("Parent_1","Parent_2"))
setMethod(f="initialize", signature="Son_of_1_2",
          definition=function(.Object,...)
          {
            ## call 'Parent_1' constructor
            .Object <- callNextMethod(.Object,...)
            ## call 'Parent_2' constructor
            .Object <- .callMethod("initialize","Parent_2",.Object,...)
            return(.Object)
          })

## Create a new instance
sonny <- new("Son_of_1_2")
}

