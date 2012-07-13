\docType{methods}
\name{cssClass}
\alias{cssClass}
\alias{cssClass,CSS-method}
\alias{cssClass<-}
\alias{cssClass<-,CSS-method}
\alias{cssStyle}
\alias{cssStyle,CSS-method}
\alias{cssStyle<-}
\alias{cssStyle<-,CSS-method}
\title{CSS Attributes Accessors}
\arguments{
  \item{object}{is an object that inherits from the CSS
  class}
}
\value{
  a core attribute as a string
}
\description{
  These methods are accessors to the \emph{CSS} attributes
  of an object. These attributes are described in the SVG
  1.1 specifications document.
}
\details{
  The \code{cssClass(object)} method returns the CSS
  \emph{class} attributes of an object.

  The \code{cssClass(object) <- value} method sets the CSS
  \emph{class} attributes of an object.

  The \code{cssStyle(object)} method returns the CSS
  \emph{style} attributes of an object.

  The \code{cssStyle(object) <- value} method sets the CSS
  \emph{style} attributes of an object.
}

