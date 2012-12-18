\docType{methods}
\name{jsAnimation}
\alias{jsAnimation}
\alias{jsAnimation,SVG-method}
\alias{jsAnimation<-}
\alias{jsAnimation<-,SVG-method}
\alias{jsHRefs}
\alias{jsHRefs,SVG-method}
\alias{jsHRefs<-}
\alias{jsHRefs<-,SVG-method}
\alias{jsInlines}
\alias{jsInlines,SVG-method}
\alias{jsInlines<-}
\alias{jsInlines<-,SVG-method}
\title{SVG Javascript Accessors}
\arguments{
  \item{object}{is the SVG document object}
}
\value{
  \code{jsAnimations} returns a logical value,
  \code{jsHRefs} and \code{jsInlines} return lists
}
\description{
  These methods allows to associate some javascript code to
  an SVG document. This code could be directly attache to
  the document or referenced as an external document.
}
\details{
  The \code{jsAnimation(object)} method returns \code{TRUE}
  if the SVG document have some animations or \code{FALSE}
  otherwise.

  The \code{jsAnimation(object) <- value} method allows to
  enable or disable the SVGMapping animation engine. Here,
  \emph{value} is logical.

  The \code{jsHRefs(object)} method returns the list of
  Javascript HREF that are associated with the SVG
  documemnt.

  The \code{jsHRefs(object) <- value} method associates a
  named list of Javascript HREF with the SVG documemnt.
  List names are used as identifiers and must be unique
  across the whole SVG document. One can use the \code{uid}
  method to automatically forge new IDs.

  The \code{jsInlines(object)} method returns the list of
  inline Javascript scripts that are associated with the
  SVG documemnt.

  The \code{jsInlines(object) <- value} method associates a
  named list of inline Javascripts with the SVG documemnt.
}

