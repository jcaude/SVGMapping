\docType{methods}
\name{SVG}
\alias{SVG}
\alias{SVG,SVG-method}
\alias{SVG<-}
\alias{SVG<-,SVG-method}
\title{SVG XML content}
\arguments{
  \item{object}{is an SVG class instance}
}
\value{
  This methods returns an \code{XMLInternalDocument} object
}
\description{
  This methods allows to access to the XML content (parsed
  using the \code{XML} package) of an SVG object.
}
\details{
  \code{SVG(object)} returns an \code{XMLNode}

  The \code{SVG(object) <- value} allows to change the SVG
  content. Value must be an \code{XMLInternalDocument} that
  corresponds to a valid SVG document.
}

