\docType{methods}
\name{addScript}
\alias{addScript}
\alias{addScript,SVG-method}
\title{SVG Script Processing}
\arguments{
  \item{object}{is the SVG document object}

  \item{id}{(optional) the script to add or update}
}
\value{
  the list of nodes that have been added to the SVG
  document.
}
\description{
  The \code{addScript(object)} method allows to insert
  script (either as inline text or href references) within
  the SVG document.
}
\details{
  First script must be added to the SVG object using the
  \code{\link{jsHRefs()}} or \code{\link{jsInlines()}}
  method. If an \code{ID} is set as argument only the
  related script will be updated. Otherwise, all referenced
  scripts are be updated.
}

