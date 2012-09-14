\docType{methods}
\name{targets}
\alias{targets}
\alias{targets,Mapping-method}
\alias{targets<-}
\alias{targets<-,Mapping-method}
\alias{values}
\alias{values,Mapping-method}
\alias{values<-}
\alias{values<-,Mapping-method}
\alias{values<-,MappingLinks-method}
\title{Mapping Data (Values and Targets) Accessors}
\arguments{
  \item{object}{the mapping instance object}
}
\value{
  an object set identifiers.
}
\description{
  Mapping requires a set of \strong{values} and
  \strong{targets} to proceed. \strong{Values}, after
  transformation, are used to modify various attributes of
  graphical shapes. \strong{Targets} are used to identify
  the shapes to alter.

  <title already defined>

  <title already defined>

  <title already defined>
}
\details{
  \code{Targets} must be a valid node selectors (SEE
  SVG:NODE-SELECTORS). The most common node selector is a
  set of shape identifiers or labels (note: labels are not
  part of the official SVG 1.1 specification and a valid
  namespace must be provided, such as Inkscape NS). But
  more complex target selection can be done using XPath
  expression for example.

  The \code{targets(object)} method retrieve the list of
  target shapes to modify
}

