\docType{methods}
\name{.xml}
\alias{.xml}
\alias{.xml,CSS-method}
\alias{.xml,Circle-method}
\alias{.xml,Ellipse-method}
\alias{.xml,Gradient-method}
\alias{.xml,GradientStop-method}
\alias{.xml,Layout-method}
\alias{.xml,Rectangle-method}
\alias{.xml,SVGNode-method}
\alias{.xml,SVGRect-method}
\alias{.xml,SVGShape-method}
\alias{.xml,Vector-method}
\title{SVG Node to XML Internal Representation Conversion}
\arguments{
  \item{object}{the SVG node}
}
\value{
  an XMLInternal Node or a list of attributes (virtual
  classes)
}
\description{
  This is the based method used by derived classes to
  transform a node into an XML internal description (aka
  XMLInternalNode).
}
\details{
  In most virtual classes the method does not return the
  XML representation, but a list of attributes. This list
  can be subsequently used by \emph{sub-classes} to
  generate valid XML internal representation. In the latter
  we usually expect a \code{XMLInternalNode} to be
  returned.
}

