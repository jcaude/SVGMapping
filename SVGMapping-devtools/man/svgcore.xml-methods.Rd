\docType{methods}
\name{.xml}
\alias{.xml}
\alias{.xml,Circle-method}
\alias{.xml,CSS-method}
\alias{.xml,Ellipse-method}
\alias{.xml,Gradient-method}
\alias{.xml,GradientStop-method}
\alias{.xml,Layout-method}
\alias{.xml,LinearGradient-method}
\alias{.xml,Mask-method}
\alias{.xml,RadialGradient-method}
\alias{.xml,Rectangle-method}
\alias{.xml,SVGCircle-method}
\alias{.xml,SVGEllipse-method}
\alias{.xml,SVGLine-method}
\alias{.xml,SVGNode-method}
\alias{.xml,SVGRect-method}
\alias{.xml,SVGShape-method}
\alias{.xml,Vector-method}
\title{XML Internal Representation Conversion}
\arguments{
  \item{object}{a SVG class instance}
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

