\docType{methods}
\name{opacity}
\alias{opacity}
\alias{opacity,GradientStop-method}
\alias{opacity,Layout-method}
\alias{opacity<-}
\alias{opacity<-,GradientStop-method}
\alias{opacity<-,Layout-method}
\title{Opacity level}
\arguments{
  \item{object}{an object with opacity attribute}
}
\value{
  the opacity level as a numeric value in the range [0,1]
}
\description{
  The methods presented here allows to control the opacity
  or transparency level of a visual shape. This level is
  defined as a numerical value in the range [0,1].
}
\details{
  \bold{Layout:} by default layout are invisible to the
  user. But one can control the opacity level of a layout
  using the methods presented here.

  Layout can be preview using the \code{\link{show}}
  method. The opacity method determines the visibility
  level of the layout borders.

  \bold{Layout:} the \code{opacity(object)<-value} method
  can be used to set the opacity level of the layout. The
  opacity \bold{value} paramter must be in the range [0,1].

  \bold{GradientStop:} the \code{opacity(object)} method
  returns the opacity leve of a Gradient stop.

  \bold{GradientStop:} the \code{opacity(object) <- value}
  method set the opacity level of a Gradient stop. This
  value must be in the range [0,1].
}

