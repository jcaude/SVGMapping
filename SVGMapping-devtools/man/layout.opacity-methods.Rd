\docType{methods}
\name{opacity}
\alias{opacity}
\alias{opacity,Layout-method}
\alias{opacity<-}
\alias{opacity<-,Layout-method}
\title{Layout Opacity level}
\arguments{
  \item{object}{a Layout object}
}
\value{
  the opacity level as a numeric value in the range [0,1]
}
\description{
  By default layout are invisible to the user. But one can
  control the opacity level of a layout using the methods
  presented here.
}
\details{
  Layout can be preview using the \code{\link{show}}
  method. The opacity method determines the visibility
  level of the layout borders.

  The \code{opacity(object)<-value} method can be used to
  set the opacity level of the layout. The opacity
  \bold{value} paramter must be in the range [0,1].
}

