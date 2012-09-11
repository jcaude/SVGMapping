\name{MappingValues}
\alias{MappingValues}
\alias{MappingValues-class}
\title{Values Mapping Operation}
\description{
  This class allows to convert input values into output
  values that can be assigned to a given shape attribute.
  For example, arbitrary numerical values can be used to
  change the width or opacity level of target shapes.
}
\details{
  The Mapping Values operation use the input value, without
  any modification (such as the linear mapping done in the
  \code{\link{MappingColors}} class), expect a character
  transformation (using default R casting). Optionally, one
  can postfix the output values with units using the
  \code{\link{targetUnit}} method. Thus, one has to use a
  transformation function (see the \code{\link{Mapping}}
  class) to change the input values prior to the mapping.
  It can be useful for example for scaling or unit
  conversion purposes.
}

