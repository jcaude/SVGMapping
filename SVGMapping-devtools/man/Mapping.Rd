\name{Mapping}
\alias{Mapping}
\alias{Mapping-class}
\title{Mapping Root Class (VIRTUAL)}
\description{
  This class is the root class for all mapping operations.
  This class can't be directly instantiate and must be
  derived.
}
\details{
  Mapping operations allows to transform the content of a
  template. This process used \strong{data}, a set of
  \strong{values} and \strong{targets}. First values are
  transformed using built-in or user-defined function (see
  \link{transFunction}). Then, these newly transformed
  values are used for by mapping operations (\emph{eg}
  color modification, change of the opacity) to alter the
  set of \strong{targets} into the template. It is
  noteworthy to mention that Mapping processes can be stack
  to create complex templante rendering.
}

