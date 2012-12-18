\docType{methods}
\name{uid}
\alias{uid}
\alias{uid,SVG-method}
\title{Unique ID Generator}
\arguments{
  \item{object}{is the SVG instance}

  \item{prefix}{is a string used to prefix forged
  identifiers}

  \item{n}{is the number of ids to return}
}
\value{
  a list of unique ids
}
\description{
  Forge unique identifier(s) among the SVG object. This
  function used random sequences and an optional prefix to
  build new ids.
}
\note{
  this function is not thread-safe at all.
}

