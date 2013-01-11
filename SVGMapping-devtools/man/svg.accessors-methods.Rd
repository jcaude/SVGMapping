\docType{methods}
\name{[}
\alias{[}
\alias{[,SVG-method}
\alias{[<-}
\alias{[<-,SVG-method}
\title{SVG Content Accessors}
\arguments{
  \item{x}{is the SVG object.}

  \item{i}{the node selector. See details below.}

  \item{j}{the attribute selector. See details below.}

  \item{drop}{This argument is ignored.}
}
\value{
  Depending on the selector, this function returns XML
  Node(s) either as atomic, vector, arrays or
  \code{\link{xmlNodeSet}}. See the \emph{note} section for
  details.
}
\description{
  These methods allows to get/set content of an SVG object.
  They heavily rely on \emph{XPath} expression (either
  automatically forged or provided by the user), thus allow
  very complex queries.
}
\note{
  SVG accessors allow a direct access to SVG XML nodes,
  given a \emph{node selector} and optionally an
  \emph{attribute selector}. These accessors can be used as
  getter or setter.

  \emph{node selector}, in its simplest form, is provided
  as a single value. In this case, the default search
  attribute (see \code{\link{defaultSearchAttrName}}) is
  used to select the node with the related value (see
  examples). But if one wants to use a different attribute
  as a key for node selection then the
  \emph{attname::value} selector must be used. Note that in
  this case the \emph{::} is used as a query seperator. For
  more complex cases, XPath queries can be used using the
  \emph{xpath::query} selector, where \emph{query} contains
  the XPath instructions. \emph{node selector} can be
  either a character string or a vector of strings. If no
  \emph{attribute selector} is provided, depending on the
  node selected an \code{\link{xmlNode}} or
  \code{\link{xmlNodeSet}} is returned.

  \emph{attribute selector} is used to select SVG
  attributes of a set a nodes selected by the \emph{node
  selector}. The \emph{attribute selector} is in most case
  the name of the SVG attribute. But similarly to the
  \emph{node selector} syntax, one can select CSS
  sub-attribute using the \emph{attname:subattribute}
  syntaxe. Finaly, this selector can be an atomic or vector
  of attributes. In the first case a vector of
  \code{\link{xmlNode}} is returned. In the second case an
  array of \code{\link{xmlNode}} is returned.

  These accessors can also be used to change the content of
  the SVG document. This is done using the
  \code{svg[node-selector,attribute-selector] <- value}
  syntax, where \code{svg} is an SVG object. If the
  required attribute doesn't exists in the list of
  attributes of the selected nodes, than a new attribute
  with the given name will be created. Otherwise, the
  attribute value will be updated. The behavior of this
  command depends, upon others, on the \code{value} type.
  Hereafter you can find the most common cases:

  \itemize{

  \item \code{value} is an atomic value. If the node
  selection contains only one node, \emph{value} will be
  assigned to the selected attribute. If the selection
  contains multiple nodes, \emph{value} will be assigned to
  the selected attribute of each nodes.

  \item \code{value} is a list of atomic values. In this
  case the length of the selected nodes must match the
  length of the \emph{value} list. The list of values will
  be assign to the selected attribute of the nodes. On has
  to be very cautious about the nodes order (usually in the
  same order as they appear in the SVG document).

  \item \code{value} is a list of vectors. This case is
  very similar to the previous one except that the node
  selection contains multiple attributes. Thus, dimensions
  of the \code{selected nodes x selected attributes} must
  match \code{length(value) x length(vectors)}. In this
  case, vector values are assigned to all selected
  attributes of the node selection. \strong{While this
  syntax is allowed by this package we hardly suggest to
  use the array format below}.

  \item \code{value} is an array. This is very similar to
  the previous case. The same constraints on the node
  selection must be complied and the same type of
  assignement occurs. But compare to the list of vectors
  syntax there couldn't be discrepencies between vectors
  length.

  }

  \emph{Text nodes} are handled using \code{SVG.VALUE}, a
  specific \emph{attribute selector}. \code{SVG.VALUE} is a
  factor defined in the package that is used instead of an
  \emph{attribute selector}. Thus, commands like
  \code{SVG[node-selector,SVG.VALUE]} will retrieve the
  content of any text node encapsulated within the
  \emph{node-selector}. In this particular implementation
  we only considered nodes where the text content is
  encapsulated within a \code{TSPAN} element.
}
\examples{
## load 'basic-sample.svg' a demo SVG template.
## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))
x <- 1
##
}

