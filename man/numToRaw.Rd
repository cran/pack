\name{numToRaw}
\alias{numToRaw}
\title{ Numeric to Raw vector }
\description{
  Convert numeric values to a raw vector.
}
\usage{
  numToRaw(x, nBytes = 1)
}
\arguments{
  \item{x}{ A number to be converted (must be > 0) }
  \item{nBytes}{ The number of bytes to use }
}
\value{
  A raw vector containing the bytes representing \code{x}.
}
\author{ Joshua M. Ulrich }
\seealso{ \code{\link{rawToNum}} }
\examples{
  # Will be left null padded
  (x <- numToRaw(421,4))
  rawToNum(x,2)
  rawToNum(x,4)
}
\keyword{ utilities }
