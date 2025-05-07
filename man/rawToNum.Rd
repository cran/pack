\name{rawToNum}
\alias{rawToNum}
\title{ Raw to Numeric vector }
\description{
  Convert raw values to numeric.
}
\usage{
  rawToNum(x, nBytes = 1)
}
\arguments{
  \item{x}{ A raw vector to be converted }
  \item{nBytes}{ The number of bytes to use }
}
\value{
  A numeric value containing the bytes in \code{x}.
}
\author{ Joshua M. Ulrich }
\seealso{ \code{\link{numToRaw}} }
\examples{
  # Will be left null padded
  (x <- numToRaw(421,4))
  rawToNum(x,2)
  rawToNum(x,4)
}
\keyword{ utilities }
