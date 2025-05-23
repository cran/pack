\name{pack}
\alias{pack}
\title{ Pack raw vectors }
\description{
  Combine values into a raw vector according to the values in \code{template}.
}
\usage{
  pack(template, \dots)
}
\arguments{
  \item{template}{ A string, see 'Details' }
  \item{\dots}{ Values/objects to be packed into a raw vector }
}
\details{
  Currently supported \code{template} values are:\cr
  'a' - A null padded string\cr
  'A' - A space padded string\cr
  'b' - An ascending bit order binary vector, (must be a multiple of 8 long)\cr
  'B' - An descending bit order binary vector, (must be a multiple of 8 long)\cr
  'C' - An unsigned char (8-bit byte/octet) value\cr
  'v' - An unsigned short (16-bit) in "VAX" (little-endian) order\cr
  'V' - An unsigned long (32-bit) in "VAX" (little-endian) order\cr
  'x' - A null byte\cr\cr
  Both 'a' and 'A' may be followed by a repeat value.  A repeat value of '*'
  will cause the remainder of the bytes in \code{values} to be placed in
  the last element.\cr\cr
  '/' allows packing and unpacking of a sequence of values where the packed
  structure contains a packed item count followed by the packed items themselves.\cr\cr
  If \code{template} requires more arguments to \code{pack} than actually given,
  \code{pack} pads with null bytes.  If \code{template} requires fewer
  arguments to \code{pack} than actually given, extra arguments are ignored.
}
\value{
  A raw vector following the elements in \code{template}.
}
\references{
  \url{https://perldoc.perl.org/functions/pack}
}
\author{ Joshua M. Ulrich }
\seealso{ \code{\link{unpack}} }
\examples{
  (x <- pack('A4 C v A8 V', 'pack', 2, 8, 'sequence', 68098))
  (u1 <- unpack('A4 C H*', x))
  (u2 <- unpack('v/A V', u1[[3]]))
}
\keyword{ utilities }
