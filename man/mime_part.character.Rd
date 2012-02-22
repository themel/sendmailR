\name{mime_part.character}
\alias{mime_part.character}
\title{Create a MIME part from a character string.}
\usage{\method{mime_part}{character}(x, name, ...)
}
\description{Create a MIME part from a character string. If the string matches
a filename, a MIME part containing that file is returned instead.}
\value{An S3 \code{mime_part} object.}
\arguments{\item{x}{Character string, possibly a filename.}
\item{name}{Name of attachment.}
\item{...}{Ignored.}
}

