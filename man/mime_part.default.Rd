\name{mime_part.default}
\alias{mime_part.default}
\title{Default MIME part method...}
\usage{\method{mime_part}{default}(x, name, ...)
}
\description{Default MIME part method}
\details{Creates a string representation of the object \code{x} using
\code{dput}. This representation is then turned into a file
attachment.}
\value{An S3 \code{mime_part} object.}
\arguments{\item{x}{R object}
\item{name}{Filename used for attachment (sans the .R extension)}
\item{...}{Ignored.}
}

