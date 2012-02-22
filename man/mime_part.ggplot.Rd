\name{mime_part.ggplot}
\alias{mime_part.ggplot}
\title{Creates a MIME part from a ggplot2 plot object...}
\usage{\method{mime_part}{ggplot}(x, name=deparse(substitute(x)), device=pdf, ...)
}
\description{Creates a MIME part from a ggplot2 plot object}
\details{Writes a PDF file of the plot defined by \code{x} and turns this
PDF file into a file attachment.}
\value{An S3 \code{mime_part} object.}
\arguments{\item{x}{A \code{ggplot} object}
\item{name}{Name of attachment (sans .pdf extension).}
\item{device}{Graphics device used to render the plot. Defaults to
\code{pdf}.}
\item{...}{Ignored.}
}

