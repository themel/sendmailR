\name{sendmail}
\Rdversion{1.1}
\alias{sendmail}
\title{Send mail from within R}
\description{
  Simplistic sendmail utility for R. Uses SMTP to submit a message to a
  local SMTP server.
}
\usage{
sendmail(from, to, subject, msg, ..., headers=list(), control=list())
}
\arguments{
  \item{from}{From whom the mail message is.}
  \item{to}{Recipient(s) of the message.}
  \item{subject}{Subject line of message.}
  \item{msg}{Body text of message or a list containing
    \code{\link{mime_part}} objects.}
  \item{\dots}{...}
  \item{headers}{Any other headers to include. Cc and Bcc headers will be
  interpreted as additional recipients.}
  \item{control}{List of SMTP server settings. Valid values are the
    possible options for \code{\link{sendmail_options}}.}
}
\note{
Addresses may be specified in either "localpart@host.name", "<localpart@host.name>" or "Local Part <localpart@host.name>" format.
}
\seealso{\code{\link{mime_part}} for a way to add attachments.}
\author{Olaf Mersmann \email{olafm@datensplitter.net}}
\examples{
\dontrun{from <- sprintf("A friendly R Script <sendmailR@\%s>", Sys.info()[4])
to <- "Olaf Mersmann <olafm@datensplitter.net>"
subject <- "Hello from R"
body <- list("It works!", mime_part(iris))
sendmail(from, to, subject, body,
         control=list(smtpServer="ASPMX.L.GOOGLE.COM"))

# More recipients 
# To: Alice and Bob
# Cc: Trent 
# Bcc: Mallory and Eve 
subject <- "Hello everyone!"
sendmail(from, to=c("alice@example.net", "bob@example.net"), subject, body, headers=list(Cc="trent@example.net", Bcc=c("mallory@example.net","eve@example.net"))) 

}}
\keyword{utilities}
\alias{sendmail}

