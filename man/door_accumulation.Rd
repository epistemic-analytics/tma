% Please edit documentation in R/replace_door.R
\name{door_accumulation}
\alias{door_accumulation}
\title{Apply door smoothing to an accumulation object}
\usage{
door_accumulation(accum, ...)
}
\arguments{
\item{accum}{Object from \code{tma::accumulate()}.}

\item{...}{Arguments passed to \code{\link{door}()}, including
\code{unit_col}, \code{time_col}, \code{method}, and method-specific
settings.}
}
\value{
\code{accum} with door-smoothed co-occurrence columns.
}
\description{
This convenience wrapper runs \code{\link{door}()} on an accumulation object's
co-occurrence table and then writes the door-smoothed values back with
\code{\link{replace_door}()}. Use \code{\link{door}()} directly when you want
to inspect or compare the intermediate \code{door_*} columns before replacing
the accumulation values.
}
