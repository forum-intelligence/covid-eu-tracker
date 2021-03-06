% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crawl.R
\name{add_norway}
\alias{add_norway}
\alias{add_scotland}
\alias{add_germany}
\alias{add_italy}
\alias{add_spain}
\alias{add_ukraine}
\alias{add_switzerland}
\alias{add_poland}
\alias{add_netherlands}
\alias{add_belgium}
\alias{add_europe}
\alias{add_update_time}
\alias{add_sweden}
\alias{add_austria}
\alias{add_uk}
\title{Individual Crawlers}
\usage{
add_norway(
  puppet = system.file("crawl/crawl_no.js", package = "covid.crawler"),
  state = c("dev", "prod")
)

add_scotland(state = c("dev", "prod"))

add_germany(state = c("dev", "prod"))

add_italy(state = c("dev", "prod"))

add_spain(state = c("dev", "prod"))

add_ukraine(state = c("dev", "prod"))

add_switzerland(
  puppet = system.file("crawl/crawl_ch.js", package = "covid.crawler"),
  state = c("dev", "prod")
)

add_poland(state = c("dev", "prod"))

add_netherlands(state = c("dev", "prod"))

add_belgium(state = c("dev", "prod"))

add_europe(state = c("dev", "prod"))

add_update_time(date_time = Sys.time(), state = c("dev", "prod"))

add_sweden(state = c("dev", "prod"))

add_austria(state = c("dev", "prod"))

add_uk(
  puppet = system.file("crawl/crawl_uk.js", package = "covid.crawler"),
  state = c("dev", "prod")
)
}
\arguments{
\item{puppet}{Paths to puppet js crawling files for UK and Switzerland.}

\item{state}{Where to push data to, dev of prod database.}

\item{date_time}{Date time of last update (dislpayed on dashboard).}
}
\description{
Push data on individual countries and Europe via the ECDC.
}
\note{
These functions can be all called at once with \code{\link{crawl_covid}}.
}
