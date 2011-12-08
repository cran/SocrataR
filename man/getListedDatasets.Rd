\name{getListedDatasets}
\alias{getListedDatasets}
\title{getListedDatasets
getListedDatasets lists databases in the retrieved html from http://opendata.socrata.com/.

getListedDatasets returns a dataframe with the datasets found. It returns max 10 per page}
\usage{
  getListedDatasets(SocrataHtml)
}
\arguments{
  \item{SocrataHtml}{The html source retrieved by the
  \link{search.Socrata} function}
}
\description{
  getListedDatasets getListedDatasets lists databases in
  the retrieved html from http://opendata.socrata.com/.

  getListedDatasets returns a dataframe with the datasets
  found. It returns max 10 per page
}
\examples{
## Not run
## Retrieve basic page
socrata.html <- search.Socrata()
getListedDatasets(socrata.html)


## Not run
## Retrieve datasets related to airplane
socrata.html <- search.Socrata(topic= 'airplane')
getListedDatasets(socrata.html)
}

