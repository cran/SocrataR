\name{nextSocrataPage}
\alias{nextSocrataPage}
\title{nextSocrataPage
nextSocrataPage retrieves the next page from the current search.

nextSocrataPage retrieves the html source page for the next page.}
\usage{
  nextSocrataPage(search.Socrata.Call, page = NULL)
}
\arguments{
  \item{search.Socrata.Call}{The URL call of the last html
  page retrieval.}

  \item{page}{The page number to retrieve. If not entered
  it will use next page.}
}
\description{
  nextSocrataPage nextSocrataPage retrieves the next page
  from the current search.

  nextSocrataPage retrieves the html source page for the
  next page.
}
\examples{
## Not run
## Retrieve basic page
socrata.html <- search.Socrata()
data <- getListedDatasets(socrata.html)
socrata1.html <- nextSocrataPage(search.Socrata.Call)
data1 <- getListedDatasets(socrata1.html)
print(data)
print(data1)
}

