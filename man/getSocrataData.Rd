\name{getSocrataData}
\alias{getSocrataData}
\title{getSocrataData
getSocrataData lists databases in the retrieved html from http://opendata.socrata.com/.

getSocrataData returns a dataframe of the selected dataset and optionally downloads the dataset.}
\usage{
  getSocrataData(rownr, datasetList, filetype = "csv",
    file = paste(datasetList[rownr, 4], ".", filetype, sep = ""),
    load.data = TRUE)
}
\arguments{
  \item{rownr}{The row in the datasetlist that has the
  dataset you want to load}

  \item{datasetList}{The dataframe retrieved by the
  \link{getListedDatasets} function}

  \item{filetype}{The file type of the data that shall be
  downloaded. Options are: csv, json, pdf, rdf, rss, xls,
  xlsx, xml}

  \item{file}{The destination file where the data will be
  written}

  \item{load.data}{Load the data into R?}
}
\description{
  getSocrataData getSocrataData lists databases in the
  retrieved html from http://opendata.socrata.com/.

  getSocrataData returns a dataframe of the selected
  dataset and optionally downloads the dataset.
}
\examples{
## Not run
## Retrieve basic page
socrata.html <- search.Socrata()
data <- getListedDatasets(socrata.html)
getSocrataData(1, data, file = NULL)

## Not run
## Retrieve datasets related to airplane
socrata.html <- search.Socrata(topic = 'airplane')
data <- getListedDatasets(socrata.html)
getSocrataData(8, data, file = NULL)
}

