\name{get.Metadata.View}
\alias{get.Metadata.View}
\title{get.Metadata.View
get.Metadata.View returns the metadata associated with the view.

get.Metadata.View returns the metadata associated with the view.}
\usage{
  get.Metadata.View(viewID)
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}
}
\description{
  get.Metadata.View get.Metadata.View returns the metadata
  associated with the view.

  get.Metadata.View returns the metadata associated with
  the view.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Metadata.View(socrata.views[1,1])}
\dontrun{print(airplane1)}
}

