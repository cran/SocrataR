\name{get.Single.Column.View}
\alias{get.Single.Column.View}
\title{get.Single.Column.View
get.Single.Column.View returns metadata about a single column on a given view.

get.Single.Column.View returns metadata about a single column on a given view.}
\usage{
  get.Single.Column.View(viewID, columnID)
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}

  \item{columnID}{The columnID for the columns can be found
  in the first column of the \link{get.Column.View}
  output.}
}
\description{
  get.Single.Column.View get.Single.Column.View returns
  metadata about a single column on a given view.

  get.Single.Column.View returns metadata about a single
  column on a given view.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
\dontrun{airplane2 <- get.Single.Column.View(socrata.views[1,1], airplane1[1,1])}
\dontrun{print(airplane2)}
}

