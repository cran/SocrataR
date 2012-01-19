\name{get.Column.View}
\alias{get.Column.View}
\title{get.Column.View
get.Column.View returns metadata about all of the columns on a given view.

get.Column.View returns metadata about all of the columns on a given view.}
\usage{
  get.Column.View(viewID)
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}
}
\description{
  get.Column.View get.Column.View returns metadata about
  all of the columns on a given view.

  get.Column.View returns metadata about all of the columns
  on a given view.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
\dontrun{print(airplane1)}
}

