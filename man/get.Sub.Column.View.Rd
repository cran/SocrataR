\name{get.Sub.Column.View}
\alias{get.Sub.Column.View}
\title{get.Sub.Column.View
get.Sub.Column.View returns metadata about the subcolumns of a single column on a given view.

get.Sub.Column.View returns metadata about the subcolumns of a single column on a given view.}
\usage{
  get.Sub.Column.View(viewID, columnID)
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}

  \item{columnID}{The columnID for the columns can be found
  in the first column of the \link{get.Column.View}
  output.}
}
\description{
  get.Sub.Column.View get.Sub.Column.View returns metadata
  about the subcolumns of a single column on a given view.

  get.Sub.Column.View returns metadata about the subcolumns
  of a single column on a given view.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
\dontrun{airplane2 <- get.Sub.Column.View(socrata.views[1,1], airplane1[1,1])}
\dontrun{print(airplane1)}
\dontrun{print(airplane2)}
}

