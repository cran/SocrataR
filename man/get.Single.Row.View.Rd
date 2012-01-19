\name{get.Single.Row.View}
\alias{get.Single.Row.View}
\title{get.Single.Row.View
get.Single.Row.View Retrieve the expanded representation of a particular row by ID.

get.Single.Row.View Retrieve the expanded representation of a particular row by ID. You must have read permissions on the view in order to access this resource.}
\usage{
  get.Single.Row.View(viewID, rowID)
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}

  \item{rowID}{The rowID for the view can be found in the
  first column of the \link{get.Rows.View} output.}
}
\description{
  get.Single.Row.View get.Single.Row.View Retrieve the
  expanded representation of a particular row by ID.

  get.Single.Row.View Retrieve the expanded representation
  of a particular row by ID. You must have read permissions
  on the view in order to access this resource.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Rows.View(socrata.views[1,1], row_ids_only = TRUE, meta = FALSE)}
\dontrun{airplane2 <- get.Rows.View(socrata.views[1,1], ids = data.frame(airplane1)[1, 1] , meta = FALSE)}
\dontrun{airplane_row <- get.Single.Row.View(socrata.views[1,1], data.frame(airplane1)[1, 1])}
\dontrun{print(t(data.frame(unlist(airplane2 ))))}
\dontrun{print(data.frame(airplane_row))}
}

