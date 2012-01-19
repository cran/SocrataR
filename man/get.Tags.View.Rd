\name{get.Tags.View}
\alias{get.Tags.View}
\title{get.Tags.View
get.Tags.View Allows you to retrieve tags for a view .

get.Tags.View Allows you to retrieve tags for a view .}
\usage{
  get.Tags.View(viewID, userTags = FALSE)
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}

  \item{userTags}{Retrieve the user tags associated with
  the view .}
}
\description{
  get.Tags.View get.Tags.View Allows you to retrieve tags
  for a view .

  get.Tags.View Allows you to retrieve tags for a view .
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Tags.View(socrata.views[1, 1])}
\dontrun{airplane_tags <- get.Tags.View(socrata.views[1, 1])}
\dontrun{print(airplane_tags)}
}

