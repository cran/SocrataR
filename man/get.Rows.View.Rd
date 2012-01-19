\name{get.Rows.View}
\alias{get.Rows.View}
\title{get.Rows.View
get.Rows.View Retrieve multiple rows from a view.

get.Rows.View  Retrieve multiple rows from a view as nested arrays instead of in the expanded form. By default this will return all the rows in the view, but you can limit the number of rows returned with max_rows or request only specific rows using ids. You can also request only row IDs by passing row_ids_only. You must have read permissions on the view in order to access this resource.}
\usage{
  get.Rows.View(viewID, row_ids_only = FALSE,
    max_rows = NULL, include_ids_after = NULL,
    search = NULL, start = NULL, length = NULL, ids = NULL,
    meta = TRUE, as_hashes = FALSE,
    exclude_system_fields = FALSE, unwrapped = FALSE,
    most_recent = FALSE, access_type = NULL,
    filetype = "json")
}
\arguments{
  \item{viewID}{The ViewId for the view can be found in the
  first column of the \link{search.Socrata.Views} output.}

  \item{row_ids_only}{If true, the service will only return
  row IDs.}

  \item{max_rows}{Limit the number of rows returned.}

  \item{start}{Start retrieving rows from row number...}

  \item{length}{The number of rows to be retrieved.}

  \item{ids}{The row ids to be retrieved. Can be a vector
  of ids.}

  \item{include_ids_after}{Include this number of rows,
  after which only row IDs are returned.}

  \item{search}{Run a full text search on the view and only
  return rows/ids that match.}

  \item{meta}{If set to 'true', will write the view object.
  Only valid if rendering JSON. Default is true.}

  \item{as_hashes}{If set to 'true', write fields in hash
  format. Otherwise, write fields in array. Only valid if
  rendering JSON. Default to false (array).}

  \item{exclude_system_fields}{If set to 'true', do not
  return system fields like row id. Only valid if rendering
  JSON. Default to false.}

  \item{unwrapped}{If set to 'true', return just array or
  rows with no outer object. Only valid if rendering JSON.
  Default to false.}

  \item{most_recent}{If set to 'true', return only the most
  recent rows added to the dataset. Only valid if rendering
  RSS. Default to true.}

  \item{access_type}{Valid values for the channel are
  PRINT, EMAIL, API, RSS, WIDGET, DOWNLOAD, WEBSITE.}

  \item{filetype}{Can be 'csv', 'json', 'pdf', 'rdf',
  'rss', 'xls', 'xlsx', 'xml'.}
}
\description{
  get.Rows.View get.Rows.View Retrieve multiple rows from a
  view.

  get.Rows.View Retrieve multiple rows from a view as
  nested arrays instead of in the expanded form. By default
  this will return all the rows in the view, but you can
  limit the number of rows returned with max_rows or
  request only specific rows using ids. You can also
  request only row IDs by passing row_ids_only. You must
  have read permissions on the view in order to access this
  resource.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
\dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
\dontrun{airplane_rows <- get.Rows.View(socrata.views[1,1], airplane1[1,1])}
\dontrun{print(airplane_rows)}
}

