\name{search.Socrata}
\alias{search.Socrata}
\title{search.Socrata
search.Socrata is an interface function to search and retrieve a listing of the datasets available on http://opendata.socrata.com/.

search.Socrata returns a html page and uses the RCurl get method to retrieve the page.}
\usage{
  search.Socrata(search = NULL, topic = NULL, Limit = NULL,
    Category = NULL, page = 1, sortby = NULL,
    sortPeriod = "week")
}
\arguments{
  \item{search}{Any search term can be entered. All data
  sets that have this term will be returned}

  \item{topic}{Lists data sets that are related to the
  topic. Possible topics are: air, epa, filter, government,
  water, 2.0, 2009, air, aircraft, airplane, all+deals,
  apps, archaeology, art, asb, automatic+revocations,
  aviation, bfads, blackfriday, blog, book, books, bp,
  budget, business, calories, campaign, campaign+finance,
  cannon+land, cartridge, census,
  chronicle+of+philanthropy, church, city, community,
  congress, contacts, cooking, county, crash, dail, data,
  datos+abiertos, dc, deals, design, diet, directory,
  districts, do, doorbusters, drinking, early+bird,
  ecology, edmonton, education, empresarios, empresas,
  energy, energystar, enter+keywords, environment, epa,
  exercise, faa, faculty, fgcu, filter, finance, food,
  football, for+sale, fun, games, government, grants,
  green, hacs, health, health+care, history,
  hospital-acquired+infections, hospitals, house,
  infections, interests, internal+revenue+service,
  internet, ireland, irs, japan, lacy+cannon, library,
  lima, list, lots, management, media, medical+errors,
  milk, money, most, mousse, movie, movies,
  municipalidad+metropolitana+de+lima, music, myrtle+beach,
  national, ncopendata, network, new+york, ning,
  nonprofits, ny, octo, of, oil, oily+debris, online,
  open+data, open+government, opendata, parliament,
  payroll, personal, peru, points, politics, population,
  precipitation, project, projects, pullups, radiation,
  radnet, rain, rank, reading, recipes,
  recursos+educacion+internet, reduced+price+lunch, rugby,
  safety, salaries, salary, sales, sc, school, science,
  seattle, sediment, senate, shopping, sleet, snow, social,
  software, songs, sort, spill, state, statistics, stats,
  strike, subsistence, tar, td, teachers, tech+integration,
  technology, test, thanksgiving, title, to, todo, tools,
  top, trabajo, trabajo+mata, training, transparency,
  transportation, travel, tv, twitter, us, video,
  visitor+records, washington, waste, watchers, water,
  weathered+oil, web, web2.0, websites, weight,
  white+house, white+house+staff, whitehouse, wildlife,
  wine, work, world,}

  \item{Limit}{Limits the results to datasets of the types.
  Possible limits are: datasets, blob, charts, calendars,
  href, filters, maps, forms}

  \item{Category}{Lists all datasets within the category.
  Category limits are: Business, Fun, Personal, Education,
  Government}

  \item{page}{The page to be retrieved.}

  \item{sortby}{Datasets can be returned in a sorted order.
  Possible values are alphabetical, most relevant, most
  accessed, newest, highest rating, most comments, oldest.}

  \item{sortPeriod}{Period over which the most accessed are
  determined. Only revelant when sortby = 'most accessed'}
}
\description{
  search.Socrata search.Socrata is an interface function to
  search and retrieve a listing of the datasets available
  on http://opendata.socrata.com/.

  search.Socrata returns a html page and uses the RCurl get
  method to retrieve the page.
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

