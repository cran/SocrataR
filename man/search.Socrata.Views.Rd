\name{search.Socrata.Views}
\alias{search.Socrata.Views}
\title{search.Socrata.Views
search.Socrata.Views is an interface function to search and retrieve a listing of the datasets available on https://opendata.socrata.com/api/.

search.Socrata.Views returns either an xml page with type = xml or a dataframe with type = json. and uses the RCurl get method to retrieve the page.}
\usage{
  search.Socrata.Views(search = NULL, topic = NULL,
    name = NULL, tags = NULL, category = NULL,
    count = FALSE, limit = 10, page = 1, type = "json")
}
\arguments{
  \item{search}{Any search term can be entered. All data
  sets that have this term in their metadata or contend
  will be returned.}

  \item{topic}{Lists views containing this text in their
  description}

  \item{name}{Lists views containing this text in their
  name}

  \item{tags}{Lists data sets that are related to the tags
  Possible tags are: air, epa, filter, government, water,
  2.0, 2009, air, aircraft, airplane, all+deals, apps,
  archaeology, art, asb, automatic+revocations, aviation,
  bfads, blackfriday, blog, book, books, bp, budget,
  business, calories, campaign, campaign+finance,
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

  \item{category}{Lists all datasets within the category.
  Category limits are: Business, Fun, Personal, Education,
  Government}

  \item{count}{Boolean. If count is true then it returns
  the total number of rows, and ignores the limit
  parameter}

  \item{limit}{Limits the total number of results to
  return, max 200.}

  \item{page}{The page to be retrieved.}

  \item{type}{The type of response format. Supports json
  and xml.}
}
\description{
  search.Socrata.Views search.Socrata.Views is an interface
  function to search and retrieve a listing of the datasets
  available on https://opendata.socrata.com/api/.

  search.Socrata.Views returns either an xml page with type
  = xml or a dataframe with type = json. and uses the RCurl
  get method to retrieve the page.
}
\examples{
## Not run
## Retrieve datasets related to airplane
\dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
}

