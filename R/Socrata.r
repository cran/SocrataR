#####################################################################################################################################
###
###                                          opendata.socrata interface for R
###
###
###                                           Browse and Download data sets from Socrata
###
###                                           Mischa Vreeburg
###
###
#####################################################################################################################################

#' @name  search.Socrata
#' @title search.Socrata
#' search.Socrata is an interface function to search and retrieve a listing of the datasets available on http://opendata.socrata.com/.
#'
#' search.Socrata returns a html page and uses the RCurl get method to retrieve the page.
#'
#' @param search Any search term can be entered. All data sets that have this term will be returned
#' @param topic Lists data sets that are related to the topic. Possible topics are: air, epa, filter, government, water, 2.0, 2009, air, aircraft, airplane, all+deals, apps, archaeology, art, asb, automatic+revocations, aviation, bfads, blackfriday, blog, book, books, bp, budget, business, calories, campaign, campaign+finance, cannon+land, cartridge, census, chronicle+of+philanthropy, church, city, community, congress, contacts, cooking, county, crash, dail, data, datos+abiertos, dc, deals, design, diet, directory, districts, do, doorbusters, drinking, early+bird, ecology, edmonton, education, empresarios, empresas, energy, energystar, enter+keywords, environment, epa, exercise, faa, faculty, fgcu, filter, finance, food, football, for+sale, fun, games, government, grants, green, hacs, health, health+care, history, hospital-acquired+infections, hospitals, house, infections, interests, internal+revenue+service, internet, ireland, irs, japan, lacy+cannon, library, lima, list, lots, management, media, medical+errors, milk, money, most, mousse, movie, movies, municipalidad+metropolitana+de+lima, music, myrtle+beach, national, ncopendata, network, new+york, ning, nonprofits, ny, octo, of, oil, oily+debris, online, open+data, open+government, opendata, parliament, payroll, personal, peru, points, politics, population, precipitation, project, projects, pullups, radiation, radnet, rain, rank, reading, recipes, recursos+educacion+internet, reduced+price+lunch, rugby, safety, salaries, salary, sales, sc, school, science, seattle, sediment, senate, shopping, sleet, snow, social, software, songs, sort, spill, state, statistics, stats, strike, subsistence, tar, td, teachers, tech+integration, technology, test, thanksgiving, title, to, todo, tools, top, trabajo, trabajo+mata, training, transparency, transportation, travel, tv, twitter, us, video, visitor+records, washington, waste, watchers, water, weathered+oil, web, web2.0, websites, weight, white+house, white+house+staff, whitehouse, wildlife, wine, work, world,
#' @param Limit Limits the results to datasets of the types. Possible limits are: datasets, blob, charts, calendars, href, filters, maps, forms
#' @param Category Lists all datasets within the category. Category limits are: Business, Fun, Personal, Education, Government
#' @param page The page to be retrieved.
#' @param sortby Datasets can be returned in a sorted order. Possible values are alphabetical, most relevant, most accessed, newest, highest rating, most comments, oldest.
#' @param sortPeriod Period over which the most accessed are determined. Only revelant when sortby = 'most accessed'
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' getListedDatasets(socrata.html)
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' socrata.html <- search.Socrata(topic= 'airplane')
#' getListedDatasets(socrata.html)
#'
#'
#' @rdname search.Socrata
#' @export search.Socrata

search.Socrata <- function(search = NULL, topic = NULL, Limit = NULL, Category = NULL, page = 1, sortby = NULL, sortPeriod = 'week'){
  require('RCurl')
  require('XML')
  ## setting curl options
  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie ,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = TRUE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE)

  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  ### Make URL
  baseSocrataUrl <- 'http://opendata.socrata.com/browse?'
  if(!is.null(Limit)){
    Limit <- match.arg( Limit, c('datasets', 'blob', 'charts', 'calendars', 'href', 'filters', 'maps', 'forms'))
  }
  if(!is.null(sortby)){
    sortby <- match.arg( sortby, c('alphabetical', 'most relevant', 'most accessed', 'newest', 'highest rating', 'most comments', 'oldest'))
  }
  if(!is.null(sortPeriod)){
    sortPeriod <- match.arg(sortPeriod, c('week', 'year', 'month'))
  }
  if(!is.null(Category)){
    Category <- match.arg( Category, c('Business', 'Fun', 'Personal', 'Education', 'Government'))
  }

  ## Tag
  if(is.null(topic)){
    Tag <- NULL
  } else {
    Tag <- paste('&tag=', topic, sep = '')
  }
  ## Category
  if(is.null(Category)){
    Category <- NULL
  } else {
    Category <- paste('&category=', Category, sep = '')
  }
  ## Limit
  if(is.null(Limit)){
    Limit <- NULL
  } else {
    Limit <- paste('&limitTo=', Limit, sep = '')
  }
  ## search
  if(is.null(search)){
    search <- NULL
  } else {
    search <- paste('&q=', search, sep = '')
  }
  ## page
  page <- paste('&page=', page, sep = '')
  ## sortby
  if(is.null(sortby)){
    sortby <- NULL
  } else {
    if(sortby == 'alphabetical'){
      sortby <- 'alpha'
    }
    if(sortby == 'most relevant'){
      sortby <- 'relevance'
    }
    if(sortby == 'most accessed'){
      sortby <- 'most_accessed'
    }
    if(sortby == 'newest'){
      sortby <- 'newest'
    }
    if(sortby == 'oldest'){
      sortby <- 'oldest'
    }
    if(sortby == 'highest rating'){
      sortby <- 'rating'
    }
    if(sortby == 'most comments'){
      sortby <- 'comments'
    }
    sortby <- paste('&sortby=', sortby, sep = '')
  }
  ## sortPeriod
  if(is.null(sortby)){
    sortPeriod <- NULL
  } else {
    sortPeriod <- paste('&sortPeriod=', sortPeriod, sep = '')
  }

  ### Retrieving html
  SocrataUrl <- paste( baseSocrataUrl, page, Tag, Category, Limit, search, sortby, sortPeriod, sep = '')

  SocrataHtml <- getURL(SocrataUrl, curl = curl, cainfo = cainfo)
  assign('search.Socrata.Call', SocrataUrl, envir=.GlobalEnv)
  return(SocrataHtml)
}

### get links from search page
getLinks = function() {
  links = character()
  list(a = function(node, ...) {
    links <<- c(links, xmlGetAttr(node, "href"))
    node

  },
  links = function()links)
}

#' @name  getListedDatasets
#' @title getListedDatasets
#' getListedDatasets lists databases in the retrieved html from http://opendata.socrata.com/.
#'
#' getListedDatasets returns a dataframe with the datasets found. It returns max 10 per page
#'
#' @param SocrataHtml The html source retrieved by the \link{search.Socrata} function
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' getListedDatasets(socrata.html)
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' socrata.html <- search.Socrata(topic= 'airplane')
#' getListedDatasets(socrata.html)
#'
#'
#' @rdname getListedDatasets
#' @export getListedDatasets

### get listing of data sets
getListedDatasets <- function(SocrataHtml){

  h1 = getLinks()
  SocrataParsedLinks <- htmlTreeParse(SocrataHtml, useInternalNodes = TRUE, handlers = h1)

  SocrataLinks <- SocrataParsedLinks$links()

  # SocrataTags <- SocrataLinks[grep('tags', SocrataLinks)]
  # SocrataCategories <- SocrataLinks[grep('category', SocrataLinks)]
  # SocrataLimits <- SocrataLinks[grep('limit', SocrataLinks)]
  SocrataPages <- SocrataLinks[grep('page', SocrataLinks)]

  ## cleanup links:

  SocrataLinks <- SocrataLinks[-grep('tags', SocrataLinks)]
  SocrataLinks <- SocrataLinks[-grep('category', SocrataLinks)]
  SocrataLinks <- SocrataLinks[-grep('limit', SocrataLinks)]
  SocrataLinks <- SocrataLinks[-grep('page', SocrataLinks)]
  SocrataLinks <- SocrataLinks[-grep('http', SocrataLinks)]
  SocrataLinks <- SocrataLinks[-grep('#', SocrataLinks)]
  SocrataLinks <- SocrataLinks[-grep('nominate', SocrataLinks)]
  SocrataLinks <- unique(SocrataLinks[nchar(SocrataLinks) != 1])

  ### listing datasets
  datasets <- data.frame(
    t(
      structure(
        unlist(
          strsplit(
            substring(SocrataLinks, first = 2),
                   split= '/')
        ),
        dim = c(3, length(SocrataLinks)))
    ),
    stringsAsFactors = FALSE)
  names(datasets) <- c('Category', 'Dataset Name', 'ID')
  datasets$links <- SocrataLinks

  ## xmlValue a href
  ## div class="description"

  SocrataHtml2 <- gsub('\r','', gsub('\t','', SocrataHtml))
  SocrataHtml2 <- strsplit(SocrataHtml2, split = '\n')[[1]]


  ## div class="description"
  description <- SocrataHtml2[grep('class="description"', SocrataHtml2)]

  description <- substring(description,
                           first = matrix(unlist(lapply(strsplit(description, split=''),
                                                        FUN = function(x){grep('>', x)}))+1, byrow=T, ncol = 2)[,1],
                           last = matrix(unlist(lapply(strsplit(description, split=''),
                                                       FUN = function(x){grep('<', x)}))-1, byrow=T, ncol = 2)[,2]
  )

  ## div class="name"
  hrefNames <- SocrataHtml2[grep('class="name"', SocrataHtml2)+1]

  hrefNames <- substring(hrefNames,
                         first = matrix(unlist(lapply(strsplit(hrefNames, split=''),
                                                      FUN = function(x){grep('>', x)}))+1, byrow=T, ncol = 2)[,1],
                         last = matrix(unlist(lapply(strsplit(hrefNames, split=''),
                                                     FUN = function(x){grep('<', x)}))-1, byrow=T, ncol = 1)[,1]
  )

  datasets$'Dataset Name' <- gsub('Ã©', 'é', gsub('&amp;', '&', gsub('Ã"','è', curlUnescape( hrefNames))))
  datasets$description <- gsub('Ã©', 'é', gsub('Ã"','è', curlUnescape( description)))

  names(datasets)
  datasets <- datasets[c(2,5,1,3,4)]

  x = length(datasets[,1])
  if(x < 10){
    y = x
  } else {
    y = as.numeric(substring(SocrataPages[length(SocrataPages)],
                             first = grep('=',
                                          strsplit( SocrataPages[length(SocrataPages)],
                                                    split = '')[[1]]
                             )+1
                            )
                  )*10
  }

  message('listing ', x, ' of ', y, ' datasets')

  return(datasets)
}

## download data set

#' @name  getSocrataData
#' @title getSocrataData
#' getSocrataData lists databases in the retrieved html from http://opendata.socrata.com/.
#'
#' getSocrataData returns a dataframe of the selected dataset and optionally downloads the dataset.
#'
#' @param rownr The row in the datasetlist that has the dataset you want to load
#' @param datasetList The dataframe retrieved by the \link{getListedDatasets} function
#' @param filetype The file type of the data that shall be downloaded. Options are: csv, json, pdf, rdf, rss, xls, xlsx, xml
#' @param file The destination file where the data will be written
#' @param load.data Load the data into R?
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' data <- getListedDatasets(socrata.html)
#' getSocrataData(1, data, file = NULL)
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' socrata.html <- search.Socrata(topic= 'airplane')
#' data <- getListedDatasets(socrata.html)
#' getSocrataData(8, data, file = NULL)
#'
#' @rdname getSocrataData
#' @export getSocrataData

getSocrataData <- function(rownr, datasetList, filetype = 'csv', file = paste(datasetList[rownr, 4],'.', filetype, sep=''), load.data = TRUE){

  filetype <- match.arg(filetype, c('csv', 'json', 'pdf', 'rdf', 'rss', 'xls', 'xlsx', 'xml'))
  ## build URL
  baseURL <- 'http://opendata.socrata.com/views/'
  dataURL <- paste(baseURL, datasetList[rownr, 4], '/rows.', filetype, '?accessType=DOWNLOAD', sep = '')

  ## setting Curl options:
  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie ,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = TRUE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE)

  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  data = NULL
  if(!is.null(file)){
    if(!is.na(file)){
      if(load.data){
        download.file(dataURL, destfile = file.path(tempdir(), paste(datasetList[rownr, 4],'.', filetype, sep='')))
        data <- read.csv(file.path(tempdir(), paste(datasetList[rownr, 4],'.', filetype, sep='')))
      }
    }
  }

  if(!is.null(file)){
    if(!is.na(file)){
      download.file(dataURL, destfile = file)
      if(load.data){
        data <- read.csv(file)
      }
    }
  }
  return(data)
}

#' @name  nextSocrataPage
#' @title nextSocrataPage
#' nextSocrataPage retrieves the next page from the current search.
#'
#' nextSocrataPage retrieves the html source page for the next page.
#'
#' @param search.Socrata.Call The URL call of the last html page retrieval.
#' @param page The page number to retrieve. If not entered it will use next page.
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' data <- getListedDatasets(socrata.html)
#' socrata1.html <- nextSocrataPage(search.Socrata.Call)
#' data1 <- getListedDatasets(socrata1.html)
#' print(data)
#' print(data1)
#'
#' @rdname nextSocrataPage
#' @export nextSocrataPage

nextSocrataPage <- function(search.Socrata.Call, page = NULL){
  require('RCurl')
  require('XML')
  ## set Curl options
  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie ,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = TRUE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE)

  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  ## make new URl
  search.Socrata.Call <- strsplit(search.Socrata.Call, split = '&')[[1]]
  pageNo <- gsub('page=', '', search.Socrata.Call[grep('page', search.Socrata.Call)])
  if(is.null(page)){
    page <- paste('page=', as.numeric(pageNo) + 1, sep = '')
  } else {
    page <- paste('page=', page, sep = '')
  }
  search.Socrata.Call[grep('page', search.Socrata.Call)] <- page
  search.Socrata.Call <- paste( search.Socrata.Call, collapse = '&')

  SocrataHtml <- getURL(search.Socrata.Call, curl = curl, cainfo = cainfo)
  assign('search.Socrata.Call', search.Socrata.Call, envir=.GlobalEnv)
  return(SocrataHtml)
}