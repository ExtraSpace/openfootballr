#' Create Header
#'
#' @param token A character string with api token created in openfootball
#'
#' @return A character vector with text required for header
#' @export
#'
#' @examples
#' \dontrun{
#' hdr <- header(token = "XXX")
#' }
#'
header <- function(token = TOKEN) {
	c('X-Auth-Token', token)
}


#' Make a url request using the header
#'
#' @param url A character of api url to make request
#' @param header The header to use to request the url
#' @importFrom httr add_headers GET
#'
#' @return A response object
#' @export
#'
make_request <- function(url, header) {
	httr::GET(
		url,
		httr::add_headers(header),
		httr::add_headers('X-Response-Control', 'minified')
	)
}



#' Get Season Data
#'
#' @param year An integer for season
#' @param header A header object
#' @importFrom httr content
#' @importFrom purrr map_dbl map_chr map_int
#' @importFrom tibble tibble
#' @return A tibble with season related data along with attributes related to the competion and players links
#' @export
#'
#' @examples
#' TOKEN <- 'XXX'
#' s_2017 <- get_season(2017, header(TOKEN))
#'
get_season <- function(year, header) {
	base_url <- 'api.football-data.org/'
	url <- paste0(base_url, 'v1/competitions/?season=', year)
	req <- make_request(url, header)
	dta <- httr::content(req)
	links <- list(
		id = purrr::map_dbl(dta, 'id'),
		self = purrr::map_chr(dta, function(x) x[[1]][['self']][['href']]),
		teams = purrr::map_chr(dta, function(x) x[[1]][['teams']][['href']]),
		fixtures = purrr::map_chr(dta, function(x) x[[1]][['fixtures']][['href']]),
		leagueTable = purrr::map_chr(dta, function(x) x[[1]][['leagueTable']][['href']])
	)
	properties <- tibble::tibble(
		id = purrr::map_dbl(dta, 'id'),
		caption = purrr::map_chr(dta, 'caption'),
		league = purrr::map_chr(dta, 'league'),
		year  = purrr::map_chr(dta, 'year'),
		currentMatchday = purrr::map_int(dta, 'currentMatchday'),
		numberOfMatchdays = purrr::map_int(dta, 'numberOfMatchdays'),
		numberOfTeams = purrr::map_int(dta, 'numberOfTeams'),
		numberOfGames = purrr::map_int(dta, 'numberOfGames'),
		lastUpdated = purrr::map_chr(dta, 'lastUpdated')
	)
	attr(properties, 'links') <- links
	return(properties)
}


#' Get teams on a competition
#'
#' @param competition_url An url for the competion
#' @param header A header for the request
#' @importFrom httr content
#' @importFrom purrr map_chr map_df
#' @importFrom tibble add_column
#' @importFrom stringr str_sub
#' @return A tibble with properties of competition along with attributes containing related links
#' @export
#'
#' @examples
#' TOKEN <- "XXX"
#' season_2017 <- get_season(2017, header(TOKEN))
#' teams_competition <- purrr::map_df(
#'     `names<-`(attr(season_2017, 'links')[['teams']], season_2017$id),
#'     ~get_teams(.x, header(TOKEN)),
#'     .id = "competition")
get_teams <- function(competition_url, header) {
	req <- make_request(competition_url, header)
	dta <- httr::content(req)
	teams <- tibble::tibble(
		name = purrr::map_chr(dta$teams, 'name', .null = NA_character_),
		code = purrr::map_chr(dta$teams, 'code', .null = NA_character_),
		shortName = purrr::map_chr(dta$teams, 'shortName', .null = NA_character_),
		squadMarketValue = purrr::map_chr(dta$teams, 'squadMarketValue', .null = NA_character_),
		crestUrl = purrr::map_chr(dta$teams, 'crestUrl', .null = NA_character_),
		self = purrr::map_chr(dta$teams, ~.x[['_links']]$self$href),
		fixtures = purrr::map_chr(dta$teams, ~.x[['_links']]$fixtures$href),
		players = purrr::map_chr(dta$teams, ~.x[['_links']]$players$href)
	)
	teams <- tibble::add_column(teams, id = as.integer(stringr::str_sub(teams$self, -4)), .before = 1)
	return(teams)
}