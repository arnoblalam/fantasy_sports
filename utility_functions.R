# Name:             utility_functions.R
# Author:           Arnob L. Alam <arnoblalam@gmail.com>
# Last Updated:     2017-09-13
#
# Utility functions that connect to the Yahoo API and get player stats
# You need to set the consumer_key and conumer_secret environmental variables


library(httr)
library(xml2)

#' Connect to the API
#'
#' @return sig the 
#' @export
#'
#' @examples
connect_to_api <- function() {
    yahoo <- oauth_endpoints("yahoo")
    app <- oauth_app(appname = "Yahoo Fantasy Sports App",
                     key = Sys.getenv("consumer_key"),
                     secret = Sys.getenv("consumer_secret"))
    yahoo_token <- oauth1.0_token(yahoo, app)
    sign_oauth1.0(app, yahoo_token$oauth_token, yahoo_token$oauth_token_secret)
}

# Connect to the API and save the signed token in sig 
sig <- connect_to_api()

base_url <- function() {
    return("https://fantasysports.yahooapis.com/fantasy/v2")
}

get_game_key <- function(season, game_code="nfl") {
    url_ <- paste0(base_url(), "/games?seasons=", season,"&game_codes=", game_code)
    cat(url_)
    r <- GET(url_, sig)
    xml_text(xml_find_all(xml_ns_strip(content(r)), "//game_key"))
}

#' Get a list of the top 200 players
#'
#' @param
#' @param game="nfl" The game to get data for
#'
#' @return a character vector of player keys
#' @export
#'
#' @examples
get_player_keys <- function(game = "nfl") {
    ret <- character()
    for (i in seq(1, 200, by = 25)) {
        url_ <- paste0(base_url(), "/game/",game,"/players?start=",i)
        cat(url_)
        r <- GET(url_, sig)
        r <- xml_ns_strip(content(r))
        ret <- append(ret, xml_text(xml_find_all(r, "//player_key")))
    }
    return(ret)
}

get_player_stats <- function(player_key) {
    url_ <- paste0(base_url(), "/players?player_keys=", 
                   paste0(player_key, collapse = ","), "&out=stats")
    cat(url_)
    r <- xml_strip_ns(content(GET(url_, sig)))
    player_names <- xml_text(xml_find_all(r, "//full"))
    games_played <- xml_text(xml_find_all(r, "//stat[stat_id=0]/value"))
    passing_attempts <- xml_text(xml_find_all(r, "//stat[stat_id=1]/value"))
    
}
