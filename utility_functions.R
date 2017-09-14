# Name:             utility_functions.R
# Author:           Arnob L. Alam <arnoblalam@gmail.com>
# Last Updated:     2017-09-13
#
# Utility functions that connect to the Yahoo API


library(httr)
library(xml2)

connect_to_api <- function() {
    yahoo <- oauth_endpoints("yahoo")
    app <- oauth_app(appname = "Yahoo Fantasy Sports App",
                     key = Sys.getenv("consumer_key"),
                     secret = Sys.getenv("consumer_secret"))
    yahoo_token <- oauth1.0_token(yahoo, app)
    sig <- sign_oauth1.0(app, yahoo_token$oauth_token, 
                         yahoo_token$oauth_token_secret)
}


get_players <- function(player_name,
                               game = "nfl") {
    url_ <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/game/",
                   game,"/players")
    cat(url_)
    r <- GET(url_, sig)
    r <- xml_ns_strip(content(r))
    return(r)
}
