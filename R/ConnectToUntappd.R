get_default_config_path <- function() {
  return(file.path(Sys.getenv("HOME"), ".config", "untappd", "utconfig.json"))
}

#' @export
load_config_file <- function(config_path=get_default_config_path()) {
  options(
    untappd_config = jsonlite::fromJSON(config_path)
  )
}

get_user_checkins <- function(user='zachmayer86', config=getOption('untappd_config')){
  response <- httr::GET(
    config$endpoint,
    path=paste0('/v4/user/checkins/', user),
    query=list(
      client_id = config$client_id,
      client_secret = config$client_secret,
      access_token = config$access_token
    )
  )
  stop_for_status(response)
  checkins = content(response)[[3]]$checkins$items
  return(checkins)
}

parse_checkins = function(c){
  out <- pbapply::pblapply(c, function(x){
    data.table::data.table(
      rating = x$rating_score,
      cid = x$checkin_id,
      ts = x$created_at,
      uid = x$user$uid,
      bid = x$beer$bid,
      abv = x$beer$beer_abv
    )
  })
  out <- data.table::rbindlist(out)
  data.table::setkeyv(out, 'cid')
  return(out)
}

#
# https://api.untappd.com/v4/user/checkins?client_id=ID&client_secret=SEC&access_token=TOKEN
# Get token:
# #https://untappd.com/oauth/authenticate/?client_id=ID&response_type=token&redirect_url=http://moderntoolmaking.blogspot.com/
