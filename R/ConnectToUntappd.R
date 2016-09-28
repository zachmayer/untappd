get_default_config_path <- function() {
  return(file.path(Sys.getenv("HOME"), ".config", "untappd", "utconfig.json"))
}

#' @export
load_config_file <- function(config_path=get_default_config_path()) {
  options(
    untappd_config = jsonlite::fromJSON(config_path)
  )
}

parse_checkins <- function(c){
  out <- lapply(c, function(x){
    data.table::data.table(
      rating = x$rating_score,
      user_id = x$user$uid,
      beer_id = x$beer$bid,
      brewery_id = x$brewery$brewery_id,
      checkin_id = x$checkin_id,
      time = x$created_at,
      brewery_name = x$brewery$brewery_name,
      beer_name = x$beer$beer_name,
      abv = x$beer$beer_abv
    )
  })
  out <- data.table::rbindlist(out)
  data.table::setkeyv(out, 'checkin_id')
  return(out)
}

get_checkins <- function(
  type = 'user',
  id = 'zachmayer86',
  wait = 5,
  n=100,
  record_per_page=25,
  config=getOption('untappd_config')){

  calls <- ceiling(n/record_per_page)
  max_id <- NULL
  checkin_list <- list()

  pb <- txtProgressBar(min=0, max=calls, style=3, char='+')
  for(i in 1:calls){
    response <- httr::GET(
      config$endpoint,
      path=paste0('/v4/', type, '/checkins/', id),
      query=list(
        client_id = config$client_id,
        client_secret = config$client_secret,
        access_token = config$access_token,
        max_id = max_id
      )
    )
    httr::stop_for_status(response)
    content <- httr::content(response)
    checkins <- content[['response']]$checkins$items
    checkins <- parse_checkins(checkins)
    checkin_list <- c(checkin_list, list(checkins))

    setTxtProgressBar(pb, i)
    if(nrow(checkins) < record_per_page){
      break
    }else{
      max_id <- content[['response']][['pagination']][['max_id']]
    }
    Sys.sleep(wait)
  }
  res <- data.table::rbindlist(checkin_list)
  return(res)
  return(checkins)
}
