devtools::load_all()
library(epitools)
library(rstanarm)
library(arm)
library(lme4)
library(fastmatch)
library(Matrix)
library(irlba)
library(Rtsne)
library(data.table)
library(lubridate)
library(httr)
options(mc.cores = parallel::detectCores())

#Load my personal ratings data
zach <- get_checkins('user', 'zachmayer86', n=25, wait=0)
zach[,user_name := NULL]
devtools::use_data(zach, overwrite=TRUE)

if(FALSE){

  #Takes about an hour, to make sure we don't run over the API limits
  tt_room <- get_checkins('venue', '290766', n=2500, wait=36)
  unique_users <- sort(unique(tt_room$user_name))
  devtools::use_data(tt_room, overwrite=TRUE)

  #Make data sets
  new_users <- as.list(rep(list(NULL), length(unique_users)))
  names(new_users) <- unique_users

  #Load last 25 checkins for each user
  save(new_users, file='~/new_users.RDS')
  #load('~/new_users.RDS')
  for(x in unique_users){
    i <- which(unique_users == x)
    print(paste('User', i, 'of', length(unique_users)))
    if(is.null(new_users[[x]])){
      #Sys.sleep(36)
      new_users[[x]] <- get_checkins('user', x, n=25, wait=0, httr_timeout=240000)
    }
  }

  new_users_full <- rbindlist(new_users)
  devtools::use_data(new_users_full, overwrite=TRUE)
}

#START HERE!!
data('tt_room')
data('new_users_full')
tt_room[,at_tt := 1]
new_users_full[,at_tt := 0]
dat <- rbind(tt_room, new_users_full, use.names=T, fill=T)

#Parse date
dat[,time := as.POSIXct(strptime(time, '%a, %d %b %Y %H:%M:%S'))]

#Exclude 0's (these are users who forogt to rate)
dat <- dat[rating > 0,]

#Exclude dupes (keep most recent)
data.table::setkeyv(dat, c('user_id', 'checkin_id'))
dat[,dup := duplicated(checkin_id, fromLast=TRUE), by='user_id']

#Lookit the data
ME <- zach$user_id[1]
mybeers <- sort(unique(zach$beer_id))
#dat[(beer_id %in% zach$beer_id) & (user_id != ME) & rating > 4,]

#Unique beers
beer <- unique(dat[rating > 0, list(
  rating_sum = sum(rating),
  n = .N,
  last_seen = max(time),
  at_tt = max(at_tt)
  ), by=c('beer_id', 'brewery_name', 'beer_name', 'abv')])
beer[,rating_mean := rating_sum / n]
M <- beer[,max(rating_mean)]
beer[,rating_bin := (rating_mean) / M]
beer[,rating_pois := pois.exact(rating_sum, n, conf.level=.95)$lower]
beer[,c('rating_sum', 'rating_bin') := NULL]
data.table::setorder(beer, -rating_pois)
beer[,see_recently := last_seen >= as.POSIXct(Sys.time() - 3600*24*7)]
beer[see_recently==T & at_tt == 1,]

#TSNE users
user_map <- dat[,sort(unique(user_id))]
beer_map <- dat[,sort(unique(beer_id))]
ME_map <- which(user_map == ME)
tt_mat <- dat[,list(
  u = fmatch(user_id, user_map),
  b = fmatch(beer_id, beer_map),
  rating,
  good = 0L
  )]
tt_mat[rating < 4, good := -1]
tt_mat[rating > 4, good := 1]
tt_mat <- sparseMatrix(
  i=tt_mat$u,
  j=tt_mat$b,
  x=tt_mat$good)
tt_dist <- as.dist(tcrossprod(tt_mat))
tt_mat <- as.matrix(tt_mat)
tt_pca <- prcomp(tt_mat, retx=TRUE, center=FALSE)$x[,1:50]
tt_tsne <- Rtsne(tt_dist, dims=2, is_distance=T, verbose=TRUE)
plot(tt_tsne$Y)
points(tt_tsne$Y[ME_map,,drop=F], col='red')

#Cosine beers
row_wise_norm <- function(m) {
  m <- Matrix(m)
  d <- sqrt(rowSums(m^2))
  d[d == 0] <- 1
  d <- Diagonal(x=1/d)
  t(crossprod(m, d))
}
user_map <- dat[,sort(unique(user_id))]
beer_map <- dat[,sort(unique(beer_id))]
ME_map <- which(user_map == ME)
mybeers_map <- which(beer_map %in% mybeers)
mybeers_ratings <- zach[beer_id %in% beer_map,rating]
not_mybeers_map <- which(! beer_map %in% mybeers)
tt_mat <- dat[,list(
  u = fmatch(user_id, user_map),
  b = fmatch(beer_id, beer_map),
  rating,
  good = 0.0
)]
tt_mat[rating < 4, good := -.1]
tt_mat[rating > 4, good := 1]
tt_mat <- sparseMatrix(
  i=tt_mat$b,
  j=tt_mat$u,
  x=tt_mat$good)
tt_mat <- row_wise_norm(tt_mat)
tt_sim_beer <- tcrossprod(tt_mat)
diag(tt_sim_beer) <- 0 #Don't self-reccomend
tt_sim_beer <- drop0(tt_sim_beer)
summary(tt_sim_beer@x)
mysims <- tt_sim_beer[mybeers_map,,drop=F] #Keep the beers I've had
mysims <- summary(mysims)
mysims_dat <- data.table(b=mybeers_map[mysims$i], rec=mysims$j, x=mysims$x)
myratings <- data.table(b=mybeers_map, w=mybeers_ratings / max(mybeers_ratings))
mysims_dat <- merge(mysims_dat, myratings, by='b', all.x=TRUE)
mysims_dat[,beer_id := beer_map[rec]]
setkeyv(mysims_dat, 'beer_id')
mysims_dat <- mysims_dat[,list(x = sum(x * w) / sum(w)), by='beer_id']
mysims_dat <- merge(mysims_dat, beer, by='beer_id', all.x=T)
setorder(mysims_dat, -x)
head(mysims_dat[!beer_id %in% mybeers & see_recently == T & at_tt == 1,], 10)

#TSNE beers
keep <- beer[n>1, sort(unique(beer_id))]
tt_mat <- dat[beer_id %in% keep,list(
  u = fmatch(user_id, user_map),
  b = fmatch(beer_id, beer_map),
  rating
)]
tt_mat <- sparseMatrix(
  i=tt_mat$b,
  j=tt_mat$u,
  x=tt_mat$rating - median(tt_mat$rating)
)
#tt_mat <- row_wise_norm(tt_mat)
tt_mat_pca <- prcomp(as.matrix(tt_mat), retx=T, center=FALSE, scale=FALSE)$x[,1:50]
set.seed(42)
tt_tsne <- Rtsne(
  tt_mat_pca, dims=2,
  check_duplicates=F,
  pca=F,
  theta=0.5,
  verbose=TRUE)
plot(tt_tsne$Y)
points(tt_tsne$Y[mybeers_map,,drop=F], col='red')
text(tt_tsne$Y[mybeers_map,,drop=F], col='red', labels=zach[beer_id %in% beer_map,beer_name])
tsne_sim <- 1 / (as.matrix(dist(tt_tsne$Y)) + 1)
tsne_sim <- scale(tsne_sim, center=T, scale=T)
summary(as.numeric(tsne_sim))
tsne_sim <- as(tsne_sim, 'dgCMatrix')

mysims <- tsne_sim[mybeers_map,,drop=F] #Keep the beers I've had
mysims <- summary(mysims)
mysims_dat <- data.table(b=mybeers_map[mysims$i], rec=mysims$j, x=mysims$x)
myratings <- data.table(b=mybeers_map, w=mybeers_ratings / max(mybeers_ratings))
mysims_dat <- merge(mysims_dat, myratings, by='b', all.x=TRUE)
mysims_dat[,beer_id := beer_map[rec]]
setkeyv(mysims_dat, 'beer_id')
mysims_dat <- mysims_dat[,list(x = max(x * w)), by='beer_id']
mysims_dat <- merge(mysims_dat, beer, by='beer_id', all.x=T)
setorder(mysims_dat, -x)
head(mysims_dat[!beer_id %in% mybeers & see_recently == T & at_tt == 1,], 10)




#Exclude me from the data
tt_graph <- dat[!user_id %in% ME, list(user_id, beer_id, rating, checkin_id)]
tt_graph <- tt_graph[rating > 0,]
tt_subgraph <- tt_graph[beer_id %in% mybeers,]