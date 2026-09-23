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
library(text2vec)
library(stringi)
library(matrixStats)
options(mc.cores = parallel::detectCores())

#Load my personal ratings data
zach <- get_checkins('user', 'zachmayer86', n=25, wait=0)
jon <- get_checkins('user', 'raven31', n=25, wait=0)
ben <- get_checkins('user', 'TraderAllenPoe', n=25, wait=0)
devtools::use_data(zach, overwrite=TRUE)
devtools::use_data(jon, overwrite=TRUE)
devtools::use_data(ben, overwrite=TRUE)

if(FALSE){

  #Takes about an hour, to make sure we don't run over the API limits
  data('tt_room')
  tt_room2 <- get_checkins('venue', '290766', n=250, wait=36)
  tt_room <- rbind(tt_room, tt_room2, fill=T, use.names=T)
  tt_room <- tt_room[!duplicated(checkin_id),]
  devtools::use_data(tt_room, overwrite=TRUE)

  #Make data sets
  unique_users <- sort(unique(tt_room$user_name))
  new_users <- as.list(rep(list(NULL), length(unique_users)))
  names(new_users) <- unique_users

  #Load last 25 checkins for each user
  save(new_users, file='~/new_users.RDS')
  #load('~/new_users.RDS')
  #Sys.sleep(600)
  for(x in unique_users){
    i <- which(unique_users == x)
    print(paste('User', i, 'of', length(unique_users)))
    if(is.null(new_users[[x]])){
      Sys.sleep(36)
      new_users[[x]] <- get_checkins('user', x, n=25, wait=0, httr_timeout=600)
    }
  }
  new_users_full <- rbindlist(new_users)
  devtools::use_data(new_users_full, overwrite=TRUE)
}

#START HERE!!
data('tt_room')
data('new_users_full')
data('zach')
data('jon')
data('ben')

#START HERE!!
tt_room[,at_tt := 1]
new_users_full[,at_tt := 0]
dat <- rbind(tt_room, new_users_full, use.names=T, fill=T)
dat <- rbind(dat, zach, use.names=T, fill=T)
dat <- rbind(dat, jon, use.names=T, fill=T)
dat <- rbind(dat, ben, use.names=T, fill=T)
dat[user_name == 'zachmayer86',]

#CHOOSE USER
mydata <- jon

#Parse date
dat[,time := as.POSIXct(strptime(time, '%a, %d %b %Y %H:%M:%S'))]

#Clean strings
clean <- function(x){
  out <- stri_replace_all_regex(x, '[\\p{C}|\\p{S}|\\p{Z}]+', ' ')
  out <- stri_trim_both(x)
  return(out)
}
dat[,brewery_name := clean(brewery_name)]
dat[,beer_name := clean(beer_name)]

#Exclude 0's (these are users who forgot to rate)
dat <- dat[rating > 0,]

#Exclude dupes checkins (keep most recent)
data.table::setkeyv(dat, c('user_id', 'checkin_id'))
dat[,dup := duplicated(checkin_id, fromLast=TRUE)]
dat <- dat[dup != TRUE,]
dat[,dup := NULL]
data.table::setkeyv(dat, c('user_id', 'checkin_id'))

#Exclude dupes ratings(keep most recent)
dat[,dup := duplicated(stri_paste(user_id, beer_id), fromLast=TRUE)]
dat <- dat[dup != TRUE,]
dat[,dup := NULL]

#Re-key
dat[,user := as.integer(factor(user_id))]
dat[,beer := as.integer(factor(beer_id))]

#Normalize ratings
dat[,rating := rating - 4]
dat[rating < 0, rating := rating / 4]
summary(dat$rating)

#Unique beers
beers <- unique(dat[rating > 0, list(
  rating = mean(rating),
  n = .N,
  zach_drank = as.integer(any(user_id == mydata$user_id[1])),
  last_seen = max(time[at_tt == 1]),
  at_tt = max(at_tt, na.rm=T)
), by=c('beer', 'beer_id', 'brewery_name', 'beer_name', 'abv')])
data.table::setorder(beers, -rating)
beers[,see_recently := as.integer(last_seen >= as.POSIXct(Sys.time() - 3600*24*7))]
beers[see_recently==1 & at_tt == 1,]

#Beers I like
GOOD_BEERS <- dat[rating>0 & user_id == mydata$user_id[1], beer]
ME <- dat[user_id == mydata$user_id[1], user[1]]

#User-beer matrix
mat <- sparseMatrix(
  i = dat$user,
  j = dat$beer,
  x = dat$rating
)
summary(mat@x)

#Reduce the matrix
mod <- irlba(mat, nu=50, nv=50, verbose=TRUE)

#Cosine sim recs on users
#sim <- sim2(mod$u[ME,,drop=F], mod$u, method='cosine', norm='none')[1,]
sim <- sim2(mat[ME,,drop=F], mat, method='cosine', norm='none')[1,]
weights <- data.table(
  user = 1:length(sim),
  weight = sim
)
user_weighted <- merge(dat, weights, by='user', all=FALSE)
user_weighted <- user_weighted[,list(zach_rating = sum(rating * weight)), by=c('beer')]
user_weighted <- merge(user_weighted, beers, by='beer')
user_weighted[,zach_rating := round(zach_rating, 3)]
user_weighted[order(zach_rating, rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#Cosine sim recs on beers
#sim <- sim2(t(mat[,GOOD_BEERS,drop=F]), t(mat), method='cosine', norm='none')[1,]
sim <- sim2(mod$v[GOOD_BEERS,,drop=F], mod$v, method='cosine', norm='none')[1,]
weights <- data.table(
  beer = 1:length(sim),
  weight = sim
)
beer_weighted <- merge(dat, weights, by='beer', all=FALSE)
beer_weighted <- beer_weighted[,list(zach_rating = sum(weight)), by=c('beer')]
beer_weighted <- merge(beer_weighted, beers, by='beer')
beer_weighted[,zach_rating := round(zach_rating, 3)]
beer_weighted[order(zach_rating, rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#Collaborative filtering
recs <- tcrossprod(mod$u[ME,,drop = F], mod$v)[1,]
round(summary(recs), 2)
recs <- data.table(
  beer = 1:length(recs),
  zach_rating = round(recs, 3)
)
recs <- merge(recs, beers, by='beer')
recs[,zach_rating := round(zach_rating, 3)]
recs[order(zach_rating, rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#TSNE on users
set.seed(42)
user_tsne <- Rtsne(mod$u, dims=2, pca=F, check_duplicates=F, verbose=TRUE, theta=0)
plot(user_tsne$Y)
points(user_tsne$Y[ME,,drop=F], col='red', pch=18, cex=2)
user_dist <- dist2(user_tsne$Y[ME,,drop=FALSE], user_tsne$Y, method='euclidean', norm='none')[1,]
sim <- 1 / user_dist
weights <- data.table(
  user = 1:length(sim),
  weight = sim
)
user_weighted_tsne <- merge(dat, weights, by='user', all=FALSE)
user_weighted_tsne <- user_weighted_tsne[,list(zach_rating = sum(rating * weight)), by=c('beer')]
user_weighted_tsne <- merge(user_weighted_tsne, beers, by='beer')
user_weighted_tsne[,zach_rating := round(zach_rating, 3)]
user_weighted_tsne[order(zach_rating, rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#TSNE on beers
set.seed(42)
beer_tsne <- Rtsne(mod$v, dims=2, pca=F, check_duplicates=F, verbose=TRUE, theta=0.5)
plot(beer_tsne$Y)
points(beer_tsne$Y[GOOD_BEERS,,drop=F], col='red', pch=18, cex=2)
beer_dist <- dist2(beer_tsne$Y[GOOD_BEERS,,drop=FALSE], beer_tsne$Y, method='euclidean', norm='none')
beer_dist <- as.matrix(beer_dist)
beer_dist <- colMins(beer_dist)
sim <- 1 / beer_dist
weights <- data.table(
  beer = 1:length(sim),
  weight = sim
)
beer_weighted_tsne <- merge(dat, weights, by='beer', all=FALSE)
beer_weighted_tsne <- beer_weighted_tsne[,list(zach_rating = sum(weight)), by=c('beer')]
beer_weighted_tsne <- merge(beer_weighted_tsne, beers, by='beer')
beer_weighted_tsne[,zach_rating := round(zach_rating, 3)]
beer_weighted_tsne[order(zach_rating, rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#Choose method
#final <- user_weighted_tsne
final <- user_weighted
final <- final[order(zach_rating, rating, decreasing=T),]

#Tip tap
final[at_tt==1 & see_recently == 1 & zach_drank == 0,list(brewery_name, beer_name, zach_rating, rating)]

#Global
head(final[zach_drank == 0,list(brewery_name, beer_name, zach_rating, rating)], 10)

########################################################
# Make recs for a new user
########################################################

make_user_recs <- function(user_id='raven31'){
  data <- get_checkins('user', user_id, n=25, wait=0)
  setorderv(data, 'checkin_id')
  data <- data[!duplicated(stri_paste(user_name, beer_id)),]
  data[,drank := 1]
  data <- merge(data, beers[,list(beer_id, beer)], by='beer_id')
  good_beer_ids <- sort(unique(data[,beer]))
  sim <- sim2(mod$v[good_beer_ids,,drop=F], mod$v, method='cosine', norm='none')[1,]
  weights <- data.table(
    beer = 1:length(sim),
    weight = sim
  )
  beer_weighted <- merge(beers, weights, by='beer', all=FALSE)
  beer_weighted <- merge(beer_weighted, data[,list(beer, drank)], by='beer', all.x=TRUE)
  beer_weighted[is.na(drank),drank := 0]
  beer_weighted <- beer_weighted[,list(zach_rating = weight[1]), by=c('beer', 'drank')]
  beer_weighted <- merge(beer_weighted, beers, by='beer')
  beer_weighted[,zach_rating := round(zach_rating, 3)]
  beer_weighted <- beer_weighted[order(zach_rating, decreasing=T),][drank == 0,]
  beer_weighted[at_tt==1 & see_recently == 1,list(brewery_name, beer_name, zach_rating, rating)]

}






make_beer_recs <- function(name = 'Old Speckled Hen'){
  library(stringdist)
  id <- beers[,beer[which.min(stringdist(tolower(name), tolower(beer_name), method='cosine'))]]
  beers[id,]

  beers[beer_id == 3121,]
  mod$v

}


########################################################
# OLD CODE
########################################################

weighted_tt[,rating := rating - median(rating)]
weighted_tt[rating < 0, rating := rating / 4]
weighted_tt[,good := sign(rating)]
weighted_tt <- weighted_tt[rating > 0,list(zach_rating = sum(rating * weight)), by=c('beer_id')]
weighted_tt <- merge(weighted_tt, beer, by='beer_id')
weighted_tt[order(zach_rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]




#Collaborative filtering
mod <- irlba(mat, nu=200, nv=200, verbose=TRUE)
recs <- tcrossprod(mod$u[ME,,drop = F], mod$v)[1,]
round(summary(recs), 2)
recs <- data.table(
  beer = 1:length(recs),
  zach_rating = round(recs, 3)
)
recs <- merge(recs, beers, by='beer')
recs[order(zach_rating, rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,][,list(brewery_name, beer_name)]










#Cosine users
#sim <- sim2(tt_pca[ME_map,,drop=F], tt_pca, method='cosine')
sim <- sim2(tt_mat_sparse[ME_map,,drop=F], tt_mat_sparse, method='cosine')
sim[,ME_map] <- 0
weights <- data.table(
  user_id = user_map,
  weight = as.numeric(sim)
)
weights <- weights[user_id != zach$user_id[1],]
weighted_tt <- merge(tt_room, weights, by='user_id', all=FALSE)
weighted_tt[,rating := rating - median(rating)]
weighted_tt[rating < 0, rating := rating / 4]
weighted_tt[,good := sign(rating)]
weighted_tt <- weighted_tt[rating > 0,list(zach_rating = sum(rating * weight)), by=c('beer_id')]
weighted_tt <- merge(weighted_tt, beer, by='beer_id')
weighted_tt[order(zach_rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]



















#Lookit the data
ME <- zach$user_id[1]
mybeers <- sort(unique(zach$beer_id))
#dat[(beer_id %in% zach$beer_id) & (user_id != ME) & rating > 4,]

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
tt_mat[, rating := rating - median(rating)]
tt_mat[rating < 0, rating := rating/4]
tt_mat[,good := sign(rating)]
tt_mat[good < 0, good := 0]
tt_mat_sparse <- sparseMatrix(
  i=tt_mat$u,
  j=tt_mat$b,
  x=tt_mat$rating)
summary(tt_mat_sparse@x)
tt_dist <- as.dist(tcrossprod(tt_mat))
tt_mat <- as.matrix(tt_mat_sparse)
tt_pca <- prcomp(tt_mat, retx=TRUE, center=FALSE)$x[,1:50] #use irlba_prcomp?
tt_tsne <- Rtsne(tt_dist, dims=2, is_distance=T, verbose=TRUE)
plot(tt_tsne$Y)
points(tt_tsne$Y[ME_map,,drop=F], col='red', pch=18)

#Cosine users
#sim <- sim2(tt_pca[ME_map,,drop=F], tt_pca, method='cosine')
sim <- sim2(tt_mat_sparse[ME_map,,drop=F], tt_mat_sparse, method='cosine')
sim[,ME_map] <- 0
weights <- data.table(
  user_id = user_map,
  weight = as.numeric(sim)
)
weights <- weights[user_id != zach$user_id[1],]
weighted_tt <- merge(tt_room, weights, by='user_id', all=FALSE)
weighted_tt[,rating := rating - median(rating)]
weighted_tt[rating < 0, rating := rating / 4]
weighted_tt[,good := sign(rating)]
weighted_tt <- weighted_tt[rating > 0,list(zach_rating = sum(rating * weight)), by=c('beer_id')]
weighted_tt <- merge(weighted_tt, beer, by='beer_id')
weighted_tt[order(zach_rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#Matrix factorization / collaborative filtering <- +!
mod <- irlba(tt_mat_sparse, nu=1, nv=1, verbose=TRUE)
recs <- tcrossprod(mod$u[ME_map,,drop = F], mod$v)[1,]
round(summary(recs), 2)
recs <- data.table(
  beer_id = beer_map,
  zach_rating = round(recs, 3)
)
recs <- merge(recs, beer, by='beer_id')
recs[order(zach_rating, rating_pois, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,][,list(brewery_name, beer_name)]

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
tt_mat[, rating := rating - median(rating)]
tt_mat[rating < 0, rating := rating/4]
tt_mat[,good := sign(rating)]
tt_mat <- sparseMatrix(
  i=tt_mat$b,
  j=tt_mat$u,
  x=tt_mat$rating)
#tt_mat <- row_wise_norm(tt_mat)
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

#Cosine beers2
sim <- sim2(tt_mat[ME_map,,drop=F], tt_mat, method='cosine')
sim[,ME_map] <- 0
dat[user_id == user_map[which.max(sim)],][order(rating),]
weights <- data.table(
  user_id = user_map,
  weight = as.numeric(sim)
)
weights <- weights[user_id != zach$user_id[1],]
weighted_tt <- merge(tt_room, weights, by='user_id', all=FALSE)
weighted_tt[,rating := rating - median(rating)]
weighted_tt[rating < 0, rating := rating / 4]
weighted_tt[,good := sign(rating)]
weighted_tt <- weighted_tt[rating > 0,list(zach_rating = sum(good * weight)), by=c('beer_id')]
weighted_tt <- merge(weighted_tt, beer, by='beer_id')
weighted_tt[order(zach_rating, decreasing=T),][at_tt==1 & see_recently == 1 & zach_drank == 0,]

#Do users with more than 1 rating
users <- dat[, list(.N), by='user_id']
users <- users[N>1,]

#TSNE beers
tt_mat <- dat[user_id %in% users$user_id,list(
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
tt_mat_pca <- prcomp(as.matrix(tt_mat), retx=T, center=FALSE, scale=FALSE)$x[,1:100]
set.seed(42)
tt_tsne <- Rtsne(
  tt_mat_pca, dims=2,
  check_duplicates=F,
  pca=F,
  theta=0.25,
  max_iter=2500,
  verbose=TRUE)
plot(tt_tsne$Y)
points(tt_tsne$Y[mybeers_map,,drop=F], col='red')
text(tt_tsne$Y[mybeers_map,,drop=F], col='red', labels=zach[beer_id %in% beer_map,beer_name])

tsne_sim <- as.matrix(dist(tt_tsne$Y))
tsne_sim <- max(tsne_sim) - tsne_sim
#tsne_sim <- 1 / (1+tsne_sim)
tsne_sim <- scale(log1p(tsne_sim), center=T, scale=T)
summary(as.numeric(tsne_sim))
tsne_sim <- as(tsne_sim, 'dgCMatrix')

mysims <- tsne_sim[mybeers_map,,drop=F] #Keep the beers I've had
mysims <- summary(mysims)
mysims_dat <- data.table(b=mybeers_map[mysims$i], rec=mysims$j, x=mysims$x)
#myratings <- data.table(b=mybeers_map, w=mybeers_ratings / max(mybeers_ratings))
mysims_dat <- merge(mysims_dat, myratings, by='b', all.x=TRUE)
mysims_dat[,beer_id := beer_map[rec]]
setkeyv(mysims_dat, 'beer_id')
mysims_dat <- mysims_dat[,list(x = max(x)), by='beer_id']
mysims_dat <- merge(mysims_dat, beer, by='beer_id', all.x=T)
setorder(mysims_dat, -x)
head(mysims_dat[!beer_id %in% mybeers & see_recently == T & at_tt == 1,], 10)

#Impute ratings
tt_mat <- dat[user_id %in% users$user_id,list(
  u = fmatch(user_id, user_map),
  b = fmatch(beer_id, beer_map),
  rating
)]
tt_mat <- sparseMatrix(
  i=tt_mat$b,
  j=tt_mat$u,
  x=tt_mat$rating - median(tt_mat$rating),
  dims = c(length(beer_map), length(user_map))
)
tt_mat[,fmatch(zach$user_id[1], user_map)]
sum(tt_mat[,fmatch(zach$user_id[1], user_map)]!=0)
imod <- irlba(tt_mat, nu=10, nv=10)
me <- imod$v[fmatch(zach$user_id[1], user_map),,drop=F]
me <- (me %*% t(imod$u))[1,]
me_ids <- beer_map[order(me)]
setkeyv(beer, 'beer_id')
me_beers <- copy(beer)
me_beers <- me_beers[!duplicated(beer_id),]
me_beers <- me_beers[match(beer_id, me_ids),]
me_beers[,my_rating := round(me, 2)]
head(me_beers[see_recently == T & at_tt == 1,][order(my_rating, decreasing=T),], 50)

#Exclude me from the data
tt_graph <- dat[!user_id %in% ME, list(user_id, beer_id, rating, checkin_id)]
tt_graph <- tt_graph[rating > 0,]
tt_subgraph <- tt_graph[beer_id %in% mybeers,]