#function for calculating distances between adjacency vectors

#euclidean
get.euc.dist = function(x1,x2){
  euc.dist = sapply(seq.int(dim(x1)[1]), function(i) as.numeric(dist(rbind(x1[i,],x2[i,]))))
}

#mahalanobis
get.mah.dist = function(x1,x2,covmats){
  mah.dist = sapply(seq.int(dim(x1)[1]), function(i) mahalanobis(x = x1[i,],center = x2[i,],cov = covmats[[i]]))
}


