# individual version


# this function tests whether a given ENA model (individual or group), m,
# is statistically different from a distribution ENA models (typically
# models where randomness has been introduced) M. This is an empirical test (i.e.,
# it does not assume a particular distribution). It can be thought of as an
#empiricall Hoetelling's Test

ena.compare.model.indiv = function(observedMod,simMods,method = c("euclidean","mahalanobis"),test = FALSE){
  
  #calculate the mean of the distribution of models (i.e., the centroid of the 
  #point cloud)
  #browser()
  meanMod = Reduce("+",simMods)/length(simMods)
  
  #transform the distribution so that we have an mxn matrix of points for each indiv-
  #idual where m = number of simulations and n = number of variables (connections)
  # will need this to produce the covariance matrices for the distance metric
  simModsByUnit = do.call("rbind",simMods)
  
  #generate covariance matrices 
  
  if (method == "euclidean"){
    #for euclidean distance all cov matrices are identity 
    #https://en.wikipedia.org/wiki/Mahalanobis_distance
    dims = ncol(simModsByUnit)
    covs = diag(1,nrow = dims,ncol = dims)
  }
  
  else if (method == "mahalanobis"){
    covs = cov(simModsByUnit)
  }
  
  else{
    stop("Chosen method not supported. Ensure you are using either euclidean or mahalanobis")
  }
  
  # calculate distance between each observed point and the centroid.
  obs.distances = mahalanobis(x = observedMod,center = meanMod,cov = covs)
  #mean these distances to get the test statistic
  testStat = obs.distances
  #Calculate distances between each point in the distribution and the centroid
  #Obtain a distribution of distances. This is the null hypothesis distribution
  distribution = sapply(seq.int(length(simMods)),
                        function (i) mahalanobis(x = simMods[[i]],center = meanMod,cov = covs))
  # get pval (number of distances more extreme than the test stat)
  x = sum(distribution > testStat)
  p = x/length(distribution) #get p-val
  ci = quantile(distribution,probs = c(0.25,0.975))
  
  return(list(statistic = testStat, pval = p, ci = ci, distribution = distribution,observed_distances = obs.distances))
}
