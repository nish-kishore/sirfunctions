################################################################################
##              SIA Failure Functions                                        ###
##              Writen by Stephanie Kovacs and Nishant Kishore               ###
##              updated by Nick Heaghney spring 2023                         ###
##              updated by Nick Heaghney Summer 2024                         ###
################################################################################

#initiate sia impact reprot function
#this function initializes a folder structure for the purpose of running the sia impact report
#' @export
#' @param folder_loc str: location of folder to set up and run SIA impact report
init_sia_impact <- function(folder_loc){

}


#Cluster Function
#this function identifies "cluster" or OBX response so we can identify rounds
#' @export
cluster_dates <- function(x, seed = 1234, method = "kmeans", grouping_days = 365){

  if(method == "kmeans"){
    #prepare the data
    y <- x %>%
      #select only dates
      select(date = sub.activity.start.date) %>%
      #calculate distance from minimum date
      mutate(date = as.numeric(date - min(date))) %>%
      #normalize values for clustering
      scale()

    set.seed(seed)
    #calculate the optimal number of clusters
    optim_k <- y %>%
      #calculate optimal number of clusters using the
      #gap statistic
      {clusGap(., FUN = kmeans, nstart = 25, K.max = max(min(nrow(.)-1, nrow(.)/2), 2), B = 100)} %>%
      #extract gap statistic matrix
      {.$Tab[,"gap"]} %>%
      #calculate the max gap statistic, given the sparsity in the data
      #am not limiting to the first max SE method
      which.max()

    set.seed(seed)
    #calculate the clusters
    x$cluster <- kmeans(y, optim_k)$cluster %>%
      #clusters don't always come out in the order we want them to
      #so here we convert them into factors, relevel and then extract
      #the numeric value to ensure that the cluster numbers are in order
      {factor(., levels = unique(.))} %>%
      as.numeric()

    #outputting the method used
    x$cluster_method <- method

    return(x)
  }else{
    if(method == "mindate"){
      x <- x %>%
        mutate(cluster = as.numeric(sub.activity.start.date - min(sub.activity.start.date)),
               cluster = cut(cluster, seq(0, grouping_days*6, by = grouping_days), include.lowest = T),
               cluster = as.numeric(cluster),
               cluster_method = method)

      return(x)
    }
  }



}
