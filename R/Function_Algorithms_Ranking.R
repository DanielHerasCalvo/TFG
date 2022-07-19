############################################################################       CLUSTERING ALGORITHM RANKING FUNCTION        ##################################################################################

Clust_Alg_Rank <- function(Dataset, Algorithm_Configs, Metrics_List) {

  #Cration of a dictionary containing every algorithm configuration
  Dictionary <- list()

  #Definition of the metric list when paramter pass is "all"
  if (length(Metrics_List) == 1 && Metrics_List[1] == "all") {

    Metrics_List <- c("Ball_Hall","Calinski_Harabasz","Gamma","PBM","Point_Biserial","Ratkowsky_Lance","Silhouette","Trace_W","Trace_WiB","Tau","Wemmert_Gancarski","Dunn","GDI11","Ksq_DetW","Banfeld_Raftery","C_index","Davies_Bouldin","G_plus","McClain_Rao","Ray_Turi","SD_Scat","SD_Dis","Xie_Beni","Log_SS_Ratio","Det_Ratio")

  }

  #Definition of metrics vectors that should be maximaize and minimize
  Metrics_Max <- c("Ball_Hall","Calinski_Harabasz","Gamma","PBM","Point_Biserial","Ratkowsky_Lance","Silhouette","Trace_W","Trace_WiB","Tau","Wemmert_Gancarski","Dunn","GDI11","Ksq_DetW")
  Metrics_Min <- c("Banfeld_Raftery","C_index","Davies_Bouldin","G_plus","McClain_Rao","Ray_Turi","SD_Scat","SD_Dis","Xie_Beni","Log_SS_Ratio","Det_Ratio")

  #Creation of the list of metrics with as many sublists as the number of metrics
  Metrics <- vector("list", length(Metrics_List))
  names(Metrics) <- Metrics_List

  #Iteration for each algorithm
  for (i in 1:length(Algorithm_Configs)) {

    #Defintion of the cartesian product between the parameters of the algorithm

    #For hclust, agnes y diana
    if(Algorithm_Configs[[i]]$name %in% c("hclust", "agnes", "diana")) {

      Height_List <- as.data.frame(Algorithm_Configs[[i]][[2]][which(names(Algorithm_Configs[[i]][[2]]) == "h")])
      Param_List <- Algorithm_Configs[[i]][[2]][which(names(Algorithm_Configs[[i]][[2]]) != "h")]
      Prod_Cart <- expand.grid(Param_List, stringsAsFactors = FALSE)
      Prod_Cart_Param_Cluster <- expand.grid(Algorithm_Configs[[i]][[2]], stringsAsFactors = FALSE)
      Prod_Cart_Param_Cluster_Extended <- cbind(name = Algorithm_Configs[[i]]$name, Prod_Cart_Param_Cluster)

      #For the rest of the algorithms
    } else {

      Prod_Cart <- expand.grid(Algorithm_Configs[[i]][[2]], stringsAsFactors = FALSE)

    }

    #Extension of cartasian product adding a column with the algorithm name

    #For algorithms without parameters. Assgn of empty vector to the cartasian product so that the extension can be done
    if (length(Prod_Cart) == 0) {

      Prod_Cart <- data.frame(vector())
      Prod_Cart_Extended <- data.frame(name = Algorithm_Configs[[i]]$name)
      p <- 0

      #For the rest of the algorithms
    } else {

      Prod_Cart_Extended <- cbind(name = Algorithm_Configs[[i]]$name, Prod_Cart)
      p <- 1

    }

    #Iteration for each configuration of the algorithm
    for (j in p:length(Prod_Cart[[1]])) {

      #Assignment of 1 to j if the algorithm has no parameters, so that the name in the dictionary the suffix 1. Empty Prod_Cart to avoid failing algorithm call with an NA value for the parameter
      if (j==0) {

        Prod_Cart <- data.frame()

        j <- 1

      }

      #Keeping the result of the algorithm and configuration calculation

      #For hclust, calculation of the distance matrix is needed first
      if (Algorithm_Configs[[i]]$name == "hclust") {

        d = dist(Dataset, method = "euclidean")
        Alg <- do.call(Algorithm_Configs[[i]][[1]], c(list(d), Prod_Cart[1,]))

        #For the rest of the algorithms
      } else {

        Alg <- do.call(Algorithm_Configs[[i]][[1]], c(list(Dataset), Prod_Cart[j,]))

      }

      #Filling the dictionary with key = diminutive algorithm names + iteration of their configuration and value = algorithm names + their configuration

      #For hclust, agnes y diana, iteration of the height list is needed to fill the Dictionary
      if (Algorithm_Configs[[i]]$name %in% c("hclust", "agnes", "diana")) {

        for (x in 1:length(Height_List$h)) {

          key <- paste0(names(Algorithm_Configs[i]), x)
          value <- c(Alg = list(Alg), Params = list(Prod_Cart_Param_Cluster_Extended[x,]))
          Dictionary[key[1]] <- list(value)

        }

        #For the rest of the algorithms
      } else {

        key <- paste0(names(Algorithm_Configs[i]), j)
        value <- c(Alg = list(Alg), Params = list(Prod_Cart_Extended[j,]))
        Dictionary[key[1]] <- list(value)

      }

      #Iteration for each metric calculating the indexes of each algorithm and its configuration and keeping it in the metric list
      for (k in 1:length(Metrics_List)) {

        #For hclust, agnes y diana, iteration of the height list is needed to label the algorithm
        if (Algorithm_Configs[[i]]$name %in% c("hclust", "agnes", "diana")) {

          for (y in 1:length(Height_List$h)) {

            ct <- cutree(Alg, h = Height_List$h[[y]])
            Index <- intCriteria(as.matrix(Dataset), as.integer(ct), Metrics_List[[k]])
            Metrics[[k]] <- append(Metrics[[k]], Index[[1]])

          }

        }

        #For Mclust, classification object is used as cluster label
        else if (Algorithm_Configs[[i]]$name == "Mclust") {

          Index <- intCriteria(as.matrix(Dataset), as.integer(Alg$classification), Metrics_List[[k]])
          Metrics[[k]] <- append(Metrics[[k]], Index[[1]])

        }

        #For Cluster_Medoids y Clara_Medoids, clusters object is used as cluster label
        else if (Algorithm_Configs[[i]]$name %in% c("Cluster_Medoids", "Clara_Medoids")) {

          Index <- intCriteria(as.matrix(Dataset), as.integer(Alg$clusters), Metrics_List[[k]])
          Metrics[[k]] <- append(Metrics[[k]], Index[[1]])

          #For the rest of the algorithms, cluster object is used as cluster label
        } else {

          Index <- intCriteria(as.matrix(Dataset), as.integer(Alg$cluster), Metrics_List[[k]])
          Metrics[[k]] <- append(Metrics[[k]], Index[[1]])

        }

      }

    }

  }

  #Creation of Dataframe, transforming every sublist, representating metric vectors values, in a column. Then, assigning to every rowname the dictionary names that represents every configuration algorithm
  Df_Metrics <- data.frame(do.call(cbind, Metrics))

  rownames(Df_Metrics) <- names(Dictionary)

  #Discard of values out of range
  Df_Metrics[Df_Metrics == 0 | Df_Metrics == 1 | Df_Metrics == Inf | Df_Metrics == -Inf] <- NaN
  Df_Metrics <- drop_na(Df_Metrics)

  #Creation and adjustments of the dataframe for containing the ranking of the best algorithms and their configurations
  Ranking <- Df_Metrics

  for (l in 1:length(Metrics_List)) {

    if(colnames(Ranking[l]) %in% Metrics_Min) {

      Ranking[l] = rank(Ranking[l], na.last = "keep", ties.method = "min")

    }

    else if (colnames(Ranking[l]) %in% Metrics_Max) {

      Ranking[l] = rank(desc(Ranking[l]), na.last = "keep", ties.method = "min")

    }

  }

  #Calculation of the ranking, based on mean rank between the configuration algorithms
  Ranking$mean_rank <- rowMeans(Ranking, na.rm = TRUE)
  Ranking$best_mean_rank = rank(Ranking$mean_rank, na.last = TRUE, ties.method = "min")

  #Calculation of the ranking, based on the times beeing the best configuration algorithm
  Ranking$count_best_rank <- rowSums(Ranking == 1, na.rm = TRUE)

  #Return of the dictionary and Ranking
  Results <- list("Dictionary" = Dictionary, "Ranking" = Ranking)
  return(Results)

}
