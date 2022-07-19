#Inicicializaciones usr
# For Data Manipulation
set.seed(2)


pointsInCircum <- function(radius, num_pts, jitter) {

  x <- mapply(function(n) {return (cos(2*pi/num_pts*n) * radius + rnorm(1, -1 * jitter, jitter))}, 1:num_pts)
  y <- mapply(function(n) {return (sin(2*pi/num_pts*n) * radius + rnorm(1, -1 * jitter, jitter))}, 1:num_pts)

  return (data.frame(x=x, y=y))
}


# Build set of concentric points
df <- pointsInCircum(100, 300, 30)
df <- rbind(df, pointsInCircum(300, 700, 30))
df <- rbind(df, pointsInCircum(500, 1000, 30))

Dataset <- df

Algorithm_Configs <- list(

  km = list(name = "kmeans",
            config = list(centers = c(3,4,5,6),
                          iter.max = 10,
                          nstart = 1,
                          algorithm = c("Hartigan-Wong")
            )
  ),

  cm = list(name = "Cluster_Medoids",
            config = list(clusters = c(3,4,5,6),
                          swap_phase = TRUE,
                          verbose = F
            )
  ),

  cl_m = list(name = "Clara_Medoids",
              config = list(clusters = c(3,4,5,6),
                            distance_metric = 'minkowski',
                            samples = 5,
                            sample_size = 0.2,
                            swap_phase = FALSE,
                            verbose = F,
                            threads = 1
              )
  ),

  hc = list(name = "hclust",
            config = list(method = "complete",
                          h = c(3,4,5,6)
            )
  ),

  ag = list(name = "agnes",
            config = list(metric = "euclidean",
                          method = "ward",
                          h = c(3,4,5,6)
            )
  ),

  di = list(name = "diana",
            config = list(h = c(3,4,5,6)
            )
  ),

  db = list(name = "dbscan",
            config = list(eps = c(32,0.50,0.56),
                          minPts = c(5,6,7)
            )
  ),

  mc = list(name = "Mclust",
            config = list(G=3:9
            )
  )

)

Metrics_List <- "all"

Entry <- Clust_Alg_Rank(Dataset, Algorithm_Configs, Metrics_List)

#Plot Mean Ranking
Option <- "Mean_Ranking"
Clust_Alg_Plot(Entry, Option)

#Plot Count Best Ranking
Option <- "Count_Best_Ranking"
Clust_Alg_Plot(Entry, Option)

#Plot Mean Best Algorithm
Option <- "Mean_Best_Algorithm"
Clust_Alg_Plot(Entry, Option)

#Plot Count Best Algorithm
Option <- "Count_Best_Algorithm"
Clust_Alg_Plot(Entry, Option)

#View Dictionary
Option <- "Dictionary"
Clust_Alg_Plot(Entry, Option)





