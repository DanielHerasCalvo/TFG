#' CLUSTERING ALGORITHM VISUALIZATION FUNCTION
#'
#' Visualize clustering rankings and winners from the different algoritm's configurations
#' @param Entry List containing rankings and dictionary of the algoritm's configurations previously calculated with the function Clust_Alg_Rank
#' @param Option Object to visualize
#' @return Visualizations of mean ranking, most times winner ranking, best mean algorithm, most times winner algorithm and dictionary with the algorithms and configurations
#' @examples
#'Option <- "Mean_Ranking"
#'Clust_Alg_Plot(Entry, Option)
#'
#'Option <- "Count_Best_Ranking"
#'Clust_Alg_Plot(Entry, Option)
#'
#'Option <- "Mean_Best_Algorithm"
#'Clust_Alg_Plot(Entry, Option)
#'
#'Option <- "Count_Best_Algorithm"
#'Clust_Alg_Plot(Entry, Option)
#'
#'Option <- "Dictionary"
#'Clust_Alg_Plot(Entry, Option)
#' @export
Clust_Alg_Plot <- function(Entry, Option) {

  #Definition of the dictionary received by parameter
  Dictionary <- Entry$Dictionary

  #Definition of the ranking received by parameter
  Ranking <- Entry$Ranking

  #Visualization of the mean rank
  if(Option=="Mean_Ranking") {

    #Total mean Ranking Bar Diagram
    Ranking <- Ranking[order(Ranking$best_mean_rank),]

    Mean_Total_Ranking <- Ranking["mean_rank"]

    Mean_Total_Ranking_BarPlot <- tibble::rownames_to_column(Mean_Total_Ranking, "Algorithm_Config")

    Mean_Total_Rank <- ggplot() +
      geom_col(data = Mean_Total_Ranking_BarPlot,
               aes(x=reorder(Algorithm_Config, mean_rank), y = mean_rank),
               fill = "blue",
               width = 0.5,
               color = "black") +
      ggtitle("Total Algorithms Configurations Mean Ranking") +
      xlab("Algorithm_Config")

    #Top 10 mean rank in a Bar Diagram
    Ranking_Mean_Winners <- head(Mean_Total_Ranking, 10)

    Ranking_Mean_Winners_BarPlot <- tibble::rownames_to_column(Ranking_Mean_Winners, "Algorithm_Config")

    Winners_Mean_Rank <- ggplot() +
      geom_col(data = Ranking_Mean_Winners_BarPlot,
               aes(x=reorder(Algorithm_Config, mean_rank), y = mean_rank),
               fill = "green",
               width = 0.5,
               color = "white") +
      ggtitle("Best Algorithms Configurations Mean Ranking") +
      xlab("Algorithm_Config")

    #Plot of mean rankings
    ggarrange(Winners_Mean_Rank, Mean_Total_Rank, nrow = 2)

  }

  #Visualization of the most times best rank
  else if (Option=="Count_Best_Ranking") {

    #Total most times best ranking in a Bar Diagram
    Ranking <- Ranking[order(-Ranking$count_best_rank),]

    Count_Best_Total_Ranking <- Ranking["count_best_rank"]

    Count_Best_Total_Ranking_BarPlot <- tibble::rownames_to_column(Count_Best_Total_Ranking, "Algorithm_Config")

    Count_Best_Total_Rank <- ggplot() +
      geom_col(data = Count_Best_Total_Ranking_BarPlot,
               aes(x=reorder(Algorithm_Config, -count_best_rank), y = count_best_rank),
               fill = "purple",
               width = 0.5,
               color = "black") +
      ggtitle("Ranking of Total Most Times Best Algorithm Configurations") +
      xlab("Algorithm_Config")

    #Top 10 most times best ranking in a Bar Diagram
    Ranking_Count_Best_Winners <- head(Count_Best_Total_Ranking, 10)

    Ranking_Count_Best_Winners_BarPlot <- tibble::rownames_to_column(Ranking_Count_Best_Winners, "Algorithm_Config")

    Winners_Count_Best_Rank <- ggplot() +
      geom_col(data = Ranking_Count_Best_Winners_BarPlot,
               aes(x=reorder(Algorithm_Config, -count_best_rank), y = count_best_rank),
               fill = "yellow",
               width = 0.5,
               color = "white") +
      ggtitle("Ranking of the Most Times Best Algorithm Configurations") +
      xlab("Algorithm_Config")

    #Plot of most times best ranking
    ggarrange(Winners_Count_Best_Rank, Count_Best_Total_Rank, nrow = 2)

  }

  #Visualization of the mean best algorithm
  else if (Option=="Mean_Best_Algorithm") {

    #Best Algorithm Configuration
    Ranking <- Ranking[order(Ranking$best_mean_rank),]

    Best_Alg_Config <- Dictionary[[row.names(head(Ranking,1))]]

    #For hclust, agnes and diana
    if(Best_Alg_Config$Params$name %in% c("hclust", "agnes", "diana")) {

      #Create Dendogram
      Best_Alg_Config_DendPlot <- as.dendrogram(Best_Alg_Config$Alg)

      #Plot of Dendogram
      Dendogram <- color_branches(Best_Alg_Config_DendPlot, h = Best_Alg_Config$Params$h)
      Dendogram %>% plot(main = "Dendogram of the Mean Best Algorithm Configuration")

    }

    #For Mclust
    else if (Best_Alg_Config$Params$name == "Mclust") {

      pca <- prcomp(Dataset, scale. = T, center = T)
      pca_variables <- predict(pca, newdata = Dataset)
      pca_data <- as.data.frame(cbind(pca_variables, group=Best_Alg_Config$Alg$classification))

      #Visualization of the pca components
      gg2 <- ggplot(pca_data) +
        geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca_data)), size=2) +
        labs(title = "PCA Graphic of the Mean Best Algorithm Configuration") +
        scale_color_brewer(name="", palette = "Set1")

      ggplotly(gg2, tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x=.9, y=.99))

    }

    #For Cluster_Medoids and Clara_Medoids
    else if (Best_Alg_Config$Params$name %in% c("Cluster_Medoids", "Clara_Medoids")) {

      pca <- prcomp(Dataset, scale. = T, center = T)
      pca_variables <- predict(pca, newdata = Dataset)
      pca_data <- as.data.frame(cbind(pca_variables, group=Best_Alg_Config$Alg$clusters))

      #Visualization of the pca components
      gg2 <- ggplot(pca_data) +
        geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca_data)), size=2) +
        labs(title = "PCA Graphic of the Mean Best Algorithm Configuration") +
        scale_color_brewer(name="", palette = "Set1")

      ggplotly(gg2, tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x=.9, y=.99))

      #For the rest of the algorithms
    } else {

      pca <- prcomp(Dataset, scale. = T, center = T)
      pca_variables <- predict(pca, newdata = Dataset)
      pca_data <- as.data.frame(cbind(pca_variables, group=Best_Alg_Config$Alg$cluster))

      #Visualization of the pca components
      gg2 <- ggplot(pca_data) +
        geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca_data)), size=2) +
        labs(title = "PCA Graphic of the Mean Best Algorithm Configuration") +
        scale_color_brewer(name="", palette = "Set1")

      ggplotly(gg2, tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x=.9, y=.99))

    }

  }

  #Visualization of the most times best algorithm
  else if (Option=="Count_Best_Algorithm") {

    #Best Algorithm Configuration
    Ranking <- Ranking[order(-Ranking$count_best_rank),]

    Best_Alg_Config <- Dictionary[[row.names(head(Ranking,1))]]

    if(Best_Alg_Config$Params$name %in% c("hclust", "agnes", "diana")) {

      #Create Dendogram
      Best_Alg_Config_DendPlot <- as.dendrogram(Best_Alg_Config$Alg)
      #ggdendrogram(Best_Alg_Config_DendPlot, labels=FALSE)

      #Plot of Dendogram
      Dendogram <- color_branches(Best_Alg_Config_DendPlot, h = Best_Alg_Config$Params$h)
      Dendogram %>% plot(main = "Dendogram of the Most Times Best Algorithm Configuration")
      #ggplot(Dendogram)

    }

    #For Mclust
    else if (Best_Alg_Config$Params$name == "Mclust") {

      pca <- prcomp(Dataset, scale. = T, center = T)
      pca_variables <- predict(pca, newdata = Dataset)
      pca_data <- as.data.frame(cbind(pca_variables, group=Best_Alg_Config$Alg$classification))

      #Visualization of the pca components
      gg2 <- ggplot(pca_data) +
        geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca_data)), size=2) +
        labs(title = "PCA Graphic of the Most Times Best Algorithm Configuration") +
        scale_color_brewer(name="", palette = "Set1")

      ggplotly(gg2, tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x=.9, y=.99))

    }

    #For Cluster_Medoids and Clara_Medoids
    else if (Best_Alg_Config$Params$name %in% c("Cluster_Medoids", "Clara_Medoids")) {

      pca <- prcomp(Dataset, scale. = T, center = T)
      pca_variables <- predict(pca, newdata = Dataset)
      pca_data <- as.data.frame(cbind(pca_variables, group=Best_Alg_Config$Alg$clusters))

      #Visualization of the pca components
      gg2 <- ggplot(pca_data) +
        geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca_data)), size=2) +
        labs(title = "PCA Graphic of the Most Times Best Algorithm Configuration") +
        scale_color_brewer(name="", palette = "Set1")

      ggplotly(gg2, tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x=.9, y=.99))

      #For the rest of the algorithms
    } else {

      pca <- prcomp(Dataset, scale. = T, center = T)
      pca_variables <- predict(pca, newdata = Dataset)
      pca_data <- as.data.frame(cbind(pca_variables, group=Best_Alg_Config$Alg$cluster))

      #Visualization of the pca components
      gg2 <- ggplot(pca_data) +
        geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca_data)), size=2) +
        labs(title = "PCA Graphic of the Most Times Best Algorithm Configuration") +
        scale_color_brewer(name="", palette = "Set1")

      ggplotly(gg2, tooltip = c("text", "x", "y")) %>%
        layout(legend = list(x=.9, y=.99))

    }

  }

  #Visualization of the dictionary
  else if (Option=="Dictionary") {

    View(Dictionary)

  }

}
