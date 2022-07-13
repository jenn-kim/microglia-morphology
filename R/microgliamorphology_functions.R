#' Would you like to get some metadata?
#'
#' This function allows you to extract metadata columns from the UniqueID column.
#' 
#' @param x is your unprocessed input .csv file
#' @param y is your processed output .csv file containing all your metadata neatly organized into separate columns
#' @keywords UniqueID
metadata_columns <- function(x,y){
  data <- read.csv(x)
  data <- data[,-1]
  PCAinput <- data[,c(1:2,4:7,9:18,20:24,27:35)]
  PCAinput <- PCAinput %>% 
    mutate_at("Name", ~gsub("_rep._scene.","",.)) %>%
    mutate_at("Name",~gsub("_M","",.)) %>% 
    mutate_at("Name",~gsub("_F_","_",.)) %>% 
    separate(Name, into=c("cohort","mouse","condition","brainregion","subregion"), sep="_") %>%
    write.csv(y)
}
