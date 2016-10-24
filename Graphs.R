# Graphics
library(ggplot2)
time_tgth <- readRDS("Reduced Data - combined timeRDS.rds")

time_sep <- readRDS("Reduced DataRDS.rds")

temp_by_year <- aggregate(time_sep[,13], list(time_sep$YY), mean, na.rm = TRUE)
ggplot(temp_by_year, aes(Group.1, x)) + geom_point() + labs(x = "Year", y = "Temp (C)")

wind <- aggregate(time_sep[,6:7], list(time_sep$WDIR), mean, na.rm = TRUE)

ggplot(wind, aes(Group.1, WSPD)) + geom_bar(stat = "identity", col = "blue") + coord_polar(theta = "x", start = 0, direction = 1) +
  labs(x = "Direction", y = "Wind Speed (m/s)")

wind2 <- aggregate(time_sep[,6:7], list(time_sep$MM), mean, na.rm = TRUE)

ggplot(time_sep, aes(as.factor(MM), WSPD)) + geom_boxplot() + labs(x = "Month", y = "Wind Speed (m/s)")



summary_stat <- function(dataset){
  no_time = subset(colnames(dataset), !(colnames(dataset) %in% c("Date", "Time")))
  mat = matrix(0, nrow = 2, ncol = length(no_time))
  for(i in c(1:length(no_time))){
    mat[1,i] = mean(dataset[,i + 2], na.rm = TRUE)
    mat[2,i] = var(dataset[,i + 2], na.rm = TRUE)
  }
  rownames(mat) <- c("Mean", "Variance")
  colnames(mat) <- no_time
  mat
}

statis <- summary_stat(time_tgth)
