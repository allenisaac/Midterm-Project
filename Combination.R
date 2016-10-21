library(plyr)


years <- c(1973:2015)
years <- years[years != 2009 & years != 2011]

files <- paste("Raw Data/", years, ".txt", sep = "")


# finding all variables to standardize them later
variable_list <- function(file_list){
  i = 1
  variables = matrix(0, nrow = 41, ncol = 18)
  while(i <= length(file_list)){
    names = colnames(read.table(file_list[i], header = TRUE, fill = TRUE, comment.char = ""))
#    names = names[!names %in% variables]
    variables[i,] = c(names, rep("NA", times = 18 - length(names)))
    i = i + 1
  }
  variables
}

all_variables <- variable_list(files)
# BAR = PRES, YYYY = YY, X.YY = YY, #YY = YY, WD = WDIR

#helper function for combine files
#   adds variables to datasets without all 18 and renames variables that 
same_var <- function(df){
  df = rename(df, c("YYYY" = "YY", "X.YY" = "YY", "WD" = "WDIR", "BAR" = "PRES"), warn_missing = FALSE)
  if(length(df) < 18){
    mm = rep(99.0, times = length(df$YY))
    df = cbind(df, mm)
    if(length(df) < 18){
      TIDE = rep(99.00, times = length(df$YY))
      df = cbind(df, TIDE)
    }
  }
  df
}


combine_files <- function(file_list){
  i = 1
  full_data <- data.frame()
  while(i <= length(file_list)){
    temp_data = read.table(file_list[i], header = TRUE, fill = TRUE, comment.char = "")
    temp_data = same_var(temp_data)
    full_data = rbind(full_data, temp_data)
    i = i + 1
  }
  full_data
}

combined_data <- combine_files(files)

# Save file


library(dplyr)

no_na_var <- function(dataset){
  i = c(1:length(dataset))
  dataset[,i] = lapply(dataset[,i], function(x) as.numeric(as.character(x)))
  dataset = dataset[rowSums(is.na(dataset)) !=ncol(dataset),]
  dataset[is.na(dataset)] = -1
  for(i in c(1:length(dataset))){
    if(max(dataset[,i]) %in% c(99, 999, 9999)){
      dataset[, i][dataset[,i] == max(dataset[,i])] = -1 #changed 12 to na
    }
    if(max(dataset[,i]) == -1){
      select(dataset, -i)
    }
  }
  dataset[dataset == -1] <- NA
  dataset
}

reduced_data <- no_na_var(combined_data)

library(tidyr)

time_comb <- function(dataset){
  dataset$mm[is.na(dataset$mm)] = 00
  unite(dataset, "Time", c(4, 17), sep = ":")
}

date_comb <- function(dataset){
  unite(dataset, "Date", c(1, 2, 3), sep = "-")
}

reduced_vars <- time_comb(reduced_data)

reduced_vars <- date_comb(reduced_vars)

# Removes rows that contain only time and NA
#   Sums the row (ignoring NA and time), 
#     rows with a sum of 0 can only contain NAs and are removed
nand_time <- function(dataset){
  new_data = filter(dataset, (rowSums(dataset[, 3:length(dataset)], na.rm = TRUE)) != 0)
  new_data
}





