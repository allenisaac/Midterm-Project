library(plyr)

# creating a list of file names to be opened
# in order to do for other sets of files the list must be adapted
years <- c(1973:2015)
years <- years[years != 2009 & years != 2011]

files <- paste("Raw Data/", years, ".txt", sep = "")


# finding all variables to standardize them later
variable_list <- function(file_list){
  variables = matrix(0, nrow = length(file_list), ncol = 18)
  for(i in c(1:length(file_list))){
    names = colnames(read.table(file_list[i], header = TRUE, fill = TRUE, comment.char = ""))
    variables[i,] = c(names, rep("NA", times = 18 - length(names)))
  }
  variables
}

all_variables <- variable_list(files)
# These are the different variables, all the others are the same
# BAR = PRES, YYYY = YY, X.YY = YY, #YY = YY, WD = WDIR

#helper function for combine files
#   adds variables to datasets without all 18 and renames variables that need it 
same_var <- function(df){
  df = plyr::rename(df, c("YYYY" = "YY", "X.YY" = "YY", "WD" = "WDIR", "BAR" = "PRES"), warn_missing = FALSE)
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

# goes through each file and adds it to the entire dataframe
combine_files <- function(file_list){
  full_data <- data.frame()
  for(i in c(1:length(file_list))){
    temp_data = read.table(file_list[i], header = TRUE, fill = TRUE, comment.char = "")
    temp_data = same_var(temp_data)
    full_data = rbind(full_data, temp_data)
  }
  full_data
}

combined_data <- combine_files(files)
# This data 

library(dplyr)

# Removes NA rows, NA columns and correctly enters the values as NA
no_na_var <- function(dataset){
  j = c(1:length(dataset))
  dataset[,j] = lapply(dataset[,j], function(x) as.numeric(as.character(x)))
  dataset = dataset[rowSums(is.na(dataset)) !=ncol(dataset),]
  dataset[is.na(dataset)] = -1
  for(i in c(1:length(dataset))){
    if(max(dataset[,i]) %in% c(99, 999, 9999)){
      dataset[, i][dataset[,i] == max(dataset[,i])] = -1 #changed 12 to na
    }
  }
  dataset[dataset == -1] <- NA
  dataset = dataset[colSums(!is.na(dataset)) > 0]
  dataset
}

reduced_data <- no_na_var(combined_data)

library(tidyr)

# Removes rows that contain only time and NA
#   Sums the row (ignoring NA and time), 
#     rows with a sum of 0 can only contain NAs and are removed
nand_time <- function(dataset){
  dataset[,1][dataset[,1] < 100] = paste("19", dataset[,1][dataset[,1] < 100], sep = "")
  times = match(c("YY", "MM", "DD", "hh", "mm"), colnames(dataset))
  i = c(1:length(dataset))
  times = subset(i, !(i %in% times))
  new_data = filter(dataset, (rowSums(dataset[, times], na.rm = TRUE)) != 0)
  new_data
}

reduced_rows <- nand_time(reduced_data)

# Combines hour and minute into Time
time_comb <- function(dataset){
  times = match(c("hh", "mm"), colnames(dataset))
  dataset$mm[is.na(dataset$mm)] = "00"
  unite(dataset, "Time", times, sep = ":")
}

# Combines Year Month and Day into Date
date_comb <- function(dataset){
  dates = match(c("YY", "MM", "DD"), colnames(dataset))
  unite(dataset, "Date", dates, sep = "-")
}

reduced_vars <- time_comb(reduced_rows)

reduced_vars <- date_comb(reduced_vars)

time_fix <- function(dataset){
  dataset[,1] = as.Date(dataset[, 1], format = '%Y-%m-%d')
  dataset
}

reduced_vars <- time_fix(reduced_vars)

# Writing one file if to use year, month, date and time seperate
# Another as the date and the time
write.csv(reduced_vars,"Reduced Data - combined time.txt")
write.csv(reduced_rows,"Reduced Data.txt")

#Also writing as R data file
saveRDS(reduced_vars,"Reduced Data - combined timeRDS.rds")
saveRDS(reduced_rows,"Reduced DataRDS.rds")


# Finds all combinations of variables without NAs
var_combinations <- function(dataset){
  i = c(1:length(dataset[,1]))
  mat = dataset 
  mat[!is.na(mat)] = col(mat)[!is.na(mat)]
  mat = 
  mat = distinct(mat)
  mat
}

variable_combos <- var_combinations(reduced_rows)

length(variable_combos[,1])

# THere are 104 different combinations of variables, and no logical way to seperate the data
