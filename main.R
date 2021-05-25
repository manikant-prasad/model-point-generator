library(openxlsx)
library(dplyr)
library(properties)
library(stringr)

source("source.R")

config <- read.properties("config.properties")
data.main <- read_file(config$file.path, config$file.type)

names(data.main)

summary(data.main)

str(data.main)

#Reads a list of columns from the properties file, you want to sample the data for
columns = str_split(string = config$columns,pattern = ',', simplify = T)

model_points = generate_model_points(data_set = data.main, columns = columns, sample_size = 40, iterations = 100, initial_iterations = 50, iteration_sample_increase = 0.4)

write_file(df = model_points, file.write = config$file.write, file.write.type = config$file.write.type)