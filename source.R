#Params
##data_set -> dataframe to extract the model points from, default is an empty dataframe
##columns -> set of categorical columns - all the levels from these columns will be included in the model points generated, default is an empty list
##sample_size -> minimum size of the sample you want for your model points, default is 10 rows
##iterations -> number of iterations to try for generating the model points, default is 15 iterations
##initial_iterations -> number of iterations to try initially without increasing the sample size, default is 10 iterations, should be lower than total iterations 
##iteration_sample_increase -> factor with which the sample size is to be increased per iteration after initial iterations are complete without generating the model points
generate_model_points <- function(data_set = data.frame(), columns = c(), sample_size = 10, iterations = 15, initial_iterations = 10, iteration_sample_increase = 0.05){
  col.levels = create_levels(data_set, columns)
  
  if(iterations < initial_iterations){
    stop("Initial iterations more than total iterations")
  }
  
  if(sample_size < 1){
    stop("Invalid sample size")
  }
  
  if(initial_iterations < 1){
    stop("Invalid parameter value: initial_iterations")
  }
  
  if(iteration_sample_increase < 0){
    stop("Invalid parameter value: iteration_sample_increase")
  }
  
  for(i in c(1:iterations)){
    if(i <= initial_iterations){
      mp = data_set[sample(nrow(data_set), sample_size), ]
      if(check_levels(mp, col.levels) == T){
        return(mp)
      }
    }else{
      mp = data_set[sample(nrow(data_set), as.integer(sample_size+(iteration_sample_increase*sample_size))), ]
      if(check_levels(mp, col.levels) == T){
        return(mp)
      }
    }
  }
  return(data.frame())
}

check_levels <- function(data_set = data.frame(), col.levels = c()){
  columns = names(col.levels)
  size = length(columns)
  if(size == 0){
    return(F)
  }
  for(i in c(1:size)){
    if(!(identical(sort(levels(as.factor(data_set[[columns[[i]]]]))), sort(levels(as.factor(col.levels[[columns[[i]]]])))) == "TRUE")){
      return(F)
    }
  }
  return(T)
}

create_levels <- function(data_set = data.frame(), columns = c()){
  size = length(columns)
  data.levels = c()
  if(size == 0){
    return(data.levels)
  }
  for(i in c(1:size)){
    lev = levels(as.factor(data_set[[columns[[i]]]]))
    data.levels = append(data.levels, list(lev))
  }
  names(data.levels) = columns
  return(data.levels)
}

#file.type supported - xlsx, csv
read_file <- function(file.path, file.type, ...){
  data = data.frame()
  if(file.type == 'xlsx'){
    data = read.xlsx(file.path, ...)
  }
  else{
    data = read.csv(file.path, ...)
  }
  return(data)
}

#file.type supported - xlsx, csv
write_file <- function(df, file.write, file.write.type, ...){
  if(file.write.type == 'xlsx'){
    write.xlsx(df, file = file.write, ...)
  }
  else{
    write.table(x = df, file = file.write, ...)
  }
}