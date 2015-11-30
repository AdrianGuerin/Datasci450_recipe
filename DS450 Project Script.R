#######################################
#                                     #
#           DS450 Project             #
#                                     #
#  Michael Patterson & Adrian Guerin  #
#                                     #
#######################################


#### Set working directory, load packages, load dataset

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Set working directory

# Load required packages
require(jsonlite)
require(data.table)
require(NLP)
require(wordcloud)
require(ggplot2)
require(splitstackshape)
require(rpart)
require(e1071)
require(class)
require(caret)
require(randomForest)

# Load dataset into R
load_recipe_data <- function(){
  setwd("C:/Users/Adrian/Documents/DS450/Project")
  #setwd("C:\\Users\\Me\\Documents\\Github\\Datasci450_recipe")
  json_file <- 'train.json'
  data <- fromJSON(paste(getwd(),json_file, sep = "/"))
  
  # calculate number of ingredients per recipe
  data$num_ingred <- sapply(data$ingredients, length)
  
  return( data)
}

#### NLP - working on ingredients lists
# Create ingredients lists - master list and raw list
calc_ingredients <- function(data) {
  
  ingredients <- unlist(data$ingredients)
  
  # clean data
  ingredients <- tolower(ingredients)
  ingredients <- gsub("_", " ", ingredients)
  ingredients <- gsub("-", " ", ingredients)
  ingredients <- gsub(",", " ", ingredients)
  ingredients <- gsub("\\.", " ", ingredients)
  ingredients <- gsub("&", "and", ingredients)
  ingredients <- gsub("reduced", "low", ingredients)
  
  ingredients_master <- unique(unlist(data$ingredients))
  length(ingredients_master)  # 6714 unique ingredients
  ##### Ingredient counts - how many times do we see each ingredient in the dataset
  # Create counts of ingredients
  dataframe <- data.frame(ingredients, 1, stringsAsFactors = FALSE)
  count_ingredients <- aggregate(X1 ~ ingredients, data = dataframe, FUN = sum)
  
  return(count_ingredients)
}

# Plot the ingredients
explore_ingredients <- function(count_ingredients) {
  # Some basic summary stats for ingredient counts
  summary(count_ingredients$X1)
  boxplot(count_ingredients$X1)  # not very useful
  hist(count_ingredients$X1)  # not very useful
  sd(count_ingredients$X1)
  
  # Trim the ingredients count to exclude 'outliers' (these are the very common ingredients, which may actually be quite useful to us)
  count_ingredients_trim <- count_ingredients[
    count_ingredients$X1 < (mean(count_ingredients$X1)+1*sd(count_ingredients$X1)),]
  
  boxplot(count_ingredients_trim$X1)
  hist(count_ingredients_trim$X1)  # looks like a power law distribution
  
  # Trim some more
  count_ingredients_trim2 <- count_ingredients[count_ingredients$X1 < 100,]
  
  boxplot(count_ingredients_trim2$X1)
  hist(count_ingredients_trim2$X1)  # looks very much like a power law distribution
  
  
  #### What are the most common ingredients? Create wordcloud
  # Order ingredients by least to most popular
  ingredients_table <- data.table(count_ingredients, key = "X1")
  
  # Create wordcloude with 50 most common
  index <- c((length(ingredients_table$X1)-49):length(ingredients_table$X1))
  wordcloud(ingredients_table$ingredients[index], ingredients_table$X1[index])
  
  # Print out 50 most common ingredients
  ingredients_table[index,]
  
  # In what proportion of recipes do we see these ingredients (assumed not in the same recipe more than once)
  df_ingredients <- as.data.frame(ingredients_table)
  ingredients_table$X2 <- round(ingredients_table$X1/nrow(data)*100, 1)  # Calculate percentage frequency
  
  tail(ingredients_table, 30)  # Salt in 45% of recipes, top 20 in 11 - 20%, the rest below 10%
  
}

#### Break up data into test and train sets - code adapted from https://ufal.mff.cuni.cz/mlnlpr13
split_train_test <- function(data, train_ratio = 0.2) {

  set.seed(100)
  randnums = runif(nrow(data), 0, 1)
  
  data_train = data[randnums < train_ratio,]
  data_test = data[randnums >= train_ratio,]

  return(list('train' = data_train, 'test' = data_test))
}

#### Duplicate recipes  # NOTE: Should we sort first so we also ID duplicates that have different ingredient ordering?
# How many duplicate recipes
find_duplicates <- function(data) {
  sum(duplicated(data$ingredients))  # 100 - meaning 200 entries are duplicates
  
  duplicates1 <- data[duplicated(data$ingredients, fromLast = TRUE),]
  duplicates2 <- data[duplicated(data$ingredients, fromLast = FALSE),]
  
  duplicates1$ingredients <- sapply(duplicates1$ingredients, function(x) paste(x, sep = ",", collapse = ", "))
  duplicates2$ingredients <- sapply(duplicates2$ingredients, function(x) paste(x, sep = ",", collapse = ", "))
  
  duplicates <- rbind(duplicates1, duplicates2)
  
  duplicates <- duplicates[order(duplicates$ingredients),]
  # View(duplicates)  # Most are true duplicates... (with dif. ids), but some, like "butter", have two cuisine types.
  return(duplicates)
}  

### Create binary indicator matrix for presence of ingredient in a recipe
binarize_ingredients <- function(data, count_ingredients, how_many_ingred = 100) {
  # this function returns top X ingredients, where X is how_many_ingred
  ingredients_table <- data.table(count_ingredients, key = "X1")
  
  # Create features for popular ingredients only
  popular_ingred <- tail(ingredients_table$ingredients, how_many_ingred)
  
  binarized_matrix <- sapply(data$ingredients, function(x) popular_ingred %in% x)
  binarized_df <- as.data.frame(t(binarized_matrix))
  
  binarized_df <- binarized_df + 0  # to convert from logical to binary
  
  data2 <- cbind(data[,2], binarized_df, data$num_ingred)
  data2 <- data.frame(data2)
  names(data2[1]) <- "cuisine"
  
  names(data2) <- c("cuisine", popular_ingred, "num_ingred")
  
  return(data2)
}

### Create binary indicator matrix of 50, 200, 1000

binarize_datasets <- function(data) {
  binarized_data50 <- binarize_ingredients(data, count_ingredients, 50)
  binarized_data1000 <- binarize_ingredients(data, count_ingredients, 1000)
  num_ingred <- length(count_ingredients$ingredients)
  binarized_data_all <- binarize_ingredients(data_train, count_ingredients, num_ingred)
  
  return(list('bd50' = binarized_data50, 'bd1000' = binarized_data1000, 'bd_all' = binarized_data_all))
}


# create graphs of ingredient counts
# also calculates num_ingredients in data, so returns processed data
explore_data <- function(data){
  # Examine data structure
  str(data)
  
  # Check cuisine types
  unique(data$cuisine)
  
  # How many unique cuisines?
  length(unique(data$cuisine))
  
  
  #### Recipes and cuisine types
  # How many recipes per cusine type?
  cuisinetable <- table(data$cuisine)
  
  cuisinetable
  plot(cuisinetable, las = 2)
  
  # Sort cuisines by most recipes
  cuisinetable <- as.table(cuisinetable[order(cuisinetable, decreasing = TRUE)])
  
  cuisinetable
  plot(cuisinetable, las = 2)
  
  # And in percentage terms...
  cuisinetable_perc <- round(cuisinetable/nrow(data)*100, 2)
  
  
  #### Recipes and ingredients
  # How many ingredients per recipe?
  
  summary(data$num_ingred)
  boxplot(data$num_ingred)
  hist(data$num_ingred, breaks = 40)
  hist(data$num_ingred[data$num_ingred < 40], breaks = 40)  # see distributin better when cutting off right tail
  
  # Number of recipes with certain number of ingredients for each cuisine type (large table)
  subset <- data.frame(data$cuisine, data$num_ingred)
  table(subset)
  
  # Check recipes with only 1 or 2 ingredients
  data[data$num_ingred == 1,]  # Maybe some of these would help predict... some not (butter, water)
  data[data$num_ingred == 2,]
  
  
  #### Ingredients and cuisine types
  # Create density plots for number of ingredients per recipe by cuisine type
  ggplot(data, aes(x=num_ingred, colour=cuisine)) + geom_density()  # Looks like quite a bit of overlap, but certainly differences.
  #ggplot(data, aes(x=num_ingred, fill=cuisine)) + geom_density()
  
  # Average number of ingredients for recipes by cuisine type
  avg_ingred_cuisine <- aggregate(num_ingred ~ cuisine, data = data, FUN = mean)
  avg_ingred_cuisine$num_ingred <- round(avg_ingred_cuisine$num_ingred, 2)  # Rounded to two decimal places
  
}

# run the prediction tree algorithms and plot them
predict_trees <- function(data) {
  
  binarized_data50 <- binarized_data$bd50
  binarized_data1000 <- binarized_data$bd1000
  binarized_data_all <- binarized_data$bd_all
  
  naive_model50 <- rpart(as.factor(cuisine) ~ ., data = binarized_data50)   
  naive_model1000 <- rpart(as.factor(cuisine) ~ ., data = binarized_data1000)
  naive_model_all <- rpart(as.factor(cuisine) ~ ., data = binarized_data_all)
  
  dt.pred50 <- predict(naive_model50, binarized_data50, type = "class")
  dt.pred1000 <- predict(naive_model1000, binarized_data1000, type = "class")
  dt.pred_all <- predict(naive_model_all, binarized_data_all, type = "class")
  
  confusionMatrix(dt.pred50, binarized_data50$cuisine)
  confusionMatrix(dt.pred1000, binarized_data1000$cuisine)
  confusionMatrix(dt.pred_all, binarized_data_all$cuisine)
  
  plot(naive_model50, margin = 0)
  text(naive_model50)
  
  plot(naive_model1000, margin = 0)
  text(naive_model1000)
  
  plot(naive_model_all, margin = 0)
  text(naive_model_all)
  
  return(list('dt50' = dt.pred50, 'dt1000' = dt.pred1000, 'dt_all' = dt.pred_all))
}

# run the prediction tree algorithms and plot them
predict_forest <- function(data) {
  
  binarized_data50 <- binarized_data$bd50
  binarized_data1000 <- binarized_data$bd1000
  
  # Some required data cleaning for RF to run properly...
  names_bd50 <- names(binarized_data50)
  names_bd50 <- gsub(" ", "_", names_bd50)
  names(binarized_data50) <- names_bd50
  
  names_bd1000 <- names(binarized_data1000)
  names_bd1000 <- gsub(" ", "_", names_bd1000)
  names_bd1000 <- gsub("2%", "twop", names_bd1000)
  names_bd1000 <- gsub("1%", "onep", names_bd1000)
  names(binarized_data1000) <- names_bd1000

  naive_model50 <- randomForest(as.factor(cuisine) ~ ., data = binarized_data50, importance = TRUE, ntree = 100)   
  
  #naive_model1000 <- randomForest(as.factor(cuisine) ~ ., data = binarized_data1000, importance = TRUE, ntree = 100, nodesize = 5)
  naive_model1000 <- randomForest(as.factor(cuisine) ~ ., data = binarized_data1000, importance = TRUE, ntree = 50, nodesize = 5)
 
  rf.pred50 <- predict(naive_model50, binarized_data50)
  rf.pred1000 <- predict(naive_model1000, binarized_data1000)
  
  confusionMatrix(rf.pred50, binarized_data50$cuisine)
  confusionMatrix(rf.pred1000, binarized_data1000$cuisine)
  
  plot(naive_model50)
  plot(naive_model1000)
  
  return(list('rf50' = rf.pred50, 'rf1000' = rf.pred1000))
}

# predict the cuisine using SVM; this takes >10 minutes on my computer, for the 1000 ingredient matrix
predict_svm <- function(data) {

  binarized_data50 <- binarized_data$bd50
  binarized_data1000 <- binarized_data$bd1000
  
  naive_model50 <- svm(cuisine ~ ., data = binarized_data50)
  naive_model1000 <- svm(cuisine ~ ., data = binarized_data1000)
  
  svm.pred50 <- predict(naive_model50, binarized_data50)
  svm.pred1000 <- predict(naive_model1000, binarized_data1000)
  
  confusionMatrix(svm.pred50, binarized_data50$cuisine)
  confusionMatrix(svm.pred1000, binarized_data1000$cuisine)
  
  return(list('svm50' = svm.pred50, 'svm1000' = svm.pred1000))
}

# Naive Bayes prediction
predict_bayes <- function(data) {

  binarized_data50 <- binarized_data$bd50
  binarized_data1000 <- binarized_data$bd1000
  
  naive_model50 <- naiveBayes(as.factor(cuisine) ~ ., data = binarized_data50)   
  naive_model1000 <- naiveBayes(as.factor(cuisine) ~ ., data = binarized_data1000)   
  
  nb.pred50 <- predict(naive_model50, binarized_data50)
  nb.pred1000 <- predict(naive_model1000, binarized_data1000)
  
  confusionMatrix(nb.pred50, binarized_data50$cuisine)
  confusionMatrix(nb.pred1000, binarized_data1000$cuisine)
  
  return(list('nb50' = nb.pred50, 'nb1000' = nb.pred1000))
}

#### Basic data investigation
# View(data)  # to look at dataset
if(interactive()) {
  # load data from file, and calc count_ingredients
  data <- load_recipe_data()
  count_ingredients <- calc_ingredients(data)
  
  # explore data
  #data <- explore_data(data)  
  #explore_ingredients(count_ingredients)
  
  # split data into train and test
  train_test_split <- split_train_test(data)
  data_train <- train_test_split$train
  data_test <- train_test_split$test
  
  # binarize the most common 50, 1000, all (6676) ingredients
  binarized_data <- binarize_datasets(data_train)
  
  ### Decision tree predictions
  tree_models <- predict_trees(binarized_data)
  
  ### Random Forest predictions
  forest_models <- predict_forest(binarized_data)
  
  ### SVM predictions
  svm_models <- predict_svm(binarized_data)
  
  ### Naive Bayes
  bayes_models <- predict_bayes(binarized_data)
  
  ### kNN
  # binarized_data$cuisine <- as.character(binarized_data$cuisine)
  # M5 <- knn(train = binarized_data[,-1], test = binarized_data[,-1], cl = binarized_data$cuisine, k=3)
}
