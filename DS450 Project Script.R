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

# Load dataset into R
load_recipe_data <- function(){
  setwd("C:/Users/user/Documents/Data Science/DATASCI450/Project")
  #setwd("C:\\Users\\Me\\Documents\\Github\\Datasci450_recipe")
  json_file <- 'train.json'
  data <- fromJSON(paste(getwd(),json_file, sep = "/"))
  
  return( data)
}

#### Break up data into test and train sets - code adapted from https://ufal.mff.cuni.cz/mlnlpr13
split_train_test <- function(data, train_ratio = 0.8) {

  set.seed(100)
  randnums = runif(nrow(data), 0,1)
  
  data_train = data[randnums < .2,]
  data_test = data[randnums >= .2,]

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
binarize_ingredients <- function(data, num_ingred = 100) {
  ingredients_table <- data.table(count_ingredients, key = "X1")
  
  # Create features for popular ingredients only
  popular_ingred <- tail(ingredients_table$ingredients, num_ingred)
  
  binarized_matrix <- sapply(data$ingredients, function(x) popular_ingred %in% x)
  binarized_df <- as.data.frame(t(binarized_matrix))
  
  binarized_df <- binarized_df + 0  # to convert from logical to binary
  
  data2 <- cbind(data[,2], binarized_df, data$num_ingred)
  data2 <- data.frame(data2)
  names(data2[1]) <- "cuisine"
  
  names(data2) <- c("cuisine", popular_ingred, "num_ingred")
  
  return(data2)
}

#### Basic data investigation
# View(data)  # to look at dataset
data <- load_recipe_data()
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
data$num_ingred <- sapply(data$ingredients, length)

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


#### NLP - working on ingredients lists
# Create ingredients lists - master list and raw list
ingredients_master <- unique(unlist(data$ingredients))
length(ingredients_master)  # 6714 unique ingredients
ingredients <- unlist(data$ingredients)


##### Ingredient counts - how many times do we see each ingredient in the dataset
# Create counts of ingredients
dataframe <- data.frame(ingredients, 1, stringsAsFactors = FALSE)
count_ingredients <- aggregate(X1 ~ ingredients, data = dataframe, FUN = sum)

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


#### Create features for ingredients
# Commented out for now... can use the binarize function above.
# data$ingredients_char <- sapply(data$ingredients, function(x) paste(x, sep = ",", collapse = ", "))
# ingred_features <- concat.split.expanded(data, "ingredients_char", fill = 0, drop = TRUE, type = "character")

train_test_split <- split_train_test(data)
data_train <- train_test_split$train
data_test <- train_test_split$test

#### Test out some decision tree models
# Basic model using only id as predictor
M1 <- rpart(cuisine ~ id, data = data_train, method = "class")
P1 <- predict(M1, data_test, type = "class")
print(table(data_test$cuisine, P1))

plot(M1, margin = 0.05)
text(M1)

# Basic model using only number of ingredients as predictor
M2 <- rpart(cuisine ~ num_ingred, data = data_train, method = "class")
P2 <- predict(M2, data_test, type = "class")
print(table(data_test$cuisine, P2))

plot(M2, margin = 0.05)
text(M2)


# for now this model is just on the entire dataset, and tested with itself.
binarized_data <- binarize_ingredients(data, num_ingred = 100)

M3 <- rpart(cuisine ~ ., data = binarized_data, method =  "class")  # Can use a model frame and subset in this formula.
P3 <- predict(M3, binarized_data, type = "class")
print(table(binarized_data$cuisine, P3))

sum(diag(table(binarized_data$cuisine, P3)))/sum(table(binarized_data$cuisine, P3))  # percent correct

plot(M3, margin = 0.05)
text(M3)


### Naive Bayes
binarized_data$cuisine <- as.factor(binarized_data$cuisine)
M4 <- naiveBayes(cuisine ~ ., data = binarized_data)   
P4 <- predict(M4, binarized_data[, -1, drop = FALSE])  
print(table(binarized_data$cuisine, P4))

sum(diag(table(binarized_data$cuisine, P4)))/sum(table(binarized_data$cuisine, P4))  # percent correct


### kNN
# binarized_data$cuisine <- as.character(binarized_data$cuisine)
# M5 <- knn(train = binarized_data[,-1], test = binarized_data[,-1], cl = binarized_data$cuisine, k=3)
