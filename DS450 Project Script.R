
#######################################
#                                     #
#           DS450 Project             #
#                                     #
#  Michael Patterson & Adrian Guerin  #
#                                     #
#######################################


#### Set working directory, load packages, load dataset

# Set working directory
setwd("C:/Users/user/Documents/Data Science/DATASCI450/Project")

# Load required packages
require(jsonlite)
require(data.table)
require(NLP)
require(wordcloud)
require(ggplot2)
require(splitstackshape)
require(rpart)

# Load dataset into R
json_file <- 'train.json'
data <- fromJSON(paste(getwd(),json_file, sep = "/"))

data_original <- data


#### Basic data investigation
# View(data)  # to look at dataset

# Examine data structure
str(data)

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

#### Duplicate recipes  # NOTE: Should we sort first so we also ID duplicates that have different ingredient ordering?
# How many duplicate recipes
sum(duplicated(data$ingredients))  # 100 - meaning 200 entries are duplicates

duplicates1 <- data[duplicated(data$ingredients, fromLast = TRUE),]
duplicates2 <- data[duplicated(data$ingredients, fromLast = FALSE),]

duplicates1$ingredients <- sapply(duplicates1$ingredients, function(x) paste(x, sep = ",", collapse = ", "))
duplicates2$ingredients <- sapply(duplicates2$ingredients, function(x) paste(x, sep = ",", collapse = ", "))

duplicates <- rbind(duplicates1, duplicates2)

duplicates <- duplicates[order(duplicates$ingredients),]
# View(duplicates)  # Most are true duplicates... (with dif. ids), but some, like "butter", have two cuisine types.


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
ggplot(data, aes(x=num_ingred, fill=cuisine)) + geom_density()

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
ingredients_table$X2 <- round(ingredients_table$X1/nrow(data)*100, 2)  # Calculate percentage frequency

tail(ingredients_table, 30)  # Salt in 45% of recipes, top 20 in 11 - 20%, the rest below 10%


#### Create features for ingredients
data$ingredients_char <- sapply(data$ingredients, function(x) paste(x, sep = ",", collapse = ", "))
ingred_features <- concat.split.expanded(data, "ingredients_char", fill = 0, drop = TRUE, type = "character")



#### Break up data into test and train sets - code adapted from https://ufal.mff.cuni.cz/mlnlpr13

# Code in number of rows for data, test, train
nrows_data <- nrow(data)
nrows_train <- round(0.9*nrow(data))
nrows_test <- nrows_data - nrows_train

# Check to make sure we have the right number of rows for test, train, and data.
nrows_data
nrows_train
nrows_test
nrows_data == nrows_train + nrows_test

set.seed(100)
random_nums <- sample(nrow(data)) 

# Create training dataset
train_rows <- random_nums[1:nrows_train]
data_train <- data[train_rows,]

# Create test dataset 
test_rows <- random_nums[(nrows_train+1):nrows_data]
data_test <- data[test_rows,]


## Quicker subsetting
# set.seed(100)
# randnums = runif(nrow(data), 0,1)
# data.train = authorship[randnums < .2,]
# data.test = authorship[randnums >= .2,]

#### Test out some decision tree models
# Basic model using only id as predictor
M1 <- rpart(cuisine ~ id, data = data_train, method = "class")
P1 <- predict(M1, data_test, type = "class")
print(table(data_test$cuisine, P1))

# Basic model using only number of ingredients as predictor
M2 <- rpart(cuisine ~ num_ingred, data = data_train, method = "class")
P2 <- predict(M2, data_test, type = "class")
print(table(data_test$cuisine, P2))


# Create features for popular ingredients only
popular_ingred <- tail(ingredients_table$ingredients, 30)
ingredient_features <- data.frame(matrix(ncol = length(popular_ingred), nrow = nrow(data)))

for (i in 1:length(popular_ingred)) {
  
  ingredient_features[,i] <- sapply(data$ingredients_char,
                                    function (x) grepl(popular_ingred[i], x))
  
}

data2 <- cbind(data[,2], ingredient_features, data$num_ingred)
names(data2[1]) <- "cuisine"

names(data2) <- c("cuisine", popular_ingred, "num_ingred")

# for now this model is just on the entire dataset, and tested with itself.
M3 <- rpart(cuisine ~ ., data = data2, method =  "class")  # Can use a model frame and subset in this formula.
P3 <- predict(M3, data2, type = "class")
print(table(data2$cuisine, P3))

plot(M3, margin = 0.05)
text(M3)
