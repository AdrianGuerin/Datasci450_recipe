
#######################################
#                                     #
#           DS450 Project             #
#                                     #
#  Michael Patterson & Adrian Guerin  #
#                                     #
#######################################


#### Set working directory, load packages, load dataset

# Set working directory
setwd("C:\\Users\\Me\\Documents\\GitHub\\Datasci450_recipe")

# Load required packages
require(jsonlite)
require(data.table)
require(NLP)
require(wordcloud)
require(ggplot2)

# Load dataset into R
json_file <- 'train.json'
data <- fromJSON(paste(getwd(),json_file, sep = "/"))


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
hist(log(count_ingredients$X1))

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
