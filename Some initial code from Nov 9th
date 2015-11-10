setwd("C:/Users/Adrian/Documents/DS450/Project")

require("jsonlite")
install.packages("jsonlite")

json_file <- 'train.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# How many cuisine types are there?
length(table(json_data$cuisine))  # 20 cuisine types

# Show count of each cuisine type
table(json_data$cuisine)

str(json_data)

ingredients_list <- unique(unlist(json_data$ingredients))

write.csv(ingredients_list, file = "ingredients.csv")
