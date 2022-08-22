#test 1 2
install.packages("DBI")
install.packages("odbc")
install.packages("dplyr")
install.packages("leaps")
install.packages("MASS")
library(DBI)
library(odbc)
library(dplyr)
library(MASS)
library(corrplot)
library(odbc)
library(dplyr)
library(corrplot)
library(leaps)
con <- dbConnect(odbc(), driver = "SqL server",
                 server = "SHAYFASH\\MSSQLSERVER02", 
                 Database = "HomePrediction", port = 1433)
dbListTables(con) #Tables in DB
dbListFields(con, "train")

#load the dataset
traindata <- tbl(con, "train") #convert movies data set into a dataframe (table - tbl)
traindata <- collect(traindata)
View(traindata)

#collect only numeric columns
num = unlist(lapply(traindata, is.numeric), use.names = FALSE) #extract tables with numeric values into a list
corrplot(cor(traindata[num]), method = 'number') #tl.cex = 0.5 in case you need to resize the plots pane


dfn <- traindata[num]
View(dfn)

# Data Cleaning
cleaned_dfn<- na.omit(dfn) # drop NA

View(cleaned_dfn)

# full multiple regression
full_model <- lm(cleaned_dfn$SalePrice ~., data = cleaned_dfn)
summary(full_model)

# step wise regression
step_model <- stepAIC(full_model, direction = "both", 
                      trace = FALSE)
summary(step_model)

#load the dataset
testdata <- tbl(con, "test") #convert movies data set into a dataframe (table - tbl)
testdata <- collect(testdata)
View(testdata)

#collect only numeric columns
numtest = unlist(lapply(testdata, is.numeric), use.names = FALSE) #extract tables with numeric values into a list
corrplot(cor(testdata[numtest]), method = 'number') #tl.cex = 0.5 in case you need to resize the plots pane


dfnt <- testdata[numtest]
View(dfnt)

# Data Cleaning
cleaned_dfnt<- na.omit(dfnt) # drop NA

View(cleaned_dfnt)

predicted_sales <- predict(step_model,cleaned_dfnt)
View(predicted_sales)
write.csv(predicted_sales, file = 'prediction_sales.csv',row.names = FALSE)
