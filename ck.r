library(stringr)
library(tm)
library(h2o)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(jsonlite)
train <- fromJSON('train.json')
test <- fromJSON('test.json')
ingre <- list()
for(i in 1:length(train$ingredients)){
  train$ingredients[i] <- str_c(unlist(train$ingredients[i]),sep=" ",collapse=' ')
  train$ingredients[i] <- gsub(pattern="[^A-Za-z]",replacement=" ",train$ingredients[i])
  train$ingredients[i] <- str_split(train$ingredients[i], " +", n = Inf)
}

for(i in 1:length(test$ingredients)){
  test$ingredients[i] <- str_c(unlist(test$ingredients[i]),sep=" ",collapse=' ')
  test$ingredients[i] <- gsub(pattern="[^A-Za-z]",replacement=" ",test$ingredients[i])
  test$ingredients[i] <- str_split(test$ingredients[i], " +", n = Inf)
}

ingre <- c(train$ingredients,test$ingredients)
train_ingredients <- Corpus(VectorSource(ingre))
train_ingredients <- tm_map(train_ingredients, stemDocument)
train_ingredientsDTM <- DocumentTermMatrix(train_ingredients)
#inspact(train_ingredientDTM)
train_sparse <- removeSparseTerms(train_ingredientsDTM, 0.99)
train_ingredientsDTM <- as.data.frame(as.matrix(train_sparse))
train_ingredientsDTM <- as.data.frame(as.matrix(train_ingredientsDTM))

train_buf <- train_ingredientsDTM[1:39774,]

train_buf$cuisine <- as.factor(train$cuisine)
train <- train_buf
test <- train_ingredientsDTM[39775:length(train_ingredientsDTM$all),]
names(train_ingredientsDTM)
train <- train_ingredientsDTM
#cloud server
h2o.init(nthreads = -1)

train.hex <- as.h2o(object = train, destination_frame = 'train.hex')
test.hex <- as.h2o(object = test, destination_frame = 'test.hex')

xVars <- names(train.hex)
xVars <- xVars[1:length(xVars)-1]
yVars <- c('cuisine')
# random forest
rfModel <- h2o.randomForest(x=xVars,y=yVars,
                            training_frame = train.hex,
                            seed = 100,
                            max_depth=20,
                            ntrees = 500,
                            sample_rate = 0.6,
                            model_id = 'rfModel.hex')
# gre boosting 
bmModel <- h2o.gbm(x=xVars,y=yVars,
                    training_frame = train.hex,
                    distribution = "multinomial",
                    seed = 100,
                    ntrees = 500, 
                    max_depth = 20,
                    learn_rate = 0.3,
                    model_id = 'gbmModel.hex')
#print(rfModel)
sample_sub <- read.csv('sample_submission.csv')
rf_submission <- data.frame(id = sample_sub$id, cuisine = as.matrix(h2o.predict(rfModel, test.hex))[,1])
gbm_submission <- data.frame(id = sample_sub$id, cuisine = as.matrix(h2o.predict(gbmModel, test.hex))[,1])
#- write out the submission file
write.csv(rf_submission, file = 'rf_submission.csv', row.names = F)
write.csv(gbm_submission, file = 'gbm_submission.csv', row.names = F)
h2o.shutdown(prompt = F)
#- close connection
# cart part
set.seed(100)
cartModelFit <- rpart(cuisine ~ ., data = train, method = "class")
cart_Predict <- predict(cartModelFit, newdata = test, type = "class")
cart_submission <- data.frame(id = sample_sub$id, cuisine = cart_Predict)
write.csv(cart_submission, file = 'cart_submission.csv', row.names = F)
