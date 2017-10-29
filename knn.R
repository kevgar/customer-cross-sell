

### read in tables


routes <-  read.csv('http://kddata.co/data/chapter6/routes.csv', 
                               colClasses = c("RouteID" = "character",
                                              "Region" = "factor",
                                              "WeekOrder" = "factor",
                                              "Day" = "factor"))

products <-  read.csv('http://kddata.co/data/chapter6/products.csv', 
                                 colClasses = c("ProductID" = "character",
                                                "Category" = "factor",
                                                "Family" = "factor",
                                                "Price" = "numeric"))

customers <- read.csv('http://kddata.co/data/chapter6/customers.csv', 
                                  colClasses = c("CustomerID" = "character",
                                                 "RouteID" = "factor",
                                                 "CustomerType" = "factor",
                                                 "ZIP" = "character",
                                                 "SeasonType" = "factor"))

visitdetails <-  read.csv('http://kddata.co/data/chapter6/visitdetails.csv', 
                                     colClasses = c("VisitDetailID" = "character",
                                                    "ProductID" = "character",
                                                    "Quantity" = "integer",
                                                    "VisitID" = "character"))

visits <-  read.csv('http://kddata.co/data/chapter6/visits.csv', 
                               colClasses = c("VisitID" = "character",
                                              "CustomerID" = "character",
                                              "SalesRepresentativeID" = "character",
                                              "VisitDate" = "character",
                                              "Amount" = "numeric",
                                              "PaymentTerm" = "factor",
                                              "Outcome" = "factor"))

visits$Date <- as.Date(visits$VisitDate, "%d-%b-%y")  
visits$Hour <- substring(visits$VisitDate,11,12)
visits$TimeIndicator <- substring(visits$VisitDate,30,31)

### merge customers and routes
customers_routes <- merge(customers, routes, by = "RouteID", all.x = TRUE)

### merge visits and visit details
visits_visitdetails <- merge(visitdetails, visits, by = "VisitID", all.x = TRUE)

### Only revenue generating products
revenue_products <- products[products$Price > 0,]

### time window
t1 <- min(visits$Date) 
t4 <- max(visits$Date)
t3 <- t4-180 #t4-425
t2 <- t3-1

### Select customers with purchases in DEP
customers_purchased <- unique(visits_visitdetails[visits_visitdetails$Date >= t3 & visits_visitdetails$Date <= t4, 
                                          "CustomerID"])

### Select data for customers with purchase
visits_visitdetails <- visits_visitdetails[visits_visitdetails$CustomerID %in% customers_purchased &
                             visits_visitdetails$ProductID %in% revenue_products$ProductID,]

### DEP period
visits_visitdetails_DEP <- visits_visitdetails[visits_visitdetails$Date >= t3 & visits_visitdetails$Date <= t4,]

### IND period 
visits_visitdetails_IND <- visits_visitdetails[visits_visitdetails$Date >= t1 & visits_visitdetails$Date <= t2,]

### num customers in DEP period
length(unique(visits_visitdetails_DEP$CustomerID))

### DEP variable
visits_visitdetails_DEP$Response <- numeric(nrow(visits_visitdetails_DEP))
for(i in 1:length(unique(visits_visitdetails$ProductID))){
  visits_visitdetails_DEP$Response <- ifelse(visits_visitdetails_DEP$ProductID == unique(visits_visitdetails$ProductID)[i],
                                     i,
                                     visits_visitdetails_DEP$Response)
}

### Find most recent purchase date by customer in DEP period
recent_purchase_date <- aggregate(visits_visitdetails_DEP$Date, 
                                  by = list("CustomerID" = visits_visitdetails_DEP$CustomerID), 
                                  min)
colnames(recent_purchase_date)[2] <- "Date"

### Merge in product most recently purchased
recent_response <- merge(recent_purchase_date, 
                         visits_visitdetails_DEP[,c("CustomerID","Date","Response","Amount")], 
                         all.x = TRUE)

### take top selling item for each customer
recent_response <- recent_response[order(recent_response$Amount, decreasing = TRUE),]
recent_response <- aggregate(recent_response$Response, 
                             by = list("CustomerID" = recent_response$CustomerID),
                             head, n = 1)
colnames(recent_response)[2] <- "Response"

### make variables for predictors

### recency
recency <- aggregate(visits_visitdetails_IND$Date, 
                     by = list("CustomerID" = visits_visitdetails_IND$CustomerID),
                     max)
recency$Recency <- as.numeric(t2 - recency$x)
recency$x <- NULL

### number of products
num_products <- aggregate(visits_visitdetails_IND$Quantity, 
                          by = list("CustomerID" = visits_visitdetails_IND$CustomerID),
                          sum)
colnames(num_products)[2] <- "Num_products"

### number of visits
num_visits <- aggregate(visits_visitdetails_IND$VisitID, 
                        by = list("CustomerID" = visits_visitdetails_IND$CustomerID),
                        length)
colnames(num_visits)[2] <- "Num_visits"

### average amount per visit
avg_amount <- aggregate(visits_visitdetails_IND$Amount, 
                        by = list("CustomerID" = visits_visitdetails_IND$CustomerID),
                        mean)
colnames(avg_amount)[2] <- "Avg_amount"
avg_amount$Avg_amount <- round(avg_amount$Avg_amount, 2)

### make list and reduce
data <- list(recency,
             num_products,
             num_visits,
             avg_amount)

basetable <- Reduce(function(x,y) merge(x,y,by="CustomerID"), data)

### merge in customer and route info
basetable <- merge(basetable, customers_routes, by = "CustomerID", all.x = TRUE)
basetable$ZIP <- factor(basetable$ZIP)

### make dummies
library(dummies)
basetable_dummy <- dummy.data.frame(basetable[,-1])
basetable <- cbind(basetable,basetable_dummy)
basetable$RouteID <- basetable$CustomerType <- basetable$ZIP <- basetable$SeasonType <- 
  basetable$Region <- basetable$WeekOrder <- basetable$Day <- NULL

######### make product dummies
library(reshape2)
test <- visits_visitdetails_IND[,c("CustomerID","ProductID","Quantity")]
test_melt <- melt(test, id.vars = c("CustomerID","ProductID"))
test_cast <- acast(test_melt, CustomerID ~ ProductID, sum)
test_cast <- as.data.frame(test_cast)
test_cast$CustomerID <- rownames(test_cast)

basetable <- merge(basetable, test_cast, by = "CustomerID", 
                   all.x = TRUE, suffixes = '')[,!duplicated(names(basetable))]
#########

### bring in response
basetable <- merge(recent_response,basetable, by = "CustomerID", all.y = TRUE)
basetable <- basetable[!is.na(basetable$Response),]
### get total number purchased for each product
num_responses <- aggregate(basetable$Response, by = list("Response" = basetable$Response), length)
### get products that sold at least 5 products
products <- head(num_responses$Response[order(num_responses$x,decreasing = TRUE)],40)
basetable <- basetable[basetable$Response %in% products,]
basetable$Response <- factor(basetable$Response)

# clean up the environment
rm(list=ls()[!ls() %in% c('basetable','visits_visitdetails')])

### test/train split
customer_ids <- basetable$CustomerID
y <- basetable$Response
# Remove unnecesary columns
basetable$CustomerID <- NULL
basetable$Response <- NULL

############# randomForest #############
# Generate 50/50 train and test indicies
set.seed(0)
ind <- 1:nrow(basetable)
indTrain <- sample(ind, ceiling(length(ind)/2))
indTest <- ind[-indTrain]
# Fit random forest on the train data
library(randomForest)
rf_model <- randomForest(x = basetable[indTrain,],
                         y = y[indTrain],
                         ntree = 1000)
# Predict labels on the test data
rf_pred <- predict(rf_model, newdata = basetable[indTest,], type = c("response"))
### accuracy
sum(diag(table(rf_pred,y[indTest])))/length(y[indTest]) # [1] 0.3042973

############# kNN Model #############
# Generate 33/33/33 train,test and validation indicies
set.seed(7)
indTrain <- sample(ind, ceiling(length(ind)/3))
indVal <- sample(ind[-indTrain], ceiling(length(ind)/3))
indTest <- ind[-c(indTrain, indVal)]

basetable_KNN <- data.frame(sapply(basetable, function(x) as.numeric(as.character(x))))

# basetabletrain <- basetable_KNN[trainind,]
# basetableval <- basetable_KNN[valind,]
# basetabletest <- basetable_KNN[testind,]
# basetabletrainbig <- rbind(basetabletrain,basetableval)
# ytrain <- y[trainind]
# yval <- y[valind]
# ytest <- y[testind]
# ytrainbig <- factor(y[c(indTrain,indVal)])

stdev <- sapply(basetable_KNN[indTrain,1:4],sd)
means <- sapply(basetable_KNN[indTrain,1:4],mean)
basetable_KNN[c(indTrain,indVal),1:4] <- 
    data.frame(t((t(basetable_KNN[c(indTrain,indVal),1:4])-means)/stdev))
basetable_KNN[indTrain,1:4] <- 
    data.frame(t((t(basetable_KNN[indTrain,1:4])-means)/stdev))
basetable_KNN[indVal,1:4] <- 
    data.frame(t((t(basetable_KNN[indVal,1:4])-means)/stdev))
basetable_KNN[indTest,1:4] <- 
    data.frame(t((t(basetable_KNN[indTest,1:4])-means)/stdev))

# #### scaling products 
# minima <- sapply(basetabletrain[,82:ncol(basetabletrain)],min)
# scaling <- sapply(basetabletrain[,82:ncol(basetabletrain)],max)-minima
# 
# ### scale train data
# basetabletrain <- data.frame(basetabletrain[,1:81],
#                                    base::scale(basetabletrain[,82:ncol(basetabletrain)],
#                                                center=minima,
#                                                scale=scaling))
# basetabletrain[sapply(basetabletrain, is.nan)] <- 0
# basetabletrain[sapply(basetabletrain, is.infinite)] <- 1
# ### scale trainbig data
# basetabletrainbig <- data.frame(basetabletrainbig[,1:81],
#                              base::scale(basetabletrainbig[,82:ncol(basetabletrainbig)],
#                                          center=minima,
#                                          scale=scaling))
# basetabletrainbig[sapply(basetabletrainbig, is.nan)] <- 0
# basetabletrainbig[sapply(basetabletrainbig, is.infinite)] <- 1
# 
# ### scale val data
# basetableval <- data.frame(basetableval[,1:81],
#                            base::scale(basetableval[,82:ncol(basetableval)],
#                                                   center=minima,
#                                                   scale=scaling))
# basetableval[sapply(basetableval, is.nan)] <- 0
# basetableval[sapply(basetableval, is.infinite)] <- 1
# 
# #### scale test data
# basetabletest <- data.frame(basetabletest[,1:81],
#                            base::scale(basetabletest[,82:ncol(basetabletest)],
#                                        center=minima,
#                                        scale=scaling))
# basetabletest[sapply(basetabletest, is.nan)] <- 0
# basetabletest[sapply(basetabletest, is.infinite)] <- 1

# 
# accuracy <- numeric(200)
# for(k in 1:200){
#   #retrieve the indicators of the k nearest neighbors of the query data 
#   indicatorsKNN <- as.integer(knnx.index(data=basetabletrain,
#                                          query=basetableval,
#                                          k=k))
#   #retrieve the actual y from the training set
#   predKNN <- as.integer(as.character(ytrain[indicatorsKNN])) 
#   predKNN <- data.frame(matrix(data=predKNN,
#                                ncol=k, 
#                                nrow=nrow(basetableval)))
#   predictions <- sapply(1:nrow(predKNN), function(x)
#     as.numeric(names(sort(table(as.numeric(predKNN[x,])),decreasing = TRUE)[1])))
#   # AUC::auc(roc(predictions,ytest))
#   table_pred <- table(predictions,yval)                                      
#   accuracy[k] <- sum(diag(table_pred))/length(predictions)
# }
# 
# plot(accuracy)
# (k <- which.max(accuracy))
# max(accuracy)

k <- 50
### work on proportions
library(e1071)
library(class)

indicatorsKNN <- as.integer(knn(train=basetable_KNN[indTrain,],
                                test=basetable_KNN[indVal,],
                                cl=y[indTrain],
                                k=k))
#retrieve the actual y from the training set
predKNN <- as.integer(as.character(y[indTrain][indicatorsKNN])) 
predKNN <- data.frame(matrix(data=predKNN,
                             ncol=k, 
                             nrow=nrow(basetable_KNN[indVal,])))

predictions <- sapply(1:nrow(predKNN), function(x)
  as.numeric(names(sort(table(as.numeric(predKNN[x,])),decreasing = TRUE)[1])))
proportion <- sapply(1:nrow(predKNN), function(x)
  as.numeric(sort(table(as.numeric(predKNN[x,])),decreasing = TRUE)[1])) / k
# AUC::auc(roc(predictions,ytest))
table_pred <- table(predictions,y[indVal])                                      
accuracy_test <- sum(diag(table_pred))/length(predictions)
customer_item_purch <- data.frame("CustomerID" = customer_ids[indVal],
                                  "NextPurch" = predictions,
                                  "Probability" = proportion)

customer_item_purch <- customer_item_purch[order(proportion, decreasing = TRUE),]
customer_item_purch$NextPurch <- unique(visits_visitdetails$ProductID)[customer_item_purch$NextPurch]
