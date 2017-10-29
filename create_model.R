create_model <- function(start_ind,end_ind,start_dep,end_dep){
    
    
    # Code to create the basetable
    source('create_basetable.R')
    basetable <- create_basetable(
        start_ind=start_ind,
        end_ind=end_ind,
        start_dep=start_dep,
        end_dep=end_dep)
    
    # Code to create the model
    ### test/train split
    customer_ids <- basetable$CustomerID
    y <- basetable$Response
    # Remove unnecesary columns
    basetable$CustomerID <- NULL
    basetable$Response <- NULL
    
    ############# randomForest #############
    # Generate 50/50 train and test indicies
    set.seed(7)
    ind <- 1:nrow(basetable)
    indTrain <- sample(ind, ceiling(length(ind)/2))
    indTest <- ind[-indTrain]
    # Fit random forest on the train data
    library(randomForest)
    cat('Fitting first model to 50% training subset..\n')
    rf_model <- randomForest(x = basetable[indTrain,],
                             y = y[indTrain],
                             ntree = 1000)
    # Predict labels on the test data
    cat('Evaluating first model on 50% test subset..\n')
    rf_pred <- predict(rf_model, newdata = basetable[indTest,], type = c("response"))
    ### accuracy
    accuracy <- sum(diag(table(rf_pred,y[indTest])))/
        length(y[indTest]) # [1] 0.3042973
    
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
    
    k <- 50
    ### work on proportions
    # library(e1071)
    library(class)
    cat('Fitting second model to 33% training subset..\n')
    indicatorsKNN <- as.integer(knn(train=basetable_KNN[indTrain,],
                                    test=basetable_KNN[indVal,],
                                    cl=y[indTrain],
                                    k=k))
    #retrieve the actual y from the training set
    cat('Tuning second model on 33% validation subset..\n')
    predKNN <- as.integer(as.character(y[indTrain][indicatorsKNN])) 
    predKNN <- data.frame(matrix(data=predKNN,
                                 ncol=k, 
                                 nrow=nrow(basetable_KNN[indVal,])))
    
    predictions <- sapply(1:nrow(predKNN), function(x)
        as.numeric(names(sort(table(as.numeric(predKNN[x,])),decreasing = TRUE)[1])))
    proportion <- sapply(1:nrow(predKNN), function(x)
        as.numeric(sort(table(as.numeric(predKNN[x,])),decreasing = TRUE)[1])) / k
    # AUC::auc(roc(predictions,ytest))
    table_pred <-table(predictions,y[indVal])                                      
    accuracy_test <- sum(diag(table_pred))/length(predictions)
    customer_item_purch <- data.frame("CustomerID" = customer_ids[indVal],
                                      "NextPurch" = predictions,
                                      "Probability" = proportion)
    customer_item_purch <- customer_item_purch[order(proportion, decreasing = TRUE),]
    # customer_item_purch$NextPurch <- unique(visits_visitdetails$ProductID)[customer_item_purch$NextPurch]
    
    
    ### TO DO:
    cat('Evaluating second model on 33% test subset..\n')
    
    
    # Return all the stuff in a list
    return(list(m1=rf_model, 
            p1=rf_pred,
            a1=accuracy,
            data1=basetable,
            m2=indicatorsKNN , 
            p2=customer_item_purch,
            a2=accuracy_test,
            data2=basetable_KNN))
}


system.time(
    result <- create_model(
    start_ind="2007-01-08",
    end_ind="2008-07-03",
    start_dep="2008-07-04",
    end_dep="2008-12-31"))

