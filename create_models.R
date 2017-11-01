# rm(list=ls())

create_models <- function(start_ind,end_ind,start_dep,end_dep){

    # Code to create the basetable
    source('create_basetable.R')
    basetable <- create_basetable(
        start_ind=start_ind,
        end_ind=end_ind,
        start_dep=start_dep,
        end_dep=end_dep)
    
   # Save the response variable
    customer_ids <- basetable$CustomerID
    y <- basetable$Response
    
    # Remove unnecesary columns
    basetable$CustomerID <- NULL
    basetable$Response <- NULL
    
    
    
    #####################################
    
    ############# randomForest ##########
    
    #####################################
    
    # Generate 50/50 train and test indicies
    set.seed(7)
    ind <- 1:nrow(basetable)
    indTrain <- sample(ind, ceiling(length(ind)/2))
    indTest <- ind[-indTrain]
    # Fit random forest on the train data
    library(randomForest)
    cat('Fitting first model to 50% training subset..\n')
    
    rf_model <- randomForest(x = basetable[indTrain,],
                             y = factor(y[indTrain]),
                             ntree = 1000)
    
    # rf_model <- randomForest(x = basetabletrain,
    #                          y = ytrain,
    #                          ntree = 1000)
    
    
    # Predict labels on the test data
    cat('Evaluating first model on 50% test subset..\n')
    rf_pred <- predict(rf_model, newdata = basetable[indTest,], type = c("response"))
    rf_pred2 <- predict(rf_model, newdata = basetable[indTest,], type = c("prob"))

    ### accuracy
    (accuracy <- sum(diag(table(rf_pred,y[indTest])))/
        length(y[indTest])) # [1] 0.3042973
    
    #x <- sapply(1:nrow(rf_pred2), function(x) max(rf_pred2[x,]))
    x <- sapply(1:nrow(rf_pred2), function(x)
        rf_pred2[x,colnames(rf_pred2)[which(colnames(rf_pred2) == as.character(rf_pred[x]))]])
    output <- data.frame("CustomerID" = customer_ids[testind],
                         "Product" = rf_pred,
                         "Probability" = x)
    output <- output[order(output$Probability, decreasing = TRUE),]
    

    #####################################
    
    ############# kNN Model #############
    
    #####################################
    
    # Generate 33/33/33 train, val and test indicies
    set.seed(7)
    indTrain <- sample(ind, ceiling(length(ind)/3))
    indVal <- sample(ind[-indTrain], ceiling(length(ind)/3))
    indTest <- ind[-c(indTrain, indVal)]
    
    basetable_KNN <- data.frame(sapply(basetable, function(x) as.numeric(as.character(x))))

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
    

    ###kNN Validation for best model and model building based on optimal parameters:
    cat('Tuning second model on 33% validation subset..\n')
    cutoffs <- c(seq(.1,.8,.05))
    neighbors <- 20:40
    index <- 0 
    true_pos <- true_neg <- true_pos_rate <- sq_error <- matrix(nrow = length(neighbors), ncol = length(cutoffs))
    rownames(sq_error) <- paste0(neighbors,"_NN")
    colnames(sq_error) <- cutoffs
    
    library(e1071)
    library(FNN)
    
    for(k in neighbors){
        
        #retrieve the indicators of the k nearest neighbors of the query data
        indicatorsKNN <- as.integer(knnx.index(data=basetable[indTrain,],
                                               query=basetable[indVal,],
                                               k=k))
        #retrieve the actual y from the training set
        #predKNN <- y[indTrain,][indicatorsKNN,]
        predKNN <- data.frame(matrix(data=indicatorsKNN,
                                     ncol=k,
                                     nrow=nrow(basetable[indVal,])))
        predictions <- lapply(1:nrow(predKNN), function(x) colMeans(y[indTrain,][as.numeric(predKNN[x,]),]))
        predictions <- do.call(rbind, predictions)
        
        
        for(j in 1:length(cutoffs)){
            predictions_logical <- lapply(1:nrow(predictions), function(x)
                ifelse(predictions[x,] > cutoffs[j], 1, 0))
            predictions_logical <- do.call(rbind, predictions_logical)
            num_pos <- sum(y[indVal,] == 1)
            num_neg <- sum(y[indVal,] == 0)
            false_neg <- sum(y[indVal,] == 1 & predictions_logical == 0)
            true_pos[index,j] <- sum(y[indVal,] == 1 & predictions_logical == 1)
            true_neg[index,j] <- sum(y[indVal,] == 0 & predictions_logical == 0)
            true_pos_rate[index,j] <- 100 * num_pos / (num_pos + false_neg)
            #sq_error[k,j] <- .5 * ((true_pos / num_pos) + (true_neg / num_neg))
            sq_error[index,j] <- sum(abs(y[indVal,] - predictions_logical)) - 5*(sum(y[indVal,] == 1 & predictions_logical == 1))
            #sum(y[indVal,][1:nrow(y[indVal,]),] == predictions[1:nrow(y[indVal,]),]) / (nrow(y[indVal,])*ncol(y[indVal,]))
        }
        index <- index + 1
    }
    
    best_par <- which(sq_error == min(sq_error, na.rm = TRUE), arr.ind = TRUE)
    k <- neighbors[best_par[1]]
    j <- best_par[2]
    
    
    cat('Evaluate final performance of second model on 33% test data subset..\n')
    #retrieve the indicators of the k nearest neighbors of the query data
    indicatorsKNN <- as.integer(knnx.index(data=basetable[c(indTrain,indVal),],
                                           query=basetable[indTest,],
                                           k=k))
    #retrieve the actual y from the training set
    #predKNN <- y[c(indTrain, indVal),][indicatorsKNN,]
    predKNN <- data.frame(matrix(data=indicatorsKNN,
                                 ncol=k,
                                 nrow=nrow(basetable[indTest,])))
    predictions <- lapply(1:nrow(predKNN), function(x) colMeans(y[c(indTrain, indVal),][as.numeric(predKNN[x,]),]))
    predictions <- do.call(rbind, predictions)
    
    predictions_logical <- lapply(1:nrow(predictions), function(x)
        ifelse(predictions[x,] > cutoffs[j], 1, 0))
    predictions_logical <- do.call(rbind, predictions_logical)
    
    num_rec <- sum(predictions_logical == 1)
    num_purch <- sum(y[indTest,] == 1)
    correct_rec <- sum(predictions_logical == 1 & y[indTest,] == 1)
    (avg_purch <- num_purch/nrow(y[indTest,]))
    (avg_rec <- num_rec/nrow(y[indTest,]))
    (perc_correct_rec <- correct_rec / num_rec)
    
    ### TO DO:
  
    # Return all the stuff in a list
    return(list(
        length_ind=as.integer(as.Date(end_ind)-as.Date(start_ind)),
        m1=rf_model,
        p1=output,
        a1=accuracy,
        m2=indicatorsKNN,
        p2=customer_item_purch,
        a2=accuracy_test))
    
    }

# system.time(
#     result <- create_models(
#         start_ind="2007-01-08",
#         end_ind="2008-01-08",
#         start_dep="2008-01-09",
#         end_dep="2008-12-31"))
