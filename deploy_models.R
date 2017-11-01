# rm(list=ls())

deploy_models <- function(object,dumpdate){
    
    # Code to create the basetable
    source('create_basetable.R')
    basetable <- create_basetable(
        start_ind=dumpdate,
        end_ind=as.Date(dumpdate)+object$length_ind,
        train=FALSE)
    
    # clean up the environment
    # rm(list=ls()[!ls() %in% c('basetable','visits_visitdetails')])
    
    ### make the response vector
    customer_ids <- basetable$CustomerID
    # y <- basetable$Response
    
    # #### make response matrix
    # test <- basetable[,c("CustomerID","Response")]
    # test_melt <- melt(test, id.vars = c("CustomerID","Response"))
    # test_melt$Response <- as.numeric(as.character( test_melt$Response )) # changed to numeric to avoid sum not meaningful for factors error
    # test_cast <- acast(test_melt, CustomerID ~ Response, sum)
    # response_cast <- as.data.frame(test_cast)
    # response_cast <- as.data.frame(sapply(response_cast, function(x){replace(x, x > 0,1)}))
    # response_cast$CustomerID <- rownames(test_cast)
    # Y <- response_cast[,-41]
    
    # Remove unnecesary columns
    basetable$CustomerID <- NULL
    # basetable$Response <- NULL
    
    
    #####################################
    
    ############# randomForest ##########
    
    #####################################
    
    # Predict labels on all of the data
    cat('Evaluating first model on full dataset..\n')
    rf_pred <- predict(#rf_model
        object$m1, newdata = basetable, type = c("response"))
    rf_pred2 <- predict(rf_model, newdata = basetable, type = c("prob"))
    
    #x <- sapply(1:nrow(rf_pred2), function(x) max(rf_pred2[x,]))
    x <- sapply(1:nrow(rf_pred2), function(x)
        rf_pred2[x,colnames(rf_pred2)[which(colnames(rf_pred2) == as.character(rf_pred[x]))]])
    output <- data.frame("CustomerID" = customer_ids,
                         "Product" = rf_pred,
                         "Probability" = x)
    output <- output[order(output$Probability, decreasing = TRUE),]
    
    #####################################
    
    ############# kNN Model #############
    
    #####################################
    
    # Predict labels on all of the data
    cat('Evaluating second model on full dataset..\n')
    
    basetable_KNN <- data.frame(sapply(basetable, function(x) as.numeric(as.character(x))))
    
    stdev <- sapply(basetable_KNN[#indTrain
        ,1:4],sd)
    means <- sapply(basetable_KNN[#indTrain
        ,1:4],mean)
    # basetable_KNN[#c(indTrain,indVal)
    #     ,1:4] <- data.frame(t((t(basetable_KNN[#c(indTrain,indVal)
    #         ,1:4])-means)/stdev))
    basetable_KNN[#indTrain
        ,1:4] <- data.frame(t((t(basetable_KNN[#indTrain
            ,1:4])-means)/stdev))
    # basetable_KNN[#indVal
    #     ,1:4] <- data.frame(t((t(basetable_KNN[#indVal
    #         ,1:4])-means)/stdev))
    # basetable_KNN[#indTest
    #     ,1:4] <- data.frame(t((t(basetable_KNN[#indTest
    #         ,1:4])-means)/stdev))
    
    
    
    ###kNN Validation for best model and model building based on optimal parameters:
    # cat('Tuning second model on 33% validation subset..\n')
    # cutoffs <- c(seq(.1,.8,.05))
    # neighbors <- 20:40
    # index <- 0 
    # true_pos <- true_neg <- true_pos_rate <- sq_error <- matrix(nrow = length(neighbors), ncol = length(cutoffs))
    # rownames(sq_error) <- paste0(neighbors,"_NN")
    # colnames(sq_error) <- cutoffs
    # 
    # library(e1071)
    # library(FNN)
    # 
    # for(k in neighbors){
    #     
    #     #retrieve the indicators of the k nearest neighbors of the query data
    #     indicatorsKNN <- as.integer(knnx.index(data=basetable[indTrain,],
    #                                            query=basetable[indVal,],
    #                                            k=k))
    #     #retrieve the actual y from the training set
    #     #predKNN <- y[indTrain,][indicatorsKNN,]
    #     predKNN <- data.frame(matrix(data=indicatorsKNN,
    #                                  ncol=k,
    #                                  nrow=nrow(basetable[indVal,])))
    #     predictions <- lapply(1:nrow(predKNN), function(x) colMeans(Y[indTrain,][as.numeric(predKNN[x,]),]))
    #     predictions <- do.call(rbind, predictions)
    #     
    #     
    #     for(j in 1:length(cutoffs)){
    #         predictions_logical <- lapply(1:nrow(predictions), function(x)
    #             ifelse(predictions[x,] > cutoffs[j], 1, 0))
    #         predictions_logical <- do.call(rbind, predictions_logical)
    #         num_pos <- sum(Y[indVal,] == 1)
    #         num_neg <- sum(Y[indVal,] == 0)
    #         false_neg <- sum(Y[indVal,] == 1 & predictions_logical == 0)
    #         true_pos[index,j] <- sum(Y[indVal,] == 1 & predictions_logical == 1)
    #         true_neg[index,j] <- sum(Y[indVal,] == 0 & predictions_logical == 0)
    #         true_pos_rate[index,j] <- 100 * num_pos / (num_pos + false_neg)
    #         #sq_error[k,j] <- .5 * ((true_pos / num_pos) + (true_neg / num_neg))
    #         sq_error[index,j] <- sum(abs(Y[indVal,] - predictions_logical)) - 5*(sum(Y[indVal,] == 1 & predictions_logical == 1))
    #         #sum(y[indVal,][1:nrow(y[indVal,]),] == predictions[1:nrow(y[indVal,]),]) / (nrow(y[indVal,])*ncol(y[indVal,]))
    #     }
    #     index <- index + 1
    # }
    # 
    # best_par <- which(sq_error == min(sq_error, na.rm = TRUE), arr.ind = TRUE)
    # k <- neighbors[best_par[1]] # [1] 34
    # j <- best_par[2] # [1] 2
    
    
    cat('Applying second model on full dataset..\n')
    #retrieve the indicators of the k nearest neighbors of the query data
    indicatorsKNN <- as.integer(knnx.index(data=basetable#[c(indTrain,indVal),]
                                           ,
                                           query=basetable#[indTest,]
                                           ,
                                           k=object$k))
    #retrieve the actual y from the training set
    #predKNN <- y[c(indTrain, indVal),][indicatorsKNN,]
    predKNN <- data.frame(matrix(data=indicatorsKNN,
                                 ncol=object$k,
                                 nrow=nrow(basetable#[indTest,]
                                           )))
    predictions <- lapply(1:nrow(predKNN), function(x) colMeans(Y#[c(indTrain, indVal),]
                                                                [as.numeric(predKNN[x,]),]))
    predictions <- do.call(rbind, predictions)
    
    predictions_logical <- lapply(1:nrow(predictions), function(x)
        ifelse(predictions[x,] > #cutoffs[j]
                   object$cutoff
                   , 1, 0))
    predictions_logical <- do.call(rbind, predictions_logical)
    
    # num_rec <- sum(predictions_logical == 1) # [1] 632
    # num_purch <- sum(Y[indTest,] == 1) # [1] 972
    # correct_rec <- sum(predictions_logical == 1 & Y[indTest,] == 1) # [1] 109
    # (avg_purch <- num_purch/nrow(Y[indTest,])) # [1] 1
    # (avg_rec <- num_rec/nrow(Y[indTest,])) # [1] 0.6502058
    # (perc_correct_rec <- correct_rec / num_rec) # [1] 0.1724684
    
    
    
    # Return all the stuff in a list
    return(list(p1=output,
                data1=basetable,
                p2=predictions_logical,
                data2=basetable_KNN))
    
    # Verify basetables are generated
    # return(list(data1=basetable,
    #             data2=basetable_KNN))
    
}

# # Run first
# source('create_models.R')
# system.time(
#     result <- create_models(
#         start_ind="2007-01-08",
#         end_ind="2008-01-03",
#         start_dep="2008-01-04",
#         end_dep="2008-12-29"))


# as.Date("2007-01-08")+360
# as.Date("2007-01-08")+360+1
# as.Date("2007-01-08")+360+1+360
# result$m1
# result$m2
# result$a1
# result$a2
# result$p1
# result$p2




system.time(
    result2 <- deploy_models(
        object=result,
        dumpdate="2007-01-08"))

