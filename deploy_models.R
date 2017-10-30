rm(list=ls())

source('create_models.R')

system.time(
    result <- create_models(
    start_ind="2007-01-08",
    end_ind="2008-07-03",
    start_dep="2008-07-04",
    end_dep="2008-12-31"))

deploy_models <- function(start_ind,end_ind,start_dep,end_dep){
    
    # Code to create the basetable
    source('create_basetable.R')
    basetable <- create_basetable(
        start_ind=start_ind,
        end_ind=end_ind,
        start_dep=start_dep,
        end_dep=end_dep, 
        train=FALSE)
    
    # clean up the environment
    # rm(list=ls()[!ls() %in% c('basetable','visits_visitdetails')])
    
    ############# randomForest #############
    # Code to create the model
    ### test/train split
    customer_ids <- basetable$CustomerID
    y <- basetable$Response
    # Remove unnecesary columns
    basetable$CustomerID <- NULL
    basetable$Response <- NULL
    # Generate 50/50 train and test indicies
    # set.seed(7)
    # ind <- 1:nrow(basetable)
    # indTrain <- sample(ind, ceiling(length(ind)/2))
    # indTest <- ind[-indTrain]
    
    # Generate predictions from the first model
    cat('Generating predictions from the first model..\n')
    ############# kNN Model #############
    
    # Generate 33/33/33 train,test and validation indicies
    # set.seed(7)
    # indTrain <- sample(ind, ceiling(length(ind)/3))
    # indVal <- sample(ind[-indTrain], ceiling(length(ind)/3))
    # indTest <- ind[-c(indTrain, indVal)]
    basetable_KNN <- data.frame(sapply(basetable, function(x) as.numeric(as.character(x))))
    
    
    ############# kNN Model #############
    # Generate 33/33/33 train,test and validation indicies
    # basetabletrain <- basetable_KNN[trainind,]
    # basetableval <- basetable_KNN[valind,]
    # basetabletest <- basetable_KNN[testind,]
    # basetabletrainbig <- rbind(basetabletrain,basetableval)
    # ytrain <- y[trainind]
    # yval <- y[valind]
    # ytest <- y[testind]
    # ytrainbig <- factor(y[c(indTrain,indVal)])
    
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
    
    
    # Generate predictions from the second model
    cat('Generating predictions from the second model..\n')
    
    # Return all the stuff in a list
    # return(list(p1=rf_pred,
    #             data1=basetable, 
    #             p2=customer_item_purch,
    #             data2=basetable_KNN))
    
    
    # Verify basetables are generated
    return(list(data1=basetable,
                data2=basetable_KNN))
    
}

system.time(
    result2 <- deploy_models(
        start_ind="2007-01-08",
        end_ind="2008-07-03",
        start_dep="2008-07-04",
        end_dep="2008-12-31"))

