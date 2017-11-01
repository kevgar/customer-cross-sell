# rm(list=ls())

create_basetable <- function(start_ind, end_ind, start_dep, end_dep, train=TRUE){
    
    if(train){
        
        library(data.table)
        cat('Reading in the data..\n')
        
        l <<- list(
            routes = data.table(read.csv('http://kddata.co/data/chapter6/routes.csv',
                                         colClasses = c("RouteID" = "character",
                                                        "Region" = "factor",
                                                        "WeekOrder" = "factor",
                                                        "Day" = "factor"))),
            products = data.table(read.csv('http://kddata.co/data/chapter6/products.csv',
                                           colClasses = c("ProductID" = "character",
                                                          "Category" = "factor",
                                                          "Family" = "factor",
                                                          "Price" = "numeric"))),
            customers = data.table(read.csv('http://kddata.co/data/chapter6/customers.csv',
                                            colClasses = c("CustomerID" = "character",
                                                           "RouteID" = "factor",
                                                           "CustomerType" = "factor",
                                                           "ZIP" = "character",
                                                           "SeasonType" = "factor"))),
            visitdetails = data.table(read.csv('http://kddata.co/data/chapter6/visitdetails.csv',
                                               colClasses = c("VisitDetailID" = "character",
                                                              "ProductID" = "character",
                                                              "Quantity" = "integer",
                                                              "VisitID" = "character"))),
            visits = data.table(read.csv('http://kddata.co/data/chapter6/visits.csv',
                                         colClasses = c("VisitID" = "character",
                                                        "CustomerID" = "character",
                                                        "SalesRepresentativeID" = "character",
                                                        "VisitDate" = "character",
                                                        "Amount" = "numeric",
                                                        "PaymentTerm" = "factor",
                                                        "Outcome" = "factor"))
            )[,Hour:=substring(VisitDate,11,12)
              ][,TimeIndicator:=substring(VisitDate,30,31)
                ][,VisitDate:=as.Date(VisitDate, format='%d-%b-%y')])
    }
    
    try( if( is.null(l) ) stop("call create_models function then try again..\n") )
    
    cat('Preparing the basetable..\n')
    ################################
    # For each customer compute:
    # Recency of last purchase
    # Number of visits
    # Number of sales reps
    # Mean item price
    # Mean number of products (per visit)
    # CustomerType dummies
    # SeasonType dummies
    # WeekOrder dummies
    # Weekday dummies
    # Zipcode dummies
    # RoutID dummies
    # Region dummies
    # Units sold (by product)
    ################################
    
    ### merge customers and routes
    customers_routes <- merge(l$customers, l$routes, by = "RouteID", all.x = TRUE)
    
    ### merge visits and visit details
    visits_visitdetails <- merge(l$visitdetails, l$visits, by = "VisitID", all.x = TRUE)
    
    ### Only revenue generating products
    revenue_products <- l$products[Price > 0,]
 
    ### time window
    t1 <- as.Date(start_ind)
    t2 <- as.Date(end_ind)
    if(train){ 
        t3 <- as.Date(start_dep)
        t4 <- as.Date(end_dep) 
        } # dump date
    
    if(train){
        ### Select customers with purchases in DEP
        customers_purchased <- unique(visits_visitdetails[VisitDate >= t3 & VisitDate <= t4, "CustomerID"])
        ### Select data for customers with purchase
        visits_visitdetails <- visits_visitdetails[
            CustomerID %in% customers_purchased$CustomerID &
                ProductID %in% revenue_products$ProductID,]
        }
    
    if(train){
        ### DEP period
        visits_visitdetails_DEP <- visits_visitdetails[
            VisitDate >= t3 & VisitDate <= t4,]
        }
    
    ### IND period 
    visits_IND <- l$visits[VisitDate >= t1 & VisitDate <= t2,]
    
    ### IND period 
    visits_visitdetails_IND <- visits_visitdetails[VisitDate >= t1 & VisitDate <= t2,]
    
    if(train){
        
        ### num customers in DEP period
        length(unique(visits_visitdetails_DEP$CustomerID)) # [1] 4655
        
        ### DEP variable
        visits_visitdetails_DEP[,Response:=0]
        for(i in 1:length(unique(visits_visitdetails$ProductID))){
            visits_visitdetails_DEP$Response <-
                ifelse(visits_visitdetails_DEP$ProductID ==
                           unique(visits_visitdetails$ProductID)[i],
                       i,visits_visitdetails_DEP$Response)
        }
        
        ### Find most recent purchase date by customer in DEP period
        recent_purchase_date <- aggregate(
            visits_visitdetails_DEP$VisitDate,
            by = list("CustomerID"=visits_visitdetails_DEP$CustomerID), min)
        colnames(recent_purchase_date)[2] <- "Date"
        
        ### Merge in product most recently purchased
        recent_response <- merge(recent_purchase_date,visits_visitdetails_DEP[
            ,c("CustomerID","VisitDate","Response","Amount")
            ], all.x = TRUE)
        
        ### take top selling item for each customer
        recent_response <- recent_response[order(recent_response$Amount,decreasing=TRUE),]
        recent_response <- aggregate(recent_response$Response,
                                     by=list("CustomerID"=recent_response$CustomerID),
                                     head,n=1)
        colnames(recent_response)[2] <- "Response"
    }
    
    ### make variables for predictors
    
    ### recency
    recency <- aggregate(visits_visitdetails_IND$VisitDate, 
                         by = list("CustomerID"=visits_visitdetails_IND$CustomerID),
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
    
    ### total dollar sales per customer
    total_amount <- aggregate(visits_IND$Amount, 
                              by = list("CustomerID" = visits_IND$CustomerID),
                              sum)
    colnames(total_amount)[2] <- "Total_amount"
    total_amount$Total_amount <- round(total_amount$Total_amount, 2)
    
    ### total quantity sales of each customer
    total_quantity <- aggregate(visits_visitdetails_IND$Quantity, 
                                by = list("CustomerID" = visits_visitdetails_IND$CustomerID),
                                sum)
    colnames(total_quantity)[2] <- "Total_quantity"
    
    ### merge total amount with total quantity
    amount_per_item <- merge(total_amount, total_quantity, by = "CustomerID", all.x = TRUE)
    amount_per_item$Amount_per_Item <- amount_per_item$Total_amount/amount_per_item$Total_quantity
    amount_per_item$Total_amount <- amount_per_item$Total_quantity <- NULL
    
    ### make list and reduce
    data <- list(recency,
                 num_products,
                 num_visits,
                 amount_per_item)
    
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
    
    basetable <- merge(x=basetable, y=test_cast,
                       by = "CustomerID", 
                       all.x = TRUE, suffixes = ''
                       )[,!duplicated(names(basetable))]
    #########
    
    if(train){
        ### bring in response
        basetable <- merge(recent_response,basetable, by = "CustomerID", all.y = TRUE)
        basetable <- basetable[!is.na(basetable$Response),]
        ### get total number purchased for each product
        num_responses <- aggregate(basetable$Response, by = list("Response" = basetable$Response), length)
        ### get the top 40 products
        l$products <- head(num_responses$Response[order(num_responses$x,decreasing = TRUE)],40)
        basetable <- basetable[basetable$Response %in% l$products,]
        basetable$Response <- factor(as.character(basetable$Response))
        # basetable$Response <- factor(basetable$Response)
    }
    
    # clean up the environment
    # rm(list=ls()[!ls() %in% c('basetable','visits_visitdetails')])
    
    return(basetable)
    
    }

# system.time(
# basetable <- create_basetable(
# start_ind="2007-01-08",
# end_ind="2008-01-03",
# start_dep="2008-01-04",
# end_dep="2008-12-29"))

