rm(list=ls())

create_basetable <- function(start_ind, end_ind, start_dep, end_dep){
    
    library(data.table)
    cat('Reading in the data..\n')
    
    routes <-  data.table(read.csv('http://kddata.co/data/chapter6/routes.csv', 
                       colClasses = c("RouteID" = "character",
                                      "Region" = "factor",
                                      "WeekOrder" = "factor",
                                      "Day" = "factor")))
    
    products <-  data.table(read.csv('http://kddata.co/data/chapter6/products.csv', 
                         colClasses = c("ProductID" = "character",
                                        "Category" = "factor",
                                        "Family" = "factor",
                                        "Price" = "numeric")))
    
    customers <-  data.table(read.csv('http://kddata.co/data/chapter6/customers.csv', 
                          colClasses = c("CustomerID" = "character",
                                         "RouteID" = "factor",
                                         "CustomerType" = "factor",
                                         "ZIP" = "character",
                                         "SeasonType" = "factor")))
    
    visitdetails <-  data.table(read.csv('http://kddata.co/data/chapter6/visitdetails.csv', 
                              colClasses = c("VisitDetailID" = "character",
                                             "ProductID" = "character",
                                             "Quantity" = "integer",
                                             "VisitID" = "character")))
    
    visits <-  data.table(read.csv('http://kddata.co/data/chapter6/visits.csv', 
                       colClasses = c("VisitID" = "character",
                                      "CustomerID" = "character",
                                      "SalesRepresentativeID" = "character",
                                      "VisitDate" = "character",
                                      "Amount" = "numeric",
                                      "PaymentTerm" = "factor",
                                      "Outcome" = "factor")))
    
    visits[,VisitDate:=as.Date(visits$VisitDate, format='%d-%b-%y')]
    visits[,Hour:=substring(visits$VisitDate,11,12)]
    visits[,TimeIndicator:=substring(visits$VisitDate,30,31)]
    
    cat('Preparing the basetable..\n')
    ################################
    # For each customer compute:
    # Recency of last purchase
    # Number of visits
    # Number of sales reps
    # Mean amount (per visit)
    # Mean number of products (per visit)
    # Mean price*quantity (per visit)
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
    customers_routes <- merge(customers, routes, by = "RouteID", all.x = TRUE)
    
    ### merge visits and visit details
    visit_sales <- merge(visitdetails, visits, by = "VisitID", all.x = TRUE)
    
    ### Only revenue generating products
    revenue_products <- products[products$Price > 0,]
    
    ### time window
    t1 <- min(visits$Date) 
    t4 <- max(visits$Date)
    t3 <- t4-180 #t4-425
    t2 <- t3-1
    
    
    
    # select the time window
    t1 <- as.Date(start_ind)
    t2 <- as.Date(end_ind)
    t3 <- as.Date(start_dep)
    t4 <- as.Date(end_dep)
    
    # First select visits that occured in the independent period
    visitsIND <- visits[(VisitDate >= t1) & (VisitDate <= t2),]
    # We also want to exclude visits where the customer was not home
    # visitsIND <- visitsIND[Outcome!='NOT HOME',]
    visitsIND <- visitsIND[Outcome=='SALES',]
    
    # Compute number of visits for each customer
    NumberVisits <- visitsIND[,list(NumberVisits=.N),by='CustomerID']
    
    # Compute average visit amount for each customer
    visitsIND[,Amount:=as.numeric(Amount)]
    AvgVisitAmount <- visitsIND[,list(AvgVisitAmount=mean(Amount)),by='CustomerID']
    
    # Number of Sales Reps
    NumberSalesReps <- visitsIND[,list(NumberSalesReps=length(unique(SalesRepresentativeID))),by='CustomerID']
    
    # Mean number of ProductID per visit
    # First we need the relevant 'CustomerID' for each visit so we join visitdetails to visits. 
    # We do an innner join because we want a CustomerID for every VisitID and vise versa
    visitdetails_visits <- merge(x=visitdetails,
                                 y=visitsIND[,list(VisitID, CustomerID)],
                                 by='VisitID')
    
    # Count distinct products by visit
    NumberProducts <- visitdetails_visits[,list(NumberProducts=length(unique(ProductID))),
                                          by=c('VisitID', 'CustomerID')]
    # Compute mean NumberProducts by cutomer
    AvgNumberProducts <- NumberProducts[,list(AvgNumberProducts = mean(NumberProducts)), 
                                        by='CustomerID']
    
    # Mean dollar sales per visit
    # First we need 'Price' from the products table. We do an inner join
    # but could also have used a left join and gotten the same result
    visitdetails_visits_products <- merge(x=visitdetails_visits,
                                          y=products[,list(ProductID, Price)],
                                          by='ProductID')
    
    # Convert Price and quantity to numeric
    visitdetails_visits_products[, Price:=as.numeric(Price)]
    visitdetails_visits_products[, Quantity:=as.numeric(Quantity)]
    
    # Apparently there were 1307 instances where a product 
    # did not have a price. Let's see what these are..
    zeroPrice <- unique(visitdetails_visits_products[is.na(Price) | Price==0,ProductID])
    
    # So the free stuff includes of coffee, scoops of ice 
    # cream, assortmants, garnitures and coupons.
    zeroPriceCoupon <- products[ProductID %in% zeroPrice & Category=='Coupon',ProductID]
    zeroPriceProduct <- products[ProductID %in% zeroPrice & Category=='Product',ProductID]
    
    # From the summary we see the price can also be negative.
    # Let's see what those products are..
    negPrice <- unique(visitdetails_visits_products[Price<0,]$ProductID)
    
    # These are all coupons. We don't know why price is negative 
    # for some coupons and 0 or missing for others. We can rename 
    # in order to keep track of these
    negPriceCoupon <- negPrice
    
    # Before proceeding we need to replace missing prices with 0
    visitdetails_visits_products[, Price:=ifelse(is.na(Price), 0, Price)]
    
    # Compute total dollar sales for each visit
    DollarSales <- visitdetails_visits_products[,list(TotalDollarSales=sum(Price*Quantity)),
                                                by=c('VisitID', 'CustomerID')]
    # Compute average dollar sales by customer
    AvgDollarSales <- DollarSales[,list(AvgDollarSales=round(mean(TotalDollarSales),2)),
                                  by='CustomerID']
    
    # Find quanitites of each product purchased by each custumer
    library(tidyr) # need tidyr::spread
    UnitSales <- visitdetails_visits_products[,list(UnitSales=sum(Quantity)), by=c('CustomerID', 'ProductID')]
    UnitSales <- spread(UnitSales, key=ProductID, value=UnitSales)
    UnitSales[is.na(UnitSales)] <- 0
    
    # Rename the columns to indicate coupons and givaway items
    names(UnitSales) <- ifelse(names(UnitSales) %in% zeroPriceCoupon, 
                               paste0('zeroPriceCoupon_', names(UnitSales)), 
                               names(UnitSales))
    
    names(UnitSales) <- ifelse(names(UnitSales) %in% zeroPriceProduct, 
                               paste0('zeroPriceProduct_', names(UnitSales)), 
                               names(UnitSales))
    
    names(UnitSales) <- ifelse(names(UnitSales) %in% negPriceCoupon, 
                               paste0('negPriceCoupon_', names(UnitSales)), 
                               names(UnitSales))
    
    # Reorder columns so the coupons are first
    ordering <- c(1, order(names(UnitSales[,-1]), decreasing = TRUE)+1)
    UnitSales <- UnitSales[,ordering, with=FALSE]
    
    # variables that came from visitsIND have 5200 rows, but variables from
    # the visitsdetails table only have 5005. Let's see why that is..
    
    # Apparently these were visits with Outcome='NOTHING NEEDED'
    # These 195 customers were visited 2339 times..
    
    # Compute recency as t2 - most recent visit where Outcome!='NOTHING NEEDED'
    RecencySales <- visitsIND[Outcome=='SALES',list(RecencySales=mean(as.integer(t2-max(VisitDate)))),
                              by='CustomerID']
    
    # Get the customer table information
    CustomerTypeZip <- customers[, c('CustomerID', 'CustomerType', 'ZIP')]
    # Note this table has customers that are not in the independent period data. 
    # These will be eliminated whenenever we do the inner join
    
    # Unstack the customer type and zipcode columns to create dummy variables
    library(dummies)
    CustomerTypeZip <- dummy.data.frame(CustomerTypeZip,names=c('CustomerType','ZIP'))
    
    # Merge dependent and independent to create the basetable
    # Notice the innter join. This gets rid of the 195 customers mentioned above
    data <- list(NumberVisits, NumberSalesReps, RecencySales, AvgNumberProducts, AvgVisitAmount, AvgDollarSales, CustomerTypeZip, UnitSales)
    basetable <- data.table(Reduce(function(x,y) merge(x, y, by='CustomerID'), data))
    
    # clean up the environment
    # rm(list=ls()[!ls() %in% c('basetable', 'data')])
    
    return(basetable)
    
    }

# system.time(
#     basetable <- create_basetable(
#         start_ind="2007-01-08",
#         end_ind="2008-07-03",
#         start_dep="2008-07-04",
#         end_dep="2008-12-31")
#     )
