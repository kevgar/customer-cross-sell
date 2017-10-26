# http://kddata.co/data/chapter6/routes.csv
# http://kddata.co/data/chapter6/products.csv
# http://kddata.co/data/chapter6/customers.csv
# http://kddata.co/data/chapter6/visitdetails.csv
# http://kddata.co/data/chapter6/visits.csv

rm(list=ls())

routes <- read.csv('http://kddata.co/data/chapter6/routes.csv', colClasses = 'character')
str(routes, vec.len=1)
# 'data.frame':	39 obs. of  4 variables:
# $ RouteID  : chr  "70671404" ...
# $ Region   : chr  "C04" ...
# $ WeekOrder: chr  "1" ...
# $ Day      : chr  "Sunday" ...
products <- read.csv('http://kddata.co/data/chapter6/products.csv', colClasses = 'character')
str(products, vec.len=1)
# 'data.frame':	317 obs. of  4 variables:
# $ ProductID: chr  "21276088156846" ...
# $ Category : chr  "Product" ...
# $ Family   : chr  "Scoop Ice" ...
# $ Price    : chr  "16.6" ...
customers <- read.csv('http://kddata.co/data/chapter6/customers.csv', colClasses = 'character')
str(customers, vec.len=1)
# 'data.frame':	5709 obs. of  5 variables:
# $ CustomerID  : chr  "719747" ...
# $ RouteID     : chr  "219019962" ...
# $ CustomerType: chr  "Private" ...
# $ ZIP         : chr  "39500" ...
# $ SeasonType  : chr  "All time" ...
visitdetails <- read.csv('http://kddata.co/data/chapter6/visitdetails.csv', colClasses = 'character')
str(visitdetails, vec.len=1)
# 'data.frame':	126778 obs. of  4 variables:
# $ VisitDetailID: chr  "56012184" ...
# $ ProductID    : chr  "23686" ...
# $ Quantity     : chr  "1" ...
# $ VisitID      : chr  "56012182" ...
visits <- read.csv('http://kddata.co/data/chapter6/visits.csv', colClasses = 'character')
str(visits, vec.len=1)
# 'data.frame':	240415 obs. of  7 variables:
# $ VisitID              : chr  "55693717" ...
# $ CustomerID           : chr  "720358" ...
# $ SalesRepresentativeID: chr  "23186288605042" ...
# $ VisitDate            : chr  "09-MAY-07 03.02.11.000000000 PM" ...
# $ Amount               : chr  "0" ...
# $ PaymentTerm          : chr  "" ...
# $ Outcome              : chr  "NOTHING NEEDED" ...

# Information for the ERD

# routs
!any(duplicated(routes$RoutID)) # [1] TRUE

# products
!any(duplicated(products$ProductID)) # [1] TRUE

# customers
!any(duplicated(customers$CustomerID)) # [1] TRUE
!any(duplicated(customers$RouteID)) # [1] FALSE

# visitdetails
!any(duplicated(visitdetails$VisitDetailID)) # [1] TRUE
!any(duplicated(visitdetails$ProductID)) # [1] FALSE
!any(duplicated(visitdetails$VisitID)) # [1] FALSE

# visits
!any(duplicated(visits$VisitID)) # [1] TRUE
!any(duplicated(visits$CustomerID)) # [1] FALSE
!any(duplicated(visits$SalesRepresentativeID)) # [1] FALSE

# Relationships
# routs to customers - RoutID 1:1
# visits to visitdetails - VisitID 1:n
# visitdetails to products - productID n:1

library(data.table)

################################
# For each CustomerID compute:
# Number of visits
# Number of sales reps
# Mean number of ProductID per visit
# Mean dollar sales per visit
################################

# First convert dataframe to data.table
routes <- data.table(routes)
products <- data.table(products)
customers <- data.table(customers)
visitdetails <- data.table(visitdetails)
visits <- data.table(visits)

# time window
visits$VisitDate <- as.Date(visits$VisitDate, format='%d-%b-%y')
t1 <- min(visits$VisitDate)
t4 <- max(visits$VisitDate)
t3 <- t4-180
t2 <- t3-1

# Next we want to exclude visits where the customer was not home
# We also select visits that are in the independent period
visitsIND <- visits[Outcome!='NOT HOME' & VisitDate <= t2,]
rm(visits)
# Compute number of visits for each customer
NumberVisits <- visitsIND[,
    list(NumberVisits=.N),
    by='CustomerID']

hist(NumberVisits$NumberVisits)

# Compute average visit amount for each customer
visitsIND$Amount <- as.numeric(visitsIND$Amount)
AvgVisitAmount <- visitsIND[,list(AvgVisitAmount=mean(Amount)), by='CustomerID']

hist(AvgVisitAmount$AvgVisitAmount)

# Confirm there are no duplicates
!any(duplicated(NumberVisits$CustomerID)) # [1] TRUE

# Number of Sales Reps
NumberSalesReps <- visitsIND[,
    list(NumberSalesReps=length(unique(SalesRepresentativeID))),
    by='CustomerID']

hist(NumberSalesReps$NumberSalesReps)

# Confirm there are no duplicates
!any(duplicated(NumberVisits$CustomerID)) # [1] TRUE

################################
# Mean number of ProductID per visit
################################

# First we need the relevant 'CustomerID' for each visit
# Thus we will join visitdetails to visits. An innner join 
# is used because we want to drop visits that dont have a 
# CustomerID and customers that dont have a VisitID
visitdetails_visits <- merge(x=visitdetails,
                      y=visitsIND[,list(VisitID, CustomerID)],
                      by='VisitID')

# Count distinct products for each visit
NumberProducts <- visitdetails_visits[,
    list(NumberProducts = length(unique(ProductID))), 
    by=c('VisitID', 'CustomerID')]

# Compute mean NumberProducts by cutomer
AvgNumberProducts <- NumberProducts[, 
    list(AvgNumberProducts = mean(NumberProducts)), 
    by='CustomerID']

hist(AvgNumberProducts$AvgNumberProducts)

# Confirm there are no duplicates
!any(duplicated(AvgNumberProducts$CustomerID)) # [1] TRUE

# Confirm there are no NAs
!any(is.na(AvgNumberProducts)) # [1] TRUE

################################
# Mean dollar sales per visit
################################

# First we'll need 'Price' from products
# First we need the relevant 'Price' for each product
# Thus we will join visitdetails_visits to products. 
# Here we use an inner join, but the result of is the 
# same had we used a left join
visitdetails_visits_products <- merge(x=visitdetails_visits,
                             y=products[,list(ProductID, Price)],
                             by='ProductID')

# Convert Price and quantity to numeric
visitdetails_visits_products[, Price:=as.numeric(Price)]
visitdetails_visits_products[, Quantity:=as.numeric(Quantity)]

# Inspect the values for Price and Quantity
summary(visitdetails_visits_products[,list(Price, Quantity)])
# Price            Quantity     
# Min.   :-25.000   Min.   :-2.000  
# 1st Qu.:  5.300   1st Qu.: 1.000  
# Median :  7.500   Median : 1.000  
# Mean   :  7.745   Mean   : 1.122  
# 3rd Qu.:  9.500   3rd Qu.: 1.000  
# Max.   : 37.000   Max.   :61.000  
# NA's   :1307

# Apparently there were 1307 instances where a product 
# did not have a price. Let's see what these are..
zeroPrice <- unique(visitdetails_visits_products[is.na(Price) | Price==0,ProductID])
products[ProductID %in% zeroPrice,]
# ProductID Category             Family Price
# 1:          23840  Product          Garniture     0
# 2:          23799  Product        Assortments     0
# 3: 21171556668044   Coupon                        0
# 4:       62001684  Product               Cups      
# 5: 18326144633838  Product        Assortments     0
# 6:       68213627   Coupon                         
# 7:       65431414  Product             Coffee      
# 8:       59354595   Coupon                         
# 9:       62875674  Product Individual Dessert      
# 10:       49330626   Coupon                         
# 11:       54251642   Coupon                         
# 12:       63444550   Coupon                         
# 13:       64013905  Product             Coffee      
# 14:       62438816  Product               Cups      
# 15:       64270183  Product          Scoop Ice      
# 16:       62003611  Product               Cups      
# 17:       64412289  Product          Scoop Ice      
# 18:       66303120  Product             Coffee      
# 19:       66443385   Coupon                          

# So the free stuff includes of coffee, scoops of ice 
# cream, assortmants, garnitures and coupons.
zeroPriceCoupon <- products[ProductID %in% zeroPrice & Category=='Coupon',ProductID]
zeroPriceProduct <- products[ProductID %in% zeroPrice & Category=='Product',ProductID]

# From the summary we see the price can also be negative.
# Let's see what those products are..
negPrice <- unique(visitdetails_visits_products[Price<0,]$ProductID)
products[ProductID %in% negPrice,]
# ProductID Category Family Price
# 1: 21454356698044   Coupon         -1.9
# 2: 21142456138046   Coupon         -4.1
# 3:       52529668   Coupon         -7.4
# 4:       53569504   Coupon         -4.1
# 5:       53614655   Coupon          -10
# 6:       47703913   Coupon          -25
# 7:         681916   Coupon          -10
# 8:       61239668   Coupon         -2.5
# 9: 22611748246244   Coupon           -1
# 10: 22092648256244   Coupon         -7.4
# 11: 20985584414848   Coupon         -3.9
# 12:       57367272   Coupon         -2.5
# 13:       54251376   Coupon         -5.2
# 14:       62157503   Coupon         -7.4
# 15:       62315111   Coupon         -4.2
# 16:       55360457   Coupon         -4.1

# These are all coupons. At this point we don't know why 
# price is negative for some coupons and 0 or missing for
# other coupons. We can rename them accordingly to keep
# track 
negPriceCoupon <- negPrice

# Before proceeding we need to replace
# missing prices with 0
visitdetails_visits_products[, Price:=ifelse(is.na(Price), 0, Price)]

# Compute total dollar sales for each visit
DollarSales <- visitdetails_visits_products[,
    list(TotalDollarSales=sum(Price*Quantity)),
    by=c('VisitID', 'CustomerID')]

# Compute average dollar sales by customer
AvgDollarSales <- DollarSales[,
    list(AvgDollarSales = round(mean(TotalDollarSales),2)),
    by='CustomerID']

# Find quanitites of each product purchased by each custumer
library(tidyr) # need tidyr::spread
QuantitySales <- visitdetails_visits_products[,list(QuantitySales=sum(Quantity)), by=c('CustomerID', 'ProductID')]
QuantitySales <- spread(QuantitySales, key=ProductID, value=QuantitySales)
QuantitySales[is.na(QuantitySales)] <- 0

# Confirm there are no duplicates 
!any(duplicated(QuantitySales$CustomerID))

# Rename the columns to indicate coupons and givaway items
names(QuantitySales) <- ifelse(names(QuantitySales) %in% zeroPriceCoupon, 
                               paste0('zeroPriceCoupon_', names(QuantitySales)), 
                               names(QuantitySales))

names(QuantitySales) <- ifelse(names(QuantitySales) %in% zeroPriceProduct, 
                               paste0('zeroPriceProduct_', names(QuantitySales)), 
                               names(QuantitySales))

names(QuantitySales) <- ifelse(names(QuantitySales) %in% negPriceCoupon, 
                               paste0('negPriceCoupon_', names(QuantitySales)), 
                               names(QuantitySales))

# Reorder columns so that coupons are first
ordering <- c(1, order(names(QuantitySales[,-1]), decreasing = TRUE)+1)
QuantitySales <- QuantitySales[,ordering, with=F]

# variables that come visitsIND have 5200 rows, but variables from
# the visitsdetails table only have 5005. Let's see why that is..
ind <- which(!visitsIND$CustomerID %in% AvgNumberProducts$CustomerID)
head(visitsIND[ind,])
# VisitID CustomerID SalesRepresentativeID  VisitDate Amount PaymentTerm        Outcome
# 1: 55697771     727782        23186288605042 2007-05-09      0             NOTHING NEEDED
# 2: 55678652     719891        23186288605042 2007-05-09      0             NOTHING NEEDED
# 3: 52135699    1402443        20126680425436 2007-01-12      0             NOTHING NEEDED
# 4: 55676241    1246017        18934048107254 2007-05-09      0             NOTHING NEEDED
# 5: 55688060    1453992        19946584592834 2007-05-09      0             NOTHING NEEDED
# 6: 52919789     724546        23186288605042 2007-02-08      0             NOTHING NEEDED

length(unique(visitsIND[ind,CustomerID])) # [1] 195
nrow(visitsIND[ind,]) # [1] 2339

# Apparently these were visits with Outcome='NOTHING NEEDED'
# These 195 customers were visited 2339 times.. maybe it's
# time to give up on them
wasteOfTime <- visitsIND[Outcome=='NOTHING NEEDED',.N,by='CustomerID']
hist(wasteOfTime$N)
visitdetails_visits[CustomerID %in% wasteOfTime$CustomerID,]

# Computer recency as t2 - most recent visit where Outcome!='NOTHING NEEDED'
recency <- visitsIND[Outcome=='SALES',list(recency=as.integer(t2-max(VisitDate))), 
                     by='CustomerID']

hist(recency$recency)

# Computer recency as t2 - most recent visit where Outcome!='NOTHING NEEDED'
RecencySales <- visitsIND[Outcome=='SALES',list(RecencySales=mean(as.integer(t2-max(VisitDate)))),
                     by='CustomerID']

hist(RecencySales$RecencySales)

# Get the customer table information
CustomerTypeZip <- customers[, c('CustomerID', 'CustomerType', 'ZIP')]
# Note this table has customers that are not in the independent period data. 
# These will be eliminated when we perform the inner join

# Confirm there are no duplicates
!any(duplicated(CustomerTypeZip$CustomerID)) # [1] TRUE

# Merge dependent and independent to create the basetable
# Notice the innter join. This gets rid of the 195 customers mentioned above
data <- list(NumberVisits, NumberSalesReps, RecencySales, AvgNumberProducts, AvgVisitAmount, AvgDollarSales, CustomerTypeZip, QuantitySales)
basetable <- data.table(Reduce(function(x,y) merge(x, y, by='CustomerID'), data))

# Inspect values of each variable
summary(basetable[,2:7])
# NumberVisits   NumberSalesReps   RecencySales    AvgNumberProducts AvgVisitAmount    AvgDollarSales  
# Min.   : 1.00   Min.   : 1.000   Min.   :  0.00   Min.   : 1.000    Min.   :-20.150   Min.   :-20.15  
# 1st Qu.:20.00   1st Qu.: 2.000   1st Qu.:  8.00   1st Qu.: 1.143    1st Qu.:  2.888   1st Qu.:  8.72  
# Median :27.00   Median : 4.000   Median : 21.00   Median : 1.400    Median :  4.733   Median : 10.94  
# Mean   :24.85   Mean   : 3.487   Mean   : 65.04   Mean   : 1.612    Mean   :  6.517   Mean   : 13.58  
# 3rd Qu.:31.00   3rd Qu.: 5.000   3rd Qu.: 56.00   3rd Qu.: 1.750    3rd Qu.:  7.572   3rd Qu.: 14.25  
# Max.   :85.00   Max.   :12.000   Max.   :541.00   Max.   :22.000    Max.   :216.733   Max.   :683.68  

# clean up the environment
rm(list=ls()[!ls() %in% c('basetable', 'data')])

# The HVC wants two recommendation engines: 
# (1) which product will a customer buy next (next product to buy model) 
# in the dependent period, and

# (2) all the products a customer will buy in the dependent period.

# TO DO:
# 1. Add dependent variable to basetable code
# 2. Put basetable code in a function that takes four 
#    arguments that specify t1, t2, t3 t4
