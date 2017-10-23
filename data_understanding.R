# http://kddata.co/data/chapter6/routes.csv
# http://kddata.co/data/chapter6/products.csv
# http://kddata.co/data/chapter6/customers.csv
# http://kddata.co/data/chapter6/visitdetails.csv
# http://kddata.co/data/chapter6/visits.csv

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

# Create table of customer Visit ID and CustomerID
# visits <- data.table(merge(x=visits, y=visitdetails, by='VisitID', all.x=TRUE))
# Number of visits
NumberVisits <- visits[,
    list(NumberVisits=.N),
    by='CustomerID']

# Confirm there are no duplicates
!any(duplicated(NumberVisits$CustomerID)) # [1] TRUE

# Number of Sales Reps
NumberSalesReps <- visits[,
    list(NumberSalesReps=length(unique(SalesRepresentativeID))),
    by='CustomerID']

# Confirm there are no duplicates
!any(duplicated(NumberVisits$CustomerID)) # [1] TRUE

################################
# Mean number of ProductID per visit
################################

# First we need 'CustomerID' from visits
visitdetails_visits <- merge(x=visitdetails,
                      y=visits[,list(VisitID, CustomerID)],
                      by='VisitID',
                      all.x=TRUE)

# Count distinct products for each visit
NumberProducts <- visitdetails_visits[,
    list(NumberProducts = length(unique(ProductID))), 
    by=c('VisitID', 'CustomerID')]

# Compute mean NumberProducts by cutomer
AvgNumberProducts <- NumberProducts[, 
    list(AvgNumberProducts = mean(NumberProducts)), 
    by='CustomerID']

# Confirm there are no duplicates
!any(duplicated(AvgNumberProducts$CustomerID)) # [1] TRUE

# Note that only customers who made at least one purchase 
# are in visitdetails, so for customers who did not make 
# a purchase we'll need to assign 0 for any variables
# that are derived from this table.

################################
# Mean dollar sales per visit
################################

# First we'll need 'Price' from products
visitdetails_visits_products <- merge(x=visitdetails_visits,
                             y=products[,list(ProductID, Price)],
                             by='ProductID',
                             all.x=TRUE)

# Convert Price and quantity to numeric
visitdetails_visits_products[, Price:=as.numeric(Price)]
visitdetails_visits_products[, Quantity:=as.numeric(Quantity)]

# Inspect the values for Price and Quantity
summary(visitdetails_visits_products[,list(Price, Quantity)])
# Price            Quantity     
# Min.   :-25.000   Min.   :-6.000  
# 1st Qu.:  5.300   1st Qu.: 1.000  
# Median :  7.400   Median : 1.000  
# Mean   :  7.723   Mean   : 1.124  
# 3rd Qu.:  9.500   3rd Qu.: 1.000  
# Max.   : 37.000   Max.   :61.000  
# NA's   :3383                     

# Apparently 3383 products do not have a price. These might
# be givaway items or complementary items recorded for
# inventory purposes.

# Before proceeding we need to replace 
# missing prices with 0
visitdetails_visits_products[, Price:=ifelse(is.na(Price), 0, Price)]


# Compute total dollar sales for each visit
DollarSales <- visitdetails_visits_products[,
    list(TotalDollarSales=sum(Price*Quantity)),
    by=c('VisitID', 'CustomerID')]

# Compute average dollar sales by cutomer
AvgDollarSales <- DollarSales[, 
    list(AvgDollarSales = round(mean(TotalDollarSales),2)), 
    by='CustomerID']


# Merge dependent and independent to create the basetable
data <- list(NumberVisits, NumberSalesReps, AvgNumberProducts, AvgDollarSales)
basetable <- data.table(Reduce(function(x,y) merge(x, y, by='CustomerID', all=TRUE), data))

# Inspect values of each variable
summary(basetable[,-1])
# NumberVisits    NumberSalesReps  AvgNumberProducts AvgDollarSales  
# Min.   :  1.00   Min.   : 1.000   Min.   : 1.000    Min.   : -9.60  
# 1st Qu.: 42.00   1st Qu.: 2.000   1st Qu.: 1.167    1st Qu.:  8.78  
# Median : 47.00   Median : 5.000   Median : 1.409    Median : 10.88  
# Mean   : 42.84   Mean   : 4.428   Mean   : 1.622    Mean   : 13.28  
# 3rd Qu.: 51.00   3rd Qu.: 7.000   3rd Qu.: 1.757    3rd Qu.: 14.12  
# Max.   :116.00   Max.   :13.000   Max.   :17.200    Max.   :566.88  
# NA's   :317       NA's   :317   

# AvgNumberProducts and AvgDollarSales both have NAs. These correspond to 
# customers who were did not make any purchases so can be replced with 0
basetable[, AvgNumberProducts:=ifelse(is.na(AvgNumberProducts), 0, AvgNumberProducts)]
basetable[, AvgDollarSales:=ifelse(is.na(AvgDollarSales), 0, AvgDollarSales)]

# Inspect values of each variable
summary(basetable)
# CustomerID         NumberVisits    NumberSalesReps  AvgNumberProducts AvgDollarSales  
# Length:5612        Min.   :  1.00   Min.   : 1.000   Min.   : 0.000    Min.   : -9.60  
# Class :character   1st Qu.: 42.00   1st Qu.: 2.000   1st Qu.: 1.125    1st Qu.:  8.38  
# Mode  :character   Median : 47.00   Median : 5.000   Median : 1.375    Median : 10.59  
# Mean   : 42.84   Mean   : 4.428   Mean   : 1.530    Mean   : 12.53  
# 3rd Qu.: 51.00   3rd Qu.: 7.000   3rd Qu.: 1.734    3rd Qu.: 13.86  
# Max.   :116.00   Max.   :13.000   Max.   :17.200    Max.   :566.88  


# The HVC wants two recommendation engines: 
# (1) which product will a customer buy next (next product to buy model) 
# in the dependent period, and

# (2) all the products a customer will buy in the dependent period.

# TO DO:
# 1. Add dependent variable to basetable code
# 2. Put basetable code in a function that takes four 
#    arguments that specify t1, t2, t3 t4

