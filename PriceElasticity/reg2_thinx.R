orders_export_1 <- read.csv('/Users/jyothi/Desktop/thinx/orders_export_1.csv', comment.char="~")
orders_export_2 <- read.csv('/Users/jyothi/Desktop/thinx/orders_export_2.csv', comment.char="~")
orders_export <- read.csv('/Users/jyothi/Desktop/thinx/orders_export.csv', comment.char="~")

# Merging three datasets 
mergedf <- rbind( orders_export, orders_export_1,orders_export_2 )

#  Remove # sign before Name field
mergedf$Name <- substring(mergedf$Name, 2)
mergedf$Billing.Zip <- substring(mergedf$Billing.Zip, 2)
mergedf$Shipping.Zip <- substring(mergedf$Shipping.Zip, 2)

# Subseting useful columns
subDf <- subset(mergedf, select=c("Name", "Created.at","Lineitem.name","Lineitem.price","Lineitem.quantity" ,"Lineitem.discount"))

# Selecting only Hiphugger items
p1 <- 'Hiphugger'
df1 <- subset(subDf, grepl(p1,Lineitem.name ) )

summary(df1)
# Converting Created.at from string to date 
df1$Created.date <- as.POSIXct(df1$Created.at ,format= "%Y-%m-%d %H:%M:%S")

# There is one NA in discount.price
df1 <- na.omit(df1)

# Finding Price after discount
attach(df1)
df1$PAD <- with(df1, (Lineitem.price  -(Lineitem.discount/Lineitem.quantity)))
df1$Order.price <- with(df1, (Lineitem.price*Lineitem.quantity)-Lineitem.discount)
library(dplyr)
library(lubridate)
library(car)
# Summary Statistics
OR<- df1 %>% group_by(Created.at.month=floor_date(Created.date)) %>%
summarize(totalsales=sum(Order.price) )
QR<- df1 %>% group_by(Created.at.month=floor_date(Created.date)) %>%
summarize(noofitems=sum(Lineitem.quantity) )
DR<- df1 %>% group_by(Created.at.month=floor_date(Created.date)) %>%
summarize(totaldiscount=sum(Lineitem.discount) )

sum_df <- cbind(OR,QR,DR)
sum_df<- sum_df[ -c(3 , 5) ]
sum_df["Unitprice"]  <- sum_df$totalsales / sum_df$noofitems
# Final dataset for creating a model
sum_df[49082:49092,]
### Creating a linear model
linear_model <- lm( sum_df$noofitems ~sum_df$Unitprice)
summary(linear_model)
# Test to explain Autocorrelation
durbinWatsonTest(linear_model)
# In this model both intercept and Coefficient are significant. And the model overall significance 
# is very high indicated by p-value: < 2.2e-16. Errors are very less for the degree of freedom
# Only problem in this model is R sqaure is very less. But to be a a good model R quare need not be  high.
# Some times even 10% explenation of variantion is also good enough.
# durbinWatsonTest  The output shows that there is no autocorrelation issues in the model
###############################################
# The coefficient indicates that for every additional raise in item price by one unit there is decrese
# in demand by  0.23.
##############################
