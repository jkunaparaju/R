orders_export_1 <- read.csv('/Users/jyothi/Desktop/thinx/orders_export_1.csv', comment.char="~")
orders_export_2 <- read.csv('/Users/jyothi/Desktop/thinx/orders_export_2.csv', comment.char="~")
orders_export <- read.csv('/Users/jyothi/Desktop/thinx/orders_export.csv', comment.char="~")
#View(orders_export_2)
# Merging three datasets 
mergedf <- rbind( orders_export, orders_export_1,orders_export_2 )
#View(mergedf)

#  Remove # sign before Name field
mergedf$Name <- substring(mergedf$Name, 2)
mergedf$Billing.Zip <- substring(mergedf$Billing.Zip, 2)
mergedf$Shipping.Zip <- substring(mergedf$Shipping.Zip, 2)

#mergedf[["Subtotal"]][is.na(mergedf[["Subtotal"]])] <- 0
# Taking necessary columns
subDf <- subset(mergedf, select=c("Name", "Created.at","Lineitem.name","Lineitem.price","Lineitem.quantity" ,"Lineitem.discount","Lineitem.fulfillment.status","Lineitem.sku"))
#View(subDf)
# Selecting only Hiphugger items
p1 <- 'Hiphugger'
df1 <- subset(subDf, grepl(p1,Lineitem.name ) )
#View(df1)
summary(df1)
# Converting Created.at from string date 
df1$Created.date <- as.Date(df1$Created.at ,format= "%Y-%m-%d %H:%M:%S")
# There is one NA in discount.price
df1 <- na.omit(df1)
#View(df1)
# Finding Price after discount
attach(df1)
df1$PAD <- with(df1, (Lineitem.price  -(Lineitem.discount/Lineitem.quantity)))
df1$Order.price <- with(df1, (Lineitem.price*Lineitem.quantity)-Lineitem.discount)
library(dplyr)
library(lubridate)
# Group by 
price_df<- df1 %>% group_by(item.price=PAD) %>%
  summarize(quantity.sold=sum(Lineitem.quantity) )
#View(price_df)
price_df
# Normalizinf Data Frame.
scaled.dat <- scale(price_df)
sca <- as.data.frame(scaled.dat)
# Scaling Does not do much of difference in Regression
# Centering price to make Intercept significant
cq <- price_df$quantity.sold - mean(price_df$quantity.sol)
#cq
# 
linear_model1 <- lm( price_df$item.price ~ cq)
summary(linear_model1)
# We use this model becasue we are evaluating demand with price.
linear_model2 <- lm( cq  ~ price_df$item.price)
summary(linear_model2)
plot(linear_model2)
# R square is low. But rest of the curves seems ok. Though this model not that significant 
# try to find th price elsticity
mean_price <- mean(price_df$item.price)
mean_quantity <- mean(price_df$quantity.sold)
mean_price
mean_quantity
# Price elasticity is delta q / delta p 
PE <- 1040.1 *(mean_price/mean_quantity)
PE
# PE is high. and is postive that means unit variation(increase in price) increses demand. 
# Obviously it will not be true. There might be some other factors that influence the sales not price
# for this period
