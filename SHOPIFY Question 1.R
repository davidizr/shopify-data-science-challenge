#this was done in R 
#import libraries
library(readxl)
library(ggplot2)

#import data
data <- read_excel("Desktop/Shopify/Dataset.xlsx")

#Create the dataframe and rename
shopifydf<-data.frame(
  orderID<-data$order_id,
  shopID<-data$shop_id,
  userID<-data$user_id,
  amount<-data$order_amount,
  itemtotal<-data$total_items,
  date<-data$created_at
)

#a.)Think about what could be going wrong with our calculation. Think about a better way to evaluate this data
#calculate AOV
mean(shopifydf$amount)
#AOV=$3145.128. This is so high because there are outliers
#user 607 for example is an outlier as they order 2000 pairs of shoes at a time

#this summary function gives us an idea of how the data looks
summary(shopifydf$amount)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#90     163     284    3145     390  704000 
#the huge difference in median and mean shows that there's some huge outliers

#this function shows how many outliers there are (amount>$10,000)
length(shopifydf$amount[shopifydf$amount>10000])
#there are 63 outliers

#OPTION 1: We should use a metric that isn't effected by ouliers, like median
median(shopifydf$amount)
#the median value of order is $284

#OPTION 2: alternativly, we can calculate the average but exclude outliers such as orders over $10,000
mean(shopifydf$amount[shopifydf$amount<10000])
#this gives us a mean of #302.58

#OPTION 3: Another option is to calculate the mean of all the values in the interquartile range (between 25th-75th percentile)
mean(shopifydf$amount[shopifydf$amount>quantile(shopifydf$amount, .25) & shopifydf$amount<quantile(shopifydf$amount, .75)])
#The mean of the interquartile range is $275.41

#plotting
ggplot(shopifydf, aes(0,amount))+
  geom_violin()+
  scale_y_log10(breaks=c(100,1000,10000,100000,1000000),labels=c("100","1,000","10,000","100,000","1,000,000"))+
  geom_hline(yintercept=mean(amount),color="blue", linetype="dotted")+
  geom_text(aes(-0.1,mean(amount),label = "AOV=$3145.13" , vjust = -1), size=3)+
  geom_hline(yintercept=median(amount),color="blue", linetype="dotted")+
  geom_text(aes(-0.1,median(amount),label = "Median=$284" , vjust = -1),size=3)+
  ylab("Purchase Amount ($)")+
  xlab("Proportion")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#b.) What metric would you report for this dataset?
#The median is the easiest to deploy and is the easiest to explain to someone who doesn't understand the concepts of percentiles and outliers
#Its also the common metric used when mean is unreliable

#c.) What is its value?
#The value of the median is $284