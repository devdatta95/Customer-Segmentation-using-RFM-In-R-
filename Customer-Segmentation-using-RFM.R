###############################################################
#                       RFM ANALYSIS                          # 
###############################################################

library(lubridate)
library(dplyr)
library(doBy)
library(data.table)

# IMPORT THE DATASET 

data <- read.table(file.choose(),header=F)
head(data)

# find the count of Mising values from each columns

colSums(is.na(data))

# 1. Customer id
# 2. Date of the transaction
# 3. No. of CD purchased
# 4. Dollar value of the transaction

# Select 1,2,4 

data <- as.data.frame(cbind(data[,1],data[,2],data[,4]))
head(data)

# Enter header 

names <- c("ID","Date","Amount")
names(data) <- names

# Convert the date into std. format 

data[,2] <- as.Date(as.character(data[,2]),"%Y%m%d")

head(data)

length(data$ID)
length(unique(data$ID))
max(data$Date)
min(data$Date)

# 1 year, 6 months.
# 546 Days.


#working in doby library and groupwise summary statistic

# The following code will sum the Aount and count the total number of times the Amount is 
# paid under the same customerID

Cust_Sum<- summaryBy(Amount~ID, data = data,
                     FUN=c(sum,length))
# The sum will calculate total Amount paid by the customer and 
# length will calculate how many times the customer visited the site

# Rename the header

Cust_Sum<- rename(Cust_Sum, Amt_sum = Amount.sum, Frequency=Amount.length)

names(Cust_Sum)

# Calculate the Max date for each customer 
# last transaction date for each customer is the Max date
# ----CAUTION-----Takes a while to run----------->

data$Max_date <- with(data, ave(Date, ID, FUN=max))
head(data)

# Join data and cust_sum by id 
# ID is the grouping variable 
# FUN is a Function to apply on the grouped data.

DB <- merge(data,Cust_Sum,by="ID")

# Remove the duplicate entries

DB2 <- DB[!duplicated(DB$ID),]
DB2<- DB2[-c(2,2:3)]

# group_by work in dplyr library
library(dplyr)

# After 33 Days

df_RFM <- DB2 %>%
  group_by(ID) %>%
  summarise(recency=as.numeric(as.Date("1998-08-01")-max(Max_date)),
            frequenci=Frequency, monitery=Amt_sum)

summary(df_RFM)
head(df_RFM)

# CREATING R, F, M LEVELS

# rankR 1 IS VERY RECENT WHILE RANKR 5 IS LEAST RECENT
df_RFM$rankR = cut(df_RFM$recency, 5, labels = F) 

#rankF 1 IS MOST FREQUENT WHILE rankF 5 IS LEST FREQUENT
df_RFM$rankF =  cut(df_RFM$frequenci, 5, labels = F) 
df_RFM$rankF <- with(df_RFM,ifelse(rankF == 5,1,ifelse(rankF == 4,2,ifelse(rankF==3,3,ifelse(rankF==2,4,5)))))

# rankM 1 IS LOWEST SALES WHILE rankM 5 IS HIGHEST SALES
df_RFM$rankM = cut(df_RFM$monitery, 5, labels = F)
df_RFM$rankM <- with(df_RFM,ifelse(rankM == 5,1,ifelse(rankM == 4,2,ifelse(rankM==3,3,ifelse(rankM==2,4,5)))))

# TOTAL RANK

attach(df_RFM)
df_RFM$Total <- (rankR*100)+(rankF*10)+(rankM)


# df_RFM <- df_RFM[order(-df_RFM$FinalRank),]

# 5*5*5 = 125 Diffrent combination 

#################### THE END  ################################
