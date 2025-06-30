####
#Simulating the dataset about the sells of two products in 20 stores along 2 years

k.stores<-20 # we will use 
k.weeks<-104 #2 years of data

#epmpty dataframe
store.df <- data.frame(matrix(NA, ncol=10, nrow =k.stores*k.weeks))

names(store.df)<- c("storeNum", "Year", "Week", "p1sales", "p2sales", "p1price", "p2price","p1prom", "p2prom", "country")

str(store.df)


#filling the variables for the store number and coutry

store.num <- 101: (100+k.stores)
store.num
store.cty <- c(rep("US",3), rep ("DE", 5), rep ("GB", 3), rep ("BR", 2), rep ("JP", 4), rep ("AU", 1), rep ("CN", 2))
store.cty

store.df$storeNum <- rep(store.num, each = k.weeks)
store.df$country <- rep(store.cty, each = k.weeks)

store.df

#filling the variables for the weeks and years

store.df$Week <- rep(1:52, times = k.stores*2)
store.df$Year <- rep(rep (1:2, each =k.weeks/2), times=k.stores)

#checking the structure
str(store.df)

#store number and country as factors
store.df$storeNum <- factor(store.df$storeNum)
store.df$country <- factor(store.df$country)

str(store.df)
head(store.df, 120)
tail(store.df, 120)

# Completing the other variables, using a specific seed number for the results to be replicable
set.seed (98250)

#for promotion of the product
store.df$p1prom <- rbinom(n=nrow(store.df), size =1, p=0.1)
store.df$p2prom <- rbinom(n=nrow(store.df), size =1, p=0.15)

#for price of the product
store.df$p1price<- sample (x=c(2.19,2.29,2.49,2.79,2.99), size =nrow(store.df),replace =TRUE)
store.df$p2price<- sample (x=c(2.29,2.49,2.59,2.99,3.19), size =nrow(store.df),replace =TRUE)

#for sales
tmp.sales1 <-rpois(nrow(store.df), lambda =120)
tmp.sales2 <-rpois(nrow(store.df), lambda =100)

tmp.sales1 <- tmp.sales1*log(store.df$p2price)/ log(store.df$p1price)
tmp.sales2 <- tmp.sales2*log(store.df$p1price)/ log(store.df$p2price)

store.df$p1sales <- floor(tmp.sales1 * (1+store.df$p1prom*0.3))
store.df$p2sales <- floor(tmp.sales2 * (1+store.df$p2prom*0.4))

head(store.df)


#Checking the created dataset
install.packages("car")
library(car) # to see some of the values
some(store.df,10)
library(dplyr) # to select some of the numeric variables for later correlation


#Saving the created file in a backup object
store.df.c<-store.df

#Opening the authors' database
store.df<-read.csv("database chapter 3.csv")


#Checking the correlations in both dagtasets
numeric_data_dplyr <- store.df %>%
  select_if(is.numeric)

# Now calculate the correlation matrix
cor(numeric_data_dplyr)


numeric_data_dplyr.c <- store.df.c %>%
  select_if(is.numeric)


cor(numeric_data_dplyr.c)




