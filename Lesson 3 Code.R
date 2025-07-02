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




#####
## SECTION 3.2. FUNCTIONS TO SUMMARIZE VARIABLES

#Raw frequencies of discrete variables
p1table <-table(store.df$p1price)
p1table
str(p1table)
plot(p1table)

#Raw frequencies in relation of another variable, prices in relation to the product being promoted
p1table2 <- table(store.df$p1price, store.df$p1prom)
p1table2

#Obtaining just the frequencies of the times that the product was promoted within each price
prop_table2 <- p1table2[,2]/(p1table2[,2]+p1table2[,1])
prop_table2

prop_table3 <- p1table2[1,]/(p1table2[1,]+p1table2[2,]+p1table2[3,]+p1table2[4,]+p1table2[5,])
prop_table3

#or the corresponding prop table
prop_table2 <- prop.table(p1table2, margin =1)
prop_table2

prop_table3 <- prop.table(p1table2, margin =2)
prop_table3
#####
## SECTION 3.2.2. CONTINUOUS VARIABLES

min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1sales)
median(store.df$p1sales)
var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)
quantile(store.df$p1sales, probs = c(.25,.50,.75))
quantile(store.df$p1sales, probs = 0:10/10)


mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df)<- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"]<- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"]<- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"]<- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"]<- IQR(store.df$p2sales)
mysummary.df

#####
## SECTION 3.3. SUMMARIZING DATA FRAMES

# with the summary function
summary(store.df)
summary(store.df$p1sales)
summary(store.df, digits = 2)

#with the describe function in psych package
library(psych)
describe(store.df)
describe(store.df[,c(2, 4:9)])

#with the apply function, for which we can apply any function we want

apply(store.df[,2:9], MARGIN = 2, FUN = mean) # MARGIN referst to the column (2) or the rows (1), FUN to the function we want to apply. 
apply(store.df[,2:9], 2, mean) # really not needed to especify the functions. We obtain the same. 
apply(store.df[,2:9], 2, sum) 
apply(store.df[,2:9], 2, sd) 

apply(store.df[,2:9], 2, function(x) {mean(x)- median(x) }) # an anonymous function using apply to see the difference between median and mean

mysummary.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df)<- c("Product 1", "Product 2")
mysummary.df[, "Median Sales"]<- apply(store.df[,4:5],2,median)
mysummary.df[, "IQR"]<- apply(store.df[,4:5],2,IQR)
mysummary.df


#####
## SECTION 3.4. SINGLE VARIABLE VISUALIZATIONS

#Histograms
hist(store.df$p1sales)

hist(store.df$p1sales,
     main= "Product 1 Weekly Sales Frequencies Across All Stores",
     xlab= "Product 1 Sales (Units)",
     ylab= "Count")
colors() #to see available colors in R

hist(store.df$p1sales,
     main= "Product 1 Weekly Sales Frequencies Across All Stores",
     xlab= "Product 1 Sales (Units)",
     ylab= "Count",
     breaks=30, # to specify the number of columns
     col="lightblue")

hist(store.df$p1sales,
     main= "Product 1 Weekly Sales Frequencies Across All Stores",
     xlab= "Product 1 Sales (Units)",
     ylab= "Relative frequency",
     breaks=30, # to specify the number of columns
     col="lightblue",
     freq=FALSE, # to get proportions instead of counts
     xaxt="n" #to eliminate the default tick marks
     )
axis(side=1, at=seq(60,300,by=20)) #separate command to modify the axes

lines(density(store.df$p1sales, bw=10),
      type ="l", col ="darkred", lwd=2) # bw is the smoothing, lwd is line width, type l refers to line type



#applying the histogram for product 2

hist(store.df$p2sales,
     main= "Product 2 Weekly Sales Frequencies Across All Stores",
     xlab= "Product 2 Sales (Units)",
     ylab= "Relative frequency",
     breaks=30, # to specify the number of columns
     col="lightblue",
     freq=FALSE, # to get proportions instead of counts
     xaxt="n" #to eliminate the default tick marks
)
axis(side=1, at=seq(60,300,by=20)) #separate command to modify the axes

lines(density(store.df$p2sales, bw=10),
      type ="l", col ="darkred", lwd=2)


## Boxplots

boxplot(store.df$p2sales,
     main= "Weekly Sales of Product 2",
     xlab= "Product Sales",
     ylab= "Product 2", 
     horizontal=TRUE
)

boxplot(store.df$p2sales~store.df$storeNum,
        main= "Weekly Sales of Product 2 by Stores",
        las=1, # so the axis info is read horizontally
        xlab= "Product Sales",
        ylab= "Product 2", 
        horizontal=TRUE
)

boxplot(p2sales~p2prom, data=store.df,
        main= "Weekly Sales of Product 2 by Promotion",
        yaxt="n",
        las=1, # so the axis info is read horizontally
        xlab= "Product Sales",
        ylab= "Promotion of Product 2?", 
        horizontal=TRUE
)
axis(side=2, at= c(1,2), labels =c("No", "Yes"))

##Beanplot

library(beanplot)

beanplot(p2sales~p2prom, data=store.df,
        main= "Weekly Sales of Product 2 by Promotion",
        yaxt="n",
        las=1, # so the axis info is read horizontally
        xlab= "Product Sales",
        ylab= "Promotion of Product 2?", 
        horizontal=TRUE, 
        what=c(0,1,1,0),
        log="",
        side="second"
)
axis(side=2, at= c(1,2), labels =c("No", "Yes"))


## QQ PLOTS AND LOGARITHMIC TRANSFORMATIONS

qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

#After the logarithmic transformation, we can see that the qqplot follows a normal distribution
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

hist(log(store.df$p1sales),
     main= "Product 1 Weekly Sales Frequencies Across All Stores",
     xlab= "Product 1 Sales (Units)",
     ylab= "Relative frequency",
     breaks=30, # to specify the number of columns
     col="lightblue",
     freq=FALSE, # to get proportions instead of counts
     xaxt="n" #to eliminate the default tick marks
)
axis(side=1, at=seq(4,6,by=0.2)) #separate command to modify the axes

lines(density(log(store.df$p1sales), bw=0.1),
      type ="l", col ="darkred", lwd=2)


##Cumulative Distributions Plots

plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly sales",
     ylab="Cumulative Proportion",
     xlab= c("P1 weekly sales, all stores", "90% of weeks sold <= 171 units"),
     yaxt ="n"     )
axis(side=2, at=seq(0,1,by=0.1), las=1,
     labels=paste(seq(0,100, by =10), "%", sep=" "))
abline(h=0.9, lty=3) # para marcar una línea horizontal (h) de puntos (lty=3) en mi punto de corte en percentil 90
abline(v=quantile(store.df$p1sales, pr=0.9),lty=3) #línea vertical(v) marcando la cantidad  del percentil 90


#Function by() para obtener funciones segregadas por variables grupales
by(store.df$p1sales, store.df$storeNum, mean)

segregation<-by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)
segregation
#Function aggregate() para obtener dataframes de resultados segregados por variables grupales
p1sales.sum <- aggregate(store.df$p1sales, by= list(country=store.df$country), sum)
p1sales.sum

p1prices_country <- aggregate(store.df$p1price, by= list(country=store.df$country), table)
p1prices_country




### Maps
install.packages("rworldmap")
install.packages("RColorBrewer")
p1sales.map<- joinCountryData2Map(p1sales.sum, joinCode="ISO2", 
                                  nameJoinColumn = "country")
p1sales.map
mapCountryData(p1sales.map, nameColumnToPlot="x",
               mapTitle= "Total P1 sales by country",
               colourPalette=brewer.pal(7,"Greens"),
               catMethod="fixedWidth",
               addLegend=FALSE
            )