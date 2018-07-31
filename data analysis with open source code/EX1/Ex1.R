mean_and_var <- function(x, use.sum=TRUE){
#'  mean_and_var - return a list with two components named mean and var
#'  x - numric vector or metrix    
#'  use.sum - if use.sum set to false calculte the mean and var using for loops. if set to true (default) use the sum() function

  
  stopifnot(is.numeric(x),length(x) > 1)
  if (use.sum) {
    mean <- sum(x)/length(x)
    var <- sum(x^2)/length(x)-mean^2
  } 
  else{
    s <-  0
    s2 <-  0
    for (i in as.vector(x)) {
      s <- s+i      
      s2 <- s2+i^2
    }
    mean <- s/length(x)
    var <- s2/length(x)-mean^2
  }
  res <-  c(mean,var)
  return (res)
}


# Q1 ----------------------------------------------------------------------


#' test mean_and_var
set.seed(1000)
#' Q1
#' Q1_a
x <- rnorm(5,50,50)
r1 <- mean_and_var(x)
print(r1)
#' Q1_b
y <- matrix(1:1000,1000,1000)
r2 <- mean_and_var(y)
print(r2)
t1 <- system.time(mean_and_var(y))['elapsed']
print(t1)
t2 <- system.time(mean_and_var(y,use.sum = FALSE))['elapsed']
print(t2)

# Q2 ----------------------------------------------------------------------


#' Q2
#' Q2_a
#' Create a matrix with 200 rows and 5 columns filled with random numbers from a normal distribution with mean=5 and var=100. (What is the corresponding standard deviation sd?)
#' the corresponding standard deviation is sqrt(100) = 10 
mat <- matrix(rnorm(200*5,5,10),200,5) 
col_mean <- apply(mat,2,mean)
print(col_mean)
col_var <- apply(mat,2,var)
print(col_var)
#' Q2_b
mat2 <- matrix(rnorm(20000*5,5,10),20000,5) 
col_mean2 <- apply(mat2,2,mean)
print(col_mean2)
col_var2 <- apply(mat2,2,var)
print(col_var2)
#' it seem that result is almost the same 
#' Q2_c
#' we can use the same seed number and compere the results 


# Q3 ----------------------------------------------------------------------
data <- read.csv("2017 World Happiness Report.csv",header = TRUE,sep = ',')

#' number of observation 
print(nrow(data))
#' number of variables
print(ncol(data))
test_data <- subset(data, select = -c(Happiness.Rank, Whisker.high, Whisker.low, Dystopia.Residual) )
colnames(test_data)[colnames(test_data)=="Happiness.Score"] <- "Happiness"
colnames(test_data)[colnames(test_data)=="Economy..GDP.per.Capita."] <- "Economy"
colnames(test_data)[colnames(test_data)=="Health..Life.Expectancy."] <- "Health"
colnames(test_data)[colnames(test_data)=="Trust..Government.Corruption."] <- "Trust"
#' the type for each class
sapply(test_data, class)
#' The diffrent regions in data
levels((test_data$Region))
#' the mim\max\mean happiness score
summary(test_data$Happiness)

library(ggplot2)
#' need to improved
p1 <- ggplot(data = test_data,mapping = aes(x=test_data$Economy,y=test_data$Happiness))+ geom_point(aes(color= 'cly'))+ geom_smooth(method = 'lm')+ xlab("Economy")+ ylab("Happiness")
p2 <- ggplot(data = test_data,mapping = aes(x=test_data$Family,y=test_data$Happiness))+ geom_point(aes(color= 'r'))+ geom_smooth(method = 'lm')+ xlab("Family")+ ylab("Happiness")
p3 <- ggplot(data = test_data,mapping = aes(x=test_data$Health,y=test_data$Happiness))+ geom_point(aes(color= 'r'))+ geom_smooth(method = 'lm')+ xlab("Health")+ ylab("Happiness")
p4 <- ggplot(data = test_data,mapping = aes(x=test_data$Freedom,y=test_data$Happiness))+ geom_point(aes(color= 'r'))+ geom_smooth(method = 'lm')+ xlab("Freedom")+ ylab("Happiness")
p5 <- ggplot(data = test_data,mapping = aes(x=test_data$Generosity,y=test_data$Happiness))+ geom_point(aes(color= 'r'))+ geom_smooth(method = 'lm')+ xlab("Generosity")+ ylab("Happiness")
p6 <- ggplot(data = test_data,mapping = aes(x=test_data$Trust,y=test_data$Happiness))+ geom_point(aes(color= 'r'))+ geom_smooth(method = 'lm')+ xlab("Trust")+ ylab("Happiness")

ggarrange(p1,p2,p3,p4,p5,p6, ncol = 2,nrow = 3) 

#' What can you tell from the plots?
#' we can see that in corrletion between happines and Economy, Family, Health and Freedom 

ggplot(test_data,aes(x=test_data$Economy,y=test_data$Happiness, color = test_data$Region)) + geom_point() + 
  labs(x="Economy",y="Happiness") +
  geom_smooth(method = 'lm',se = FALSE)



ggplot(data = test_data,aes(Region)) + geom_bar(aes(fill = Region))

ggplot(data = test_data,aes(x=Region, y=Happiness)) + 
  geom_boxplot(mapping = aes(x = reorder(Region,Happiness,FUN=median),y=Happiness)) +
  geom_boxplot(aes(fill = Region)) 

#' Which Region has the most variability in Happiness?
#' The region with most variability is Middle East and Northern Africa
#' Which Region has the least variability?
#' The region with least variability is Australia and New Zealand
#' hat is the problem with the answer to this last question?
#' I think the problem is last qution is number of country in region and variability? in diffrent aspects 


library(GGally)


ggpairs(test_data, columns = 3:9)

#' What does this function plot?
#' the function plot the distrabution, corrlation and scattor plot of all variblae 
#' What does it show on the diagonal?
#' we see the distrabution of variable
#' Above the diagonal?
#' corrlation btewwen two variabales 
#' Below the diagonal?
#' scatter plots
#' What can you learn from this plot that you couldn't tell from the plots you created in section (b)?
#' 
  








