#1.a
# What proportion of the books
# were published in 2014? (Give your answer to 2 decimal places.) Of those
# books published in 2014, which were published by Penguin?

df<- read.csv("Data/AmazonBestSellers2014(1)(13).csv")

sprintf(nrow(df[df$PublicationYear==2014,])/nrow(df),fmt = "%#.2f")
#sprintf(22.2034,fmt = "%#.2f")



#b
# Use the aggregate function to compute the average number of reviews, review rating
# and price for each genre in the datset. Label the columns of the resulting data frame
# appropriately. Which genre has the lowest mean number of reviews? Which
# has the highest mean price? 

df %>%
  group_by(Genre) %>%
    summarise(avgRating = mean(Stars,na.rm = TRUE),
              avgPrice = mean(Price,na.rm = TRUE))

df %>%
  group_by(Genre) %>%
  summarise(avgRating = mean(Stars,na.rm = TRUE),
            avgPrice = mean(Price,na.rm = TRUE)
            ) %>%
    filter(avgRating==min(avgRating) | avgPrice==max(avgPrice))

#c
# Use par(mfrow=c(1,2)) to create a 1×2 plotting matrix. The left-hand plot should
# contain a boxplot of the number of reviews for the books published in 2014. The
# right hand plot should contain a boxplot of the number of reviews for the books
# published published prior to 2014. Label axes, include titles and use colour. Ensure
# the limits of the y axes match in the two plots. Comment on the resulting plots.
df1<-df[df$PublicationYear==2014,]
df2<-df[df$PublicationYear<2014,]

par(mfrow = c(1,2))
boxplot(NumReviews~PublicationYear,data=df1,xlab = "Year",ylab="reviews",main="Reviews in 2014",col="green")
boxplot(NumReviews~PublicationYear,data = df2,xlab = "Year",ylab="reviews",main="Reviews in 2014",col="green")

#interpreting box plots

#t.test
t.test(x=df$Price[df$Genre == "ChildrensBooks"],y=df$Price[df$Genre == "ContemporaryFiction"])

#explanation
# Based on the result, you can say: at 95% confidence level, there is no significant
# difference (p-value = 0.0794) of the two means. Here you should accept the null hypothesis
# that the two means are equal because the p-value is larger than 0.05. The maximum difference
# of the mean can be as low as -3.37 and as high as 0.21. The output also produces estimates
# of the sample means, the mean and the degree of freedom of the t-distribution.
# Note that Welch’s t-test is a t-test with unequal variances.


model3 <- lm(formula = Price ~ NumReviews + Stars + NumPages, data = df)
summary(model3)


# 
# Y<- as.matrix(df$Price)
# X<-matrix(data=0,ncol = 5,nrow = nrow(df))
# X[1,]
# X[,1]
# X[,1]<-1
# X[,2]<-df$NumReviews
# X[,2]<-df$NumPages
# X[,2]<-df$NumReviews
# X[,3]<-df$NumPages
# X[,4]<-df$Stars
# X[,5]<-df$PublicationYear
# View(X)
# View(Y)
# beta <- ((t(X)*X)^-1)*t(X)Y
# beta <- (inv((t(X)*X))*t(X)Y
# beta <- (inv((t(X)*X))*t(X)*Y
# )
# beta <- ((t(X)*X)^-1)*t(X)*Y
# beta <- ((t(X)%*%X)^-1)*t(X)*Y
# beta <- ((t(X)*X)^-1)*t(X)*Y
# t(X)
# t(X)*X
# t(X)%*%X
# beta <- ((t(X)%*%X)^-1)%*%t(X)%*%Y
# View(beta)
# PY <- X%*%beta
# View(PY)
# plot(PY,col="red")
# line(PY,col="red")
# lines(PY,col="red")
# lines(Y,col="green")
# lines(PY,col="red")
# plot(PY,col="red")
# lines(Y,col="green")
# plot(Y,col="green")
# df2$Y <- Y
# df2 <- data.frame()
# df2$Y<-Y
# df2$Y<-as.data.frame(Y)
# plot(x,y1,type="l",col="red")
# lines(x,y2,col="green")
# plot(Y,type="l",col="red")
# lines(x,y2,col="green")
# lines(PY,col="green")
# plot(type="n",col="red")
# plot(Y,PY,type="n",col="red")
# lines(PY,col="green")
# lines(Y,col="red")
# View(Y)
# beta <- (solve(t(X)%*%X))%*%t(X)%*%Y
# PY <- X%*%beta
# View(PY)
# plot(Y,PY,type="n",col="red")
# lines(PY,col="green")
# plot(Y,PY,type="n",col="red")
# lines(PY,col="green")
# lines(Y,col="red")
# cor(Y,PY)



#Binning attribute tenure
# tdata <- mutate(tdata, tenure_bin = tenure)
# 
# tdata$tenure_bin[tdata$tenure_bin >=0 & tdata$tenure_bin <= 12] <- '0-1 year'
# tdata$tenure_bin[tdata$tenure_bin > 12 & tdata$tenure_bin <= 24] <- '1-2 years'
# tdata$tenure_bin[tdata$tenure_bin > 24 & tdata$tenure_bin <= 36] <- '2-3 years'
# tdata$tenure_bin[tdata$tenure_bin > 36 & tdata$tenure_bin <= 48] <- '3-4 years'
# tdata$tenure_bin[tdata$tenure_bin > 48 & tdata$tenure_bin <= 60] <- '4-5 years'
# tdata$tenure_bin[tdata$tenure_bin > 60 & tdata$tenure_bin <= 72] <- '5-6 years'

#tdata$tenure_bin <- as.factor(tdata$tenure_bin)

#summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))






#PART 2

df3 <- read.csv("Data/FroudShips1907(1)(13).csv") 
df3 <-df3[df3$Power == "Sail" | df3$Power == "Steam",]

model1 <- glm(formula = CrewSize ~ Power+LogTonnage,data = df3,family = "poisson")
model1$coefficients

predict(model1,df3[10:15,])



#PART 3 profiling
# Rprof(tmp <- tempfile())
# example(glm)
# Rprof()
# summaryRprof(tmp)
