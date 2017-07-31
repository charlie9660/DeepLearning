require(quantmod)
require(RSNNS)

data <- read.csv('data.csv',na.strings = ',')
data <- data[,'DEXUSEU']
data <- data[data!='.']
plot(1:length(data),data,type = 'l')
write.csv(data,"out.csv")
data <- as.ts(data)
data <- log(data)
data <- as.ts(scale(data))
data <- as.zoo(data)
x_1 <- Lag(data,k=1)
x_2 <- Lag(data,k=2)
x_3 <- Lag(data,k=3)
x_4 <- Lag(data,k=4)
x_5 <- Lag(data,k=5)

table <- cbind(data,x_1,x_2,x_3,x_4,x_5)

n_train = 45
table <- table[-(1:5),]
n = nrow(table)
set.seed(500)
n_train <- 400
train <- sample(1:n,n_train,FALSE)
head(table)
inputs<- table[,2:6]
outputs<- table[,1]

fit <- elman(inputs[train],outputs[train],size = c(10,10),learnFuncParams=c(0.1),maxit=1000)
plotIterativeError(fit)
summary(fit)
pred<-predict(fit,inputs[-train])

plot(outputs[-train])
lines(pred,col='red')
cor(outputs[-train],pred)^2
