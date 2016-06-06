DEBUG = F

source("./readData.R")
source("./transform.R")
source("./utils.R")
source("./train.R")
source("./levelCrossing.R")
source("./extractRandomness.R")
source("./reconcilation.R")
source("./performance.R")

library(entropy)

codeTable = loadCodeTable()

expResult = list()

expSet = c(1:7,9:12,14:15)

expSet = c(2)

exp = 2




file_alice = paste("./datas/5_30/h1/aa",exp,"hh1.csv",sep = "")
file_bob = paste("./datas/5_30/h2/aa",exp,"hh2.csv",sep = "")
data_alice = readData(file_alice)
data_bob = readData(file_bob)
# data_alice = data_alice[250:500,];data_bob = data_bob[245:495,]
# matplot(cbind(data_alice[250:750,1],data_bob[250:750,1])[1:500,],type="l")
data_timealigned = cross_correlation(data_alice,data_bob)
data_alice = data_timealigned$alice; data_bob = data_timealigned$bob
# matplot(cbind(data_alice[,1],data_bob[,1])[1:250,],type="l")

trainDataFromBob = getTrainData(data_alice)
trainDataFromAlice = getTrainData(data_bob)

trainParameter = train(trainDataFromBob, trainDataFromAlice)

converted_data_alice = convertData(data_alice,trainParameter$initTheta1)
converted_data_bob = convertData(data_bob,trainParameter$initTheta2)

tdAlice = converted_data_alice[1:50,]
tdBob = converted_data_bob[1:50,]

pcaData = cbind(tdAlice[,1],tdBob[,1])
epca6A <- prcomp(pcaData,
                 center = F,
                 scale. = F)
newData = predict(epca6A, newdata = pcaData)

noiceAlice1 = tdAlice[,1] - newData[,1]
noiceBob1 = tdBob[,1] - newData[,1]

bobrawAndNoise = cbind(tdBob[,1],noiceBob1)
bobrawAndNoise = as.data.frame(bobrawAndNoise)
names(bobrawAndNoise) = c("raw","noice")
formulaPu= "noice~raw"
modelPu = lm(as.formula(formulaPu),bobrawAndNoise,interval = "prediction",na.rm = TRUE)
# summary(modelPu)
testBob1 = as.data.frame(converted_data_bob[51:250,1])
testAlice1 = as.data.frame(converted_data_alice[51:250,1])

names(testBob1) = c("raw")
names(testAlice1) = c("raw")

predictPu <-
  data.frame(predict(
    modelPu,testBob1,interval = "prediction",level = 0.95,se.fit = FALSE
  ))
fitBob1 = testBob1 - predictPu$fit

mean(newData[,1])
mean(converted_data_alice[,1])
mean(converted_data_bob[,1])
cor(testAlice1,testBob1)
cor(testAlice1,fitBob1)

matplot(cbind(converted_data_alice[,1],converted_data_bob[,1],newData[,1])[100:200,],type="l")
matplot(cbind(converted_data_alice[,1]-newData[,1],converted_data_bob[,1]-newData[,1]),type="l")

cor(converted_data_alice[,1],converted_data_bob[,1])
cor(converted_data_alice[,1],newData[,1])
cor(converted_data_bob[,1],newData[,1])
cor(converted_data_alice[,1],converted_data_alice[,1]-newData[,1])
cor(converted_data_alice[,1],converted_data_alice[,1]-newData[,1])

matplot(cbind(converted_data_alice[,1],converted_data_alice[,1]-newData[,1]),type = "l")

cor(converted_data_alice[,1],converted_data_alice[,1]-newData[,1])
cor(converted_data_alice[,2],converted_data_alice[,2]-newData[,1])

bobrawAndNoise = cbind(converted_data_bob[,1],converted_data_bob[,1]-newData[,1])
bobrawAndNoise = as.data.frame(bobrawAndNoise)
names(bobrawAndNoise) = c("raw","noice")
formulaPu= "noice~raw"
modelPu = lm(as.formula(formulaPu),bobrawAndNoise,interval = "prediction",na.rm = TRUE)
summary(modelPu)
predictPu <-
  data.frame(predict(
    modelPu,data_bob[,1],interval = "prediction",level = 0.95,se.fit = FALSE
  ))

