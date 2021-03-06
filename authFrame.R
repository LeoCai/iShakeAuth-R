#认证框架，主入口
DEBUG = F

source("./readData.R")
source("./train.R")
source("./levelCrossing.R")
source("./extractRandomness.R")
source("./reconcilation.R")
source("./performance.R")
library(entropy)
library(zoo)

codeTable = loadCodeTable()

expResult = list()

expSet = c(1:7,9:12,14:15)

# expSet = c(2)

for (exp in expSet) {
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
  smoothed_converted_data_alice = rollapply(converted_data_alice,5,mean)
  smoothed_converted_data_bob = rollapply(converted_data_bob,5,mean)
  
  rs = bestLevelCrossing(smoothed_converted_data_alice, smoothed_converted_data_bob)
  bits_alice_bob = rs$bits
  bits_alice = bits_alice_bob$a
  bits_bob = bits_alice_bob$b
  
  # bits_reconcilation = reconcilation(bits_alice,bits_bob)
  bits_reconcilation = bits_alice
  key = randomnessExtract(bits_reconcilation,codeTable)
  
  bitsLen = length(bits_alice)
  mismatch = getMismatch(bits_alice,bits_bob)
  time = nrow(data_alice)/50
  mimatchRate = mismatch / time
  metric = rs$metric
  bitrate = bitsLen / time
  et = entropy(key)
  
  expResult[[exp]] = list(
    cor = trainParameter$maxCor, bitrate = bitrate,mimatchRate = mimatchRate, metric = metric,entropy = et,bitLen = bitsLen, key =
      key
  )
  
  print(key)
}

resMt = matrix(nrow = 15,ncol = 6)
for(i in expSet){
  resMt[i,1] = expResult[[i]]$cor
  resMt[i,2] = expResult[[i]]$bitrate
  resMt[i,3] = expResult[[i]]$mimatchRate
  resMt[i,4] = expResult[[i]]$metric
  resMt[i,5] = expResult[[i]]$entropy
  resMt[i,6] = expResult[[i]]$bitLen
}
res = as.data.frame(resMt)
names(res) = c("cor","bitrate","mismatchrate","metric","entopy","bitlen")

write.table(res,"./results/expResut-smooth.csv",sep = ",")
# entropy(converted_data_alice[,1])
