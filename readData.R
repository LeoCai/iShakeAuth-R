
#get target coloumn of data
getTargetData = function(data){
  targetData = cbind(data[,c(4:12,19:21)])
  return(targetData)
}

#smooth data using filters
smooth = function(data){
  return(data)
}

#read data from csv file
readData = function(file){
  data = read.csv(file)
  data = getTargetData(data)
  data = smooth(data)
  return(data)
}

#align data using cross correlation, return aligned data
#data1:base data
#data2:data to be adjusted
#return data2 aligned indexes
testAlign = function(data1, data2,s1,e1,tag = "") {
  c1 = sqrt(data1[,1] ^ 2 + data1[,2] ^ 2 + data1[,3] ^ 2)
  c2 = sqrt(data2[,1] ^ 2 + data2[,2] ^ 2 + data2[,3] ^ 2)
  c11 = c1[s1:e1]
  maxCor = 0
  maxS2 = s1
  maxE2 = e1
  range = round(s1 / 2)
  for (i in 1:range) {
    s2 = s1 - range / 2 + i
    e2 = e1 - range / 2 + i
    c22 = c2[s2:e2]
    cvdata = cor(c11,c22)
    # print(paste(s2,e2,cvdata))
    if (cvdata > maxCor) {
      maxS2 = s2; maxE2 = e2
      maxCor = cvdata
    }
    # print(cvdata)
  }
  c22 = c2[maxS2:maxE2]
  # matplot(1:length(c11),cbind(c11,c22),type = "l",main = paste(tag,round(maxCor, 4)))
  # print(paste(tag,"[",maxS2,":",maxE2,",]", sep = ""))
  # print(round(maxCor, 4))
  # print("")
  return (c(maxS2,maxE2))
  # matplot(1:length(c1),cbind(c1,c2),type="l",main = round(cor(c1,c2), 4) )
}

#algin data using cross correlation
cross_correlation = function(d1, d2){
  s1 = 2; e1 = 50
  r = testAlign(d1,d2,s1,e1)
  return (list(alice=d1[s1:e1,], bob = d2[r[1]:r[2],]))
}

#get train data : first several data of data
getTrainData = function(d){
  return(d[1:20,])
}

data_alice = readData("./datas/alice.csv")
data_bob = readData("./datas/bob.csv")
d = cross_correlation(data_alice,data_bob)
getTrainData(d$alice)
# plot(data_alice[,10],type="l")
