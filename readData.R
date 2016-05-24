
getTargetData = function(data){
  targetData = cbind(data[,c(4:12,19:21)])
  return(targetData)
}
smooth = function(data){
  return(data)
}
readData = function(file){
  data = read.csv(file)
  data = getTargetData(data)
  data = smooth(data)
  return(data)
}

# readData("./datas/alice.csv")
