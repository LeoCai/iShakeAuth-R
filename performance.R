#性能相关函数

computePerformance = function(bits, mismatchNum,numExchanged = 0, time) {
  numExtracted = length(bits)
  bitRate = getBitRate(numBits, time)
  entropyPerBit = entropy(bits)
  secretBitsRate = getSecretBitsRate(numExtracted, numExchanged, entropyPerBit,time)
  metric = getMetric(entropyPerBit, numBits, numExchanged, time)
}

getBitRate = function(numBits, time) {
  return(numBits / time)
}

getSecretBitsRate = function(numExtracted, numExchanged, entropyPerBit,time) {
  return((numExtracted - numExchanged * entropyPerBit) / time)
}


getMetric = function(entropyPerBit, numBits, numExchanged, time) {
  return(entropyPerBit * (numBits - numExchanged) / time)
}

getMismatch = function(alice, bob) {
  return(sum((alice - bob) != 0))
}

getLevelCrossingMetric = function(mismatchNum,correct, time,scalParam = 50) {
  misRate = mismatchNum / time; correctRate = correct / time
  if (misRate > scalParam)
    return(0)
  else{
    return(correctRate * (1 - misRate / scalParam))
  }
}
