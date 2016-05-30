levelCrossing = function(d1, d2,alpha = 0.2) {
  alice = c()
  bob = c()
  for (i in 1:3) {
    tempBits1 = generateTempBits(as.numeric(d1[,i]),alpha)
    tempBits2 = generateTempBits(as.numeric(d2[,i]),alpha)
    bitsOneDim = bitsByCooperate(tempBits1,tempBits2,2)
    alice = c(alice,bitsOneDim$a)
    bob = c(bob,bitsOneDim$b)
  }
  return(list(
    a = alice,b = bob,bitLen = length(alice)
  ))
}

generateTempBits = function(d,alpha = 0.2) {
  meanData = mean(d); sdData = sd(d)
  q_plus = meanData + alpha * sdData; q_minus = meanData - alpha * sdData
  tempBits = ifelse(d > q_plus,1,2)
  tempBits = ifelse(d < q_minus,0,tempBits)
  return(tempBits)
}

bitsByCooperate = function(alice, bob,m) {
  excurtionIndexesAlice = getExcurtionIndexes(alice,m)
  excurtionIndexesBob = getIndexesFromBob(excurtionIndexesAlice, bob,m)
  return(list(a = alice[excurtionIndexesBob], b = bob[excurtionIndexesBob]))
}

getExcurtionIndexes = function(d, m = 3) {
  len = length(d)
  indexes = c()
  preBit = d[1]
  consecutive = 1;
  for (i in 2:len) {
    cuBit = d[i]
    
    if (cuBit == preBit && cuBit != 2)
      consecutive = consecutive + 1
    else {
      consecutive = 1; preBit = cuBit
    }
    
    if (consecutive == m) {
      consecutive = 0
      indexes = c(indexes, i - (m - 1) / 2)
    }
  }
  return(indexes)
}

getIndexesFromBob = function(excurtionIndexesAlice, tempBits, m = 3) {
  len = length(excurtionIndexesAlice)
  indexes = c()
  for (i in 1:len) {
    index = excurtionIndexesAlice[i]
    target = tempBits[index]
    hasExcurtion = T
    start = index - (m - 1) / 2; end = index + (m - 1) / 2
    
    for (j in start:end) {
      if (tempBits[j] != target ||
          tempBits[j] == 2) {
        hasExcurtion = F; break;
      }
    }
    
    if (hasExcurtion)
      indexes = c(indexes, index)
  }
  return(indexes)
}

bestLevelCrossing = function(converted_data_alice, converted_data_bob){
  bestAlpha = 0;  maxMetric = 0;  bestMismatch = 0
  bestRes = list()
  for (alpha in seq(0.01,1,0.02)) {
    rs = levelCrossing(converted_data_alice, converted_data_bob,alpha)
    mismatchNum = getMismatch(rs$a,rs$b)
    correct = length(rs$a) - mismatchNum
    time = (nrow(converted_data_alice) - 1) * 0.02
    metric = getLevelCrossingMetric(mismatchNum,correct, time)
    if(metric>maxMetric){
      maxMetric = metric
      bestAlpha = alpha
      bestRes = rs
      bestMismatch = mismatchNum
    }
    print(paste("alpha:",alpha,"  bitsLen",rs$bitLen,"  mis",mismatchNum,"  metric",metric))
  }
  return(list(bits = bestRes, alpha = bestAlpha, mismatch = bestMismatch ,metric = maxMetric))
}

if (DEBUG) {
  bestLevelCrossing(converted_data_alice, converted_data_bob)
#   bestAlpha = 0
#   maxMetric = 0
#   for (alpha in seq(0.01,1,0.02)) {
#     rs = levelCrossing(converted_data_alice, converted_data_bob,alpha)
#     mismatchNum = getMismatch(rs$a,rs$b)
#     correct = length(rs$a) - mismatchNum
#     time = (nrow(converted_data_alice) - 1) * 0.02
#     metric = getLevelCrossingMetric(mismatchNum,correct, time)
#     if(metric>maxMetric){
#       maxMetric = metric
#       bestAlpha = alpha
#     }
#     print(paste("alpha:",alpha,"  bitsLen",rs$bitLen,"  mis",mismatchNum,"  metric",metric))
#   }
}
