levelCrossing = function(d1, d2) {
  tempBits1 = generateTempBits(d1)
  tempBits2 = generateTempBits(d2)
  bits = bitsByCooperate(tempBits1,tempBits2)
  return(bits)
}

generateTempBits = function(d,alpha = 0.2) {
  len = nrow(d)
  connectedData = c(d[,1],d[,2],d[,3])
  meanData = mean(connectedData); sdData = sd(connectedData)
  q_plus = meanData + alpha * sdData; q_minus = meanData - alpha * sdData
  tempBits = ifelse(connectedData > q_plus,1,2)
  tempBits = ifelse(connectedData < q_minus,0,tempBits)
  return(tempBits)
}

bitsByCooperate = function(alice, bob) {
  excurtionIndexesAlice = getExcurtionIndexes(alice)
  excurtionIndexesBob = getIndexesFromBob(excurtionIndexesAlice, bob)
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

if (DEBUG) {
  levelCrossing(converted_data_alice, converted_data_bob)
}
