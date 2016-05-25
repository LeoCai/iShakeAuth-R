generateTempBits = function(d){
  len = nrow(d)
  connectedData = rbind(d1[,1],d2[,2],d3[,3])
  meanData = mean(connectedData); sdData = sd(connectedData)
  q_plus = meanData + alpha*sdData; q_minus = meanData - alpha*sdData
  tempBits = ifelse(connectedData>q_plus,1,2)
  tempBits = ifelse(tempBits<q_minus,0,tempBits)
  return(tempBits)
}

bitsByCooperate = function(alice, bob){
  excurtionIndexesAlice = getExcurtionIndexes(alice,m)
  excurtionIndexesBob = getIndexesFromBob(excurtionIndexesAlice, bob)
  return(list(a = alice[excurtionIndexesBob,], b= bob[excurtionIndexesBob,]))
}

levelCrossing = function(d1, d2){
  tempBits1 = generateTempBits(d1)
  tempBits2 = generateTempBits(d2)
  bits = bitsByCooperate(tempBits1,tempBits2)
  return(bits)
}