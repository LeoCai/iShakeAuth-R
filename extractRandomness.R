CODING_LEN = 4
MALKOV_ORDER = 4

loadCodeTable = function() {
  codeTable = list()
  for (i in 1:CODING_LEN) {
    keys = generateKey(i, CODING_LEN)
    codesize = log2(length(keys))
    if (codesize == 0)
      codesize = 1
    for (j in 1:length(keys)) {
      codeTable[keys[j]] = sprintf("%010d",as.numeric(number2binary(j)))
    }
  }
  return(codeTable)
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if (missing(noBits)) {
    return(paste(binary_vector, collapse = ''))
  } else {
    paste(binary_vector[-(1:(length(binary_vector) - noBits))], collapse = '')
  }
}

permutations = list()
cuIndex = 1
generateKey = function(numOne, n) {
  pList = c()
  recursiveGenerate(0,0,pList,numOne,n)
  keys = c()
  permutationSize = length(permutations)
  for (i in 1:permutationSize) {
    keys = c(keys,paste(permutations[[i]], collapse = ''))
  }
  return(keys)
}

recursiveGenerate = function(cuN, cuNumOne, pList, numOne, totalBit) {
  if (cuN == totalBit) {
    if (cuNumOne == numOne) {
      permutations[[cuIndex]] <<- pList; cuIndex <<- cuIndex + 1
    }
    return
  }else{
    cuN = cuN + 1
    recursiveGenerate(cuN, cuNumOne, c(pList,0), numOne, totalBit)
    if (cuNumOne < numOne)
      recursiveGenerate(cuN, cuNumOne + 1, c(pList,1), numOne, totalBit)
  }
  
}

randomnessExtract = function(bits,codingTable) {
  subStrings = subStringByMalkov(bits, MALKOV_ORDER)
  return(codeByTable(subStrings, CODING_LEN,codingTable))
}

subStringByMalkov = function(bits, malkov_order){
  len = (length(bits)/malkov_order - 1)*malkov_order
  stringNum = 2^malkov_order
  subStrings = rep('',stringNum)
  for(i in 1:len){
    strIndex = 0
    for(j in 1:malkov_order){
      strIndex = strIndex*2 + bits[i+j]
    }
    subStrings[strIndex] = paste(subStrings[strIndex],bits[i+malkov_order],sep = "") 
  }
  return(subStrings)
}

codeByTable = function(subStrings, CODING_LEN,codingTable) {
  for (str in subStrings) {
    strLen = nchar(str)
    for( i in 1:strLen){
      if ((i + CODING_LEN) > strLen)
        key = substr(str,i,strLen)
      else
        key = substr(str,i, i + CODING_LEN)
      code = codingTable[key]
      for (j in 1:codeLen)
        finalKey = paste(finalKey, code,sep = "")
    }
   
  }
  return(finalKey)
}

codingTable = loadCodeTable()
randomnessExtract(c(1,1,0,1,0,0,1,1,0,1,1,0),codingTable)