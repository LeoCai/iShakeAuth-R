CODING_LEN = 4
MALKOV_ORDER = 4

loadCodeTable = function() {
  codeTable = list()
  for (i in 0:CODING_LEN) {
    keys = generateKey(i, CODING_LEN)
    codesize = ceiling(log2(length(keys)))
    if (codesize == 0)
      codesize = 1
    rtmcode = sample(0:(length(keys) - 1),length(keys))
    for (j in 1:length(keys)) {
      codeTable[keys[j]] = sprintf(paste("%0",codesize,"d",sep = ""),as.numeric(number2binary(rtmcode[j])))
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
  permutations <<- list()
  cuIndex <<- 1
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

subStringByMalkov = function(bits, malkov_order) {
  len = (length(bits) / malkov_order - 1) * malkov_order
  stringNum = 2 ^ malkov_order
  subStrings = rep('',stringNum)
  for (i in 1:len) {
    strIndex = 0
    for (j in 0:(malkov_order - 1)) {
      strIndex = strIndex * 2 + bits[i + j]
    }
    subStrings[strIndex] = paste(subStrings[strIndex],bits[i + malkov_order],sep = "")
  }
  return(subStrings)
}

codeByTable = function(subStrings, CODING_LEN,codingTable) {
  finalKey = ""
  for (str in subStrings) {
    strLen = nchar(str)
    if (strLen != 0) {
      for (i in seq(1,strLen,CODING_LEN)) {
        if ((i + CODING_LEN - 1) > strLen){
          key = substr(str,i,strLen)
          b = 0
          for (m in 1:(i + CODING_LEN - 1 - strLen)) {
            key = paste(c(b,key),collapse = '')
            b = 1 - b
          }
        }
        else
          key = substr(str,i, i + CODING_LEN - 1)
        
        key = sprintf(paste("%0",CODING_LEN,"d",sep = ""),as.numeric(key))
        code = codingTable[key]
        finalKey = paste(finalKey, code,sep = "")
      }
    }
  }
  return(as.numeric(strsplit(finalKey,"")[[1]]))
}

DEBUG = F
if (DEBUG) {
  codingTable = loadCodeTable()
  randomnessExtract(c(1,1,0,1,0,0,1,1,0,1,1,0),codingTable)
}
