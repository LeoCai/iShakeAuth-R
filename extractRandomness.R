loadCodeTable = function(){
  for (i in 1:CODING_LEN){
    keys = generateKey(i, CODING_LEN)
    codesize = log2(length(keys))
    if(codesize == 0) codesize = 1
    for(j in 1:length(keys)){
      codeTable[keys[j]] = val
    }
  }
  return(codeTable)
}

randomnessExtract = function(bits){
  subStrings = subStringByMalkov(bits, MALKOV_ORDER)
  codeTable(subStrings, CODING_LEN)
}

codeTable = function(subStrings, CODING_LEN){
  for(str in subStrings){
    if((i+CODING_LEN)>strLen) key = substr(str,i,strLen)
    else key = substr(str,i, i+CODING_LEN)
    code = codeTable[key]
    for(j in 1:codeLen) finalKey = paste(finalKey, code,sep = "")
  }
  return(finalKey)
}