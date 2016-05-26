reconcilation = function(bits_alice, bits_bob) {
  iterNum = 20
  blockNum = 5
  for (i in 1:iterNum) {
    for (j in 1:blockNum) {
      if (compareParity(bits_alice,bits_bob)) {
        return(bits_alice)
      }
      
      while (subStart != subEnd) {
        subMid = (subStart + subEnd) / 2
        if (!compareParity(block_alice[subStart, subMid],block_bob[subStart, subMid]))
          subEnd = subMid
        else
          subStart = subMid + 1
      }
      
      block_bob[target] = 1 - block_bob[target]
      
    }
  }
}

compareParity = function(block_alice, block_bob) {
  masterEven = computeEven(block_alice)
  slaveEven = computeEven(block_bob)
  return(masterEven == slaveEven)
}

computeEven = function(block) {
  parity = 0
  even = F
  for (bit in block)
    parity = parity + bit
  return(parity %% 2 == 0)
}

if(DEBUG){
  reconcilation(c(1,0,1,1,1,1,0,0,0,1,0,1,1,0), c(1,1,0,0,0,1,0,1,1,0,1,0,1,1))
}
