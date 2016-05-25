reconcilation = function(bits_alice, bits_bob) {
  for (i in 1:iterNum) {
    for (j in 1:blockNum) {
      binaryBlockParityCompare(block_alice, block_bob)
    }
  }
}

binaryBlockParityCompare = function(block_alice, block_bob) {
  if(compareParity(block_alice,block_bob)){
    return
  }
  while (subStart != subEnd) {
    subMid = (subStart + subEnd) / 2
    if(!compareParity(block_alice[subStart, subMid],block_bob[subStart, subMid])) subEnd = subMid
    else subStart = subMid +1
  }
  block_bob[target] = 1-block_bob[target]
}
