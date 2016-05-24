iterTheta = function(initTheta1, initTheta2){
  initMatrix6A = getInitMatrix(gaccInit6A, initTheta6A)
  initMatrix88 = getInitMatrix(gaccInit88, initTheta88)
  
  (matrix6A = updateMatrixByMultiGYR(gyr_6A, initMatrix6A,0.02))
  (matrix88 = updateMatrixByMultiGYR(gyr_88, initMatrix88,0.02))
  
  accGloble6A = getGlobleAccs(lacc6A, matrix6A)
  accGloble88 = getGlobleAccs(lacc88, matrix88)
  
  c1 = abs(cor(as.numeric(accGloble6A[,1]),as.numeric(accGloble88[,1])))
  c2 = abs(cor(as.numeric(accGloble6A[,2]),as.numeric(accGloble88[,2])))
  c3 = abs(cor(as.numeric(accGloble6A[,3]),as.numeric(accGloble88[,3])))
  print(paste(initTheta88,"sd=",sd(c(c1,c2,c3)), "mean=",mean(c(c1,c2,c3)),c1,c2,c3))
  return(list(cor = c(c1,c2,c3), globleAcc6A = accGloble6A, globleAcc88 = accGloble88, iterCor = c(c1,c2,c3)))
}

tarin = function(d1, d2){
  maxCor = 0; maxIndex = 1
  for( i in 1:iterNum){
    rs = iterTheta(initTheta1, initTheta2)
    if(rs$maxCor > maxCor) {maxCor = rs$maxCor; maxIndex = rs$maxIndex}
  }
  return(iterTheta(initTheta1, initTheta2))
}