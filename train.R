source("./transform.R")
source("./utils.R")
train = function(d1, d2) {
  maxCor = 0; maxIndex = 1
  iterNum = 20
  for (i in 1:iterNum) {
    initTheta2 =  2 * pi / iterNum * i
    rs = iterTheta(d1, d2, 0, initTheta2)
    cuCor = mean(rs$cor)
    if (cuCor > maxCor) {
      maxCor = cuCor; maxIndex = i
    }
  }
  initTheta2 =  2 * pi / maxIndex * i
  # rs = iterTheta(d1, d2, 0, initTheta2)
  return(list(initTheta1 = 0, initTheta2 = initTheta2,maxCor = maxCor))
}

iterTheta = function(d1, d2, initTheta1, initTheta2) {
  gaccInit1 = d1[1,4:6]; gaccInit2 = d2[1,4:6]
  initMatrix1 = getInitMatrix(gaccInit1, initTheta1)
  initMatrix2 = getInitMatrix(gaccInit2, initTheta2)
  
  accGloble1 = gyroConvert(d1, initMatrix1)
  accGloble2 = gyroConvert(d2, initMatrix2)
  
  c1 = abs(cor(as.numeric(accGloble1[,1]),as.numeric(accGloble2[,1])))
  c2 = abs(cor(as.numeric(accGloble1[,2]),as.numeric(accGloble2[,2])))
  c3 = abs(cor(as.numeric(accGloble1[,3]),as.numeric(accGloble2[,3])))
  print(paste(initTheta2,"sd=",sd(c(c1,c2,c3)), "mean=",mean(c(c1,c2,c3)),c1,c2,c3))
  return(list(
    cor = c(c1,c2,c3), accGloble1 = accGloble1, accGloble2 = accGloble2
  ))
}

convertData = function(d, initTheta){
  initMatrix = getInitMatrix(as.numeric(d[1,4:6]), initTheta)
  return(gyroConvert(d, initMatrix))
}

gyroConvert = function(d, initMatrix) {
  firstData = d[1,]
  initGVPre = as.numeric(d[1,4:6])
  cuMatrixCarlibrated = initMatrix
  preMatrixCarlibrated = initMatrix
  carlibratedAccs = c()
  
  
  for (i in 2:nrow(d)) {
    sdata = d[i,]; lacc = d[i,1:3]; gyr = d[i,7:9]; gacc = d[i,4:6]; dt = d[i,12]
    cuMatrixCarlibrated = getUpdateMatrix(cuMatrixCarlibrated, gyr, dt)
    
    if (i %% 50 == 0) {
      accMatrix = getCarlibratedMatirx(gacc,initGVPre)
      cuMatrixCarlibrated = carlibrate(preMatrixCarlibrated %*% accMatrix,gacc,cuMatrixCarlibrated)
      
      initGVPre = gacc
      
      preMatrixCarlibrated = cuMatrixCarlibrated
    }
    
    carlibratedAcc = rotationByMatrix(lacc, cuMatrixCarlibrated)
    carlibratedAccs = rbind(carlibratedAccs, as.numeric(carlibratedAcc))
    
  }
  carlibratedAccs = rbind(as.numeric(firstData[1,1:3]),carlibratedAccs)
  return(carlibratedAccs)
}

getCarlibratedMatirx = function(computedInitGV, initRealGV){
  vectorBefore = computedInitGV; vectorAfter = initRealGV
  rotationAxis = crossProduct(vectorBefore, vectorAfter)
  rotationAngle = acos(dotProduct(vectorBefore, vectorAfter) / absVector1(vectorBefore)/absVector1(vectorAfter))
  rotationMatrix = RotationMatrix(rotationAngle, rotationAxis)
  return(rotationMatrix)
}
RotationMatrix = function(angle, u){
  u = as.numeric(u); angle = as.numeric(angle)
  norm = absVector1(u)
  u[1] = u[1] / norm; u[2] = u[2] / norm; u[3] = u[3] / norm 
  rotationMatrix = matrix(nrow = 3, ncol = 3)
  rotationMatrix[1,1] = cos(angle) + u[1]^2*(1-cos(angle))
  rotationMatrix[1,2] = u[1]*u[2]*(1-cos(angle)) - u[3]*sin(angle)
  rotationMatrix[1,3] = u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle))
  
  rotationMatrix[2,1] = u[3]*sin(angle) + u[1]*u[2]*(1-cos(angle))
  rotationMatrix[2,2] = cos(angle) + u[2]^2 * (1-cos(angle))
  rotationMatrix[2,3] = -u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle))
  
  rotationMatrix[3,1] = -u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle))
  rotationMatrix[3,2] = u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle))
  rotationMatrix[3,3] = cos(angle) + u[3]^2*(1-cos(angle))
  
  return(rotationMatrix)
}

# train(d$alice,d$bob)
