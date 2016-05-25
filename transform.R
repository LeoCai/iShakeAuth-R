I = matrix(
  c(1,0,0,
    0,1,0,
    0,0,1),nrow = 3, ncol = 3, byrow = T
)

getUpdateMatrix = function(cuMatrix, gyr, dt) {
  gyr = as.numeric(gyr); dt = as.numeric(dt)
  delta = sqrt((gyr[1] * dt) ^ 2 + (gyr[2] * dt) ^ 2 + (gyr[3] * dt) ^ 2)
  B = matrix(
    c(0,-gyr[3] * dt,    gyr[2] * dt,
      gyr[3] * dt,   0,-gyr[1] * dt,-gyr[2] * dt,  gyr[1] * dt,     0),
    nrow = 3, ncol = 3, byrow = TRUE
  )
  B1 = B * (sin(delta) / delta)
  B2 = (1 - cos(delta)) / (delta ^ 2) * (B %*% B)
  updateMatrix = I + B1 + B2
  return(cuMatrix %*% updateMatrix)
}

updateMatrixByGYR = function(gyr, dt, cuMatrix) {
  delta = as.numeric(sqrt((gyr[1] * dt) ^ 2 + (gyr[2] * dt) ^ 2 + (gyr[3] * dt) ^ 2))
  if(delta == 0) return(cuMatrix);
  B = matrix(
    c(
      as.numeric(0), as.numeric(-gyr[3] * dt), as.numeric(gyr[2] * dt),
      as.numeric(gyr[3] * dt), as.numeric(0), as.numeric(-gyr[1] * dt),
      as.numeric(-gyr[2] * dt), as.numeric(gyr[1] * dt), as.numeric(0)
    ),
    nrow = 3, ncol = 3, byrow = TRUE
  )
  B1 = B * (sin(delta) / delta)
  B2 = (1 - cos(delta)) / (delta ^ 2) * (B %*% B)
  updateMatrix = I + B1 + B2
  return(cuMatrix %*% updateMatrix)
}

updateMatrixByMultiGYR = function(gyrs, initMatrix,dt) {
  cuMatrix = initMatrix
  matrixs = list()
  (lenG = nrow(gyrs))
  for (i in 1:lenG) {
    # cuMatrix = updateMatrixByGYR(gyrs[i,1:3],gyrs[i,4],cuMatrix)
    cuMatrix = updateMatrixByGYR(gyrs[i,1:3],dt,cuMatrix)
    matrixs = rbind(matrixs,as.vector(cuMatrix))
  }
  return (matrixs)
}

getGlobleAccs = function(accs, rtMats) {
  globleAccs = list()
  for(i in 1:nrow(accs)){
    rtMat = matrix(as.numeric(rtMats[i,]), nrow= 3, ncol = 3 )
    glaccs = rtMat %*% matrix(
      as.numeric(accs[i,]),nrow = 3, ncol = 1, byrow = T
    )
    globleAccs = rbind(globleAccs,as.numeric(c(glaccs[1,],glaccs[2,],glaccs[3,])))
  }
  return (globleAccs)
}

getInitMatrix = function(gaccInit, initTheta) {
  vgaccInit = as.numeric(c(gaccInit[1],gaccInit[2],gaccInit[3]))
  normGcc = vgaccInit / (norm(vgaccInit,type = "2"))
  gx = normGcc[1]
  gy = normGcc[2]
  gz = normGcc[3]
  r = 1
  fi = pi/2 - atan2(sqrt(gx ^ 2 + gy ^ 2),gz)
  theta = initTheta
  xInit = convertFromSphericalToCardinal(r,fi,theta)
  xInit = xInit / (norm(matrix(xInit),type = "2"))
  yInit = product(normGcc,xInit)                     #wrong!!!!
  yInit = yInit / (norm(matrix(yInit),type = "2"))
  initMatrix = matrix(
    c(gx,gy,gz,
      xInit[1],xInit[2],xInit[3],
      yInit[1],yInit[2],yInit[3]),
    nrow = 3,ncol = 3
  )
  return (initMatrix)
}

convertFromSphericalToCardinal = function(r, fi, theta) {
  x = r * sin(fi) * cos(theta)
  y = r * sin(fi) * sin(theta)
  z = r * cos(fi)
  return(c(x,y,z))
}

product <- function(x, y, i=1:3) {
  # Project inputs into 3D, since the cross product only makes sense in 3D.
  To3D <- function(x) head(c(x, rep(0, 3)), 3)
  x <- To3D(x)
  y <- To3D(y)
  
  # Indices should be treated cyclically (i.e., index 4 is "really" index 1, and
  # so on).  Index3D() lets us do that using R's convention of 1-based (rather
  # than 0-based) arrays.
  Index3D <- function(i) (i - 1) %% 3 + 1
  
  # The i'th component of the cross product is:
  # (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
  # as long as we treat the indices cyclically.
  return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
            x[Index3D(i + 2)] * y[Index3D(i + 1)])
}

carlibrate = function(rm2,rotationAxis, gyromatrix){
  mindist = 10000000;  bestAngle = 0;  bestRm = 0
  
  for(angle in 0:360){
    rm = RotationMatrix(angle/pi*180, rotationAxis);dist = mean((rm2%*%rm - gyromatrix)^2)
    if(dist<mindist){mindist = dist; bestAngle = angle;bestRm = rm2%*%rm}
    # print(paste(dist,angle))
  }
  # print(paste(mindist,bestAngle))
  return (bestRm)
}


getInitMatrix(c(2,1,0), 0)

updateMatrixByGYR(c(0,pi,0), 0.5, I)

#convertFromSphericalToCardinal(1,pi/2,0)
