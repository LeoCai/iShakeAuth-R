source("../handshake_9_11/model/GloableConvert.R")
library(zoo)

lowFilter = function(mdata,n = 3){
  # rollapply(mdata,n,mean)
  bf = butter(1, 1/8)
  b <- filter(bf, mdata)
  b[1:20] = mdata[1:20]
  return(b)
}

unitVector = function(vector) {
  absVector  = norm(matrix(vector),type = "2")
  if(absVector == 0) return (vector)
  return(vector / absVector)
}

absVector = function(v1, v2, v3) {
  return(sqrt(v1 ^ 2 + v2 ^ 2 + v3 ^ 2))
}

normVector = function(v1, v2, v3) {
  abs = absVector(v1, v2, v3)
  return(cbind(v1 / abs, v2 / abs, v3 / abs))
}

combineVectors = function (data) {
  return(sqrt(as.numeric(data[,1]) ^2 +as.numeric(data[,2]) ^2  +as.numeric(data[,3]) ^2 ))
}

filterGravity = function(ax, ay, az, len) {
  gravity = as.double(as.vector(c(ax[1],ay[1],az[1])))
  #gravity = as.double(as.vector(c(0,0,0)))
  alpha = 0.8
  
  gv1 = as.double(c(1:len))
  gv2 = as.double(c(1:len))
  gv3 = as.double(c(1:len))
  gvectors = data.frame(gv1, gv2, gv3)
  
  lv1 = as.double(c(1:len))
  lv2 = as.double(c(1:len))
  lv3 = as.double(c(1:len))
  lvectors = data.frame(lv1, lv2, lv3)
  
  for (i in 1:len) {
    gravity[1] = alpha * gravity[1] + (1 - alpha) * ax[i]
    gravity[2] = alpha * gravity[2] + (1 - alpha) * ay[i]
    gravity[3] = alpha * gravity[3] + (1 - alpha) * az[i]
    
    gvectors[i, 1] = gravity[1]
    gvectors[i, 2] = gravity[2]
    gvectors[i, 3] = gravity[3]
    
    lvectors[i, 1] = ax[i] - gravity[1]
    lvectors[i, 2] = ay[i] - gravity[2]
    lvectors[i, 3] = az[i] - gravity[3]
  }
  return (cbind(gvectors,  lvectors, combineVectors(gvectors)))
  
}

computeAngle = function(v1, v2){
  sum = 0
  for(i in 1:3){
    sum = sum + v1[i]*v2[i]
  }
  cosAngle = sum / (absVector(v1[1],v1[2],v1[3])*absVector(v2[1],v2[2],v2[3]))
  sign = 1
  
  return(sign*acos(cosAngle)*180/pi)
}

computeProjOnGravaty = function(lv, gv) {
  absProj = ((
    lv[,1] * gv[1] + lv[,2] * gv[2] + lv[,3] * gv[3]
  ) /
    absVector(gv[1], gv[2], gv[3])
  )
  
  nvg = gv/absVector(gv[1],gv[2],gv[3])
  results = data.frame()
  for(i in 1:length(absProj)){
    results = rbind(results,nvg*absProj[i])
  }
  return(results)
}

computeProjOnHori = function(lv, gv) {
  pg = computeProjOnGravaty(lv, gv)
  return(cbind(lv[,1] - pg[,1],lv[,2] - pg[,2],lv[,3] - pg[,3]))
}

computeProjOnHoriSingleData = function(lv, gv) {
  absProj = ((
    lv[1] * gv[1] + lv[2] * gv[2] + lv[3] * gv[3]
  ) /
    absVector(gv[1], gv[2], gv[3])
  )
  pg = gv/absVector(gv[1],gv[2],gv[3])*absProj
  return(cbind(lv[1] - pg[1], lv[2] - pg[2], lv[3] - pg[3]))
}

arrows3d = function(v,lim3d = c(-1,1),title="3d"){
  open3d()
  data = rbind(c(0,0,0),v)
  plot3d(data,type = "l", col="red", lwd = 5, xlim = lim3d, ylim = lim3d, zlim = lim3d,main  =title, cex.main = 1.5, cex.lab = 1.5,xlab = "right",ylab="forward",zlab="up")
  #   vec=rbind( c( 0, 0, 0 ), v )
  #   segments3d( vec )
  #   cone3d(base=vec[2,]-(vec[1,]+vec[2,]/6), 
  #          #this makes the head go 1/6th the length of the arrow
  #          rad=0.1,
  #          tip=vec[2,],
  #          col="blue",
  #          front="lines",
  #          back="lines")
}

crossProduct = function(a, b){
  c = rep(0,3)
  c[1] = a[2]*b[3] - a[3]*b[2]
  c[2] = a[3]*b[1] - a[1]*b[3]
  c[3] = a[1]*b[2] - a[2]*b[1]
  return(c)
}

dotProduct = function(a,b){
  return(a[1]*b[1]+a[2]*b[2]+a[3]*b[3])
}

absVector1 = function(v){
  return(sqrt(v[1]^2 + v[2]^2 + v[3]^2))
}

vectorToMatrix = function(v){
  return(matrix(v,nrow=3))
}

matrixToVector = function(matrixV){
  return (c(matrixV[1,1],matrixV[2,1],matrixV[3,1]))
}

rotationByMatrix = function(v, rotationMatrix){
  temp = rotationMatrix%*%vectorToMatrix(as.numeric(v))
  return(matrixToVector(temp))
}

sampleByData = function(mdata, step = 2){
  end = nrow(mdata)
  return(mdata[seq(1,end,step),])
}
