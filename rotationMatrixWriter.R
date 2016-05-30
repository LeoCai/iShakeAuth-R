
indexGacc = 7:9
indexMag = 13:15
indexGyr = 10:12
indexDt = 22

getMutipleRtms = function(fileName,MAG = T){
  rtms_mag_list = data.frame()
  rtms_gyro_list = data.frame()
 
  
  ds = read.csv(fileName)
  
  #first data
  d = ds[1,]
  gacc = as.numeric(d[indexGacc]); mag = as.numeric(d[indexMag]); gyr = as.numeric(d[indexGyr]);dt = as.numeric(d[indexDt])
  rtm_mag = getRtmByMag(gacc, mag)
  rtm_gyr = rtm_mag
  rtms_mag_list = rbind(rtms_mag_list,rtm_mag)
  rtms_gyro_list = rbind(rtms_gyro_list,rtm_gyr)
  ###
  
  for(i in 2:nrow(ds)){
    d = ds[i,]
    gacc = as.numeric(d[indexGacc]); mag = as.numeric(d[indexMag]); gyr = as.numeric(d[indexGyr]);dt = as.numeric(d[indexDt])
    rtm_mag = getRtmByMag(gacc, mag)
    rtm_gyr = getRtmByGyr(rtm_gyr, gyr, dt)
    rtms_mag_list = rbind(rtms_mag_list,rtm_mag)
    rtms_gyro_list = rbind(rtms_gyro_list,rtm_gyr)
  }
  names(rtms_mag_list) = 1:9
  names(rtms_gyro_list) = 1:9
  return(list(rtms_mag_list = rtms_mag_list,rtms_gyro_list = rtms_gyro_list))
}

getRtmByMag = function(gv,mag){
  Ax = gv[1]; Ay = gv[2]; Az = gv[3]
  Ex = mag[1]; Ey = mag[2]; Ez = mag[3]
  Hx = Ey*Az - Ez*Ay; Hy = Ez*Ax - Ex*Az; Hz = Ex*Ay - Ey*Ax
  normH = sqrt(Hx*Hx + Hy*Hy + Hz*Hz)
  invH = 1.0 / normH
  Hx = Hx*invH; Hy = Hy*invH; Hz = Hz*invH
  invA = 1.0 / sqrt(Ax*Ax + Ay*Ay + Az*Az)
  Ax = Ax*invA; Ay = Ay*invA; Az = Az*invA
  Mx = Ay*Hz - Az*Hy; My = Az*Hx - Ax*Hz; Mz = Ax*Hy - Ay*Hx
  return (c(Hx,Mx,Ax,
            Hy,My,Ay,
            Hz,Mz,Az))
}

getRtmByGyr = function(rtm_gyr, gyr, dt){
  rtm_gyr = matrix(rtm_gyr,nrow = 3, ncol = 3,byrow = T)
  rtm = getUpdateMatrix(rtm_gyr,gyr,dt)
  return (c(rtm[1,],rtm[2,],rtm[3,]))
}

result = getMutipleRtms("./datas/mobile_1.csv")
write.csv(result$rtms_mag_list,"./datas/rtm_mag.csv",row.names = F)
write.csv(result$rtms_gyro_list,"./datas/rtm_gyr.csv",row.names = F)

