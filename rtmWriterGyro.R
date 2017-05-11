


indexGacc = 7:9
indexGyr = 10:12
indexDt = 21 

getMutipleRtms = function(fileName,MAG = T) {
  rtms_gyro_list = data.frame()
  
  
  ds = read.csv(fileName)
  
  #first data
  d = ds[1,]
  gacc = as.numeric(d[indexGacc]);gyr = as.numeric(d[indexGyr]);dt = as.numeric(d[indexDt])
  rtm_gyr = c(1,0,0,0,1,0,0,0,1)
  rtms_gyro_list = rbind(rtms_gyro_list,rtm_gyr)
  ###
  
  for (i in 2:nrow(ds)) {
    d = ds[i,]
    gacc = as.numeric(d[indexGacc]); gyr = as.numeric(d[indexGyr]);dt = as.numeric(d[indexDt])
    rtm_gyr = getRtmByGyr(rtm_gyr, gyr, dt)
    rtms_gyro_list = rbind(rtms_gyro_list,rtm_gyr)
  }
  names(rtms_gyro_list) = 1:9
  return(rtms_gyro_list)
}



getRtmByGyr = function(rtm_gyr, gyr, dt) {
  rtm_gyr = matrix(rtm_gyr,nrow = 3, ncol = 3,byrow = T)
  rtm = getUpdateMatrix(rtm_gyr,gyr,dt)
  return (c(rtm[1,],rtm[2,],rtm[3,]))
}

result = getMutipleRtms("./datas/3_10_data/top/41.csv")
write.table(
  result,paste("./datas/rtms/top.csv",sep = ""),sep = ",", row.names =
    F, col.names =
    F
)

result = getMutipleRtms("./datas/3_10_data/leftpants/41.csv")
write.table(
  result,paste("./datas/rtms/leftpants.csv",sep = ""),sep = ",", row.names =
    F, col.names =
    F
)

writePair = function(expNum) {
  for (i in 1:2) {
    result = getMutipleRtms(paste("./datas/5_30/h",i,"/aa",expNum,"hh",i,".csv",sep = ""))
    write.table(
      result,paste("./datas/rtms/",expNum,"h",i,".csv",sep = ""),sep = ",", row.names =
        F, col.names =
        F
    )
  }
}
for(i in 11:15)
writePair(i)
