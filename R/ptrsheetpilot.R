

ptrsheetspilot<- function(objecttype){
  
  runs<- c(1:3)
  scramlev<- c(1,4,16,64,256)
  
  for (runno in runs) {
    data<-stuff[[runno]]
    weight<- rep(1,4)
    length<- rep(3,4)
    time<- rep(NA, 4)
    
    for (scram in scramlev){
      time<- round(data$stim0_on[data$tiling==scram], digits = 1)
      ptr<- cbind(time, length, weight)
      filename<- sprintf('pilot ptr sheets/%s_ptr_run%d_scram%d_pilot.csv',objecttype, runno, scram)
      write.csv(ptr, file = filename, quote = FALSE, row.names = FALSE)
    }
    
    
  }
}
