

ptrsheets<- function(objecttype,session){
  run <- c(0:4)
  stuff<- list()
  counter<- 1
  for (r in run){
    filename<-sprintf('data/p6- Holly/p6_%s_data_s%d_run%d.csv', objecttype, session, r)
    stuff[[counter]]<-read.csv(filename,header = TRUE)
    counter<- counter + 1
  }
  
  runs<- c(1:5)
  scramlev<- c(1,4,16,64,256)
  
  for (runno in runs) {
    data<-stuff[[runno]]
    weight<- rep(1,4)
    length<- rep(6,4)
    time<- rep(NA, 4)
    
    for (scram in scramlev){
      time<- round(data$stim0_on[data$tiling==scram], digits = 0)
      ptr<- cbind(time, length, weight)
      filename<- sprintf('pilot ptr sheets/Novel-Sem/%s_ptr_run%d_scram%d_pilot.txt',objecttype, runno, scram)
      write.table(ptr, file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    
    
  }
  #return(stuff)
}
