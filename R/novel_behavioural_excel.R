for (i in 1:length(datan$Path)) {
  
  datan$Path2[i]<-sprintf('stim/%d_stim_obj%s_offset.tif', datan$scramble[i], datan$imageno[i])
  
}

datan$leftimage<- NA
datan$rightimage<- NA

for(i in 1:length(datan$Path)) {
  ind<- c[i]
 datan$leftimage[i]<-datan$Path[ind]
 datan$rightimage[i]<-datan$Path2[ind]
  
  
   
}
