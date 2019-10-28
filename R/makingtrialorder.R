novell <- read_csv("Novel_stim.csv")


for (trial in 1:800){
  
  novell$Path[trial]<- sprintf('stim/%d_stim_obj%d.tif', novell$scramble[trial], novell$imageno[trial])
  
  
}


counter<- 1
for (trial in seq(from = 3, to = 1200, by = 3)) {

  if (novelll$imageno[trial] <= 120 && novelll$imageno[trial] >= 81){
  novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset10.tif', novelll$scramble[trial], novelll$imageno[trial])
  } else {
    
    novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset180.tif', novelll$scramble[trial], stuff[counter])
  counter<- counter + 1
  
  }
  
}

trials<- 41:80
stuff<- sample(trials)
counter<- 1
for (trial in seq(from = 3, to = 1200, by = 3)) {
  
  if (novelll$imageno[trial] <= 120 && novelll$imageno[trial] >= 81){
    #imageno<- novelll$imageno[trial]
    #number<- imageno - 80
    #novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset10.tif', novelll$scramble[trial], number)
  } else {
    imageno<- novelll$imageno[trial]
    number<- imageno - 80
    novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset180.tif', novelll$scramble[trial], stuff[counter])
    counter<- counter + 1
    
  }
  
}


for (trial in seq(from = 3, to = 1200, by = 3)) {
  
  if (novelll$imageno[trial] <= 120 && novelll$imageno[trial] >= 81){
    #imageno<- novelll$imageno[trial]
    #number<- imageno - 80
    #novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset10.tif', novelll$scramble[trial], number)
  } else {
    imageno<- novelll$imageno[trial]
    number<- imageno - 80
    novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset180.tif', novelll$scramble[trial], novelll$imageno[trial]-80)
    counter<- counter + 1
    
  }
  
}




for (trial in seq(from = 1, to = 1200, by = 3)) {
  
    imageno<- novelll$imageno[trial]
    number<- imageno - 80
    novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d.tif', novelll$scramble[trial], number)

  
}


for (trial in seq(from = 2, to = 1200, by = 3)) {

    novelll$Path[trial] <- sprintf('stim/Mask.tif')
    novelll$time[trial]<- .50
}


for (trial in seq(from = 3, to = 1200, by = 3)) {
  
  if (novelll$imageno[trial] <= 120 && novelll$imageno[trial] >= 81){
    novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset10.tif', novelll$scramble[trial], novelll$imageno[trial])
    novelll$time[trial]<- 3
  } else {
    novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset180.tif', novelll$scramble[trial], novelll$imageno[trial])
    novelll$time[trial]<- 3
  }
}


for (trial in seq(from = 3, to = 1200, by = 3)) {
  
  if (novelll$imageno[trial] <= 120 && novelll$imageno[trial] >= 81){
    #novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset10.tif', novelll$scramble[trial], novelll$imageno[trial])
    novelll$Wait[trial]<- 1
  } else {
    #novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset180.tif', novelll$scramble[trial], novelll$imageno[trial])
    novelll$Wait[trial]<- 2
  }
}



number<- c()
for (i in 1:400) {
  
  number<-c(number,rep(rnorm(1),3))
}

novelll$number<- number
novelll<- novelll[order(novelll$number),]



counter<- 1
Answer<-c()
Condition<- c()

for (trial in seq(from = 3, to = 1200, by = 3)) {
  
  if (novelll$imageno[trial] <= 120 && novelll$imageno[trial] >= 81){
    #novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset10.tif', novelll$scramble[trial], novelll$imageno[trial])
    Condition[counter]<- 'Same'
    Answer[counter]<- 1
    counter<- counter + 1
  } else {
    #novelll$Path[trial] <- sprintf('stim/%d_stim_obj%d_offset180.tif', novelll$scramble[trial], novelll$imageno[trial])
    Condition[counter]<- 'Different' 
    Answer[counter]<- 2
    counter<- counter + 1
  }
}



c<- c(41:80)
imagenos<-sample(c, size = 400, replace = TRUE)
for (i in 1:400) {
  if (data2$Condition[i] == 'Different') {
    image <- imagenos[i]
      if (image == data2$Imageno[i]) {
        image <- sample(imagenos, 1)
      } else {
        print('yay')
      }
   
     data2$Image2[i] <-
      sprintf('stim/%d_stim_obj%d_offset180.tif',
              data2$Scramble[i],
              image)
  } else {
    print('Same')
  }
}

novel<- data2[sample(nrow(data2),400),]




for (trial in 1:400){
  
  semtrials$Path1[trial]<- sprintf('stim/%d_stim_obj%d.tif',semtrials$Scramble[trial], semtrials$IMAGE_Num[trial])

  
}

semtrials<- data.frame(rep(NA, 400),rep(NA, 400),rep('NA', 400),rep(NA, 400),rep(NA, 400),rep(NA, 400),rep(NA, 400))
for (i in 1:400) {
  j<- trials[i]
  semtrials[i,]<- ST[j,]
}
