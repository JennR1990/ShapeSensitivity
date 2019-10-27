voxelX<-rep(NA, 5624)
voxelY<-rep(NA, 5624)
voxelZ<-rep(NA, 5624)
VCluster<-rep(NA, 5624)



for (i in 1:nrow(clusters)) {
  Vx<- 45
  Vy<- 63
  Vz<- 36
  
  X<- ceiling(clusters$X[i])
  if((X %% 2) == 0) {
  } else {
    if ( X < 0) {
      X <- X - 1
    } else {
      X<- X + 1      
    }
  }
  X<- X/2
  Y<- ceiling(clusters$Y[i])
  if((Y %% 2) == 0) {
  } else {
    if ( Y < 0) {
      Y <- Y - 1
    } else {
      Y<- Y + 1      
    }
  }
  Y<- Y/2
  Z<- round(clusters$Z[i])
  if((Z %% 2) == 0) {
  } else {
    if ( Z < 0) {
      Z <- Z - 1
    } else {
      Z<- Z + 1      
    }
  }
  Z<- Z/2

  
  voxelX[i]<- Vx - X
  voxelY[i]<- Vy + Y
  voxelZ[i]<- Vz + Z
  VCluster[i]<-clusters$Cluster[i]
  
}

VoxelClusters<- data.frame(voxelX, voxelY, voxelZ, VCluster)
