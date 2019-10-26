voxelX<-rep(NA, 5624)
voxelY<-rep(NA, 5624)
voxelZ<-rep(NA, 5624)
VCluster<-rep(NA, 5624)
VoxelClusters<- data.frame(voxelX, voxelY, voxelZ, VCluster)


for (i in 1:nrow(clusters)) {
  Vx<- 45
  Vy<- 63
  Vz<- 36
  
  X<- ceiling(clusters$X[i]/2)
  Y<- ceiling(clusters$Y[i]/2)
  Z<- ceiling(clusters$Z[i]/2)
  
  voxelX[i]<- Vx - X
  voxelY[i]<- Vy + Y
  voxelZ[i]<- Vz + Z
  VCluster[i]<-clusters$Cluster[i]
  
}