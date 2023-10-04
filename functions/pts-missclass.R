
# Function from DHS Spatial report 8
# Appendix B.3 Point-in-Polygon Analysis
#
#  Citation: Perez-Heydrich, Carolina, Joshua L. Warren, Clara R. Burgert, and Michael E. Emch. 2013. Guidelines
#  on the Use of DHS GPS Data. Spatial Analysis Reports No. 8. Calverton, Maryland, USA: ICF Interna-
#  tional.
#  
#  
#  
#  
#Output from Function Call:
# Matrix with one entry for each location:

# 1) Weighted Average of Polygon Values
# 2) Most Probable Polygon Value
# 3) Missclassification Error

#Notes:
#1) Rural_Code={0,1};
 ##0: Maximum Rural Distance is 5,000m
 ##1: Maximum Rural Distance is 10,000m

#2) n_Approximation; We suggest 10,000 or larger. Larger values will lead to
#improved approximations but will take more time.

#3) NA_Option={0,1};
 ##0: Leaves missing polygon values as missing in weighted means
 ##1: Removes the missing polygon values and reweights the remaining polygon
#values to calculate the weighted means.


# Function
point_in_polygon_fun<-function(Observed_DHS_Points,Polygon,Polygon_Values,
                               Rural_Code,n_Approximation,NA_Option){
  #Packages
  require(rgdal)
  require(maptools)
  require(rgeos)
  require(spatstat)
  require(splancs)
  require(fields)
  
  Observed_DHS_Points<-
   spTransform(Observed_DHS_Points, CRS("+proj=utm +zone=36 +datum=WGS84"))
  Polygon<-spTransform(Polygon, CRS("+proj=utm +zone=36 +datum=WGS84"))
  
  #Assigning Offset Distances
  n<-length(Observed_DHS_Points)
  
  #Typical Case
  if(Rural_Code==0){
    offset.dist<-ifelse(Observed_DHS_Points$URBAN_RURA=="U", 2000, 5000)
  }
  #Worst Case Scenario
  if(Rural_Code==1){
    offset.dist<-ifelse(Observed_DHS_Points$URBAN_RURA=="U", 2000, 10000)
  }
  
  final<-as.data.frame(matrix(0,nrow=n,ncol=3))

  
  for(i in 1:n){
    
    #Creating Buffer Around Point with Maximum Offset as Radius
    pdsc<-disc(radius = offset.dist[i], centre = c(coordinates(Observed_DHS_Points)[i,1],
                                                   coordinates(Observed_DHS_Points)[i,2]))
    
    pdsc<-as(pdsc, "SpatialPolygons")
    proj4string(pdsc) <- CRS("+proj=utm +zone=36 +datum=WGS84")
   
     #Filling in the Buffer with Points
    rpt<-csr(pdsc@polygons[[1]]@Polygons[[1]]@coords, n_Approximation)
    rpt<-SpatialPoints(rpt)
    proj4string(rpt)<-CRS("+proj=utm +zone=36 +datum=WGS84")
   
     #Which Region is each Point in?
    ov<-over(rpt, Polygon)
    # ov<-ov[is.na(ov[,1])==0,] #Removing Points Outside of the Polygon
    ov<-na.omit(ov) #Removing Points Outside of the Polygon
    proportions<-matrix(0,nrow=length(unique(ov[,1])),ncol=2)
    
     for(j in 1:length(unique(ov[,1]))){
   
       #Proportion of Points in this Region
      proportions[j,1]<-mean(ov[,1]==unique(ov[,1])[j])
      #Polygon Value for the Proportion
      proportions[j,2]<-Polygon_Values[unique(ov[,1])[j]]
    }
    
    
    #Determining Polygon of Observed Point
    ov_point<-over(Observed_DHS_Points[i,], Polygon)
    
    #Leave Missing Polygon Values as Missing
    if(NA_Option==0){
      #Continous Polygon Value
      final[i,1]<-proportions[,1]%*%proportions[,2]
      #Discrete Polygon Value
      final[i,2]<-proportions[proportions[,1]==max(proportions[,1]),2]
    }
   
     #Reweighting the Non-Missing Polygon Values
    if(NA_Option==1){
      #Removing the Missing Observations
      proportions_1<-proportions[is.na(proportions[,2])==0,1]
      proportions_2<-proportions[is.na(proportions[,2])==0,2]
      if(length(proportions_1)>0){
        
        #Reweighting the Proportions
        proportions_1<-proportions_1/sum(proportions_1)
        #Continous Polygon Value
        final[i,1]<-proportions_1%*%proportions_2
        #Discrete Polygon Value
        final[i,2]<-proportions_2[proportions_1==max(proportions_1)]
      }
      if(length(proportions_1)==0){
        #Continous Polygon Value
        final[i,1]<-NA
        #Discrete Polygon Value
        final[i,2]<-NA
      }
    }
    #Probability of Missclassification
    final[i,3]<-1-proportions[(unique(ov[,1])==ov_point[,1]),1]
   
     #Completion Percentage
    print(c("Percent Complete", 100*round(i/n,2)))
    # print misclassification
    # print(final[i,])
    
    }
  
  return(final)
}