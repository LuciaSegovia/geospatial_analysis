
library(dplyr)

# Loading the data
check <- read.csv(here::here("data", "predicted", "Se_raw_OK_maize.csv"))
data.df  <- readRDS(here::here("data", "inter-output", "mwi-grain-se_raw.RDS")) # cleaned geo-loc maize Se data (2 datasets)
# Saving exponential OK
fname<-paste("data/OK/",paste(Sys.Date(), element,"_OK_exp",Crop.target,".csv",sep=""))

# Loading the resutls from the fitted variogram (CH)
fname<-paste0("data/OK/2024-05-03",element,"_OK_exp",Crop.target,".csv",sep="")

# Added here() to solve path issue
ok.df <- read.csv(here::here(fname), header = TRUE, as.is = "numeric")

# Housekeeping: Check NA and zeros
sum(is.na(data.df$Se_raw))
data.df <- subset(data.df, !is.na(Se_raw)) #excluding NA
sum(data.df$Se_raw == 0)
min(data.df$Se_raw[data.df$Se_raw>0])
# Changin zeros to min (for log transformation)
data.df$Se_raw[data.df$Se_raw == 0] <- min(data.df$Se_raw[data.df$Se_raw>0])

# Exponential from log transformed
check$predSe <- exp(check$Zhat)

left_join(data.df, check)

# Joinging the OK back transformation using the eq. and the normal expo
test <- left_join(ok.df, check, by = c("Longitude", "Latitude"))

# Plotting the "equivalent" values
plot(Zhat_exp ~ predSe, test)

hist(check$Zhat)
hist(check$predSe)
min(check$predSe)
sum(check$predSe<0)
mean(check$predSe)
hist(data.df$Se_raw)
min(data.df$Se_raw)

quantiles<-as.character(cut(check[,"kv"],quantile(check[,"kv"],
                                                       c(0,0.25,0.5,0.75,1)),include.lowest=T,
                            label=c("blue","green","yellow","red")))	


# Classified post-plot
dev.new()
options(scipen=5)
plot(check[,"Longitude"],check[,"Latitude"], # Lon & Lat variables
     pch=16,col=quantiles,asp=1,
     xlab="Longitude",ylab="Latitude",cex=0.5)
