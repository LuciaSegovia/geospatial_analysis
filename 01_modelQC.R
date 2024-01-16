
library(dplyr)

check <- read.csv(here::here("Se_raw_OK_maize.csv"))
data.df  <- readRDS(here::here("data", "inter-output", "mwi-grain-se_raw.RDS")) # cleaned geo-loc maize Se data (2 datasets)

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
