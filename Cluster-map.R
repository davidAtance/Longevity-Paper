##################################################################################################################################
##################################################################################################################################
#We need the information from the RData generated with 
#Estimate_Longevity_Indicators.R

load(file = "Indicadores_DM_NU.RData")
load(file = "Indicadores_FM_NU.RData")

load(file = "datos_NU.194.RData") #OK 
paisesNU <- paises

poph_NU <- read.table("DPmale_NU.txt", header = TRUE)
popm_NU <- read.table("DPfemale_NU.txt", header = TRUE)



pobNU <- c("AFG", "ALB", "DZA", "AGO", "ATG", "ARG", "ARM", "ABW", "AUS", 
           "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", 
           "BEN", "BTN", "BOL", "BIH", "BWA", "BRA", "BRN", "BGR", "BFA",
           "BDI", "CPV", "KHM", "CMR", "CAN", "CAF", "TCD", "CHL", "CHN", 
           "HKG", "MAC", "B77", "COL", "COM", "COG", "CRI", "CIV", "HRV", 
           "CUB", "CUW", "CYP", "CZE", "PRK", "COD", "DNK", "DJI", "DOM", 
           "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "ETH", "FJI", "FIN", 
           "FRA", "PYF", "GAB", "GMB", "GEO", "DEU", "GHA", "GRC", "GRD", 
           "GUM", "GTM", "GIN", "GNB", "GUY", "HTI", "HND", "HUN", "ISL", 
           "IND", "IDN", "IRN", "IRQ", "IRL", "ISR", "ITA", "JAM", "JPN", 
           "JOR", "KAZ", "KEN", "KIR", "KWT", "KGZ", "LAO", "LVA", "LBN", 
           "LSO", "LBR", "LBY", "LTU", "LUX", "MDG", "MWI", "MYS", "MDV", 
           "MLI", "MLT", "MRT", "MUS", "MEX", "FSM", "MNG", "MNE", "MAR",
           "MOZ", "MMR", "NAM", "NPL", "NLD", "NCL", "NZL", "NIC", "NER", 
           "NGA", "MKD", "NOR", "OMN", "PAK", "PAN", "PN1", "PRY", "PER", 
           "PHL", "POL", "PR1", "PRI", "QAT", "KOR", "MDA", "ROU", "RUS", 
           "RWA", "LCA", "VCT", "WSM", "STP", "SAU", "SEN", "SRB", "SYC", 
           "SLE", "SGP", "SVK", "SVN", "SLB", "SOM", "ZAF", "SDS", "ESP", 
           "LKA", "PSX", "SDN", "SUR", "SWE", "CHE", "SYR", "TJK", "THA", 
           "TLS", "TGO", "TON", "TTO", "TUN", "TUR", "TKM", "UGA", "UKR", 
           "ARE", "GBR", "TZA", "USA", "VIR", "URY", "UZB", "VUT", "VEN", 
           "VNM", "B28", "YEM", "ZMB", "ZWE")



##########################################################################################
############################### WHO ##########################################
##################################HOMBRES#################################################
datosh <- NULL
datosm <- NULL
for(i in 1:length(pobNU)){
  #i <- 1
  minyear <- min(paisesNU[[i]]$hombres$any)
  maxyear <- max(paisesNU[[i]]$hombres$any)
  years <- seq(minyear,maxyear, 5)
  nyears <- length(years)
  for(j in 1: nyears){
    any <- years[j]
    datosht <- data.frame(pob = pobNU[i], ano = any, 
                          ex0 = esperanzas_DM.NU[[i]]$e0[j, 1], ex65 = esperanzas_DM.NU[[i]]$e65[j, 1],
                          ex70 = esperanzas_DM.NU[[i]]$e70[j, 1], ex75 = esperanzas_DM.NU[[i]]$e75[j, 1],
                          EM = edad_modal_DM.NU[[i]][j,1], GI0 = gini.index_DM.NU[[i]][j,1], GI65 = gini.index_DM.NU[[i]][j,3],
                          GI70 = gini.index_DM.NU[[i]][j,5], GI75 = gini.index_DM.NU[[i]][j,7],
                          p65 = pob.llegarvivo_DM.NU[[i]][j,1], p70 = pob.llegarvivo_DM.NU[[i]][j, 3], 
                          p75 = pob.llegarvivo_DM.NU[[i]][j,5], SD0 = con.std.des_DM.NU[[i]][j,1], 
                          SD65 = con.std.des_DM.NU[[i]][j,3], SD70 = con.std.des_DM.NU[[i]][j,5], 
                          SD75 = con.std.des_DM.NU[[i]][j,7], EP0 = Eprep.vida_DM.NU[[i]][j,1], 
                          EP65 = Eprep.vida_DM.NU[[i]][j,3]) 
    datosmt <- data.frame(pob = pobNU[i], ano = any,
                          ex0 = esperanzas_DM.NU[[i]]$e0[j, 2], ex65 = esperanzas_DM.NU[[i]]$e65[j, 2],
                          ex70 = esperanzas_DM.NU[[i]]$e70[j, 2], ex75 = esperanzas_DM.NU[[i]]$e75[j, 2],
                          EM = edad_modal_DM.NU[[i]][j,2], GI0 = gini.index_DM.NU[[i]][j,2], GI65 = gini.index_DM.NU[[i]][j,4],
                          GI70 = gini.index_DM.NU[[i]][j,6], GI75 = gini.index_DM.NU[[i]][j,8],
                          p65 = pob.llegarvivo_DM.NU[[i]][j,2], p70 = pob.llegarvivo_DM.NU[[i]][j, 4], 
                          p75 = pob.llegarvivo_DM.NU[[i]][j,6], SD0 = con.std.des_DM.NU[[i]][j,2], 
                          SD65 = con.std.des_DM.NU[[i]][j,4], SD70 = con.std.des_DM.NU[[i]][j,6], 
                          SD75 = con.std.des_DM.NU[[i]][j,8], EP0 = Eprep.vida_DM.NU[[i]][j,2], 
                          EP65 = Eprep.vida_DM.NU[[i]][j,4]) 
    
    datosh <- rbind(datosh, datosht)
    datosm <- rbind(datosm, datosmt)
  }
  years_fm <- c(2020, 2025, 2030)
  nyears_fm <- length(years_fm)
  for(h in 1: nyears_fm){
    any <- years_fm[h]
    datosht <- data.frame(pob = pobNU[i], ano = any, 
                          ex0 = esperanzas_FM.NU[[i]][h,1], ex65 = esperanzas_FM.NU[[i]][h,3],
                          ex70 = esperanzas_FM.NU[[i]][h,5], ex75 = esperanzas_FM.NU[[i]][h, 7],
                          EM = edad_modal_FM.NU[[i]][h,1], GI0 = gini.index_FM.NU[[i]][h,1], GI65 = gini.index_FM.NU[[i]][h,3],
                          GI70 = gini.index_FM.NU[[i]][h,5], GI75 = gini.index_FM.NU[[i]][h,7],
                          p65 = pob.llegarvivo_FM.NU[[i]][h,1], p70 = pob.llegarvivo_FM.NU[[i]][h, 3], 
                          p75 = pob.llegarvivo_FM.NU[[i]][h,5], SD0 = con.std.des_FM.NU[[i]][h,1], 
                          SD65 = con.std.des_FM.NU[[i]][h,3], SD70 = con.std.des_FM.NU[[i]][h,5], 
                          SD75 = con.std.des_FM.NU[[i]][h,7], EP0 = Eprep.vida_FM.NU[[i]][h,1], 
                          EP65 = Eprep.vida_FM.NU[[i]][h,3]) 
    datosmt <- data.frame(pob = pobNU[i], ano = any, 
                          ex0 = esperanzas_FM.NU[[i]][h,2], ex65 = esperanzas_FM.NU[[i]][h,4],
                          ex70 = esperanzas_FM.NU[[i]][h,6], ex75 = esperanzas_FM.NU[[i]][h, 8],
                          EM = edad_modal_FM.NU[[i]][h,2], GI0 = gini.index_FM.NU[[i]][h,2], GI65 = gini.index_FM.NU[[i]][h,4],
                          GI70 = gini.index_FM.NU[[i]][h,6], GI75 = gini.index_FM.NU[[i]][h,8],
                          p65 = pob.llegarvivo_FM.NU[[i]][h,2], p70 = pob.llegarvivo_FM.NU[[i]][h, 4], 
                          p75 = pob.llegarvivo_FM.NU[[i]][h,6], SD0 = con.std.des_FM.NU[[i]][h,2], 
                          SD65 = con.std.des_FM.NU[[i]][h,4], SD70 = con.std.des_FM.NU[[i]][h,6], 
                          SD75 = con.std.des_FM.NU[[i]][h,8], EP0 = Eprep.vida_FM.NU[[i]][h,2], 
                          EP65 = Eprep.vida_FM.NU[[i]][h,4]) 
    
    datosh <- rbind(datosh, datosht)
    datosm <- rbind(datosm, datosmt)
    
  }
}
#Ya tengo creados todos los datos 
library(gganimate)
library(gifski)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)

###################################################################
###################################################################
###################################################################
#We are not going to use all the indicators,
#SO we need to anulate some columns of the data.frame
#hombres
datosh$p65 <- NULL
datosh$p70 <- NULL
datosh$p75 <- NULL
datosh$ex70 <- NULL
datosh$ex75 <- NULL
datosh$GI70 <- NULL
datosh$GI75 <- NULL
datosh$SD70 <- NULL
datosh$SD75 <- NULL
#mujeres
datosm$p65 <- NULL
datosm$p70 <- NULL
datosm$p75 <- NULL
datosm$ex70 <- NULL
datosm$ex75 <- NULL
datosm$GI70 <- NULL
datosm$GI75 <- NULL
datosm$SD70 <- NULL
datosm$SD75 <- NULL

#We only need the information for some specific periods 
hombres1990 <- datosh[datosh$ano == 1990, ]
hombres1995 <- datosh[datosh$ano == 1995, ]
hombres2000 <- datosh[datosh$ano == 2000, ]
hombres2005 <- datosh[datosh$ano == 2005, ]
hombres2010 <- datosh[datosh$ano == 2010, ]
hombres2015 <- datosh[datosh$ano == 2015, ]
hombres2020 <- datosh[datosh$ano == 2020, ]
hombres2025 <- datosh[datosh$ano == 2025, ]
hombres2030 <- datosh[datosh$ano == 2030, ]

mujeres1990 <- datosm[datosm$ano == 1990, ]
mujeres1995 <- datosm[datosm$ano == 1995, ]
mujeres2000 <- datosm[datosm$ano == 2000, ]
mujeres2005 <- datosm[datosm$ano == 2005, ]
mujeres2010 <- datosm[datosm$ano == 2010, ]
mujeres2015 <- datosm[datosm$ano == 2015, ]
mujeres2020 <- datosm[datosm$ano == 2020, ]
mujeres2025 <- datosm[datosm$ano == 2025, ]
mujeres2030 <- datosm[datosm$ano == 2030, ]

library(dplyr)
#Hombres 1990
hombres1990 <- data.frame(hombres1990)
hombres1990$pob <- NULL
hombres1990$ano <- NULL
hombres1990$pop <- NULL
head(hombres1990)

#Hombres 1995
hombres1995 <- data.frame(hombres1995)
hombres1995$pob <- NULL
hombres1995$ano <- NULL
hombres1995$pop <- NULL
head(hombres1995)

#Hombres 2000
hombres2000 <- data.frame(hombres2000)
hombres2000$pob <- NULL
hombres2000$ano <- NULL
hombres2000$pop <- NULL
head(hombres2000)

#Hombres 2005
hombres2005 <- data.frame(hombres2005)
hombres2005$pob <- NULL
hombres2005$ano <- NULL
hombres2005$pop <- NULL
head(hombres2005)

#Hombres 2010
hombres2010 <- data.frame(hombres2010)
hombres2010$pob <- NULL
hombres2010$ano <- NULL
hombres2010$pop <- NULL
head(hombres2010)

#Hombres 2015
hombres2015 <- data.frame(hombres2015)
hombres2015$pob <- NULL
hombres2015$ano <- NULL
hombres2015$pop <- NULL
head(hombres2015)

#Hombres 2020
hombres2020 <- data.frame(hombres2020)
hombres2020$pob <- NULL
hombres2020$ano <- NULL
hombres2020$pop <- NULL
head(hombres2020)

#Hombres 2025
hombres2025 <- data.frame(hombres2025)
hombres2025$pob <- NULL
hombres2025$ano <- NULL
hombres2025$pop <- NULL
head(hombres2025)

#Hombres 2030
hombres2030 <- data.frame(hombres2030)
hombres2030$pob <- NULL
hombres2030$ano <- NULL
hombres2030$pop <- NULL
head(hombres2030)

#Mujeres 1990
mujeres1990 <- data.frame(mujeres1990)
mujeres1990$pob <- NULL
mujeres1990$ano <- NULL
mujeres1990$pop <- NULL
head(mujeres1990)

#Mujeres 1995
mujeres1995 <- data.frame(mujeres1995)
mujeres1995$pob <- NULL
mujeres1995$ano <- NULL
mujeres1995$pop <- NULL
head(mujeres1995)

#Mujeres 2000
mujeres2000 <- data.frame(mujeres2000)
mujeres2000$pob <- NULL
mujeres2000$ano <- NULL
mujeres2000$pop <- NULL
head(mujeres2000)

#Mujeres 2005
mujeres2005 <- data.frame(mujeres2005)
mujeres2005$pob <- NULL
mujeres2005$ano <- NULL
mujeres2005$pop <- NULL
head(mujeres2005)

#Mujeres 2010
mujeres2010 <- data.frame(mujeres2010)
mujeres2010$pob <- NULL
mujeres2010$ano <- NULL
mujeres2010$pop <- NULL
head(mujeres2010)

#Mujeres 2015
mujeres2015 <- data.frame(mujeres2015)
mujeres2015$pob <- NULL
mujeres2015$ano <- NULL
mujeres2015$pop <- NULL
head(mujeres2015)

#Mujeres 2020
mujeres2020 <- data.frame(mujeres2020)
mujeres2020$pob <- NULL
mujeres2020$ano <- NULL
mujeres2020$pop <- NULL
head(mujeres2020)

#Mujeres 2025
mujeres2025 <- data.frame(mujeres2025)
mujeres2025$pob <- NULL
mujeres2025$ano <- NULL
mujeres2025$pop <- NULL
head(mujeres2025)

#Mujeres 2030
mujeres2030 <- data.frame(mujeres2030)
mujeres2030$pob <- NULL
mujeres2030$ano <- NULL
mujeres2030$pop <- NULL
head(mujeres2030)

#Hombres
length(hombres1990$ex0)
length(hombres1995$ex0)
length(hombres2000$ex0)
length(hombres2005$ex0)
length(hombres2010$ex0)
length(hombres2015$ex0)
length(hombres2020$ex0)
length(hombres2025$ex0)
length(hombres2030$ex0)

#Mujeres
length(mujeres1990$ex0)
length(mujeres1995$ex0)
length(mujeres2000$ex0)
length(mujeres2005$ex0)
length(mujeres2010$ex0)
length(mujeres2015$ex0)
length(mujeres2020$ex0)
length(mujeres2025$ex0)
length(mujeres2030$ex0)

indica <- c("ex0", "ex65", "EM", "GI0", "GI65",
            "SD0", "SD65", "EP0", "EP65")
mean_SD <- list(mean = list("1990" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "1995" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2000" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2005" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2010" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2015" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2020" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2025" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                            "2030" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres")))),
                sd = list("1990" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "1995" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2000" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2005" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2010" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2015" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2020" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2025" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres"))),
                          "2030" = matrix(NA, 9, 2, dimnames = list(indica, c("hombres", "mujeres")))))

mean_SD$mean$`1990`[,1] <- colMeans(hombres1990)
mean_SD$mean$`1990`[,2] <- colMeans(mujeres1990)
mean_SD$mean$`1995`[,1] <- colMeans(hombres1995)
mean_SD$mean$`1995`[,2] <- colMeans(mujeres1995)
mean_SD$mean$`2000`[,1] <- colMeans(hombres2000)
mean_SD$mean$`2000`[,2] <- colMeans(mujeres2000)
mean_SD$mean$`2005`[,1] <- colMeans(hombres2005)
mean_SD$mean$`2005`[,2] <- colMeans(mujeres2005)
mean_SD$mean$`2010`[,1] <- colMeans(hombres2010)
mean_SD$mean$`2010`[,2] <- colMeans(mujeres2010)
mean_SD$mean$`2015`[,1] <- colMeans(hombres2015)
mean_SD$mean$`2015`[,2] <- colMeans(mujeres2015)
mean_SD$mean$`2020`[,1] <- colMeans(hombres2020)
mean_SD$mean$`2020`[,2] <- colMeans(mujeres2020)
mean_SD$mean$`2025`[,1] <- colMeans(hombres2025)
mean_SD$mean$`2025`[,2] <- colMeans(mujeres2025)
mean_SD$mean$`2030`[,1] <- colMeans(hombres2030)
mean_SD$mean$`2030`[,2] <- colMeans(mujeres2030)

mean_SD$sd$`1990`[,1] <- apply(hombres1990, 2, sd)
mean_SD$sd$`1990`[,2] <- apply(mujeres1990, 2, sd)
mean_SD$sd$`1995`[,1] <- apply(hombres1995, 2, sd)
mean_SD$sd$`1995`[,2] <- apply(mujeres1995, 2, sd)
mean_SD$sd$`2000`[,1] <- apply(hombres2000, 2, sd)
mean_SD$sd$`2000`[,2] <- apply(mujeres2000, 2, sd)
mean_SD$sd$`2005`[,1] <- apply(hombres2005, 2, sd)
mean_SD$sd$`2005`[,2] <- apply(mujeres2005, 2, sd)
mean_SD$sd$`2010`[,1] <- apply(hombres2010, 2, sd)
mean_SD$sd$`2010`[,2] <- apply(mujeres2010, 2, sd)
mean_SD$sd$`2015`[,1] <- apply(hombres2015, 2, sd)
mean_SD$sd$`2015`[,2] <- apply(mujeres2015, 2, sd)
mean_SD$sd$`2020`[,1] <- apply(hombres2020, 2, sd)
mean_SD$sd$`2020`[,2] <- apply(mujeres2020, 2, sd)
mean_SD$sd$`2025`[,1] <- apply(hombres2025, 2, sd)
mean_SD$sd$`2025`[,2] <- apply(mujeres2025, 2, sd)
mean_SD$sd$`2030`[,1] <- apply(hombres2030, 2, sd)
mean_SD$sd$`2030`[,2] <- apply(mujeres2030, 2, sd)

#We we scale the data we use the next function (x- mean)/sd

#we scale the longevity indicators that all data are in the same mean
shombres1990 <- scale(hombres1990)
shombres1995 <- scale(hombres1995)
shombres2000 <- scale(hombres2000)
shombres2005 <- scale(hombres2005)
shombres2010 <- scale(hombres2010)
shombres2015 <- scale(hombres2015)
shombres2020 <- scale(hombres2020)
shombres2025 <- scale(hombres2025)
shombres2030 <- scale(hombres2030)
sum(is.na(hombres2030))

smujeres1990 <- scale(mujeres1990)
smujeres1995 <- scale(mujeres1995)
smujeres2000 <- scale(mujeres2000)
smujeres2005 <- scale(mujeres2005)
smujeres2010 <- scale(mujeres2010)
smujeres2015 <- scale(mujeres2015)
smujeres2020 <- scale(mujeres2020)
smujeres2025 <- scale(mujeres2025)
smujeres2030 <- scale(mujeres2030)
sum(is.na(mujeres2030))

##CLUSTERS 
library(cluster)
library(factoextra)
library(rnaturalearth)        
library(rnaturalearthdata)        
library(ggplot2)
library(viridis)
library(NbClust)

k_colores = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#F5C710", "#86868699")

#We fix the number of cluster in 5
##################################################################
#HOMBRES 1990 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster1990h <- hkmeans(x = shombres1990, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster1990h$centers)

centro_cluster1990h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster1990h$centers[,j]*mean_SD$sd$`1990`[j,1]+mean_SD$mean$`1990`[j,1]) 
  centro_cluster1990h <- cbind(centro_cluster1990h, prueba)
}
names(centro_cluster1990h) <- indica
centro_cluster1990h

#With this we visualizate the centroid of the cluster 
data.frame(t(centro_cluster1990h))

#We get the min and max of each cluster 
val.cluster1990h <- data.frame(shombres1990, cluster = cluster1990h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster1990h[val.cluster1990h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`1990`[,1], 1) + c(mean_SD$mean$`1990`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster1990h[val.cluster1990h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`1990`[,1], 1) + c(mean_SD$mean$`1990`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro1990 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster1990h[val.cluster1990h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster1990h[val.cluster1990h$cluster == cl,][j,] - c(cluster1990h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro1990 <- c(pais_centro1990, cluj)
}
pais_centro1990
c("CAF", "GEO", "IDN", "B77", "RWA")
datosh[pais_centro1990[1],]
datosh[pais_centro1990[2],]
datosh[pais_centro1990[3],]
datosh[pais_centro1990[4],]
datosh[pais_centro1990[5],]

##################################################################
#HOMBRES 1995 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster1995h <- hkmeans(x = shombres1995, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster1995h$centers)

centro_cluster1995h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster1995h$centers[,j]*mean_SD$sd$`1995`[j,1]+mean_SD$mean$`1995`[j,1]) 
  centro_cluster1995h <- cbind(centro_cluster1995h, prueba)
}
names(centro_cluster1995h) <- indica
centro_cluster1995h

#We get the min and max of each cluster 
data.frame(t(centro_cluster1995h))

#Finally we calculate the country more close to the centroid of each cluster
val.cluster1995h <- data.frame(shombres1995, cluster = cluster1995h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster1995h[val.cluster1995h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`1995`[,1], 1) + c(mean_SD$mean$`1995`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster1995h[val.cluster1995h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`1995`[,1], 1) + c(mean_SD$mean$`1995`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro1995 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster1995h[val.cluster1995h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster1995h[val.cluster1995h$cluster == cl,][j,] - c(cluster1995h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro1995 <- c(pais_centro1995, cluj)
}
pais_centro1995
c("TJK", "IRN", "NZL", "TZA", "PRI")
datosh[pais_centro1995[1],]
datosh[pais_centro1995[2],]
datosh[pais_centro1995[3],]
datosh[pais_centro1995[4],]
datosh[pais_centro1995[5],]

##################################################################
#HOMBRES 2000 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2000h <- hkmeans(x = shombres2000, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2000h$centers)

centro_cluster2000h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2000h$centers[,j]*mean_SD$sd$`2000`[j,1]+mean_SD$mean$`2000`[j,1]) 
  centro_cluster2000h <- cbind(centro_cluster2000h, prueba)
}
names(centro_cluster2000h) <- indica
centro_cluster2000h

data.frame(t(centro_cluster2000h))

#We get the min and max of each cluster 
val.cluster2000h <- data.frame(shombres2000, cluster = cluster2000h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2000h[val.cluster2000h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2000`[,1], 1) + c(mean_SD$mean$`2000`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2000h[val.cluster2000h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2000`[,1], 1) + c(mean_SD$mean$`2000`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2000 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2000h[val.cluster2000h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2000h[val.cluster2000h$cluster == cl,][j,] - c(cluster2000h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2000 <- c(pais_centro2000, cluj)
}
pais_centro2000
c("VCT", "AUT", "RWA", "AZE", "PRI")
datosh[pais_centro2000[1],]
datosh[pais_centro2000[2],]
datosh[pais_centro2000[3],]
datosh[pais_centro2000[4],]
datosh[pais_centro2000[5],]

##################################################################
#HOMBRES 2005 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2005h <- hkmeans(x = shombres2005, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2005h$centers)

centro_cluster2005h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2005h$centers[,j]*mean_SD$sd$`2005`[j,1]+mean_SD$mean$`2005`[j,1]) 
  centro_cluster2005h <- cbind(centro_cluster2005h, prueba)
}
names(centro_cluster2005h) <- indica
centro_cluster2005h

data.frame(t(centro_cluster2005h))

#We get the min and max of each cluster 
val.cluster2005h <- data.frame(shombres2005, cluster = cluster2005h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2005h[val.cluster2005h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2005`[,1], 1) + c(mean_SD$mean$`2005`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2005h[val.cluster2005h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2005`[,1], 1) + c(mean_SD$mean$`2005`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2005 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2005h[val.cluster2005h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2005h[val.cluster2005h$cluster == cl,][j,] - c(cluster2005h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2005 <- c(pais_centro2005, cluj)
}
pais_centro2005
c("YEM", "GRC", "GRD", "THA", "CMR")
datosh[pais_centro2005[1],]
datosh[pais_centro2005[2],]
datosh[pais_centro2005[3],]
datosh[pais_centro2005[4],]
datosh[pais_centro2005[5],]

##################################################################
#HOMBRES 2010 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2010h <- hkmeans(x = shombres2010, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2010h$centers)

centro_cluster2010h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2010h$centers[,j]*mean_SD$sd$`2010`[j,1]+mean_SD$mean$`2010`[j,1]) 
  centro_cluster2010h <- cbind(centro_cluster2010h, prueba)
}
names(centro_cluster2010h) <- indica
centro_cluster2010h

data.frame(t(centro_cluster2010h))

#We get the min and max of each cluster 
val.cluster2010h <- data.frame(shombres2010, cluster = cluster2010h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2010h[val.cluster2010h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2010`[,1], 1) + c(mean_SD$mean$`2010`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2010h[val.cluster2010h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2010`[,1], 1) + c(mean_SD$mean$`2010`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2010 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2010h[val.cluster2010h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2010h[val.cluster2010h$cluster == cl,][j,] - c(cluster2010h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2010 <- c(pais_centro2010, cluj)
}
pais_centro2010
c("KHM", "AUT", "MEX", "ARG", "ZMB")
datosh[pais_centro2010[1],]
datosh[pais_centro2010[2],]
datosh[pais_centro2010[3],]
datosh[pais_centro2010[4],]
datosh[pais_centro2010[5],]

##################################################################
#HOMBRES 2015 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2015h <- hkmeans(x = shombres2015, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2015h$centers)

centro_cluster2015h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2015h$centers[,j]*mean_SD$sd$`2015`[j,1]+mean_SD$mean$`2015`[j,1]) 
  centro_cluster2015h <- cbind(centro_cluster2015h, prueba)
}
names(centro_cluster2015h) <- indica
centro_cluster2015h


data.frame(t(centro_cluster2015h))

#We get the min and max of each cluster 
val.cluster2015h <- data.frame(shombres2015, cluster = cluster2015h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2015h[val.cluster2015h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2015`[,1], 1) + c(mean_SD$mean$`2015`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2015h[val.cluster2015h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2015`[,1], 1) + c(mean_SD$mean$`2015`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2015 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2015h[val.cluster2015h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2015h[val.cluster2015h$cluster == cl,][j,] - c(cluster2015h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2015 <- c(pais_centro2015, cluj)
}
pais_centro2015
c("TLS", "GBR", "MEX", "SLB", "MOZ")
datosh[pais_centro2015[1],]
datosh[pais_centro2015[2],]
datosh[pais_centro2015[3],]
datosh[pais_centro2015[4],]
datosh[pais_centro2015[5],]

##################################################################
#HOMBRES 2020 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2020h <- hkmeans(x = shombres2020, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2020h$centers)

centro_cluster2020h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2020h$centers[,j]*mean_SD$sd$`2020`[j,1]+mean_SD$mean$`2020`[j,1]) 
  centro_cluster2020h <- cbind(centro_cluster2020h, prueba)
}
names(centro_cluster2020h) <- indica
centro_cluster2020h


data.frame(t(centro_cluster2020h))

#We get the min and max of each cluster 
val.cluster2020h <- data.frame(shombres2020, cluster = cluster2020h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2020h[val.cluster2020h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2020`[,1], 1) + c(mean_SD$mean$`2020`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2020h[val.cluster2020h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2020`[,1], 1) + c(mean_SD$mean$`2020`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2020 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2020h[val.cluster2020h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2020h[val.cluster2020h$cluster == cl,][j,] - c(cluster2020h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2020 <- c(pais_centro2020, cluj)
}
pais_centro2020
c("LAO", "MAC", "BTN", "CMR", "LKA")
datosh[pais_centro2020[1],]
datosh[pais_centro2020[2],]
datosh[pais_centro2020[3],]
datosh[pais_centro2020[4],]
datosh[pais_centro2020[5],]

##################################################################
#HOMBRES 2025 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2025h <- hkmeans(x = shombres2025, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2025h$centers)

centro_cluster2025h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2025h$centers[,j]*mean_SD$sd$`2025`[j,1]+mean_SD$mean$`2025`[j,1]) 
  centro_cluster2025h <- cbind(centro_cluster2025h, prueba)
}
names(centro_cluster2025h) <- indica
centro_cluster2025h


data.frame(t(centro_cluster2025h))

#We get the min and max of each cluster 
val.cluster2025h <- data.frame(shombres2025, cluster = cluster2025h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2025h[val.cluster2025h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2025`[,1], 1) + c(mean_SD$mean$`2025`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2025h[val.cluster2025h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2025`[,1], 1) + c(mean_SD$mean$`2025`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2025 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2025h[val.cluster2025h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2025h[val.cluster2025h$cluster == cl,][j,] - c(cluster2025h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2025 <- c(pais_centro2025, cluj)
}
pais_centro2025
c("MMR", "IDN", "SGP", "BTN", "CMR")
datosh[pais_centro2025[1],]
datosh[pais_centro2025[2],]
datosh[pais_centro2025[3],]
datosh[pais_centro2025[4],]
datosh[pais_centro2025[5],]

##################################################################
#HOMBRES 2030 
##################################################################
set.seed(12345)
#HIERARCHICAL K-MEANS CLUSTERING
cluster2030h <- hkmeans(x = shombres2030, hc.metric = "euclidean",
                        hc.method = "average", k = 5)
#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2030h$centers)

centro_cluster2030h <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2030h$centers[,j]*mean_SD$sd$`2030`[j,1]+mean_SD$mean$`2030`[j,1]) 
  centro_cluster2030h <- cbind(centro_cluster2030h, prueba)
}
names(centro_cluster2030h) <- indica
centro_cluster2030h


data.frame(t(centro_cluster2030h))

#We get the min and max of each cluster 
val.cluster2030h <- data.frame(shombres2030, cluster = cluster2030h$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2030h[val.cluster2030h$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2030`[,1], 1) + c(mean_SD$mean$`2030`[,1], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2030h[val.cluster2030h$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2030`[,1], 1) + c(mean_SD$mean$`2030`[,1], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2030 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2030h[val.cluster2030h$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2030h[val.cluster2030h$cluster == cl,][j,] - c(cluster2030h$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2030 <- c(pais_centro2030, cluj)
}
pais_centro2030
c("CPV", "AUT", "CUW", "ZMB", "DOM")
datosh[pais_centro2030[1],]
datosh[pais_centro2030[2],]
datosh[pais_centro2030[3],]
datosh[pais_centro2030[4],]
datosh[pais_centro2030[5],]

#Ver que país ocupa cada cluster
data.frame(sort(cluster1990h$cluster)) 
data.frame(sort(cluster1995h$cluster)) 
data.frame(sort(cluster2000h$cluster))
data.frame(sort(cluster2005h$cluster))
data.frame(sort(cluster2010h$cluster))
data.frame(sort(cluster2015h$cluster))
data.frame(sort(cluster2020h$cluster))
data.frame(sort(cluster2025h$cluster))
data.frame(sort(cluster2030h$cluster))
#In a excel file we reorder the number of the cluster 
#to ensure that the cluster continues in all period 
#the same number and color

##############################################################################
#MOSTRAR MAPAS DE LOS CLUSTERS
##############################################################################
#We get data txt files with the information of the cluster
clusters <- read.table(file = "cluster_194_todosperiod_male.txt", header=TRUE)
latitude <- read.table(file = "LatLong_194.txt", header=TRUE)
head(latitude)
length(latitude$Lat)

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library(cowplot)
library(googleway)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
#Latitude y Longuitud unida con cada país
centers <- data.frame(row.names = world$name_long, name = world$name_long,
                      longitude = latitude$Lon, latitude = latitude$Lat)

#To remark the centroid of each cluster, we need to mark which are the center in every period
#Centers 1990 
centers1990 <- NULL
centers1990 <- centers[centers$name == "Central African Republic",] 
centers1990 <- rbind(centers1990, centers[centers$name == "Georgia",])
centers1990 <- rbind(centers1990, centers[centers$name == "Taiwan",])
centers1990 <- rbind(centers1990, centers[centers$name == "Indonesia",])
centers1990 <- rbind(centers1990, centers[centers$name == "Rwanda",])

datosh[pais_centro1995,]
#Centers 1995 
centers1995 <- NULL
centers1995 <- centers[centers$name == "Tajikistan",] 
centers1995 <- rbind(centers1995, centers[centers$name == "Iran",])
centers1995 <- rbind(centers1995, centers[centers$name == "New Zealand",])
centers1995 <- rbind(centers1995, centers[centers$name == "Tanzania",])
centers1995 <- rbind(centers1995, centers[centers$name == "Puerto Rico",])

datosh[pais_centro2000,]
#Centers 2000 
centers2000 <- NULL
centers2000 <- centers[centers$name == "Saint Vincent and the Grenadines",] 
centers2000 <- rbind(centers2000, centers[centers$name == "Austria",])
centers2000 <- rbind(centers2000, centers[centers$name == "Rwanda",])
centers2000 <- rbind(centers2000, centers[centers$name == "Azerbaijan",])
centers2000 <- rbind(centers2000, centers[centers$name == "Puerto Rico",])

datosh[pais_centro2005,]
#Centers 2005 
centers2005 <- NULL
centers2005 <- centers[centers$name == "Yemen",] 
centers2005 <- rbind(centers2005, centers[centers$name == "Greece",])
centers2005 <- rbind(centers2005, centers[centers$name == "Grenada",])
centers2005 <- rbind(centers2005, centers[centers$name == "Thailand",])
centers2005 <- rbind(centers2005, centers[centers$name == "Cameroon",])

datosh[pais_centro2010,]
#Centers 2010 
centers2010 <- NULL
centers2010 <- centers[centers$name == "Cambodia",] 
centers2010 <- rbind(centers2010, centers[centers$name == "Austria",])
centers2010 <- rbind(centers2010, centers[centers$name == "Mexico",])
centers2010 <- rbind(centers2010, centers[centers$name == "Argentina",])
centers2010 <- rbind(centers2010, centers[centers$name == "Zambia",])

datosh[pais_centro2015,]
#Centers 2015 
centers2015 <- NULL
centers2015 <- centers[centers$name == "Timor-Leste",] 
centers2015 <- rbind(centers2015, centers[centers$name == "United Kingdom",])
centers2015 <- rbind(centers2015, centers[centers$name == "Mexico",])
centers2015 <- rbind(centers2015, centers[centers$name == "Solomon Islands",])
centers2015 <- rbind(centers2015, centers[centers$name == "Mozambique",])

datosh[pais_centro2020,]
#Centers 2020 
centers2020 <- NULL
centers2020 <- centers[centers$name == "Lao PDR",] 
centers2020 <- rbind(centers2020, centers[centers$name == "Macao",])
centers2020 <- rbind(centers2020, centers[centers$name == "Bhutan",])
centers2020 <- rbind(centers2020, centers[centers$name == "Cameroon",])
centers2020 <- rbind(centers2020, centers[centers$name == "Sri Lanka",])

datosh[pais_centro2025,]
#Centers 2025 
centers2025 <- NULL
centers2025 <- centers[centers$name == "Myanmar",] 
centers2025 <- rbind(centers2025, centers[centers$name == "Indonesia",])
centers2025 <- rbind(centers2025, centers[centers$name == "Singapore",])
centers2025 <- rbind(centers2025, centers[centers$name == "Bhutan",])
centers2025 <- rbind(centers2025, centers[centers$name == "Cameroon",])

datosh[pais_centro2030,]
#Centers 2030 
centers2030 <- NULL
centers2030 <- rbind(centers2030, centers[centers$name == "Cape Verde",])
centers2030 <- rbind(centers2030, centers[centers$name == "Austria",])
centers2030 <- rbind(centers2030, centers[centers$name == "Curaçao",])
centers2030 <- rbind(centers2030, centers[centers$name == "Zambia",])
centers2030 <- rbind(centers2030, centers[centers$name == "Dominican Republic",])

#Once we have the centroids we develop one column of the data.frame 
#for each country and population
world$h1990 <- clusters$h1990 
world$h1995 <- clusters$h1995 
world$h2000 <- clusters$h2000 
world$h2005 <- clusters$h2005 
world$h2010 <- clusters$h2010 
world$h2015 <- clusters$h2015 
world$h2020 <- clusters$h2020 
world$h2025 <- clusters$h2025 
world$h2030 <- clusters$h2030 

#With the following codes we create the world map 
#and we color every country with its cluster-color
png("MapaMundo1990hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h1990))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  labs(title = "1990 hombres") + 
  geom_point(data = centers1990, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo1995hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h1995))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 hombres") + 
  geom_point(data = centers1995, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2000hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2000))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 hombres") + 
  geom_point(data = centers2000, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2005hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2005))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 hombres") + 
  geom_point(data = centers2005, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2010hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2010))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white") +
  geom_point(data = centers2010, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") +
  #labs(title = "2010 hombres") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2015hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2015))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 hombres") + 
  geom_point(data = centers2015, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2020hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2020))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 hombres") + 
  geom_point(data = centers2020, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2025hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2025))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 hombres") + 
  geom_point(data = centers2025, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2030hombres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2030))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white") +
  #labs(title = "2030 hombres") + 
  geom_point(data = centers2030, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

#Equally in male case, same for females
#############################################################################
#MUJERES
##################################################################
#MUJERES 1990 
##################################################################
set.seed(12345)
cluster1990m <- hkmeans(x = smujeres1990, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster1990m$centers)

centro_cluster1990m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster1990m$centers[,j]*mean_SD$sd$`1990`[j,2]+mean_SD$mean$`1990`[j,2]) 
  centro_cluster1990m <- cbind(centro_cluster1990m, prueba)
}
names(centro_cluster1990m) <- indica
centro_cluster1990m


data.frame(t(centro_cluster1990m))

#We get the min and max of each cluster 
val.cluster1990m <- data.frame(smujeres1990, cluster = cluster1990m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster1990m[val.cluster1990m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`1990`[,2], 1) + c(mean_SD$mean$`1990`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster1990m[val.cluster1990m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`1990`[,2], 1) + c(mean_SD$mean$`1990`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro1990 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster1990m[val.cluster1990m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster1990m[val.cluster1990m$cluster == cl,][j,] - c(cluster1990m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro1990 <- c(pais_centro1990, cluj)
}
pais_centro1990
datosm[pais_centro1990, ]
c("TTO", "LUX", "LAO", "LVA", "SLE")

##################################################################
#MUJERES 1995 
##################################################################
set.seed(12345)
cluster1995m <- hkmeans(x = smujeres1995, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster1995m$centers)

centro_cluster1995m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster1995m$centers[,j]*mean_SD$sd$`1995`[j,2]+mean_SD$mean$`1995`[j,2]) 
  centro_cluster1995m <- cbind(centro_cluster1995m, prueba)
}
names(centro_cluster1995m) <- indica
centro_cluster1995m


data.frame(t(centro_cluster1995m))

#We get the min and max of each cluster 
val.cluster1995m <- data.frame(smujeres1995, cluster = cluster1995m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster1995m[val.cluster1995m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`1995`[,2], 1) + c(mean_SD$mean$`1995`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster1995m[val.cluster1995m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`1995`[,2], 1) + c(mean_SD$mean$`1995`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro1995 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster1995m[val.cluster1995m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster1995m[val.cluster1995m$cluster == cl,][j,] - c(cluster1995m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro1995 <- c(pais_centro1995, cluj)
}
pais_centro1995
datosm[pais_centro1995, ]
c("TTO", "LUX", "LAO", "LVA", "SLE")

##################################################################
#MUJERES 2000 
##################################################################
set.seed(12345)
cluster2000m <- hkmeans(x = smujeres2000, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2000m$centers)

centro_cluster2000m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2000m$centers[,j]*mean_SD$sd$`2000`[j,2]+mean_SD$mean$`2000`[j,2]) 
  centro_cluster2000m <- cbind(centro_cluster2000m, prueba)
}
names(centro_cluster2000m) <- indica
centro_cluster2000m


data.frame(t(centro_cluster2000m))

#We get the min and max of each cluster 
val.cluster2000m <- data.frame(smujeres2000, cluster = cluster2000m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2000m[val.cluster2000m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2000`[,2], 1) + c(mean_SD$mean$`2000`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2000m[val.cluster2000m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2000`[,2], 1) + c(mean_SD$mean$`2000`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2000 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2000m[val.cluster2000m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2000m[val.cluster2000m$cluster == cl,][j,] - c(cluster2000m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2000 <- c(pais_centro2000, cluj)
}
pais_centro2000
datosm[pais_centro2000, ]
c("MMR", "LUX", "ROU", "CIV", "NIC")

##################################################################
#MUJERES 2005 
##################################################################
set.seed(12345)
cluster2005m <- hkmeans(x = smujeres2005, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2005m$centers)

centro_cluster2005m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2005m$centers[,j]*mean_SD$sd$`2005`[j,2]+mean_SD$mean$`2005`[j,2]) 
  centro_cluster2005m <- cbind(centro_cluster2005m, prueba)
}
names(centro_cluster2005m) <- indica
centro_cluster2005m


data.frame(t(centro_cluster2005m))

#We get the min and max of each cluster 
val.cluster2005m <- data.frame(smujeres2005, cluster = cluster2005m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2005m[val.cluster2005m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2005`[,2], 1) + c(mean_SD$mean$`2005`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2005m[val.cluster2005m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2005`[,2], 1) + c(mean_SD$mean$`2005`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2005 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2005m[val.cluster2005m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2005m[val.cluster2005m$cluster == cl,][j,] - c(cluster2005m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2005 <- c(pais_centro2005, cluj)
}
pais_centro2005
datosm[pais_centro2005, ]
c("YEM", "JAM", "LUX", "ZMB", "VEN")

##################################################################
#MUJERES 2010 
##################################################################
set.seed(12345)
cluster2010m <- hkmeans(x = smujeres2010, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2010m$centers)

centro_cluster2010m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2010m$centers[,j]*mean_SD$sd$`2010`[j,2]+mean_SD$mean$`2010`[j,2]) 
  centro_cluster2010m <- cbind(centro_cluster2010m, prueba)
}
names(centro_cluster2010m) <- indica
centro_cluster2010m


data.frame(t(centro_cluster2010m))

#We get the min and max of each cluster 
val.cluster2010m <- data.frame(smujeres2010, cluster = cluster2010m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2010m[val.cluster2010m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2010`[,2], 1) + c(mean_SD$mean$`2010`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2010m[val.cluster2010m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2010`[,2], 1) + c(mean_SD$mean$`2010`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2010 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2010m[val.cluster2010m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2010m[val.cluster2010m$cluster == cl,][j,] - c(cluster2010m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2010 <- c(pais_centro2010, cluj)
}
pais_centro2010
datosm[pais_centro2010, ]
c("AFG", "JOR", "ISR", "THA", "DOM")

##################################################################
#MUJERES 2015 
##################################################################
set.seed(12345)
cluster2015m <- hkmeans(x = smujeres2015, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2015m$centers)

centro_cluster2015m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2015m$centers[,j]*mean_SD$sd$`2015`[j,2]+mean_SD$mean$`2015`[j,2]) 
  centro_cluster2015m <- cbind(centro_cluster2015m, prueba)
}
names(centro_cluster2015m) <- indica
centro_cluster2015m


data.frame(t(centro_cluster2015m))

#We get the min and max of each cluster 
val.cluster2015m <- data.frame(smujeres2015, cluster = cluster2015m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2015m[val.cluster2015m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2015`[,2], 1) + c(mean_SD$mean$`2015`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2015m[val.cluster2015m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2015`[,2], 1) + c(mean_SD$mean$`2015`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2015 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2015m[val.cluster2015m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2015m[val.cluster2015m$cluster == cl,][j,] - c(cluster2015m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2015 <- c(pais_centro2015, cluj)
}
pais_centro2015
datosm[pais_centro2015, ]
c("CMR", "BRN", "MLT", "NIC", "MMR")

##################################################################
#MUJERES 2020 
##################################################################
set.seed(12345)
cluster2020m <- hkmeans(x = smujeres2020, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2020m$centers)

centro_cluster2020m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2020m$centers[,j]*mean_SD$sd$`2020`[j,2]+mean_SD$mean$`2020`[j,2]) 
  centro_cluster2020m <- cbind(centro_cluster2020m, prueba)
}
names(centro_cluster2020m) <- indica
centro_cluster2020m


data.frame(t(centro_cluster2020m))

#We get the min and max of each cluster 
val.cluster2020m <- data.frame(smujeres2020, cluster = cluster2020m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2020m[val.cluster2020m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2020`[,2], 1) + c(mean_SD$mean$`2020`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2020m[val.cluster2020m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2020`[,2], 1) + c(mean_SD$mean$`2020`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2020 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2020m[val.cluster2020m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2020m[val.cluster2020m$cluster == cl,][j,] - c(cluster2020m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2020 <- c(pais_centro2020, cluj)
}
pais_centro2020
datosm[pais_centro2020, ]
c("ZAF", "AZE", "SLV", "DOM", "IRL")

##################################################################
#MUJERES 2025 
##################################################################
set.seed(12345)
cluster2025m <- hkmeans(x = smujeres2025, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2025m$centers)

centro_cluster2025m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2025m$centers[,j]*mean_SD$sd$`2025`[j,2]+mean_SD$mean$`2025`[j,2]) 
  centro_cluster2025m <- cbind(centro_cluster2025m, prueba)
}
names(centro_cluster2025m) <- indica
centro_cluster2025m


data.frame(t(centro_cluster2025m))

#We get the min and max of each cluster 
val.cluster2025m <- data.frame(smujeres2025, cluster = cluster2025m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2025m[val.cluster2025m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2025`[,2], 1) + c(mean_SD$mean$`2025`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2025m[val.cluster2025m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2025`[,2], 1) + c(mean_SD$mean$`2025`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2025 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2025m[val.cluster2025m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2025m[val.cluster2025m$cluster == cl,][j,] - c(cluster2025m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2025 <- c(pais_centro2025, cluj)
}
pais_centro2025
datosm[pais_centro2025, ]
c("ZAF", "SLV", "STP", "DOM", "IRL")

##################################################################
#MUJERES 2030 
##################################################################
set.seed(12345)
cluster2030m <- hkmeans(x = smujeres2030, hc.metric = "euclidean",
                        hc.method = "average", k = 5)

#With this we visualizate the centroid of the cluster 
center <- as.data.frame(cluster2030m$centers)

centro_cluster2030m <- data.frame(row.names = c(1:5))
for(j in 1:9){
  prueba <- data.frame(row.names = c(1:5), 
                       j = cluster2030m$centers[,j]*mean_SD$sd$`2030`[j,2]+mean_SD$mean$`2030`[j,2]) 
  centro_cluster2030m <- cbind(centro_cluster2030m, prueba)
}
names(centro_cluster2030m) <- indica
centro_cluster2030m


data.frame(t(centro_cluster2030m))

#We get the min and max of each cluster 
val.cluster2030m <- data.frame(smujeres2030, cluster = cluster2030m$cluster)
min_cluster <- max_cluster <- NULL
for(cl in 1:5){
  minimo <- apply(val.cluster2030m[val.cluster2030m$cluster == cl,], 2, min) *
    c(mean_SD$sd$`2030`[,2], 1) + c(mean_SD$mean$`2030`[,2], 0)
  min_cluster <- rbind(minimo, min_cluster)
  
  maximo <- apply(val.cluster2030m[val.cluster2030m$cluster == cl,], 2, max) *
    c(mean_SD$sd$`2030`[,2], 1) + c(mean_SD$mean$`2030`[,2], 0)
  max_cluster <- rbind(maximo, max_cluster)
}

min_cluster <- data.frame(row.names = c(5:1), min_cluster)
max_cluster <- data.frame(row.names = c(5:1), max_cluster)

data.frame(t(min_cluster))
data.frame(t(max_cluster))

#Finally we calculate the country more close to the centroid of each cluster
pais_centro2030 <- NULL
for(cl in 1:5){
  ncluster <- length(val.cluster2030m[val.cluster2030m$cluster == cl,][,1]) 
  trey <- NULL
  for(j in 1:ncluster){
    rey <- abs(val.cluster2030m[val.cluster2030m$cluster == cl,][j,] - c(cluster2030m$centers[cl,],0))
    trey <- rbind(trey, rey)
    minimos <- apply(trey, 1, sum)
    cluj <- names(which(minimos == min(minimos), arr.ind=TRUE))
  }
  pais_centro2030 <- c(pais_centro2030, cluj)
}
pais_centro2030
datosm[pais_centro2030, ]
c("MDA", "IRL", "DOM", "UGA", "TTO")

#Ver que país ocupa cada cluster
data.frame(sort(cluster1990m$cluster)) 
data.frame(sort(cluster1995m$cluster)) 
data.frame(sort(cluster2000m$cluster))
data.frame(sort(cluster2005m$cluster))
data.frame(sort(cluster2010m$cluster))
data.frame(sort(cluster2015m$cluster))
data.frame(sort(cluster2020m$cluster))
data.frame(sort(cluster2025m$cluster))
data.frame(sort(cluster2030m$cluster))

##############################################################################
#MOSTRAR MAPAS DE LOS CLUSTERS
##############################################################################
#Again we modify the number of the cluster 
#to keep the same cluster with the same color in all the periods
clusters <- read.table(file = "cluster_194_todosperiod_female.txt", header=TRUE)

head(clusters)
head(latitude)

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library(cowplot)
library(googleway)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(libwgeom)
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
#Latitude y Longuitud unida con cada país
centers <- data.frame(row.names = world$name_long, name = world$name_long,
                      longitude = latitude$Lon, latitude = latitude$Lat, 
                      acronymous = world$iso_a3)

#Again we need to specify the name of the centroid in every cluster
datosm[pais_centro1990, ]
#Centers 1990 
centers1990 <- NULL
centers1990 <- centers[centers$name == "Trinidad and Tobago",] 
centers1990 <- rbind(centers1990, centers[centers$name == "Luxembourg",])
centers1990 <- rbind(centers1990, centers[centers$name == "Lao PDR",])
centers1990 <- rbind(centers1990, centers[centers$name == "Latvia",])
centers1990 <- rbind(centers1990, centers[centers$name == "Sierra Leone",])

datosm[pais_centro1995, ]
#Centers 1995 
centers1995 <- NULL
centers1995 <- centers[centers$name == "Myanmar",] 
centers1995 <- rbind(centers1995, centers[centers$name == "Luxembourg",])
centers1995 <- rbind(centers1995, centers[centers$name == "Saint Vincent and the Grenadines",])
centers1995 <- rbind(centers1995, centers[centers$name == "Venezuela",])
centers1995 <- rbind(centers1995, centers[centers$name == "Tanzania",])

datosm[pais_centro2000, ]
#Centers 2000 
centers2000 <- NULL
centers2000 <- centers[centers$name == "Myanmar",] 
centers2000 <- rbind(centers2000, centers[centers$name == "Luxembourg",])
centers2000 <- rbind(centers2000, centers[centers$name == "Romania",])
centers2000 <- rbind(centers2000, centers[centers$name == "Côte d'Ivoire",])
centers2000 <- rbind(centers2000, centers[centers$name == "Nicaragua",])

datosm[pais_centro2005, ]
#Centers 2005 
centers2005 <- NULL
centers2005 <- centers[centers$name == "Yemen",] 
centers2005 <- rbind(centers2005, centers[centers$name == "Jamaica",])
centers2005 <- rbind(centers2005, centers[centers$name == "Luxembourg",])
centers2005 <- rbind(centers2005, centers[centers$name == "Zambia",])
centers2005 <- rbind(centers2005, centers[centers$name == "Venezuela",])

datosm[pais_centro2010, ]
#Centers 2010 
centers2010 <- NULL
centers2010 <- centers[centers$name == "Afghanistan",] 
centers2010 <- rbind(centers2010, centers[centers$name == "Jordan",])
centers2010 <- rbind(centers2010, centers[centers$name == "Israel",])
centers2010 <- rbind(centers2010, centers[centers$name == "Thailand",])
centers2010 <- rbind(centers2010, centers[centers$name == "Dominican Republic",])

datosm[pais_centro2015, ]
#Centers 2015 
centers2015 <- NULL
centers2015 <- centers[centers$name == "Cameroon",] 
centers2015 <- rbind(centers2015, centers[centers$name == "Brunei Darussalam",])
centers2015 <- rbind(centers2015, centers[centers$name == "Malta",])
centers2015 <- rbind(centers2015, centers[centers$name == "Nicaragua",])
centers2015 <- rbind(centers2015, centers[centers$name == "Myanmar",])

datosm[pais_centro2020, ]
#Centers 2020 
centers2020 <- NULL
centers2020 <- centers[centers$name == "South Africa",] 
centers2020 <- rbind(centers2020, centers[centers$name == "Azerbaijan",])
centers2020 <- rbind(centers2020, centers[centers$name == "El Salvador",])
centers2020 <- rbind(centers2020, centers[centers$name == "Dominican Republic",])
centers2020 <- rbind(centers2020, centers[centers$name == "Ireland",])


datosm[pais_centro2025, ]
#Centers 2025 
centers2025 <- NULL
centers2025 <- centers[centers$name == "South Africa",] 
centers2025 <- rbind(centers2025, centers[centers$name == "El Salvador",])
centers2025 <- rbind(centers2025, centers[centers$name == "São Tomé and Principe",])
centers2025 <- rbind(centers2025, centers[centers$name == "Dominican Republic",])
centers2025 <- rbind(centers2025, centers[centers$name == "Ireland",])

datosm[pais_centro2030, ]
#Centers 2030 
centers2030 <- NULL
centers2030 <- centers[centers$name == "Moldova",] 
centers2030 <- rbind(centers2030, centers[centers$name == "Ireland",])
centers2030 <- rbind(centers2030, centers[centers$name == "Dominican Republic",])
centers2030 <- rbind(centers2030, centers[centers$name == "Uganda",])
centers2030 <- rbind(centers2030, centers[centers$name == "Trinidad and Tobago",])

#Once, we have the centroid we create a column in the data.frame 
#for each period analyzed
world$h1990 <- clusters$m1990 
world$h1995 <- clusters$m1995 
world$h2000 <- clusters$m2000 
world$h2005 <- clusters$m2005 
world$h2010 <- clusters$m2010 
world$h2015 <- clusters$m2015 
world$h2020 <- clusters$m2020 
world$h2025 <- clusters$m2025 
world$h2030 <- clusters$m2030 

png("MapaMundo1990mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h1990))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  labs(title = "1990 mujeres") + 
  geom_point(data = centers1990, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo1995mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h1995))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 mujeres") + 
  geom_point(data = centers1995, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2000mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2000))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 mujeres") + 
  geom_point(data = centers2000, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2005mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2005))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "1990 mujeres") + 
  geom_point(data = centers2005, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2010mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2010))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "2010 mujeres") + 
  geom_point(data = centers2010, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2015mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2015))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "2010 mujeres") + 
  geom_point(data = centers2015, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2020mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2020))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "2010 mujeres") + 
  geom_point(data = centers2020, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2025mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2025))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "2010 mujeres") + 
  geom_point(data = centers2025, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()

png("MapaMundo2030mujeres.png")
ggplot(data = world) +
  geom_sf(aes(fill = factor(h2030))) +
  scale_fill_manual(values = c("orange", "#619CFF", "#F8766D", "yellow", 
                               "forestgreen"), 
                    na.value = "white")+
  #labs(title = "2030 mujeres") + 
  geom_point(data = centers2030, aes(x = longitude, y = latitude), size = 2, 
             shape = 19, col = "green") + 
  theme(legend.position = "bottom", 
        axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  guides(fill=guide_legend(title=""))
dev.off()
