#Before the estimation we need to install some packages
library(HMDHFDplus)
library(demography)
library(forecast)
library(StMoMo)

#These data are from WHO; we only transform the excel in txt to
#facilate the transcription to R but you can obtain the information from this link:
#https://population.un.org/wpp/download/standard/mortality/

datos_hombres <- read.table("WHO_male.txt", header=T, sep = ";")
datos_mujeres <- read.table("WHO_female.txt", header=T, sep = ";")

name.paises <- c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", "Argentina",
                 "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain",
                 "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
                 "Bolivia (Plurinational State of)", "Bosnia and Herzegovina", "Botswana",
                 "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde",
                 "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile",
                 "China", "China, Hong Kong SAR", "China, Macao SAR", "China, Taiwan Province of China",
                 "Colombia", "Comoros", "Congo", "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba",
                 "Curaçao", "Cyprus", "Czechia", "Dem. People's Republic of Korea", "Democratic Republic of the Congo",
                 "Denmark", "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador",
                 "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France",
                 "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece",
                 "Grenada", "Guam", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti",
                 "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran (Islamic Republic of)",
                 "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan",
                 "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic",
                 "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Lithuania", "Luxembourg",
                 "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Mauritania",
                 "Mauritius", "Mexico", "Micronesia (Fed. States of)", "Mongolia", "Montenegro",
                 "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Netherlands", "New Caledonia",
                 "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman",
                 "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland",
                 "Portugal", "Puerto Rico", "Qatar", "Republic of Korea", "Republic of Moldova",
                 "Romania", "Russian Federation", "Rwanda", "Saint Lucia", "Saint Vincent and the Grenadines",
                 "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles",
                 "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia",
                 "South Africa", "South Sudan", "Spain", "Sri Lanka", "State of Palestine", "Sudan",
                 "Suriname", "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan", "Thailand",
                 "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan",
                 "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United Republic of Tanzania",
                 "United States of America", "United States Virgin Islands", "Uruguay", "Uzbekistan",
                 "Vanuatu", "Venezuela (Bolivarian Republic of)", "Viet Nam", "Western Sahara",
                 "Yemen", "Zambia", "Zimbabwe")

acr.paises <- c("AFG", "ALB", "DZA", "AGO", "ATG", "ARG", "ARM", "ABW", "AUS",
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

length(acr.paises)
length(name.paises)

n_ages <- c(1, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, -1)
#Once, we have the data we transform the WHO data in data.frame

paso1 <- list()
paises <- list()
for(i in 1:length(acr.paises)){
  male <- NULL
  female <- NULL
  #1. We obtain the data which correspond to every country
  male <- datos_hombres[datos_hombres$Region == name.paises[i], ]
  female <- datos_mujeres[datos_mujeres$Region == name.paises[i], ]
  paso1[[paste0(acr.paises[i])]] <- data.frame(Period = male$Period, Age = male$Age,
                                               mxt_m = male$mxt, axt_m = male$axt,
                                               mxt_f = female$mxt, axt_f = female$axt)
  #2. We estimate the qxt based on mxt
  n <- rep(n_ages, ((max(paso1[[i]]$Period) - min(paso1[[i]]$Period))/5 + 1))
  qxt_male <- (n*paso1[[i]]$mxt_m)/(1+(n - paso1[[i]]$axt_m)+paso1[[i]]$mxt_m)
  qxt_female <- (n*paso1[[i]]$mxt_f)/(1+(n - paso1[[i]]$axt_f)+paso1[[i]]$mxt_f)
  male2 <- NULL
  female2 <- NULL
  male2 <- cbind.data.frame(Age = paso1[[i]]$Age, Year = paso1[[i]]$Period,
                            mxt = paso1[[i]]$mxt_m, ax = paso1[[i]]$axt_m,
                            qxt = qxt_male)
  male2$qxt[22] <- male2$mxt[22] <- 1
  male2$qxt[44] <- male2$mxt[44] <- 1
  male2$qxt[66] <- male2$mxt[66] <- 1
  male2$qxt[88] <- male2$mxt[88] <- 1
  male2$qxt[110] <- male2$mxt[110] <- 1
  male2$qxt[132] <- male2$mxt[132] <- 1

  female2 <- cbind.data.frame(Age = paso1[[i]]$Age, Year = paso1[[i]]$Period,
                              mxt = paso1[[i]]$mxt_f, ax = paso1[[i]]$axt_f,
                              qxt = qxt_female)
  female2$qxt[22] <- female2$mxt[22] <- 1
  female2$qxt[44] <- female2$mxt[44] <- 1
  female2$qxt[66] <- female2$mxt[66] <- 1
  female2$qxt[88] <- female2$mxt[88] <- 1
  female2$qxt[110] <- female2$mxt[110] <- 1
  female2$qxt[132] <- female2$mxt[132] <- 1

  #3. We use periods from 1990 to the last available period
  male2 <- male2[male2$Year > 1989, ]
  male2 <- male2[male2$Age < 100, ]
  female2 <- female2[female2$Year > 1989, ]
  female2 <- female2[female2$Age < 100, ]
  paises[[paste0(acr.paises[i])]][[paste0("hombres")]] <- male2
  paises[[paste0(acr.paises[i])]][[paste0("mujeres")]] <- female2

}

pob <- acr.paises
length(pob)
length(paises)
sex <- c("hombres", "mujeres")

num.NA <- list()

#We have to check that there are not any qxt > 1 or with NA
for(i in 1:length(pob)){
  for(j in 1:2){
    num.NA[[paste0(pob[i])]][[paste0(sex[j])]] <- sum(is.na(paises[[i]][[j]]$qx))
  }
}

for(i in 1:length(pob)){
  for(j in 1:2){
    if( length(paises[[i]][[j]][paises[[i]][[j]]$qxt >= 1,]$qxt) > 1)
      paises[[i]][[j]][paises[[i]][[j]]$qxt >= 1,]$qxt <- 1 }
}


#Now we transform the data.frame in matrix to facilate the estimation of the indicators
datos_mat <- list()
edades <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)

sex <- c("hombres", "mujeres")
for(i in 1:length(pob)){
  for(j in 1:2){
    ages <- edades
    nages <- length(ages)
    minyear <- min(paises[[i]]$hombres$Year)
    maxyear <- max(paises[[i]]$hombres$Year)
    years <- seq(minyear, maxyear, by=5)
    nyears <- length(years)
    datos_mat[[paste0(pob[i])]][[paste0(sex[j])]]$qxt <- matrix(paises[[i]][[j]]$qxt, nages, nyears, dimnames = list(ages, years))
    datos_mat[[paste0(pob[i])]][[paste0(sex[j])]]$mxt <- matrix(paises[[i]][[j]]$mxt, nages, nyears, dimnames = list(ages, years))
    datos_mat[[paste0(pob[i])]][[paste0(sex[j])]]$ax <- matrix(paises[[i]][[j]]$ax, nages, nyears, dimnames = list(ages, years))
  }
}

#We save the data of the populations to facilitate its use as plot
datos_mat_inter <- list()
for(i in 1:length(pob)){
  for(j in 1:2){
    ages <- edades
    nages <- length(ages)
    minyear <- 1990
    maxyear <- 2015
    years <- seq(minyear, maxyear, by=1)
    nyears <- length(years)
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt  <- matrix(NA, nages, nyears, dimnames = list(ages, years))
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt <- matrix(NA, nages, nyears, dimnames = list(ages, years))
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax <- matrix(NA, nages, nyears, dimnames = list(ages, years))

    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt[,1] <- datos_mat[[i]][[j]]$qxt[,1]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt[,6] <- datos_mat[[i]][[j]]$qxt[,2]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt[,11]<- datos_mat[[i]][[j]]$qxt[,3]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt[,16]<- datos_mat[[i]][[j]]$qxt[,4]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt[,21]<- datos_mat[[i]][[j]]$qxt[,5]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$qxt[,26]<- datos_mat[[i]][[j]]$qxt[,6]

    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt[,1] <- datos_mat[[i]][[j]]$mxt[,1]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt[,6] <- datos_mat[[i]][[j]]$mxt[,2]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt[,11]<- datos_mat[[i]][[j]]$mxt[,3]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt[,16]<- datos_mat[[i]][[j]]$mxt[,4]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt[,21]<- datos_mat[[i]][[j]]$mxt[,5]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$mxt[,26]<- datos_mat[[i]][[j]]$mxt[,6]

    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax[,1] <- datos_mat[[i]][[j]]$ax[,1]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax[,6] <- datos_mat[[i]][[j]]$ax[,2]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax[,11]<- datos_mat[[i]][[j]]$ax[,3]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax[,16]<- datos_mat[[i]][[j]]$ax[,4]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax[,21]<- datos_mat[[i]][[j]]$ax[,5]
    datos_mat_inter[[paste0(pob[i])]][[paste0(sex[j])]]$ax[,26]<- datos_mat[[i]][[j]]$ax[,6]
  }
}

i <- j <- 1
#If we need the interpolation between the periods
#The next formula will be used and then we could use data between the periods
#P1991 + ((1992 - 1991$)/($2000-1991$))*($P2000-P1991)
#for(i in 1:length(pob)){
#  for(j in 1:2){
#    for(h in 1:4){
#      datos_mat_inter[[i]][[j]]$qxt[,(h+1)] <- datos_mat_inter[[i]][[j]]$qxt[,1] + h/5*(datos_mat_inter[[i]][[j]]$qxt[,6]-datos_mat_inter[[i]][[j]]$qxt[,1])
#      datos_mat_inter[[i]][[j]]$qxt[,(h+6)] <- datos_mat_inter[[i]][[j]]$qxt[,6] + h/5*(datos_mat_inter[[i]][[j]]$qxt[,11]-datos_mat_inter[[i]][[j]]$qxt[,6])
#      datos_mat_inter[[i]][[j]]$qxt[,(h+11)] <- datos_mat_inter[[i]][[j]]$qxt[,11] + h/5*(datos_mat_inter[[i]][[j]]$qxt[,16]-datos_mat_inter[[i]][[j]]$qxt[,11])
#      datos_mat_inter[[i]][[j]]$qxt[,(h+16)] <- datos_mat_inter[[i]][[j]]$qxt[,16] + h/5*(datos_mat_inter[[i]][[j]]$qxt[,21]-datos_mat_inter[[i]][[j]]$qxt[,26])
#      datos_mat_inter[[i]][[j]]$qxt[,(h+21)] <- datos_mat_inter[[i]][[j]]$qxt[,21] + h/5*(datos_mat_inter[[i]][[j]]$qxt[,26]-datos_mat_inter[[i]][[j]]$qxt[,21])
#
#      datos_mat_inter[[i]][[j]]$mxt[,(h+1)] <- datos_mat_inter[[i]][[j]]$mxt[,1] + h/5*(datos_mat_inter[[i]][[j]]$mxt[,6]-datos_mat_inter[[i]][[j]]$mxt[,1])
#      datos_mat_inter[[i]][[j]]$mxt[,(h+6)] <- datos_mat_inter[[i]][[j]]$mxt[,6] + h/5*(datos_mat_inter[[i]][[j]]$mxt[,11]-datos_mat_inter[[i]][[j]]$mxt[,6])
#      datos_mat_inter[[i]][[j]]$mxt[,(h+11)] <- datos_mat_inter[[i]][[j]]$mxt[,11] + h/5*(datos_mat_inter[[i]][[j]]$mxt[,16]-datos_mat_inter[[i]][[j]]$mxt[,11])
#      datos_mat_inter[[i]][[j]]$mxt[,(h+16)] <- datos_mat_inter[[i]][[j]]$mxt[,16] + h/5*(datos_mat_inter[[i]][[j]]$mxt[,21]-datos_mat_inter[[i]][[j]]$mxt[,26])
#      datos_mat_inter[[i]][[j]]$mxt[,(h+21)] <- datos_mat_inter[[i]][[j]]$mxt[,21] + h/5*(datos_mat_inter[[i]][[j]]$mxt[,26]-datos_mat_inter[[i]][[j]]$mxt[,21])
#
#      datos_mat_inter[[i]][[j]]$ax[,(h+1)] <- datos_mat_inter[[i]][[j]]$ax[,1] + h/5*(datos_mat_inter[[i]][[j]]$ax[,6]-datos_mat_inter[[i]][[j]]$ax[,1])
#      datos_mat_inter[[i]][[j]]$ax[,(h+6)] <- datos_mat_inter[[i]][[j]]$ax[,6] + h/5*(datos_mat_inter[[i]][[j]]$ax[,11]-datos_mat_inter[[i]][[j]]$ax[,6])
#      datos_mat_inter[[i]][[j]]$ax[,(h+11)] <- datos_mat_inter[[i]][[j]]$ax[,11] + h/5*(datos_mat_inter[[i]][[j]]$ax[,16]-datos_mat_inter[[i]][[j]]$ax[,11])
#      datos_mat_inter[[i]][[j]]$ax[,(h+16)] <- datos_mat_inter[[i]][[j]]$ax[,16] + h/5*(datos_mat_inter[[i]][[j]]$ax[,21]-datos_mat_inter[[i]][[j]]$ax[,26])
#      datos_mat_inter[[i]][[j]]$ax[,(h+21)] <- datos_mat_inter[[i]][[j]]$ax[,21] + h/5*(datos_mat_inter[[i]][[j]]$ax[,26]-datos_mat_inter[[i]][[j]]$ax[,21])
#
#   }}}
#In this case we do not interpolate the data and we use age-specific death rates
#which corresponds to five periods.



#Once, we have the qxt we calculate the life tables which allow us to get
#the different longevity indicators that we employ in this analysis
n_edad <- n_ages

lifetable = datos_mat$AFG$hombres$qxt[,1]
ax= datos_mat$AFG$hombres$ax[,1]
mx = datos_mat$AFG$hombres$mxt[,1]

#For that we create this function
tablamort <- function(lifetable, ax, mx){
  lx <- vector("numeric", length(lifetable) )
  dx <- vector("numeric", length(lifetable) )
  Lx <- vector("numeric", length(lifetable) )
  Tx <- vector("numeric", length(lifetable))
  ex <- vector("numeric", length(lifetable) )
  ax_ <- ax
  lx[1] <- 100000 #contiene lx de la edad cero
  for(i in 1: (length(lifetable) - 1)) {lx[i+1]<-lx[i]*(1-lifetable[i])}
  for(i in 1: (length(lifetable))) {dx[i] <- lx[i]*lifetable[i]}
  for(i in 1: (length(lifetable)-1)) {Lx[i] <- lx[i+1]*n_edad[i]+ax_[i]*dx[i]
  Lx[length(lifetable)]= lx[length(lifetable)] / mx[length(lifetable)]}
  for(i in 1: (length(lifetable))) {Tx[i]<-sum(Lx[i:length(lifetable)])}
  for(i in 1: (length(lifetable))) {ex[i]<-Tx[i]/lx[i]}
  # lx[x+1] contiene lx de la edad x
  devuelve<-cbind(x=edades,lx=lx,dx=dx,qx=lifetable,ex=ex, mx = mx, ax=ax_, Lx=Lx, Tx=Tx)
  devuelve
}


#Now, we apply this function for all countries and sexes, to get
#in every country its own life table
tablas_mort <- list()
for(i in 1:length(pob)){
  ages <- edades
  nages <- length(ages)
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear,maxyear, 5)
  nyears <- length(years)
  tabla_hombres <- tabla_mujeres <- NULL
  for(h in 1:nyears){
    any <- rep(minyear + 5*(h-1), nages)
    tablaauxh <- cbind(any, tablamort(lifetable = datos_mat[[i]]$hombres$qxt[,h],
                                      ax = datos_mat[[i]]$hombres$ax[,h],
                                      mx = datos_mat[[i]]$hombres$mxt[,h]))
    tablaauxm <- cbind(any, tablamort(lifetable = datos_mat[[i]]$mujeres$qxt[,h],
                                      ax = datos_mat[[i]]$mujeres$ax[,h],
                                      mx = datos_mat[[i]]$mujeres$mxt[,h]))

    tabla_hombres <- rbind(tabla_hombres, tablaauxh)
    tabla_mujeres <- rbind(tabla_mujeres, tablaauxm)
  }
  tablas_mort[[paste0(pob[i])]][[paste0("hombres")]] <- tabla_hombres
  tablas_mort[[paste0(pob[i])]][[paste0("mujeres")]] <- tabla_mujeres
}

#Once, we have the life tables we collect the life expectancy at
#different ages x=0; 65; 70; 75
esperanzas.dm <- list()
for(i in 1:length(pob)){
  #i <- 1
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear,maxyear, 5)
  nyears <- length(years)
  esperanzas.dm[[paste0(pob[i])]]$e0 <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  esperanzas.dm[[paste0(pob[i])]]$e65 <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  esperanzas.dm[[paste0(pob[i])]]$e70 <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  esperanzas.dm[[paste0(pob[i])]]$e75 <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  for(j in 1: nyears){
    esperanzas.dm[[i]]$e0[j, 1] <- tablas_mort[[i]]$hombres[(1+21*(j-1)),6]
    esperanzas.dm[[i]]$e0[j, 2] <- tablas_mort[[i]]$mujeres[(1+21*(j-1)),6]

    esperanzas.dm[[i]]$e65[j, 1] <- tablas_mort[[i]]$hombres[(15+21*(j-1)),6]
    esperanzas.dm[[i]]$e65[j, 2] <- tablas_mort[[i]]$mujeres[(15+21*(j-1)),6]

    esperanzas.dm[[i]]$e70[j, 1] <- tablas_mort[[i]]$hombres[(16+21*(j-1)),6]
    esperanzas.dm[[i]]$e70[j, 2] <- tablas_mort[[i]]$mujeres[(16+21*(j-1)),6]

    esperanzas.dm[[i]]$e75[j, 1] <- tablas_mort[[i]]$hombres[(17+21*(j-1)),6]
    esperanzas.dm[[i]]$e75[j, 2] <- tablas_mort[[i]]$mujeres[(17+21*(j-1)),6]
  }
}

capture.output(esperanzas.dm, file = "Esperanzasdm.txt")

#Also, in-sample we need another mortality indicators:
#Modal age of death,
#Life expectancy at age x = 0, 65, 70, 75 (which we obtain before)
#probability of be alive at x = 0, 65, 70, 75
#Gini Coefficient at x = 0, 65, 70, 75
#Conditional standar desviation x = 0, 65, 70, 75
#Life preparancy (an actuarial longevity indicator)

#Before to start with the estimation fo the mortality indicators in-sample
#we get the values of the deaths
for(i in 1:length(pob)){
  for(j in 1:2){
    ages <- edades
    nages <- length(ages)
    minyear <- min(paises[[i]]$hombres$Year)
    maxyear <- max(paises[[i]]$hombres$Year)
    years <- seq(minyear,maxyear, 5)
    nyears <- length(years)
    datos_mat[[paste0(pob[i])]][[paste0(sex[j])]]$dx <- matrix(tablas_mort[[i]][[j]][,4], nages, nyears,
                                                               dimnames = list(ages, years))

  }
}

#To get Modal age-at-death
edad_modal_DM <- list()
int_edades <- c("<1", "1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35",
                "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70",
                "71-75", "76-80", "81-85", "86-90", "91-95", "95-99")

for(i in 1:length(pob)){
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear,maxyear,5)
  nyears <- length(years)
  nages <- length(int_edades)
  edad_int <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  ed_modal <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  for(h in 1:nyears){
    ed_modal[h,1] <- which(datos_mat[[i]]$hombres$dx[(5:nages),h] == max(datos_mat[[i]]$hombres$dx[(5:nages),h]), arr.ind=TRUE)[1]
    ed_modal[h,2] <- which(datos_mat[[i]]$mujeres$dx[(5:nages),h] == max(datos_mat[[i]]$mujeres$dx[(5:nages),h]), arr.ind=TRUE)[1]

    edad_int[h,1] <- edades[(ed_modal[h,1])+4]
    edad_int[h,2] <- edades[(ed_modal[h,2])+4]
  }
  edad_modal_DM[[paste0(pob[i])]] <- edad_int
}


capture.output(edad_modal_DM, file = "EdadModal_DM.txt")
int_edades

#GET the GINI coefficient
edad_media <- c(0.5, 3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53,
                58, 63, 68, 73, 78, 83, 88, 93, 98)

#The function developed to facilitate the estimations
igini<-function(lifetable){
  lx <- vector("numeric",length(lifetable) )
  dx <- vector("numeric",length(lifetable) )
  Ni0 <- vector("numeric",length(lifetable) )
  Yi0 <- vector("numeric",length(lifetable) )
  Gi0 <- vector("numeric", length(lifetable) )
  Ni65 <- vector("numeric",length(5) )
  Yi65 <- vector("numeric",length(5) )
  Gi65 <- vector("numeric", length(5))
  Ni70 <- vector("numeric",length(4) )
  Yi70 <- vector("numeric",length(4) )
  Gi70 <- vector("numeric", length(4))
  Ni75 <- vector("numeric",length(3) )
  Yi75 <- vector("numeric",length(3) )
  Gi75 <- vector("numeric", length(3))
  lx[1]<-100000 #contiene lx de la edad cero
  for(i in 1: (length(lifetable)-1)) {lx[i+1]<-lx[i]*(1-lifetable[i])}
  for(i in 1: (length(lifetable))) {dx[i]<-lx[i]*lifetable[i]}
  #Para cada edad 0, 65, 70 y 75 necesito
  #0
  for(i in 1: (length(lifetable))){ Ni0[i] <- sum(dx[1:i])/sum(dx)}
  for(i in 1: (length(lifetable))){ Yi0[i] <- sum((dx[1:i]*edad_media[1:i]))/sum((dx*edad_media))}
  Gi0[1] <- Ni0[1] * Yi0[1]
  for(i in 2: (length(lifetable))){ Gi0[i] <- (Ni0[i]-Ni0[i-1])*(Yi0[i-1]+Yi0[i])}

  #65
  for(i in 1: 5){ Ni65[i] <- sum(dx[15:(14+i)])/sum(dx[15:length(lifetable)])}
  for(i in 1: 5){ Yi65[i] <- sum((dx[15:(14+i)]*edad_media[15:(14+i)]))/sum((dx[15:length(lifetable)]*edad_media[15:length(lifetable)]))}
  Gi65[1] <- Ni65[1] * Yi65[1]
  for(i in 2: 5){ Gi65[i] <- (Ni65[i]-Ni65[i-1])*(Yi65[i-1]+Yi65[i])}

  #70
  for(i in 1: 4){ Ni70[i] <- sum(dx[16:(15+i)])/sum(dx[16:length(lifetable)])}
  for(i in 1: 4){ Yi70[i] <- sum((dx[16:(15+i)]*edad_media[16:(15+i)]))/sum((dx[16:length(lifetable)]*edad_media[16:length(lifetable)]))}
  Gi70[1] <- Ni70[1] * Yi70[1]
  for(i in 2: 4){ Gi70[i] <- (Ni70[i]-Ni70[i-1])*(Yi70[i-1]+Yi70[i])}

  #75
  for(i in 1: 3){ Ni75[i] <- sum(dx[17:(16+i)])/sum(dx[17:length(lifetable)])}
  for(i in 1: 3){ Yi75[i] <- sum((dx[17:(16+i)]*edad_media[17:(16+i)]))/sum((dx[17:length(lifetable)]*edad_media[17:length(lifetable)]))}
  Gi75[1] <- Ni75[1] * Yi75[1]
  for(i in 2: 3){ Gi75[i] <- (Ni75[i]-Ni75[i-1])*(Yi75[i-1]+Yi75[i])}

  result<-list(gini0=abs(1-sum(Gi0)), gini65=abs(1-sum(Gi65)),
               gini70=abs(1-sum(Gi70)), gini75=abs(1-sum(Gi75)))
  result
}

#Here, we estimate the gini index for all considered populations
gini.index_DM <- list()
tabl <- c("G0hombres", "G0mujeres", "G65hombres", "G65mujeres",
          "G60hombres", "G60mujeres", "G75hombres", "G75mujeres")

for(i in 1:length(pob)){
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear, maxyear, 5)
  nyears <- length(years)
  nages <- length(edades)
  index.gin <- matrix(NA, nyears, 8, dimnames = list(years, tabl))
  hombres <- mujeres <- NULL
  for(h in 1:nyears){
    hombres <- igini(lifetable = datos_mat[[i]]$hombres$qxt[,h])
    mujeres <- igini(lifetable = datos_mat[[i]]$mujeres$qxt[,h])
    index.gin[h, 1] <- hombres$gini0
    index.gin[h, 2] <- mujeres$gini0
    index.gin[h, 3] <- hombres$gini65
    index.gin[h, 4] <- mujeres$gini65
    index.gin[h, 5] <- hombres$gini70
    index.gin[h, 6] <- mujeres$gini70
    index.gin[h, 7] <- hombres$gini75
    index.gin[h, 8] <- mujeres$gini75
  }
  gini.index_DM[[paste0(pob[i])]] <- index.gin
}

capture.output(gini.index_DM, file = "Index.Gini.DM.txt")

##########################################################################################
#Now we estimate the probability to be alive as:
#p65=l65/l0; p70=l70/l0; p75=l75/l0
#For this issue, we use this function
Pob.ll.vivo <- function(qx){
  lx<- vector("numeric",length(lifetable) )
  lx[1]<-100000 #contiene lx de la edad cero
  for(i in 1: (length(qx)-1)) {lx[i+1]<-lx[i]*(1-qx[i])}
  result <- list(p65 = lx[15]/lx[1], p70 = lx[16]/lx[1], p75 = lx[17]/lx[1])
}

#Here, we calculate the probability to be alive for
#all considered populations
pob.llegarvivo_DM <- list()
tabl.vivo <- c("p65hombres", "p65mujeres", "p70hombres", "p70mujeres",
               "p75hombres", "p75mujeres")
for(i in 1:length(pob)){
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear, maxyear, 5)
  nyears <- length(years)
  nages <- length(edades)
  pobvivo <- matrix(NA, nyears, 6, dimnames = list(years, tabl.vivo))
  hombres <- mujeres <- NULL
  for(h in 1:nyears){
    hombres <- Pob.ll.vivo(qx = datos_mat[[i]]$hombres$qxt[,h])
    mujeres <- Pob.ll.vivo(qx = datos_mat[[i]]$mujeres$qxt[,h])
    pobvivo[h, 1] <- hombres$p65
    pobvivo[h, 2] <- mujeres$p65
    pobvivo[h, 3] <- hombres$p70
    pobvivo[h, 4] <- mujeres$p70
    pobvivo[h, 5] <- hombres$p75
    pobvivo[h, 6] <- mujeres$p75
  }
  pob.llegarvivo_DM[[paste0(pob[i])]] <- pobvivo
}
capture.output(pob.llegarvivo_DM, file = "Pobllvivo.DM.txt")

##########################################################################################
##########################################################################################
#Equally as before, but now with Conditional Standard deviation

#we develop again a function to estimate them
csdev <- function(qx, ax, mx){
  lx <- vector("numeric", length(qx) )
  dx <- vector("numeric", length(qx) )
  Lx <- vector("numeric", length(qx) )
  Tx <- vector("numeric", length(qx))
  ex <- vector("numeric", length(qx) )
  ax_ <- vector("numeric", length(qx))
  Mx_ <- vector("numeric", length(qx))
  ax_ <- ax
  lx[1] <- 100000 #contiene lx de la edad cero
  for(i in 1: (length(qx) - 1)) {lx[i+1]<-lx[i]*(1-qx[i])}
  for(i in 1: (length(qx))) {dx[i] <- lx[i]*qx[i]}
  for(i in 1: (length(qx)-1)) {Lx[i] <- lx[i+1]*n_edad[i]+ax_[i]*dx[i]
  Lx[length(qx)]= lx[length(qx)] / mx[length(qx)]}
  for(i in 1: (length(qx))) {Tx[i]<-sum(Lx[i:length(qx)])}
  for(i in 1: (length(qx))) {ex[i]<-Tx[i]/lx[i]}
  # lx[x+1] contiene lx de la edad x
  Mx0 <- ex[1] + edad_media[1]
  Mx65 <- ex[15] + edad_media[15]
  Mx70 <- ex[16] + edad_media[16]
  Mx75 <- ex[17] + edad_media[17]
  Sx0 <-  sqrt(sum(dx*(edad_media+ax-Mx0)^2)/lx[1])
  Sx65 <-  sqrt(sum(dx[15:19]*(edad_media[15:19]+ax[15:19]-Mx65)^2)/lx[15])
  Sx70 <-  sqrt(sum(dx[16:19]*(edad_media[16:19]+ax[16:19]-Mx70)^2)/lx[16])
  Sx75 <-  sqrt(sum(dx[17:19]*(edad_media[17:19]+ax[17:19]-Mx75)^2)/lx[17])
  devuelve<-list(s0 = Sx0, s65 = Sx65, s70 = Sx70, s75 = Sx75)
  devuelve
}

#Once, I have the function we estimate the conditional standar desviation
#for all considered populations from WHO
con.std.des_DM <- list()
tabl.std <- c("s0hombres", "s0mujeres", "s65hombres", "s65mujeres",
              "s70hombres", "s70mujeres", "s75hombres", "s75mujeres")
for(i in 1:length(pob)){
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear, maxyear, 5)
  nyears <- length(years)
  nages <- length(edades)
  std.des <- matrix(NA, nyears, 8, dimnames = list(years, tabl.std))
  hombres <- mujeres <- NULL
  for(h in 1:nyears){
    hombres <- csdev(qx = datos_mat[[i]]$hombres$qxt[,h], ax = datos_mat[[i]]$hombres$ax[,h],
                     mx = datos_mat[[i]]$hombres$mxt[,h])
    mujeres <- csdev(qx = datos_mat[[i]]$mujeres$qxt[,h], ax = datos_mat[[i]]$mujeres$ax[,h],
                     mx = datos_mat[[i]]$mujeres$mxt[,h])
    std.des[h, 1] <- hombres$s0
    std.des[h, 2] <- mujeres$s0
    std.des[h, 3] <- hombres$s65
    std.des[h, 4] <- mujeres$s65
    std.des[h, 5] <- hombres$s70
    std.des[h, 6] <- mujeres$s70
    std.des[h, 7] <- hombres$s75
    std.des[h, 8] <- mujeres$s75
  }
  con.std.des_DM[[paste0(pob[i])]] <- std.des
}
capture.output(con.std.des_DM, file = "ConStdDesvi.DM.txt")

##########################################################################################
##########################################################################################
#Finally, we develop a function to estimate Life Preparancy
Life_preparancy <- function(qxt, x, z){
  lx <- vector("numeric", length(qxt) )
  lx[1] <- 100000 #contiene lx de la edad cero
  for(t in 1: (length(qxt) - 1)) {lx[t+1]<-lx[t]*(1-qxt[t])}
  ny <- sum(edades <= x)
  lxz <- lx[ny]*(1-z)
  xg <- sum(lx > lxz) - 1
  xs <- 19 - sum(lx < lxz)
  d1 <- lx[(xg+1)] - lxz
  d2 <- lxz - lx[(xs+1)]
  d2[is.na(d2)] <- 0
  d <- d1 + d2
  if(xg == 0){
    yupi = 0 } else{yupi = edad_media[xg]}
  hxz <- (d1/d)*edad_media[xs] + (d2/d)*yupi
  hxz
}

#Once, we have the function we are able to estimate it in all populations
Eprep.vida_DM <- list()
tabl.vida <- c("hage0.50%_hombres", "hage0.50%_mujeres", "hage65.25%_hombres", "hage65.25%_mujeres")
for(i in 1:length(pob)){
  minyear <- min(paises[[i]]$hombres$Year)
  maxyear <- max(paises[[i]]$hombres$Year)
  years <- seq(minyear, maxyear, 5)
  nyears <- length(years)
  prep.vid <- matrix(NA, nyears, 4, dimnames = list(years, tabl.vida))
  hombres1 <- mujeres1 <- NULL
  hombres2 <- mujeres2 <- NULL
  for(h in 1:nyears){
    hombres1 <- Life_preparancy(qxt = datos_mat[[i]]$hombres$qxt[,h],
                                x = 0, z = 0.5)
    hombres2 <- Life_preparancy(qxt = datos_mat[[i]]$hombres$qxt[,h],
                                x = 65, z = 0.25)
    mujeres1 <- Life_preparancy(qxt = datos_mat[[i]]$mujeres$qxt[,h],
                                x = 0, z = 0.5)
    mujeres2 <- Life_preparancy(qxt = datos_mat[[i]]$mujeres$qxt[,h],
                                x = 65, z = 0.25)
    prep.vid[h, 1] <- hombres1
    prep.vid[h, 2] <- mujeres1
    prep.vid[h, 3] <- hombres2
    prep.vid[h, 4] <- mujeres2
  }
  Eprep.vida_DM[[paste0(pob[i])]] <- prep.vid
}
capture.output(Eprep.vida_DM, file = "EdadPrepVida.DM.txt")

#Now, we save the inficators DM (in-sample) in RData
save(esperanzas.dm, edad_modal_DM, gini.index_DM, pob.llegarvivo_DM,
     con.std.des_DM, Eprep.vida_DM,
     file = "indicadores_DM.RData")

##########################################################################################
#FORECAS QXT
##########################################################################################
#For forecast qxt we use the StMoMo library
library(StMoMo)
#####################################################
###########  LC-UNIFACTORIAL ########################
#####################################################
constLC <- function(ax, bx, kt, b0x, gc, wxt, ages){
  c1 <- mean(kt[1, ], na.rm = TRUE)
  c2 <- sum(bx[, 1], na.rm = TRUE)
  list(ax = ax + c1 * bx, bx = bx / c2, kt =  c2 * (kt - c1))
}

#For predict we develop this function that allow us to forecast
#the value of qxt t years ahead.
funcion.predict<-function(t,n,ax,bx,kt,lo){
  #t = number of periods that we want to forecast
  #n = number of ages that we forecast
  lee <- matrix(rep(ax,t), nrow=n, ncol= t) + (matrix(bx, nrow=n, ncol= 1)%*%matrix(kt, nrow=1, ncol= t))
  if(lo==1){ predicciones<-inv.logit(lee) }
  else{ predicciones <- exp(lee) }
  prueba <- list(pred=predicciones)
  prueba
}
inv.logit<-function(x){exp(x)/(1+exp(x))}

#Before forecast qxt we include in our data.frame the variable lx
paises_inter <- list()
for(i in 1:length(pob)){
  for(j in 1:2){
    minyear <- 1990
    maxyear <- 2015
    years <- seq(minyear, maxyear, 5)
    nyears <- length(years)
    nages
    paises_inter_hombres <- paises_inter_mujeres <- NULL
    for(h in 1:nyears){
      any <- rep(minyear + 5*(h-1), nages)
      tablaauxh <- cbind(any, ages,
                         qx = datos_mat[[i]]$hombres$qxt[,h],
                         ax = datos_mat[[i]]$hombres$ax[,h],
                         mx = datos_mat[[i]]$hombres$mx[,h],
                         lx = tablas_mort[[i]]$hombres[(1+nages*(h-1)):(nages+nages*(h-1)),3])
      tablaauxm <- cbind(any, ages,
                         qx = datos_mat[[i]]$mujeres$qxt[,h],
                         ax = datos_mat[[i]]$mujeres$ax[,h],
                         mx = datos_mat[[i]]$mujeres$mx[,h],
                         lx = tablas_mort[[i]]$mujeres[(1+nages*(h-1)):(nages+nages*(h-1)),3])

      paises_inter_hombres <- rbind(paises_inter_hombres, tablaauxh)
      paises_inter_mujeres <- rbind(paises_inter_mujeres, tablaauxm)
    }
    paises_inter[[paste0(pob[i])]][[paste0("hombres")]] <- as.data.frame(paises_inter_hombres)
    paises_inter[[paste0(pob[i])]][[paste0("mujeres")]] <- as.data.frame(paises_inter_mujeres)

  }
}

#We use several list() to save the information of the Lee-Carter adjustment
#and also to save the forecasted values of qxt for every country and for both sexes
library(gnm)
LCfit <- list()
LCfut_mean <- list()
LCfut_low <- list()
LCfut_high <- list()

n_edad2 <- n_edad

for(j in 1:length(pob)){
  for(i in 1:2){
    nages <- length(paises[[j]][[i]][paises[[j]][[i]]$any == 1990,]$ages)
    minyear <- 1990
    maxyear <- 2015
    years <- seq(minyear,maxyear, 5)
    nyears <- length(years)
    wxt_LC <- genWeightMat(ages = nages, years = years, clip = 0)
    edades <- paises[[j]][[i]][paises[[j]][[i]]$any == 1990,]$ages
    n_edad <- n_edad2[1:nages]

    if(length(paises[[j]][[i]][paises[[j]][[i]]$qx > 1, ]$qx) > 0){
      paises[[j]][[i]][paises[[j]][[i]]$qx > 1, ]$qx <- 1

    }

    model <- gnm(paises[[j]][[i]]$qx ~ - 1 + factor(paises[[j]][[i]]$ages),
                 weights = paises[[j]][[i]]$lx, family=quasibinomial, data = paises[[j]][[i]])

    biplotStart <- residSVD(model, factor(paises[[j]][[i]]$ages), factor(paises[[j]][[i]]$any),1)
    ainih <- coef(model)+biplotStart[1]*biplotStart[(nages+1)]*biplotStart[1:nages]/biplotStart[1]
    binih <- biplotStart[1:nages]/biplotStart[1]
    kinih <- biplotStart[1]*biplotStart[(nages+1):(nyears+nages)]-biplotStart[1]*biplotStart[(nages+1)]

    lc1 <- gnm(paises[[j]][[i]]$qx ~ - 1 + factor(paises[[j]][[i]]$ages) + Mult(factor(paises[[j]][[i]]$ages), factor(paises[[j]][[i]]$any)),
               weights = paises[[j]][[i]]$lx, constrain= c((nages+1), (nages*2+2-1)), constrainTo=c(1,0)
               ,family=quasibinomial, data = paises[[j]][[i]], start=c(ainih, binih, kinih))

    ax <- as.numeric(coef(lc1)[1:nages]+mean(c(0,coef(lc1)[(nages*2+2):(2*nages + nyears)])*sum(c(1,coef(lc1)[(nages+2):(nages*2)])))*c(1,coef(lc1)[(nages+2):(nages*2)])/sum(c(1,coef(lc1)[(nages+2):(nages*2)])))
    bx <- as.numeric(c(1,coef(lc1)[(nages+2):(nages*2)])/sum(c(1,coef(lc1)[(nages+2):(nages*2)])))
    kt <- as.numeric(c(0,coef(lc1)[(nages*2+2):(2*nages + nyears)])*sum(c(1,coef(lc1)[(nages+2):(nages*2)]))-mean(c(0,coef(lc1)[(nages*2+2):(2*nages + nyears)])*sum(c(1,coef(lc1)[(nages+2):(nages*2)]))))

    ktfut_mean <- forecast(auto.arima(kt), (4))$mean[1:(4)]
    ktfut_high <- forecast(auto.arima(kt), (4))$upper[1:(4),2]
    ktfut_low <- forecast(auto.arima(kt), (4))$lower[1:(4),2]

    qx_LC <- funcion.predict(nyears, nages, ax, bx, kt, 1)$pred
    qxfut_LC_mean <- funcion.predict((4), nages, ax, bx, ktfut_mean, 1)$pred
    qxfut_LC_high <- funcion.predict((4), nages, ax, bx, ktfut_high, 1)$pred
    qxfut_LC_low <- funcion.predict((4), nages, ax, bx, ktfut_low, 1)$pred

    LCfit[[paste0(pob[j])]][[paste0(sex[i])]]$ax <- ax
    LCfit[[paste0(pob[j])]][[paste0(sex[i])]]$bx <- bx
    LCfit[[paste0(pob[j])]][[paste0(sex[i])]]$kt <- kt
    LCfit[[paste0(pob[j])]][[paste0(sex[i])]]$qxtLCdm <- matrix(qx_LC, ncol=nyears, nrow=nages, dimnames = list(edades, years))

    LCfut_mean[[paste0(pob[j])]][[paste0(sex[i])]]$kt <- ktfut_mean
    LCfut_mean[[paste0(pob[j])]][[paste0(sex[i])]]$qxtLCfm <- matrix(qxfut_LC_mean, ncol=(4), nrow=nages, dimnames = list(edades, seq(2020, 2035, by = 5)))
    LCfut_mean[[paste0(pob[j])]][[paste0(sex[i])]]$mxtLCfm <-
      matrix(qxfut_LC_mean, ncol=4, nrow=nages)/(matrix(n_edad, ncol=4, nrow=nages)-matrix(qxfut_LC_mean, ncol=4, nrow=nages)*
                                                   (matrix(n_edad, ncol=4, nrow=nages)-matrix(datos_mat[[j]][[i]]$ax[(1:nages),nyears],ncol=4, nrow=nages)))

    LCfut_high[[paste0(pob[j])]][[paste0(sex[i])]]$kt <- ktfut_high
    LCfut_high[[paste0(pob[j])]][[paste0(sex[i])]]$qxtLCfm <- matrix(qxfut_LC_high, ncol=(4), nrow=nages, dimnames = list(edades, seq(2020, 2035, by = 5)))
    LCfut_high[[paste0(pob[j])]][[paste0(sex[i])]]$mxtLCfm <-
      matrix(qxfut_LC_high, ncol=4, nrow=nages)/(matrix(n_edad, ncol=4, nrow=nages)-matrix(qxfut_LC_high, ncol=4, nrow=nages)*
                                                   (matrix(n_edad, ncol=4, nrow=nages)-matrix(datos_mat[[j]][[i]]$ax[(1:nages),nyears],ncol=4, nrow=nages)))

    LCfut_low[[paste0(pob[j])]][[paste0(sex[i])]]$kt <- ktfut_low
    LCfut_low[[paste0(pob[j])]][[paste0(sex[i])]]$qxtLCfm <- matrix(qxfut_LC_low, ncol=(4), nrow=nages, dimnames = list(edades, seq(2020, 2035, by = 5)))
    LCfut_low[[paste0(pob[j])]][[paste0(sex[i])]]$mxtLCfm <-
      matrix(qxfut_LC_low, ncol=4, nrow=nages)/(matrix(n_edad, ncol=4, nrow=nages)-matrix(qxfut_LC_low, ncol=4, nrow=nages)*
                                                  (matrix(n_edad, ncol=4, nrow=nages)-matrix(datos_mat[[j]][[i]]$ax[(1:nages),nyears],ncol=4, nrow=nages)))

  }}

warnings()

LCfut_mean$CZE$hombres$qxtLCfm
LCfut_mean$CZE$mujeres$qxtLCfm
LCfut_mean$CZE$mujeres$mxtLCfm
paises$CZE$mujeres$qx

library(scatterplot3d)
library(rgl)

#If you want to show the qxt surfaces of one country
j <- 30
plot3d(edades, seq(min(paises[[j]][[1]]$any), max(paises[[j]][[1]]$any), by=5), log(LCfit[[j]][[1]]$qxtLCdm), col="red", type="n", zlab = "log(qxt)",ylab = "t=periodo",xlab = "edad = x")
surface3d(edades, seq(min(paises[[j]][[1]]$any), max(paises[[j]][[1]]$any), by=5), log(LCfit[[j]][[1]]$qxtLCdm), col="blue")
surface3d(edades, seq(min(paises[[j]][[2]]$any), max(paises[[j]][[2]]$any), by=5), log(LCfit[[j]][[2]]$qxtLCdm), col="red")

#Check if there are any NA
num.NA <- matrix(NA, length(pob), 2, dimnames = list(pob, sex))
length(pob)
for(i in 1:length(pob)){
  num.NA[i, 1] <- sum(is.na(LCfut_mean[[i]]$hombres$qxtLCfm))
  num.NA[i, 2] <- sum(is.na(LCfut_mean[[i]]$mujeres$qxtLCfm))

}
sum(num.NA[,1])
sum(num.NA[,2])

#Save the information fo the LC forecast
save(LCfit, LCfut_mean, LCfut_high, LCfut_low,
     file = "AjusteLC+qxtDM_y_FM+Intervalos.RData")


################################################################################################################
#Once we have the qxt for out-of-sample we again estimate the life tables
#but now with the forecasted values of qxt
tablas_mort_proyectada <- list()
tablas_mort_proyectada_low <- list()
tablas_mort_proyectada_high <- list()
for(i in 1:length(pob)){
  ages <- edades
  nages <- length(ages)
  minyear_dm <- min(paises[[i]]$hombres$any)
  maxyear_dm <- max(paises[[i]]$hombres$any)
  years_dm <- seq(minyear_dm,maxyear_dm,5)
  minyear_fm <- 2020
  nyears_dm <- length(years_dm)
  years_fm <- seq(minyear_fm,2035, by= 5)
  nyears_fm <- length(years_fm)
  tabla_hombres <- tabla_mujeres <- NULL
  tabla_hombreshig <- tabla_mujereshig <- NULL
  tabla_hombreslow <- tabla_mujereslow <- NULL
  mxt_fue_hom <- mxt_fue_muj <- NULL
  mxt_fue_hom.low <- mxt_fue_muj.low <- NULL
  mxt_fue_hom.high <- mxt_fue_muj.high <- NULL
  for(h in 1:nyears_fm){
    any <- rep(minyear_fm + 5*(h-1) , nages)
    #mean
    tablaauxh <- cbind(any, tablamort(lifetable = matrix(LCfut_mean[[i]]$hombres$qxtLCfm, ncol=nyears_fm, nrow=nages)[,h],
                                      ax = datos_mat[[i]]$hombres$ax[,nyears_dm],
                                      mx = matrix(LCfut_mean[[i]]$hombres$mxtLCfm, ncol=nyears_fm, nrow = nages)[,h]))
    tablaauxm <- cbind(any, tablamort(lifetable = matrix(LCfut_mean[[i]]$mujeres$qxtLCfm, ncol=nyears_fm, nrow=nages)[,h],
                                      ax = datos_mat[[i]]$mujeres$ax[,nyears_dm],
                                      mx = matrix(LCfut_mean[[i]]$mujeres$mxtLCfm, ncol=nyears_fm, nrow = nages)[,h]))

    tabla_hombres <- rbind(tabla_hombres, tablaauxh)
    tabla_mujeres <- rbind(tabla_mujeres, tablaauxm)
    #high
    tabla_ahhig <- cbind(any, tablamort(lifetable = matrix(LCfut_high[[i]]$hombres$qxtLCfm, ncol=nyears_fm, nrow=nages)[,h],
                                        ax = datos_mat[[i]]$hombres$ax[,nyears_dm],
                                        mx = matrix(LCfut_high[[i]]$hombres$mxtLCfm, ncol=nyears_fm, nrow = nages)[,h]))
    tabla_muhig <- cbind(any, tablamort(lifetable = matrix(LCfut_high[[i]]$mujeres$qxtLCfm, ncol=nyears_fm, nrow=nages)[,h],
                                        ax = datos_mat[[i]]$mujeres$ax[,nyears_dm],
                                        mx = matrix(LCfut_high[[i]]$mujeres$mxtLCfm, ncol=nyears_fm, nrow = nages)[,h]))

    tabla_hombreshig <- rbind(tabla_hombreshig, tabla_ahhig)
    tabla_mujereshig <- rbind(tabla_mujereshig, tabla_muhig)
    #low
    tabla_hlow <- cbind(any, tablamort(lifetable = matrix(LCfut_low[[i]]$hombres$qxtLCfm, ncol=nyears_fm, nrow=nages)[,h],
                                       ax = datos_mat[[i]]$hombres$ax[,nyears_dm],
                                       mx = matrix(LCfut_low[[i]]$hombres$mxtLCfm, ncol=nyears_fm, nrow = nages)[,h]))
    tabla_mlow <- cbind(any, tablamort(lifetable = matrix(LCfut_low[[i]]$mujeres$qxtLCfm, ncol=nyears_fm, nrow=nages)[,h],
                                       ax = datos_mat[[i]]$mujeres$ax[,nyears_dm],
                                       mx = matrix(LCfut_low[[i]]$mujeres$mxtLCfm, ncol=nyears_fm, nrow = nages)[,h]))

    tabla_hombreslow <- rbind(tabla_hombreslow, tabla_hlow)
    tabla_mujereslow <- rbind(tabla_mujereslow, tabla_mlow)

  }
  tablas_mort_proyectada[[paste0(pob[i])]][[paste0("hombres")]] <- tabla_hombres
  tablas_mort_proyectada[[paste0(pob[i])]][[paste0("mujeres")]] <- tabla_mujeres
  #HIGH
  tablas_mort_proyectada_high[[paste0(pob[i])]][[paste0("hombres")]] <- tabla_hombreshig
  tablas_mort_proyectada_high[[paste0(pob[i])]][[paste0("mujeres")]] <- tabla_mujereshig
  #LOW
  tablas_mort_proyectada_low[[paste0(pob[i])]][[paste0("hombres")]] <- tabla_hombreslow
  tablas_mort_proyectada_low[[paste0(pob[i])]][[paste0("mujeres")]] <- tabla_mujereslow
}


#Now, we collect the data from life expectancy that we need
esperanzas.fm <- list()
esperanzas.fm_low <- list()
esperanzas.fm_hig <- list()
tabla.esperanzas <- c("e0hombres", "e0mujeres", "e65hombres", "e65mujeres",
                      "e70hombres", "e70mujeres", "e75hombres", "e75mujeres")

for(i in 1:length(pob)){
  maxyear <- 2020
  years <- seq(2020,2035, by = 5)
  nyears <- length(years)
  esperanzas.fm[[paste0(pob[i])]] <- matrix(NA, nyears, 8, dimnames = list(years, tabla.esperanzas))
  esperanzas.fm_hig[[paste0(pob[i])]] <- matrix(NA, nyears, 8, dimnames = list(years, tabla.esperanzas))
  esperanzas.fm_low[[paste0(pob[i])]] <- matrix(NA, nyears, 8, dimnames = list(years, tabla.esperanzas))
  for(j in 1: nyears){
    esperanzas.fm[[i]][j, 1] <- tablas_mort_proyectada[[i]]$hombres[(1+22*(j-1)),6]
    esperanzas.fm[[i]][j, 2] <- tablas_mort_proyectada[[i]]$mujeres[(1+22*(j-1)),6]
    esperanzas.fm[[i]][j, 3] <- tablas_mort_proyectada[[i]]$hombres[(15+22*(j-1)),6]
    esperanzas.fm[[i]][j, 4] <- tablas_mort_proyectada[[i]]$mujeres[(15+22*(j-1)),6]
    esperanzas.fm[[i]][j, 5] <- tablas_mort_proyectada[[i]]$hombres[(16+22*(j-1)),6]
    esperanzas.fm[[i]][j, 6] <- tablas_mort_proyectada[[i]]$mujeres[(16+22*(j-1)),6]
    esperanzas.fm[[i]][j, 7] <- tablas_mort_proyectada[[i]]$hombres[(17+22*(j-1)),6]
    esperanzas.fm[[i]][j, 8] <- tablas_mort_proyectada[[i]]$mujeres[(17+22*(j-1)),6]
    #HIGH
    esperanzas.fm_hig[[i]][j, 1] <- tablas_mort_proyectada_high[[i]]$hombres[(1+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 2] <- tablas_mort_proyectada_high[[i]]$mujeres[(1+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 3] <- tablas_mort_proyectada_high[[i]]$hombres[(15+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 4] <- tablas_mort_proyectada_high[[i]]$mujeres[(15+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 5] <- tablas_mort_proyectada_high[[i]]$hombres[(16+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 6] <- tablas_mort_proyectada_high[[i]]$mujeres[(16+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 7] <- tablas_mort_proyectada_high[[i]]$hombres[(17+22*(j-1)),6]
    esperanzas.fm_hig[[i]][j, 8] <- tablas_mort_proyectada_high[[i]]$mujeres[(17+22*(j-1)),6]
    #LOW
    esperanzas.fm_low[[i]][j, 1] <- tablas_mort_proyectada_low[[i]]$hombres[(1+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 2] <- tablas_mort_proyectada_low[[i]]$mujeres[(1+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 3] <- tablas_mort_proyectada_low[[i]]$hombres[(15+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 4] <- tablas_mort_proyectada_low[[i]]$mujeres[(15+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 5] <- tablas_mort_proyectada_low[[i]]$hombres[(16+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 6] <- tablas_mort_proyectada_low[[i]]$mujeres[(16+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 7] <- tablas_mort_proyectada_low[[i]]$hombres[(17+22*(j-1)),6]
    esperanzas.fm_low[[i]][j, 8] <- tablas_mort_proyectada_low[[i]]$mujeres[(17+22*(j-1)),6]
  }
}

capture.output(esperanzas.fm, file = "Esperanzasfm.txt")
capture.output(esperanzas.fm_hig, file = "Esperanzasfm_hig.txt")
capture.output(esperanzas.fm_low, file = "Esperanzasfm_low.txt")

########################################################################################################
########################################################################################################

#Now, we obtain the modal age at death out-of-sample
int_edades
datos_mat_fm <- list()
datos_mat_fm_high <- list()
datos_mat_fm_low <- list()
for(i in 1:length(pob)){
  for(j in 1:2){
    ages <- edades
    nages <- length(ages)
    minyear <- 2020
    maxyear <- 2035
    years <- seq(minyear,maxyear, by = 5)
    nyears <- length(years)
    #mean
    datos_mat_fm[[paste0(pob[i])]][[paste0(sex[j])]]$lx <- matrix(tablas_mort_proyectada[[i]][[j]][,3], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm[[paste0(pob[i])]][[paste0(sex[j])]]$dx <- matrix(tablas_mort_proyectada[[i]][[j]][,4], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm[[paste0(pob[i])]][[paste0(sex[j])]]$qx <- matrix(tablas_mort_proyectada[[i]][[j]][,5], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm[[paste0(pob[i])]][[paste0(sex[j])]]$ex <- matrix(tablas_mort_proyectada[[i]][[j]][,6], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm[[paste0(pob[i])]][[paste0(sex[j])]]$mx <- matrix(LCfut_mean[[i]][[j]]$mxtLCfm, nages, nyears, dimnames = list(ages, years))
    #high
    datos_mat_fm_high[[paste0(pob[i])]][[paste0(sex[j])]]$lx <- matrix(tablas_mort_proyectada_high[[i]][[j]][,3], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_high[[paste0(pob[i])]][[paste0(sex[j])]]$dx <- matrix(tablas_mort_proyectada_high[[i]][[j]][,4], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_high[[paste0(pob[i])]][[paste0(sex[j])]]$qx <- matrix(tablas_mort_proyectada_high[[i]][[j]][,5], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_high[[paste0(pob[i])]][[paste0(sex[j])]]$ex <- matrix(tablas_mort_proyectada_low[[i]][[j]][,6], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_high[[paste0(pob[i])]][[paste0(sex[j])]]$mx <- matrix(LCfut_high[[i]][[j]]$mxtLCfm, nages, nyears, dimnames = list(ages, years))
    #mean
    datos_mat_fm_low[[paste0(pob[i])]][[paste0(sex[j])]]$lx <- matrix(tablas_mort_proyectada_low[[i]][[j]][,3], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_low[[paste0(pob[i])]][[paste0(sex[j])]]$dx <- matrix(tablas_mort_proyectada_low[[i]][[j]][,4], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_low[[paste0(pob[i])]][[paste0(sex[j])]]$qx <- matrix(tablas_mort_proyectada_low[[i]][[j]][,5], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_low[[paste0(pob[i])]][[paste0(sex[j])]]$ex <- matrix(tablas_mort_proyectada_low[[i]][[j]][,6], nages, nyears, dimnames = list(ages, years))
    datos_mat_fm_low[[paste0(pob[i])]][[paste0(sex[j])]]$mx <- matrix(LCfut_low[[i]][[j]]$mxtLCfm, nages, nyears, dimnames = list(ages, years))

  }
}

#Remark that we want long modal age at death
int_edades
edad_modal_fm <- list()
edad_modal_fm_low <- list()
edad_modal_fm_high <- list()
for(i in 1:length(pob)){
  minyear <- 2020
  maxyear <- 2035
  years <- seq(minyear,maxyear, by = 5)
  nyears <- length(years)
  nages <- length(ages)
  ed_modal <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  ed_modal_low <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  ed_modal_high <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  ed_int <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  ed_int_low <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  ed_int_high <- matrix(NA, nyears, 2, dimnames = list(years, sex))
  for(h in 1:nyears){
    ed_modal[h,1] <- which(datos_mat_fm[[i]]$hombres$dx[(5:nages),h] == max(datos_mat_fm[[i]]$hombres$dx[(5:nages),h]), arr.ind=TRUE)[1]
    ed_modal[h,2] <- which(datos_mat_fm[[i]]$mujeres$dx[(5:nages),h] == max(datos_mat_fm[[i]]$mujeres$dx[(5:nages),h]), arr.ind=TRUE)[1]

    ed_int[h,1] <- edades[(ed_modal[h,1]+4)]
    ed_int[h,2] <- edades[(ed_modal[h,2]+4)]

    #LOW
    ed_modal_low[h,1] <- which(datos_mat_fm_low[[i]]$hombres$dx[(5:nages),h] == max(datos_mat_fm_low[[i]]$hombres$dx[(5:nages),h]), arr.ind=TRUE)[1]
    ed_modal_low[h,2] <- which(datos_mat_fm_low[[i]]$mujeres$dx[(5:nages),h] == max(datos_mat_fm_low[[i]]$mujeres$dx[(5:nages),h]), arr.ind=TRUE)[1]

    ed_int_low[h,1] <- edades[(ed_modal_low[h,1]+4)]
    ed_int_low[h,2] <- edades[(ed_modal_low[h,2]+4)]

    #HIGH
    ed_modal_high[h,1] <- which(datos_mat_fm_high[[i]]$hombres$dx[(5:nages),h] == max(datos_mat_fm_high[[i]]$hombres$dx[(5:nages),h]), arr.ind=TRUE)[1]
    ed_modal_high[h,2] <- which(datos_mat_fm_high[[i]]$mujeres$dx[(5:nages),h] == max(datos_mat_fm_high[[i]]$mujeres$dx[(5:nages),h]), arr.ind=TRUE)[1]

    ed_int_high[h,1] <- edades[(ed_modal_high[h,1]+4)]
    ed_int_high[h,2] <- edades[(ed_modal_high[h,2]+4)]

  }
  edad_modal_fm[[paste0(pob[i])]] <- ed_int
  edad_modal_fm_low[[paste0(pob[i])]] <- ed_int_low
  edad_modal_fm_high[[paste0(pob[i])]] <- ed_int_high
}

capture.output(edad_modal_fm, file = "edad_modal_fm.txt")
capture.output(edad_modal_fm_high, file = "edad_modal_fm_high.txt")
capture.output(edad_modal_fm_low, file = "edad_modal_fm_low.txt")

###################################################################################################
###################################################################################################
#GIni index out-of-sample
gini.index_FM <- list()
gini.index_FM.low <- list()
gini.index_FM.high <- list()
tabl <- c("G0hombres", "G0mujeres", "G65hombres", "G65mujeres",
          "G60hombres", "G60mujeres", "G75hombres", "G75mujeres")
i <- 1
for(i in 1:length(pob)){
  minyear <- 2020
  maxyear <- 2035
  years <- seq(minyear,maxyear, by = 5)
  nyears_fm <- length(years)
  nyears_dm <- length(seq(min(paises[[i]]$hombres$any),(max(paises[[i]]$hombres$any)),1))
  nages <- length(edades)
  index.gin <- matrix(NA, nyears_fm, 8, dimnames = list(years, tabl))
  index.gin.low <- matrix(NA, nyears_fm, 8, dimnames = list(years, tabl))
  index.gin.high <- matrix(NA, nyears_fm, 8, dimnames = list(years, tabl))
  hombres <-  mujeres <- total <- NULL
  hombres.low <- mujeres.low <- NULL
  hombres.high <- mujeres.high <- NULL
  for(h in 1:nyears_fm){
    #MEAN
    hombres <- igini(lifetable = matrix(LCfut_mean[[i]]$hombres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h])
    mujeres <- igini(lifetable = matrix(LCfut_mean[[i]]$mujeres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h])
    index.gin[h, 1] <- hombres$gini0
    index.gin[h, 2] <- mujeres$gini0
    index.gin[h, 3] <- hombres$gini65
    index.gin[h, 4] <- mujeres$gini65
    index.gin[h, 5] <- hombres$gini70
    index.gin[h, 6] <- mujeres$gini70
    index.gin[h, 7] <- hombres$gini75
    index.gin[h, 8] <- mujeres$gini75
    #LOW
    hombres.low <- igini(lifetable = matrix(LCfut_low[[i]]$hombres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h])
    mujeres.low <- igini(lifetable = matrix(LCfut_low[[i]]$mujeres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h])
    index.gin.low[h, 1] <- hombres.low$gini0
    index.gin.low[h, 2] <- mujeres.low$gini0
    index.gin.low[h, 3] <- hombres.low$gini65
    index.gin.low[h, 4] <- mujeres.low$gini65
    index.gin.low[h, 5] <- hombres.low$gini70
    index.gin.low[h, 6] <- mujeres.low$gini70
    index.gin.low[h, 7] <- hombres.low$gini75
    index.gin.low[h, 8] <- mujeres.low$gini75
    #HIGH
    hombres.high <- igini(lifetable = matrix(LCfut_high[[i]]$hombres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h])
    mujeres.high <- igini(lifetable = matrix(LCfut_high[[i]]$mujeres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h])
    index.gin.high[h, 1] <- hombres.high$gini0
    index.gin.high[h, 2] <- mujeres.high$gini0
    index.gin.high[h, 3] <- hombres.high$gini65
    index.gin.high[h, 4] <- mujeres.high$gini65
    index.gin.high[h, 5] <- hombres.high$gini70
    index.gin.high[h, 6] <- mujeres.high$gini70
    index.gin.high[h, 7] <- hombres.high$gini75
    index.gin.high[h, 8] <- mujeres.high$gini75

  }
  gini.index_FM[[paste0(pob[i])]] <- index.gin
  gini.index_FM.low[[paste0(pob[i])]] <- index.gin.low
  gini.index_FM.high[[paste0(pob[i])]] <- index.gin.high
}

capture.output(gini.index_FM, file = "Index.Gini.FM.txt")
capture.output(gini.index_FM.low, file = "Index.Gini.FM.low.txt")
capture.output(gini.index_FM.high, file = "Index.Gini.FM.high.txt")

##########################################################################################
##########################################################################################
#Now, we calculate the probability of be alived
pob.llegarvivo_FM <- list()
pob.llegarvivo_FM.low <- list()
pob.llegarvivo_FM.high <- list()
tabl.vivo <- c("p65hombres", "p65mujeres", "p70hombres", "p70mujeres",
               "p75hombres", "p75mujeres")
for(i in 1:length(pob)){
  minyear <- 2020
  maxyear <- 2035
  years <- seq(minyear,maxyear, by = 5)
  nyears <- length(years)
  nages <- length(edades)
  pobvivo <- matrix(NA, nyears, 6, dimnames = list(years, tabl.vivo))
  pobvivo.low <- matrix(NA, nyears, 6, dimnames = list(years, tabl.vivo))
  pobvivo.high <- matrix(NA, nyears, 6, dimnames = list(years, tabl.vivo))
  hombres <- mujeres <-  NULL
  hombres.low <- mujeres.low <- NULL
  hombres.high <- mujeres.high <- NULL
  for(h in 1:nyears){
    #Mean
    hombres <- Pob.ll.vivo(qx = matrix(LCfut_mean[[i]]$hombres$qxtLCfm, ncol = nyears, nrow = nages)[,h])
    mujeres <- Pob.ll.vivo(qx = matrix(LCfut_mean[[i]]$mujeres$qxtLCfm, ncol = nyears, nrow = nages)[,h])
    pobvivo[h, 1] <- hombres$p65
    pobvivo[h, 2] <- mujeres$p65
    pobvivo[h, 3] <- hombres$p70
    pobvivo[h, 4] <- mujeres$p70
    pobvivo[h, 5] <- hombres$p75
    pobvivo[h, 6] <- mujeres$p75
    #Low
    hombres.low <- Pob.ll.vivo(qx = matrix(LCfut_low[[i]]$hombres$qxtLCfm, ncol = nyears, nrow = nages)[,h])
    mujeres.low <- Pob.ll.vivo(qx = matrix(LCfut_low[[i]]$mujeres$qxtLCfm, ncol = nyears, nrow = nages)[,h])
    pobvivo.low[h, 1] <- hombres.low$p65
    pobvivo.low[h, 2] <- mujeres.low$p65
    pobvivo.low[h, 3] <- hombres.low$p70
    pobvivo.low[h, 4] <- mujeres.low$p70
    pobvivo.low[h, 5] <- hombres.low$p75
    pobvivo.low[h, 6] <- mujeres.low$p75
    #High
    hombres.high <- Pob.ll.vivo(qx = matrix(LCfut_high[[i]]$hombres$qxtLCfm, ncol = nyears, nrow = nages)[,h])
    mujeres.high <- Pob.ll.vivo(qx = matrix(LCfut_high[[i]]$mujeres$qxtLCfm, ncol = nyears, nrow = nages)[,h])
    pobvivo.high[h, 1] <- hombres.high$p65
    pobvivo.high[h, 2] <- mujeres.high$p65
    pobvivo.high[h, 3] <- hombres.high$p70
    pobvivo.high[h, 4] <- mujeres.high$p70
    pobvivo.high[h, 5] <- hombres.high$p75
    pobvivo.high[h, 6] <- mujeres.high$p75
  }
  pob.llegarvivo_FM[[paste0(pob[i])]] <- pobvivo
  pob.llegarvivo_FM.low[[paste0(pob[i])]] <- pobvivo.low
  pob.llegarvivo_FM.high[[paste0(pob[i])]] <- pobvivo.high
}
capture.output(pob.llegarvivo_FM, file = "Pobllvivo.FM.txt")
capture.output(pob.llegarvivo_FM.low, file = "Pobllvivo.FM.low.txt")
capture.output(pob.llegarvivo_FM.high, file = "Pobllvivo.FM.high.txt")

##########################################################################################
##########################################################################################
#CONDITIONAL STANDARD DESVIATION
con.std.des_FM <- list()
con.std.des_FM.low <- list()
con.std.des_FM.high <- list()
tabl.std <- c("s0hombres", "s0mujeres", "s65hombres", "s65mujeres",
              "s70hombres", "s70mujeres", "s75hombres", "s75mujeres")
edad_media
i <- 1
for(i in 1:length(pob)){
  minyear <- 2020
  maxyear <- 2035
  years <- seq(minyear,maxyear, by = 5)
  nyears_fm <- length(years)
  nyears_dm <- length(seq(min(paises[[i]]$hombres$any),max(paises[[i]]$hombres$any),5))
  nages <- length(edades)
  std.des <- matrix(NA, nyears_fm, 8, dimnames = list(years, tabl.std))
  std.des.low <- matrix(NA, nyears_fm, 8, dimnames = list(years, tabl.std))
  std.des.high <- matrix(NA, nyears_fm, 8, dimnames = list(years, tabl.std))
  hombres <- mujeres <- NULL
  hombres.low <- mujeres.low <- NULL
  hombres.high <- mujeres.high <- NULL
  mxt_fue_hom <- mxt_fue_muj <- NULL
  mxt_fue_hom.low <- mxt_fue_muj.low <- NULL
  mxt_fue_hom.high <- mxt_fue_muj.high <- NULL
  for(h in 1:nyears_fm){
    #MEAN
    hombres <- csdev(qx = matrix(LCfut_mean[[i]]$hombres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h],
                     ax = datos_mat[[i]]$hombres$ax[,nyears_dm],
                     mx = datos_mat_fm[[i]]$hombres$mx[,h])
    mujeres <- csdev(qx = matrix(LCfut_mean[[i]]$mujeres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h],
                     ax = datos_mat[[i]]$mujeres$ax[,nyears_dm],
                     mx = datos_mat_fm[[i]]$mujeres$mx[,h])
    std.des[h, 1] <- hombres$s0
    std.des[h, 2] <- mujeres$s0
    std.des[h, 3] <- hombres$s65
    std.des[h, 4] <- mujeres$s65
    std.des[h, 5] <- hombres$s70
    std.des[h, 6] <- mujeres$s70
    std.des[h, 7] <- hombres$s75
    std.des[h, 8] <- mujeres$s75
    #LOW
    hombres.low <- csdev(qx = matrix(LCfut_low[[i]]$hombres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h],
                         ax = datos_mat[[i]]$hombres$ax[,nyears_dm],
                         mx = datos_mat_fm_low[[i]]$hombres$mx[,h])
    mujeres.low <- csdev(qx = matrix(LCfut_low[[i]]$mujeres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h],
                         ax = datos_mat[[i]]$mujeres$ax[,nyears_dm],
                         mx = datos_mat_fm_low[[i]]$mujeres$mx[,h])
    std.des.low[h, 1] <- hombres.low$s0
    std.des.low[h, 2] <- mujeres.low$s0
    std.des.low[h, 3] <- hombres.low$s65
    std.des.low[h, 4] <- mujeres.low$s65
    std.des.low[h, 5] <- hombres.low$s70
    std.des.low[h, 6] <- mujeres.low$s70
    std.des.low[h, 7] <- hombres.low$s75
    std.des.low[h, 8] <- mujeres.low$s75
    #HIGH
    hombres.high <- csdev(qx = matrix(LCfut_high[[i]]$hombres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h],
                          ax = datos_mat[[i]]$hombres$ax[,nyears_dm],
                          mx = datos_mat_fm_high[[i]]$hombres$mx[,h])
    mujeres.high <- csdev(qx = matrix(LCfut_high[[i]]$mujeres$qxtLCfm, ncol = nyears_fm, nrow = nages)[,h],
                          ax = datos_mat[[i]]$mujeres$ax[,nyears_dm],
                          mx = datos_mat_fm_high[[i]]$mujeres$mx[,h])
    std.des.high[h, 1] <- hombres.high$s0
    std.des.high[h, 2] <- mujeres.high$s0
    std.des.high[h, 3] <- hombres.high$s65
    std.des.high[h, 4] <- mujeres.high$s65
    std.des.high[h, 5] <- hombres.high$s70
    std.des.high[h, 6] <- mujeres.high$s70
    std.des.high[h, 7] <- hombres.high$s75
    std.des.high[h, 8] <- mujeres.high$s75

  }
  con.std.des_FM[[paste0(pob[i])]] <- std.des
  con.std.des_FM.low[[paste0(pob[i])]] <- std.des.low
  con.std.des_FM.high[[paste0(pob[i])]] <- std.des.high
}
capture.output(con.std.des_FM, file = "ConStdDesvi.FM.txt")
capture.output(con.std.des_FM.low, file = "ConStdDesvi.FM.low.txt")
capture.output(con.std.des_FM.high, file = "ConStdDesvi.FM.high.txt")

##########################################################################################
##########################################################################################
#Finally, Life Preparancy
Eprep.vida <- list()
Eprep.vida.low <- list()
Eprep.vida.high <- list()
tabl.vida <- c("hage0.50%_hombres", "hage0.50%_mujeres",
               "hage65.25%_hombres", "hage65.25%_mujeres")
for(i in 1:length(pob)){
  minyear <- 2020
  maxyear <- 2035
  years <- seq(minyear,maxyear, by = 5)
  nyears <- length(years)
  nages <- length(edades)
  prep.vid <- matrix(NA, nyears, 4, dimnames = list(years, tabl.vida))
  prep.vid.low <- matrix(NA, nyears, 4, dimnames = list(years, tabl.vida))
  prep.vid.high <- matrix(NA, nyears, 4, dimnames = list(years, tabl.vida))
  hombres1 <- mujeres1 <- NULL
  hombres2 <- mujeres2 <- NULL
  #LOW
  hombres1.low <- mujeres1.low <- NULL
  hombres2.low <- mujeres2.low <- NULL
  #HIGH
  hombres1.high <- mujeres1.high <- NULL
  hombres2.high <- mujeres2.high <- NULL
  for(h in 1:nyears){
    hombres1 <- Life_preparancy(qxt = matrix(LCfut_mean[[i]]$hombres$qxt, ncol = nyears, nrow = nages)[,h],
                                x = 0, z = 0.5)
    hombres2 <- Life_preparancy(qxt = matrix(LCfut_mean[[i]]$hombres$qxt, ncol = nyears, nrow = nages)[,h],
                                x = 65, z = 0.25)
    mujeres1 <- Life_preparancy(qxt = matrix(LCfut_mean[[i]]$mujeres$qxt, ncol = nyears, nrow = nages)[,h],
                                x = 0, z = 0.5)
    mujeres2 <- Life_preparancy(qxt = matrix(LCfut_mean[[i]]$mujeres$qxt, ncol = nyears, nrow = nages)[,h],
                                x = 65, z = 0.25)
    prep.vid[h, 1] <- hombres1
    prep.vid[h, 2] <- mujeres1
    prep.vid[h, 3] <- hombres2
    prep.vid[h, 4] <- mujeres2
    #LOW
    hombres1.low <- Life_preparancy(qxt = matrix(LCfut_low[[i]]$hombres$qxt, ncol = nyears, nrow = nages)[,h],
                                    x = 0, z = 0.5)
    hombres2.low <- Life_preparancy(qxt = matrix(LCfut_low[[i]]$hombres$qxt, ncol = nyears, nrow = nages)[,h],
                                    x = 65, z = 0.25)
    mujeres1.low <- Life_preparancy(qxt = matrix(LCfut_low[[i]]$mujeres$qxt, ncol = nyears, nrow = nages)[,h],
                                    x = 0, z = 0.5)
    mujeres2.low <- Life_preparancy(qxt = matrix(LCfut_low[[i]]$mujeres$qxt, ncol = nyears, nrow = nages)[,h],
                                    x = 65, z = 0.25)
    prep.vid.low[h, 1] <- hombres1.low
    prep.vid.low[h, 2] <- mujeres1.low
    prep.vid.low[h, 3] <- hombres2.low
    prep.vid.low[h, 4] <- mujeres2.low
    #HIGH
    hombres1.high <- Life_preparancy(qxt = matrix(LCfut_high[[i]]$hombres$qxt, ncol = nyears, nrow = nages)[,h],
                                     x = 0, z = 0.5)
    hombres2.high <- Life_preparancy(qxt = matrix(LCfut_high[[i]]$hombres$qxt, ncol = nyears, nrow = nages)[,h],
                                     x = 65, z = 0.25)
    mujeres1.high <- Life_preparancy(qxt = matrix(LCfut_high[[i]]$mujeres$qxt, ncol = nyears, nrow = nages)[,h],
                                     x = 0, z = 0.5)
    mujeres2.high <- Life_preparancy(qxt = matrix(LCfut_high[[i]]$mujeres$qxt, ncol = nyears, nrow = nages)[,h],
                                     x = 65, z = 0.25)
    prep.vid.high[h, 1] <- hombres1.high
    prep.vid.high[h, 2] <- mujeres1.high
    prep.vid.high[h, 3] <- hombres2.high
    prep.vid.high[h, 4] <- mujeres2.high

  }
  Eprep.vida[[paste0(pob[i])]] <- prep.vid
  Eprep.vida.low[[paste0(pob[i])]] <- prep.vid.low
  Eprep.vida.high[[paste0(pob[i])]] <- prep.vid.high
}
capture.output(Eprep.vida, file = "EdadPrepVida.FM.txt")
capture.output(Eprep.vida.low, file = "EdadPrepVida.FM.low.txt")
capture.output(Eprep.vida.high, file = "EdadPrepVida.FM.high.txt")

we <- 1


esperanzas_FM.NU <- esperanzas.fm
edad_modal_FM.NU <- edad_modal_fm
gini.index_FM.NU <- gini.index_FM
pob.llegarvivo_FM.NU <- pob.llegarvivo_FM
con.std.des_FM.NU <- con.std.des_FM
Eprep.vida_FM.NU <- Eprep.vida

esperanzas_DM.NU <- esperanzas.dm
edad_modal_DM.NU <- edad_modal_DM
gini.index_DM.NU <- gini.index_DM
pob.llegarvivo_DM.NU <- pob.llegarvivo_DM
con.std.des_DM.NU <- con.std.des_DM
Eprep.vida_DM.NU <- Eprep.vida_DM

#Here we save the longevity indicators out-of-sample (FM)
#in a RData.
save(esperanzas_FM.NU, edad_modal_FM.NU, gini.index_FM.NU,
     pob.llegarvivo_FM.NU, con.std.des_FM.NU, Eprep.vida_FM.NU,
     file = "Indicadores_FM_NU.RData")

save(esperanzas_DM.NU, edad_modal_DM.NU, gini.index_DM.NU, pob.llegarvivo_DM.NU,
     con.std.des_DM.NU, Eprep.vida_DM.NU,
     file = "indicadores_DM_NU.RData")

save(paises, file= "datos_NU.194.RData")

length(pob)

