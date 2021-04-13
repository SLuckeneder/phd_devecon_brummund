
### REPLICATION

### inspiration from http://egap.org/methods-guides/10-things-you-need-know-about-spillovers
rm(list=ls())
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)




#### analysis starts here
###read in data

### load data from csv in character
dta <- read.csv("replication_data/raw_final_noID.csv", stringsAsFactors=FALSE)
names(dta)[names(dta) == 'hh_id'] <- 'hhid'

### corrections 2010302 and 2011309 say they cultivate rice but then provide zero for number of plots, we assume they meant to says they did not cultivate rice
dta$rice.plant_rice[dta$hhid %in% c("2010302","2011309") ] <- 2
sum(dta$rice.plant_rice == 1) ## 202 households have planted rice.
planters <- dta$hhid[dta$rice.plant_rice==1]

### this is only for reshaping, max number of plots used for rice farming is 3
plot1 <- dta[,c(10, 38:61, 86:91, 98:104)]
plot2 <- dta[,c(10, 62:85, 92:97,105:111)]
plot3 <- dta[,c(10, 112:141)]


names(plot1) <- c("hhid", substr(names(plot1),start=14, stop=110)[2:length(substr(names(plot1),start=14, stop=110))])
names(plot2) <- names(plot1)

plot3 <- plot3[c("hhid","rice.plot.3..bund_wdth","rice.plot.3..est_kg","rice.plot.3..use_plough","rice.plot.3..h2o_deth","rice.plot.3..leave_strw","rice.plot.3..keep_h2o","rice.plot.3..seed_source","rice.plot.3..NPK_use","rice.plot.3..Urea_use"
                 ,"rice.plot.3..sow_date",
                 "rice.plot.3..plot_name",
                 "rice.plot.3..leave_strw2",
                 "rice.plot.3..irri_sch_loc",
                 "rice.plot.3..seed_qty",
                 "rice.plot.3..enum_confirm",
                 "rice.plot.3..labour_hired",
                 "rice.plot.3..urea_qty",
                 "rice.plot.3..yield_paddy",
                 "rice.plot.3..nurse_use",
                 "rice.plot.3..bags_paddy",
                 "rice.plot.3..harvested_yet",
                 "rice.plot.3..bund_hgt",
                 "rice.plot.3..plot_area",
                 "rice.plot.3..total_kilogrames",
                 "rice.plot.3..plantin_rows",
                 "rice.plot.3..NPK_qty",
                 "rice.plot.3..depth_plant",
                 "rice.plot.3..seedlings_no",
                 "rice.plot.3..dist_hill",
                 "rice.plot.3..trans_days")]

plot3[,32:38] <- NA
names(plot3) <- names(plot1)
plot1$plotID <- 1
plot2$plotID <- 2
plot3$plotID <- 3

plots <- rbind(plot1, plot2, plot3) ### these are 241 *3 = 723 plots but many have missings

plots$use_plough[plots$use_plough == "a"] <- 1
plots$use_plough[plots$use_plough == "b"] <- 2
plots$use_plough[plots$use_plough == "c"] <- 3
plots$use_plough[plots$use_plough == "d"] <- 4
### convert data to numeric type
plots <- as.data.frame(sapply(plots, as.numeric))

plots$est_kg[plots$bags_paddy == 0] <- 120
plots$yield_paddy[plots$bags_paddy == 0] <- 0
plots$total_kilogrames[plots$bags_paddy == 0] <- 0

plots$yield[!is.na(plots$yield_expect)] <- plots$yield_expect[!is.na(plots$yield_expect)]
plots$yield[!is.na(plots$yield_paddy)] <- plots$yield_paddy[!is.na(plots$yield_paddy)]

plots$est_kg[plots$bags_paddy == 0] <- 120

## merge in treatments
treats <- read.csv("/home/bjvca/data/projects/PASIC/riceRCT/data/rice_ID.csv")

### make corrections to blocks based on field notes
## bloc 35 has Ctrl missing, 61 has an extra Ctrl that should have been matched to 24 - drop or just use this Ctrl to fill gap in 35?
treats$bloc[treats$hhid == "2122210" & treats$bloc == 61] <- 35

treats$bloc[treats$hhid == "2122202" & treats$bloc == 46] <- 16
treats$bloc[treats$hhid == "2122713" & treats$bloc == 16] <- 60

treats$bloc[treats$hhid == "2122213" & treats$bloc == 19] <- NA
treats$bloc[treats$hhid == "2122212" & treats$bloc == 29] <- NA

all <- merge(treats, plots, by.x="hhid", by.y="hhid")
### prepare network matrix based on gps location

all$id_info.HH._gps_latitude <- as.numeric(as.character(all$id_info.HH._gps_latitude))
all$id_info.HH._gps_longitude <- as.numeric(as.character( all$id_info.HH._gps_longitude))


for (village in names(table(all$VL))) {
  
  #village <- 30
  for (farmer in names(table(all$hhid[all$VL==village]))) {
    ### some data corrections: if lattitude is 2sds of average village latitude, put on average
    sel <- all$id_info.HH._gps_latitude[dta$VL==village] > mean(all$id_info.HH._gps_latitude[dta$VL==village], na.rm=T) + 2*sd(all$id_info.HH._gps_latitude[dta$VL==village], na.rm=T) | all$id_info.HH._gps_latitude[dta$VL==village] < mean(all$id_info.HH._gps_latitude[dta$VL==village], na.rm=T) - 2*sd(all$id_info.HH._gps_latitude[dta$VL==village], na.rm=T) 
    
    all$id_info.HH._gps_latitude[dta$VL==village][sel] <- mean(all$id_info.HH._gps_latitude[dta$VL==village][!sel], na.rm=T)
    
    sel <- all$id_info.HH._gps_longitude[dta$village==village] > mean(all$id_info.HH._gps_longitude[dta$village==village], na.rm=T) + 2*sd(all$id_info.HH._gps_longitude[dta$village==village], na.rm=T) | all$id_info.HH._gps_longitude[dta$village==village] < mean(all$id_info.HH._gps_longitude[dta$village==village], na.rm=T) - 2*sd(all$id_info.HH._gps_longitude[dta$village==village], na.rm=T) 
    
    all$id_info.HH._gps_longitude[dta$village==village][sel] <- mean(all$id_info.HH._gps_longitude[dta$village==village][!sel], na.rm=T)
  }
}

ag_gps <- aggregate(cbind( all$id_info.HH._gps_latitude, all$id_info.HH._gps_longitude),list(all$hhid), FUN=max)
names(ag_gps) <- c("hhid","lat","long")
####################################
### interlude
all$production[!is.na(all$total_kg_notharvested)]  <- all$total_kg_notharvested[!is.na(all$total_kg_notharvested)]
all$production[!is.na(all$total_kilogrames)]  <- all$total_kilogrames[!is.na(all$total_kilogrames)]
all$production[all$production == 0] <- NA
summary(lm(log(production)~tech,data=all))

### start with what is in pre-analysis plan
plots$plot_area  <- as.numeric(as.character(plots$plot_area))
plots$bags_paddy <- as.numeric(as.character(plots$bags_paddy))
plots$est_kg <- as.numeric(as.character(plots$est_kg))
# 1. total rice production
plots$total_kilogrames <- as.numeric(as.character(plots$total_kilogrames))




plots$total_kg_notharvested <- as.numeric(as.character(plots$total_kg_notharvested))

plots$prod[!is.na(plots$total_kg_notharvested)]  <- plots$total_kg_notharvested[!is.na(plots$total_kg_notharvested)]
plots$prod[!is.na(plots$total_kilogrames)]  <- plots$total_kilogrames[!is.na(plots$total_kilogrames)]

plots <- subset(plots, !is.na(prod))

hhprod <- aggregate(plots$prod, list(plots$hhid), sum, na.rm=T)
names(hhprod) <- c("hhid","tot_prod")
hh_prod <- merge(treats, hhprod)
img_hh_prod <- hh_prod

hh_prod$tot_prod[hh_prod$tot_prod == 0] <- NA
summary(lm(log(tot_prod)~tech,data=hh_prod))

dta2 <- merge(hh_prod,ag_gps, by.x="hhid", by.y="hhid")
dta2 <- subset(dta2, dta2$tot_prod>0 & !is.na(dta2$bloc))


mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(log(dta2$tot_prod)~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(log(dta2$tot_prod)~dta2$inder)


res_prod[1,1,1] <- summary(lm(log(tot_prod)~tech + (mat %*% as.numeric(dta2$tech)),data=dta2))$coefficients[2,1]
res_prod[2,1,1] <- summary(lm(log(tot_prod)~tech + (mat %*% as.numeric(dta2$tech)),data=dta2))$coefficients[2,2]
res_prod[3,1,1] <- summary(lm(log(tot_prod)~tech + (mat %*% as.numeric(dta2$tech)),data=dta2))$coefficients[2,4]

summary(lm(log(tot_prod)~retrn,data=hh_prod))
res_prod[1,1,2] <- summary(lm(log(tot_prod)~retrn + (mat %*% as.numeric(dta2$retrn)),data=dta2))$coefficients[2,1]
res_prod[2,1,2] <- summary(lm(log(tot_prod)~retrn + (mat %*% as.numeric(dta2$retrn)),data=dta2))$coefficients[2,2]
res_prod[3,1,2] <- summary(lm(log(tot_prod)~retrn + (mat %*% as.numeric(dta2$retrn)),data=dta2))$coefficients[2,4]
#RI(hh_prod$tot_prod,hh_prod$retrn,hh_prod$bloc, model=1, nr_repl = 10000)
#RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(.25,.35,.1), nr_repl = 1000)
#RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(-.25,-.15,.1), model=1,nr_repl = 1000)

res_prod[1,2,1] <- summary(lm(log(tot_prod)~tech + (mat %*% as.numeric(dta2$tech)),data=dta2))$coefficients[3,1]
res_prod[2,2,1] <- summary(lm(log(tot_prod)~tech + (mat %*% as.numeric(dta2$tech)),data=dta2))$coefficients[3,2]
res_prod[3,2,1] <- summary(lm(log(tot_prod)~tech + (mat %*% as.numeric(dta2$tech)),data=dta2))$coefficients[3,4]
#RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(.25,.35,.1),model=2, nr_repl = 1000)
#RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(-.25,-.15,.1), model=2,nr_repl = 1000)
summary(lm(log(tot_prod)~retrn+as.factor(bloc),data=hh_prod))
res_prod[1,2,2] <- summary(lm(log(tot_prod)~retrn + (mat %*% as.numeric(dta2$retrn)),data=dta2))$coefficients[3,1]
res_prod[2,2,2] <- summary(lm(log(tot_prod)~retrn + (mat %*% as.numeric(dta2$retrn)),data=dta2))$coefficients[3,2]
res_prod[3,2,2] <- summary(lm(log(tot_prod)~retrn + (mat %*% as.numeric(dta2$retrn)),data=dta2))$coefficients[3,4]
#RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(.25,.35,.1),model=2, nr_repl = 1000)
#RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(-.25,-.15,.1), model=2,nr_repl = 1000)


# 2. rice area planted
res_area <- array(NA,c(3,2,2))

hharea <- aggregate(plots$plot_area, list(plots$hhid), sum, na.rm=T)
names(hharea) <- c("hhid","rice_area")

hh_area <- merge(treats,hharea)
hh_area_m <-  hh_area
dta2 <- merge(hh_area,ag_gps, by.x="hhid", by.y="hhid")
dta2 <- subset(dta2, dta2$rice_area>0 & !is.na(dta2$bloc))



mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated


mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(log(dta2$rice_area)~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(log(dta2$rice_area)~dta2$inder)

summary(lm(log(rice_area)~tech,data=hh_area))
res_area[1,1,1] <- summary(lm(log(rice_area)~tech,data=hh_area))$coefficients[2,1]
res_area[2,1,1] <- summary(lm(log(rice_area)~tech,data=hh_area))$coefficients[2,2]
res_area[3,1,1] <- summary(lm(log(rice_area)~tech,data=hh_area))$coefficients[2,4]
#RI_CI(log(hh_area$rice_area),hh_area$tech,hh_area$bloc,c(.15,.25,.01), nr_repl = 1000)
#RI_CI(log(hh_area$rice_area,hh_area$tech,hh_area$bloc,c(-.25,-.15,.01), nr_repl = 1000)
summary(lm(log(rice_area)~retrn,data=hh_area))
res_area[1,1,2] <- summary(lm(log(rice_area)~retrn,data=hh_area))$coefficients[2,1]
res_area[2,1,2] <- summary(lm(log(rice_area)~retrn,data=hh_area))$coefficients[2,2]
res_area[3,1,2] <- summary(lm(log(rice_area)~retrn,data=hh_area))$coefficients[2,4]
#RI_CI(log(hh_area$rice_area),hh_area$retrn,hh_area$bloc,c(.15,.25,.01), nr_repl = 1000)
#RI_CI(log(hh_area$rice_area),hh_area$retrn,hh_area$bloc,c(-.25,-.15,.01), nr_repl = 1000)
summary(lm(log(rice_area)~tech+as.factor(bloc),data=hh_area))
res_area[1,2,1] <- summary(lm(log(rice_area)~tech+retrn+as.factor(bloc) ,data=hh_area))$coefficients[2,1]
res_area[2,2,1] <- summary(lm(log(rice_area)~tech+retrn+as.factor(bloc),data=hh_area))$coefficients[2,2]
res_area[3,2,1] <- summary(lm(log(rice_area)~tech+retrn+as.factor(bloc),data=hh_area))$coefficients[2,4]
#RI_CI(log(hh_area$rice_area),hh_area$tech,hh_area$bloc,c(.15,.25,.01), model=2,nr_repl = 1000)
#RI_CI(log(hh_area$rice_area),hh_area$tech,hh_area$bloc,c(-.09,-.07,.01), model=2, nr_repl = 1000)

summary(lm(log(rice_area)~retrn+as.factor(bloc),data=hh_area))
res_area[1,2,2] <- summary(lm(log(rice_area)~retrn+tech+as.factor(bloc),data=hh_area))$coefficients[2,1]
res_area[2,2,2] <- summary(lm(log(rice_area)~retrn+tech+as.factor(bloc),data=hh_area))$coefficients[2,2]
res_area[3,2,2] <- summary(lm(log(rice_area)~retrn+tech+as.factor(bloc),data=hh_area))$coefficients[2,4]
#RI_CI(log(hh_area$rice_area),hh_area$retrn,hh_area$bloc,c(.15,.25,.01), model=2,nr_repl = 1000)
#RI_CI(log(hh_area$rice_area),hh_area$retrn,hh_area$bloc,c(-.09,-.07,.01), model=2, nr_repl = 1000)


# 3. area as share of total cropping area
res_share <- array(NA,c(3,4,2))
rice_share <- merge(hh_area_m, dta[c("hhid","rice.tt_area")])

rice_share$share <- rice_share$rice_area/rice_share$rice.tt_area
rice_share$share[rice_share$share >= 1] <- 1


dta2 <- merge(rice_share,ag_gps, by.x="hhid", by.y="hhid")



mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(dta2$share~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(dta2$share~dta2$inder)

summary(lm(share~tech,data=rice_share))
res_share[1,1,1] <- summary(lm(share~tech,data=rice_share))$coefficients[2,1]
res_share[2,1,1] <- summary(lm(share~tech,data=rice_share))$coefficients[2,2]
res_share[3,1,1] <- summary(lm(share~tech,data=rice_share))$coefficients[2,4]
#RI_CI(rice_share$share,rice_share$tech,rice_share$bloc,c(.15,.25,.01), nr_repl = 1000)
#RI_CI(rice_share$share,rice_share$tech,rice_share$bloc,c(-.25,-.15,.01), nr_repl = 1000)
summary(lm(share~retrn,data=rice_share))
res_share[1,1,2] <- summary(lm(share~retrn,data=rice_share))$coefficients[2,1]
res_share[2,1,2] <- summary(lm(share~retrn,data=rice_share))$coefficients[2,2]
res_share[3,1,2] <- summary(lm(share~retrn,data=rice_share))$coefficients[2,4]
#RI_CI(rice_share$share,rice_share$retrn,rice_share$bloc,c(.15,.25,.01), nr_repl = 1000)
#RI_CI(rice_share$share,rice_share$retrn,rice_share$bloc,c(-.25,-.15,.01), nr_repl = 1000)

summary(lm(share~tech+as.factor(bloc),data=rice_share))
#RI(rice_share$share,rice_share$tech,rice_share$bloc, model=2, nr_repl = 10000)
res_share[1,2,1] <- summary(lm(share~tech+as.factor(bloc) ,data=rice_share))$coefficients[2,1]
res_share[2,2,1] <- summary(lm(share~tech+as.factor(bloc),data=rice_share))$coefficients[2,2]
res_share[3,2,1] <- summary(lm(share~tech+as.factor(bloc),data=rice_share))$coefficients[2,4]
#RI_CI(rice_share$share,rice_share$tech,rice_share$bloc,c(.15,.25,.01),model=2, nr_repl = 1000)
#RI_CI(rice_share$share,rice_share$tech,rice_share$bloc,c(-.25,-.15,.01), model=2, nr_repl = 1000)
summary(lm(share~retrn+as.factor(bloc),data=rice_share))
res_share[1,2,2] <- summary(lm(share~retrn+as.factor(bloc),data=rice_share))$coefficients[2,1]
res_share[2,2,2] <- summary(lm(share~retrn+as.factor(bloc),data=rice_share))$coefficients[2,2]
res_share[3,2,2] <- summary(lm(share~retrn+as.factor(bloc),data=rice_share))$coefficients[2,4]
#RI_CI(rice_share$share,rice_share$retrn,rice_share$bloc,c(.15,.25,.01),model=2, nr_repl = 1000)
#RI_CI(rice_share$share,rice_share$retrn,rice_share$bloc,c(-.25,-.15,.01), model=2, nr_repl = 1000)


# 4. rice yield
#calculate mean yields at the plot level
hh_prod <- img_hh_prod
hh_yield <- merge(hh_prod[c("hhid","tot_prod")],hh_area, by="hhid")
hh_yield$yield <- hh_yield$tot_prod / hh_yield$rice_area

#calculate mean yields at the household level
res <- array(NA,c(3,4,2))

hh_yield$yield[hh_yield$yield > 5000] <- NA 
hh_yield$yield[hh_yield$yield < 100] <- NA 



dta2 <- merge(hh_yield,ag_gps, by.x="hhid", by.y="hhid")
dta2 <- subset(dta2, dta2$yield>0 & !is.na(dta2$bloc))


mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(dta2$yield~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(dta2$yield~dta2$inder)



summary(lm(yield~tech,data=hh_yield))
res[1,1,1] <- summary(lm((yield)~tech,data=hh_yield))$coefficients[2,1]
res[2,1,1] <- summary(lm((yield)~tech,data=hh_yield))$coefficients[2,2]
res[3,1,1] <- summary(lm((yield)~tech,data=hh_yield))$coefficients[2,4]
#RI_CI(hh_yield$yield,hh_yield$tech,hh_yield$bloc,c(220,300,10), nr_repl = 1000)
#RI_CI(hh_yield$yield,hh_yield$tech,hh_yield$bloc,c(-300,-220,10), nr_repl = 1000)
summary(lm((yield)~retrn,data=hh_yield))
res[1,1,2] <- summary(lm((yield)~retrn,data=hh_yield))$coefficients[2,1]
res[2,1,2] <- summary(lm((yield)~retrn,data=hh_yield))$coefficients[2,2]
res[3,1,2] <- summary(lm((yield)~retrn,data=hh_yield))$coefficients[2,4]
#RI_CI(hh_yield$yield,hh_yield$retrn,hh_yield$bloc,c(220,300,10), nr_repl = 1000)
#RI_CI(hh_yield$yield,hh_yield$tech,hh_yield$bloc,c(-300,-220,10), nr_repl = 1000)
summary(lm((yield)~tech+as.factor(bloc),data=hh_yield))
res[1,2,1] <- summary(lm((yield)~tech+retrn+as.factor(bloc) ,data=hh_yield))$coefficients[2,1]
res[2,2,1] <- summary(lm((yield)~tech+retrn+as.factor(bloc),data=hh_yield))$coefficients[2,2]
res[3,2,1] <- summary(lm((yield)~tech+retrn+as.factor(bloc),data=hh_yield))$coefficients[2,4]
#RI_CI(hh_yield$yield,hh_yield$tech,hh_yield$bloc,c(220,300,10), model=2,nr_repl = 1000)
#RI_CI(hh_yield$yield,hh_yield$tech,hh_yield$bloc,c(-300,-220,10), model=2, nr_repl = 1000)
summary(lm((yield)~retrn+as.factor(bloc),data=hh_yield))
res[1,2,2] <- summary(lm((yield)~retrn+tech+as.factor(bloc),data=hh_yield))$coefficients[2,1]
res[2,2,2] <- summary(lm((yield)~retrn+tech+as.factor(bloc),data=hh_yield))$coefficients[2,2]
res[3,2,2] <- summary(lm((yield)~retrn+tech+as.factor(bloc),data=hh_yield))$coefficients[2,4]

### merge in average yields from the baseline
library(foreign)
dta4a <- merge(hh_yield["hhid"], read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC4A.dta")[c("hhid","PID","PLID","f1")], by="hhid" , all.x=T)
dta4a$f1[is.na(dta4a$f1)] <- 0
dta4a <- aggregate(dta4a$f1, list(dta4a$hhid), sum)
names(dta4a) <- c("hhid", "area")

dta7a <- merge(hh_yield["hhid"], read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC7A.dta")[ c("hhid", "PID", "PLID", "m2a", "m2b")], by = "hhid", all.x = T)

dta7a$m2a[is.na(dta7a$m2a)] <- 0
dta7a$m2b[is.na(dta7a$m2b)] <- 0
dta7a$prod <- dta7a$m2a + dta7a$m2b 

dta7a <- aggregate(dta7a$prod, list(dta7a$hhid), sum)
names(dta7a) <- c("hhid","prod")
bline <- merge(dta4a, dta7a)
bline <-  merge(bline, treats[c("hhid","bloc",  "tech", "retrn")])
bline$yield <- bline$prod/bline$area
bline$time <- 0
hh_yield$time <- 1

#bline$yield[bline$yield>5000] <- NA
#bline$yield[bline$yield<100] <- NA

#dif in dif
DID <- rbind(hh_yield[c("hhid","tech","retrn","time","yield","bloc")], bline[c("hhid","tech","retrn","time","yield", "bloc")])
boxplot(DID$yield)
DID$yield[DID$yield < 100] <- NA
DID$yield[DID$yield > 5000] <- NA

summary(lm((yield)~tech*time + retrn ,data=DID))
res[1,3,1] <- summary(lm((yield)~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm((yield)~tech*time +retrn ,data=DID))$coefficients)[1],1]
res[2,3,1] <- summary(lm((yield)~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm((yield)~tech*time +retrn ,data=DID))$coefficients)[1],2]
res[3,3,1] <- summary(lm((yield)~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm((yield)~tech*time +retrn ,data=DID))$coefficients)[1],4]
summary((lm( log(yield)~retrn*time + tech+ as.factor(bloc), data=DID   )))
res[1,3,2] <- summary(lm((yield)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm((yield)~retrn*time +tech ,data=DID))$coefficients)[1],1]
res[2,3,2] <- summary(lm((yield)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm((yield)~retrn*time +tech  ,data=DID))$coefficients)[1],2]
res[3,3,2] <- summary(lm((yield)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm((yield)~retrn*time +tech  ,data=DID))$coefficients)[1],4]

### baseline balance for yield

basebal[1,1] <- mean(DID$yield[DID$time==0], na.rm=T)
basebal[2,1] <- sd(DID$yield[DID$time==0], na.rm=T)
basebal[1,2] <- coefficients(lm((yield)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[2,2] <- coef(summary(lm((yield)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[1,3] <- coefficients(lm((yield)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[2,3] <- coef(summary(lm((yield)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]



#and include lagged dependent
lagdep <- merge(hh_yield[c("hhid","tech","retrn","time","yield","bloc")], bline[c("hhid","yield")], by="hhid" )
#lagdep <- cbind(hh_yield[c("hhid","tech","retrn","time","yield","bloc")], bline[c("yield")])
names(lagdep) <- c("hhid","tech","retrn","time","yield","bloc","l_yield")
lagdep$l_yield[lagdep$l_yield < 100] <- NA
lagdep$l_yield[lagdep$l_yield > 5000] <- NA
summary((lm( (yield)~tech+l_yield + as.factor(bloc), data=lagdep)))
res[1,4,1] <- summary(lm( (yield)~tech+ retrn+l_yield + as.factor(bloc), data=lagdep))$coefficients[ 2,1]
res[2,4,1] <- summary(lm( (yield)~tech+ retrn+l_yield + as.factor(bloc), data=lagdep))$coefficients[ 2,2]
res[3,4,1] <- summary(lm( (yield)~tech+ retrn+l_yield + as.factor(bloc), data=lagdep))$coefficients[ 2,4]


df_compl <-  data.frame(lagdep$yield, lagdep$tech, lagdep$bloc,lagdep$l_yield)
df_compl <- subset(df_compl,complete.cases(df_compl))
###search for confidence bounds
#RI_CI(df_compl$lagdep.yield,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(120,150,1), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_yield)

summary((lm( (yield)~ retrn +l_yield + as.factor(bloc), data=lagdep)))
res[1,4,2] <- summary(lm( (yield)~ retrn+ tech +l_yield + as.factor(bloc), data=lagdep))$coefficients[ 2,1]
res[2,4,2] <- summary(lm( (yield)~ retrn+ tech +l_yield + as.factor(bloc), data=lagdep))$coefficients[ 2,2]
res[3,4,2] <- summary(lm( (yield)~ retrn+ tech +l_yield + as.factor(bloc), data=lagdep))$coefficients[ 2,4]
df_compl <-  data.frame(lagdep$yield, lagdep$retrn, lagdep$bloc,lagdep$l_yield)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.yield,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(-380,-360,1), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_yield)
# merge in education level from midline
DID_soc <- merge(DID, read.csv("/home/bjvca/data/projects/PASIC/riceRCT/endline/data/socnet.csv"), by="hhid", all.x=T)
DID_educ <- merge(DID,dta[c("hhid","rice.resp_educ")], by="hhid")



### area in baseline 
hh_area$time <- 1
names(hh_area)[names(hh_area) == "rice_area"] <- "area"
DID <- rbind(hh_area[c("hhid","tech","retrn","time","area","bloc")], bline[c("hhid","tech","retrn","time","area", "bloc")])
DID$area[DID$area == 0] <- NA
summary((lm( log(area)~tech*time, data=DID   )))


res_area[1,3,1] <- summary(lm(log(area)~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm(log(area)~tech*time +retrn ,data=DID))$coefficients)[1],1]
res_area[2,3,1] <- summary(lm(log(area)~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm(log(area)~tech*time +retrn ,data=DID))$coefficients)[1],2]
res_area[3,3,1] <- summary(lm(log(area)~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm(log(area)~tech*time +retrn ,data=DID))$coefficients)[1],4]

res_area[1,3,2] <- summary(lm(log(area)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(log(area)~tech*time +retrn ,data=DID))$coefficients)[1],1]
res_area[2,3,2] <- summary(lm(log(area)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(log(area)~tech*time +retrn ,data=DID))$coefficients)[1],2]
res_area[3,3,2] <- summary(lm(log(area)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(log(area)~tech*time +retrn ,data=DID))$coefficients)[1],4]

### baseline balance for yield

basebal[3,1] <- mean(log(DID$area[DID$time==0]), na.rm=T)
basebal[4,1] <- sd(log(DID$area[DID$time==0]), na.rm=T)
basebal[3,2] <- coefficients(lm(log(area)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[4,2] <- coef(summary(lm(log(area)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[3,3] <- coefficients(lm(log(area)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[4,3] <- coef(summary(lm(log(area)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]

#and include lagged dependent (area)
lagdep <-  merge(hh_yield[c("hhid","tech","retrn","time","area","bloc")], bline[c("hhid","area")], by="hhid" )
names(lagdep) <- c("hhid","tech","retrn","time","area","bloc","l_area")
lagdep$l_area[lagdep$l_area==0] <- NA
summary(lm( log(area)~tech+log(l_area) + as.factor(bloc), data=lagdep))
df_compl <-  data.frame(log(lagdep$area), lagdep$tech, lagdep$bloc,log(lagdep$l_area))
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$log.lagdep.area.,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.4,-.2,.01), model=3,nr_repl = 1000,ldep=df_compl$log.lagdep.l_area)

res_area[1,4,1] <- summary(lm( log(area)~tech+ retrn+log(l_area) + as.factor(bloc), data=lagdep))$coefficients[2,1]
res_area[2,4,1] <- summary(lm( log(area)~tech+ retrn+log(l_area) + as.factor(bloc), data=lagdep))$coefficients[2,2]
res_area[3,4,1] <- summary(lm( log(area)~tech+ retrn+log(l_area) + as.factor(bloc), data=lagdep))$coefficients[2,4]


res_area[1,4,2] <- summary(lm( log(area)~ retrn+ tech+log(l_area) + as.factor(bloc), data=lagdep))$coefficients[2,1]
res_area[2,4,2] <- summary(lm( log(area)~ retrn+ tech+log(l_area) + as.factor(bloc), data=lagdep))$coefficients[2,2]
res_area[3,4,2] <- summary(lm( log(area)~ retrn+ tech+log(l_area) + as.factor(bloc), data=lagdep))$coefficients[2,4]
summary(lm( log(area)~retrn+log(l_area) + as.factor(bloc), data=lagdep))
df_compl <-  data.frame(log(lagdep$area), lagdep$retrn, lagdep$bloc,log(lagdep$l_area))
df_compl <- subset(df_compl,complete.cases(df_compl))
# RI_CI(df_compl$log.lagdep.area.,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.2,.3,.01), model=3,nr_repl = 1000,ldep=df_compl$log.lagdep.l_area)


# merge in education level from midline
DID_educ <- merge(DID,dta[c("hhid","rice.resp_educ")], by="hhid")

### prod in baseline
hh_prod$time <- 1
names(hh_prod)[names(hh_prod) == "tot_prod"] <- "prod"
DID <- rbind(hh_prod[c("hhid","tech","retrn","time","prod","bloc")], bline[c("hhid","tech","retrn","time","prod", "bloc")])

DID$prod[DID$prod == 0] <- NA

#and include blocs
summary((lm( log(prod)~tech*time + as.factor(bloc), data=DID   )))
res_prod[1,3,1] <- summary(lm(log(prod)~tech*time +retrn ,data=DID))$coefficients[ dim( summary(lm(log(prod)~tech*time +retrn ,data=DID))$coefficients)[1],1]
res_prod[2,3,1] <- summary(lm(log(prod)~tech*time +retrn,data=DID))$coefficients[ dim( summary(lm(log(prod)~tech*time +retrn ,data=DID))$coefficients)[1],2]
res_prod[3,3,1] <- summary(lm(log(prod)~tech*time +retrn,data=DID))$coefficients[ dim( summary(lm(log(prod)~tech*time +retrn ,data=DID))$coefficients)[1],4]
summary((lm( log(prod)~retrn*time + as.factor(bloc), data=DID   )))
res_prod[1,3,2] <- summary(lm(log(prod)~retrn*time +tech ,data=DID))$coefficients[ dim( summary(lm(log(prod)~retrn*time +tech  ,data=DID))$coefficients)[1],1]
res_prod[2,3,2] <- summary(lm(log(prod)~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(log(prod)~retrn*time +tech ,data=DID))$coefficients)[1],2]
res_prod[3,3,2] <- summary(lm(log(prod)~retrn*time +tech ,data=DID))$coefficients[ dim( summary(lm(log(prod)~retrn*time +tech ,data=DID))$coefficients)[1],4]


basebal[5,1] <- mean(log(DID$prod[DID$time==0]), na.rm=T)
basebal[6,1] <- sd(log(DID$prod[DID$time==0]), na.rm=T)
basebal[5,2] <- coefficients(lm(log(prod)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[6,2] <- coef(summary(lm(log(prod)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[5,3] <- coefficients(lm(log(prod)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[6,3] <- coef(summary(lm(log(prod)~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]


### control for lagged production
lagdep <- merge(hh_prod[c("hhid","tech","retrn","time","prod","bloc")], bline[c("hhid","prod")], by="hhid" )
names(lagdep) <- c("hhid","tech","retrn","time","prod","bloc","l_prod")
lagdep$l_prod[lagdep$l_prod==0] <- NA
lagdep$prod[lagdep$prod==0] <- NA

summary(lm(log(prod)~tech+ log(l_prod) + as.factor(bloc),data=lagdep))
res_prod[1,4,1] <- summary(lm(log(prod)~tech + retrn+ log(l_prod) + as.factor(bloc),data=lagdep))$coefficients[2,1]
res_prod[2,4,1] <- summary(lm(log(prod)~tech + retrn+ log(l_prod) + as.factor(bloc),data=lagdep))$coefficients[2,2]
res_prod[3,4,1] <- summary(lm(log(prod)~tech + retrn+ log(l_prod) + as.factor(bloc),data=lagdep))$coefficients[2,4]
df_compl <-  data.frame(log(lagdep$prod), lagdep$tech, lagdep$bloc,log(lagdep$l_prod))
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$log.lagdep.prod.,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$log.lagdep.l_prod.)

summary(lm(log(prod)~retrn+ log(l_prod) + as.factor(bloc),data=lagdep))
res_prod[1,4,2] <- summary(lm(log(prod)~tech + retrn+ log(l_prod) + as.factor(bloc),data=lagdep))$coefficients[3,1]
res_prod[2,4,2] <- summary(lm(log(prod)~tech + retrn+ log(l_prod) + as.factor(bloc),data=lagdep))$coefficients[3,2]
res_prod[3,4,2] <- summary(lm(log(prod)~tech + retrn+ log(l_prod) + as.factor(bloc),data=lagdep))$coefficients[3,4]
df_compl <-  data.frame(log(lagdep$prod), lagdep$retrn, lagdep$bloc,log(lagdep$l_prod))
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$log.lagdep.prod.,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$log.lagdep.l_prod.)

# merge in education level from midline
DID_educ <- merge(DID,dta[c("hhid","rice.resp_educ")], by="hhid")

### merge in total land area for rice share in baseline, this is in section 2
dta2 <- read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC2.dta")[c("hhid","ll1")]
dta2 <- aggregate(dta2$ll1 , list(dta2$hhid), sum, na.rm=T)
names(dta2) <- c("hhid","tot_area")
bline <- merge(bline,dta2)
bline$share <- bline$area/bline$tot_area
rice_share$time <- 1
DID <- rbind(rice_share[c("hhid","tech","retrn","time","share","bloc")], bline[c("hhid","tech","retrn","time","share", "bloc")])

#and include blocs
summary(lm(share~tech*time +retrn ,data=DID))
res_share[1,3,1] <- summary(lm(share~tech*time +retrn ,data=DID))$coefficients[ dim( summary(lm(share~tech*time +retrn ,data=DID))$coefficients)[1],1]
res_share[2,3,1] <- summary(lm(share~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm(share~tech*time +retrn ,data=DID))$coefficients)[1],2]
res_share[3,3,1] <- summary(lm(share~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm(share~tech*time +retrn ,data=DID))$coefficients)[1],4]
summary(lm(share~retrn*time + tech ,data=DID))
res_share[1,3,2] <- summary(lm(share~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(share~retrn*time + tech ,data=DID))$coefficients)[1],1]
res_share[2,3,2] <- summary(lm(share~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(share~retrn*time + tech ,data=DID))$coefficients)[1],2]
res_share[3,3,2] <- summary(lm(share~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(share~retrn*time + tech ,data=DID))$coefficients)[1],4]

basebal[7,1] <- mean(DID$share[DID$time==0], na.rm=T)
basebal[8,1] <- sd(DID$share[DID$time==0], na.rm=T)
basebal[7,2] <- coefficients(lm(share~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[8,2] <- coef(summary(lm(share~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[7,3] <- coefficients(lm(share~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[8,3] <- coef(summary(lm(share~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]

## include lagged rice share
lagdep <- merge(rice_share[c("hhid","tech","retrn","time","share","bloc")], bline[c("hhid","share")], by="hhid")
names(lagdep) <- c("hhid","tech","retrn","time","share","bloc","l_share")
lagdep$l_share[lagdep$l_share > 1] <- 1
lagdep$share[lagdep$share > 1] <- 1


summary(lm(share~tech + l_share + as.factor(bloc),data=lagdep))
#RI(lagdep$share, lagdep$tech, lagdep$bloc,model=3,ldep=lagdep$l_share)
res_share[1,4,1] <- summary(lm(share~tech+retrn+l_share+as.factor(bloc),data=lagdep))$coefficients[2,1]
res_share[2,4,1] <- summary(lm(share~tech+retrn+l_share+as.factor(bloc),data=lagdep))$coefficients[2,2]
res_share[3,4,1] <- summary(lm(share~tech+retrn+l_share+as.factor(bloc),data=lagdep))$coefficients[2,4]
df_compl <-  data.frame(lagdep$share, lagdep$tech, lagdep$bloc,lagdep$l_share)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.share,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_share)


df_compl <-  data.frame(lagdep$share, lagdep$retrn, lagdep$bloc,lagdep$l_share)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.share,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_share)

summary((lm( share~retrn*time, data=DID   )))
res_share[1,4,2] <- summary(lm(share~tech+retrn+l_share+as.factor(bloc),data=lagdep))$coefficients[3,1]
res_share[2,4,2] <- summary(lm(share~tech+retrn+l_share+as.factor(bloc),data=lagdep))$coefficients[3,2]
res_share[3,4,2] <- summary(lm(share~tech+retrn+l_share+as.factor(bloc),data=lagdep))$coefficients[3,4]


################################### practices ######################################
####################################################################################

#do treated households use more fertilizer: dummy is one if hh uses fert (NPK or DAP) on at least one plot
res_fert <- array(NA,c(3,4,2))

plots$NPK_use <- as.numeric(as.character(plots$NPK_use))
plots$Urea_use <- as.numeric(as.character(plots$Urea_use))

plots$fert_use <- 0
plots$fert_use[plots$NPK_use == 1 | plots$Urea_use == 1] <- 1
fert_hh <- aggregate(plots$fert_use, list(plots$hhid),max)
names(fert_hh) <- c("hhid","fert_use")
fert_hh <- merge(treats,fert_hh)
#only for planters
fert_hh <- subset(fert_hh, hhid %in% planters)
fert_hh <- merge(fert_hh, select[c("hhid")], by="hhid")


dta2 <- merge(fert_hh,ag_gps, by.x="hhid", by.y="hhid")



mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(dta2$fert_use~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(dta2$fert_use~dta2$inder)



summary(lm(fert_use~tech,data=fert_hh))
res_fert[1,1,1] <- summary(lm(fert_use~tech,data=fert_hh))$coefficients[2,1]
res_fert[2,1,1] <- summary(lm(fert_use~tech,data=fert_hh))$coefficients[2,2]
res_fert[3,1,1] <- summary(lm(fert_use~tech,data=fert_hh))$coefficients[2,4]
#RI_CI(fert_hh$fert_use,fert_hh$tech,fert_hh$bloc,c(.1,.11,.01), model=1,nr_repl = 1000)

summary(lm(fert_use~retrn,data=fert_hh))
res_fert[1,1,2] <- summary(lm(fert_use~retrn,data=fert_hh))$coefficients[2,1]
res_fert[2,1,2] <- summary(lm(fert_use~retrn,data=fert_hh))$coefficients[2,2]
res_fert[3,1,2] <- summary(lm(fert_use~retrn,data=fert_hh))$coefficients[2,4]
#RI_CI(fert_hh$fert_use,fert_hh$retrn,fert_hh$bloc,c(.1,.11,.01), model=1,nr_repl = 1000)
summary(lm(fert_use~tech+as.factor(bloc),data=fert_hh))
res_fert[1,2,1] <- summary(lm(fert_use~tech+retrn+as.factor(bloc) ,data=fert_hh))$coefficients[2,1]
res_fert[2,2,1] <- summary(lm(fert_use~tech+retrn+as.factor(bloc),data=fert_hh))$coefficients[2,2]
res_fert[3,2,1] <- summary(lm(fert_use~tech+retrn+as.factor(bloc),data=fert_hh))$coefficients[2,4]
#RI_CI(fert_hh$fert_use,fert_hh$retrn,fert_hh$bloc,c(0.10,0.12,.01), model=2,nr_repl = 1000)

res_fert[1,2,2] <- summary(lm(fert_use~tech+retrn+as.factor(bloc) ,data=fert_hh))$coefficients[3,1]
res_fert[2,2,2] <- summary(lm(fert_use~tech+retrn+as.factor(bloc),data=fert_hh))$coefficients[3,2]
res_fert[3,2,2] <- summary(lm(fert_use~tech+retrn+as.factor(bloc),data=fert_hh))$coefficients[3,4]


### now merge in baseline data
bline <- read.dta("/home/bjvca/data/projects/PASIC/data/rice/cleaned/RSEC0.dta")
bline$fert_use <- bline$hh12==1
bline <- bline[c("hhid","fert_use")]

names(bline) <- c("hhid","fert_use")
bline <- merge(bline, treats)
bline$time <- 0
fert_hh$time <- 1
DID <- rbind(fert_hh[c("hhid","tech","retrn","time","fert_use","bloc")], bline[c("hhid","tech","retrn","time","fert_use", "bloc")])


res_fert[1:3,3,1] <- summary(lm(fert_use~tech*time +retrn ,data=DID))$coefficients[ dim( summary(lm(fert_use~tech*time +retrn  ,data=DID))$coefficients)[1],c(1,2,4)]
summary((lm( fert_use~retrn*time + as.factor(bloc), data=DID   )))
res_fert[1:3,3,2] <- summary(lm(fert_use~retrn*time +tech ,data=DID))$coefficients[ dim( summary(lm(fert_use~retrn*time  + tech ,data=DID))$coefficients)[1],c(1,2,4)]
basebal[9,1] <- mean(DID$fert_use[DID$time==0], na.rm=T)
basebal[10,1] <- sd(DID$fert_use[DID$time==0], na.rm=T)
basebal[9,2] <- coefficients(lm(fert_use~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[10,2] <- coef(summary(lm(fert_use~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[9,3] <- coefficients(lm(fert_use~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[10,3] <- coef(summary(lm(fert_use~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]

motivate[1,1] <- mean(DID$fert_use[DID$time==0 & DID$tech==FALSE], na.rm=T)
motivate[2,1] <- sd(DID$fert_use[DID$time==0 & DID$tech==FALSE], na.rm=T)

motivate[1,2] <- mean(DID$fert_use[DID$time==1 & DID$tech==FALSE], na.rm=T)
motivate[2,2] <- sd(DID$fert_use[DID$time==1 & DID$tech==FALSE], na.rm=T)

motivate[1,3] <- t.test(DID$fert_use[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$statistic
motivate[2,3] <- t.test(DID$fert_use[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$p.value

motivate[1,4] <- mean(DID$fert_use[DID$time==0 & DID$retrn==FALSE], na.rm=T)
motivate[2,4] <- sd(DID$fert_use[DID$time==0 & DID$retrn==FALSE], na.rm=T)

motivate[1,5] <- mean(DID$fert_use[DID$time==1 & DID$retrn==FALSE], na.rm=T)
motivate[2,5] <- sd(DID$fert_use[DID$time==1 & DID$retrn==FALSE], na.rm=T)

motivate[1,6] <- t.test(DID$fert_use[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$statistic
motivate[2,6] <- t.test(DID$fert_use[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$p.value




## include lagged fert use
lagdep <- merge(fert_hh[c("hhid","tech","retrn","time","fert_use","bloc")], bline[c("hhid","fert_use")], by="hhid")
names(lagdep) <- c("hhid","tech","retrn","time","fert_use","bloc","l_fert_use")
res_fert[1:3,4,1] <- summary(lm(fert_use~tech+retrn+l_fert_use+as.factor(bloc) ,data=lagdep))$coefficients[2,c(1,2,4)]
res_fert[1:3,4,2] <- summary(lm(fert_use~tech+retrn+l_fert_use+as.factor(bloc) ,data=lagdep))$coefficients[3,c(1,2,4)]

summary(lm(fert_use~tech+l_fert_use+as.factor(bloc) ,data=lagdep))$coefficients
df_compl <-  data.frame(lagdep$fert_use, lagdep$tech, lagdep$bloc,lagdep$l_fert_use)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.fert_use,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_fert_use)
summary(lm(fert_use~retrn+l_fert_use+as.factor(bloc) ,data=lagdep))$coefficients
df_compl <-  data.frame(lagdep$fert_use, lagdep$retrn, lagdep$bloc,lagdep$l_fert_use)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.fert_use,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.14,.15,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_fert_use)

### transplant after 14 days
res_trans <- array(NA,c(3,4,2))
hh_trans <- aggregate(as.numeric(as.character(plots$trans_days)), list(plots$hhid), min, na.rm=T)
names(hh_trans) <- c("hhid","days")
hh_trans$days_d <- NA
hh_trans$days_d[(hh_trans$days > 14) & (hh_trans$days < Inf)] <- 0
hh_trans$days_d[hh_trans$days <= 14] <- 1
hh_trans <- merge(treats, hh_trans)


dta2 <- merge(fert_hh,ag_gps, by.x="hhid", by.y="hhid")



mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0



summary(lm(days_d~tech,data=hh_trans))
res_trans[1:3,1,1] <- summary(lm(days_d~tech,data=hh_trans))$coefficients[2,c(1,2,4)]
summary(lm(days_d~retrn,data=hh_trans))
res_trans[1:3,1,2] <- summary(lm(days_d~retrn,data=hh_trans))$coefficients[2,c(1,2,4)]

summary(lm(days_d~tech+as.factor(bloc),data=hh_trans))
res_trans[1:3,2,1] <- summary(lm(days_d~tech+as.factor(bloc) ,data=hh_trans))$coefficients[2,c(1,2,4)]

summary(lm(days_d~retrn+as.factor(bloc),data=hh_trans))
res_trans[1:3,2,2] <- summary(lm(days_d~retrn+as.factor(bloc),data=hh_trans))$coefficients[2,c(1,2,4)]

### merge in baseline data

sec3f <- read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC3F.dta")
sec3f <- subset(sec3f,kk1 == "Transplant seedlings 14 days after sowing in the nursery bed")
sec3f$days <- sec3f$kk2 == "Yes, recommended"
bline <- merge(sec3f[c("hhid","days")], treats)
bline$days_d <- as.numeric(bline$days)
bline$time <- 0
hh_trans$time <- 1

DID <- rbind(hh_trans[c("hhid","tech","retrn","time","days_d","bloc")], bline[c("hhid","tech","retrn","time","days_d", "bloc")])


res_trans[1:3,3,1] <- summary(lm(days_d~tech*time + retrn ,data=DID))$coefficients[ dim( summary(lm(days_d~tech*time + retrn ,data=DID))$coefficients)[1],c(1,2,4)]
summary((lm( days_d~retrn*time+ tech + as.factor(bloc), data=DID   )))
res_trans[1:3,3,2] <- summary(lm(days_d~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(days_d~retrn*time + tech ,data=DID))$coefficients)[1],c(1,2,4)]


## include lagged rice share
lagdep <- merge(hh_trans[c("hhid","tech","retrn","time","days_d","bloc")], bline[c("hhid","days_d")], by="hhid")
names(lagdep) <- c("hhid","tech","retrn","time","days_d","bloc","l_days_d")
res_trans[1:3,4,1] <- summary(lm(days_d~tech+retrn+l_days_d+as.factor(bloc) ,data=lagdep))$coefficients[2,c(1,2,4)]
res_trans[1:3,4,2] <- summary(lm(days_d~tech+retrn+l_days_d+as.factor(bloc) ,data=lagdep))$coefficients[3,c(1,2,4)]

### row planting
res_row <- array(NA,c(3,4,2))

#plant in rows -  not significant
plot_treat <- merge(plots, treats, by="hhid")
plot_treat$plantin_rows[is.na(plot_treat$plantin_rows)] <- 2
plot_treat$plantin_rows <- as.numeric(as.character(plot_treat$plantin_rows))

rowplant <- aggregate((as.numeric(as.character(plot_treat$plantin_rows)) == 1), list(plot_treat$hhid), max, na.rm=T)
names(rowplant) <- c("hhid","rowplant")
rowplant <- subset(rowplant, hhid %in% planters)
rowplant <- merge(treats,rowplant)


dta2 <- merge(rowplant,ag_gps, by.x="hhid", by.y="hhid")



mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(dta2$rowplant~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(dta2$rowplant~dta2$inder)





summary(lm(rowplant~tech, data=rowplant))
#RI_CI(rowplant$rowplant,rowplat$tech,rowplant$bloc,c(.1,.11,.01), model=1,nr_repl = 1000)
res_row[1:3,1,1] <-  summary(lm(rowplant~tech,data=rowplant))$coefficients[2,c(1,2,4)]
res_row[1:3,1,2] <-  summary(lm(rowplant~retrn,data=rowplant))$coefficients[2,c(1,2,4)]
summary(lm(rowplant~tech+as.factor(bloc),data=rowplant))
res_row[1:3,2,1] <- summary(lm(rowplant~tech+retrn+as.factor(bloc) ,data=rowplant))$coefficients[2,c(1,2,4)]

summary(lm(rowplant~retrn,data=rowplant))
#RI_CI(rowplant$rowplant,rowplat$retrn,rowplant$bloc,c(.01,0,.01), model=1,nr_repl = 1000)
res_row[1:3,2,2] <- summary(lm(rowplant~retrn+tech+as.factor(bloc),data=rowplant))$coefficients[2,c(1,2,4)]

summary(lm(rowplant~tech+ as.factor(bloc),data=rowplant))

# RI_CI(rowplant$rowplant,rowplant$tech,rowplant$bloc,c(-.22,-.20,.01), model=2,nr_repl = 1000)
summary(lm(rowplant~retrn+ as.factor(bloc),data=rowplant))

# RI_CI(rowplant$rowplant,rowplant$retrn,rowplant$bloc,c(-.22,-.20,.01), model=2,nr_repl = 1000)


###merge in baseline
sec3f <- read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC3F.dta")
sec3f <- subset(sec3f,kk1 == "Planting should be done in rows")
sec3f$rowplant <- sec3f$kk2 == "Yes, recommended"
bline <- merge(sec3f[c("hhid","rowplant")], treats)
bline$row <- as.numeric(bline$row)
bline$time <- 0
rowplant$time <- 1


DID <- rbind(rowplant[c("hhid","tech","retrn","time","rowplant","bloc")], bline[c("hhid","tech","retrn","time","rowplant", "bloc")])

res_row[1:3,3,1] <- summary(lm(rowplant~tech*time +retrn ,data=DID))$coefficients[ dim( summary(lm(rowplant~tech*time +retrn ,data=DID))$coefficients)[1],c(1,2,4)]
res_row[1:3,3,2] <- summary(lm(rowplant~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(rowplant~retrn*time +tech ,data=DID))$coefficients)[1],c(1,2,4)]

basebal[11,1] <- mean(DID$rowplant[DID$time==0], na.rm=T)
basebal[12,1] <- sd(DID$rowplant[DID$time==0], na.rm=T)
basebal[11,2] <- coefficients(lm(rowplant~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[12,2] <- coef(summary(lm(rowplant~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[11,3] <- coefficients(lm(rowplant~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[12,3] <- coef(summary(lm(rowplant~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]

motivate[3,1] <- mean(DID$rowplant[DID$time==0 & DID$tech==FALSE], na.rm=T)
motivate[4,1] <- sd(DID$rowplant[DID$time==0 & DID$tech==FALSE], na.rm=T)

motivate[3,2] <- mean(DID$rowplant[DID$time==1 & DID$tech==FALSE], na.rm=T)
motivate[4,2] <- sd(DID$rowplant[DID$time==1 & DID$tech==FALSE], na.rm=T)

motivate[3,3] <- t.test(DID$rowplant[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$statistic
motivate[4,3] <- t.test(DID$rowplant[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$p.value

motivate[3,4] <- mean(DID$rowplant[DID$time==0 & DID$retrn==FALSE], na.rm=T)
motivate[4,4] <- sd(DID$rowplant[DID$time==0 & DID$retrn==FALSE], na.rm=T)

motivate[3,5] <- mean(DID$rowplant[DID$time==1 & DID$retrn==FALSE], na.rm=T)
motivate[4,5] <- sd(DID$rowplant[DID$time==1 & DID$retrn==FALSE], na.rm=T)

motivate[3,6] <- t.test(DID$rowplant[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$statistic
motivate[4,6] <- t.test(DID$rowplant[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$p.value

## include lagged rice share
lagdep <- merge(rowplant[c("hhid","tech","retrn","time","rowplant","bloc")], bline[c("hhid","rowplant")], by="hhid")
names(lagdep) <- c("hhid","tech","retrn","time","rowplant","bloc","l_rowplant")
res_row[1:3,4,1] <- summary(lm(rowplant~tech+retrn+l_rowplant+as.factor(bloc) ,data=lagdep))$coefficients[2,c(1,2,4)]
res_row[1:3,4,2] <- summary(lm(rowplant~tech+retrn+l_rowplant+as.factor(bloc) ,data=lagdep))$coefficients[3,c(1,2,4)]

summary(lm(rowplant~tech+l_rowplant+as.factor(bloc) ,data=lagdep))
df_compl <-  data.frame(lagdep$rowplant, lagdep$tech, lagdep$bloc,lagdep$l_rowplant)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.rowplant,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_rowplant)
summary(lm(rowplant~retrn+l_rowplant+as.factor(bloc) ,data=lagdep))$coefficients
df_compl <-  data.frame(lagdep$rowplant, lagdep$retrn, lagdep$bloc,lagdep$l_rowplant)
df_compl <- subset(df_compl,complete.cases(df_compl))
#RI_CI(df_compl$lagdep.rowplant,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.14,.15,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_rowplant)
#RI_CI(df_compl$lagdep.rowplant,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.02,.04,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_rowplant)

### water management was also an important recommended practice that featured in the video
res_h2o <- array(NA,c(3,4,2))
hh_h2o <- aggregate((as.numeric(as.character(plots$keep_h2o)) == 1), list(plots$hhid), max, na.rm=T)
names(hh_h2o) <- c("hhid","h2o")
hh_h2o <- subset(hh_h2o, hhid %in% planters)
hh_h2o <- merge(treats,hh_h2o)
hh_h2o <- subset(hh_h2o, h2o>=0)

dta2 <- merge(hh_h2o,ag_gps, by.x="hhid", by.y="hhid")



mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
diag(mat) <- NA
### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
mat[mat>.0225] <- NA
mat <- (.0225 - mat)/.0225

### keep only distances for treated
dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
mat[!is.na(mat)] <- 1
mat[is.na(mat)] <- 0

dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1

t.test(dta2$h2o~dta2$inder)
dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1

t.test(dta2$h2o~dta2$inder)


summary(lm(h2o~tech, data=hh_h2o))
#RI_CI(hh_h2o$h2o,hh_h2o$tech,hh_h2o$bloc,c(.1,.11,.01), model=1,nr_repl = 1000)
res_h2o[1:3,1,1] <- summary(lm(h2o~tech,data=hh_h2o))$coefficients[2,c(1,2,4)]
summary(lm(h2o~retrn,data=hh_trans))
# RI_CI(hh_h2o$h2o,hh_h2o$retrn,hh_h2o$bloc,c(-.12,-.09,.01), model=1,nr_repl = 1000)

res_h2o[1:3,1,2] <- summary(lm(h2o~retrn,data=hh_h2o))$coefficients[2,c(1,2,4)]

summary(lm(h2o~tech+as.factor(bloc),data=hh_h2o))
#RI_CI(hh_h2o$h2o,hh_h2o$tech,hh_h2o$bloc,c(.1,.11,.01), model=2,nr_repl = 1000)
res_h2o[1:3,2,1] <- summary(lm(h2o~tech+retrn+as.factor(bloc) ,data=hh_h2o))$coefficients[2,c(1,2,4)]

summary(lm(h2o~retrn+as.factor(bloc),data=hh_h2o))
res_h2o[1:3,2,2] <- summary(lm(h2o~retrn+tech+as.factor(bloc),data=hh_h2o))$coefficients[2,c(1,2,4)]

###merge in baseline
sec3f <- read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC3F.dta")
sec3f <- subset(sec3f,kk1 == "Water depth must not exceed 5cm during tillering")
sec3f$h2o <- sec3f$kk2 == "Yes, recommended"
bline <- merge(sec3f[c("hhid","h2o")], treats)
bline$h2o <- as.numeric(bline$h2o)
bline$time <- 0
hh_h2o$time <- 1

DID <- rbind(hh_h2o[c("hhid","tech","retrn","time","h2o","bloc")], bline[c("hhid","tech","retrn","time","h2o", "bloc")])

res_h2o[1:3,3,1] <- summary(lm(h2o~tech*time +retrn ,data=DID))$coefficients[ dim( summary(lm(h2o~tech*time +retrn ,data=DID))$coefficients)[1],c(1,2,4)]
res_h2o[1:3,3,2] <- summary(lm(h2o~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(h2o~retrn*time +tech ,data=DID))$coefficients)[1],c(1,2,4)]

basebal[13,1] <- mean(DID$h2o[DID$time==0], na.rm=T)
basebal[14,1] <- sd(DID$h2o[DID$time==0], na.rm=T)
basebal[13,2] <- coefficients(lm(h2o~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
basebal[14,2] <- coef(summary(lm(h2o~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]

basebal[13,3] <- coefficients(lm(h2o~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
basebal[14,3] <- coef(summary(lm(h2o~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]

motivate[5,1] <- mean(DID$h2o[DID$time==0 & DID$tech==FALSE], na.rm=T)
motivate[6,1] <- sd(DID$h2o[DID$time==0 & DID$tech==FALSE], na.rm=T)

motivate[5,2] <- mean(DID$h2o[DID$time==1 & DID$tech==FALSE], na.rm=T)
motivate[6,2] <- sd(DID$h2o[DID$time==1 & DID$tech==FALSE], na.rm=T)

motivate[5,3] <- t.test(DID$h2o[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$statistic
motivate[6,3] <- t.test(DID$h2o[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$p.value

motivate[5,4] <- mean(DID$h2o[DID$time==0 & DID$retrn==FALSE], na.rm=T)
motivate[6,4] <- sd(DID$h2o[DID$time==0 & DID$retrn==FALSE], na.rm=T)

motivate[5,5] <- mean(DID$h2o[DID$time==1 & DID$retrn==FALSE], na.rm=T)
motivate[6,5] <- sd(DID$h2o[DID$time==1 & DID$retrn==FALSE], na.rm=T)

motivate[5,6] <- t.test(DID$h2o[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$statistic
motivate[6,6] <- t.test(DID$h2o[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$p.value

## include lagged rice share
lagdep <- merge(hh_h2o[c("hhid","tech","retrn","time","h2o","bloc")], bline[c("hhid","h2o")], by="hhid")
names(lagdep) <- c("hhid","tech","retrn","time","h2o","bloc","l_h2o")
res_h2o[1:3,4,1] <- summary(lm(h2o~tech+retrn+l_h2o+as.factor(bloc) ,data=lagdep))$coefficients[2,c(1,2,4)]

summary(lm(h2o~tech+l_h2o+as.factor(bloc) ,data=lagdep)
        df_compl <-  data.frame(lagdep$h2o, lagdep$tech, lagdep$bloc,lagdep$l_h2o)
        df_compl <- subset(df_compl,complete.cases(df_compl))
        #RI_CI(df_compl$lagdep.h2o,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_h2o)
        res_h2o[1:3,4,2] <- summary(lm(h2o~tech+retrn+l_h2o+as.factor(bloc) ,data=lagdep))$coefficients[3,c(1,2,4)]
        
        summary(lm(h2o~retrn+l_h2o+as.factor(bloc) ,data=lagdep))
        
        df_compl <-  data.frame(lagdep$h2o, lagdep$retrn, lagdep$bloc,lagdep$l_h2o)
        df_compl <- subset(df_compl,complete.cases(df_compl))
        #RI_CI(df_compl$lagdep.h2o,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.14,.15,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_h2o)
        #RI_CI(df_compl$lagdep.rowplant,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.02,.04,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_rowplant)
        
        #####  use nursary --- do we have this in baseline?
        res_nurse <- array(NA,c(3,4,2))
        hh_nurse <- aggregate((as.numeric(as.character(plots$nurse_use)) == 1), list(plots$hhid), max, na.rm=T)
        names(hh_nurse) <- c("hhid","use_nurse")
        hh_nurse <- subset(hh_nurse, hhid %in% planters)
        hh_nurse <- merge(treats,hh_nurse)
        
        
        dta2 <- merge(hh_nurse,ag_gps, by.x="hhid", by.y="hhid")
        
        
        
        mat <- as.matrix(dist(cbind(dta2$lat,dta2$long)))
        diag(mat) <- NA
        ### .06 is maximum distance within village - but we take .0225 as cutoff also based on visual inspection of the map - this corresponds to about 2.5km
        mat[mat>.0225] <- NA
        mat <- (.0225 - mat)/.0225
        
        ### keep only distances for treated
        dta2$dist <- rowMeans(mat*outer(as.numeric(dta2$tech), as.numeric(dta2$tech)), na.rm=T)
        mat[!is.na(mat)] <- 1
        mat[is.na(mat)] <- 0
        
        
        dta2$inder <- mat %*% as.numeric(dta2$tech) > median(mat %*% as.numeric(dta2$tech))+1
        
        t.test(dta2$use_nurse~dta2$inder)
        dta2$inder <- mat %*% as.numeric(dta2$retrn) > median(mat %*% as.numeric(dta2$retrn))+1
        
        t.test(dta2$use_nurse~dta2$inder)
        summary(lm(use_nurse~tech, data=hh_nurse))
        #RI(hh_nurse$use_nurse,hh_nurse$tech, hh_nurse$bloc, nr_repl=10000)
        
        #RI_CI(hh_nurse$use_nurse,hh_nurse$tech,hh_nurse$bloc,c(-.02,.0,.01), model=1,nr_repl = 1000)
        res_nurse[1:3,1,1] <- summary(lm(use_nurse~tech,data=hh_nurse))$coefficients[2,c(1,2,4)]
        summary(lm(use_nurse~retrn, data=hh_nurse))
        #RI_CI(hh_nurse$use_nurse,hh_nurse$retrn,hh_nurse$bloc,c(-.02,.0,.01), model=1,nr_repl = 1000)
        res_nurse[1:3,1,2] <- summary(lm(use_nurse~retrn,data=hh_nurse))$coefficients[2,c(1,2,4)]
        
        summary(lm(use_nurse~tech+as.factor(bloc) ,data=hh_nurse))
        #RI_CI(hh_nurse$use_nurse,hh_nurse$tech,hh_nurse$bloc,c(-.02,.0,.01), model=2,nr_repl = 1000)
        summary(lm(use_nurse~retrn+as.factor(bloc) ,data=hh_nurse))
        #RI_CI(hh_nurse$use_nurse,hh_nurse$retrn,hh_nurse$bloc,c(-.02,.0,.01), model=2,nr_repl = 1000)
        res_nurse[1:3,2,1] <- summary(lm(use_nurse~tech+retrn+as.factor(bloc) ,data=hh_nurse))$coefficients[2,c(1,2,4)]
        res_nurse[1:3,2,2] <- summary(lm(use_nurse~tech+retrn+as.factor(bloc) ,data=hh_nurse))$coefficients[3,c(1,2,4)]
        
        sec3f <- read.dta("/home/bjvca/data/projects/PASIC/riceRCT/baseline/RSEC3F.dta")
        sec3f <- subset(sec3f,kk1 == "Prepare the bed immediately after the first ploughing (field)")
        sec3f$use_nurse <- sec3f$kk2 == "Yes, recommended"
        bline <- merge(sec3f[c("hhid","use_nurse")], treats)
        bline$use_nurse <- as.numeric(bline$use_nurse)
        bline$time <- 0
        hh_nurse$time <- 1
        
        DID <- rbind(hh_nurse[c("hhid","tech","retrn","time","use_nurse","bloc")], bline[c("hhid","tech","retrn","time","use_nurse", "bloc")])
        summary(lm(use_nurse~tech*time +retrn + as.factor(bloc),data=DID))
        res_nurse[1:3,3,1] <- summary(lm(use_nurse~tech*time +retrn ,data=DID))$coefficients[ dim( summary(lm(use_nurse~tech*time +retrn ,data=DID))$coefficients)[1],c(1,2,4)]
        res_nurse[1:3,3,2] <- summary(lm(use_nurse~retrn*time + tech ,data=DID))$coefficients[ dim( summary(lm(use_nurse~retrn*time +tech ,data=DID))$coefficients)[1],c(1,2,4)]
        
        basebal[15,1] <- mean(DID$use_nurse[DID$time==0], na.rm=T)
        basebal[16,1] <- sd(DID$use_nurse[DID$time==0], na.rm=T)
        basebal[15,2] <- coefficients(lm(use_nurse~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[2]
        basebal[16,2] <- coef(summary(lm(use_nurse~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[2,2]
        
        basebal[15,3] <- coefficients(lm(use_nurse~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,]))[3]
        basebal[16,3] <- coef(summary(lm(use_nurse~tech+ retrn + as.factor(bloc),data=DID[DID$time==0,])))[3,2]
        
        motivate[7,1] <- mean(DID$use_nurse[DID$time==0 & DID$tech==FALSE], na.rm=T)
        motivate[8,1] <- sd(DID$use_nurse[DID$time==0 & DID$tech==FALSE], na.rm=T)
        
        motivate[7,2] <- mean(DID$use_nurse[DID$time==1 & DID$tech==FALSE], na.rm=T)
        motivate[8,2] <- sd(DID$use_nurse[DID$time==1 & DID$tech==FALSE], na.rm=T)
        
        motivate[7,3] <- t.test(DID$use_nurse[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$statistic
        motivate[8,3] <- t.test(DID$use_nurse[DID$tech==FALSE]~ DID$time[DID$tech==FALSE] )$p.value
        
        motivate[7,4] <- mean(DID$use_nurse[DID$time==0 & DID$retrn==FALSE], na.rm=T)
        motivate[8,4] <- sd(DID$use_nurse[DID$time==0 & DID$retrn==FALSE], na.rm=T)
        
        motivate[7,5] <- mean(DID$use_nurse[DID$time==1 & DID$retrn==FALSE], na.rm=T)
        motivate[8,5] <- sd(DID$use_nurse[DID$time==1 & DID$retrn==FALSE], na.rm=T)
        
        motivate[7,6] <- t.test(DID$use_nurse[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$statistic
        motivate[8,6] <- t.test(DID$use_nurse[DID$retrn==FALSE]~ DID$time[DID$retrn==FALSE] )$p.value
        ## include lagged rice share
        lagdep <- merge(hh_nurse[c("hhid","tech","retrn","time","use_nurse","bloc")], bline[c("hhid","use_nurse")], by="hhid")
        names(lagdep) <- c("hhid","tech","retrn","time","use_nurse","bloc","l_use_nurse")
        res_nurse[1:3,4,1] <- summary(lm(use_nurse~tech+retrn+l_use_nurse+as.factor(bloc) ,data=lagdep))$coefficients[2,c(1,2,4)]
        res_nurse[1:3,4,2] <- summary(lm(use_nurse~tech+retrn+l_use_nurse+as.factor(bloc) ,data=lagdep))$coefficients[3,c(1,2,4)]
        
        summary(lm(use_nurse~tech+l_use_nurse+as.factor(bloc) ,data=lagdep))
        df_compl <-  data.frame(lagdep$use_nurse, lagdep$tech, lagdep$bloc,lagdep$l_use_nurse)
        df_compl <- subset(df_compl,complete.cases(df_compl))
        #RI_CI(df_compl$lagdep.use_nurse,df_compl$lagdep.tech,df_compl$lagdep.bloc,c(-.28,-.26,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_use_nurse)
        
        summary(lm(use_nurse~retrn+l_use_nurse+as.factor(bloc) ,data=lagdep))
        df_compl <-  data.frame(lagdep$use_nurse, lagdep$retrn, lagdep$bloc,lagdep$l_use_nurse)
        df_compl <- subset(df_compl,complete.cases(df_compl))
        #RI_CI(df_compl$lagdep.use_nurse,df_compl$lagdep.retrn,df_compl$lagdep.bloc,c(.14,.15,.001), model=3,nr_repl = 1000,ldep=df_compl$lagdep.l_use_nurse)
        ##########################
        
        #plant in rows -  not significant
        plot_treat <- merge(plots, treats, by="hhid")
        plot_treat$plantin_rows[is.na(plot_treat$plantin_rows)] <- 2
        plot_treat$plantin_rows <- as.numeric(as.character(plot_treat$plantin_rows))
        
        rowplant <- aggregate((as.numeric(as.character(plot_treat$plantin_rows)) == 1), list(plot_treat$hhid), max, na.rm=T)
        names(rowplant) <- c("hhid","rowplant")
        rowplant <- subset(rowplant, hhid %in% planters)
        rowplant <- merge(treats,rowplant)
        
        summary(lm(rowplant~tech, data=rowplant))
        
        
        
        ### note: should merge with those who did not harvest yet - not in video
        plot_treat$leave_strw2 <- as.numeric(as.character(plot_treat$leave_strw2))
        summary(lm((leave_strw2==1)~tech+retrn+as.factor(bloc), data=plot_treat))
        
        
        ### note: should merge with those who did not harvest yet - not in video
        plot_treat$labour_hired <- as.numeric(as.character(plot_treat$labour_hired))
        summary(lm((labour_hired==1)~tech+retrn+as.factor(bloc), data=plot_treat))
        
        
        
        #### use quadratic loss function -  not significant
        plot_treat$seedlings_no <- as.numeric(as.character(plot_treat$seedlings_no))
        summary(lm((2-seedlings_no)^2~retrn+tech+as.factor(bloc), data=plot_treat))
        
        #### use quadratic loss function -  not significant
        plot_treat$bund_hgt <- as.numeric(as.character(plot_treat$bund_hgt))
        plot_treat$bund_hgt[plot_treat$bund_hgt<6] <- NA
        
        summary(lm((20-bund_hgt)^2~tech+retrn+as.factor(bloc), data=plot_treat))
        
        plot_treat$bund_wdth <- as.numeric(as.character(plot_treat$bund_wdth))
        plot_treat$bund_wdth[plot_treat$bund_wdth<6] <- NA
        summary(lm((20-bund_wdth)^2~tech+retrn+as.factor(bloc), data=plot_treat))
        
        plot_treat$seed_qty <- as.numeric(as.character(plot_treat$seed_qty))
        
        summary(lm((15-seed_qty/plot_area)^2~tech+retrn+as.factor(bloc), data=plot_treat))
        
        plot_treat$depth_plant <- as.numeric(as.character(plot_treat$depth_plant))
        
        summary(lm((depth_plant-1.5)^2~retrn+as.factor(bloc), data=plot_treat))
        
        
        plot_treat$dist_hill <- as.numeric(as.character(plot_treat$dist_hill))
        plot_treat$dist_hill[plot_treat$dist_hill > 20] <- NA
        
        summary(lm((8-dist_hill)^2~retrn+as.factor(bloc), data=plot_treat))
        
        
        plot_treat$h2o_deth <- as.numeric(as.character(plot_treat$h2o_deth))
        
        
        summary(lm((2-h2o_deth)^2~tech +retrn+as.factor(bloc), data=plot_treat))
        
        
        ## aspirations
        ### farming as a business: do you want your child to become a rice farmers
        biz <- merge(treats,dta[c("hhid","rice.rice_main_biz","rice.child_rice")])
        biz$rice.child_rice <- as.numeric(as.character(biz$rice.child_rice))
        
        summary(lm((rice.child_rice==1)~retrn+tech+as.factor(bloc), data=biz))
        
        biz$rice.rice_main_biz <- as.numeric(as.character(biz$rice.rice_main_biz))
        
        summary(lm((rice.rice_main_biz==1)~retrn+tech+as.factor(bloc), data=biz))
        
        
        
        ### locus of control
        
        loc <- merge(treats, dta[c(10,175:188)]) 
        loc$rice.bags_one_acre <- as.numeric(as.character(loc$rice.bags_one_acre))
        loc$rice.bags_like_get <- as.numeric(as.character(loc$rice.bags_like_get))
        loc$rice.bags_able <- as.numeric(as.character(loc$rice.bags_able))
        loc$rice.bags_best_farmer <- as.numeric(as.character(loc$rice.bags_best_farmer))
        
        summary(lm((rice.bags_like_get -  rice.bags_one_acre)~retrn+tech+as.factor(bloc), data=loc))
        summary(lm(( rice.bags_best_farmer - rice.bags_able)~retrn+tech+as.factor(bloc), data=loc))
        
        
        # loc=chance 91 percent
        
        summary(lm((rice.locus1==1)~retrn+tech+as.factor(bloc), data=loc))
        
        # loc=other 84 percent
        
        summary(lm((rice.locus2==1)~retrn+tech+as.factor(bloc), data=loc))
        
        # determination 76 percent
        summary(lm((rice.locus3 == 1)~retrn+tech+as.factor(bloc), data=loc))
        
        ## fatalist 75 percent
        summary(lm((rice.locus4 == 1)~retrn+tech+as.factor(bloc), data=loc))
        
        ## self esteem 86 percent
        summary(lm((rice.locus5 == 1)~retrn+tech+as.factor(bloc), data=loc))
        
        
        ## locus = self 81 percent
        summary(lm((rice.locus6 == 1)~retrn+tech+as.factor(bloc), data=loc))
        ## optimist 76 percent
        summary(lm((rice.locus8 == 1)~retrn+tech+as.factor(bloc), data=loc))
        
        ## remember video
        rem <- merge(treats, dta[c(10,167:172)])
        # time management
        summary(lm((rice.best_transp ==1)~tech, data=rem))
        summary(lm((rice.best_transp ==1)~retrn, data=rem))
        summary(lm((rice.best_transp ==1)~tech+as.factor(bloc), data=rem))
        summary(lm((rice.best_transp ==1)~retrn+as.factor(bloc), data=rem))
        
        summary(lm((rice.rem_vid ==1)~tech, data=rem))
        summary(lm((rice.rem_vid ==1)~retrn, data=rem))
        summary(lm((rice.rem_vid ==1)~tech+as.factor(bloc), data=rem))
        summary(lm((rice.rem_vid ==1)~retrn+as.factor(bloc), data=rem))
        
        ## water management
        summary(lm((rice.urea_app ==3)~tech, data=rem))
        summary(lm((rice.urea_app ==3)~retrn, data=rem))
        summary(lm((rice.urea_app ==3)~tech+as.factor(bloc), data=rem))
        summary(lm((rice.urea_app ==3)~retrn+as.factor(bloc), data=rem))
        
        summary(lm((rice.rem_water ==1)~tech, data=rem))
        summary(lm((rice.rem_water ==1)~retrn, data=rem))
        summary(lm((rice.rem_water ==1)~tech+as.factor(bloc), data=rem))
        summary(lm((rice.rem_water ==1)~retrn+as.factor(bloc), data=rem))
        
        ## straws in field
        
        summary(lm((rice.done_straw ==1)~tech, data=rem))
        summary(lm((rice.done_straw ==1)~retrn, data=rem))
        summary(lm((rice.done_straw ==1)~tech+as.factor(bloc), data=rem))
        summary(lm((rice.done_straw ==1)~retrn+as.factor(bloc), data=rem))
        
        summary(lm((rice.rem_straw ==1)~tech, data=rem))
        summary(lm((rice.rem_straw ==1)~retrn, data=rem))
        summary(lm((rice.rem_straw ==1)~tech+as.factor(bloc), data=rem))
        summary(lm((rice.rem_straw ==1)~retrn+as.factor(bloc), data=rem))
        
        
        
        pdf("/home/bjvca/data/projects/PASIC/riceRCT/paper/WDprop/endline_knowledge.pdf")
        par(mfrow=c(1,3))
        siglev <-  1.96
        
        rem$tech[rem$tech==TRUE] <- "TI video"
        rem$tech[rem$tech==FALSE] <- "No TI video"
        
        means <- tapply(rem$rice.best_transp ==1,rem$tech, FUN=mean)
        barCenters <- barplot(tapply(rem$rice.best_transp ==1,rem$tech, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Timing",xlab="",ylab = "Proportion choosing correct answer")
        
        segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
                 means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5)
        
        arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
               means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5, angle = 90,
               code = 3, length = 0.05)
        
        means <- tapply(rem$rice.urea_app ==3,rem$tech, FUN=mean)
        barCenters <- barplot(tapply(rem$rice.urea_app ==3,rem$tech, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Water",xlab="")
        
        segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
                 means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5)
        
        arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
               means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5, angle = 90,
               code = 3, length = 0.05)
        
        means <- tapply(rem$rice.done_straw ==1,rem$tech, FUN=mean)
        barCenters <- barplot(tapply(rem$rice.done_straw ==1,rem$tech, FUN=mean), ylim=c(0,1), xpd = FALSE, main="Straws",xlab="")
        
        segments(barCenters, means  -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
                 means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5)
        
        arrows(barCenters, means -  siglev*sqrt(means*(1-means)/dim(dta)[1]), barCenters,
               means +  siglev*sqrt(means*(1-means)/dim(dta)[1]), lwd = 1.5, angle = 90,
               code = 3, length = 0.05)
        dev.off()
        
        ###baseline knowledge
        ### load data as submitted by field team
        raw_dta_jap <- read.csv("/home/bjvca/data/projects/PASIC/riceRCT/data/raw/Intervention_jap.csv", na.strings = "n/a")
        raw_dta_nyole <- read.csv("/home/bjvca/data/projects/PASIC/riceRCT/data/raw/Intervention_nyole.csv", na.strings = "n/a")
        raw_dta_sog <- read.csv("/home/bjvca/data/projects/PASIC/riceRCT/data/raw/Intervention_sog.csv", na.strings = "n/a")
        raw_dta <- rbind(raw_dta_jap,raw_dta_nyole,raw_dta_sog)
        names(raw_dta)[names(raw_dta) == "hh_id"] <- "hhid"
        
        
        ### merge in bloc numbers from original sampling list
        raw_dta <- merge(raw_dta,read.csv("/home/bjvca/data/projects/PASIC/riceRCT/sampling/sample_ID.csv")[c("hhid","bloc")], by="hhid", all.x=TRUE)
        
        ### handle replacements that were made in the field (see fieldnotes.docx)
        ### preserve original bloc numbers
        raw_dta$bloc[raw_dta$hhid==2213408] <- 5
        raw_dta$bloc[raw_dta$hhid==2213214] <-26
        raw_dta$bloc[raw_dta$hhid==2122203] <- 2
        raw_dta$bloc[raw_dta$hhid==2121506] <- 4
        raw_dta$bloc[raw_dta$hhid==2122903] <- 35
        raw_dta$bloc[raw_dta$hhid==2122210] <- 61
        raw_dta$bloc[raw_dta$hhid==2122211] <- 45
        raw_dta$bloc[raw_dta$hhid==2122202] <- 46
        raw_dta$bloc[raw_dta$hhid==2122713] <- 16
        raw_dta$bloc[raw_dta$hhid==2121801] <- 15
        raw_dta$bloc[raw_dta$hhid==2122611] <- 5
        
        raw_dta$bloc[raw_dta$hhid==2122212] <- 29
        raw_dta$bloc[raw_dta$hhid==2122213] <- 19
        dta <- raw_dta
        
        names(dta)[names(dta) == "id_info.HH.video_shown"] <- "treat"
        ### main effects in factorial design
        dta$tech <- dta$treat == "Tech" | dta$treat == "Tech+Retrn" 
        dta$retrn <- dta$treat == "Retrn" | dta$treat == "Tech+Retrn"
        
        
        ### knows best time to transplant (in tech video)
        dta$timing <- dta$id_info.HH.q11=="After_14_days"
        ### knows water should be kept in field after urea application (in tech video)
        dta$water <- dta$id_info.HH.q12=="Water_kept"
        ### knows what to do with straws (not in videos)
        dta$straws <- dta$id_info.HH.q13=="spread_straws"
        
        summary(lm(timing~tech, data=dta))
        summary(lm(water~tech, data=dta))
        summary(lm(straws~tech, data=dta))
        
        res_know[1,1] <- mean(dta$timing[dta$tech==FALSE])
        res_know[2,1] <-sd(dta$timing[dta$tech==FALSE])
        res_know[1,2] <- summary(lm(timing~tech,data=dta))$coefficients[2,1]
        
        #RI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc, model=1, nr_repl = 10000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc,c(.29,.31,.001), nr_repl = 1000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc,c(-.2,-.19,.001), model=1,nr_repl = 1000)
        
        res_know[1,1] <- mean(dta$water[dta$tech==FALSE])
        res_know[2,1] <-sd(dta$water[dta$tech==FALSE])
        res_know[1,2] <- summary(lm(timing~tech,data=dta))$coefficients[2,1]
        summary(lm(water~tech,data=dta))
        res_know[1,1,1] <- summary(lm(water~tech,data=dta))$coefficients[2,1]
        res_know[2,1,1] <- summary(lm(water~tech,data=hh_prod))$coefficients[2,2]
        res_know[3,1,1] <- summary(lm(water~tech,data=hh_prod))$coefficients[2,4]
        #RI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc, model=1, nr_repl = 10000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc,c(.29,.31,.001), nr_repl = 1000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc,c(-.2,-.19,.001), model=1,nr_repl = 1000)
        
        summary(lm(water~tech+as.factor(bloc),data=hh_prod))
        res_know[1,2,1] <- summary(lm(water~tech+as.factor(bloc) ,data=dta))$coefficients[2,1]
        res_know[2,2,1] <- summary(lm(water~tech+as.factor(bloc),data=hh_prod))$coefficients[2,2]
        res_know[3,2,1] <- summary(lm(water~tech+as.factor(bloc),data=hh_prod))$coefficients[2,4]
        #RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(.25,.35,.1),model=2, nr_repl = 1000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(-.25,-.15,.1), model=2,nr_repl = 1000)
        
        summary(lm(straws~tech,data=dta))
        res_know[1,1,1] <- summary(lm(straws~tech,data=hh_prod))$coefficients[2,1]
        res_know[2,1,1] <- summary(lm(straws~tech,data=hh_prod))$coefficients[2,2]
        res_know[3,1,1] <- summary(lm(straws~tech,data=hh_prod))$coefficients[2,4]
        #RI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc, model=1, nr_repl = 10000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc,c(.29,.31,.001), nr_repl = 1000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$tech,hh_prod$bloc,c(-.2,-.19,.001), model=1,nr_repl = 1000)
        
        summary(lm(straws~tech+as.factor(bloc),data=hh_prod))
        res_know[1,2,1] <- summary(lm(straws~tech+as.factor(bloc) ,data=hh_prod))$coefficients[2,1]
        res_know[2,2,1] <- summary(lm(straws~tech+as.factor(bloc),data=hh_prod))$coefficients[2,2]
        res_know[3,2,1] <- summary(lm(straws~tech+as.factor(bloc),data=hh_prod))$coefficients[2,4]
        #RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(.25,.35,.1),model=2, nr_repl = 1000)
        #RI_CI(log(hh_prod$tot_prod),hh_prod$retrn,hh_prod$bloc,c(-.25,-.15,.1), model=2,nr_repl = 1000)
        
        
        mean((dta$timing)[dta$retrn==FALSE])
        sd((dta$timing)[dta$retrn==FALSE])
        mean((rem$rice.best_transp==1)[rem$retrn==FALSE])
        sd((rem$rice.best_transp==1)[rem$retrn==FALSE])
        t.test((rem$rice.best_transp==1)[rem$retrn==FALSE],dta$timing[dta$retrn==FALSE])
        
        
        mean((dta$water)[dta$retrn==FALSE])
        sd((dta$water)[dta$retrn==FALSE])
        mean((rem$rice.urea_app==3)[rem$retrn==FALSE])
        sd((rem$rice.urea_app==3)[rem$retrn==FALSE])
        t.test((rem$rice.urea_app==3)[rem$retrn==FALSE],dta$water[dta$retrn==FALSE])
        
        
        mean((dta$straws)[dta$retrn==FALSE])
        sd((dta$straws)[dta$retrn==FALSE])
        mean((rem$rice.done_straw==1)[rem$retrn==FALSE])
        sd((rem$rice.done_straw==1)[rem$retrn==FALSE])
        t.test((rem$rice.done_straw==1)[rem$retrn==FALSE],dta$straws[dta$retrn==FALSE])
        
        
        
        
        
        
        
        
        
        