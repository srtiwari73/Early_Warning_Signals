######################## Resiliency Code ###########################

########## Library Import ###############

#### pkg to check existence of rqrd pkg and install them
library(librarian)

#### required package
pkg_load <- c("readxl","rio","plyr","PerformanceAnalytics","timetk","tidyverse","zoo","tseries","psych","sjPlot","writexl","stargazer","dplyr","forecast","epiDisplay","aTSA","rmgarch","rugarch","parallel","quantmod","DataExplorer","BSDA","geovol","ConnectednessApproach","primes")

librarian::shelf(pkg_load)

### Data Import ###
library(readxl)
df <- read_excel("data.xlsx")
df_zoo <- read.zoo(df)
df_zoo[is.na(df_zoo)] <- 0

########## Optimal Lag #######################
library(vars)
VARselect(df_zoo, lag.max = 10, type = "const")

########### Descriptive statistics ###########
# Test statistics export
lag_order <- 1
ADF_test_stat <- function(dz,lag_value){
  
  require(tseries)
  y <- tseries::adf.test(dz,k=lag_value)
  return(as.numeric(y$statistic))
}

cols <- ncol(df_zoo)

x <- as.data.frame(matrix(numeric(),nrow = 1 , ncol = cols))

for (i in 1:(cols)) {
  
  x[1,i] <- ADF_test_stat(df_zoo[,i],lag_order)
  
}

x <- t(x)
x <- round(x,5)

desc <- psych::describe(df_zoo)
desc <- as.data.frame(round(desc,5))
desc_export <- cbind(desc[,c(3,4,8,9,11,12)],x)
colnames(desc_export)[7] <- "ADF-Test"
write.csv(desc_export,"Summary.csv")

### correlation
library(corrtable)
corr_matrix <- correlation_matrix(as.data.frame(df_zoo))
write.csv(corr_matrix,"correlation.csv")

################### Systemic Risk Estimation ###########
##### Static Analysis at frequency ######
library(ConnectednessApproach)

lag_value <- 1
forecast_horizon <- 20

partition = c(pi+0.00001, pi/5, pi/20, 0)
dcaf_static <- ConnectednessApproach(df_zoo, 
                                     model="VAR",
                                     connectedness="Frequency",
                                     nlag=1,
                                     nfore=20,
                                     window.size=NULL,
                                     Connectedness_config = list(
                                       FrequencyConnectedness=list(partition=c(pi+0.00001, pi/5, pi/20, 0), generalized=TRUE, scenario="ABS")
                                     ))

write.csv(dcaf_static$TABLE,"static.csv")

###### Connectedness Ratios
#### 1 to 5 days
### Risk Absorption Ratio
### extractinng the FROM table and idiosyncratic shock
risk_abs_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_abs_df[1,i] <- as.numeric(dcaf_static$TABLE[i,ncol(dcaf_static$TABLE),2])/as.numeric(dcaf_static$TABLE[i,i,2])    # 2 for 1to5 days
  
}
colnames(risk_abs_df) <- colnames(df_zoo)

### Risk Amplification Ratio
### extractinng the TO table and idiosyncratic shock
risk_amp_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_amp_df[1,i] <- as.numeric(dcaf_static$TABLE[(ncol(dcaf_static$TABLE)+1),i,2])/as.numeric(dcaf_static$TABLE[i,i,2]) # 2 for 1to5 days
  
}
colnames(risk_amp_df) <- colnames(df_zoo)

###### stability score ######
res_val <- function(dz){
  
  dz <- as.numeric(dz)
  
  mean_val <- mean(dz)
  max_val <- max(dz)
  min_val <- min(dz)
  
  spec_val <- matrix(,ncol = length(dz))
  
  for (i in 1:length(dz)) {
    temp_val <- abs((dz[i] - mean_val)/(max_val-min_val))
    spec_val[1,i] <- ceiling(100 * temp_val)
  }
  return(spec_val)
}

avg_abs_stab <- res_val(risk_abs_df)

avg_amp_stab <- res_val(risk_amp_df)

colnames(avg_abs_stab) <- colnames(df_zoo)
colnames(avg_amp_stab) <- colnames(df_zoo)


######  Average Aggregate Stability Score
## Nth prime of the number
p_abs_avg <- nth_prime(avg_abs_stab)
p_amp_avg <- nth_prime(avg_amp_stab)


##### Aggregation of the two scores
avg_stab_score <- (p_abs_avg * p_amp_avg)/(100 * 100)

avg_stab_score <- as.data.frame(avg_stab_score)

##### Portfolio with equal weights
wt_matrix <- rep(1/ncol(df_zoo), ncol(df_zoo))

avg_stab_score_port <- wt_matrix * avg_stab_score

avg_sys_stab <- sum(avg_stab_score_port)

avg_sys_res <- 100/avg_sys_stab

avg_sys_risk <-  as.numeric(t(wt_matrix) %*% as.matrix(dcaf_static$PCI[,,1,2])  %*% wt_matrix)   # 2 for 1to5 days

## exporting the output
res_matrix <- cbind(t(risk_abs_df),t(risk_amp_df),avg_stab_score,wt_matrix,avg_stab_score_port)

empty_df <- matrix(,nrow = ncol(df_zoo))
res_matrix <- cbind(res_matrix,empty_df,empty_df)

res_matrix[round(ncol(df_zoo)/2),6] <- avg_sys_res
res_matrix[round(ncol(df_zoo)/2),7] <- avg_sys_risk

colnames(res_matrix) <- c("Risk Absorption Ratio", "Risk Amplification Ratio","Avg Stability Score","Weights","Avg Score * W", "Avg System Resilience","Avg Systemic Risk")
rownames(res_matrix) <- colnames(df_zoo)

write.csv(res_matrix,"static_resiliency_matrix_1to5.csv")

#### 5 to 20 days
### Risk Absorption Ratio
### extractinng the FROM table and idiosyncratic shock
risk_abs_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_abs_df[1,i] <- as.numeric(dcaf_static$TABLE[i,ncol(dcaf_static$TABLE),3])/as.numeric(dcaf_static$TABLE[i,i,3])    # 2 for 5toinf days
  
}
colnames(risk_abs_df) <- colnames(df_zoo)

### Risk Amplification Ratio
### extractinng the TO table and idiosyncratic shock
risk_amp_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_amp_df[1,i] <- as.numeric(dcaf_static$TABLE[(ncol(dcaf_static$TABLE)+1),i,3])/as.numeric(dcaf_static$TABLE[i,i,3]) # 3 for 5toinf days
  
}
colnames(risk_amp_df) <- colnames(df_zoo)

###### stability score ######
avg_abs_stab <- res_val(risk_abs_df)

avg_amp_stab <- res_val(risk_amp_df)

colnames(avg_abs_stab) <- colnames(df_zoo)
colnames(avg_amp_stab) <- colnames(df_zoo)


######  Average Aggregate Stability Score
## Nth prime of the number
p_abs_avg <- nth_prime(avg_abs_stab)
p_amp_avg <- nth_prime(avg_amp_stab)


##### Aggregation of the two scores
avg_stab_score <- (p_abs_avg * p_amp_avg)/(100 * 100)

avg_stab_score <- as.data.frame(avg_stab_score)

##### Portfolio with equal weights
wt_matrix <- rep(1/ncol(df_zoo), ncol(df_zoo))

avg_stab_score_port <- wt_matrix * avg_stab_score

avg_sys_stab <- sum(avg_stab_score_port)

avg_sys_res <- 100/avg_sys_stab

avg_sys_risk <-  as.numeric(t(wt_matrix) %*% as.matrix(dcaf_static$PCI[,,1,3])  %*% wt_matrix)   # 2 for 5to20 days

## exporting the output
res_matrix <- cbind(t(risk_abs_df),t(risk_amp_df),avg_stab_score,wt_matrix,avg_stab_score_port)

empty_df <- matrix(,nrow = ncol(df_zoo))
res_matrix <- cbind(res_matrix,empty_df,empty_df)

res_matrix[round(ncol(df_zoo)/2),6] <- avg_sys_res
res_matrix[round(ncol(df_zoo)/2),7] <- avg_sys_risk

colnames(res_matrix) <- c("Risk Absorption Ratio", "Risk Amplification Ratio","Avg Stability Score","Weights","Avg Score * W", "Avg System Resilience","Avg Systemic Risk")
rownames(res_matrix) <- colnames(df_zoo)

write.csv(res_matrix,"static_resiliency_matrix_5to20.csv")

#### 20 to inf days
### Risk Absorption Ratio
### extractinng the FROM table and idiosyncratic shock
risk_abs_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_abs_df[1,i] <- as.numeric(dcaf_static$TABLE[i,ncol(dcaf_static$TABLE),4])/as.numeric(dcaf_static$TABLE[i,i,4])    # 4 for 20toinf days
  
}
colnames(risk_abs_df) <- colnames(df_zoo)

### Risk Amplification Ratio
### extractinng the TO table and idiosyncratic shock
risk_amp_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_amp_df[1,i] <- as.numeric(dcaf_static$TABLE[(ncol(dcaf_static$TABLE)+1),i,4])/as.numeric(dcaf_static$TABLE[i,i,4]) # 4 for 20toinf days
  
}
colnames(risk_amp_df) <- colnames(df_zoo)

###### stability score ######
avg_abs_stab <- res_val(risk_abs_df)

avg_amp_stab <- res_val(risk_amp_df)

colnames(avg_abs_stab) <- colnames(df_zoo)
colnames(avg_amp_stab) <- colnames(df_zoo)


######  Average Aggregate Stability Score
## Nth prime of the number
p_abs_avg <- nth_prime(avg_abs_stab)
p_amp_avg <- nth_prime(avg_amp_stab)


##### Aggregation of the two scores
avg_stab_score <- (p_abs_avg * p_amp_avg)/(100 * 100)

avg_stab_score <- as.data.frame(avg_stab_score)

##### Portfolio with equal weights
wt_matrix <- rep(1/ncol(df_zoo), ncol(df_zoo))

avg_stab_score_port <- wt_matrix * avg_stab_score

avg_sys_stab <- sum(avg_stab_score_port)

avg_sys_res <- 100/avg_sys_stab

avg_sys_risk <-  as.numeric(t(wt_matrix) %*% as.matrix(dcaf_static$PCI[,,1,4])  %*% wt_matrix)   # 4 for 20toinf days

## exporting the output
res_matrix <- cbind(t(risk_abs_df),t(risk_amp_df),avg_stab_score,wt_matrix,avg_stab_score_port)

empty_df <- matrix(,nrow = ncol(df_zoo))
res_matrix <- cbind(res_matrix,empty_df,empty_df)

res_matrix[round(ncol(df_zoo)/2),6] <- avg_sys_res
res_matrix[round(ncol(df_zoo)/2),7] <- avg_sys_risk

colnames(res_matrix) <- c("Risk Absorption Ratio", "Risk Amplification Ratio","Avg Stability Score","Weights","Avg Score * W", "Avg System Resilience","Avg Systemic Risk")
rownames(res_matrix) <- colnames(df_zoo)

write.csv(res_matrix,"static_resiliency_matrix_20toinf.csv")

#### all days or same as dy12 only
### Risk Absorption Ratio
### extractinng the FROM table and idiosyncratic shock
risk_abs_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_abs_df[1,i] <- as.numeric(dcaf_static$TABLE[i,ncol(dcaf_static$TABLE),1])/as.numeric(dcaf_static$TABLE[i,i,1])    # last 1 for all days
  
}
colnames(risk_abs_df) <- colnames(df_zoo)

### Risk Amplification Ratio
### extractinng the TO table and idiosyncratic shock
risk_amp_df <- matrix(nrow = 1, ncol = ncol(df_zoo))

for (i in 1:ncol(df_zoo)) {
  
  risk_amp_df[1,i] <- as.numeric(dcaf_static$TABLE[(ncol(dcaf_static$TABLE)+1),i,1])/as.numeric(dcaf_static$TABLE[i,i,1]) # last 1 for all days
  
}
colnames(risk_amp_df) <- colnames(df_zoo)

###### stability score ######
avg_abs_stab <- res_val(risk_abs_df)

avg_amp_stab <- res_val(risk_amp_df)

colnames(avg_abs_stab) <- colnames(df_zoo)
colnames(avg_amp_stab) <- colnames(df_zoo)


######  Average Aggregate Stability Score
## Nth prime of the number
p_abs_avg <- nth_prime(avg_abs_stab)
p_amp_avg <- nth_prime(avg_amp_stab)


##### Aggregation of the two scores
avg_stab_score <- (p_abs_avg * p_amp_avg)/(100 * 100)

avg_stab_score <- as.data.frame(avg_stab_score)

##### Portfolio with equal weights
wt_matrix <- rep(1/ncol(df_zoo), ncol(df_zoo))

avg_stab_score_port <- wt_matrix * avg_stab_score

avg_sys_stab <- sum(avg_stab_score_port)

avg_sys_res <- 100/avg_sys_stab

avg_sys_risk <-  as.numeric(t(wt_matrix) %*% as.matrix(dcaf_static$PCI[,,1,1])  %*% wt_matrix)   # last 1 for all days

## exporting the output
res_matrix <- cbind(t(risk_abs_df),t(risk_amp_df),avg_stab_score,wt_matrix,avg_stab_score_port)

empty_df <- matrix(,nrow = ncol(df_zoo))
res_matrix <- cbind(res_matrix,empty_df,empty_df)

res_matrix[round(ncol(df_zoo)/2),6] <- avg_sys_res
res_matrix[round(ncol(df_zoo)/2),7] <- avg_sys_risk

colnames(res_matrix) <- c("Risk Absorption Ratio", "Risk Amplification Ratio","Avg Stability Score","Weights","Avg Score * W", "Avg System Resilience","Avg Systemic Risk")
rownames(res_matrix) <- colnames(df_zoo)

write.csv(res_matrix,"static_resiliency_matrix_all.csv")
############ Static Analysis ends #####################


########################### Dynamic Analysis ##########

#######  Dynamic Connectedness
window_size <- 120

dcaf_rolling = ConnectednessApproach(df_zoo, 
                              model="VAR",
                              connectedness="Frequency",
                              nlag=1,
                              nfore=20,
                              window.size=window_size,
                              Connectedness_config = list(
                                FrequencyConnectedness=list(partition=c(pi+0.00001, pi/5, pi/20, 0), generalized=TRUE, scenario="ABS")
                              ))

#### Date extraction
date <- df$Date
date_roll <- date[-(1:(window_size-2))]

### Total Rolling
write.csv(dcaf_rolling$TCI,"Total_rolling.csv")

### TO connectedness
write.csv(dcaf_rolling$TO,"TO_rolling.csv")

### FROM connectedness
write.csv(dcaf_rolling$FROM,"FROM_rolling.csv")

### NET connectedness
write.csv(dcaf_rolling$NET,"NET_rolling.csv")

##### 1 to 5 days #####
#### matrix 
idio_matrix <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = ncol(df)-1)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  for (j in 1:(ncol(df)-1)) {
    idio_matrix[i,j] <- dcaf_rolling$CT[,,i,2][j,j]   # 2 to access band for 1to5 days
  }
}

colnames(idio_matrix) <- colnames(df)[-1]
#### combining the date column
rownames(idio_matrix) <- rownames(dcaf_rolling$TCI)

idio_matrix <- idio_matrix * 100

write.csv(idio_matrix,"idio_risk_1to5.csv")

########### Connectedness Ratios ############
abs_idio <- dcaf_rolling$FROM[,,2]/idio_matrix   # 2 to access band for 1to5 days
amp_idio <- dcaf_rolling$TO[,,2]/idio_matrix     # 2 to access band for 1to5 days

#### Stability scores
abs_stab <- matrix(,ncol = ncol(df_zoo))
amp_stab <- matrix(,ncol = ncol(df_zoo))

for (i in 1:nrow(abs_idio)) {
  temp1 <- res_val(abs_idio[i,])
  abs_stab <- rbind(abs_stab,temp1)
  temp2 <- res_val(amp_idio[i,])
  amp_stab <- rbind(amp_stab,temp2)
}

abs_stab <- abs_stab[-1,]
amp_stab <- amp_stab[-1,]

colnames(abs_stab) <- colnames(abs_idio)
colnames(amp_stab) <- colnames(abs_idio)

############  Calculate the N-th prime before multiplication 

nth_prime_abs <- matrix(, ncol = ncol(abs_stab))
nth_prime_amp <- matrix(, ncol = ncol(abs_stab))

for (i in 1:nrow(abs_stab)) {
  
  ###### ABsorption matrix 
  p_abs <- nth_prime(abs_stab[i,])
  nth_prime_abs <- rbind(nth_prime_abs,p_abs)
  
  p_amp <- nth_prime(amp_stab[i,])
  nth_prime_amp <- rbind(nth_prime_amp,p_abs)
  
}

nth_prime_abs <- nth_prime_abs[-1,]
nth_prime_amp <- nth_prime_amp[-1,]

dimnames(nth_prime_abs) <- NULL
dimnames(nth_prime_amp) <- NULL

###### Combined score #########
stab_score <- (nth_prime_abs * nth_prime_amp)/(100 * 100)


colnames(nth_prime_abs) <- colnames(abs_idio)
colnames(nth_prime_amp) <- colnames(abs_idio)
colnames(stab_score) <- colnames(abs_idio)

#################### Portfolio ######

##########Portfolio parameters Univariate ############

stab_port <- matrix(nrow = nrow(stab_score), ncol = 1)


###### Average spectrum score weighted
for (i in 1:nrow(stab_score)) {
  
  stab_port[i,1] <- as.numeric(t(wt_matrix) %*% stab_score[i,])
  
}

colnames(stab_port) <- c("Portfolio_Stability_Score")


port_res_score <- 100/stab_port

colnames(port_res_score) <- c("Portfolio Resiliency")


##### Systemic risk estimation

sys_risk <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = 1)

wt_matrix <- as.matrix(wt_matrix)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  
  sys_data <- as.matrix(dcaf_rolling$PCI[,,i,2])  # # 2 to access band for 1to5 days
  
  #sys_data <- mapply(sys_data, FUN=as.numeric)
  
  sys_risk[i,1] <- t(wt_matrix) %*% sys_data  %*% wt_matrix
}

colnames(sys_risk) <- c("Portfolio Systemic Risk")


port_res_sys_roll <- cbind(date_roll,port_res_score,sys_risk)

write.csv(port_res_sys_roll,"Res_sys_1to5.csv")
library(openxlsx)
results1to5 <- list("Absorption Ratio" = abs_idio, "Amplification Ratio" = amp_idio, "Stability Score" = stab_score)
write.xlsx(results1to5, "results1to5.xlsx")

##### 5 to 20 days #####
#### matrix 
idio_matrix <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = ncol(df)-1)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  for (j in 1:(ncol(df)-1)) {
    idio_matrix[i,j] <- dcaf_rolling$CT[,,i,3][j,j]   # 3 to access band for 5to20 days
  }
}

colnames(idio_matrix) <- colnames(df)[-1]
#### combining the date column
rownames(idio_matrix) <- rownames(dcaf_rolling$TCI)

idio_matrix <- idio_matrix * 100

write.csv(idio_matrix,"idio_risk_5to20.csv")

########### Connectedness Ratios ############
abs_idio <- dcaf_rolling$FROM[,,3]/idio_matrix   # 3 to access band for 5to20 days
amp_idio <- dcaf_rolling$TO[,,3]/idio_matrix     # 3 to access band for 5to20 days

#### Stability scores
abs_stab <- matrix(,ncol = ncol(df_zoo))
amp_stab <- matrix(,ncol = ncol(df_zoo))

for (i in 1:nrow(abs_idio)) {
  temp1 <- res_val(abs_idio[i,])
  abs_stab <- rbind(abs_stab,temp1)
  temp2 <- res_val(amp_idio[i,])
  amp_stab <- rbind(amp_stab,temp2)
}

abs_stab <- abs_stab[-1,]
amp_stab <- amp_stab[-1,]

colnames(abs_stab) <- colnames(abs_idio)
colnames(amp_stab) <- colnames(abs_idio)

############  Calculate the N-th prime before multiplication 

nth_prime_abs <- matrix(, ncol = ncol(abs_stab))
nth_prime_amp <- matrix(, ncol = ncol(abs_stab))

for (i in 1:nrow(abs_stab)) {
  
  ###### ABsorption matrix 
  p_abs <- nth_prime(abs_stab[i,])
  nth_prime_abs <- rbind(nth_prime_abs,p_abs)
  
  p_amp <- nth_prime(amp_stab[i,])
  nth_prime_amp <- rbind(nth_prime_amp,p_abs)
  
}

nth_prime_abs <- nth_prime_abs[-1,]
nth_prime_amp <- nth_prime_amp[-1,]

dimnames(nth_prime_abs) <- NULL
dimnames(nth_prime_amp) <- NULL

###### Combined score #########
stab_score <- (nth_prime_abs * nth_prime_amp)/(100 * 100)


colnames(nth_prime_abs) <- colnames(abs_idio)
colnames(nth_prime_amp) <- colnames(abs_idio)
colnames(stab_score) <- colnames(abs_idio)

#################### Portfolio ######

##########Portfolio parameters Univariate ############

stab_port <- matrix(nrow = nrow(stab_score), ncol = 1)


###### Average spectrum score weighted
for (i in 1:nrow(stab_score)) {
  
  stab_port[i,1] <- as.numeric(t(wt_matrix) %*% stab_score[i,])
  
}

colnames(stab_port) <- c("Portfolio_Stability_Score")


port_res_score <- 100/stab_port

colnames(port_res_score) <- c("Portfolio Resiliency")


##### Systemic risk estimation

sys_risk <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = 1)

wt_matrix <- as.matrix(wt_matrix)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  
  sys_data <- as.matrix(dcaf_rolling$PCI[,,i,3])  # # 3 to access band for 5to20 days
  
  #sys_data <- mapply(sys_data, FUN=as.numeric)
  
  sys_risk[i,1] <- t(wt_matrix) %*% sys_data  %*% wt_matrix
}

colnames(sys_risk) <- c("Portfolio Systemic Risk")


port_res_sys_roll <- cbind(date_roll,port_res_score,sys_risk)

write.csv(port_res_sys_roll,"Res_sys_5to20.csv")
#library(openxlsx)
results5to20 <- list("Absorption Ratio" = abs_idio, "Amplification Ratio" = amp_idio, "Stability Score" = stab_score)
write.xlsx(results5to20, "results5to20.xlsx")

##### 20 to inf days #####
idio_matrix <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = ncol(df)-1)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  for (j in 1:(ncol(df)-1)) {
    idio_matrix[i,j] <- dcaf_rolling$CT[,,i,4][j,j]   # 4 to access band for 20toinf days
  }
}

colnames(idio_matrix) <- colnames(df)[-1]
#### combining the date column
rownames(idio_matrix) <- rownames(dcaf_rolling$TCI)

idio_matrix <- idio_matrix * 100

write.csv(idio_matrix,"idio_risk_20toinf.csv")

########### Connectedness Ratios ############
abs_idio <- dcaf_rolling$FROM[,,4]/idio_matrix   # 4 to access band for 20toinf days
amp_idio <- dcaf_rolling$TO[,,4]/idio_matrix     # 4 to access band for 20toinf days

#### Stability scores
abs_stab <- matrix(,ncol = ncol(df_zoo))
amp_stab <- matrix(,ncol = ncol(df_zoo))

for (i in 1:nrow(abs_idio)) {
  temp1 <- res_val(abs_idio[i,])
  abs_stab <- rbind(abs_stab,temp1)
  temp2 <- res_val(amp_idio[i,])
  amp_stab <- rbind(amp_stab,temp2)
}

abs_stab <- abs_stab[-1,]
amp_stab <- amp_stab[-1,]

colnames(abs_stab) <- colnames(abs_idio)
colnames(amp_stab) <- colnames(abs_idio)

############  Calculate the N-th prime before multiplication 

nth_prime_abs <- matrix(, ncol = ncol(abs_stab))
nth_prime_amp <- matrix(, ncol = ncol(abs_stab))

for (i in 1:nrow(abs_stab)) {
  
  ###### ABsorption matrix 
  p_abs <- nth_prime(abs_stab[i,])
  nth_prime_abs <- rbind(nth_prime_abs,p_abs)
  
  p_amp <- nth_prime(amp_stab[i,])
  nth_prime_amp <- rbind(nth_prime_amp,p_abs)
  
}

nth_prime_abs <- nth_prime_abs[-1,]
nth_prime_amp <- nth_prime_amp[-1,]

dimnames(nth_prime_abs) <- NULL
dimnames(nth_prime_amp) <- NULL

###### Combined score #########
stab_score <- (nth_prime_abs * nth_prime_amp)/(100 * 100)


colnames(nth_prime_abs) <- colnames(abs_idio)
colnames(nth_prime_amp) <- colnames(abs_idio)
colnames(stab_score) <- colnames(abs_idio)

#################### Portfolio ######

##########Portfolio parameters Univariate ############

stab_port <- matrix(nrow = nrow(stab_score), ncol = 1)


###### Average spectrum score weighted
for (i in 1:nrow(stab_score)) {
  
  stab_port[i,1] <- as.numeric(t(wt_matrix) %*% stab_score[i,])
  
}

colnames(stab_port) <- c("Portfolio_Stability_Score")


port_res_score <- 100/stab_port

colnames(port_res_score) <- c("Portfolio Resiliency")


##### Systemic risk estimation

sys_risk <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = 1)

wt_matrix <- as.matrix(wt_matrix)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  
  sys_data <- as.matrix(dcaf_rolling$PCI[,,i,4])  # # 4 to access band for 20toinf days
  
  #sys_data <- mapply(sys_data, FUN=as.numeric)
  
  sys_risk[i,1] <- t(wt_matrix) %*% sys_data  %*% wt_matrix
}

colnames(sys_risk) <- c("Portfolio Systemic Risk")


port_res_sys_roll <- cbind(date_roll,port_res_score,sys_risk)

write.csv(port_res_sys_roll,"Res_sys_20toinf.csv")
#library(openxlsx)
results20toinf <- list("Absorption Ratio" = abs_idio, "Amplification Ratio" = amp_idio, "Stability Score" = stab_score)
write.xlsx(results20toinf, "results20toinf.xlsx")

##### all days #####
idio_matrix <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = ncol(df)-1)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  for (j in 1:(ncol(df)-1)) {
    idio_matrix[i,j] <- dcaf_rolling$CT[,,i,1][j,j]   # 1 to access band for all days
  }
}

colnames(idio_matrix) <- colnames(df)[-1]
#### combining the date column
rownames(idio_matrix) <- rownames(dcaf_rolling$TCI)

idio_matrix <- idio_matrix * 100

write.csv(idio_matrix,"idio_risk_all.csv")

########### Connectedness Ratios ############
abs_idio <- dcaf_rolling$FROM[,,1]/idio_matrix   # 1 to access band for all days
amp_idio <- dcaf_rolling$TO[,,1]/idio_matrix     # 1 to access band for all days

#### Stability scores
abs_stab <- matrix(,ncol = ncol(df_zoo))
amp_stab <- matrix(,ncol = ncol(df_zoo))

for (i in 1:nrow(abs_idio)) {
  temp1 <- res_val(abs_idio[i,])
  abs_stab <- rbind(abs_stab,temp1)
  temp2 <- res_val(amp_idio[i,])
  amp_stab <- rbind(amp_stab,temp2)
}

abs_stab <- abs_stab[-1,]
amp_stab <- amp_stab[-1,]

colnames(abs_stab) <- colnames(abs_idio)
colnames(amp_stab) <- colnames(abs_idio)

############  Calculate the N-th prime before multiplication 

nth_prime_abs <- matrix(, ncol = ncol(abs_stab))
nth_prime_amp <- matrix(, ncol = ncol(abs_stab))

for (i in 1:nrow(abs_stab)) {
  
  ###### ABsorption matrix 
  p_abs <- nth_prime(abs_stab[i,])
  nth_prime_abs <- rbind(nth_prime_abs,p_abs)
  
  p_amp <- nth_prime(amp_stab[i,])
  nth_prime_amp <- rbind(nth_prime_amp,p_abs)
  
}

nth_prime_abs <- nth_prime_abs[-1,]
nth_prime_amp <- nth_prime_amp[-1,]

dimnames(nth_prime_abs) <- NULL
dimnames(nth_prime_amp) <- NULL

###### Combined score #########
stab_score <- (nth_prime_abs * nth_prime_amp)/(100 * 100)


colnames(nth_prime_abs) <- colnames(abs_idio)
colnames(nth_prime_amp) <- colnames(abs_idio)
colnames(stab_score) <- colnames(abs_idio)

#################### Portfolio ######

##########Portfolio parameters Univariate ############

stab_port <- matrix(nrow = nrow(stab_score), ncol = 1)


###### Average spectrum score weighted
for (i in 1:nrow(stab_score)) {
  
  stab_port[i,1] <- as.numeric(t(wt_matrix) %*% stab_score[i,])
  
}

colnames(stab_port) <- c("Portfolio_Stability_Score")


port_res_score <- 100/stab_port

colnames(port_res_score) <- c("Portfolio Resiliency")


##### Systemic risk estimation

sys_risk <- matrix(nrow = nrow(dcaf_rolling$TCI),ncol = 1)

wt_matrix <- as.matrix(wt_matrix)

for (i in 1:nrow(dcaf_rolling$TCI)) {
  
  sys_data <- as.matrix(dcaf_rolling$PCI[,,i,1])  # # 1 to access band for all days
  
  #sys_data <- mapply(sys_data, FUN=as.numeric)
  
  sys_risk[i,1] <- t(wt_matrix) %*% sys_data  %*% wt_matrix
}

colnames(sys_risk) <- c("Portfolio Systemic Risk")


port_res_sys_roll <- cbind(date_roll,port_res_score,sys_risk)

write.csv(port_res_sys_roll,"Res_sys_all.csv")
#library(openxlsx)
resultsall <- list("Absorption Ratio" = abs_idio, "Amplification Ratio" = amp_idio, "Stability Score" = stab_score)
write.xlsx(resultsall, "resultsall.xlsx")


##################### Early Warning Signal ###############

EWS_func <- function(score_df,burn_period,criterion,method_used,threshold_val){
  
  require(EWSmethods)
  time_stamp <- 1:nrow(score_df)
  avg_spect_score <- cbind(time_stamp,score_df)
  ews_eg <- uniEWS(data = avg_spect_score, metrics = criterion,method = method_used ,burn_in = burn_period,threshold = threshold_val)
  return(ews_eg)
  
}

### Parameters
burn_period <- 50
criterion <- "SD"
method_used <- "expanding"
threshold_val <- 1


ews_th1 <- EWS_func(sys_risk,burn_period,criterion,method_used,threshold_val)
plot(ews_th1)
### merging with date
th1 <- ews_th1$EWS[,c(2,6)]
th1 <- cbind(date_roll[-(1:(burn_period-1))],th1)

write.csv(th1,"Th1.csv")

threshold_val <- 1.5
ews_th2 <- EWS_func(stab_port,burn_period,criterion,method_used,threshold_val)
plot(ews_th2)
### merging with date
th2 <- ews_th2$EWS[,c(2,6)]
th2 <- cbind(date_roll[-(1:(burn_period-1))],th2)

write.csv(th2,"Th2.csv")

threshold_val <- 2
ews_th3 <- EWS_func(stab_port,burn_period,criterion,method_used,threshold_val)
plot(ews_th3)
### merging with date
th3 <- ews_th3$EWS[,c(2,6)]
th3 <- cbind(date_roll[-(1:(burn_period-1))],th3)

write.csv(th3,"Th3.csv")


###################  Robustness Check ################
burn_period <- 50
method_used <- "expanding"
threshold_val <- 2
######### CV #############
criterion <- "cv"
ews_cv <- EWS_func(stab_port,burn_period,criterion,method_used,threshold_val)
plot(ews_cv)
### merging with date
cv <- ews_cv$EWS[,c(2,6)]
cv <- cbind(date_roll[-(1:(burn_period-1))],cv)
write.csv(cv,"cv.csv")

####### Return rate
criterion <- "rr"
ews_rr <- EWS_func(stab_port,burn_period,criterion,method_used,threshold_val)
plot(ews_rr)

### merging with date
rr <- ews_rr$EWS[,c(2,6)]
rr <- cbind(date_roll[-(1:(burn_period-1))],rr)
write.csv(rr,"rr.csv")

####### Density Ratio
criterion <- "dr"
ews_dr <- EWS_func(stab_port,burn_period,criterion,method_used,threshold_val)
plot(ews_dr)

### merging with date
dr <- ews_dr$EWS[,c(2,6)]
dr <- cbind(date_roll[-(1:(burn_period-1))],dr)
write.csv(dr,"dr.csv")


