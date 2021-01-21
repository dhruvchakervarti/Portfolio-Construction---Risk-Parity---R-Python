library(readxl)
library(data.table)
library(zoo)
library(fBasics)
library(DescTools)
setwd("D:/Quarter3/QAM")



CRSP_Bonds <- read.csv('Bonddata.csv',na.strings=c("","NA"))

Bonds <- function(data){
  data$MCALDT<- as.Date(data$MCALDT)
  data$Year <- format(data$MCALDT, "%Y")
  data$Month <- month(data$MCALDT)
  data <- as.data.table(data)
  data <- data[,Bond_lag_MV:= shift(TMTOTOUT), by=KYCRSPID]
  Total_MV <- data[,list(Bond_lag_MV_Total = sum(Bond_lag_MV,na.rm=TRUE)*1000), by = list(MCALDT)] #check by
  data <- merge(data, Total_MV, by="MCALDT", all.x=T)
  data <- data[,weights:= Bond_lag_MV/Bond_lag_MV_Total]
  data1 <- data[,list(Bond_lag_MV = sum(Bond_lag_MV,na.rm=TRUE)*1000, Bond_Ew_Ret = mean(TMRETNUA,na.rm=TRUE),
                                  Bond_Vw_Ret = sum(weights*TMRETNUA,na.rm = TRUE)*1000), by = list(Year,Month)]
  data1 <- data1[data1$Year>=1926,]
  return(data1)
  
}

Monthly_CRSP_Bonds <- Bonds(CRSP_Bonds)
head(Monthly_CRSP_Bonds)
tail(Monthly_CRSP_Bonds)

#temp <- data.table(read_excel('Market_data.xlsx'), na.strings =c("","NA"))
CRSP_Stocks <- data.table(read.csv('QAM1.csv',na.strings=c("","NA")))
#CRSP_Stocks <- temp[,1:8]


Stocks <- function(data) {
  names(data) <- c("PERMNO","date","SHRCD","EXCHCD","DLRET","PRC", "RET","SHROUT")
  data$date <- as.Date(as.character(data$date),"%Y%m%d")
  data$Year <- format(data$date, "%Y")
  data$Month <- month(data$date)
  data <- data[EXCHCD %in% c(1,2,3,4)] #As per sir's instruction in lecture
  data$RET <- as.numeric(ifelse(as.character(data$RET) %like any% c("A", "B", "C","D","E"), NA, as.character(data$RET)))
  data$DLRET <- as.numeric(ifelse(as.character(data$DLRET) %like any% c("A", "P", "S","T"), NA, as.character(data$DLRET)))
  data <- as.data.table(data)
  data <- data[,Stock_lag_MV:= abs(shift(PRC))*shift(SHROUT), by = PERMNO]
  data <- data[,CumReturns:= ifelse(!is.na(data$RET), ifelse(!is.na(data$DLRET), (1+data$RET)*(1+data$DLRET)-1, data$RET), data$DLRET)]
  Total_MV <- data[,list(Stock_lag_MV_Total = sum(Stock_lag_MV,na.rm=TRUE)), by = list(date)]
  data <- merge(data,Total_MV,by="date",all.x=T)
  data <- data[,weights:=Stock_lag_MV/Stock_lag_MV_Total]
  data1 <- data[,list(Stock_lag_MV = sum(Stock_lag_MV,na.rm=TRUE) ,Stock_Ew_Ret = mean(CumReturns,na.rm=TRUE),
                       Stock_Vw_Ret = sum(weights*CumReturns,na.rm = TRUE)), by = list(Year,Month)]
  data1 <- data1[data1$Year>=1926,]
  return(data1)
}

Monthly_CRSP_Stocks <- Stocks(CRSP_Stocks)
head(Monthly_CRSP_Stocks)
tail(Monthly_CRSP_Stocks)


Monthly_CRSP_Riskless <- read.csv('Treasurydata.csv',na.strings=c("","NA"))


Universe <- function(Stocks, Bonds, Riskless){
  Riskless$caldt<- as.Date(as.character(Riskless$caldt),"%Y%m%d")
  Riskless$Year <- format(Riskless$caldt, "%Y")
  Riskless$Month <- month(Riskless$caldt)
  data <- Stocks[,list(Year = Year, Month = Month,
                       Stock_lag_MV = Stock_lag_MV)]
  temp <- merge(Stocks,Riskless, by=c("Year","Month"))
  temp <- temp[,Stock_Excess_Vw_Ret:= Stock_Vw_Ret - t90ret]
  data <- data[,Stock_Excess_Vw_Ret:= temp$Stock_Excess_Vw_Ret]
  temp2 <- merge(Bonds,Riskless, by=c("Year","Month"))
  temp2 <- temp2[,Bond_Excess_Vw_Ret:= Bond_Vw_Ret - t30ret]
  data <- data[,Bond_lag_MV:=temp2$Bond_lag_MV]
  data <- data[,Bond_Excess_Vw_Ret:=temp2$Bond_Excess_Vw_Ret]
  return(data)
}

Monthly_CRSP_Universe <- Universe(Monthly_CRSP_Stocks,Monthly_CRSP_Bonds,Monthly_CRSP_Riskless)
head(Monthly_CRSP_Universe)
tail(Monthly_CRSP_Universe)


Port_returns<- function(data){
  data <- data[,Total_MV:= Stock_lag_MV + Bond_lag_MV]
  data <- data[,Excess_Vw_Ret:= ((Stock_Excess_Vw_Ret*Stock_lag_MV) + (Bond_lag_MV*Bond_Excess_Vw_Ret))/(Total_MV)]
  data$Excess_Vw_Ret[1] <- 0
  data <- data[,Excess_60_40_Ret:= 0.6*Stock_Excess_Vw_Ret + 0.4*Bond_Excess_Vw_Ret]
  Stock_sigma_hat <- rollapply(data$Stock_Excess_Vw_Ret, list(-seq(1:35)), sd)
  Stock_sigma_hat <- append(rep(NA,35), Stock_sigma_hat)
  data <- data[,Stock_inverse_sigma_hat:= 1/Stock_sigma_hat]
  Bond_sigma_hat <- rollapply(data$Bond_Excess_Vw_Ret, list(-seq(1:35)), sd)
  Bond_sigma_hat <- append(rep(NA,35), Bond_sigma_hat)
  data <- data[,Bond_inverse_sigma_hat:= 1/Bond_sigma_hat]
  data <- data[,Unlevered_k:= 1/(Stock_inverse_sigma_hat + Bond_inverse_sigma_hat)]
  data <- data[,Excess_Unlevered_RP_Ret:= Unlevered_k*Stock_inverse_sigma_hat*(Stock_Excess_Vw_Ret) + 
                Bond_inverse_sigma_hat*Unlevered_k*(Bond_Excess_Vw_Ret)]
  data$Excess_Unlevered_RP_Ret[1] <- NA
  levered_k = sqrt((sd(data$Excess_Vw_Ret))^2/(2*(1+cov(data$Stock_Excess_Vw_Ret,data$Bond_Excess_Vw_Ret)/
                                                    (sd(data$Stock_Excess_Vw_Ret)*sd(data$Bond_Excess_Vw_Ret)))))
  data <- data[,Levered_k:= levered_k]
  data <- data[,Excess_Levered_RP_Ret:= (Levered_k*Stock_inverse_sigma_hat*(Stock_Excess_Vw_Ret)) + 
                 Bond_inverse_sigma_hat*Levered_k*(Bond_Excess_Vw_Ret)]
  data2 <- data[,list(Stock_Excess_Vw_Ret = Stock_Excess_Vw_Ret, Bond_Excess_Vw_Ret = Bond_Excess_Vw_Ret,
                      Excess_Vw_Ret = Excess_Vw_Ret, Excess_60_40_Ret = Excess_60_40_Ret, Stock_inverse_sigma_hat = Stock_inverse_sigma_hat,
                      Bond_inverse_sigma_hat = Bond_inverse_sigma_hat, Unlevered_k = Unlevered_k, Excess_Unlevered_RP_Ret = Excess_Unlevered_RP_Ret,
                      Levered_k = Levered_k, Excess_Levered_RP_Ret = Excess_Levered_RP_Ret), by = list(Year,Month)]
  return(data2)
}


Port_Rets <- Port_returns(Monthly_CRSP_Universe)
head(Port_Rets)
tail(Port_Rets)


#Question 4

Comparative_Stats <- function(data){
  data1 <- data[data$Year %in% c(1930:2010)]
  Annualized_Mean <- rbind(mean(data1$Stock_Excess_Vw_Ret)*12, mean(data1$Bond_Excess_Vw_Ret)*12, mean(data1$Excess_Vw_Ret)*12, 
                           mean(data1$Excess_60_40_Ret)*12, mean(data1$Excess_Unlevered_RP_Ret)*12,mean(data1$Excess_Levered_RP_Ret)*12 )
  t_stat <- rbind(t.test(data1$Stock_Excess_Vw_Ret)$statistic[[1]], t.test(data1$Bond_Excess_Vw_Ret)$statistic[[1]], t.test(data1$Excess_Vw_Ret)$statistic[[1]], 
                  t.test(data1$Excess_60_40_Ret)$statistic[[1]], t.test(data1$Excess_Unlevered_RP_Ret)$statistic[[1]],t.test(data1$Excess_Levered_RP_Ret)$statistic[[1]])
  Annualized_Standard_Deviation <- rbind(sd(data1$Stock_Excess_Vw_Ret)*sqrt(12), sd(data1$Bond_Excess_Vw_Ret)*sqrt(12), sd(data1$Excess_Vw_Ret)*sqrt(12), 
                                         sd(data1$Excess_60_40_Ret)*sqrt(12), sd(data1$Excess_Unlevered_RP_Ret)*sqrt(12), sd(data1$Excess_Levered_RP_Ret)*sqrt(12))
  table1 <- as.data.frame(cbind(Annualized_Mean, t_stat, Annualized_Standard_Deviation))
  rownames(table1) <- c("CRSP stocks", "CRSP Bonds", "Value-weighted portfolio", "60/40 portfolio", "Unlevered RP", "Levered RP")
  colnames(table1) <- c("Annualized Mean", "t-stat of Annualized Mean", "Annualized Standard Deviation")
  table1$`Sharpe Ratio` <- table1$`Annualized Mean`/table1$`Annualized Standard Deviation`
  
  Skewness <- rbind(skewness(data1$Stock_Excess_Vw_Ret), skewness(data1$Bond_Excess_Vw_Ret), skewness(data1$Excess_Vw_Ret), 
                    skewness(data1$Excess_60_40_Ret), skewness(data1$Excess_Unlevered_RP_Ret), 
                    skewness(data1$Excess_Levered_RP_Ret))
  
  Excess_Kurtosis <- rbind(kurtosis(data1$Stock_Excess_Vw_Ret, method = "excess")[[1]], kurtosis(data1$Bond_Excess_Vw_Ret, method = "excess")[[1]],
                           kurtosis(data1$Excess_Vw_Ret, method = "excess")[[1]], 
                           kurtosis(data1$Excess_60_40_Ret, method = "excess")[[1]], kurtosis(data1$Excess_Unlevered_RP_Ret, method = "excess")[[1]], 
                           kurtosis(data1$Excess_Levered_RP_Ret, method = "excess")[[1]])
  
  table1 <- cbind(table1,Skewness,Excess_Kurtosis)
  table1$`Annualized Mean` <- table1$`Annualized Mean`*100
  return(table1)
}

Final_Output <- Comparative_Stats(Port_Rets)
Final_Output

