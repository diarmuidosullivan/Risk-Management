SPX <- read.csv("/Users/diarmuidosullivan/Downloads/^SPX.csv")
GFL <- read.csv("/Users/diarmuidosullivan/Downloads/GFL.csv")
NLOP <- read.csv("/Users/diarmuidosullivan/Downloads/NLOP.csv")
IWGFF <- read.csv("/Users/diarmuidosullivan/Downloads/IWGFF.csv")
XPEL <- read.csv("/Users/diarmuidosullivan/Downloads/XPEL.csv")
BRK <- read.csv("/Users/diarmuidosullivan/Downloads/BRK-B.csv")
SENEA <- read.csv("/Users/diarmuidosullivan/Downloads/SENEA.csv")
FCFS <- read.csv("/Users/diarmuidosullivan/Downloads/FCFS.csv")
DAKT <- read.csv("/Users/diarmuidosullivan/Downloads/DAKT.csv")
CROX <- read.csv("/Users/diarmuidosullivan/Downloads/CROX.csv")
IDT <- read.csv("/Users/diarmuidosullivan/Downloads/IDT.csv")
#I am using a swedinsh index as a benchmark for EG7
EG7 <- read.csv("/Users/diarmuidosullivan/Downloads/EG7.ST.csv")
OMXSBPI <-read.csv("/Users/diarmuidosullivan/Downloads/^OMXSBPI.csv")
#I am using a greek index for SPSIL
EPSIL <- read.csv("/Users/diarmuidosullivan/Downloads/EPSIL.AT.csv")
ATH <- read.csv("/Users/diarmuidosullivan/Downloads/ATH.csv")

#Closing Prices
SPX_close <- SPX$Adj.Close
GFL_close <- GFL $Adj.Close
NLOP_close <- NLOP$Adj.Close
IWGFF_close <- IWGFF$Adj.Close
XPEL_close <- XPEL$Adj.Close
EPSIL_close <- EPSIL$Adj.Close
ATH_close_old <- ATH$Close
ATH_close <- as.numeric(gsub(",","",ATH_close_old))
BRK_close <- BRK$Adj.Close
SENEA_close <- SENEA$Adj.Close
FCFS_close <- FCFS$Adj.Close
DAKT_close <- DAKT$Adj.Close
CROX_close <- CROX$Adj.Close
IDT_close <- IDT$Adj.Close
EG7_close <- EG7$Adj.Close
OMXSBPI_close <- OMXSBPI$Adj.Close

elements <- strsplit(ATH_close, " ")[[1]]

vector_of_doubles <- as.numeric(elements)


#Find the log returns
SPX_Log_Returns <- c()
for (i in 1:(length(SPX_close)-1)){
  temp = log(SPX_close[i+1]/SPX_close[i])
  SPX_Log_Returns = c(SPX_Log_Returns, temp)
}

GFL_Log_Returns <- c()
for (i in 1:(length(GFL_close)-1)){
  temp = log(GFL_close[i+1]/GFL_close[i])
  GFL_Log_Returns = c(GFL_Log_Returns, temp)
}

NLOP_Log_Returns <- c()
for (i in 1:(length(NLOP_close)-1)){
  temp = log(NLOP_close[i+1]/NLOP_close[i])
  NLOP_Log_Returns = c(NLOP_Log_Returns, temp)
}

IWGFF_Log_Returns <- c()
for (i in 1:(length(IWGFF_close)-1)){
  temp = log(IWGFF_close[i+1]/IWGFF_close[i])
  IWGFF_Log_Returns = c(IWGFF_Log_Returns, temp)
}

XPEL_Log_Returns <- c()
for (i in 1:(length(XPEL_close)-1)){
  temp = log(XPEL_close[i+1]/XPEL_close[i])
  XPEL_Log_Returns = c(XPEL_Log_Returns, temp)
}

EPSIL_Log_Returns <- c()
for (i in 1:(length(EPSIL_close)-1)){
  temp = log(EPSIL_close[i+1]/EPSIL_close[i])
  EPSIL_Log_Returns = c(EPSIL_Log_Returns, temp)
}
ATH_Log_Returns <- c()
for (i in 1:(length(ATH_close)-1)){
  temp = log(ATH_close[i+1]/ATH_close[i])
  ATH_Log_Returns = c(ATH_Log_Returns, temp)
}

BRK_Log_Returns <- c()
for (i in 1:(length(BRK_close)-1)){
  temp = log(BRK_close[i+1]/BRK_close[i])
  BRK_Log_Returns = c(BRK_Log_Returns, temp)
}

SENEA_Log_Returns <- c()
for (i in 1:(length(SENEA_close)-1)){
  temp = log(SENEA_close[i+1]/SENEA_close[i])
  SENEA_Log_Returns = c(SENEA_Log_Returns, temp)
}

FCFS_Log_Returns <- c()
for (i in 1:(length(FCFS_close)-1)){
  temp = log(FCFS_close[i+1]/FCFS_close[i])
  FCFS_Log_Returns = c(FCFS_Log_Returns, temp)
}


DAKT_Log_Returns <- c()
for (i in 1:(length(DAKT_close)-1)){
  temp = log(DAKT_close[i+1]/DAKT_close[i])
  DAKT_Log_Returns = c(DAKT_Log_Returns, temp)
}

CROX_Log_Returns <- c()
for (i in 1:(length(CROX_close)-1)){
  temp = log(CROX_close[i+1]/CROX_close[i])
  CROX_Log_Returns = c(CROX_Log_Returns, temp)
}

IDT_Log_Returns <- c()
for (i in 1:(length(IDT_close)-1)){
  temp = log(IDT_close[i+1]/IDT_close[i])
  IDT_Log_Returns = c(IDT_Log_Returns, temp)
}

EG7_Log_Returns <- c()
for (i in 1:(length(EG7_close)-1)){
  temp = log(EG7_close[i+1]/EG7_close[i])
  EG7_Log_Returns = c(EG7_Log_Returns, temp)
}

OMXSBPI_Log_Returns <- c()
for (i in 1:(length(OMXSBPI_close)-1)){
  temp = log(OMXSBPI_close[i+1]/OMXSBPI_close[i])
  OMXSBPI_Log_Returns = c(OMXSBPI_Log_Returns, temp)
}


gen_log_retuns <- data.frame(SPX_Log_Returns, GFL_Log_Returns, IWGFF_Log_Returns, XPEL_Log_Returns, BRK_Log_Returns, SENEA_Log_Returns, FCFS_Log_Returns, DAKT_Log_Returns, CROX_Log_Returns, IDT_Log_Returns)
EG7_log_retuns.df <- data.frame(OMXSBPI_Log_Returns, EG7_Log_Returns)
NLOP_log_returns.df <- data.frame(SPX_Log_Returns[156:250], NLOP_Log_Returns)
EPSIL_log_returns.df <- data.frame(EPSIL_Log_Returns, ATH_Log_Returns)

write.csv(gen_log_retuns,  file = "/Users/diarmuidosullivan/Downloads/gen_log_retuns.csv", col.names = T)
write.csv(EG7_log_retuns.df,  file = "/Users/diarmuidosullivan/Downloads/EG7_log_retuns_compare.csv", col.names = T)
write.csv(NLOP_log_returns.df,  file = "/Users/diarmuidosullivan/Downloads/NLOP_log_returns.csv", col.names = T)
write.csv(EPSIL_log_returns.df,  file = "/Users/diarmuidosullivan/Downloads/EPSIL_log_returns.csv", col.names = T)

GFL_estimate <- lm(gen_log_retuns$GFL_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(GFL_estimate)

EPSIL_estimate <- lm(EPSIL_log_returns.df$EPSIL_Log_Returns~EPSIL_log_returns.df$ATH_Log_Returns)
summary(EPSIL_estimate)

IDT_estimate <- lm(gen_log_retuns$IDT_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(IDT_estimate)

IWGFF_estimate <- lm(gen_log_retuns$IWGFF_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(IWGFF_estimate)

XPEL_estimate <- lm(gen_log_retuns$XPEL_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(XPEL_estimate)

BRK_estimate <- lm(gen_log_retuns$BRK_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(BRK_estimate)

SENEA_estimate <- lm(gen_log_retuns$SENEA_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(SENEA_estimate)

FCFS_estimate <- lm(gen_log_retuns$FCFS_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(FCFS_estimate)

DAKT_estimate <- lm(gen_log_retuns$DAKT_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(DAKT_estimate)

CROX_estimate <- lm(gen_log_retuns$CROX_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(CROX_estimate)

EG7_estimate <- lm(EG7_log_retuns.df$EG7_Log_Returns~EG7_log_retuns.df$OMXSBPI_Log_Returns)
summary(EG7_estimate)

NLOP_estimate <- lm(NLOP_log_returns.df$NLOP_Log_Returns~NLOP_log_returns.df$SPX_Log_Returns.156.250.)
summary(NLOP_estimate)

EPSN_estimate <- lm(gen_log_retuns$EPSN_Log_Returns~gen_log_retuns$SPX_Log_Returns)
summary(EPSN_estimate)
