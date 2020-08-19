#-------------------------------------------------------------------------------------
# Does government-backed lending prevent unemployment? An assessment of the Swiss 
# Covid--19 lending program
#
# Daniel Kaufmann, daniel.kaufmann@unine.ch
#-------------------------------------------------------------------------------------
# Import the data
#-------------------------------------------------------------------------------------
# TODO
#-------------------------------------------------------------------------------------

# Packages and settings
rm(list = ls())
source("AllPackages.R")

#-------------------------------------------------------------------------------------
# Download the data
#-------------------------------------------------------------------------------------
# Zeitreihe
CreditTS     <- read.csv("../Data/Credit_Seco_update.csv", sep = ";", stringsAsFactors = FALSE)
CreditTS <- xts(CreditTS[,c(2, 3)], order.by = as.Date(CreditTS[,1], "%d.%m.%y"))
CreditTS <- data.frame(as.Date(index(CreditTS)), CreditTS)
colnames(CreditTS) <- c("Date", "NumberCredit", "CumCredit")

# Zeitreihe
FirmsTS     <- xlsx::read.xlsx("../Data/NumberFirms_update.xlsx", as.data.frame=T, sheetIndex = 1)
FirmsTS <- FirmsTS[,c(1,2,3)]
colnames(FirmsTS) <- c("Year", "NumberFirms", "NumberSME")

# Zeitreihe
CRFTS   <- xlsx::read.xlsx("../Data/SNB_CRF_update.xlsx", as.data.frame = T, startRow = 21, sheetIndex = 1)
CRFTS   <- data.frame(as.Date(paste(CRFTS[,1], "-01", sep = ""))+29, CRFTS$EPB.SNB.snbbipo.GD.*1000000)
colnames(CRFTS) <- c("Date", "VolumeCRF")

# BfS Berchnungen
CreditShare     <- xlsx::read.xlsx("../Data/BfS_Calculations.xlsx", sheetName = "Anteil", as.data.frame = TRUE, startRow = 7)
CreditShare <- CreditShare[1:26, 1:5]
colnames(CreditShare) <- c("Canton", "Share5mio", "Share200mio", "share500mio", "sharenon")

CreditRule <- xlsx::read.xlsx("../Data/BfS_Calculations.xlsx", sheetName = "Kredit", as.data.frame = TRUE, startRow = 10)
CreditRule <- CreditRule[c(1:26), 1:11]
colnames(CreditRule) <- c("Canton", "Base5mio", "Base5mioCV", "Base200mio",  "Base200mioCV", "Plus200mio", "Plus200mioCV", "Base500mio", "Base500mioCV", "Plus500mio", "Plus500mioCV")
CreditRule$Base <- rowSums(CreditRule[, c(2, 4, 8)], na.rm = TRUE)
CreditRule$Plus <- colSums(CreditRule[, c(6, 10)], na.rm = TRUE)

# Kurzarbeit Mai
ShortTime     <- xlsx::read.xlsx("../Data/ShortTimeWork_update.xlsx", sheetName = "ForImport", as.data.frame = TRUE, startRow = 4)
colnames(ShortTime) <- c("Canton", "KAFirms20", "KAHours20", "KAEmp20", "KAFirms19", "KAHours19", "KAEmp19")

# Canton productivity in 2019 (https://www.ubs.com/global/de/media/display-page-ndp/de-20190519-staf.html)
Compet     <- xlsx::read.xlsx("../Data/Competitiveness2019.xlsx", sheetName = "Sheet1", as.data.frame = TRUE, startRow = 1)
colnames(Compet) <- c("Canton", "Compet")

# Anteil geschlossener Betriebe
Closed     <- xlsx::read.xlsx("../Data/Firms_Covid19_update.xlsx", sheetName = "T1_ 11. MaiForImport", as.data.frame = TRUE, startRow = 3)
Closed <- Closed[c(-1), -2]
colnames(Closed) <- c("Canton", "NumberClosed", "ShareClosed")
Closed$ShareClosed <- Closed$ShareClosed/100

# Konkurse 2019
Default     <- xlsx::read.xlsx("../Data/Defaults_Cantons_update.xlsx", sheetName = "ForImport", as.data.frame = TRUE, startRow = 3)
Default <- Default[, c(1,2)]
colnames(Default) <- c("Canton", "Konkurs")

# Covid19 cases and incidence. Cumulative until 24.6.2020
Covid19     <- xlsx::read.xlsx("../Data/Covid19_Cantons.xlsx", sheetName = "COVID19 Kantone", as.data.frame = TRUE, startRow = 7)
colnames(Covid19) <- c("Canton", "Cases", "Incidence")

# Number of residents in Q1 2020 (to measure density mostly)
Pop     <- xlsx::read.xlsx("../Data/Inhabitants.xlsx", sheetName = "Q1_ForImport", as.data.frame = TRUE, startRow = 5)
PopQ1 <- Pop[, c(-3, -4, -6, -7, -9, -10, -12, -13)]
colnames(PopQ1) <- c("Canton", "Permanent", "Perm. Swiss", "Perm. Foreign", "Non-permanent")
PopQ1$PopTotal <- PopQ1$Permanent+PopQ1$`Non-permanent`

# Working age population (linear interpolation)
WorkPop     <- xlsx::read.xlsx("../Data/Inhabitants_Annual.xlsx", sheetName = "ForImport", as.data.frame = TRUE, startRow = 1)
WorkPop <- WorkPop[, c(1, 11)]
colnames(WorkPop) <- c("Canton", "Working age pop. (2020)")

# Number of firms
Total <- xlsx::read.xlsx("../Data/Firms_update.xlsx", sheetName = "ForImportTotal", as.data.frame = TRUE, startRow = 1)
SME   <- xlsx::read.xlsx("../Data/Firms_update.xlsx", sheetName = "ForImportSME", as.data.frame = TRUE, startRow = 1)
Firms <- data.frame(Total$Canton)
colnames(Firms) <- c("Canton")
Firms$ShareSME <- SME$X2020/Total$X2020
Firms$FirmsSME      <- SME$X2020
Firms$FirmsTotal    <- Total$X2020

# Covid-19 credits, cumulative, Updated on 25. Juni 2020, In CHF and number of contracts
Credit <- read.csv("../Data/Credit_Cantons.csv", sep = ";")
Credit <- Credit[, c(-5, -6)]
colnames(Credit) <- c("Canton", "Volume", "Number", "Average")

# Covid-19 credits, Updated on 25. Juni 2020
CreditFTE <- read.csv("../Data/Credit_FirmSize.csv", sep = ";")
CreditFTE <- data.frame(c(sum(CreditFTE$cred_sum), sum(CreditFTE$cred_sum[1:12]), sum(CreditFTE$cred_sum[13])), c(sum(CreditFTE$n), sum(CreditFTE$n[1:12]), sum(CreditFTE$n[13])))
colnames(CreditFTE) <- c("Volume", "Number")
rownames(CreditFTE) <- c("Total", "<250 FTE", ">=250 FTE")

# Updated on 10.08.2020
CreditType <- data.frame(c(135805, 134748, 1057), c(16668.5*10^6 , 13800*10^6, 2868.5*10^6))
colnames(CreditType) <- c("Number", "Volume")
rownames(CreditType) <- c("Total", "Covid-19", "Covid-19 Plus")

# Unemployment in May (because short-time work also for May)
Unemp <- xlsx::read.xlsx("../Data/Unemp_Seco_update.xlsx", sheetName = "ForImport", as.data.frame = TRUE, startRow = 4)
Unemp <- data.frame(Unemp[,1], Unemp[,4], Unemp[,16])
colnames(Unemp) <- c("Canton", "Unemployed (19)", "Unemployed (20)")

# Unemployment in May (because short-time work also for May)
CrossB <- xlsx::read.xlsx("../Data/CrossBorder.xlsx", sheetName = "ForImport", as.data.frame = TRUE, startRow = 1)
CrossB <- data.frame(CrossB[,1], CrossB[,2], CrossB[,3])
colnames(CrossB) <- c("Canton", "CrossB (19)", "CrossB (20)")

AllData <- full_join(Covid19, PopQ1, by = "Canton")
AllData <- full_join(AllData, WorkPop, by = "Canton")
AllData <- full_join(AllData, Firms, by = "Canton")
AllData <- full_join(AllData, Closed, by = "Canton")
AllData <- full_join(AllData, CrossB, by = "Canton")
AllData <- full_join(AllData, ShortTime, by = "Canton")
AllData <- full_join(AllData, Credit, by = "Canton")
AllData <- full_join(AllData, Unemp, by = "Canton")
AllData <- full_join(AllData, CreditShare, by = "Canton")
AllData <- full_join(AllData, CreditRule, by = "Canton")
AllData <- full_join(AllData, Default, by = "Canton")
AllData <- full_join(AllData, Compet, by = "Canton")

# Definition border canton
# https://www.newsd.admin.ch/newsd/message/attachments/60462.pdf
#Um den Vergleich zu vereinfachen, haben wir 15 Grenzkantone in der Analyse ber?cksichtigt: AG, BS, BL, JU, GE, GR, JU, NE, SG, SH, SO, TI, VD, VS und ZH. 
AllData$BorderC <- FALSE
AllData$BorderC[AllData$Canton %in% c("AG", "BS", "BL", "JU", "GE", "GR", "JU", "NE", "SG", "SH", "SH", "TI", "VD", "VS", "ZH")] <- TRUE

# save data for further use
save(list = c("AllData", "CreditTS", "CreditFTE", "CreditType", "CRFTS", "FirmsTS"), file = "../Data/AllData.RData")

