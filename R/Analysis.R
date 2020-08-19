#-------------------------------------------------------------------------------------
# Does government-backed lending prevent unemployment? An assessment of the Swiss 
# Covid--19 lending program
#
# Daniel Kaufmann, daniel.kaufmann@unine.ch
#-------------------------------------------------------------------------------------
# Analysis
#-------------------------------------------------------------------------------------
# TODO:
#-------------------------------------------------------------------------------------

# Packages and settings
rm(list = ls())
source("AllPackages.R")

#-------------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------------
load("../Data/AllData.Rdata")

# Put together FE data
FEData <- data.frame(c(AllData$Canton, AllData$Canton), c(rep(1, 26), rep(2, 26)))
colnames(FEData) <- c("Canton", "Time")
FEData$Unemp <- NA
FEData$Unemp[FEData$Time == 1] <- AllData$`Unemployed (19)`/AllData$`Working age pop. (2020)`
FEData$Unemp[FEData$Time == 2] <- AllData$`Unemployed (20)`/AllData$`Working age pop. (2020)`

FEData$ShortT <- NA
FEData$ShortT[FEData$Time == 1] <- AllData$KAEmp19/AllData$`Working age pop. (2020)`
FEData$ShortT[FEData$Time == 2] <- AllData$KAEmp20/AllData$`Working age pop. (2020)`

FEData$Volume <- NA
FEData$Volume[FEData$Time == 1] <- 0/AllData$`Working age pop. (2020)`/100000
FEData$Volume[FEData$Time == 2] <- AllData$Volume/AllData$`Working age pop. (2020)`/100000

FEData$InstSME <- NA
FEData$InstSME[FEData$Time == 1] <- 0/AllData$`Working age pop. (2020)`
FEData$InstSME[FEData$Time == 2] <- AllData$FirmsSME/AllData$`Working age pop. (2020)`

FEData$Cases <- NA
FEData$Cases[FEData$Time == 1] <- 0/AllData$`Working age pop. (2020)`
FEData$Cases[FEData$Time == 2] <- AllData$Cases/AllData$`Working age pop. (2020)`

FEData$Closed <- NA
FEData$Closed[FEData$Time == 1] <- 0/AllData$`Working age pop. (2020)`
FEData$Closed[FEData$Time == 2] <- AllData$NumberClosed/AllData$`Working age pop. (2020)`

FEData <- plm.data(FEData, index=c("Canton","Time"))

# Put together PC data
AllData$Rate       <- (AllData$`Unemployed (20)`)/AllData$`Working age pop. (2020)`
AllData$Rate19     <- (AllData$`Unemployed (19)`)/AllData$`Working age pop. (2020)`
AllData$VolumePC   <- AllData$Volume/AllData$`Working age pop. (2020)`/100000
AllData$NumberPC   <- AllData$Number/AllData$`Working age pop. (2020)`
AllData$ClosedPC   <- AllData$NumberClosed/AllData$`Working age pop. (2020)`
AllData$CasesPC    <- AllData$Cases/AllData$`Working age pop. (2020)`
AllData$KAEmpPC    <- (AllData$KAEmp20)/AllData$`Working age pop. (2020)`
AllData$NonPermPC  <- AllData$`Non-permanent`/AllData$`Working age pop. (2020)`
AllData$SMEPC      <- AllData$FirmsSME/AllData$`Working age pop. (2020)`
AllData$BasePC     <- AllData$Base/AllData$`Working age pop. (2020)`
AllData$BasePlusPC <- (AllData$Plus+AllData$Base)/AllData$`Working age pop. (2020)`
AllData$BaseShare  <- AllData$Share5mio+AllData$Share200mio
AllData$Base5mioPC <- AllData$Base5mio/AllData$`Working age pop. (2020)`
AllData$PlusPC     <- AllData$Plus/AllData$`Working age pop. (2020)`
AllData$Compet     <- AllData$Compet/100
AllData$BasePlusPC <- (AllData$Base+AllData$Plus)/AllData$`Working age pop. (2020)`
AllData$KonkursPC  <- AllData$Konkurs/AllData$`Working age pop. (2020)`

#-------------------------------------------------------------------------------------
# Descriptive stats
#-------------------------------------------------------------------------------------
# Compute share of volume covered by CRF
# Figures end of May (CRF) and June (Volume)
ShareCRF <- CRFTS$VolumeCRF[CRFTS$Date == "2020-06-30"]/sum(AllData$Volume)
print(ShareCRF)
print(CRFTS$VolumeCRF[CRFTS$Date == "2020-06-30"]/1000000000)

# Share of Base and Plus program
ShareType        <- CreditType
ShareType$Volume <- ShareType$Volume/ShareType$Volume[1]
ShareType$Number <- ShareType$Number/ShareType$Number[1]
ShareType$class  <- rownames(ShareType)
ShareType <- ShareType[-1,]
ShareType <- melt(ShareType, id= 'class')
print(ShareType)

# Number of firms
print(FirmsTS$NumberFirms[FirmsTS$Year==2020])

#-------------------------------------------------------------------------------------
# Charts
#-------------------------------------------------------------------------------------
# Figure 1a)
p <- ggplot(AllData, aes(x=VolumePC, y=Rate)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_text(label=AllData$Canton, vjust=1.5)+
  xlab("Loan volume (in 100,000 CHF, per working age pop.)")+ylab("Unemployment rate July 2020")
p <- ggLayout(p)
p
ggsave(filename = "../Results/ScatterUnempCredit.pdf", width = figwidth, height = figheight)

# Figure 1b)
p <- ggplot(AllData, aes(x=VolumePC, y=CasesPC)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_text(label=AllData$Canton, vjust=1.5)+
  xlab("Loan volume (in 100,000 CHF, per working age pop.)")+ylab("Covid-19 cases per working age pop.")
p <- ggLayout(p)
p
ggsave(filename = "../Results/ScatterCovidCredit.pdf", width = figwidth, height = figheight)

# Figure 2a)
CreditTS$`Share firms` <- CreditTS$CumCredit/FirmsTS$NumberFirms[FirmsTS$Year==2020]
Toplot <- CreditTS[,c(-2,-3)]
Toplot <- melt(Toplot, id= 'Date')
p <- ggplot(Toplot, aes(x=Date, y = value, color = variable)) + geom_line() +
  xlab("")+ylab("Cumulative share of firms with Covid-19 loan")
p <- ggLayout(p)
p <- ggColor2(p)
p <- p + theme(legend.position = "none")
p <- p + scale_x_date(labels =  date_format("%b %Y"))
p
ggsave(filename = "../Results/TimeNumberContracts.pdf", width = figwidth, height = figheight)

# Deutsche Version für Volkswirtschaft
Toplot$variable <- as.character(Toplot$variable)
Toplot[Toplot$variable=="Share SME", 2] <- "Anteil KMU"
Toplot[Toplot$variable=="Share firms", 2] <- "Anteil alle Firmen"

p <- ggplot(Toplot, aes(x=Date, y = value, color = variable)) + geom_line() +
  xlab("")+ylab("Kumulierter Anteil Firmen mit Covid-19 Kredit")
p <- ggLayout(p)
p <- ggColor3(p)
p <- p + theme(legend.position = "none")
p <- p + scale_x_date(labels =  date_format("%b %Y"))
p
ggsave(filename = "../Results/German/TimeNumberContracts_D.pdf", width = figwidth, height = figheight)
xlsx::write.xlsx(Toplot, "../Results/German/TimeNumberContracts_D.xlsx")

# Figure 2b)
ShareFTE        <- CreditFTE
ShareFTE$Volume <- ShareFTE$Volume/ShareFTE$Volume[1]
ShareFTE$Number <- ShareFTE$Number/ShareFTE$Number[1]
ShareFTE$class  <- rownames(ShareFTE)
ShareFTE <- ShareFTE[-1,]
ShareFTE <- melt(ShareFTE, id= 'class')
ShareFTE[ShareFTE$class=="<250 FTE", 1] <- "<250 full-time equivalents"
ShareFTE[ShareFTE$class==">=250 FTE",1 ] <- ">=250 full-time equivalents"

p <- ggplot(ShareFTE, aes(fill=class, y=value, x=variable)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Share of Covid-19 loans")
p <- ggLayout(p)
p <- p+ ggplot2::guides(fill=guide_legend(nrow=1,byrow=TRUE))
p
ggsave(filename = "../Results/BarShare.pdf", width = figwidth, height = figheight)

# German version for Die Volkswirtschaft
ShareFTE$variable <- as.character(ShareFTE$variable)
ShareFTE[ShareFTE$class=="<250 full-time equivalents", 1] <- "KMU"
ShareFTE[ShareFTE$class==">=250 full-time equivalents",1 ] <- "Grossunternehmen"
ShareFTE[ShareFTE$variable=="Volume", 2] <- "Volumen"
ShareFTE[ShareFTE$variable=="Number", 2] <- "Anzahl"
p <- ggplot(ShareFTE, aes(fill=class, y=value, x=variable)) + 
  geom_bar(position="fill", stat="identity")+
  xlab("")+ylab("Anteil Covid-19 Kredite")
p <- ggLayout(p)
p <- p+ ggplot2::guides(fill=guide_legend(nrow=1,byrow=TRUE))
p
ggsave(filename = "../Results/German/BarShare_D.pdf", width = figwidth, height = figheight)
xlsx::write.xlsx(ShareFTE, "../Results/German/BarShare_D.xlsx")

# Figure 3a)
p <- ggplot(AllData, aes(x=SMEPC, y=VolumePC)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_text(label=AllData$Canton, vjust=1.5)+
  ylab("Loan volume (in 100,000 CHF, per working age pop.)")+xlab("Number of SME (per working age pop.)")
p <- ggLayout(p)
p
ggsave(filename = "../Results/ScatterInstrumentSME.pdf", width = figwidth, height = figheight)

# Figure 3b)
p <- ggplot(AllData, aes(x=BasePC/100, y=VolumePC)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  geom_text(label=AllData$Canton, vjust=1.5)+
  ylab("Loan volume (in 100,000 CHF, per working age pop.)")+xlab("Max. loan supply (in 100,000, per working age pop.)")
p <- ggLayout(p)
p
ggsave(filename = "../Results/ScatterInstrumentBase.pdf", width = figwidth, height = figheight)

#-------------------------------------------------------------------------------------
# Do Main Regressions
#-------------------------------------------------------------------------------------
# 1) OLS Raw correlation
Reg1 <- lm(Rate~VolumePC, data = AllData)
coeftest(Reg1, df = Inf, vcov = vcovHC(Reg1, type = "HC1"))
Vc1 <- sqrt(diag(vcovHC(Reg1, type = "HC1")))

# 3) OLS controlling for all
Reg3 <- lm(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, data = AllData)
coeftest(Reg3, df = Inf, vcov = vcovHC(Reg3, type = "HC1"))
Vc3 <- sqrt(diag(vcovHC(Reg3, type = "HC1")))

# 6) IV controlling for all
IV1 <- ivreg(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19 | SMEPC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, data = AllData)
summary(IV1, diagnostics = T)
IVstat1 <- summary(IV1, diagnostics = T)$diagnostics[c(1,2),c(3,4)]
coeftest(IV1, df = Inf, vcov = vcovHC(IV1, type = "HC1"))
VcIV1 <- sqrt(diag(vcovHC(IV1, type = "HC1")))

IV3 <- ivreg(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19 | BasePC+ClosedPC+CasesPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, data = AllData)
summary(IV3, diagnostics = T)
IVstat3 <- summary(IV3, diagnostics = T)$diagnostics[c(1,2),c(3,4)]
coeftest(IV3, df = Inf, vcov = vcovHC(IV3, type = "HC1"))
VcIV3 <- sqrt(diag(vcovHC(IV3, type = "HC1")))

# All instruments
IV4 <- ivreg(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19 | SMEPC+BasePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, data = AllData)
summary(IV4, diagnostics = T)
IVstat4 <- summary(IV4, diagnostics = T)$diagnostics[c(1,2,3),c(3,4)]
coeftest(IV4, df = Inf, vcov = vcovHC(IV4, type = "HC1"))
VcIV4 <- sqrt(diag(vcovHC(IV4, type = "HC1")))

outputFile <- "../Results/RegResults.tex"
stargazer(Reg1, Reg3,  IV1,  IV3, IV4,  type="latex", out= outputFile, label= "tab:Results", 
          se=list(Vc1, Vc3, VcIV1,  VcIV3, VcIV4),
          model.names = F,
          column.labels = c("OLS", "OLS", "IV", "IV", "IV"),
          style = "qje", digits = 2, font.size = "footnotesize", omit.stat=c("LL","ser","f"), 
          add.lines = list(c("Instrument", "", "", c("Share SME", "Max. supply", "Both")),
                           c("{Irrelevant instrument (p-val.)}", "", "",  format(round(c(IVstat1[2,1], IVstat3[2,1], IVstat4[2,1]),2), nsmall=2,digits=2,scientific=F)),
                           c("", "", "",                                  format(round(c(IVstat1[2,2], IVstat3[2,2], IVstat4[2,2]),2), nsmall=2,digits=2,scientific=F)),
                           c("{Weak instrument (p-val.)}", "", "",        format(round(c(IVstat1[1,1], IVstat3[1,1], IVstat4[1,1]),2), nsmall=2,digits=2,scientific=F)),
                           c("", "", "",                                  format(round(c(IVstat1[1,2], IVstat3[1,2], IVstat4[1,2]),2), nsmall=2,digits=2,scientific=F)),
                           c("{Overidentifying restrictions (p-val.)}",  "", "", "", "", format(round(c(IVstat4[3,1]),2), nsmall=2,digits=2,scientific=F)),
                            c("", "", "", "", "",                         format(round(c(IVstat4[3,2]),2),nsmall=2,digits=2,scientific=F))),
          dep.var.labels = c("Dependent variable: Unemployment rate July 2020"),model.numbers = T,
          notes        = c("Heteroskedasticity-consistent standard errors in parentheses.", "All variables normalized by the working age population."), 
          title = "Effect of loan volume on unemployment rate",
          covariate.labels = c("\\multirow{2}{4cm}{Volume (Until June, in CHF 100,000)}", 
                               "\\multirow{2}{4cm}{Covid-19 cases (up to June)}",
                               "\\multirow{2}{4cm}{Firms in lockdown (May)}",
                               "\\multirow{2}{4cm}{Non-permanent residents (Q1)}",
                               "\\multirow{2}{4cm}{Employees on short-time work (May)}",
                               "\\multirow{2}{4cm}{New default proceedings (2019)}",
                               "\\multirow{2}{4cm}{Competitiveness (0-1, 2019)}",
                               "\\multirow{2}{4cm}{Unemployment rate (July 2019)}"))

#-------------------------------------------------------------------------------------
# Do FE Regressions
#-------------------------------------------------------------------------------------
FE1 <- plm(Unemp ~ Volume, data=FEData, model = "within", effect = "twoways")
coeftest(FE1, df = Inf, vcov = vcovHC(FE1, type = "HC1"))
VcFE1 <- sqrt(diag(vcovHC(FE1, type = "HC1")))

FE2 <- plm(Unemp ~ Volume+Cases+ShortT+Closed, data=FEData, model = "within", effect = "twoways")
coeftest(FE2, df = Inf, vcov = vcovHC(FE2, type = "HC1"))
VcFE2 <- sqrt(diag(vcovHC(FE2, type = "HC1")))

FE3 <- plm(Unemp ~ Volume+Cases+ShortT+Closed | InstSME+Cases+ShortT+Closed, data=FEData, model = "within", effect = "twoways")
coeftest(FE3, df = Inf, vcov = vcovHC(FE3, type = "HC1"))
VcFE3 <- sqrt(diag(vcovHC(FE3, type = "HC1")))

FE4 <- plm(Unemp ~ Volume+Cases, data=FEData, model = "within", effect = "twoways")
coeftest(FE4, df = Inf, vcov = vcovHC(FE4, type = "HC1"))
VcFE4 <- sqrt(diag(vcovHC(FE4, type = "HC1")))

FE5 <- plm(Unemp ~ Volume+Cases | InstSME+Cases, data=FEData, model = "within", effect = "twoways")
coeftest(FE5, df = Inf, vcov = vcovHC(FE5, type = "HC1"))
VcFE5 <- sqrt(diag(vcovHC(FE5, type = "HC1")))

outputFile <- "../Results/FEResults.tex"
stargazer(FE1, FE2,  FE3, FE4, FE5,  type="latex", out= outputFile, label= "tab:FEResults", 
          se=list(VcFE1, VcFE2, VcFE3, VcFE4, VcFE5),
          model.names = F,
          column.labels = c("OLS", "OLS",  "IV", "OLS",  "IV"),
          style = "qje", digits = 2, font.size = "footnotesize", omit.stat=c("LL","ser","f"), 
          dep.var.labels = c("Dependent variable: Unemployment rate July"),model.numbers = T,
          add.lines = list(c("Instrument", "", "", c("Share SME"), "", "Share SME")),
          notes        = c("Heteroskedasticity-consistent standard errors in parentheses.", "All variables normalized by the working age population.", "All specifications include time and fixed effects."), 
          title = "Effect of loan volume on unemployment rate (panel data)",
          covariate.labels = c("\\multirow{2}{4cm}{Volume (Until June, in CHF 100,000)}", 
                               "\\multirow{2}{4cm}{Covid-19 cases (up to June)}",
                               "\\multirow{2}{4cm}{Employees on short-time work (May)}",
                               "\\multirow{2}{4cm}{Firms in lockdown (May)}"))



# 3) OLS controlling for all
Reg3 <- lm(Rate~VolumePC+NumberPC+CasesPC, data = AllData)
coeftest(Reg3, df = Inf, vcov = vcovHC(Reg3, type = "HC1"))
Vc3 <- sqrt(diag(vcovHC(Reg3, type = "HC1")))


sdf
#-------------------------------------------------------------------------------------
# Do Outlier analysis, remove one Canton at a time with four approaches, and outl. robust OLS
#-------------------------------------------------------------------------------------
Estimates  <- array(NA, c(4, 26))
colnames(Estimates) <- AllData$Canton
rownames(Estimates) <- c("OLS", "IV", "POLS", "PIV")
tStatistic <- Estimates

for(cant in AllData$Canton) {
  
  # OLS Estimate
  OLS   <- lm(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, data = subset(AllData, Canton != cant))
  OLSt  <- coeftest(OLS, df = Inf, vcov = vcovHC(OLS, type = "HC1"))[2, 3]
  
  # IV Estimate
  IV    <- ivreg(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19 | SMEPC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, data = subset(AllData, Canton != cant))
  IVt   <- coeftest(IV, df = Inf, vcov = vcovHC(IV, type = "HC1"))[2, 3]
  
  # Panel OLS Estimate
  POLS  <- plm(Unemp ~ Volume+Cases+ShortT, data=subset(FEData, Canton != cant), model = "within", effect = "twoways")
  POLSt <- coeftest(POLS, df = Inf, vcov = vcovHC(POLS, type = "HC1"))[1, 3]
  
  # Panel IV Estiamte
  PIV   <- plm(Unemp ~ Volume+Cases+ShortT | InstSME+Cases+ShortT, data=subset(FEData, Canton != cant), model = "within", effect = "twoways")
  PIVt <- coeftest(PIV, df = Inf, vcov = vcovHC(PIV, type = "HC1"))[1, 3]
  
  # Save results
  Estimates["OLS", cant]  <- OLS$coefficients[2]
  Estimates["IV", cant]   <- IV$coefficients[2]
  Estimates["POLS", cant] <- POLS$coefficients[1]
  Estimates["PIV", cant]  <- PIV$coefficients[1]
  
  tStatistic["OLS", cant]  <- OLSt
  tStatistic["IV", cant]   <- IVt
  tStatistic["POLS", cant] <- POLSt
  tStatistic["PIV", cant]  <- PIVt
  
}

ShareSig <- rowMeans(tStatistic < -1.96)
MeanCoef <- rowMeans(Estimates)

Table <- rbind(MeanCoef, ShareSig)
print(Table)

# Figure 4a)
toplot <- data.frame(t(Estimates))
p1 <- ggplot(toplot, aes(x=OLS)) + 
  geom_histogram() + xlab("Coefficient") + ylab("Number")+ggtitle("OLS")
p1 <- ggLayout(p1)
p1

p2 <- ggplot(toplot, aes(x=IV)) + 
  geom_histogram() + xlab("Coefficient") + ylab("Number")+ggtitle("IV")
p2 <- ggLayout(p2)
p2

p3 <- ggplot(toplot, aes(x=POLS)) + 
  geom_histogram() + xlab("Coefficient") + ylab("Number")+ggtitle("Panel OLS")
p3 <- ggLayout(p3)
p3

p4 <- ggplot(toplot, aes(x=PIV)) + 
  geom_histogram() + xlab("Coefficient") + ylab("Number")+ggtitle("Panel IV")
p4 <- ggLayout(p4)
p4

Combined <- ggarrange(p1, p2, p3, p4, 
                      ncol = 2, nrow = 2)
Combined
ggsave(filename = "../Results/CoeffOutlier.pdf", width = figwidth, height = figheight)


# Figure 4b)
toplot <- data.frame(t(tStatistic))
p1 <- ggplot(toplot, aes(x=OLS)) + 
  geom_histogram() + xlab("t-statistic") + ylab("Number")+ggtitle("OLS")
p1 <- ggLayout(p1)
p1

p2 <- ggplot(toplot, aes(x=IV)) + 
  geom_histogram() + xlab("t-statistic") + ylab("Number")+ggtitle("IV")
p2 <- ggLayout(p2)
p2

p3 <- ggplot(toplot, aes(x=POLS)) + 
  geom_histogram() + xlab("t-statistic") + ylab("Number")+ggtitle("Panel OLS")
p3 <- ggLayout(p3)
p3

p4 <- ggplot(toplot, aes(x=PIV)) + 
  geom_histogram() + xlab("t-statistic") + ylab("Number")+ggtitle("Panel IV")
p4 <- ggLayout(p4)
p4

Combined <- ggarrange(p1, p2, p3, p4, 
          ncol = 2, nrow = 2)
Combined
ggsave(filename = "../Results/tStatsOutlier.pdf", width = figwidth, height = figheight)

# Outlier robust OLS estimate
# OLS Estimate
rOLS1      <- rlm(Rate~VolumePC, method = "MM", data = AllData)
rOLStest1  <- coeftest(rOLS1, df = Inf, vcov = vcovHC(rOLS1, type = "HC1"))
print(rOLStest1)

rOLS2      <- rlm(Rate~VolumePC+CasesPC+ClosedPC+NonPermPC+KAEmpPC+KonkursPC+Compet+Rate19, method = "MM", data = AllData)
rOLStest2  <- coeftest(rOLS2, df = Inf, vcov = vcovHC(rOLS2, type = "HC1"))
print(rOLStest2)


