#-------------------------------------------------------------------------------------
# Does government-backed lending prevent unemployment? An assessment of the Swiss 
# Covid--19 lending program
#
# Daniel Kaufmann, daniel.kaufmann@unine.ch
#-------------------------------------------------------------------------------------
# Packages and functions
#-------------------------------------------------------------------------------------
# TODO
#-------------------------------------------------------------------------------------

library(tsbox)
library(ggplot2)
library(xts)
library(ggpubr)
library(lubridate)
library(xlsx)
library(quantmod)
library(XLConnect)
library(matlib)
library(reshape2)
library(scales)
library(stringr)
library(dplyr)   
library(sandwich)
library(lmtest)
library(AER)
library(stargazer)
library(plm)
library(MASS)
library(ggpubr)
library(multiwayvcov)

# SEttings for all scripts
figwidth  <- 6
figheight <- 5

normalize <- function(x){
  x_norm = (x-mean(x, na.rm =TRUE))/sd(x, na.rm =TRUE)
  return(x_norm)
}

# Make output directory forder
makeOutDir <- function(mainDir, outDir){
  
  if (file.exists(outDir)){
    setwd(file.path(mainDir, outDir))
  } else {
    dir.create(file.path(mainDir, outDir))
  }
  return(paste(mainDir, outDir , sep="")) # combines the stringe mainDir and outDir with seperation "" (i.e. w/o any separation)
}


L.op <- function(ts, p) {
  # Lead/lag operator
  myDate <- index(ts)
  if(p<0){
    ts <- xts(dplyr::lead(as.matrix(ts), abs(p)), order.by=myDate)
  }
  else{
    ts <- xts(dplyr::lag(as.matrix(ts), abs(p)), order.by=myDate)
  }
  return(ts)
}

ggLayout <- function(p) {
  p <- p + theme_minimal() +
    ggplot2::scale_color_brewer(palette = "Dark2")+
    theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(fill=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
    theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
    theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
    theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
  return(p)
}

ggColor2 <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
    scale_color_manual(values = c("firebrick4", "blue4"))+ 
    scale_alpha_manual(values = c(0.5, 0.5))
  return(p)
}


ggColor3 <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
    scale_color_manual(values = c("tomato1", "firebrick4", "blue4"))+ 
    scale_alpha_manual(values = c(0.5, 0.5, 1))
  return(p)
}

ggColorMany <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=1) + scale_color_grey(start = 0.8, end = 0.2) +
           theme(legend.position = "none")
  return(p)
}

addLines <- function(p, myLines, myLabels, yInter, hor){
  if(missing(hor)) {
    hor = "vert" 
  }
    
  myLines <- as.Date(myLines)
  p <- p + geom_vline(xintercept=myLines , colour="black", size = .5, alpha = 0.5) 
  for(i in 1:length(myLines)){
    
    if(hor == "hor"){
      p <- p + ggplot2::annotate(geom="text", x=myLines[i], y=yInter, label=myLabels[i], color="black",  alpha = 0.7, hjust = -.3)
    }
    else{
      p <- p + ggplot2::annotate(geom="text", x=myLines[i], y=yInter, label=myLabels[i], color="black",  angle=90, alpha = 0.7, vjust = -1, hjust = 0)
    }
    
    
  }
  return(p)
}

addCorr <- function(p, Corr, xInter, yInter){
  p <- p + ggplot2::annotate(geom="text", x=as.Date(xInter), y=yInter, label=paste("Correlation: ", round(Corr, 2), sep = ""), vjust=0, hjust = -.2, color="black")
  return(p)
}




