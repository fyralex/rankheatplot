#########       COPD       #########
library(fields)
library(RColorBrewer)
library(circlize)
treatments<-c("ACLI", "AZD3199", "BECL/FORM", "BUDE", "FLUT", "FLUT/TIOT", "FORM", "FORM/BUDE", "FORM/BUDE/TIOT", "FORM/MOME", "FORM/TIOT", "GLYC", "GSK961081", "INDA", "INDA/GLYC", "INDA/TIOT", "MOME", "Placebo", "SALM", "SALM/FLUT", "SALM/TIOT", "TIOT","TIOT/BUDE/FORM","TIOT/FLUT/SALM","TIOT+Resp","TRIAM","UMEC","VILA", "VILA/FLUT", "VILA/UMEC")
arrhythmia<- c(62.8, NA, NA, 39.1, NA, NA,46.9, 20.8,NA,NA,NA, 45.8,NA, 48.6, 40.9 ,NA , NA, 74.3 ,69.4, 74.8,NA,62.5, NA, NA, NA, NA , NA, NA ,  14.1, NA)
pneumonia<-c(77.4, NA,   46.3, 74.6, 18.4, NA,   61.8, 34.7, NA,   60.5, NA ,  70.2, NA ,  55.8 ,66.5, NA,   40,   52.8 ,NA ,  11.2,41.8, 59 ,  54.8, 33.7 ,NA ,  NA ,  62.7, 45.1, 11.5, 71.3)
cvm<-c(28.2, 70.2 ,NA  , 49.4, 61.4, NA,   58.5 ,27  , NA ,  NA ,NA ,  83.4, NA ,  63.4, 77 ,  32.3 ,NA ,  50.5, 75.9, 65.3,NA,   40.2, NA ,  NA ,  32,   19.2, 48.8, 35.5, 54.6 ,27.4)
mortality<-c(66.9, 67.7, 60.8, 64.9, 41.2, 52.8, 28.2, 43.1, 26.9, 66.8,63.6, 70.2, NA,   64.2 ,60.5, 46.6, 29.7 ,44.7, 58.2 ,70.6,30.3 ,50.5 ,NA  , 28.5, 49.8, 64.7, 44.6, 27.9, 39.2, 37.1)
exacerbations<-c(33.8, 67.7, 66.3, 40.7, 36.6 ,NA ,  57.6 ,81.3, NA ,  51.8,35.2, 44.5, 79.9, 32.1, 70.2, 77.5, 46.6 ,13.7 ,37.3, 44.1,48.8, 63.4, 96.2, 72.9, NA ,  NA ,  16.5 ,16.8, 51,17.4)
mydata<-data.frame(treatments, exacerbations, cvm, mortality, pneumonia, arrhythmia)
rankheatplot(data=mydata,format="percentage",lab.plot="numbers", color="color.blind", title.name="COPD", cex=0.65, legend.treatment="TRUE", pos.outcome.label=c(0.24,-0.45), pos.treatment.label=c(1,-0.45), asterisk="TRUE",show.numbers="false")
#######################################################################
########       5-HT3 surgery        #########
library(fields)
library(RColorBrewer)
library(circlize)
#SUCRA values
treatments<-c("Placebo", "Ondansetron","Granisetron", "Dolasetron","Tropisetron", "Ondansetron+Dexamethasone", "Palonosetron","Ramosetron", "Ondansetron+DroperidolIV", "Ondansetron+MetoclopramideIV", "Granisetron+Dexamethasone", "Palonosetron+Dexamethasone", "Dolasetron+Dexamethasone", "Dolasetron+DroperidolIV", "Granisetron+DroperidolIV")
arrhythmia<- c(42.8, 54.6, 40.7, 81.5, 54.6, 83.3, NA, 35.3, NA, NA, 2.9, NA, NA, 54.2, NA)
delirium<-c(51.9, 64.9, 93.5, 27.7, 35.3, NA, NA, NA, NA, NA, NA, NA, 26.6, NA, NA)
nausea<-c(0.8, 25.4, 44.8, 13.3, 23.2, 71.9, 54.9, 51.3, 65.4, 52.8, 81.2, 43.7, 72.8, 76.8, 71.7)
vomiting<-c(4.6, 29.4, 54.3, 22, 42.2, 76, 30.1, 48.4, 84.6, 78.8, 84.3, 7.8, 73.9, 68.8, 44.8)
ponv<-c(0.1, 20.1, 42.5, 37.7, 16.3, 63, 72.3, 34.9, 76.9, NA, 86.3, 67, NA, 71.9, 61.1)
mydata<-data.frame(treatments,vomiting, nausea, ponv, delirium, arrhythmia)
vector<-c("PLAC","ONDA","GRAN","DOLA", "TROP","ONDA+DEX","PALO", "RAMO", "ONDA+DROP", "ONDA+MET", "GRAN+DEX", "PALO+DEX", "DOLA+DEX", "DOLA+DROP", "GRAN+DROP") 
rankheatplot(data=mydata,format="percentage",lab.plot="numbers", color="color.blind", title.name="5-HT3 surgery", cex=0.65, legend.treatment="TRUE", pos.outcome.label=c(0.2,-0.45), pos.treatment.label=c(0.9,-0.45), asterisk="TRUE",show.numbers="TRUE")
#######################################################################
