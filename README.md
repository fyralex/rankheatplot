# Rankheat Plot
The rank-heat plot is a visual representation of the treatment hierarchy estimated in a network meta-analysis across multiple outcomes. Several ranking statistics can be displayed within a rank-heat plot, including the probability of being the best, mean/median ranking, and the surface under the cumulative ranking curve (SUCRA). 

See more at https://rh.ktss.ca/

#### Prerequisites

<b>You need to download and install the R packages ‘fields’, ‘RColorBrewer’, ‘circlize’ </b>*


## Usage
`rankheatplot (data, format, lab.plot, color, title.name, legend.treatment="TRUE", cex=0.65, pos.outcome.label=c(0.01,-0.4), pos.treatment.label=c(-0.25,-0.4), asterisk="TRUE")`

###### REQUIRED ARGUMENTS
**data:**   Data-frame with rows corresponding to the treatments included in the network meta-analysis and columns corresponding to the studied outcomes. The first row should present the names of the outcomes, and the first column should present the names of the treatments.
Optional arguments

**format:**   Ranking statistics, with "percentage" (with values ranging between 0 and 100) (e.g., 54) referring to SUCRA values or the probability of being the best, and "rank" (e.g., 5.4) referring to median or mean rank of the included treatments (default is "percentage").	

**lab.plot:** Labels referred to the included treatments across the sectors of circles. Should be one of: 
- **numbers**, for numerical values
- **vector**, for titles given by the user
- **default.titles**, the first column of the dataset (names of the treatments) (default)

**color:**  Color palette of the plot, with options:
- **color**, the scale consists of the transformation of three colors red (0%), yellow (50%), and green (100%) (default)
- **grey**, a dark grey corresponds to the smallest ranking value (0%) and a light grey corresponds to the highest ranking value (100%).
- **color.blind**, the scale consists of the transformation of three colors red (0%), yellow (50%), and green (100%) associated with a specific pattern. 

**title.name:**  Title of the plot

**cex:**    Font size for lab.plot (default is 0.65)

**legend.treatment:**   To present the legend with the names of the treatments (default is "FALSE")

**pos.outcome.label:**	The x and y coordinates referring to the position of the legend for the outcomes.

**pos.treatment.label:** The x and y coordinates referring to the position of legend.treatment.

**asterisk:**     Indicates whether a sector refers to treatments without data on the outcome within the circle (default is "FALSE")

**show.numbers:**   Indicates whether to show the percentage of each sector (default is "TRUE")

## Details
In the rank-heat plot, circles from outside in refer to the outcomes in the same order as presented in the dataset by the user (column 2, 3, etc.). Note that the number of outcomes included in the rank-heat plot should be ≤13.
Care is needed for the correct selection between "rank" and "percentage" in the format argument, so as to avoid erroneous results!

## EXAMPLES
Below we present 2 empirical examples using the SUCRA values presented in a systematic review and NMA published by Tricco et al.[2-4] See Appendix 1 for further details.


#########       COPD       #########
> library(fields)
> library(RColorBrewer)
>library(circlize)
> treatments<-c("ACLI", "AZD3199", "BECL/FORM", "BUDE", "FLUT", "FLUT/TIOT", "FORM", "FORM/BUDE", "FORM/BUDE/TIOT", "FORM/MOME", "FORM/TIOT", "GLYC", "GSK961081", "INDA", "INDA/GLYC", "INDA/TIOT", "MOME", "Placebo", "SALM", "SALM/FLUT", "SALM/TIOT", "TIOT","TIOT/BUDE/FORM","TIOT/FLUT/SALM","TIOT+Resp","TRIAM","UMEC","VILA", "VILA/FLUT", "VILA/UMEC")
> arrhythmia<- c(62.8, NA, NA, 39.1, NA, NA,46.9, 20.8,NA,NA,NA, 45.8,NA, 48.6, 40.9 ,NA , NA, 74.3 ,69.4, 74.8,NA,62.5, NA, NA, NA, NA , NA, NA ,  14.1, NA)
> pneumonia<-c(77.4, NA,   46.3, 74.6, 18.4, NA,   61.8, 34.7, NA,   60.5, NA ,  70.2, NA ,  55.8 ,66.5, NA,   40,   52.8 ,NA ,  11.2,41.8, 59 ,  54.8, 33.7 ,NA ,  NA ,  62.7, 45.1, 11.5, 71.3)
> cvm<-c(28.2, 70.2 ,NA  , 49.4, 61.4, NA,   58.5 ,27  , NA ,  NA ,NA ,  83.4, NA ,  63.4, 77 ,  32.3 ,NA ,  50.5, 75.9, 65.3,NA,   40.2, NA ,  NA ,  32,   19.2, 48.8, 35.5, 54.6 ,27.4)
> mortality<-c(66.9, 67.7, 60.8, 64.9, 41.2, 52.8, 28.2, 43.1, 26.9, 66.8,63.6, 70.2, NA,   64.2 ,60.5, 46.6, 29.7 ,44.7, 58.2 ,70.6,30.3 ,50.5 ,NA  , 28.5, 49.8, 64.7, 44.6, 27.9, 39.2, 37.1)
> exacerbations<-c(33.8, 67.7, 66.3, 40.7, 36.6 ,NA ,  57.6 ,81.3, NA ,  51.8,35.2, 44.5, 79.9, 32.1, 70.2, 77.5, 46.6 ,13.7 ,37.3, 44.1,48.8, 63.4, 96.2, 72.9, NA ,  NA ,  16.5 ,16.8, 51,17.4)
> mydata<-data.frame(treatments, exacerbations, cvm, mortality, pneumonia, arrhythmia)
> rankheatplot(data=mydata,format="percentage",lab.plot="numbers", color="grey", title.name="COPD", cex=0.65, legend.treatment="TRUE", pos.outcome.label=c(0.1,-0.45), pos.treatment.label=c(1,-0.45), asterisk="TRUE",show.numbers="TRUE")



########       5-HT3 surgery        #########
> library(fields)
> library(RColorBrewer)
> library(circlize)
> treatments<-c("Placebo", "Ondansetron","Granisetron", "Dolasetron","Tropisetron", "Ondansetron+Dexamethasone", "Palonosetron","Ramosetron", "Ondansetron+DroperidolIV", "Ondansetron+MetoclopramideIV", "Granisetron+Dexamethasone", "Palonosetron+Dexamethasone", "Dolasetron+Dexamethasone", "Dolasetron+DroperidolIV", "Granisetron+DroperidolIV")
> arrhythmia<- c(42.8, 54.6, 40.7, 81.5, 54.6, 83.3, NA, 35.3, NA, NA, 2.9, NA, NA, 54.2, NA)
> delirium<-c(51.9, 64.9, 93.5, 27.7, 35.3, NA, NA, NA, NA, NA, NA, NA, 26.6, NA, NA)
> nausea<-c(0.8, 25.4, 44.8, 13.3, 23.2, 71.9, 54.9, 51.3, 65.4, 52.8, 81.2, 43.7, 72.8, 76.8, 71.7)
> vomiting<-c(4.6, 29.4, 54.3, 22, 42.2, 76, 30.1, 48.4, 84.6, 78.8, 84.3, 7.8, 73.9, 68.8, 44.8)
> ponv<-c(0.1, 20.1, 42.5, 37.7, 16.3, 63, 72.3, 34.9, 76.9, NA, 86.3, 67, NA, 71.9, 61.1)
> mydata<-data.frame(treatments,vomiting, nausea, ponv, delirium, arrhythmia)
> vector<-c("PLAC","ONDA","GRAN","DOLA", "TROP","ONDA+DEX","PALO", "RAMO", "ONDA+DROP", "ONDA+MET", "GRAN+DEX", "PALO+DEX", "DOLA+DEX", "DOLA+DROP", "GRAN+DROP") 
> rankheatplot(data=mydata,format="percentage",lab.plot="vector", color="color.blind", title.name="5-ht3surgery", cex=0.65, legend.treatment="FALSE", pos.outcome.label=c(0.01,-0.4), pos.treatment.label=c(-0.25,-0.4), asterisk="FALSE")


## Authors

Areti Angeliki Veroniki, Sharon E Straus, Alexandros Fyraridis, Andrea Tricco

## Citation
Veroniki AA, Straus SE, Fyraridis A, Tricco AC. The rank-heat plot is a novel way to present the results from a network meta-analysis including multiple outcomes. J. Clin. Epidemiol. 2016; pii: S0895-4356(16)00153-0

