getwd()
setwd("/home/liz/Documents/MS_Big_Data_TP_et_projets/Data Mining/Projet/Parkinson_R")
#PARTIE 1
A<-read.table("parkinsons.data_headerless")
dim(A)
label<-attributes(A)$row.names
View(A)
names(A)
library(dplyr, quietly = TRUE)
A2 <- A %>% rename( MDVP_Fo = V2 , MDVP_Fhi = V3, MDVP_Flow = V4, 
                    MDVP_JitterRel = V5, MDVP_JitterAbs =V6, MDVP_Rap=V7, 
                    MDVP_PPQ = V8, Jitter_DDP = V9, MDVP_Shimmer = V10, 
                    MDVP_ShimmerDB = V11, Shimmer_APQ3 = V12, 
                    Shimmer_APQ5 = V13, MDVP_APQ = V14, Shimmer_DDA = V15, 
                    NHR = V16, HNR = V17, status = V18, RPDE = V19, DFA = V20, 
                    spread1 = V21, spread2 = V22, D2 = V23, PPE = V24
)

names(A2)
View(A2)
summary(A2)
