getwd()
setwd("/home/liz/Documents/MS_Big_Data_TP_et_projets/Data Mining/Projet/")
#PARTIE 1
A<-read.table("parkinsons.data_headerless")
dim(A)
label<-attributes(A)$row.names
View(A)
names(A)
library(dplyr, quietly = TRUE)
A2 <- A %>% rename( MDVP_Fo = V1 , MDVP_Fhi = V2, MDVP_Flow = V3, 
                    MDVP_JitterRel = V4, MDVP_JitterAbs =V5, MDVP_Rap=V6, 
                    MDVP_PPQ = V7, Jitter_DDP = V8, MDVP_Shimmer = V9, 
                    MDVP_ShimmerDB = V10, Shimmer_APQ3 = V11, 
                    Shimmer_APQ5 = V12, MDVP_APQ = V13, Shimmer_DDA = V14, 
                    NHR = V15, HNR = V16, status = V17, RPDE = V18, DFA = V19, 
                    spread1 = V20, spread2 = V21, D2 = V22, PPE = V23
)

names(A2)
View(A2)
summary(A2)
