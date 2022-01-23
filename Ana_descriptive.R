################################################################################
# STEP 2 : DESCRIPTIVE ANALYSIS
################################################################################
################################################################################
#
# cf CR paragraphes 2.5 et 2.6.1
# Min, max, moy, mediane, 1er et 3ème quartiles,  des 23 paramètres quantitatifs 
# étudiés
summary(P_init)
# infos sur le nb d'observations, sur l'existence de val manquantes, 
# et autres quantiles
describe(P_init)

################################################################################
# DISTRIBUTION
# Représentation graphique de la dispersion des valeurs des 23 attributs, et
# détection éventuelle des outliers
################################################################################
# cf paragraphe 2.6.2
#
#
# attribut StATUT (binaire)
# permet de visualiser le ratio des signaux de patients sains vs malades
# Figure 262-1
mytable <- table(P_init$status)
names(mytable)
lbls <- paste(c("Healthly", "Parkinson Disease"), "\n",mytable, sep="")
pie3D(mytable, col=c("purple","#dd00dd"), labels = lbls, explode=0.1,
      main="Nb of signals corresponding \n with status patient") 

################################################################################
# les 22 split ci dessus sert à repérer sur quels sous partie du dataset se 
# focaliserait les outliers

################################################################################
#
# REPRESENTATION EN BOITES DE TURKEY
#
################################################################################
# the 3 measures of Fo

# MDVP_Fo 
# Figure 262-2
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$MDVP_Fo, P_pd$MDVP_Fo, P_h$MDVP_Fo,
        col = c("purple"),           #Pour la couleur
        main = paste("Fundamental Frequencies (avg) Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_Fhi
# # Figure 262-3
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$MDVP_Fhi, P_pd$MDVP_Fhi, P_h$MDVP_Fhi,
        col = c("purple"),           #Pour la couleur
        main = paste("Fundamental Frequencies (highest) Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_Flow
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
# Figure 262-4
boxplot(P_init$MDVP_Flow, P_pd$MDVP_Flow, P_h$MDVP_Flow,
        col = c("purple"),           #Pour la couleur
        main = paste("Fundamental Frequencies (lowest) Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

################################################################################
#
# the 5 jitter attributes

# JitterAbs
# Figure 262-6
boxplot(P_init$MDVP_JitterAbs, P_pd$MDVP_JitterAbs, P_h$MDVP_JitterAbs,
        col = c("blue"),           #Pour la couleur
        main = paste("MDVP_JitterAbs Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# Jitter_DDP
# Figure 262-7
# on voit ci-dessous que le caractère malade ne change pas vraiment l'occurence 
# d'outliers
boxplot(P_init$Jitter_DDP, P_pd$Jitter_DDP, P_h$Jitter_DDP,
        col = c("blue"),           #Pour la couleur
        main = paste("Jitter_DDP Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_JitterRel
# Figure 262-8
boxplot(P_init$MDVP_JitterRel, P_pd$MDVP_JitterRel, P_h$MDVP_JitterRel,
        col = c("blue"),           #Pour la couleur
        main = paste("MDVP_JitterRel Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_Rap
# Figure 262-9
boxplot(P_init$MDVP_Rap, P_pd$MDVP_Rap, P_h$MDVP_Rap,
        col = c("blue"),           #Pour la couleur
        main = paste("MDVP_Rap Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_PPQ
# Figure 262-10
boxplot(P_init$MDVP_PPQ, P_pd$MDVP_PPQ, P_h$MDVP_PPQ,
        col = c("blue"),           #Pour la couleur
        main = paste("MDVP_PPQ Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées


################################################################################

# the 6 shimmer attributes

# SHIMMERDB
# Figure 262-13
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$MDVP_ShimmerDB, P_pd$MDVP_ShimmerDB, P_h$MDVP_ShimmerDB,
        col = c("orange"),           #Pour la couleur
        main = paste("MDVP_ShimmerDB Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_Shimmer
# Figure 262-14
boxplot(P_init$MDVP_Shimmer, P_pd$MDVP_Shimmer, P_h$MDVP_Shimmer,
        col = c("orange"),           #Pour la couleur
        main = paste("MDVP_Shimmer Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# Shimmer_APQ3
# Figure 262-15
boxplot(P_init$Shimmer_APQ3, P_pd$Shimmer_APQ3, P_h$Shimmer_APQ3,
        col = c("orange"),           #Pour la couleur
        main = paste("Shimmer_APQ3 Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# Shimmer_APQ5
# Figure 262-16
boxplot(P_init$Shimmer_APQ5, P_pd$Shimmer_APQ5, P_h$Shimmer_APQ5,
        col = c("orange"),           #Pour la couleur
        main = paste("Shimmer_APQ5 Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# MDVP_APQ
# Figure 262-17
boxplot(P_init$MDVP_APQ, P_pd$MDVP_APQ, P_h$MDVP_APQ,
        col = c("orange"),           #Pour la couleur
        main = paste("MDVP_APQ Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# Shimmer_DDA
# Figure 262-18
boxplot(P_init$Shimmer_DDA, P_pd$Shimmer_DDA, P_h$Shimmer_DDA,
        col = c("orange"),           #Pour la couleur
        main = paste("Shimmer_DDA Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

################################################################################

# the 2 measures of ratio of noise to tonal components in the voice

# NHR
# Figure 262-20
boxplot(P_init$NHR, P_pd$NHR, P_h$NHR,
        col = c("purple"),           #Pour la couleur
        main = paste("NHR (noise) Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# HNR
# FIgure 262-21
boxplot(P_init$HNR, P_pd$HNR, P_h$HNR,
        col = c("purple"),           #Pour la couleur
        main = paste("HNR (noise) Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

################################################################################

# the 2 Nonlinear dynamical complexity measures

# D2
# figure 262-23
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$D2, P_pd$D2, P_h$D2,
        col = c("yellow"),           #Pour la couleur
        main = paste("D2 Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# RPDE
# figure 262-24
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$RPDE, P_pd$RPDE, P_h$RPDE,
        col = c("yellow"),           #Pour la couleur
        main = paste("RPDE Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées


################################################################################

# the 3 attributes nonlinear measures of Fo variation

# spread1
# figure 262-26
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$spread1, P_pd$spread1, P_h$spread1,
        col = c("cyan"),           #Pour la couleur
        main = paste("spread1 Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# spread2
# figure 262-27
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$spread2, P_pd$spread2, P_h$spread2,
        col = c("cyan"),           #Pour la couleur
        main = paste("spread2 Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

# PPE
# figure 262-28
#boxplot de l'attribut PPE en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$PPE, P_pd$PPE, P_h$PPE,
        col = c("cyan"),           #Pour la couleur
        main = paste("PPE Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

################################################################################
# DFA
# figure 262-31
#boxplot de l'attribut en comparant le dataset d'origine,
# celui des patients malades, puis celui des patients sains
boxplot(P_init$DFA, P_pd$DFA, P_h$DFA,
        col = c("grey"),           #Pour la couleur
        main = paste("DFA Boxplot"),     #Pour le titre
        names = c("all", "PD", "healthly"), #Pour le labelling de l'axe x
        ylab = "Quantiles")          #Pour le titre de l’axe des ordonnées

################################################################################
#
# REPRESENTATION GRAPHIQUES EN LIGNES DE CRETES 
#  Pour avoir une meilleure idée de la distribution
################################################################################

# the 3 measures of Fo
# graphique en ligne de crêtes, exemple avec MDVP_Fo, MDVP_Flow, MDVP_Fhi
# Figure 262-5
vioplot( P_pd$MDVP_Fo, P_pd$MDVP_Flow, P_pd$MDVP_Fhi, col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("F0 (avg)", "F0 (lowest)", "F0 (highest)"))
vioplot(P_h$MDVP_Fo, P_h$MDVP_Flow, P_h$MDVP_Fhi,  col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")

# the 5 jitter attributes 
# graphique en ligne de crêtes, exemple avec MDVP_JitterRel, MDVP_PPQ, MDVP_Rap, Jitter_DDP
# Figure 262-11
vioplot( P_pd$MDVP_JitterRel, P_pd$MDVP_Rap, P_pd$MDVP_PPQ, P_pd$Jitter_DDP, col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("Jitter (%)", "Rel. Avg Perturb", "5-pt PPQ", "Jitter DDP"))
vioplot(P_h$MDVP_JitterRel, P_h$MDVP_Rap, P_h$MDVP_PPQ,  P_h$Jitter_DDP, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")
# graphique en ligne de crêtes, exemple avec MDVP_JitterAbs
# Figure 262-12
vioplot( P_pd$MDVP_JitterAbs,  col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("JitterAbs"))
vioplot(P_h$MDVP_JitterAbs,  data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")

# the 5 shimmer attributes (excluding MDVP_ShimmerDB)
# graphique en ligne de crêtes, exemple avec MMDVP_Shimmer, Shimmer_APQ3, Shimmer_APQ5, MDVP_APQ, Shimmer_DDA
# Figure 262-19
vioplot( P_pd$MDVP_Shimmer, P_pd$Shimmer_APQ3, P_pd$Shimmer_APQ5,  P_pd$MDVP_APQ, P_pd$Shimmer_DDA, col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("Shimmer", "3APQ Shim", "5APQ Shim", "Sh. APQ", "Sh. DDA"))
vioplot(P_h$MDVP_Shimmer, P_h$Shimmer_APQ3, P_h$Shimmer_APQ5, P_h$MDVP_APQ, P_h$Shimmer_DDA, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")


# the noise attributes (only one of both HNR and NHR)
# graphique en ligne de crêtes, exemple avec NHR
# Figure 262-22
vioplot( P_pd$NHR,  col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("NHR"))
vioplot(P_h$NHR, data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")

#  the 2 Nonlinear dynamical complexity measures
# Figure 262-25
vioplot( P_pd$RPDE, P_pd$D2, col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("RPDE", "D2"))
vioplot(P_h$RPDE, P_h$D2, data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")

# the 3 Nonlinear measures of fundamental frequency variation 
# Figure 262-29
# graphique en ligne de crêtes, exemple avec spread1 (bug sur la mise à échelle automatique des axes)
vioplot( P_pd$spread1, col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("spread1"))
vioplot(P_h$spread1, data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")                                                                                                                     
# graphique en ligne de crêtes, exemple avec  PPE, spread2
# Figure 262-30
vioplot( P_pd$PPE, P_pd$spread2,  col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("Pitch period entropy", "spread2"))
vioplot(P_h$PPE, P_h$spread2, data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")

# the signal fractal scaling exponent (DFA) - (bug sur la mise à échelle automatique des axes)
# Figure 262-32
vioplot( P_pd$DFA,  col = "palevioletred", plotCentre = "line", 
         side = "left",  names=c("Signal fractal scaling exponent"))
vioplot(P_h$DFA, data = P_h, col = "lightblue", plotCentre = "line", 
        side = "right", add = T)
legend("bottomright", fill = c("palevioletred", "lightblue"), legend = c("PD", 
                                                                         "Healthly"), title = "Status")


# on constate des distributions en cloche, mais pour certains attributs, 
# les étendues de valeurs sont clairement déplacés selon le statut malade/pas malade

################################################################################
# Etude des CORRELATION 
################################################################################

# jenleve le status
P_stless <- P_init[ , -17]
P_stless

# mat de correlation (coeff de Peason par defaut)
Mcor_P_stless <- round(cor(P_stless),2)
View(Mcor_P_stless)

# heatmap
# corrplot(Mcor_P_stless, type="upper", order="hclust", tl.col="black", tl.srt=45)

################################################################################
# Figure 262-33
################################################################################

# Fondre la matrice de corrélation
melted_cormat <- melt(Mcor_P_stless)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Obtenir le triangle inférieur
get_lower_tri<-function(Mcor_P_stless){
  Mcor_P_stless[upper.tri(Mcor_P_stless)] <- NA
  return(Mcor_P_stless)
}
# Obtenir le triangle supérieur
get_upper_tri <- function(Mcor_P_stless){
  Mcor_P_stless[lower.tri(Mcor_P_stless)]<- NA
  return(Mcor_P_stless)
}
#utilisation
upper_tri <- get_upper_tri(Mcor_P_stless)
upper_tri
# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(Mcor_P_stless){
  # Utiliser la corrélation entre les variables
  # comme mésure de distance
  dd <- as.dist((1-Mcor_P_stless)/2)
  hc <- hclust(dd)
  Mcor_P_stless <-Mcor_P_stless[hc$order, hc$order]
}


# Reordonner la matrice de corrélation
Mcor_P_stless <- reorder_cormat(Mcor_P_stless)
upper_tri <- get_upper_tri(Mcor_P_stless)
# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Créer un ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Afficher heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

################################################################################
# A NOTER !!!
# On observe déjà que DFA n'a peu de correlation avec aucune autre variable : ce param sera 
# a priori très "informatif"
# Idem pour MDVP_Fhi, MDVP_fo, MDVP_Flow


