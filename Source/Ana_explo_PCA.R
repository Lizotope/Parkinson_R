################################################################################
# ANALXSE EN COMPOSANTES PRINCIPALES
#
################################################################################
# pca sur jeu de données normalisé pour lequel nous avons retiré l'attribut à prédire (la 17è colonne)
res.pca <- PCA(P_initsc[,-17], scale.unit = TRUE, graph = TRUE)

# VARIANCE, VECTEURS PROPRES (=COMPOSANTES PRINCIPALES)
# desc des val propres portant le plus l'information (inertie)
eig.val <- get_eigenvalue(res.pca)
eig.val
# a priori 5 dimensuions suffisent (86.5% de l'info, c'est bien)

# graphique des val propres (screeplot)
# figure 512-1
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# extraction des résultats de l'ACP
var <- get_pca_var(res.pca)

################################################################################
# ETUDE DES CORRELATIONS DES ATTRIBUTS INITIAUX AUX COMPOSANTES PRINCIPALES
################################################################################
# cosinus carré qui permet le calcul de coordonnées des attributs initaux  qui serviront
# à la visualisation du cercle de corrélation
var$cos2
# cercle des corrélation des attributs initiaux aux composantes principales
# Colorer en fonction du cos2: qualité de représentation
# figure 512-2
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

# sur le cercle, on ne peut visualiser que 2 dim (plan!)
# pour visualiser toutes les dimensions de 1 à 5 : 
# figure 512-3
corrplot(var$cos2, is.corr=FALSE)

################################################################################
# ETUDE DES CONTRIBUTIONS DES ATTRIBUTS INITIAUX AUX COMPOSANTES PRINCIPALES
################################################################################
# extractions des contributions en %
var$contrib
# on voit que DFA, MDVP_Fo, PRDE ont des gros coeff contributeurs aux nouveaux 
#composants
# visu du cercle de contributions : attention, seulement les 2 premières dimensions 
#ont représentées
# figure 512-4
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# extensions aux 5 composantes
# figure 512-5
corrplot(var$contrib, is.corr=FALSE)   

# barplot pour chacune des 5 composantes de la contribution des attributs initiaux

# Contributions des variables à PC1
# figure 512-6
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions des variables à PC2
# figure 512-7
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) # MDVP_Fo et MDVP_Flow surpassent toutes les autres

# Contributions des variables à PC3
# figure 512-8
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10) # D2, DFA, spread2...

# Contributions des variables à PC4
# figure 512-9
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10) # DFA, Shimmet_APQ5

# Contributions des variables à PC5
# figure 512-10
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10) # RPDE, DFA, spread2


