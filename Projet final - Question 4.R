# PROJET FINAL
# Auteur: Ronnie Liu
# Date: Mercredi 27 juillet 2021
# Objet: Fichier R pour QUESTION 4

################################################################################
#########################        QUESTION 4        #############################
################################################################################

path <- "C:/KINGSTON/Semestre 5/STT 3220/Devoirs/Projet final"
setwd(path)

################################################################################
# ÉTAPES PRÉLIMINAIRES POUR L'ANALYSE DES DONNÉES
# 1) CUEILLETTE D'INFORMATIONS

# Les données sont tirées du site: https://www150.statcan.gc.ca/n1/fr/type/donnees
# Quelques informations pertinentes par rapport aux données cueillies:

# Numéro d'identification: 24-10-0002-01 (anciennement CANSIM 427-0002)
# Nom d'étude: Nombre de véhicules voyageant entre le Canada et les États-Unis
#               (section: Total des véhicules entrant au Canada)
# Période considérée: Janvier 2009 - Avril 2018

# On veut étudier le nombre total des véhicules entrant au Canada entre janvier
# 2009 et avril 2018. Étant donné qu'on reçoit des millions de véhicules par 
# mois, l'analyse sera abordée selon les données recueillies, mais divisées par
# un million afin de travailler avec des nombres plus petits. Les conclusions
# demeurent identiques; le but est uniquement de faciliter la tâche à visualiser
# les résultats en réduisant le nombre de chiffres dans une donnée.

# Nous avons créé un fichier nommé Data_Q4.csv qui contient toutes les données
# nécessaires. L'annexe A montrera le contenu de ce dernier.

data <- read.csv('Data_Q4.csv', sep=';')
mois <- data$ï..Mois
y <- data$Qty
y_new <- data$Qty/1000000

write.csv(cbind(mois, y_new),'Data_Q4_modified.csv')
data <- read.csv('Data_Q4_modified.csv', sep=',')
y <- data$y_new

# Premier aperçu des données recueillies
plot(seq(1,112,1), y,
     main="Nombre de véhicules total entrant au Canada (jan 2009-avril 2018)",
     xlab = "Temps (en mois)",
     ylab = "Nombre de véhicules (en millions)",
     type="l",
     col="blue")

# Commentaires: La série chronologique est saisonnière de fréquence mensuelle.
# On peut voir que chaque année, le mois qui a le plus de véhicules qui entre
# au Canada est le mois d'août, ce qui a du sens étant donné que c'est la 
# période des vacances.

# Selon le graphique, nous n'étions pas certain si la tendance est linéaire
# dans cette série chronologique, mais il y a une tendance peu importe.
# Nous avons essayé plusieurs tentatives pour éliminer cette tendance: 
# la racine carrée, l'inverse, le logarithmique, mais la tendance demeure 
# présente. L'hypothèse sera de faire une différenciation deux fois puisqu'on
# voit une tendance qui ressemble à une fonction quadratique. Ensuite,
# on fait une différenciation saisonnière afin que la série soit plus 
# stationnaire.



# 2) AJUSTEMENT DE CETTE SÉRIE AVEC LES MÉTHODES ARIMA

# Ajustement 1: Diviser les données par un million pour simplifier la tâche
#               d'analyse
# Ajustement 2: Différenciation première (et double?) pour éliminer la tendance
# Ajustement 3: Différenciation saisonnière où s = 12 mois



# 3) NOMBRE D'OBSERVATIONS

# Troncage des 12 dernières données, afin de faire l'analyse avec les 110 
# premières observations

n <- 100
t <- seq(1,100,1)
y <- data$y_new[-seq(101, 112,1)]

plot(t, y,
     main="Nombre de véhicules total entrant au Canada (jan 2009-mai 2017)",
     xlab = "Temps (en mois)",
     xlim = c(1,112),
     ylab = "Nombre de véhicules (en millions)",
     type="l")

library(astsa)

################################################################################
# a) Transformation de la série

# En faisant plusieurs expérimentations, on rejette notre hypothèse initiale,
# c'est-à-dire de faire deux différenciations non saisonnières.
# Avec une seule différenciation, on observe que les données semblent plus 
# "stationnaires" et visuellement, nous avons éliminé la tendance.

dy <- diff(y)
ddy <- diff(dy, 12)

plot.ts(cbind(y, dy, ddy), main="")

# On observe que la présence d'un motif pour chaque mois, donc il faut
# l'éliminer à l'aide de la différenciation saisonnière afin que les données
# semblent être stationnaires.

par(mfrow=c(1,1))
monthplot(dy)
monthplot(ddy)
par(mfrow=c(1,1))

# Conclusion, on prend les données brutes qui sont différenciées
# de façon linéaire et saisonnière une fois chacune.

################################################################################
# b) Les ordres du modèle choisi

# Commençons par analyser les ACF et les PACF du modèle
acf2(ddy, max.lag=50)

# COMPOSANTE NON SAISONNIÈRE (délais faibles entre 1 et 11):
# ACF: Observation d'un seul pic (ou) décroissance exponentielle des auto-
#      corrélations
# PACF: Observation d'un seul pic aussi (ou) décroissance exponentielle des 
#       auto-corrélations

# Nous sommes certains la valeur de p ou de q ne dépassera pas de 1, donc nous
# avons trois modèles possibles à tester: 
# ARIMA(1,1,0) - ARIMA(0,1,1) - ARIMA(1,1,1)


# COMPOSANTE SAISONNIÈRE (délais 12, 24, 36...):
# Dans le graphique des ACF et des PACF, c'est la partie la plus compliquée
# à remarquer puisque plusieurs phénomènes peuvent y arriver.

# Voici quelques hypothèses par rapport à la composante saisonnière du modèle:
# Le délai 12 dans ACF n'atteint pas vraiment la ligne "bleue", mais est proche,
# tandis que ce dernier n'est pas du tout atteint dans PACF.

# 1. Il se peut que le délai 12 est ignoré dans les deux graphiques, donc ça va
#       être ARIMA(0,1,0)_12.
# 2. Sinon, puisque le délai 12 a une auto-corrélation plus élevée dans ACF, il
#       se peut que ça soit un pic à considérer, donc ARIMA(0,1,1)_12 ou
#       ARIMA(1,1,1)_12 (peut considérer comme une décroissance exponentielle
#       dans le graphique ACF).

# Noter que les valeurs à côté de la ligne "sarima" sont les valeurs 
# de log(vraisemblance) correspondante.

# Test 1: Avec ARMA(1,1) non saisonnier (modèles 1 à 3)
sarima(y, 1,1,1, 0,1,0, 12)             #92.00  -- non-significatif
sarima(y, 1,1,1, 0,1,1, 12)             #93.88  -- non-significatif
sarima(y, 1,1,1, 1,1,1, 12)             #94.82  -- non-significatif

# Test 2: Avec AR(1) non saisonnier (modèles 4 à 6)
sarima(y, 1,1,0, 0,1,0, 12)             #91.93  -- significatif
sarima(y, 1,1,0, 0,1,1, 12)             #93.76  -- significatif
sarima(y, 1,1,0, 1,1,1, 12)             #94.76  -- non significatif

# Test 3: Avec MA(1) non saisonnier (modèles 7 à 9)
sarima(y, 0,1,1, 0,1,0, 12)             #91.96  -- significatif
sarima(y, 0,1,1, 0,1,1, 12)             #93.86  -- significatif
sarima(y, 0,1,1, 1,1,1, 12)             #94.80  -- non-significatif

# Dès le départ les modèles 1, 2, 3, 6 et 9 ne sont pas considérées puisqu'il
# existe des paramètres dont la p-valeur n'est pas du tout significative.

# Il reste à choisir un modèle parmi 4, 5, 7 et 8.
# La valeur log(vraisemblance) la plus élevée (AIC la moins élevée) est le 
# modèle 8, donc on choisira ce dernier et l'analyser pour confirmer si
# c'est un bon choix.

# Modèle choisi: ARIMA(0,1,1) x (0,1,1)_12

################################################################################
# c) Statistiques diagnostics et autres diagnostics
# Observations des tests et des résidus

# MODÈLE ARMA(0,1,1) X (0,1,1)_12
sarima(y, 0,1,1, 0,1,1, 12) 

# AIC de -181.72 qui est le plus petit parmi les autres modèles testés.
# Log likelihood de 93.86.

# Standardized Residuals: Les résidus sont majoritairement entre -3 et 3 comme
# dans la loi normale centrée réduite (sauf au temps t=80 où ça dépasse un peu
# -3). On peut dire que les résidus se comportent selon la loi normale.

# ACF of Residuals: Aucune présence d'auto-corrélation entre les résidus, donc
# le modèle est accepté.

# Ici, les résidus sont selon la loi normale de façon majoritaire. On peut
# voir un peu de déviations lors des extrémités, mais la plupart des données
# demeurent "normales".

# Ljung-Box: les p-valeurs se trouvent au-dessus de la ligne bleue, donc on ne 
# rejette pas l'hypothèse nulle pour tous les délais. Donc, on peut prendre 
# ce modèle pour faire des prévisions des 12 prochains mois.

# DISCUSSION DES P-VALEURS DES ESTIMATEURS ET DE LA VALEUR ALPHA:
# Il est vrai que les estimateurs ont des p-valeurs significatives, mais en
# ce qui concerne de sma1: la p-valeur est 0.0448, ce qui est très proche de 5%.

# Par conséquent, le modèle choisi va dépendre de l'erreur du type I tolérée.
# Si par exemple on choisit un modèle avec un alpha de 2.5%, ce modèle ne sera
# plus valide. Dans ce cas-ci, on prendra le modèle 7 (car le modèle 5 a aussi
# un estimateur avec une p-valeur proche de 5%) qui est ARIMA(0,1,1)x(0,1,0)_12. 

# Bref, ici, alpha = 5% et c'est pour cela qu'on prend le modèle choisi en b).
# Cependant, le modèle 5 doit être réservé si jamais on veut des prévisions plus
# précises (alpha < 5%).

################################################################################
# d) Performance prévisionnelle

# Vraies valeurs des 12 derniers mois
last_y <- data$y_new[seq(101,112,1)]

# Valeurs prédites des 12 derniers mois
predict_model <- sarima.for(y, 12, 0,1,1, 0,1,1, 12)
predict_y <- predict_model$pred

# EQMP
EQMP <- mean((predict_y - last_y)^2)
EQMP

# Ligne bleue: les prévisions
# Ligne noire: les données brutes
plot(seq(1,112,1), data$y_new, xlim=c(1,112), type="b")
lines(seq(101,112,1), predict_y, col="blue")

# Nous avons une EQMP de 0.01698084, ce qui est proche de zéro. Cela montre
# que le modèle choisi prédit assez bien les 12 prochains mois.

################################################################################
# e) Intervalles de prévision

# En se servant de la formule y_hat +/- 1.95*se, on obtient les intervalles
# de prévision suivants pour les 12 dernières données

borneInf <- predict_y - 1.96*predict_model$se
borneSup <- predict_y + 1.96*predict_model$se

cbind(borneInf, predict_y, borneSup)

################################################################################
# f) Représentation graphique des prévisions

# Les lignes violettes représentent les bornes inférieures et supérieures
# des intervalles de prévisions.

sarima.for(y, 12, 0,1,1, 0,1,1, 12)
lines(seq(101,112,1), borneInf, col="purple")
lines(seq(101,112,1), borneSup, col="purple")

# Modèle choisi:
# Y_t = Y_{t-1} + Y_{t-12} + Y_{t-13} + e_t - 0.3226 e_{t-1} - 0.3164 e_{t-12} + 
#       0.10207064 e_{t-13}

# où e = epsilon

