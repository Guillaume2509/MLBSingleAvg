library(dplyr)
library(readr)
Batting <- read_csv("C:/Users/Guillaume/Downloads/baseballdatabank-master_2018-03-28/baseballdatabank-master/core/Batting.csv")
People <- read_csv("C:/Users/Guillaume/Downloads/baseballdatabank-master_2018-03-28/baseballdatabank-master/core/People.csv")

Frap <- inner_join(Batting, People, by = "playerID")

##Création du jeu de données Frappeurs
Frappeurs <- Frap %>%
  select(playerID, yearID, AB, H, "2B", "3B", HR, bats) %>%
  mutate(simples = H - `2B` - `3B` - HR) %>%
  select(playerID, yearID, bats, AB, simples) %>%
  filter(bats %in% c("R", "L") & AB >= 100) %>%
  mutate(propsimple = simples/AB) %>%
  select(playerID, yearID, bats, propsimple) %>%
  group_by(playerID, bats) %>%
  summarize(avgpropsimple = mean(propsimple))

##Vérification des suppositions
#créer un sous-ensemble pour groupe droitier
droitier <- Frappeurs[Frappeurs$bats == "R", ]
#calculer les résidus - groupe droitier
droitier.res <- droitier$avgpropsimple - mean(droitier$avgpropsimple)
#créer un sous-ensemble pour groupe gaucher
gaucher <- Frappeurs[Frappeurs$bats == "L", ]
#calculer les résidus - groupe gaucher
gaucher.res <- gaucher$avgpropsimple - mean(gaucher$avgpropsimple)
#combiner résidus dans jeu de données
Frappeurs$Res <- c(droitier.res, gaucher.res)
#vérifier normalité avec test Anderson-Darling
library("nortest")
ad.test(Frappeurs$Res)
#combiner graphique dans une fenêtre
par(mfrow = c(1, 2))
#graphique quantile-quantile
qqnorm(Frappeurs$Res, ylab = "Quantiles observés",
       xlab = "Quantiles théoriques",
       main = "Graphique quantile-quantile")
qqline(Frappeurs$Res)
#ajouter lettre a
text(x = -2, y = 14.5, labels = "a", cex = 1.2)
#diagramme de boîtes et moustaches - homoscédasticité
boxplot(Res ~ bats, data = Frappeurs)
#ajouter lettre b
text(x = 0.5, y = 14.5, labels = "b", cex = 1.2)


##Test t
t.out <- t.test(avgpropsimple ~ bats, data = Frappeurs, var.equal = TRUE,
                alternative = "greater")
t.out

#moyenne des groupes
moy.droitier <- mean(droitier$avgpropsimple)
moy.gaucher <- mean(gaucher$avgpropsimple)
#SD des groupes
sd.droitier <- sd(droitier$avgpropsimple)
sd.gaucher <- sd(gaucher$avgpropsimple)
#n des groupes
n.droitier <- nrow(droitier)
n.gaucher <- nrow(gaucher)
#SE des moyennes
SE.droitier <- sd.droitier/sqrt(n.droitier)
SE.gaucher <- sd.gaucher/sqrt(n.gaucher)
#IC's à 95%
IC.inf95.droitier <- moy.droitier - qt(p = 0.025, df = n.droitier - 1) * SE.droitier
IC.sup95.droitier <- moy.droitier + qt(p = 0.025, df = n.droitier - 1) * SE.droitier
IC.inf95.gaucher <- moy.gaucher - qt(p = 0.025, df = n.gaucher - 1) * SE.gaucher
IC.sup95.gaucher <- moy.gaucher + qt(p = 0.025, df = n.gaucher - 1) * SE.gaucher
#graphique
plot(y = 0, x = 0, xlab = "Type de frappeur",
     ylab = "Proportion de simples par présence au bâton",
     main = "Moyennes ± IC à 95%",
     ylim = range(c(IC.inf95.droitier, IC.inf95.gaucher,
                    IC.sup95.droitier, IC.sup95.gaucher)),
     xlim = c(0, 1),
     xaxt = "n")
#ajout de l'axe des x
axis(side = 1, at = c(0.3, 0.75), labels = c("droitier", "gaucher"))
#ajout des points
points(y = c(moy.droitier, moy.gaucher), x = c(0.3, 0.75))
# ajout des barres d'erreur
arrows(x0 = c(0.3, 0.75), x1 = c(0.3, 0.75),
       y0 = c(IC.inf95.droitier, IC.inf95.gaucher),
       y1 = c(IC.sup95.droitier, IC.sup95.gaucher),
       length = 0.05, code = 3, angle = 90)