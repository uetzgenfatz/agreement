library(dplyr)
library(tidyr)
library(car)
library(lme4)
library(ggplot2)
library(Hmisc)
library(cAIC4)


# Daten einlesen und bereinigen

daten <- read.csv("/Users/uetzelsche/Desktop/2023-04-Datensatz.csv", sep=";", row.names=,1, na.strings="")

daten_kongruenz <- daten %>%
  select(CASE, matches("[FIS][ABCDEFHGIJKL]")) %>%
  select(!c(2, 111:115)) %>%
  select(!2:13)


# Ins Long-Format konvertieren

daten_kongruenz_mod <- pivot_longer(daten_kongruenz, starts_with("S"), names_to = "Bedingung", values_to = "Bewertung")


# NAs aussortieren

daten_kongruenz_mod <- drop_na(daten_kongruenz_mod)


# Items aus den einzelnen Bedingungen herauslösen

daten_kongruenz_mod$Item <- substr(daten_kongruenz_mod$Bedingung, 2, 2)


# Bedingungen separieren und rekodieren

daten_kongruenz_mod$Bedingung <- substr(daten_kongruenz_mod$Bedingung, 4, 4) 

bedingung_nr <- as.numeric(daten_kongruenz_mod$Bedingung)

bedingungen <- c("SV.Sg.adj", "SV.Pl.adj", "VS.Sg.adj", "VS.Pl.adj", "SV.Sg.dist", "SV.Pl.dist", "VS.Sg.dist", "VS.Pl.dist")

daten_kongruenz_mod$Bedingung <- bedingungen[bedingung_nr]


# Bedingung in drei Spalten aufteilen und Spaltenreihenfolge ändern

daten_kongruenz_mod <- separate(daten_kongruenz_mod, "Bedingung", into=c("Wortstellung", "Kongruenz", "Distanz"))

col_abfolge <- c("CASE", "Item", "Wortstellung", "Kongruenz", "Distanz", "Bewertung")

daten_kongruenz_mod <- daten_kongruenz_mod[, col_abfolge] 


# Als Faktor rekodieren:

daten_kongruenz_mod$Wortstellung <- factor(daten_kongruenz_mod$Wortstellung)

daten_kongruenz_mod$Kongruenz <- factor(daten_kongruenz_mod$Kongruenz)

daten_kongruenz_mod$Distanz <- factor(daten_kongruenz_mod$Distanz)

daten_kongruenz_mod$Item <- factor(daten_kongruenz_mod$Item)


# Teilstudien aus dem Datensatz herauslösen

zweifelsfall1 <- filter(daten_kongruenz_mod, Item %in% c("A", "B", "C", "D")) 

zweifelsfall2 <- filter(daten_kongruenz_mod, Item %in% c("E", "F", "G", "H")) 

zweifelsfall3 <- filter(daten_kongruenz_mod, Item %in% c("I", "J", "K", "L")) 


# Deskriptive Auswertung der Daten (Mittelwerte)

tab_zweifelsfall1 <- xtabs(Bewertung ~ Wortstellung + Kongruenz + Distanz, aggregate(Bewertung ~ Wortstellung + Kongruenz + Distanz, zweifelsfall1, mean)) 

tab_zweifelsfall2 <- xtabs(Bewertung ~ Wortstellung + Kongruenz + Distanz, aggregate(Bewertung ~ Wortstellung + Kongruenz + Distanz, zweifelsfall2, mean)) 

tab_zweifelsfall3 <- xtabs(Bewertung ~ Wortstellung + Kongruenz + Distanz, aggregate(Bewertung ~ Wortstellung + Kongruenz + Distanz, zweifelsfall3, mean)) 


# Kontrastkodierung für das gemischte lineare Modell

contrasts(daten_kongruenz_mod$Wortstellung) <- contr.Sum(levels(daten_kongruenz_mod$Wortstellung))

contrasts(daten_kongruenz_mod$Kongruenz) <- contr.Sum(levels(daten_kongruenz_mod$Kongruenz))

contrasts(daten_kongruenz_mod$Distanz) <- contr.Sum(levels(daten_kongruenz_mod$Distanz))


# Modelle für die Teilstudien

## Teilstudie 1 (Zweifelsfall 1)

modell_zweifelsfall1 <- lmer(Bewertung ~ Wortstellung + Kongruenz + Distanz + (1|Item) + (1|CASE), zweifelsfall1, REML= TRUE)

summary(rePCA(modell_zweifelsfall1))
summary(modell_zweifelsfall1)
Anova(modell_zweifelsfall1)

zweifelsfall1$FittedBewertung <- fitted(modell_zweifelsfall1)

## Prüfung der Modellgüte (Rückwärtsselektion via AIC)

modell_zweifelsfall1_step <- stepcAIC(modell_zweifelsfall1, direction = "backward", trace = TRUE, data = zweifelsfall1)

## Maximalmodell schneidet am besten ab.


# Teilstudie 2 (Zweifelsfall 2)

modell_zweifelsfall2 <- lmer(Bewertung ~ Wortstellung + Kongruenz + Distanz + (1|Item) + (1|CASE), zweifelsfall2, REML= TRUE)

summary(rePCA(modell_zweifelsfall2))
summary(modell_zweifelsfall2)
Anova(modell_zweifelsfall2)

zweifelsfall2$FittedBewertung <- fitted(modell_zweifelsfall2)

## Prüfung der Modellgüte (Rückwärtsselektion via AIC)

modell_zweifelsfall2_step <- stepcAIC(modell_zweifelsfall2, direction = "backward", trace = TRUE, data = zweifelsfall2)

## Wir können Item als zufälligen Faktor verwerfen

modell_zweifelsfall2_mod <- lmer(Bewertung ~ Wortstellung + Kongruenz + Distanz + (1|CASE), zweifelsfall2, REML= TRUE)

zweifelsfall2$FittedBewertung <- fitted(modell_zweifelsfall2_mod)

summary(modell_zweifelsfall2_mod)
Anova(modell_zweifelsfall2_mod)


# Teilstudie 3 (Zweifelsfall 3)

modell_zweifelsfall3 <- lmer(Bewertung ~ Wortstellung + Kongruenz + Distanz + (1|Item) + (1|CASE), zweifelsfall3, REML= TRUE)

summary(rePCA(modell_zweifelsfall3))
summary(modell_zweifelsfall3)
Anova(modell_zweifelsfall3)

zweifelsfall3$FittedBewertung <- fitted(modell_zweifelsfall3)

## Prüfung der Modellgüte (Rückwärtsselektion via AIC)

modell_zweifelsfall3_step <- stepcAIC(modell_zweifelsfall3, direction = "backward", trace = TRUE, data = zweifelsfall3)

## Maximalmodell schneidet am besten ab.

# Plots

modell_fit_zweifelsfall1 <-
  ggplot(zweifelsfall1, aes(x=Kongruenz, y=Bewertung)) + 
  stat_summary(aes(color="observed"), fun.data=mean_cl_boot) +
  stat_summary(aes(y=FittedBewertung, color="model"), fun.data=mean_cl_boot) + facet_grid(Wortstellung ~ Kongruenz ~ Distanz) + xlab("Kongruenz") + ylab("Bewertung") + scale_color_discrete(name  ="Modellgüte", breaks=c("model", "observed"), labels=c("Modell", "Daten"))

modell_fit_zweifelsfall2 <-
  ggplot(zweifelsfall2, aes(x=Kongruenz, y=Bewertung)) + 
  stat_summary(aes(color="observed"), fun.data=mean_cl_boot) +
  stat_summary(aes(y=FittedBewertung, color="model"), fun.data=mean_cl_boot) + facet_grid(Wortstellung ~ Kongruenz ~ Distanz) + xlab("Kongruenz") + ylab("Bewertung") + scale_color_discrete(name  ="Modellgüte", breaks=c("model", "observed"), labels=c("Modell", "Daten")) 

modell_fit_zweifelsfall3 <-
  ggplot(zweifelsfall3, aes(x=Kongruenz, y=Bewertung)) + 
  stat_summary(aes(color="observed"), fun.data=mean_cl_boot) +
  stat_summary(aes(y=FittedBewertung, color="model"), fun.data=mean_cl_boot) + facet_grid(Wortstellung ~ Kongruenz ~ Distanz) + xlab("Kongruenz") + ylab("Bewertung") + scale_color_discrete(name  ="Modellgüte", breaks=c("model", "observed"), labels=c("Modell", "Daten"))   


alle_effekte_zweifelsfall1 <- 
  ggplot(zweifelsfall1, aes(x=Kongruenz, y=Bewertung, color=Wortstellung)) + stat_summary(fun.data=mean_cl_boot) + facet_wrap(~Distanz) + xlab("Kongruenz") + ylab("Bewertung") + ylim(0, 4)

alle_effekte_zweifelsfall2 <- 
  ggplot(zweifelsfall2, aes(x=Kongruenz, y=Bewertung, color=Wortstellung)) + stat_summary(fun.data=mean_cl_boot) + facet_wrap(~Distanz) + xlab("Kongruenz") + ylab("Bewertung") + ylim(0, 4)

alle_effekte_zweifelsfall3 <- 
  ggplot(zweifelsfall3, aes(x=Kongruenz, y=Bewertung, color=Wortstellung)) + stat_summary(fun.data=mean_cl_boot) + facet_wrap(~Distanz) + xlab("Kongruenz") + ylab("Bewertung") + ylim(0, 4)


# Einzelne Effekte können Sie z.B. folgendermaßen plotten:

einzelne_effekte_zweifelsfall1 <- 
  ggplot(subset(zweifelsfall1, Distanz=='adj'), aes(x=Kongruenz, y=Bewertung, color=Wortstellung)) + stat_summary(fun.data=mean_cl_boot) + xlab("Kongruenz") + ylab("Bewertung") + ylim(0, 4)

# Codeschnipsel, um Plots zu speichern:

# Working directory anpassen (müssen Sie entsprechend ändern)

setwd("/Users/uetzelsche/Desktop")

alle_effekte_zweifelsfall1
quartz.save("zweifelfall1.png","png", dpi = 150) 

modell_fit_zweifelsfall1
quartz.save("fit_zweifelfall1.png","png", dpi = 150)

alle_effekte_zweifelsfall2
quartz.save("zweifelfall2.png","png", dpi = 150)

modell_fit_zweifelsfall2
quartz.save("fit_zweifelfall2.png","png", dpi = 150)

alle_effekte_zweifelsfall3
quartz.save("zweifelfall3.png","png", dpi = 150)

modell_fit_zweifelsfall3
quartz.save("fit_zweifelfall3.png","png", dpi = 150)

# Mass- vs. Count-Nomen als zusätzliche Bedingung

zweifelsfall1_alternativ <- filter(daten_kongruenz_mod, Item %in% c("A", "B", "C", "D")) 

zweifelsfall1_alternativ <-
  zweifelsfall1_alternativ %>%
  mutate(Quantifikation = as.factor(ifelse(Item == 'A' | Item == 'C', "mass", ifelse(Item == 'B' | Item == 'D', "count", NA)))) %>%
  drop_na()

# Alternatives Modell: Da Item nun ein fixer Effekt ist, nehmen wir nur CASE als zufälligen Effekt

modell_alternativ <- lmer(Bewertung ~ Wortstellung + Quantifikation + Kongruenz + Distanz + (1|CASE), zweifelsfall1_alternativ, REML= TRUE)

summary(rePCA(modell_alternativ))
summary(modell_alternativ)
Anova(modell_alternativ)

zweifelsfall1_alternativ$FittedBewertung <- fitted(modell_alternativ)

## Prüfung der Modellgüte (Rückwärtsselektion via AIC)

modell_alternativ_step <- stepcAIC(modell_alternativ, direction = "backward", trace = TRUE, data = zweifelsfall1_alternativ)

## Maximalmodell schneidet am besten ab.

modell_fit_zweifelsfall1_alternativ <-
  ggplot(zweifelsfall1_alternativ, aes(x=Kongruenz, y=Bewertung)) + 
  stat_summary(aes(color="observed"), fun.data=mean_cl_boot) +
  stat_summary(aes(y=FittedBewertung, color="model"), fun.data=mean_cl_boot) + facet_grid(Wortstellung ~ Kongruenz ~ Distanz ~ Quantifikation) + xlab("Kongruenz") + ylab("Bewertung") + scale_color_discrete(name  ="Modellgüte", breaks=c("model", "observed"), labels=c("Modell", "Daten"))   

alle_effekte_zweifelsfall1_alternativ <- 
  ggplot(zweifelsfall1_alternativ, aes(x=Kongruenz, y=Bewertung, color=Wortstellung)) + stat_summary(fun.data=mean_cl_boot) + facet_wrap(~Distanz + Quantifikation) + xlab("Kongruenz") + ylab("Bewertung") + ylim(0, 4)


modell_fit_zweifelsfall1_alternativ
quartz.save("fit_zweifelfall1_alternativ.png","png", dpi = 150)

alle_effekte_zweifelsfall1_alternativ

quartz.save("zweifelfall1_alternativ.png","png", dpi = 150)