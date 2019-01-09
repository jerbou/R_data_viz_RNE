
# ==== 00 charge lib -----------------------------------------------------------
library("doBy", lib.loc="C:/R-3.5.0/library")
library("gridExtra", lib.loc="C:/R-3.5.0/library")
library(forcats) 
library(sf)
library(spatstat)
library(sp)
library(maptools)
library(raster)
library(cartography)
library(SpatialPosition)
library(ggplot2)
library(ggplotgui)
library(doBy)
library(gghighlight)
library(dplyr)
library(rgdal)
library(remotes)
library(readr)
library("gghighlight", lib.loc="C:/R-3.5.0/library")
library(ggridges)
library("viridis", lib.loc="C:/R-3.5.0/library")
library(GGally)
library(RColorBrewer)
library(colorRamps)
library(ggrepel)
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggpubr)
library(reshape2)
library(readr)
library(doBy)
library(ggplot2)
library(viridis)
library(RPostgreSQL)
library(dplyr)
library(readxl)
library(plyr)
library(ggiraph)

# charger le dossier de travail 
setwd("G:00_data_ref/data_gouv_fr/RNE_elus")

# GET("https://www.data.gouv.fr/fr/datasets/r/67621532-8182-4254-9b55-5e7292f3ab76")

list.files()

df0 <- read.delim("9-rne-maires.txt", sep='\t', header=TRUE, skip=1)
names(df0)
df_copy <- df0
# ===== pyramide des ages =====
# https://rpubs.com/walkerke/pyramids_ggplot2

df0$Date.de.naissance <- as.Date(df0$Date.de.naissance,format="%d/%m/%Y")
str(df0)
View(df0)
# lubridate::year(df0$Date.de.naissance)
df0$age <-  as.numeric(format(Sys.Date(), "%Y")) - as.numeric(lubridate::year(df0$Date.de.naissance))
View(df0)

# on cree la date de comptage
df2 <- count(df0, c('Code.sexe', 'age'))
df2<-na.omit(df2)

View(df2)
df2$freq <- ifelse(df2$Code.sexe == "M" , -1*df2$freq, df2$freq)

summary(df2$age)
summary(df2$freq)

sum(abs(df2$freq))

# http://stackoverflow.com/questions/14680075/simpler-population-pyramid-in-ggplot2
# http://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
g1 <- ggplot(df2, aes(x=age, y=freq, fill= Code.sexe, tooltip = paste("age :", age, "nombre", abs(freq), sep=" "))) +
  geom_bar(data=subset(df2,Code.sexe =="M"), stat="identity", width=1, colour="grey20") +
  geom_bar(data=subset(df2,Code.sexe =="F"), stat="identity", width=1, colour="grey20") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  coord_flip() + expand_limits(x=c(20,100), y=c(-150, 150)) +
  scale_y_continuous(breaks = seq(-1500,1500,100), labels=abs(seq(-1500,1500,100))) +  scale_colour_manual(values = c(A = "green")) + theme_minimal() + 
  ggtitle("Pyramides des ages des maires") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Sexe", caption = "RNE data.gouv.fr") + ylab("Nombre") +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) 

g1

# a la sauce interactive ... TO COMPLTETE COMMENT PUBLIER UN HTML ONLINE GITHUB desktop.
# https://pages.github.com/
g2 <- g1 +  geom_bar_interactive(stat = "identity")
ggiraph(code = print(g2))
  # geom_vline(xintercept = median(df0$age), linetype="dashed", color = "darkred", size = 1) + 
  # geom_vline(xintercept = mean(df0$age), linetype="dashed", color = "darkgreen", size = 1) +  scale_colour_manual(values = c(A = "green"))
  # geom_text(aes(x=20, label="moyenne", y=0), colour="darkgreen", angle=0) +
  # geom_text(aes(x=22, label="médiane", y=0), colour="darkred", angle=0) 

# geom_text(aes(x=mean(df0$age-2), label="moyenne", y=-1300), colour="darkgreen", angle=0) +
# +  stat_bin(breaks = seq(-1500,1500,100))


# == boxplot sdelon la categorie pro' ====
g3 <- ggplot(df0, aes(y=age, x=Code.du.département..Maire., fill= Code.du.département..Maire.)) + geom_boxplot() + theme_minimal() +  theme(legend.position="none") +
   labs(title = "Boxplot des ages par departements" , x= "département", caption = "RNE data.gouv.fr") + theme(plot.title = element_text(hjust = 0.5))
  guides(col = guide_legend(nrow  = 20)) 
theme(legend.justification=c(0,1), legend.position=c(0.1,0.1))  
facet_wrap(~Code.du.département..Maire.) + 
+theme(axis.title.x=element_blank(), axis.text=element_text(size=8))

# Nombre de maires par CSP
ggplot(df0, aes(x= fct_rev(fct_infreq(Libellé.de.la.profession)))) +  geom_bar(stat="count") +
  coord_flip() + ggtitle("Catégorie socio pro des Maires") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Catégorie Socio-Pro") + ylab("nombre")

ggplot(df0, aes(x= fct_rev(fct_infreq(Libellé.de.la.profession)))) +  geom_bar(stat="count", aes(fill=Code.sexe)) +
  coord_flip() + ggtitle("Catégorie socio pro des Maires") + labs(fill = "Sexe") + 
  ylab("Catégorie Socio-Pro") + xlab("nombre") + theme(legend.justification=c(1,0), legend.position=c(0.80,0.05))


names(df0)
# Le nombre de maires par departement
ggplot(df0, aes(x= fct_rev(fct_infreq(Code.du.département..Maire.)))) +  geom_bar(stat="count", aes(fill=Code.sexe)) +
   ggtitle("Nombre de maires par département") + labs(fill = "Sexe") + 
  ylab("Nombre de maires") + xlab("Département") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.justification=c(1,0), legend.position=c(0.80,0.75))

# pyramide des ages par départements
df_dep <- count(df0, c('Code.du.département..Maire.', 'Code.sexe', 'age'))
df_dep$freq <- ifelse(df_dep$Code.sexe == "M" , -1*df_dep$freq, df_dep$freq)


ggplot(df_dep, aes(x=age, y=freq, fill= Code.sexe, color=Code.sexe)) +
  geom_bar(data=subset(df_dep,Code.sexe =="M"), stat="identity", width=1) +
  geom_bar(data=subset(df_dep,Code.sexe =="F"), stat="identity", width=1) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  coord_flip() + expand_limits(x=c(20,100), y=c(-150, 150)) +  facet_wrap(~ Code.du.département..Maire., scales="free") +
  scale_y_continuous(breaks = seq(-1500,1500,100), labels=abs(seq(-1500,1500,100))) + theme_minimal() +
  ggtitle("Pyramides des ages des maires") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Sexe") + ylab("Nombre") 
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + 
  geom_vline(xintercept = median(df0$age), linetype="dashed", color = "darkred", size = 1) + 
  geom_vline(xintercept = mean(df0$age), linetype="dashed", color = "green", size = 1)

  
  
  p <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
    geom_histogram() +
    gghighlight()
  p + facet_wrap(~ Species)
  
  df_bfc <- subset(df_dep, Code.du.département..Maire.==21 | Code.du.département..Maire.== 25 | Code.du.département..Maire.== 39 | Code.du.département..Maire.== 58 | Code.du.département..Maire.== 70 | Code.du.département..Maire.== 71 | Code.du.département..Maire.== 89 | Code.du.département..Maire.== 90)
  
ggplot(df_bfc, aes(x=age, y=freq, fill= Code.sexe, color=Code.sexe)) +
    geom_bar(data=subset(df_bfc,Code.sexe =="M"), stat="identity", width=1) +
    geom_bar(data=subset(df_bfc,Code.sexe =="F"), stat="identity", width=1) +
    scale_x_continuous(breaks = seq(0,100,10)) + gghighlight() + facet_wrap(~ Code.du.département..Maire., nrow=4) + coord_flip() + theme_bw() +
  scale_y_continuous(breaks = seq(-150,150,10), labels=abs(seq(-150,150,10)))
  
  