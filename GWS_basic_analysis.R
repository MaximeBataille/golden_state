library("BasketballAnalyzeR")
library(gridExtra)
library(dplyr)
data(package="BasketballAnalyzeR")

PbP <- PbPmanipulation(PbP.BDB)


#West conference Teams playing play offs
selteams <- which(Tadd$Conference == 'W' & Tadd$Playoff == 'Y')
length(selteams)

#Four Factors
FF.sel <- fourfactors(Tbox[selteams, ], Obox[selteams, ])
listPlots <- plot(FF.sel)

grid.arrange(grobs=listPlots[1], ncol=1)
grid.arrange(grobs=listPlots[2], ncol=1)
grid.arrange(grobs=listPlots[3], ncol=1)
grid.arrange(grobs=listPlots[4], ncol=1)

#Force de Golden State : Efficacité au tir, gros rythme
#Faiblesse de Golden State : Rebonds offensif et défensif, pas mal de pertes de balles. 

#analyse force au tir
X <- Tbox[selteams, ]
labs <- c("Percentage 3 pts", "Percentage 2 pts", "Percentage FT")
barline(data=X, id="Team", bars=c("P3p","P2p","FTp"),
          line="PTS", order.by="W", labels.bars=labs)


X <- data.frame(Tbox[selteams, ], FF.sel)
mypal <- colorRampPalette(c("blue","yellow","red"))

scatterplot(X, data.var=c("P2p","P3p"),
            labels=X$Team , palette=mypal)

scatterplot(X, data.var=c("P3A","P3p"),
            labels=X$Team , palette=mypal)

scatterplot(X, data.var=c("P3A","P2A"),
            labels=X$Team , palette=mypal)

#Gros pourcentages au tir mais peu de tentatives. car gros rythme et pertes de balles
#et peu de rebonds. mais la réussite au tir est tellement haute que ca compense, sauf 
#face à Houston en saison régulière (voir Offensive Rating)

attach(Tbox[selteams, ])
X <- data.frame(T=Team, P2p, P3p, FTp, W, AS=P2A+P3A+FTA)
detach(Tbox[selteams, ])
labs <- c("2-point shots",
          "3-point shots",
            "Games won",
          "Total shots attempted")
          
bubbleplot(X, id="T", x="P2p", y="P3p", col="W",
           size="AS", labels=labs)



#Analyse perte de balles, passes décisives
selTeams <- subset(Tadd, Conference == 'W' & Playoff == 'Y')$Team
selTeams <- as.character(selTeams)
Pbox.sel <- Pbox %>%
  filter(Team %in% selTeams) %>%
  filter(MIN >= 500)

attach(Pbox.sel)
X <- data.frame(AST, TOV, PTS) / MIN
detach(Pbox.sel)

mypal <- colorRampPalette(c("blue","yellow","red"))
SAS <- which(Pbox.sel$Team=="Golden State Warriors")
scatterplot(X, data.var=c("AST","TOV"), z.var="PTS",
            labels=Pbox.sel$Player, palette=mypal,
            subset = SAS)


FF.sel
scatterplot(FF.sel, data.var = c('F3.Off', 'F3.Def'),
            labels = FF.sel$Team)


#Comprendre comment cette équipe fait pour être aussi forte au tir


#La vraibilité à trois points est plus grande à Golden State sur le taux de réussite

#On va tenter de comprendre quels sont les joueurs qui ont le plus de poids dans cette
#réussite
#graph inégalités 3points mis
Tbox.WP <- Tbox[selteams, ]
no.teams <- nrow(Tbox.WP)
INEQ_P3M <- array(0, no.teams)
INEQ_P3p <- array(0, no.teams)
for (k in 1:no.teams) {
  Teamk <- Tbox.WP$Team[k]
  Pbox.sel <- subset(Pbox, Team==Teamk)
  
  index <- inequality(Pbox.sel$P3M, npl=8)
  INEQ_P3M[k] <- index$Gini
  
  index <- inequality(Pbox.sel$P3p, npl=8)
  INEQ_P3p[k] <- index$Gini
}

dts <- data.frame(T = Tbox.WP$Team, INEQ_P3M, INEQ_P3p, P3M=Tbox.WP$P3M, P3p = Tbox.WP$P3p)
mypal <- colorRampPalette(c("blue","red"))
labs = c("Inequality 3 pts made",
         "Inequality 3 pts percentage",
         "3 pts percentage",
         "3 pts made")
bubbleplot(dts,id = 'T', x = "INEQ_P3M", y = "INEQ_P3p",
              labels=labs, col = 'P3p',
              size = 'P3M')


attach(Pbox.GSW)
Pbox.GSW <- Pbox.GSW[order(-P3M), ]
head(Pbox.GSW, 4)$Player


PbP.GSW <- subset(PbP, team=="GSW")
netdata <- assistnet(PbP.GSW)
set.seed(7)
plot(netdata, layout="circle", edge.thr=20)


#Comprendre l'organisation de l'équipe
PbP.GSW <- subset(PbP, team=="GSW")
PbP.GSW2 <- subset(PbP, team=="GSW" & ShotType == '2P')
PbP.GSW3 <- subset(PbP, team=="GSW" & ShotType == '3P')


netdata3 <- assistnet(PbP.GSW3)
set.seed(1)
plot(netdata3, layout = "circle", edge.thr = 10)


plot(netdata3, layout="circle", edge.thr=10,
     node.col="FGM", node.size="FGM_ASTp")


#Comprendre d'où sont tirés les shoots à trois points
PbP.GSW3$xx <- PbP.GSW3$original_x/10
PbP.GSW3$yy <- PbP.GSW3$original_y/10 - 41.75

KT <- subset(PbP.GSW3, player=="Klay Thompson")
SC <- subset(PbP.GSW3, player=="Stephen Curry")
KD <- subset(PbP.GSW3, player=="Kevin Durant")

shotchart(data=KT, x="xx", y="yy", type="density-raster",
          scatter=TRUE, pt.col="tomato", pt.alpha=0.1)

shotchart(data=SC, x="xx", y="yy", type="density-raster",
          scatter=TRUE, pt.col="tomato", pt.alpha=0.1)

shotchart(data=KD, x="xx", y="yy", type="density-raster",
          scatter=TRUE, pt.col="tomato", pt.alpha=0.1)

#Scoring probability 

pl1 <- c("Kevin Durant","Stephen Curry","Klay Thompson")

p1 <- scoringprob(data=PbP.GSW, shot.type="3P",
                  players=pl1, var="playlength",
                  col.team="gray")

p2 <- scoringprob(data=PbP.GSW, shot.type="3P",
                  players=pl1, var="totalTime",
                  col.team="gray", bw = 1500)

#scoring probability : top - bottom team
PbP <- PbPmanipulation(PbP.BDB)
top <- subset(Tadd, Playoff=="Y" & team!="GSW")$team
bot <- subset(Tadd, Playoff=="N")$team

bot_top <- function(X, k) {
  dts <- subset(subset(X, oppTeam %in% get(k)),
                team=="GSW")
  dts$player <- paste(dts$player, k)
  return(dts)   
  }

PbP.GSW <- rbind(bot_top(PbP, "top"), bot_top(PbP, "bot"))

pl <- c("Stephen Curry top","Stephen Curry bot",
        "Kevin Durant top", "Kevin Durant bot", 
        "Klay Thompson top", "Klay Thompson bot")

mypal <- colorRampPalette(c("green","orange", "black"))

p3 <- scoringprob(data=PbP.GSW, shot.type="3P",
                  players=pl, var="playlength",
                  col.team="gray", palette = mypal)

p4 <- scoringprob(data=PbP.GSW, shot.type="3P",
                  players=pl, var="totalTime",
                  col.team="gray", bw = 1500, palette = mypal)
