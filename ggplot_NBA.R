#################################################################################################

# ggplot2 Tutorial

#################################################################################################



# Getting the Data --------------------------------------------------------

rm(list=ls())

options(scipen=999)
library(SportsAnalytics)
library(dplyr)

## Data consists of NBA player statistics for the 2013-2014 season
data<-fetch_NBAPlayerStatistics(season="15-16")
data$Position <- factor(data$Position, levels=c('PG','SG','SF','PF','C'))
data$FBCourt <- factor(ifelse(data$Position %in% c('PG','SG'),'Backcourt','Frontcourt'))


## Get Per Game Stats  
calc.per.game <- function(var) {var/data$GamesPlayed}

data <- data %>%
  mutate(Name.Pos = paste(Position,Name),
         Minutes.PG = calc.per.game(TotalMinutesPlayed),
         Points.PG = calc.per.game(TotalPoints),
         FG.PG = calc.per.game(FieldGoalsMade),
         FGAttempts.PG = calc.per.game(FieldGoalsAttempted),
         Three.PG = calc.per.game(ThreesMade),
         ThreeAttempts.PG = calc.per.game(ThreesAttempted),
         FT.PG = calc.per.game(FreeThrowsMade),
         FTAttempts.PG = calc.per.game(FreeThrowsAttempted),
         Offensive.Rebounts.PG = calc.per.game(OffensiveRebounds),
         Total.Rebounds.PG = calc.per.game(TotalRebounds),
         Assists.PG = calc.per.game(Assists),
         Steals.PG = calc.per.game(Steals),
         Turnovers.PG = calc.per.game(Turnovers),
         Blocks.PG = calc.per.game(Blocks),
         Fouls.PG = calc.per.game(PersonalFouls))

data <- data[!is.na(data$Position) & data$TotalMinutesPlayed >= 500,]

data$GamesPlayed.Quartile <- cut(data$GamesPlayed,quantile(data$GamesPlayed),include.lowest = T)


# ggplot2 Introduction ----------------------------------------------------

library(ggplot2)

##Scatter Plots
#       ggplot(data = , mapping = aes()) +
#           theme() +
#           geometry +
#           features
#

# Set global features of plot
g1 <- ggplot(data=data, aes(x=FTAttempts.PG,y=FT.PG,colour=FBCourt))

g1

# Add axis labels and title
g1 <- g1  +
  ggtitle("FT Made by FT Attempts\nPer Game Minutes") +
  xlab("FT Attempts Per Game") +
  ylab("FT Made Per Game") +
  theme_bw()

g1

# Add geometries (points, lines, polygons, boxplots, barplots, etc.)
g1 + geom_point()

# Can add aesthetics to geometries
g1 <- g1 + geom_point(aes(shape=Position))

g1

##Add smoothers
g1 + geom_smooth()

g1 + geom_smooth(method="lm")

##Faceting
g1 + geom_smooth() + 
  facet_wrap(~Position)

## Faceting on 2 Variables
g1 + geom_smooth(se=FALSE) +
  facet_grid(GamesPlayed.Quartile~FBCourt)


## Label outliers
labels <- data %>%
  select (Name, FBCourt, Position, FTAttempts.PG, FT.PG, FreeThrowsAttempted, FreeThrowsMade) %>%
  mutate(Per = FreeThrowsMade/FreeThrowsAttempted) %>%
  mutate(Label = ifelse(FTAttempts.PG > 5 & Per < .6, Name,"")) %>%
  select(-Name)

g1 + geom_smooth() + 
  facet_grid(.~FBCourt) +
  geom_text(data=labels, aes(x=FTAttempts.PG,y=FT.PG,label=Label),vjust = 1.5,col='black') +
  coord_fixed(ratio=1)

################################################################################

#Box Plots

g2 <- ggplot(data, aes(x=Position,y=Points.PG)) +
  ggtitle("Boxplot: Position vs Points Per Game") +
  ylab("Points Per Game")

g2

g2 <- g2 + geom_boxplot()

g2
  
## Add Jitter Plot
g2 <- g2 + 
  geom_jitter(aes(color=FBCourt, shape=GamesPlayed.Quartile))
g2

## Add Mean
g2 + 
  stat_summary(fun.y=mean,geom="point",shape=5,size=4)

################################################################################

##Bar plots

means <- aggregate(Points.PG~Position,data,mean)$Points.PG
sds <- aggregate(Points.PG~Position,data,sd)$Points.PG
counts <- aggregate(Points.PG~Position,data,length)$Points.PG

ciu <- means + qt(p=.975, df=(counts-1)) * sds/sqrt(sds)
cil <- means - qt(p=.975, df=(counts-1)) * sds/sqrt(sds)

tp.df <- data.frame(Position = levels(data$Position), means, ciu,cil)
tp.df$Position <- factor(tp.df$Position, levels = c("PG","SG","SF","PF","C"))

g3 <- ggplot(tp.df, aes(x=Position, y=means)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=cil,ymax=ciu),size=.3,width=.2) +
  geom_text(aes(label = round(means,1)),hjust=1.5, vjust=-.2) +
  geom_text(aes(y=ciu,label = round(ciu,1)),vjust=-.2) +
  geom_text(aes(y=cil,label = round(cil,1)),vjust=1) +
  ylab("Points Per Game") +
  ggtitle("Average Points Per Game with 95% Error Bars")

################################################################################

##Some Fun

## Only going to focus on top 50 Minutes Per Game
top50 <- head(data[order(data$Minutes.PG,decreasing=T),],50)

##Limit to just Per Game Stats
PerGame <- top50[,c(2:4,26:42)]


##Do some hierarchical clustering
clust <- PerGame[,7:20]

clust <- scale(clust)

require(graphics)
d <- dist(clust,method="euclidean")
fit <- hclust(d,method="ward.D2")

##Plot Dendrogram
png("cluster_dg.png",height = 900, width = 1600)
plot(fit,labels=PerGame$Name.Pos,cex=.6)
rect.hclust(fit, k=7, border="red") 
dev.off()

##Reorder data and add clusters
memb <- cutree(fit,7)
ord <- fit$order

PerGame$Cluster <- memb
PerGame$Cluster <- factor(PerGame$Cluster)


##Rearange data into long format
PerGame <- PerGame[ord,]

PerGameLong <- PerGame

PerGameLong[,7:20] <- scale(PerGameLong[,7:20])

library(tidyr)

PerGameLong <- gather(PerGameLong,Statistic, Value, Points.PG:Fouls.PG)
PerGameLong$NamePos <- factor(PerGameLong$Name.Pos, levels = unique(PerGame$Name.Pos))

##Plot heat map
p <- ggplot(PerGameLong, aes(Statistic, NamePos)) + 
  geom_tile(aes(fill=Value,col=Cluster),size=1) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  guides(col=FALSE) +
  scale_fill_gradient(low="white",high="steelblue") +
  ggtitle("Heatmap: Per Game Statistics\nTop 50 Players by Minutes Played Per Game") +
  coord_flip()
  
png("heat_map.png",height=900,width=1600)
p
dev.off()


