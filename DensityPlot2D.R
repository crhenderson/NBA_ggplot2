###############################################################################
#
# Building a 2D Density Plot
# Comparing Spurs and Warriors
#
###############################################################################


library(XML)
library(dplyr)

rm(list=ls())
X<-list()
offset <- seq(0,4*82,100)
offset <- ifelse(offset==0,0,offset-1)
tms <- c("SAS","GSW","OKC")
n <- length(offset)
pb <- txtProgressBar(min = 0, max = n, style = 3)
for(i in 1:n) {
    Sys.sleep(0.1)
  Y.cache <- data.frame()
  for(j in tms) {
    #Pro-Football reference is missing week 2 of the 2001 season
    #if(i != 4 | j!=2) {
    url<-paste('http://www.basketball-reference.com/play-index/tgl_finder.cgi?request=1&match=game&lg_id=NBA&year_min=2013&year_max=2016&team_id=',j,'&opp_id=&is_playoffs=N&round_id=&best_of=&team_seed_cmp=eq&team_seed=&opp_seed_cmp=eq&opp_seed=&is_range=N&game_num_type=team&game_num_min=&game_num_max=&game_month=&game_location=&game_result=&is_overtime=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c5val=&order_by=date_game&order_by_asc=Y&offset=',offset[i],sep='')
    Y<-readHTMLTable(url, stringsAsFactors = FALSE, header=T, skip.rows=1)
    #}
    Y <- Y$stats
    names(Y) <- c("Rk","Date","Tm","Loc","Opp","Result","MP","Tm.FG","Tm.FGA","Tm.FGP","Tm.2P","Tm.2PA",
                  "Tm.2PP","Tm.3P","Tm.3PA","Tm.3PP","Tm.FT","Tm.FTA","Tm.FTP","Tm.PTS",
                  "Opp.FG","Opp.FGA","Opp.FGP","Opp.2P","Opp.2PA",
                  "Opp.2PP","Opp.3P","Opp.3PA","Opp.3PP","Opp.FT","Opp.FTA","Opp.FTP","Opp.PTS")
    Y <- Y[Y$Rk != 'Rk',]
    Y.cache <- rbind(Y.cache,Y)
  }
    X[[i]]<-Y.cache
    setTxtProgressBar(pb, i)
  }
close(pb)

to.numeric <- function(x) {as.numeric(as.character(x))}

Y<-do.call("rbind",lapply(X, 
                          function(Data) {
                              select(Data,-Result) %>%
                              mutate(Tm = factor(Tm, levels = c('SAS','GSW','OKC'),labels = c('Spurs','Warriors','Thunder')),
                                     WL = factor(ifelse(to.numeric(Tm.PTS) > to.numeric(Opp.PTS),'Win','Loss'),levels=c('Win','Loss')),
                                     OT = factor(ifelse(to.numeric(MP)>240,1,0),levels=c(0,1)),
                                     Loc = factor(ifelse(Loc == '@','Away','Home'),levels=c('Home','Away')),
                                     Date = as.Date(Date,format='%Y-%m-%d'),
                                     Rk = to.numeric(Rk),
                                     Opp = factor(Opp),
                                     Diff = to.numeric(Tm.PTS) - to.numeric(Opp.PTS)
                              )%>%
                              arrange(Rk)
                          }
))%>%
  unique()

for(i in 6:32) {
  Y[,i] <- to.numeric(Y[,i])
}

###########################################################################

library(ggplot2)

text.df <- data.frame(Text = c("Games Won","Games Lost"), x=c(75,137.5),y=c(137.5,75),Rk=1)
Data <- aggregate(Rk~Tm + Loc + Tm.PTS + Opp.PTS,Y,length)
Data$Weight <- factor(Data$Rk)

png("Density2D.png",height=900,width=1600)
ggplot(Data, aes(x=Opp.PTS,y=Tm.PTS,weight=Rk)) +
  stat_density2d(aes(fill=Loc,alpha=.2),geom='polygon') +
  geom_point(aes(shape=Weight)) +
  coord_cartesian(c(60, 150), c(60, 150)) +
  geom_abline(slope=1,intercept=0,lty=1,size=1) +
  geom_vline(xintercept=100,lty=2) +
  geom_hline(yintercept=100,lty=2) +
  facet_grid(Loc~Tm) +
  guides(alpha=F) +
  geom_text(data=text.df,aes(x=x,y=y,label=Text),size=5) +
  ggtitle("2D Density Plots:\nGame Scores\nSpurs Vs Warriors Vs Thunder") +
  xlab("Opponent Points") +
  ylab("Team Points") +
  theme_bw()
dev.off()


###############################################################################

medians <- aggregate(Diff~Tm + Opp + Loc,Y,median)
medians <- subset(medians, medians$Opp %in% c("SAS","GSW","OKC","CLE","LAC","TOR","MIA","HOU","IND","ATL","CHI","DAL"))

g3 <- ggplot(medians, aes(x=Loc, y=Diff, col=Opp, group = Opp)) +
  geom_line() +
  geom_point(shape=21,fill="white") +
  facet_wrap(~Tm) +
  geom_text(aes(label = Opp,hjust = ifelse(Loc == "Home", 1.5,-1.2))) +
  scale_y_continuous(breaks=seq(-12,24,2)) +
  geom_hline(yintercept = 0, linetype="dashed", size=0.5, alpha=.3) +
  ylab("Median Point Differential") +
  xlab("Home vs Away") + 
  ggtitle("Median Point Differential Per Game\nBy Opponent")

png("PointsByOpp.png",height=900,width=1600)  
g3
dev.off()





