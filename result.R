SIVAPRASAD PR

AIM

Perform 'Exploratory Data Analysis' on dataset 'Indian Premier League'

OBJECTIVE 

1.Find out the most successful teams,players
2.Suggest a team/player a company should endorse

PROCEDURE AND CODE

First we need to read the data sets. 
we have two separate data sets for analysis.Let's read it using the below commands

```{r}
 matches <- read.csv("/home/siva/Downloads/matches.csv")
 del <- read.csv("/home/siva/Downloads/deliveries.csv")
```

now we have the data sets.let's perform some basic analysis to check the dimensions like number of rows,columns and to know how much space the data is occupying.

```{r}
dim(matches)
dim(del)
object.size(matches)
object.size(del)
```

next we are going to analyze the names,look into some entries and the summary to get more info about the data.

```{r}
names(matches)
names(del)
head(matches)
head(del)
summary(matches)
summary(del)
```

now we have completed the basic steps of data analysis.
since we have two data sets we can perform EXPLORATORY DATA ANALYSIS(EDA) on two sets separately and later on we can combine them for more.
in order to perform EDA we need to add some libraries 

```{r}
library(ggplot2)
library(magrittr)#for using group by
library(dplyr)
```


EDA ON DATA SET - MATCHES

let's analyze this on various aspects.

1) Does Toss winner have any advantage over winning a match

```{r}
toss<-data.frame(matches$team1,matches$team2,matches$toss_winner,matches$winner)
tossdesc<-ifelse(as.character((matches$toss_winner))==as.character((matches$winner)),"won","lose")
toss<-data.frame(toss,tossdesc)
ggplot(toss[which(!is.na(toss$tossdesc)),],aes(tossdesc,fill=tossdesc))+geom_bar()+xlab("TOSS")+ylab("Matches Won")+ggtitle("DOES TOSS WINNING HAVE ANY ADVANTAGE")
```

from the analysis we can conclude that Toss winning gives some advantage to a Team.

2)Does Home play give any advantage for winning a match 

```{r}
teamcity<-matches[matches$season!="2009",]
teamcity$date<- as.Date(teamcity$date)
teamcity2<-teamcity[teamcity$date < as.Date("2014-04-16") | teamcity$date > as.Date("2014-04-30"),]
teamcity2$hometeam[teamcity2$city=="Bangalore"]<- "Royal Challengers Bangalore"
teamcity2$hometeam[teamcity2$city=="Chennai"]<- "Chennai Super Kings"
teamcity2$hometeam[teamcity2$city=="Delhi"]<- "Delhi Daredevils"
teamcity2$hometeam[teamcity2$city=="Chandigarh"]<- "Kings XI Punjab"
teamcity2$hometeam[teamcity2$city=="Jaipur"]<- "Rajasthan Royals"
teamcity2$hometeam[teamcity2$city=="Mumbai"]<- "Mumbai Indians"
teamcity2$hometeam[teamcity2$city=="Kolkata"]<- "Kolkata Knight Riders"
teamcity2$hometeam[teamcity2$city=="Kochi"]<- "Kochi Tuskers Kerala"
teamcity2$hometeam[teamcity2$city=="Hyderabad" & teamcity2$season <=2012]<- "Deccan Chargers"
teamcity2$hometeam[teamcity2$city=="Hyderabad" & teamcity2$season >2012]<- "Sunrisers Hyderabad"
teamcity2$hometeam[teamcity2$city=="Ahmedabad"]<- "Rajasthan Royals"
teamcity2$hometeam[teamcity2$city=="Dharamsala"]<- "Kings XI Punjab"
teamcity2$hometeam[teamcity2$city=="Visakhapatnam" & teamcity2$season== 2015]<- "Sunrisers Hyderabad"
teamcity2$hometeam[teamcity2$city=="Ranchi" & teamcity2$season== 2013]<- "Kolkata Knight Riders"
teamcity2$hometeam[teamcity2$city=="Ranchi" & teamcity2$season > 2013]<- "Chennai Super Kings"
teamcity2$hometeam[teamcity2$city=="Rajkot" ]<- "Gujarat Lions"
teamcity2$hometeam[teamcity2$city=="Kanpur" ]<- "Gujarat Lions"
teamcity2$hometeam[teamcity2$city=="Raipur" ]<- "Delhi Daredevils"
teamcity2$hometeam[teamcity2$city=="Nagpur" ]<- "Deccan Chargers"
teamcity2$hometeam[teamcity2$city=="Indore" ]<- "Kochi Tuskers Kerala"
teamcity2$hometeam[teamcity2$city=="Pune" & teamcity2$season!= 2016]<- "Pune Warriors"
teamcity2$hometeam[teamcity2$city=="Pune" & teamcity2$season== 2016]<- "Rising Pune Supergiants"
teamcity2<-teamcity2[ which(!is.na(teamcity2$hometeam)),]
teamcity2$result<-ifelse(as.character((teamcity2$winner))==as.character((teamcity2$hometeam)),"home","away")
ggplot(teamcity2[which(!is.na(teamcity2$result)),],aes(result,fill=result))+geom_bar()+xlab("Ground")+ylab("Matches Won")+ggtitle("DOES HOME GROUND GIVE ANY ADVANTAGE")

```

from the analysis the probability of match played in home and away grounds are almost same.so home play cannot be consider as an advantage

3)Total Number of matches won by each team in IPL

```{r}
ggplot(matches,aes(winner))+geom_bar(fill="brown")+theme(axis.text.x = element_text(angle = 90,))+xlab("Teams")+ylab("Number of matches won")+ggtitle("Total No of matches won by each team")

```
from the bar plot we can analyze that team MI,CSK and KKR are among the top three teams. 

4)Number of matches lost by each team in IPL

```{r}
losers<-table(matches$team1)+table(matches$team2)
b<-table(matches$winner)
losers<-c(4,losers)
losec<-losers-b
losec<-data.frame(losec)
names(losec)<-c("team","count")
ggplot(losec,aes(x=team,y=count))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90))+ylab("No of matches Lost")+ggtitle("NO OF MATCHES LOST BY EACH TEAM")
```
5) Win By Runs 

```{r}
f<-data.frame(winner=matches$winner,runs=matches$win_by_runs)
f[f=='0']<-NA
f<-na.omit(f)
ggplot(f, aes(x=1,y=runs)) +
  geom_point(col="red") +
  facet_wrap(~winner)+
  scale_color_viridis_d()+geom_hline(yintercept = 75)+ggtitle("WIN BY RUNS")
```

6) Win By Wickets

```{r}
fw<-data.frame(winner=matches$winner,wicket=matches$win_by_wickets)
fw[fw=='0']<-NA
fw<-na.omit(fw)
ggplot(fw, aes(x=1,y=wicket)) +
  geom_point(col="green") +
  facet_wrap(~winner)+
  scale_color_viridis_d()+ggtitle("WIN BY WICKETS")
```

from these analysis we can conclude that the most successful teams in IPL are :

          1. Mumbai Indians
          2. Chennai Super Kings
          3. Kolkata Knight Riders

Now let's analyze the data set del.

8) TOP BATSMAN

```{r}
del %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs))%>% arrange(desc(runs))%>% filter(runs > 3000)%>%ggplot(aes(reorder(batsman,+runs),y=runs,fill=batsman))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90))+xlab("Batsman")+ylab("runs scored")+ggtitle("Top batsman with score > 3000")+guides(fill=F)

```
9) TOP BOWLERS

```{r}
del %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed))%>%filter(wickets>150)%>%ggplot(aes(reorder(bowler,+wickets),y=wickets,fill=bowler))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=90))+xlab("Bowler")+ylab("wickets")+ggtitle("Top bowlers with wickets > 100")+guides(fill=F)

```

10) Contributions given by top batsman

```{r}
colnames(matches)[1]<- "match_id"
del %>% left_join(matches) %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma") %>% filter(result == "normal") %>%
  group_by(batsman,match_id,batting_team,winner) %>%  summarise(Runs=sum(batsman_runs)) %>% ggplot(aes(match_id,Runs))+geom_line()+geom_smooth()+facet_wrap(~batsman)+ggtitle("CONTRIBUTIONS OF TOP BATSMAN FOR THIER TEAM ")
```
11) Contributions of top batsman for winning and losing

```{r}
del %>% left_join(matches) %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma") %>% filter(result == "normal") %>%
  group_by(batsman,match_id,batting_team,winner) %>%  summarise(Runs=sum(batsman_runs)) %>% ggplot(aes(match_id,Runs))+geom_line()+geom_smooth()+facet_grid(batsman ~ifelse(as.character(batting_team)== as.character(winner),"CONTRIBUTION FOR WIN","CONTRIBUTION FOR LOSE"))+ggtitle("CONTRIBUTIONS OF TOP BATSMAN FOR THIER TEAM ")
```

12)Seasonal Runs

```{r}
del %>% left_join(matches)%>% group_by(batsman,season) %>% filter(batsman == "V Kohli"| batsman == "SK Raina" | batsman == "RG Sharma") %>% summarise(match_id,batsman,season,batsman_runs,seasonal_runs = sum(batsman_runs))%>%ggplot(aes(season,seasonal_runs,col=batsman))+geom_line()+ scale_x_continuous(breaks = 2008:2019)+ggtitle("SEASONAL RUNS ANALYSIS")
```

13)Seasonal Boundaries

```{r}
del %>% left_join(matches)%>% filter(batsman == "V Kohli"| batsman == "SK Raina" | batsman == "RG Sharma") %>% filter(batsman_runs == 4| batsman_runs == 6) %>%  group_by(batsman,season) %>% summarise(match_id,batsman,season,batsman_runs,boundary = length(batsman_runs))%>%ggplot(aes(season,boundary,col=batsman))+geom_line()+ scale_x_continuous(breaks = 2008:2019)+ggtitle("SEASONAL BOUNDARY ANALYSIS")

```

RESULT

from the analysis we can conclude that 

Top Batsman

1.V Kohli
2.SK Raina
3.RG Sharma

Top Bowlers

1.SL Malinga
2.DJ Bravo
3.A Mishra
Team a company should endorse

Mumbai Indians

Player a company should endorse

V Kohli

