library(DataComputing)
GSS%>%
  filter(NATARMS==1, YEAR>=2000)%>%
  group_by(YEAR,PARTYID)%>%
  summarise(total=n())%>%
  ggplot(aes(x=YEAR, y=total))+geom_line()+facet_wrap(~PARTYID)

GSS%>%
  filter(YEAR>=2000)%>%
  group_by(YEAR, PARTYID)%>%
  ggplot(aes(x=PARTYID))+geom_histogram(binwidth = 1)+facet_wrap(~YEAR)

GSS1<-GSS%>%
  select(-OWNGUN)%>%
  filter(YEAR==c(2000:2005))


correlation<- function(x,y){
  cov(x,y)/(sd(x)*sd(y))
}

correlation(GSS1$YEAR,GSS1$PARTYID)
correlation(GSS1$YEAR,GSS1$NATARMS)
