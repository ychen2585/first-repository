library(ggplot2)
library(ggrepel)
library(dplyr)
library(socviz)

data <- tibble(names=c("hailey","thamyres","abby"),
               chocolate=c(1,2,5),
               coffee=c(10,0,1))
data
ggplot(data)+
  geom_point(aes(chocolate,coffee))+
  geom_text(aes(x=chocolate,y=coffee,label=names))+
  geom_hline(yintercept = 6)+
  geom_vline(xintercept = 4, color="red", linetype="dashed")

ggsave(filename = "1.jpg",dpi = 600)

#elections_historic
ggplot(elections_historic,aes(popular_pct,ec_pct))+
  geom_point()+
  geom_text(aes(x=popular_pct,y=ec_pct,label = winner,color=two_term))+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 0.5)


data2 <- filter(data, coffee >5)

ggplot() +
  geom_point(data, mapping=aes(chocolate,coffee))+
  geom_text_repel(data2,mapping=aes(x=chocolate,y=coffee,label = names))


elections_historic

name <- filter(elections_historic,winner== "Donald Trump" |winner== "Barack Obama")
ggplot()+
  geom_point(elections_historic,mapping=aes(popular_pct,ec_pct,color=two_term))+
  geom_text_repel(name, mapping=aes(x=popular_pct,y=ec_pct,label = winner))+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 0.5)

#organdata
organdata

data3 <- organdata %>% group_by(country) %>% summarize(health_mean=mean(health,na.rm= TRUE),gdp_mean= mean(gdp,na.rm=TRUE))

gdpdata<- filter(data3,gdp_mean>25000 | gdp_mean<20000)

ggplot()+
  geom_point(data3,mapping=aes(gdp_mean,health_mean))+
  geom_text_repel(gdpdata, mapping=aes(x=gdp_mean,y=health_mean,label=country))
  
ggplot(data)+
  geom_point(aes(chocolate,coffee))+
  geom_text(aes(x=chocolate,y=coffee,label=names))+
  annotate(geom = "rect",xmin=1.5, xmax=2.5,ymin = -1, ymax = 1,
           alpha=0.2, color="red")+
  geom_text_repel(aes(x=chocolate,y=coffee, label=names))

ggplot(data)+
  geom_point(aes(chocolate,coffee))+
  annotate(geom="text",label="low caffiene",x=2, y=1.25)+
  geom_text_repel(aes(x=chocolate,y=coffee,label=names))+
  theme(plot.title = element_text(face = "ilatic"))

ggplot(data3)+
  geom_point(aes(gdp_mean,health_mean))+
  annotate(geom = "rect",xmin=25000,xmax=30000,ymin=2000,ymax=4500,alpha=0.2,fill="grey", color="red")+
  geom_text_repel(data=gdpdata,aes(x=gdp_mean,y=health_mean,label=country))

mpg
ggplot(mpg,aes(displ,hwy,color=drv))+
  geom_point()+
  labs(title = "car mileage statistics",
       subtitle = "Something about them",
       caption = "Data from R",
       x= "engine displacement",
       y="highway miles per gallon")+
  theme(plot.subtitle = element_text(hjust=0.5),
        panel.background = element_rect(fill="lightblue"),
        panel.grid.major= element_line(color="darkgreen",linewidth = 1))


ggplot(mpg,aes(displ,hwy,color=drv))+
  geom_point()+
  labs(title = "car mileage statistics",
       subtitle = "Something about them",
       caption = "Data from R",
       x= "engine displacement",
       y="highway miles per gallon")+
  theme(axis.text = element_text(face="italic"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "pink",fill=NA),
        plot.background = element_rect("grey"))


ggplot(iris,aes(Sepal.Length,Sepal.Width,color=Species,shape = Species))+
  geom_point(,alpha=0.8)+labs(title = "Properties of Flowers")+
  theme(legend.position="bottom",
        plot.title=element_text(face="bold",hjust=0.5),
        panel.border = element_rect(color = "black",fill=NA,linewidth = 1),
        panel.grid.major = element_line(color="black"),
        plot.subtitle = element_text(hjust=0.5),
        panel.background = element_rect(fill="white"))
ggsave(filename = "nice.pdf",dpi = 600)









