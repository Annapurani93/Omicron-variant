library(tidyverse)
library(readxl)
library(patchwork)
read_excel("Omicron.xlsx")->data

data$Day<-as.Date(data$Day)
data
data%>%
  select(Day,Entity,Omicron,Others)

data%>%
  gather("Variant","Value",3:4)%>%
  group_by(Entity)%>%
  data.frame()->data1

data1%>%
  filter(Entity=="United States")%>%
  ggplot(aes(x = "", y = Value,fill=Variant)) +
  geom_col() +
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("red","gray30"))+
  facet_wrap(~Day,ncol=5)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="UNITED STATES",
       subtitle = str_wrap("Share of Omicron cases of the total infections",120))->US


data1%>%
  filter(Entity=="India")%>%
  ggplot(aes(x = "", y = Value,fill=Variant)) +
  geom_col() +
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("red","gray30"))+
  facet_wrap(~Day,ncol=5)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="INDIA",
       subtitle = str_wrap("Share of Omicron cases of the total infections",120))->India


data1%>%
  filter(Entity=="Brazil")%>%
  ggplot(aes(x = "", y = Value,fill=Variant)) +
  geom_col() +
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("red","gray30"))+
  facet_wrap(~Day,ncol=5)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="BRAZIL",
       subtitle = str_wrap("Share of Omicron cases of the total infections",120))->Brazil

data1%>%
  filter(Entity=="United Kingdom")%>%
  ggplot(aes(x = "", y = Value,fill=Variant)) +
  geom_col() +
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("red","gray30"))+
  facet_wrap(~Day,ncol=5)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="UNITED KINGDOM",
       subtitle = str_wrap("Share of Omicron cases of the total infections",120))->uk

data1%>%
  filter(Entity=="France")%>%
  ggplot(aes(x = "", y = Value,fill=Variant)) +
  geom_col() +
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("red","gray30"))+
  facet_wrap(~Day,ncol=5)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="white",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="FRANCE",
       subtitle = str_wrap("Share of Omicron cases of the total infections",120))->France

US+India+Brazil+uk+France+plot_layout(nrow=5)&
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, face="bold",colour="white",margin=margin(b=21)),
        plot.caption = element_text(size=10, colour="white",hjust=0,margin=margin(t=25)))&
  plot_annotation(
    title = "SHARE OF OMICRON INFECTIONS SURGE IN THE US, UK AND FRANCE",
    subtitle = str_wrap("The number of infections of the Omicron variant has surged over the last three months in the top five countries with the highest number of cumulative Covid cases. But in the US, the UK and France, more than 80% cases are now of the Omicron variant",120),
    caption = "Source: Our World in Data|Design and Analysis: @annapurani93",
  )->plot
  

ggsave("Omicron.png",plot,width=12,height = 16)
