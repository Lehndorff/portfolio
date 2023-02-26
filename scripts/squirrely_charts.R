library(nycsquirrels18)
library(tidyverse)

data("squirrels")
#Geographic distribution of squirrels
squirrels %>%
  mutate(above_ground_sighter_measurement=as.numeric(above_ground_sighter_measurement)) %>% 
  filter(!is.na(above_ground_sighter_measurement)) %>% 
  ggplot(aes(x=lat,y=above_ground_sighter_measurement))+
  geom_jitter(alpha=.2)+
  geom_rug()

squirrels %>% 
  ggplot()+
  geom_density2d(aes(y=lat,x=long))+
  theme_minimal()+
  coord_map()

squirrels %>% 
  ggplot()+
  geom_bin2d(aes(y=lat,x=long))+
  theme_minimal()+
  coord_map()+
  labs(
    x=NULL,y=NULL,
    fill="Count of\nSquirrels",subtitle="Heat Map of Squirrel Sightings in Central Park",title='Squirrel Town',
    caption = 
      "This plot attempts to answer where(on a map)\nsquirrels were most commonly observed.\nThe highest concentration of squirrel sightings\nwas observed at the central north end of the park.")+
  theme(
    legend.position = 'bottom',
    plot.caption = element_text(hjust = 0)
    )
# ggsave("~/Desktop/squirrel_town.pdf",width=6.5,height=9.5)

#squirrel behaviors
squirrels %>%
  filter(!is.na(location)&age%in%c("Adult","Juvenile")) %>%
  group_by(date,age) %>%
  summarise(n=n()) %>%
  ggplot()+
  geom_line(aes(x=date,y=n,color=age))

squirrels %>%
  mutate(age=ifelse(!age%in%c("Adult","Juvenile"),"Unknown",age)) %>%
  ggplot()+
  geom_bar(aes(x=age,color=foraging),position = 'dodge')

squirrels %>%
  mutate(age=ifelse(!age%in%c("Adult","Juvenile"),"Unknown",age)) %>%
  ggplot()+
  geom_bar(aes(x=age,fill=foraging),position = 'fill')

squirrels %>%
  mutate(age=ifelse(!age%in%c("Adult","Juvenile"),"Unknown",age)) %>%
  ggplot()+
  geom_bar(aes(fill=age,x=foraging),position = 'fill')

activity_data<-NULL
for(i in c("running", "chasing", "climbing", "eating", "foraging")){

  act_dat<-squirrels %>% 
    select(unique_squirrel_id,running:foraging) %>% 
    rename(int=i) %>% 
    pivot_longer(-c(int,unique_squirrel_id)) %>% 
    filter(int==TRUE) %>% 
    group_by(primary=i,name) %>% 
    summarise(freq=mean(value))
  
  act_dat<-bind_rows(
    act_dat,
    data.frame(primary=i,name=i,freq=1-sum(act_dat$freq))
    )
  activity_data<-bind_rows(activity_data,act_dat)
}

activity_data %>% 
  mutate(secondary=ifelse(name==primary,"No","Yes")) %>% 
  ggplot()+
  geom_col(aes(x=name,y=freq,fill=secondary))+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
  facet_grid(.~paste(primary,"and..."),switch='x')+
  theme_minimal()+
  scale_fill_brewer(type='qual',palette = 7)+
  labs(x=NULL,y=NULL,fill="Doing two things at once?",subtitle="The Simultaneous Activities of Squirrels",title='Going Nuts',
       caption="Squirrels often do more than one thing at once.\nThis figure attempts to determine the frequency of secondary activities for each primary activity.\nThe most common activity for squirrels that are primarily eating is to also be foraging.\nHowever, squirels that are primarily foraging are typically only foraging. "
         )+
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 45,hjust=1),
    plot.caption = element_text(hjust = 0)
    )
# ggsave("~/Desktop/going_nuts.pdf",width=6,height=4)

#Squirrel colors
squirrels %>% 
  group_by(date,primary_fur_color) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  ggplot()+
  geom_area(aes(x=date,y=n,color=primary_fur_color))

squirrels %>% 
  group_by(date,primary_fur_color) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  ggplot()+
  geom_area(aes(x=date,y=n,fill=primary_fur_color))

squirrels %>% 
  ggplot()+
  geom_bin2d(aes(x=primary_fur_color,y=highlight_fur_color))

squirrels %>%  
  group_by(date,primary_fur_color,highlight_fur_color) %>% 
  summarise(n=n()) %>% 
  group_by(date) %>% 
  mutate(total=sum(n)) %>% 
  filter(paste(primary_fur_color,highlight_fur_color)%in%c("Gray NA","Gray Cinnamon","Gray White")) %>% 
  mutate(type=ifelse(is.na(highlight_fur_color),"Gray Only",paste(primary_fur_color,"and",highlight_fur_color))) %>% 
  ggplot()+
  geom_line(aes(x=date,y=n/total,group=type),color='darkgray',size=1.1)+
  geom_line(aes(x=date,y=n/total,linetype=!is.na(highlight_fur_color),group=type,color=type),size=1.1)+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),limits = c(0,NA),breaks = seq(0,.4,.2))+
  scale_x_date(breaks=c(min(squirrels$date),as.Date('2018-10-13'),max(squirrels$date)),date_labels="%B %e")+
  scale_color_manual(values=c("darkred","white","gray"),name="Squirrel Color")+
  scale_linetype_manual(values=c("solid", "dashed"),guide='none')+
  labs(title="Same Squirrel, Different Day...",subtitle = "Frequency by Day of the Three Most Commonly Observed Squirrel Colors",y="Percent of all Squirrels Observed",x=NULL,
       caption="What are the most common color combinations of squirrels and\nhow did the frequency of those colors change over time for surveyors?\nWhile Gray only squirrels were observed most frequently,\nat times gray and cinnamon squirrels were actually more common")+
  theme_minimal()+
  theme(
    legend.key = element_rect(fill=c('black')),
    legend.key.width = unit(.8, 'in'),
    legend.key.height = unit(.02, 'in'),
    panel.background = element_rect(fill='black'),
    panel.grid.major = element_line(color="white"),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    plot.caption = element_text(hjust = 0)
    )+
  guides(
    color=guide_legend(override.aes = list(linetype = c(2,2,1)),nrow = 3),
    linetype='none')
# ggsave("~/Desktop/same_squirrel.pdf",width=6,height=4)
