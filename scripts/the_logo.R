library(dplyr)
library(ggplot2)

start<-runif(3)
amount<-10000
margins=.4

grid<-data.frame(xin=runif(amount,-1,1),yin=runif(amount,-1,1),size=runif(amount,-1,1),group=round(runif(amount,1,100))) %>% 
  group_by(group) %>% 
  mutate(
    order=1:n(),
    r=rep(start[1]+runif(1,start[1]*-margins,start[1]*margins),n()),
    g=rep(start[2]+runif(1,start[2]*-margins,start[2]*margins),n()),
    b=rep(start[3]+runif(1,start[3]*-margins,start[3]*margins),n())
    ) %>% 
  ungroup() %>% 
  mutate(across(r:b,function(x){
    x[x<0]<-0
    x[x>1]<-1
    x
  })) %>% 
  mutate(color=rgb(r,g,b)) %>% 
  arrange(order)

#### V1 ####
# plot<-ggplot(grid)+
#   geom_rect(xmin=-1.2,xmax=1.2,ymax=1.2,ymin=-1.2,fill="white")+
#   geom_path(aes(x=xin,y=yin,group=group,color=color))+
#   theme_void()+
#   guides(color='none')+
#   scale_color_manual(values = unique(grid$color))+
#   geom_rect(xmin=-.25,xmax=.25,ymax=1,ymin=.25,fill="white")+
#   geom_rect(xmin=-.25,xmax=.25,ymax=-.25,ymin=-1,fill="white")

#### V2 ####
plot<-grid %>% 
  filter(!(between(yin,.25,1)&between(xin,-.25,.25))) %>% 
  filter(!(between(yin,-1,-.25)&between(xin,-.25,.25))) %>% 
  ggplot()+
  geom_point(shape="H",aes(x=xin,y=yin,color=color,size=size))+
  scale_size_continuous(range = c(4,12))+
  theme_void()+
  guides(color='none',size='none')+
  scale_color_manual(values = unique(grid$color))

ggsave(plot,file="data/logo.png",width=10,height=10)  
ggsave(plot,file=sprintf("data/Logos/%s.png",lubridate::now()),width=10,height=10)  

