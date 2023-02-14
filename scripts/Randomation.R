# RT
library(dplyr)
library(ggplot2)

amount<-10000

random_line<-function(amount){
  zzz<-cbind(xin=runif(amount,-10,10),yin=runif(amount,-10,10),rin=runif(amount,-1,1),gin=runif(amount,-1,1),bin=runif(amount,-1,1)) %>% 
    data.frame() %>% 
    mutate(x=cumsum(xin),y=cumsum(yin),r=64,g=64,b=64)

  for(i in 1:nrow(zzz)){
    if(i==1){next}
    zzz$r[i]<-zzz$r[i-1]+zzz$rin[i]
    zzz$r[i]<-ifelse(zzz$r[i]>255,255,zzz$r[i])
    zzz$r[i]<-ifelse(zzz$r[i]<0,0,zzz$r[i])
    zzz$g[i]<-zzz$g[i-1]+zzz$gin[i]
    zzz$g[i]<-ifelse(zzz$g[i]>255,255,zzz$g[i])
    zzz$g[i]<-ifelse(zzz$g[i]<0,0,zzz$g[i])
    zzz$b[i]<-zzz$b[i-1]+zzz$bin[i]
    zzz$b[i]<-ifelse(zzz$b[i]>255,255,zzz$b[i])
    zzz$b[i]<-ifelse(zzz$b[i]<0,0,zzz$b[i])
  }
  
  zzz$color<-rgb(zzz$r,zzz$g,zzz$b,maxColorValue = 255)
  return(zzz)
}

a<-random_line(amount) %>% mutate(group="a")
b<-random_line(amount) %>% mutate(group="b")
c<-random_line(amount) %>% mutate(group="c")
d<-random_line(amount) %>% mutate(group="d")
e<-random_line(amount) %>% mutate(group="e")

z<-bind_rows(a,b,c,d,e)

ggplot(z)+
  geom_path(aes(x=x,y=y,color=color,group=group),alpha=.4,size=3,linejoin = "round",lineend = "round")+
  scale_color_manual(breaks=z$color,values = z$color)+
  guides(color=FALSE)+
  theme_void()+
  # theme(plot.background = element_rect(fill=rgb(r=runif(1,0,1),g=runif(1,0,1),b=runif(1,0,1))))+
  labs()

ggplot(z)+
  geom_point(aes(x=x,y=y,color=color))+
  scale_color_manual(breaks=z$color,values = z$color)+
  guides(color=FALSE)

ggplot(z)+
  geom_point(aes(x=x,y=y,color=color,size=xin))+
  scale_color_manual(breaks=z$color,values = z$color)+
  guides(color=FALSE,size=FALSE)

amount<-1000
grid<-data.frame(xin=runif(amount,-1,1),yin=runif(amount,-1,1),size=runif(amount,-1,1),group=round(runif(amount,1,10))) %>% 
  group_by(group) %>% 
  mutate(order=1:n())

colors<-data.frame(r=runif(3,0,1),g=runif(3,0,1),b=runif(3,0,1)) %>% mutate(color=rgb(r,g,b))
  
zzz<-ggplot(grid)+
  geom_path(aes(x=xin,y=yin,color=group),alpha=.1)+
  theme_void()+
  scale_color_gradient2(high=colors$color[1],low=colors$color[2],mid = colors$color[3],midpoint = 5)+
  # scale_color_gradient2(high="red",low="blue",mid = "white",midpoint = 5)+
  # theme(plot.background = element_rect(fill = "black"))+
  guides(color=FALSE)
ggsave(zzz,file="~/desktop/hhh.jpg",device = "jpeg",height = 8,width = 8)

library(gganimate)
# library(av)
# library(gifski)

zzz<-ggplot(grid)+
  geom_point(aes(x=xin,y=yin,color=group),alpha=.1)+
  theme_void()+
  scale_color_gradient2(high=colors$color[1],low=colors$color[2],mid = colors$color[3],midpoint = 5)+
  # scale_color_gradient2(high="red",low="blue",mid = "white",midpoint = 5)+
  # theme(plot.background = element_rect(fill = "black"))+
  guides(color=FALSE)+
  transition_time(group)+
  ease_aes("linear")

plot<-ggplot(grid)+
  geom_path(aes(x=xin,y=yin,color=group,group=order),alpha=1,size=2)+
  theme_void()+
  # scale_color_gradient2(high=colors$color[1],low=colors$color[2],mid = colors$color[3],midpoint = 5)+
  scale_color_gradient2(high="red",low="blue",mid = "white",midpoint = 5)+
  # theme(plot.background = element_rect(fill = "black"))+
  guides(color=FALSE)+
  transition_reveal(order)+
  labs()

animate(plot,nframes = 1000)
anim_save(filename="test.gif",path="~/desktop/")
