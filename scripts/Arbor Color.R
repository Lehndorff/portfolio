library(ggplot2)
library(dplyr)
library(gganimate)
library(ggforce)
library(useful)

test_dieoff<-function(lifespan,tests=10000){
  start<-data.frame(t=1:tests,age=NA)
  
  for(t in start$t){
    age<-1
    while(!die_off(age,lifespan)){
      age<-age+1
      
    }
    start$age[start$t==t]<-age
    
  }
  
  plot<-ggplot(start)+
    geom_bar(aes(x=age))+
    geom_vline(xintercept=median(start$age),color="red")+
    scale_x_continuous(breaks = min(start$age):max(start$age))
  return(plot)
}

# germinate<-function(new_tree){
#   father_trees<-new_tree %>% 
#     filter(age>5) %>% 
#     ungroup() %>% 
#     mutate(saps=round(runif(n(),0,5)))
#   
#   if(nrow(father_trees)==0){
#     return(NULL)
#   }
#   
#   saplings<-data.frame(ft=rep(father_trees$tree,father_trees$saps)) %>% 
#     left_join(father_trees,by=c("ft"="tree")) %>% 
#     ungroup() %>% 
#     mutate(range=runif(n(),t,r*10),dir=runif(n(),0,360)) %>% 
#     select(ft,x,y,t,r,year,age,old_t,die,range,dir)
#   saplings$x<-pol2cart(saplings$range,saplings$dir)$x+saplings$x
#   saplings$y<-pol2cart(saplings$range,saplings$dir)$y+saplings$y
#   saplings$t<-1
#   saplings$r<-1^1.5
#   saplings$age=0
#   saplings<-saplings %>% 
#     select(-range,-dir)
#   saplings$tree<-(max(new_tree$tree)+1):(max(new_tree$tree)+nrow(saplings))
#   
#   return(saplings)
#   
# }

die_off<-function(age,lifespan){
  if(age>1){
    pool<-c(rep(T,age),rep(F,lifespan))
    mean(pool)
    outcome<-sample(pool,1)

  }
  if(age<=1){
    outcome<-F
  }
  return(outcome)
}
die_off(2)

pop_ot<-function(n_lifespan=lifespan,n_cross_prob=cross_prob,n_trees=trees,n_years=years){
  
  start<-data.frame(t=paste("1 -",(1:n_trees)),age=1,year=1)
  temp_out<-start
  all_output<-start %>% group_by(year) %>% summarise(n=n_distinct(t))
  for(y in 2:n_years){
    # print(y)
    prev<-temp_out %>% 
      filter(year==y-1)
    
    if(nrow(prev)==0){next}
    
    old_t<-prev %>% 
      group_by(t) %>% 
      mutate(die=die_off(age,n_lifespan)) %>% 
      filter(die==F) %>% 
      mutate(year=y) %>% 
      mutate(age=age+1)
    
    new_t<-old_t %>% 
      ungroup() %>% 
      mutate(
        cross=sample(c(rep(T,round(n_cross_prob,2)*100),rep(F,(1-round(n_cross_prob,2))*100)),nrow(old_t),replace = T)
      ) %>% 
      filter(cross) %>% 
      select(-cross)
    
    if(nrow(new_t)>0){
      new_t<-new_t %>% 
        mutate(
          t=paste(y,"-",1:n()),
          age=1,
          year=y
      )
    }

    temp_out<-bind_rows(
      old_t,new_t
    )
    
    all_output<-bind_rows(all_output,temp_out %>% group_by(year) %>% summarise(n=n_distinct(t)))
    
    
  }
  
  plot<-all_output %>% 
    ggplot()+
    geom_line(aes(x=year,y=n))+
    scale_x_continuous(limits = c(0,n_years))+
    geom_hline(yintercept = 0,color=NA)
  return(plot)
  
}


years<-1000
trees<-100
range<-500
trunk_start_max<-5
trunk_limit<-100000
cross_prob<-.12
lifespan=50
traits=T

# pop_ot()

test_dieoff(lifespan)

trees_start<-data.frame(tree=1:trees,x=runif(n = trees,min = 0,max=range),y=runif(n = trees,min = 0,max=range),t=runif(n = trees,min = 1,max=trunk_start_max)) %>%
  mutate(r=t^1.5,year=0,age=runif(n = trees,min = 1,max=1),old_t=t) %>% 
  group_by(row_number()) %>% 
  mutate(
    red=round(runif(1,0,255))/255,
    green=round(runif(1,0,255))/255,
    blue=round(runif(1,0,255))/255,
    color=rgb(red,green,blue)
  )

ggplot(trees_start)+
  # geom_point(aes(x=x,y=y,color=factor(tree),size=t))+
  geom_circle(aes(x0=x,y0=y,r=t,fill=color))+
  scale_fill_manual(values = trees_start$color)+
  guides(fill='none')+
  theme_void()+
  coord_fixed()

tree_dat<-trees_start
for(i in 1:years){
  print(i)
  current_tree<-tree_dat %>% 
    filter(year==i-1)
  
  #Die off
  if(nrow(current_tree)>0){
    current_tree<-current_tree %>% 
      group_by(tree) %>% 
      mutate(die=die_off(age,lifespan)) %>%
      # mutate(die=F) %>% 
      ungroup()
  }
  new_trees<-current_tree %>% 
    filter(!die) %>% 
    mutate(old_t=t) %>% 
    mutate(t=t+1,year=i,age=age+1) %>% 
    mutate(t=ifelse(t>trunk_limit,trunk_limit,t))
  for(this_tree in unique(new_trees$tree[new_trees$age>1])){
    
    if(!sample(c(rep(T,round(cross_prob,2)*100),rep(F,(1-round(cross_prob,2))*100)),1)){next}
    
    # print(this_tree)
    this_one<-subset(new_trees,tree==this_tree)
    others_ones<-subset(new_trees, tree!=this_tree)
    others_ones$dist<-sqrt((others_ones$x-this_one$x)^2+(others_ones$y-this_one$y)^2)
    closest_one<-others_ones %>% 
      filter(dist==min(dist)) %>% 
      filter(1:n()==1)
    
    new_tree<-bind_rows(this_one,closest_one) %>% 
      select(where(is.numeric)) %>% 
      summarise(across(everything(),mean)) %>% 
      mutate(
        t=1,
        x=x+runif(1,range/-10,range/10),
        y=y+runif(1,range/-10,range/10),
        age=1,
        year=i,
        tree=max(new_trees$tree)+1,
        color=rgb(red,green,blue)
      )
    
    if(traits){
      
      #### Color Traits ####
      #blue
      blue_eff<-sample(c(T,F),size=1,prob = c(new_tree$blue,1-new_tree$blue))
      # new_tree$age<-new_tree$age-blue_eff*round(runif(1,0,new_tree$blue*20))
      new_tree$blue<-new_tree$blue+blue_eff*runif(1,0,new_tree$blue/5)
      new_tree$red<-new_tree$red-blue_eff*runif(1,0,new_tree$red/5)
      new_tree$green<-new_tree$green-blue_eff*runif(1,0,new_tree$green/5)
      new_tree$blue[new_tree$blue>1]<-1
      new_tree$red[new_tree$red<0]<-0
      new_tree$green[new_tree$green<0]<-0

      #green
      green_eff<-sample(c(T,F),size=1,prob = c(new_tree$green,1-new_tree$green))
      # new_tree$age<-new_tree$age+green_eff*round(runif(1,0,new_tree$green*10))
      new_tree$green<-new_tree$green+green_eff*runif(1,0,new_tree$green/5)
      new_tree$red<-new_tree$red-green_eff*runif(1,0,new_tree$red/5)
      new_tree$blue<-new_tree$blue-green_eff*runif(1,0,new_tree$blue/5)
      new_tree$green[new_tree$green>1]<-1
      new_tree$red[new_tree$red<0]<-0
      new_tree$blue[new_tree$blue<0]<-0

      #red
      red_eff<-sample(c(T,F),size=1,prob = c(new_tree$red,1-new_tree$red))
      # new_tree$y<-new_tree$y+red_eff*round(runif(1,0,new_tree$red*range*.2))
      new_tree$red<-new_tree$red+red_eff*runif(1,0,new_tree$red/5)
      new_tree$blue<-new_tree$blue-red_eff*runif(1,0,new_tree$blue/5)
      new_tree$green<-new_tree$green-red_eff*runif(1,0,new_tree$green/5)
      new_tree$red[new_tree$red>1]<-1
      new_tree$blue[new_tree$blue<0]<-0
      new_tree$green[new_tree$green<0]<-0
      
      #### Traits 2 ####
      
      # #blue
      # blue_eff<-sample(c(T,F),size=1,prob = c(new_tree$blue,1-new_tree$blue))
      # # new_tree$age<-new_tree$age-blue_eff*round(runif(1,0,new_tree$blue*20))
      # new_tree$blue<-new_tree$blue+blue_eff*runif(1,0,new_tree$blue/10)
      # new_tree$red<-new_tree$red-blue_eff*runif(1,0,new_tree$red/10)
      # new_tree$green<-new_tree$green-blue_eff*runif(1,0,new_tree$green/10)
      # new_tree$blue[new_tree$blue>1]<-1
      # new_tree$red[new_tree$red<0]<-0
      # new_tree$green[new_tree$green<0]<0
      # 
      # #green
      # green_eff<-sample(c(T,F),size=1,prob = c(new_tree$green,1-new_tree$green))
      # # new_tree$age<-new_tree$age+green_eff*round(runif(1,0,new_tree$green*10))
      # new_tree$green<-new_tree$green+green_eff*runif(1,0,new_tree$green/10)
      # new_tree$red<-new_tree$red-green_eff*runif(1,0,new_tree$red/10)
      # new_tree$blue<-new_tree$blue-green_eff*runif(1,0,new_tree$blue/10)
      # new_tree$green[new_tree$green>1]<-1
      # new_tree$red[new_tree$red<0]<-0
      # new_tree$blue[new_tree$blue<0]<0
      # 
      # #red
      # red_eff<-sample(c(T,F),size=1,prob = c(new_tree$green,1-new_tree$green))
      # new_tree$y<-new_tree$y+red_eff*round(runif(1,0,new_tree$red*range*.2))
      # new_tree$red<-new_tree$red+red_eff*runif(1,0,new_tree$red/10)
      # new_tree$red[new_tree$red>1]<-1


      
      new_tree$color<-rgb(new_tree$red,new_tree$green,new_tree$blue)

    }
    
    new_trees<-bind_rows(new_trees,new_tree)
  
}

  tree_dat<-bind_rows(tree_dat,new_trees)
  print(paste(nrow(new_trees),"trees"))
  
  # print(tree_dat$r[tree_dat$tree==9])
}


# ggplot(tree_dat)+
#   # geom_point(aes(x=x,y=y,color=factor(tree),size=t))+
#   geom_circle(aes(x0=x,y0=y,r=t,fill=color),alpha=1)+
#   scale_fill_manual(values = tree_dat$color)+
#   guides(fill=F)+
#   theme_void()+
#   coord_fixed()

x_range<-range(tree_dat$x)
y_range<-range(tree_dat$y)

# save.image("data/arbor_color.rdata")
save(list = ls(),file="data/arbor_color.rdata")

# for(i in unique(tree_dat$year)){
#   print(i)
#   year_dat<-tree_dat %>% 
#     filter(year==i)
#   
#   this_plot<-ggplot(year_dat)+
#     geom_circle(aes(x0=x,y0=y,r=t,fill=factor(tree)),alpha=1)+
#     scale_fill_manual(values = year_dat$color)+
#     coord_fixed(xlim = x_range,ylim = y_range)+
#     guides(fill=F)+
#     theme_void()
#   ggsave(this_plot,file=paste0("~/desktop/Arbor Color/",i,".jpg"),width=8,height=8)
#   
# }
# 
tree_dat %>%
  group_by(year) %>%
  summarise(trees=n_distinct(tree)) %>%
  ggplot()+
  geom_line(aes(x=year,y=trees))

# start<-proc.time()
# input=80
# tree_plot<-tree_dat %>% 
#   filter(year<=input) %>% 
#   ggplot()+
# geom_circle(aes(x0=x,y0=y,r=t,fill=factor(tree)),alpha=1)+
# scale_fill_manual(values = tree_dat$color)+
# coord_fixed(xlim = x_range,ylim = y_range)+
# guides(fill='none')+
# theme_void()+
# transition_states(year)+
# ease_aes("linear")
# 
# animate(tree_plot,nframes = input*2,duration = input/10,end_pause = 1,renderer = gifski_renderer())
# proc.time()-start
# 
# z<-data.frame(x=c(10,20,40,80),y=c(31,87,283,1183)) %>% 
#   mutate(x2=x^2)
# 
# mod=summary(lm("y~x2",data=z))$coefficients %>% data.frame()
# 
# ggplot(z)+
#   geom_point(aes(x=x,y=y))+
#   geom_function(fun = function(x) mod$Estimate[1]+x^2*mod$Estimate[2])
# 
# (mod$Estimate[1]+1000^2*mod$Estimate[2])/60/60


