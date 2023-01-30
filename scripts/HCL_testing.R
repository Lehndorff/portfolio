library(tidyverse)
library(colorspace)

#Based on testing, polarLUV function takes hue as a value between -134 and 107
# -134:107

output<-NULL
for(h in seq(-134,107,16)){
  print(h)
  z=expand.grid(
  l=seq(1,150,.1),#Luminance of 0 == black
  c=seq(0,200,.1) #Negative chroma exist and are cool, but they are complicated
  ) %>% 
    mutate(
      h=h,
      color=hex(polarLUV(H=h,C=c,L=l))
      ) %>% 
    filter(!is.na(color)) %>% #There are many impossible combinations of HCL
    slice_sample(n=10000,replace = T) %>% 
    distinct()
  
  output<-bind_rows(output,z)
  
}

# output<-output %>% 
#   group_by(h) %>% 
#   mutate(c=c/max(c))

ggplot(output)+
  geom_point(aes(x=l,y=c),color=output$color)+
  theme_minimal()+
  labs(x="Luminance",y="Chroma",title="Impact of Luminance and Chroma variation for a sample of Hues")+
  facet_wrap(h~.)

single_hue=expand.grid(
  l=seq(1,150,.5),#Luminance of 0 == black
  c=seq(0,200,.5) #Negative chroma exist and are cool, but they are complicated
  ) %>% 
    mutate(
      h=-102,
      color=hex(polarLUV(H=h,C=c,L=l))
      ) %>% 
    filter(!is.na(color))

ggplot(single_hue)+
  geom_point(aes(x=l,y=c,group=color),color=single_hue$color)+
  theme_minimal()+
  labs(x="Luminance",y="Chroma",title="Impact of Luminance and Chroma variation for a sample of Hues")+
  facet_wrap(h~.)
plotly::ggplotly()
