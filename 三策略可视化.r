library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)
setwd("D:/social nerwork/1Replicator dynamics for involution in an infinite well-mixed population (2)")

duplicate_dynamic_xy<-function(y,x,M,beta1,beta2,d,N,c,l){
  
  #-----参数解释说明------#
  # #N个个体
  # N<-4
  # #M资源 c(5,15,25)
  # M<-5
  
  # y 整体内卷比例
  # x 整体躺平比例
  
  # #内卷相对躺平的效用
  # beta1<-4
  # #合作相对躺平的效用
  # beta2<-2
  
  # #more effort的成本
  # d<-4
  # #less effort的成本
  # c<-1
  # #躺平的成本
  # l<-0.5
  #-----------------------#
  
  Pc<-0
  Pd<-0
  Pl<-0 
  for (Nd in 0:(N-1)){ 
    for (Nl in 0:(N-1-Nd)){
      Nc <- N-1-Nd-Nl
      pai_d <- beta1*d*M/(beta1*(Nd+1)*d +beta2*Nc*c     +Nl*l)-d 
      pai_c <- beta2*c*M/(beta1*Nd*d     +beta2*(Nc+1)*c +Nl*l)-c          
      pai_l <-       l*M/(beta1*Nd*d     +beta2*Nc*c     +(Nl+1)*l)-l  
      Pd <- Pd + choose(N-1,Nd)*choose(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_d
      Pc <- Pc + choose(N-1,Nd)*choose(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_c
      Pl <- Pl + choose(N-1,Nd)*choose(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_l
    }
  }
  
  R_ <- y*Pd+x*Pl+(1-x-y)*Pc  ##均值
  y. <- ifelse(is.na(y*(Pd-R_)),0,y*(Pd-R_))
  x. <- ifelse(is.na(x*(Pl-R_)),0,x*(Pl-R_))
  
  result<-data.frame(y=y,x=x,y.=y.,x.=x.)
  return(result)
}

#x+y<=1
data<-data.frame()
for(y in seq(0,1,0.01)){
  for(x in seq(0,1-y,0.01)){
    temp<-duplicate_dynamic_xy(y,x,M=100,beta1=4,beta2=2,d=4,N=50,c=1,l=0.5)
    data<-rbind(data,temp)
  }
}

#----------------------------------------------#
#-----1.1当躺平人数一定时，内卷人数的稳定点-----#
#----------------------------------------------#
#当x=0.1时

df<-data %>% 
  subset(x==0.1)%>% 
  mutate(y1=c(y.[-1],1)) %>% 
  mutate(y0=y*y1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

ystar<-ifelse(sum(df$y[df$y0<0])==0,0,df$y[df$y0<0]+0.005)

ggplot(df,aes(x=y,y=y.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=ystar,y=0))+
  geom_text(aes(x=ystar,y=0),label=paste("y*:",ystar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "y", y = "y.")


#当x=0.2时
df<-data %>% 
  subset(x==0.2)%>% 
  mutate(y1=c(y.[-1],1)) %>% 
  mutate(y0=y*y1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

ystar<-ifelse(sum(df$y[df$y0<0])==0,0,df$y[df$y0<0]+0.005)

ggplot(df,aes(x=y,y=y.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=ystar,y=0))+
  geom_text(aes(x=ystar,y=0),label=paste("y*:",ystar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "y", y = "y.")


#----------------------------------------------#
#--------1.2内卷人数比例对躺平人数的影响--------#
#----------------------------------------------#
#当y=0.1时
df<-data %>% 
  subset(y==0.1)%>% 
  mutate(x1=c(x.[-1],0)) %>% 
  mutate(x0=x.*x1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

xstar<-ifelse(sum(df$x[df$x0<0])==0,0,df$x[df$x0<0]+0.005)

ggplot(df,aes(x=x,y=x.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=xstar,y=0))+
  geom_text(aes(x=xstar,y=0),label=paste("x*:",xstar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "x", y = "x.")

#当y=0.7时
df<-data %>% 
  subset(y==0.2)%>% 
  mutate(x1=c(x.[-1],0)) %>% 
  mutate(x0=x.*x1)#通过前一项与后一项相乘是否小于0，来获取临界点ystar

xstar<-ifelse(sum(df$x[df$x0<0])==0,0,df$x[df$x0<0]+0.005)

ggplot(df,aes(x=x,y=x.))+
  geom_line(color="black",size=0.3)+
  theme_few() +
  geom_point(aes(x=xstar,y=0))+
  geom_text(aes(x=xstar,y=0),label=paste("x*:",xstar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
  geom_hline(aes(yintercept = 0),size=0.3)+
  labs(x = "x", y = "x.")

#内卷比例看不出来影响躺平

df = data.frame()
for (y in seq(0.01,0.99,0.01)){
    for (x in seq(0.01,0.99,0.01)){
        if (x>=1-y){
            df=rbind(df,data.frame(y=y,x=x,y.=NaN,x.=NaN))
        }
        else{
            df=rbind(df,duplicate_dynamic_xy(y,x,M=100,beta1=1,beta2=1,d=4,N=50,c=1,l=0.5))  
        }
    }
}
df

Y<-matrix(df[,3],nrow = 99, ncol = 99, byrow = TRUE)
Y

library("plot3D")
# install.packages("RColorBrewer")
library(RColorBrewer)

persp3D(x=seq(0.01, 0.99, length.out = nrow(Y)),
        y=seq(0.01, 0.99, length.out = nrow(Y)),
        z=Y,
        col = brewer.pal(n=10, name = 'PiYG'),
        phi = 0, theta = -45,
        shade = 0.4
        )

library('plotly')

pY=plot_ly(x=seq(0.01, 0.99, length.out = nrow(Y)),
        y=seq(0.01, 0.99, length.out = nrow(Y)),
        z=Y,)%>%add_surface()

pY

X<-matrix(df[,4],nrow = 99, ncol = 99, byrow = TRUE)
X

persp3D(x=seq(0.01, 0.99, length.out = nrow(X)),
        y=seq(0.01, 0.99, length.out = nrow(X)),
        z=X,
        col = brewer.pal(n=10, name = 'PiYG'),
        phi = 25, theta = 0,
        shade = 0.3
        )

pX=plot_ly(x=seq(0.01, 0.99, length.out = nrow(X)),
        y=seq(0.01, 0.99, length.out = nrow(X)),
        z=X,)%>%add_surface()

pX

#----------------------------------------------------------------------#
#---------------------------图1（下）----------------------------------#
#----------------------------------------------------------------------#
M =100
    steps = 5000
    endings = c()
    df = data.frame()
    gp = ggplot(x = seq(1,steps+1,1))
    beta1 = 1
    beta2 = 1
    d = 4
    N = 50
    c = 1
    l = 0.5
    # 找到要跑多少步
    for (starting in seq(0.05,0.95,0.05)){
        t = seq(1,steps,1)
        y = c(starting)
        x = c((1-starting)/3)
        
        for (i in t){
            temp = duplicate_dynamic_xy(y[length(y)],x[length(x)],M,beta1,beta2,d,N,c,l)
            y. = temp$y.
            x. = temp$x.
#             cat(temp,ending="\n")
            if (abs(y.) <0.0001 & abs(x.)<0.0001){
                endings = c(endings,i)
                break
            }
            y = c(y,y. + y[length(y)])
            x = c(x,x. + x[length(x)])
        }
    }
    ending = max(endings)

    # 按最大步数再跑一遍
    for (starting in seq(0.05,0.95,0.05)){
        t = seq(1,ending,1)
        y = c(starting)
        x = c((1-starting)/3)
        for (i in t){
            temp = duplicate_dynamic_xy(y[length(y)],x[length(x)],M,beta1,beta2,d,N,c,l)
            y. = temp$y.
            x. = temp$x.
            y = c(y,y.  + y[length(y)])
            x = c(x,x. + x[length(x)])
        }
#         cat(rep(as.character(starting),length(y)),end="\n")
#         cat(endings,end="\n")
#         cat(x,end="\n")
#         cat(y,end="\n")
        df = rbind(df,data.frame(label=rep(as.character(starting),length(y)),t=seq(1,ending+1,1),y=y,x=x))
    }
    ggplot(df,aes(x=t,y=y,color=label))+
        geom_line()+
        geom_point()+
        theme(legend.position="none")
    ggplot(df,aes(x=t,y=x,color=label))+
    geom_line()+
    geom_point()+
    theme(legend.position="none")

#----------------------------------------------------------------------#
#------------------------------图2-------------------------------------#
#----------------------------------------------------------------------#

#--------2.1资源量对内卷的影响--------#
#固定x=0.3
ystar_collect<-function(M,beta1,beta2,d){

  y.<-c()
  
  for(y in seq(0, 0.7, 0.01)){
    temp<-duplicate_dynamic_xy(y,x=0.3,M,beta1,beta2,d,N=50,c=1,l=0.5)
    
    # N: 个体数
    # M: 资源 c(5,15,25)
    # c: less effort的成本
    # beta: 投入效用
    # d: more effort的成本
    
    y. = c(y.,temp$y.)
  }
    
  df <- data.frame(x = seq(0, 0.7, 0.01), y = y.) %>% 
  mutate(y1=c(y.[-1],1)) %>% 
  mutate(y0=y*y1)  
    
  ystar<-ifelse(sum(df$x[df$y0<0])==0,0,df$x[df$y0<0]+0.005)
  
  return(ystar)
}

#均衡ystar值收集
parameters<-data.frame(beta1=c(4,4,4,2,2,2),beta2=c(2,2,2,2,2,2),d=c(2,4,8,2,4,8))

result<-data.frame()

for(i in 1:6){
  beta1<-parameters$beta1[i]
  beta2<-parameters$beta2[i]
  d<-parameters$d[i]
  
  ystars<-c()
  
  for (M in seq(0,400,4)){
    temp<-ystar_collect(M,beta1,beta2,d)
    ystars<-c(ystars,temp)
  }
  
  data<-data.frame(beta1=rep(beta1,length(ystars)),beta2=rep(beta2,length(ystars)),d=rep(d,length(ystars)),M=seq(0,400,4),ystar=ystars)
  
  result<-rbind(result,data)
}


#数据可视化
df<-result %>% 
  mutate(label=paste("beta=1",beta1,",","beta=2",beta2,",","d=",d,sep=""))

ggplot(df,aes(x=M,y=ystar,color=label))+
  geom_line()+
  geom_point()+
  theme_few()


#------------------------------------#
#--------2.2资源量对躺平的影响--------#
#------------------------------------#
#固定y=0.3
xstar_collect<-function(M,beta1,beta2,d){

  x.<-c()
  
  for(x in seq(0, 0.7, 0.01)){
    temp<-duplicate_dynamic_xy(y=0.3,x,M,beta1,beta2,d,N=50,c=1,l=0.5)
    
    # N: 个体数
    # M: 资源 c(5,15,25)
    # c: less effort的成本
    # beta: 投入效用
    # d: more effort的成本
    
    x. = c(x.,temp$x.)
  }
    
  df <- data.frame(x = seq(0, 0.7, 0.01), x. = x.) %>% 
  mutate(x1=c(x.[-1],1)) %>% 
  mutate(x0=x.*x1)  
    
  xstar<-ifelse(sum(df$x[df$x0<0])==0,0,df$x[df$x0<0]+0.005)
  
  return(xstar)
}

#均衡xstar值收集
parameters<-data.frame(beta1=c(4,4,4,2,2,2),beta2=c(2,2,2,2,2,2),d=c(2,4,8,2,4,8))

result<-data.frame()

for(i in 1:6){
  beta1<-parameters$beta1[i]
  beta2<-parameters$beta2[i]
  d<-parameters$d[i]
  
  xstars<-c()
  
  for (M in seq(0,400,4)){
    temp<-ystar_collect(M,beta1,beta2,d)
    xstars<-c(xstars,temp)
  }
  
  data<-data.frame(beta1=rep(beta1,length(xstars)),beta2=rep(beta2,length(xstars)),d=rep(d,length(xstars)),M=seq(0,400,4),xstar=xstars)
  
  result<-rbind(result,data)
}


#数据可视化
df<-result %>% 
  mutate(label=paste("beta=1",beta1,",","beta=2",beta2,",","d=",d,sep=""))

ggplot(df,aes(x=M,y=xstar,color=label))+
  geom_line()+
  geom_point()+
  theme_few()


#----------------------------------------------------------------------#
#-----------------------------图5(a)-----------------------------------#
#----------------------------------------------------------------------#
#------------------y.-------------------#
library(progress)

#均衡ystar值收集
result<-data.frame()
beta1<-4
beta2<-2
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 40, clear = FALSE, width= 60)
for(M in seq(1,400,10)){
  pb$tick()
  for(d in 1:20){
    ystars<-c()
    
      temp<-ystar_collect(M,beta1,beta2,d)
      ystars<-c(ystars,temp)
    
    data<-data.frame(M=M,d=d,beta=beta1,ystar=ystars)
    
    result<-rbind(result,data)
  }
}

ggplot(result,aes(x=d,y=M,fill=ystar))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(result$ystar),end=max(result$ystar)))+
  theme_few()+
  guides(fill=guide_colorbar(title= expression(y^.),title.hjust =  .2))

ggsave("fig5-beta=0.6.png",width=15, height=13,units="cm",dpi = 600)

#------------------x.-------------------#
#均衡xstar值收集
result<-data.frame()
beta1<-4
beta2<-2
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed:elapsedfull",total = 40, clear = FALSE, width= 60)
for(M in seq(1,400,10)){
  pb$tick()
  for(d in 1:20){
    xstars<-c()
    
    temp<-xstar_collect(M,beta1,beta2,d)
    xstars<-c(xstars,temp)
    
    data<-data.frame(M=M,d=d,beta=beta1,xstar=xstars)
    
    result<-rbind(result,data)
  }
}

ggplot(result,aes(x=d,y=M,fill=xstar))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(result$xstar),end=max(result$xstar)))+
  theme_few()+
  guides(fill=guide_colorbar(title= expression(x^*),title.hjust =  .2))

ggsave("fig5-beta=0.6.png",width=15, height=13,units="cm",dpi = 600)






