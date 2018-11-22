#--LUCIANO ANDRIAN TP FINAL LABORATORIO PARA EL ....ETC
####################################################################\m/####################################################################
rm(list=ls())
library(maps)
library(readxl)
library(maptools)
library(ggplot2)
library(ggmap)
library(mapproj)
library(grid)
library(gridExtra)
library(akima)
####################################################################\m/####################################################################

estaciones<-as.data.frame(read_excel("datos_estaciones.xls"))
estaciones2<-data.frame(estaciones[[1]],estaciones[[2]],estaciones[[6]])
colnames(estaciones2)<-c("lon","lat","Estaciones")
puntos <- data.frame(lon=estaciones2[,1], lat=estaciones2[,2])
mapa <- get_map(location = c(left = -75, bottom = -55, right=-50, top = -20 ), source = 'stamen', maptype = "toner-lite",zoom=6)
arg1 <- ggmap(mapa, extent = 'normal')
arg<-arg1 + geom_point(data=puntos, col="red",size=1)
plot(arg)

ggsave("ESTACIONES.jpg",plot = arg ,width = 30,height = 15 ,units = "cm")

####################################################################\m/####################################################################

pp_arg<-list()
for(i in 1:12){
  pp_arg[[i]]<-as.data.frame(read_excel("pparg_1979_2012.xls",sheet=i))
} 

aux<- as.array(unlist(pp_arg))
pp<-array(aux,dim = c(34,69,12))
pp1<-array(array(c(pp[,,12],pp[,,1:12]),dim=c(34,69,12)),dim=c(34,69,3,4))

pp_estacional<-array(NA,dim=c(33,69,4))

pp_estacional[,,1]<-pp1[1:33,,1,1]+pp1[2:34,,2,1]+pp1[2:34,,3,1]
pp_estacional[,,2:4]<-apply(pp1[2:34,,,2:4],c(1,2,4),sum)

colnames(pp_estacional)<-c(colnames(pp_arg[[12]]))

####################################################################\m/####################################################################
#ESTACIONES SELECCIONADAS  
#Cordoba: 87344
#M.caseros: 87393
#junin: 87548
#Salta: 87047
#Bariloche: 87765
#Rio Grande: 87934

estaciones_seleccionadas=c("87344","87393","87548","87047","87765","87934")
series<-list(Cordoba=matrix(0,nrow=33,ncol=4),M.Caseros=matrix(0,nrow=33,ncol=4),
             Junin=matrix(0,nrow=33,ncol=4),Salta=matrix(0,nrow=33,ncol=4),Bariloche=matrix(0,nrow=33,ncol=4),
             R.Grande=matrix(0,nrow=33,ncol=4))

i=1
for(i in 1:6){
  for(j in 1:4){
    series[[i]][,j]<-pp_estacional[,,j][,estaciones_seleccionadas[i]]
  }
  series[[i]]<-as.data.frame(cbind(series[[i]],c(1980:2012)))
  colnames(series[[i]])<-c("DEF","MAM","JJA","SON","ANIOS")
}

Q <- list()
colores=c("cyan3","coral3","darkolivegreen2","orange2","tomato2","yellow2")
for(i in 1:6){
  
  p1 <- ggplot(series[[i]], aes(x=ANIOS)) + geom_line(aes(x=ANIOS, y=DEF),col="firebrick") +
    xlab("Anios") + ylab("Precipitaci?n") + labs(title="DEF") + ylim(c(0,900))+ theme_bw()+theme(plot.background = element_rect(fill = colores[i]))
  p2 <- ggplot(series[[i]], aes(x=ANIOS)) + geom_line(aes(x=ANIOS, y=MAM),col="orange") +
    xlab("Anios") + ylab("Precipitaci?n") + labs(title="MAM") + ylim(c(0,900))+ theme_bw() +theme(plot.background = element_rect(fill = colores[i]))
  p3 <- ggplot(series[[i]], aes(x=ANIOS)) + geom_line(aes(x=ANIOS, y=JJA),col="deepskyblue") +
    xlab("Anios") + ylab("Precipitaci?n") + labs(title="JJA") + ylim(c(0,900))+ theme_bw()+theme(plot.background = element_rect(fill = colores[i]))
  p4 <- ggplot(series[[i]], aes(x=ANIOS)) + geom_line(aes(x=ANIOS, y=SON),col="forestgreen") +
    xlab("Anios") + ylab("Precipitaci?n") + labs(title="SON") + ylim(c(0,900))+ theme_bw()+theme(plot.background = element_rect(fill = colores[i]))
  Q[[i]] <- grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2, top=textGrob(names(series)[i], gp=gpar(fontsize=15,font=1)))
  
}

ggsave("serie_1.png",plot = grid.arrange(Q[[1]],Q[[2]],ncol=2),width = 30,height = 15 ,units = "cm")
ggsave("serie_2.png",plot = grid.arrange(Q[[3]],Q[[4]],ncol=2),width = 30,height = 15 ,units = "cm")
ggsave("serie_3.png",plot = grid.arrange(Q[[5]],Q[[6]],ncol=2),width = 30,height = 15 ,units = "cm")

####################################################################\m/####################################################################

#LOS DATOS ESTAN COMPLETADOS GENERANDO TENDENCIAS NEGATIVAS EN MUCHOS LUGARES

pp_acum_anual<-apply(pp[,2:69,1:12],c(1,2),sum)

fit<-(lm(pp_acum_anual ~ c(1979:2012))) 
cf<-round(fit[[1]],2)
pp_tendencia<-cbind(puntos,cf[2,1:68])

v<-data.frame(X = pp_tendencia[,1],Y = pp_tendencia[,2],Tendencias = pp_tendencia[,3])

#PARA TENER UNA ESCALA DE COLORES DISCRETAS CON MAS NIVELES
v$Tendencias[which(v[3]<0-10)]<--10
v$Tendencias[which(-10<v[3]&v[3]<0-7)]<--8
v$Tendencias[which(-7<v[3]&v[3]<0-3)]<--5
v$Tendencias[which(-3<v[3]&v[3]<0-1)]<--2
v$Tendencias[which(-1<v[3]&v[3]<1)]<-0
v$Tendencias[which(1<v[3]&v[3]<3)]<-2
v$Tendencias[which(3<v[3]&v[3]<7)]<-5
v$Tendencias[which(7<v[3]&v[3]<10)]<-8
v$Tendencias[which(10<v[3])]<-10

pp_tend<-ggmap(mapa, extent = 'normal') +
  geom_point(data=v,aes(x=X,y=Y,size=Tendencias,colour=Tendencias))+
  scale_color_gradient2(low = "red3",mid ="gold2" ,high = "forestgreen")+
  guides(color=guide_legend(), size = guide_legend())

ggsave("Tendencia_PP.jpg",plot = pp_tend ,width = 30,height = 15 ,units = "cm")

####################################################################\m/####################################################################

indices<-list()
for(i in 1:6){
  indices[[i]]<-as.data.frame(read_excel("tp_indices_mensuales.xls",sheet=i))
}

aux2<-as.array(unlist(indices))
indices2<-array(aux2,dim = c(34,13,6))
indices_promedio<-array(NA,dim=c(34,4,6)) #2DA DIMENCION CADA ESTACION DEL ANIO,3ERA ESTACIONES CADA INDICE
indices_promedio[2:34,1,1:6]<-(indices2[1:33,13,1:6]+indices2[2:34,2,1:6]+indices2[2:34,3,1:6])/3
indices_promedio[2:34,2,1:6]<-apply(indices2[2:34,4:6,1:6],c(1,3),mean)
indices_promedio[2:34,3,1:6]<-apply(indices2[2:34,7:9,1:6],c(1,3),mean)
indices_promedio[2:34,4,1:6]<-apply(indices2[2:34,10:12,1:6],c(1,3),mean)
colnames(indices_promedio)<-c("DEF","MAM","JJA","SON")

#TEST DE CORRELACION

corr<-array(NA,dim = c(68,4,6,2))
confianza=c(0.90,0.95,0.99)
confianza2=c()
est<-c(1.694,2.037,2.738)

for(i in 1:6){
  for(j in 1:4){
    for(p in 2:69){
      for(c in 1:3){
        l<-cor.test(indices_promedio[2:34,j,i],pp_estacional[,p,j],method = "pearson",conf.level = confianza[c],alternative = "two.sided")
        corr[p-1,j,i,2]<-l$estimate
        if(abs(l$statistic)>est[c]){ 
          corr[p-1,j,i,1]<-confianza[c]##DEPENDIENDO DE SI RECHAZO O NO Ho
        }
      } 
    }
  }
}

#INTERPOLACION
#REORDENO LOS DATOS PARA USAR UN SOLO FOR
aux3<-as.array(unlist(corr[,,,2]))
corr2<-array(aux3,dim = c(68,24))

aux4<-as.array(unlist(corr[,,,1]))
corr3<-array(aux4,dim = c(68,24))

data(wrld_simpl)
mymap <- fortify(wrld_simpl) #PARA QUE FUNCIONE EN GGPLOT
DMJS<-rep(c("DEF","MAM","JJA","SON"),6)
V<-list()
#GRAFICADO DE TODOS LOS MAPAS
for(i in 1:24)
  local({
  d<-data.frame(corr2[,i],puntos[,1],puntos[,2])
  colnames(d)<-c("corr","lon","lat")
  
  akima_li <- with(d,interp(x = d$lon, y = d$lat, z = d$corr,
                            yo = seq(min(d$lat)-1, max(d$lat)+1, length = 20*15.71),
                            xo = seq(min(d$lon)-1, max(d$lon)+1, length = 20*8.87),
                            linear = F,extrap = T))
 
  dInterp <- data.frame(expand.grid(x = akima_li$x, y = akima_li$y), z = c(akima_li$z))
  dInterp$z[which(abs(dInterp$z)>1)]<-1
  gg<-data.frame(x=puntos[,1],y=puntos[,2],Confianzax100=corr3[,i])
  gg2<-gg[which(!is.na(gg[,3])),]

  V[[i]]<<-ggplot() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), fill = "white", color = "black") +
    geom_tile(data = dInterp, aes(x = x, y = y, fill = z), alpha = 0.7) + 
    scale_fill_gradientn(name=expression("R"),limits=c(-1,1),colors = c("blue", "lightblue", "white", "orange", "red")) +
    scale_x_continuous(limits = c(min(d$lon)-1, max(d$lon)+1), expand = c(0, 0)) +
    scale_y_continuous(limits = c( min(d$lat)-1,max(d$lat)+1), expand = c(0, 0)) +
    geom_point(data=puntos,aes(x=puntos[,1],y=puntos[,2],z=0), col="black",size=1)+
    xlab("Longitud") + 
    ylab("Latitud") +
    geom_point(data=gg2,aes(x=x,y=y,color=factor(gg2[,3])),size=3*gg2[,3],show.legend = T)+
    scale_color_hue(name=expression("Confianza")) +
    ggtitle(DMJS[i]) + theme(plot.title = element_text(hjust=0.5)) +
    coord_equal()
}) 

source("funciones_tp_final.R")

indices_nombres<-c("AAO","DOI","NINO3.4","SOI","SAODI","OLR")
ggsave("Corr_AAO_PP.JPG",plot=grid.arrange_tp.final(V[[1]],V[[2]],V[[3]],V[[4]],ncol=2,nrow = 2,position = "right",titulo = "CORRELACION AAO VS PP ACUMULADA"),width = 20,height = 15 ,units = "cm")
ggsave("Corr_DOI_pp.JPG",plot=grid.arrange_tp.final(V[[5]],V[[6]],V[[7]],V[[8]],ncol=2,nrow = 2,position = "right",titulo = "CORRELACION DOI VS PP ACUMULADA"),width = 20,height = 15 ,units = "cm")
ggsave("Corr_NINO3.4_PP.JPG",plot=grid.arrange_tp.final(V[[9]],V[[10]],V[[11]],V[[12]],ncol=2,nrow = 2,position = "right",titulo = "CORRELACION NINO3.4 VS PP ACUMULADA"),width = 20,height = 15 ,units = "cm")
ggsave("Corr_SOI_PP.JPG",plot=grid.arrange_tp.final(V[[13]],V[[14]],V[[15]],V[[16]],ncol=2,nrow = 2,position = "right",titulo = "CORRELACION SOI VS PP ACUMULADA "),width = 20,height = 15 ,units = "cm")
ggsave("Corr_SAODI_pp.JPG",plot=grid.arrange_tp.final(V[[17]],V[[18]],V[[19]],V[[20]],ncol=2,nrow = 2,position = "right",titulo = "CORRELACION SAODI VS PP ACUMULADA"),width = 20,height = 15 ,units = "cm")
ggsave("Corr_OLR_PP.JPG",plot=grid.arrange_tp.final(V[[21]],V[[22]],V[[23]],V[[24]],ncol=2,nrow = 2,position = "right",titulo = "CORRELACION OLR VS PP ACUMULADA"),width = 20,height = 15 ,units = "cm")


#CORRELACION INDICES-AGOSTO VS PP-SON

corr_son<-array(NA,c(68,6,2))
for(i in 1:6){
  for(p in 2:69){
    for(c in 1:3){
      l<-cor.test(indices_promedio[2:34,4,i],pp_estacional[,p,4],method = "pearson",conf.level = confianza[c],alternative = "two.sided")
      corr_son[p-1,i,2]<-l$estimate
      if(abs(l$statistic)>est[c]){ 
        corr_son[p-1,i,1]<-confianza[c]##DEPENDIENDO DE SI RECHAZO O NO Ho
      } 
    }
  }
}


title<-c("AAO vs PP","DOI vs PP","NINO3.4 vs PP","SOI vs PP","SAODI vs PP","OLR vs PP")
VSON<-list()

for(i in 1:6)
  local({
    i<-i
    d<-data.frame(corr_son[,i,2],puntos[,1],puntos[,2])
    colnames(d)<-c("corr","lon","lat")
    
    akima_li <- with(d,interp(x = d$lon, y = d$lat, z = d$corr,
                              yo = seq(min(d$lat)-1, max(d$lat)+1, length = 20*15.71),
                              xo = seq(min(d$lon)-1, max(d$lon)+1, length = 20*8.87),
                              linear = F,extrap = T))
    
    dInterp <- data.frame(expand.grid(x = akima_li$x, y = akima_li$y), z = c(akima_li$z))
    dInterp$z[which(abs(dInterp$z)>1)]<-1
    gg<-data.frame(x=puntos[,1],y=puntos[,2],Confianzax100=corr_son[,i,1])
    gg3<-gg[which(!is.na(gg[,3])),]
    
    VSON[[i]]<<-ggplot() +
      geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id), fill = "white", color = "black") +
      geom_tile(data = dInterp, aes(x = x, y = y, fill = z), alpha = 0.7) + 
      scale_fill_gradientn(name=expression("R"),limits=c(-1,1),colors = c("blue", "lightblue", "white", "orange", "red")) +
      scale_x_continuous(limits = c(min(d$lon)-1, max(d$lon)+1), expand = c(0, 0)) +
      scale_y_continuous(limits = c( min(d$lat)-1,max(d$lat)+1), expand = c(0, 0)) +
      geom_point(data=puntos,aes(x=puntos[,1],y=puntos[,2],z=0), col="black",size=1)+
      xlab("Longitud") + 
      ylab("Latitud") +
      geom_point(data=gg3,aes(x=x,y=y,color=factor(gg3[,3])),size=3*gg3[,3],show.legend = T)+
      scale_color_hue(name=expression("Confianza")) +
      ggtitle(title[i]) + theme(plot.title = element_text(hjust=0.5)) +
      coord_equal()
    
  })

ggsave("Corr_SON.JPG",plot=grid.arrange_tp.final(VSON[[1]],VSON[[2]],VSON[[3]],VSON[[4]],VSON[[5]],VSON[[6]],ncol=3,nrow=2,position = "right",titulo = "Correlacion Indices en Agosto vs PP en SON"),width = 30,height = 15 ,units = "cm")

####################################################################\m/####################################################################

estaciones_reg<-array(NA,c(33,2,3))
estaciones_reg[,,1]<-cbind(pp_estacional[,12,4],indices_promedio[2:34,4,2])  #posadas vs doi
estaciones_reg[,,2]<-cbind(pp_estacional[,26,4],indices_promedio[2:34,4,3])  #mcaseros vs nino34
estaciones_reg[,,3]<-cbind(pp_estacional[,33,4],indices_promedio[2:34,4,5])  #Rosario vs SAODI
colnames(estaciones_reg)<-c("x","y")

titulo<-c("Regresion PP-SON Posadas vs DOI-Agosto","Regresion PP-SON M.Caseros vs Nino3.4-Agosto","Regresion PP-SON Rosario vs SAODI-Agosto")

posy<-c(1,2,-2)
posx<-c(500,500,300)
Vlag<-list()

for(i in 1:3){
Vlag[[i]] <- ggplot(as.data.frame(estaciones_reg[,,i]), aes(x=x, y=y)) + geom_point(color = "black", size = 2) +
  ylab("indice")+ xlab("PP Acumulada ")+ theme_bw() + ggtitle(titulo[i]) + 
  stat_smooth(method = "lm", formula = y~x, se = TRUE, level= 0.95) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(x = posx[i], y = posy[i], label = regre(as.data.frame(estaciones_reg[,,i])), parse = TRUE,size=5)  
}
  
ggsave("linear_reg.JPG",plot=grid.arrange(Vlag[[1]],Vlag[[2]],Vlag[[3]],ncol=2),width = 30,height = 15 ,units = "cm")

####################################################################\m/####################################################################