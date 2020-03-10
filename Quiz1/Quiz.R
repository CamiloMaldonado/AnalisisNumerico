fpoly3<-function(x){
  return(-0.0002009*x^3+0.0275644*x^2-0.2324786*x+27.0583125)
}

x_poly3<-spine$sacr_slope
y_poly3<-fpoly3(x)
means_poly3<-data.frame(x_poly3,y_poly3)
spine_poly3<-spine[,c(4,3)]
means_poly3$group<-1:310
spine_poly3$group<-1:310
colnames(means_poly3)<-c("sacr_slope","l_l_angle","group")
groups_poly3<-rbind(means_poly3,spine_poly3)

poly3<-ggplot()+geom_point(data=spine,aes(x=sacr_slope,y=l_l_angle))+
  stat_function(aes(x=spine$sacr_slope),fun=fpoly3)+
  geom_point(data=means_poly3,aes(x=x_poly3,y=y_poly3),color="red")+
  geom_line(data=groups_poly3,aes(x=sacr_slope,y=l_l_angle,group=group),color="grey",alpha=0.4)+ggtitle("Polynomial degree 3")
poly3

#Punto 2
plot(datos)
puntos[1]=2
puntos[15]=1
puntos[48]
suavizado.datos <- smooth.spline(datos$y,w=puntos,spar=0.75)
lines(suavizado.datos$y,col="red")

plot(rep(0,nrow(datos)),col="red",type="l")
lines(residuals(suavizado.datos),col="blue")
