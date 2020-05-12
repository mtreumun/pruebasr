library(readxl)


#cirrosis <- read_excel("H:/MPGI/INP3130-1 METODOLOGIAS CUALITATIVAS Y CUANTITATIVAS DE LA INVESTIGACION/cirrosis_v2.xlsx")

cirrosis <- read.csv("H:/MPGI/INP3130-1 METODOLOGIAS CUALITATIVAS Y CUANTITATIVAS DE LA INVESTIGACION/cirrosis_v2.csv")

View(cirrosis)

attach(cirrosis)

boxplot(EDAD_CANT~SEXO)
dev.print(png, "H:/MPGI/INP3130-1 METODOLOGIAS CUALITATIVAS Y CUANTITATIVAS DE LA INVESTIGACION/cirr_edad_sexo.png",height=18, width=22, units = "cm", res = 400)
dev.off()
boxplot(EDAD_CANT~ANO_DEF)
dev.print(png, "H:/MPGI/INP3130-1 METODOLOGIAS CUALITATIVAS Y CUANTITATIVAS DE LA INVESTIGACION/cirr_edad_ano.png",height=18, width=22, units = "cm", res = 400)
dev.off()
boxplot(EDAD_CANT~LOCAL_DEF)
dev.print(png, "H:/MPGI/INP3130-1 METODOLOGIAS CUALITATIVAS Y CUANTITATIVAS DE LA INVESTIGACION/cirr_edad_local.png",height=18, width=22, units = "cm", res = 400)
dev.off()
boxplot(EDAD_CANT~URBA_RURAL)
dev.print(png, "H:/MPGI/INP3130-1 METODOLOGIAS CUALITATIVAS Y CUANTITATIVAS DE LA INVESTIGACION/cirr_edad_urba.png",height=18, width=22, units = "cm", res = 400)
dev.off()
table(ANO_DEF,SEXO)

ANO_DEF <- c(2013:2017)
hombre <- c(8647026, 8739819, 8844799, 8965256, 9097254)
mujer <- c(8924481, 9019140, 9124554, 9243812, 9373185)

DFmaster <- data.frame(ANO_DEF, hombre, mujer)
DFmaster
DFcirrosis<-table(ANO_DEF,SEXO)
DFcirrosis
Def_Hombres <- c(2219, 2378, 2506, 2389, 2347)
Def_Mujeres <- c(782, 771, 825, 834, 767)

DFmaster<- data.frame(DFmaster,Def_Hombres, Def_Mujeres)
DFmaster
Tasa_Hombres <- ((Def_Hombres/Total_Hombres)*1000)
Tasa_Hombres

Tasa_Mujeres <- ((Def_Mujeres/Total_Mujeres)*1000)
Tasa_Mujeres

DFmaster<- data.frame(DFmaster,Tasa_Hombres, Tasa_Mujeres)
DFmaster

ggplot(data = DFmaster, aes(x= Año)) + 
geom_line(data = DFmaster, aes(y = Tasa_Hombres, colour = "Hombres"), size=1) +
geom_line(data = DFmaster, aes(y = Tasa_Mujeres, colour = "Mujeres"), size=1) +
scale_color_manual(values = c("Hombres"="red", "Mujeres" = "blue"))+
geom_point(data = DFmaster, aes(y = Tasa_Hombres), color = "red", size=2, fill="white") +
geom_point(data = DFmaster, aes(y = Tasa_Mujeres), color = "blue", size=2, fill="white") +
labs(title="Tasa de mortalidad por cirrosis", x='Año', y='Tasa de mortalidad/1000 habitantes', colour= "Sexo")+
ylim(0,0.35)+
theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom")