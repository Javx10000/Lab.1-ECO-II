#################################################################

#Lab.1 Econometría II
#Elaborado por: Edwin Rueda & Mayerli Obando
#01/10/2022

#################################################################

#Librerías----
#Librerías usadas para el Lab.1 
library(fpp2)
library(tidyverse)
library(readxl)
library(forecast)
library(ggplot2)
library(lubridate)
library(car)
library(dplyr)
library(fitdistrplus)
library(stargazer)
library(tseries)
library(lmtest)
library(fitdistrplus)
library(fpp)

#Datos----
Datos <- read_excel("C:/Users/dell/Desktop/Trabajos económicos/Rstudio ECO II/Regresión Rstudio/Datos.xlsx")
View(Datos)

#Datos 2018-2020----
Datos3 <- read_excel("C:/Users/dell/Desktop/Trabajos económicos/Rstudio ECO II/Regresión Rstudio/Datos3.xlsx")
View(Datos3)

#Serie de tiempo general----
attach(Datos)
data.ts <- ts(Datos, start = c(2006,1), frequency = 4)
data.ts
plot(data.ts)


#PIB----
attach(Datos)
PIB <- c(PIB)
PIB.serie <- ts(PIB, start = c(2006,1), frequency = 4)
plot(PIB.serie)
PIB.serie

#C----
Cons <- c(Consumo)
Cons.serie <- ts(Cons, start = c(2006,1), frequency = 4)
plot(Cons.serie)
Cons.serie

#G----
Gasto <- c(Gasto)
Gasto.serie <- ts(Gasto, start = c(2006,1), frequency = 4)
plot(Gasto.serie)
Gasto.serie

#I----
Inv <- c(Inversión)
Inv.serie <- ts(Inv, start = c(2006,1), frequency = 4)
plot(Inv.serie)
Inv.serie

#Exp----
EXP <- c(Exp)
EXP.serie <- ts(EXP, start = c(2006,1), frequency = 4)
plot(EXP.serie)
EXP.serie

#Imp----
IMP <- c(Imp)
IMP.serie <- ts(IMP, start = c(2006,1), frequency = 4)
plot(IMP.serie)
IMP.serie

#Descomposición del PIB----
PIB.D <- decompose(PIB.serie, type = 'multiplicative')
autoplot(PIB.D)+
  labs(title = "Descomposición de la serie del PIB",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#Descomposición de C----
C.D <- decompose(Cons.serie, type = 'multiplicative')
autoplot(C.D)+
  labs(title = "Descomposición de la serie del Consumo",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#Descomposición de G----
G.D <- decompose(Gasto.serie, type = 'multiplicative')
autoplot(G.D)+
  labs(title = "Descomposición de la serie del Gasto",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#Descomposición de I----
I.D <- decompose(Inv.serie, type = 'multiplicative')
autoplot(I.D)+
  labs(title = "Descomposición de la serie de la Inversión",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#Descomposición de Exp----
EXP.D <- decompose(EXP.serie, type = 'multiplicative')
autoplot(EXP.D)+
  labs(title = "Descomposición de la serie de las exportaciones",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#Descomposición de Imp----
IMP.D <- decompose(IMP.serie, type = 'multiplicative')
autoplot(IMP.D)+
  labs(title = "Descomposición de la serie de las Importaciones",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#PIB con tendencia----
autoplot(PIB.serie, series = "PIB")+
  autolayer(trendcycle(PIB.D), series = "Tendencia")+
  labs(title = "Descomposición del PIB",
       x = "Tiempo", y = "Valor",
       colour = "Gears")+theme_bw()

#índice estacional del PIB----
D <- decompose(x = PIB.serie, type = "multiplicative")
IE <-  D$figure
Trimestres <- c("1T", "2T", 
                "3T", "4T")
df.ie <- data.frame(Trimestres,IE)

#Con líneas
ggplot(data = df.ie,aes(x=1:4,y=IE)) +
  geom_line(color= "blue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = 1,
             lty        = 2,
             color      = "black") +
  scale_x_continuous(breaks = NULL,name = "Trimestres") +
  scale_y_continuous(name = "Índice estacional") +
  annotate(geom  = "text",
           x     = 1:4 ,
           y     = IE*0.99,
           label = Trimestres)

#Con barras
ggplot(data = df.ie)+ 
  geom_bar(aes(x=Trimestres, y=IE),stat="identity", fill="grey",
           colour="black", size=0.5)+
  labs(title= "índice estacional",
       x="Trimestre",y="Factor estacional")

#PIB (2018-2020)----
attach(Datos3)

PIB1820 <- ts(PIB1, frequency = 4,
              start=c(2018,1), end=c(2020,4))

PIBrec <- decompose(PIB1820, type = 'multiplicative')
autoplot(PIBrec)+
  labs(title = "Descomposición de la serie del PIB (2018-2020)",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#C (2018-2020)----
CONS1820 <- ts(Consumo1,frequency = 4,
               start=c(2018,1), end=c(2020,4))

CONSrec <- decompose(CONS1820, type = 'multiplicative')
autoplot(CONSrec)+
  labs(title = "Descomposición de la serie del Consumo (2018-2020)",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#G (2018-2020)----
GASTO1820 <- ts(Gasto1,frequency = 4,
               start=c(2018,1), end=c(2020,4))

GASTOrec <- decompose(GASTO1820, type = 'multiplicative')
autoplot(GASTOrec)+
  labs(title = "Descomposición de la serie del Gasto (2018-2020)",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#I (2018-2020)----
INV1820 <- ts(Inversión1,frequency = 4,
                start=c(2018,1), end=c(2020,4))

INVrec <- decompose(INV1820, type = 'multiplicative')
autoplot(INVrec)+
  labs(title = "Descomposición de la serie de la Inversión (2018-2020)",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+theme_bw()

#EXP (2018-2020)----
EXP1820 <- ts(Exp1,frequency = 4,
              start=c(2018,1), end=c(2020,4))

EXPrec <- decompose(EXP1820, type = 'multiplicative')
autoplot(EXPrec)+
  labs(title = "Descomposición de la serie de las exportaciones (2018-2020)",
       y = "Valor",
       colour = "Gears")+theme_bw()

#IMP (2018-2020)----
IMP1820 <- ts(Imp1,frequency = 4,
              start=c(2018,1), end=c(2020,4))

IMPrec <- decompose(IMP1820, type = 'multiplicative')
autoplot(IMPrec)+
  labs(title = "Descomposición de la serie de las importaciones (2018-2020)",
       y = "Valor",
       colour = "Gears")+theme_bw()

#Modelo Log-Log 
#Elasticidades Y=PIB , X(DA)----
lnPIB <- log(PIB)
lnPIB
lnCons <- log(Cons)
lnCons
lnGasto <- log(Gasto)
lnGasto
lnInv <- log(Inv)
lnInv
lnExp <- log(EXP)
lnExp
lnImp <- log(IMP)
lnImp

reglog <- data.frame(lnCons,lnGasto,lnInv,lnImp,lnExp)
log<-lm(formula = lnPIB~.,reglog)
summary(log)

#Función Consumo Kyenesiana
# C = f(Yd) (Kyenes, 1956)
FC <- lm(formula = Cons~PIB, data = Datos)
summary(FC)

#Gráfico de dispersión de C ~ Y
grafica1 = ggplot(Datos, aes(PIB,Cons))
grafica1+geom_point()+geom_smooth(method = 
"lm", colour = "blue")+labs(title = "Diagrama de dispersión")

#Regresión con tendencia de la f(C) Keynesiana
PIBCONS = cbind(PIB.serie,Cons.serie)
PIBCONS

tendencia=seq_along(Cons.serie)
print(tendencia)
options(scipen = 999)
ModeloKeynes = lm(Cons.serie~PIB.serie+tendencia, data = PIBCONS)
summary(ModeloKeynes)
plot(ModeloKeynes)

#Pruebas a los supuestos
plot(fitted(ModeloKeynes), resid(ModeloKeynes), col = "black", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, col = "blue", lwd = 3)

#Homocedasticidad (Breusch & Pagan, 1979)
ncvTest(ModeloKeynes)
bptest(ModeloKeynes)

#Normalidad (Jaque & Bera, 1987) y (Shapiro & Wilk, 1965)
shapiro.test(ModeloKeynes$residuals)
jarque.bera.test(ModeloKeynes$residuals)

#Histograma
hist(ModeloKeynes$residuals,prob=TRUE)
+lines(density(ModeloKeynes$residuals))

MKnormal <- fitdist(ModeloKeynes$residuals, distr = "norm")
plot(MKnormal)

#Multicolinealidad
VifsMK <- vif(ModeloKeynes)
print(VifsMK)
#VIF < 10 (Aceptable en el modelo)

##################################################################
