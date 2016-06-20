#
#	EST-46114:	Inferencia Bayesiana en Alta Dimension (Maestria en Ciencia de Datos)
#	Autor: 		Juan~Carlos Martinez Ovando
#	Email:		juan.martinez.ovando@itam.mx
#					jc.martinez.ovando@gmail.com
#	
#	Seleccion Estocastica de Variables
#	== Modelos Lineales Generalizados: Probit ==
#

path_to_project <- "/home/denny/github/DPA_Ecobici/"
setwd(paste0(path_to_project,"data/"))


#	----------------------------------------------------
#		Libraries
#	----------------------------------------------------
#install.packages('mvtnorm')
library(mvtnorm)

#	----------------------------------------------------
#		Datos
#	----------------------------------------------------
dde <- read.csv(paste('master_y.csv',sep = ""), header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE)


# dde$yy <- seq(0.0,length(nrow(dde_1$flujo_rel)))
# for(i in 1:length(dde$flujo_rel)){
#   if((dde$flujo_rel[i] > - 0.01) & (dde$flujo_rel[i] < 0.01)){
#     dde$yy[i] <- 0.0
#   }else {
#     dde$yy[i] <- 1.0
#   }
# }

# 0 equivale a que están balanceadas (376,107 obs)
# equivale que están desvaleanceadas (314,499 obs)


#complete cases:
dde_1 <- dde[complete.cases(dde),]

# 	Normalization -NO UTILIZO ESTA NORMALIZACION
#dde_1$flujo_rel<- (dde_1$flujo_rel - mean(dde_1$flujo_rel))/sqrt(var(dde_1$flujo_rel))


#	--------------------------------------------			
#		Analisis frecuentista
#	--------------------------------------------			
#REGION 12
X_r0 <- subset(dde_1, dde_1$region_12==1)

X <-  cbind(X_r0$edad_prom,X_r0$prop_fem,X_r0$duracion_prom,X_r0$distancia_prom, 
            X_r0$maxcapacity,X_r0$MAX_DIA_O3,X_r0$MAX_DIA_PM10,X_r0$MAX_DIA_RH,X_r0$MAX_DIA_TMP,X_r0$MAX_DIA_WDR,X_r0$MAX_DIA_WSP,
            X_r0$wd_3,X_r0$wd_4,X_r0$wd_5,X_r0$wd_6,
            X_r0$hora_6,X_r0$hora_7,X_r0$hora_8,X_r0$hora_9,X_r0$hora_10,X_r0$hora_11)
Y <- X_r0$yy
#Me marcó error en dummies, elimino wd 2, será la base del analisis

names_X <- c("edad_prom", "prop_fem","duracion_prom", "distancia_prom", "maxcapacity",
             "MAX_DIA_O3","MAX_DIA_PM10","MAX_DIA_RH","MAX_DIA_TMP","MAX_DIA_WDR","MAX_DIA_WSP",
             "wd_3","wd_4","wd_5","wd_6",
             "hora_6","hora_7","hora_8","hora_9","hora_10","hora_11")

#frec <- lm(Y ~ -1+X)
dde_mle<- glm(Y ~ -1+X, family=binomial("probit"))


#	Estimador maximo verosimil
beta_mle<- dde_mle$coef
#beta_mle <- frec$coefficients
betas_iniciales <- as.data.frame(beta_mle)

#	--------------------------------------------			
#		Analisis bayesiano
#	--------------------------------------------			

# 		Especificacion inicial 
#beta0 <- rep(0,length(dde_mle$coefficients))
beta0 <- rep(0,ncol(X))
#Pbeta0 <- 0.25*diag(length(dde_mle$coefficients))
Pbeta0 <- 0.25*diag(ncol(X))
#		MCMC
#beta <- rep(0,length(dde_mle$coefficients))	# Valor inicial de la cadena de Markov
beta <- rep(0,ncol(X))	# Valor inicial de la cadena de Markov
n <- nrow(X)		# Numero de observaciones/individuos
z <- rep(0,nrow(X))		# Valores iniciales de las variables latentes
G <- 1000		# Numero de iteraciones del MCMC

# 		Gibbs sampler
gt <- 1
for(gt in 1:G){
  eta<- X%*%beta 	# Predictor lineal (variable auxiliar)
  
  #	Muestreo de la distribucion truncada para Z
  #	de las distribuciones condicionales completas
  z[Y==0] <- qnorm(runif(sum(1-Y), 0,pnorm(0,eta[Y==0],1)), eta[Y==0],1)
  z[Y==1] <- qnorm(runif(sum(Y), pnorm(0,eta[Y==1],1),1), eta[Y==1],1)
  
  z <- as.vector(z)
  #	Muestreo de la distribucion final completa para beta
  Vbeta <- (solve(Pbeta0 + t(X)%*%X))
  Ebeta <- (Vbeta%*%(Pbeta0%*%beta0 + t(X)%*%z))
  beta <- c(rmvnorm(1,Ebeta,Vbeta))
  
  #	Output
  write(t(beta),file=paste(path_to_project,'results/R_12_beta.out',sep = ""), ncol=ncol(X), append=T)
  print(c(gt,round(beta*100)/100))
}

#	Trace-plots para beta[1] y beta[2]
#pdf(paste(path_to_project,'results/R_12_beta_traces.pdf',sep = ""),width=7,height=5)
# par(mfrow=c(2,1))
beta_out<- matrix(scan(paste(path_to_project,'results/R_12_beta.out',sep = "")), ncol=ncol(X), byrow=T)
# plot(beta_out[,1],type="l",xlab="iteration",ylab="(beta_1)")
# plot(beta_out[,2],type="l",xlab="iteration",ylab="(beta_2)")
#dev.off()

pdf(paste(path_to_project,'results/R_12_beta_traces_0.pdf',sep = ""),width=7,height=5)
par(mfrow=c(3,3))
plot(beta_out[,1],type="l",xlab="iteration",ylab=names_X[1])
plot(beta_out[,2],type="l",xlab="iteration",ylab=names_X[2])
plot(beta_out[,3],type="l",xlab="iteration",ylab=names_X[3])
plot(beta_out[,4],type="l",xlab="iteration",ylab=names_X[4])
plot(beta_out[,5],type="l",xlab="iteration",ylab=names_X[5])
plot(beta_out[,6],type="l",xlab="iteration",ylab=names_X[6])
plot(beta_out[,7],type="l",xlab="iteration",ylab=names_X[7])
plot(beta_out[,8],type="l",xlab="iteration",ylab=names_X[8])
plot(beta_out[,9],type="l",xlab="iteration",ylab=names_X[9])
dev.off()

pdf(paste(path_to_project,'results/R_12_beta_traces_1.pdf',sep = ""),width=7,height=5)
par(mfrow=c(3,3))
plot(beta_out[,10],type="l",xlab="iteration",ylab=names_X[10])
plot(beta_out[,11],type="l",xlab="iteration",ylab=names_X[11])
plot(beta_out[,12],type="l",xlab="iteration",ylab=names_X[12])
plot(beta_out[,13],type="l",xlab="iteration",ylab=names_X[13])
plot(beta_out[,14],type="l",xlab="iteration",ylab=names_X[14])
plot(beta_out[,15],type="l",xlab="iteration",ylab=names_X[15])
plot(beta_out[,16],type="l",xlab="iteration",ylab=names_X[16])
plot(beta_out[,17],type="l",xlab="iteration",ylab=names_X[17])
plot(beta_out[,18],type="l",xlab="iteration",ylab=names_X[18])
dev.off()

pdf(paste(path_to_project,'results/R_12_beta_traces_2.pdf',sep = ""),width=7,height=5)
par(mfrow=c(3,3))
plot(beta_out[,19],type="l",xlab="iteration",ylab=names_X[19])
plot(beta_out[,20],type="l",xlab="iteration",ylab=names_X[20])
plot(beta_out[,21],type="l",xlab="iteration",ylab=names_X[21])
dev.off()

#Para conocer cual es la media de los coeficientes de las últimas 100 iteraciones:
betas_finales <- as.data.frame(colMeans(tail(beta_out,-100)))
betas_ok <- cbind(betas_iniciales,betas_finales)
betas_ok <- cbind(names_X,betas_ok)
names(betas_ok) <- c("variable","coef_MV", "coef_final")
print(betas_ok)
write.csv(betas_ok,file='../results/R_12_betas_finales.csv')

#	Distribucion final marginal
pdf(paste(path_to_project,'results/R12_density_1.pdf',sep = ""),width=7,height=5)
par(mfrow=c(3,3))
slp1 <- beta_out[101:1000,1]
slp2 <- beta_out[101:1000,2]
slp3 <- beta_out[101:1000,3]
slp4 <- beta_out[101:1000,4]
slp5 <- beta_out[101:1000,5]
slp6 <- beta_out[101:1000,6]
slp7 <- beta_out[101:1000,7]
slp8 <- beta_out[101:1000,8]
slp9 <- beta_out[101:1000,9]
par(mar=c(5,5,5,5))
plot(density(slp1),type="l",xlab=names_X[1],ylab="Posterior Density",cex=1.2, main=names_X[1])
abline(v=mean(slp1))
par(mar=c(5,5,5,5))
plot(density(slp2),type="l",xlab=names_X[2],ylab="Posterior Density",cex=1.2, main=names_X[2])
abline(v=mean(slp2))
par(mar=c(5,5,5,5))
plot(density(slp3),type="l",xlab=names_X[3],ylab="Posterior Density",cex=1.2, main=names_X[3])
abline(v=mean(slp3))
par(mar=c(5,5,5,5))
plot(density(slp4),type="l",xlab=names_X[4],ylab="Posterior Density",cex=1.2, main=names_X[4])
abline(v=mean(slp4))
par(mar=c(5,5,5,5))
plot(density(slp5),type="l",xlab=names_X[5],ylab="Posterior Density",cex=1.2, main=names_X[5])
abline(v=mean(slp5))
par(mar=c(5,5,5,5))
plot(density(slp6),type="l",xlab=names_X[6],ylab="Posterior Density",cex=1.2, main=names_X[6])
abline(v=mean(slp6))
par(mar=c(5,5,5,5))
plot(density(slp7),type="l",xlab=names_X[7],ylab="Posterior Density",cex=1.2, main=names_X[7])
abline(v=mean(slp7))
par(mar=c(5,5,5,5))
plot(density(slp8),type="l",xlab=names_X[8],ylab="Posterior Density",cex=1.2, main=names_X[8])
abline(v=mean(slp8))
par(mar=c(5,5,5,5))
plot(density(slp9),type="l",xlab=names_X[9],ylab="Posterior Density",cex=1.2, main=names_X[9])
abline(v=mean(slp9))
dev.off()

pdf(paste(path_to_project,'results/R12_density_2.pdf',sep = ""),width=7,height=5)
par(mfrow=c(3,3))
slp10 <- beta_out[101:1000,10]
slp11 <- beta_out[101:1000,11]
slp12 <- beta_out[101:1000,12]
slp13 <- beta_out[101:1000,13]
slp14 <- beta_out[101:1000,14]
slp15 <- beta_out[101:1000,15]
slp16 <- beta_out[101:1000,16]
slp17 <- beta_out[101:1000,17]
slp18 <- beta_out[101:1000,18]
par(mar=c(5,5,5,5))
plot(density(slp10),type="l",xlab=names_X[10],ylab="Posterior Density",cex=1.2, main=names_X[10])
abline(v=mean(slp10))
par(mar=c(5,5,5,5))
plot(density(slp11),type="l",xlab=names_X[11],ylab="Posterior Density",cex=1.2, main=names_X[11])
abline(v=mean(slp11))
par(mar=c(5,5,5,5))
plot(density(slp12),type="l",xlab=names_X[12],ylab="Posterior Density",cex=1.2, main=names_X[12])
abline(v=mean(slp12))
par(mar=c(5,5,5,5))
plot(density(slp13),type="l",xlab=names_X[13],ylab="Posterior Density",cex=1.2, main=names_X[13])
abline(v=mean(slp13))
par(mar=c(5,5,5,5))
plot(density(slp14),type="l",xlab=names_X[14],ylab="Posterior Density",cex=1.2, main=names_X[14])
abline(v=mean(slp14))
par(mar=c(5,5,5,5))
plot(density(slp15),type="l",xlab=names_X[15],ylab="Posterior Density",cex=1.2, main=names_X[15])
abline(v=mean(slp15))
par(mar=c(5,5,5,5))
plot(density(slp16),type="l",xlab=names_X[16],ylab="Posterior Density",cex=1.2, main=names_X[16])
abline(v=mean(slp16))
par(mar=c(5,5,5,5))
plot(density(slp17),type="l",xlab=names_X[17],ylab="Posterior Density",cex=1.2, main=names_X[17])
abline(v=mean(slp17))
par(mar=c(5,5,5,5))
plot(density(slp18),type="l",xlab=names_X[18],ylab="Posterior Density",cex=1.2, main=names_X[18])
abline(v=mean(slp18))
dev.off()


pdf(paste(path_to_project,'results/R12_density_3.pdf',sep = ""),width=7,height=5)
par(mfrow=c(3,3))
slp19 <- beta_out[101:1000,19]
slp20 <- beta_out[101:1000,20]
slp21 <- beta_out[101:1000,21]
par(mar=c(5,5,5,5))
plot(density(slp19),type="l",xlab=names_X[19],ylab="Posterior Density",cex=1.2, main=names_X[19])
abline(v=mean(slp19))
par(mar=c(5,5,5,5))
plot(density(slp20),type="l",xlab=names_X[20],ylab="Posterior Density",cex=1.2, main=names_X[20])
abline(v=mean(slp20))
par(mar=c(5,5,5,5))
plot(density(slp21),type="l",xlab=names_X[21],ylab="Posterior Density",cex=1.2, main=names_X[21])
abline(v=mean(slp21))
dev.off()


#FITTED
#Se obtiene la Y estimada
a <- as.matrix(betas_finales)
X_F <- as.matrix(X[,1:21])
fit <- (X_F)%*%(a)
dev.new()
hist(fit,1000)


#Se invierte la Y estimada para obtener los valores entre 0 y 1
#install.packages('VGAM')
library(VGAM)
#dev.new()
#hist(probit(fit,deriv = 0, inverse=TRUE))
fit_1 <- probit(fit,deriv = 0, inverse=TRUE)

#Tomamos el umbral en 0.5
yy_fit2 <- seq(0.0,length(nrow(fit_1)))
for(i in 1:length(fit_1)){
  if(fit_1[i] < 0.5){
    yy_fit2[i] <- 0.0
  }else {
    yy_fit2[i] <- 1.0
  }
}

#Comparamos los valores estimados vs los reales
compare <- cbind(Y, yy_fit2)
mal <- rep(0,length(yy_fit2))
for(i in 1:length(Y)){
  if(Y[i]==yy_fit2[i]){
    mal[i] <- 0
  }else{
    mal[i] <-1
  }
}
compare <- cbind(compare, mal)
proporcion <- sum(mal)/length(Y)
#0.2782038