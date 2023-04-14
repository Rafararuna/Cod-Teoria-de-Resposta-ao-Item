library(irtoys)
library(ltm)
library(mirt)
library(CTT)



#############################UNIDADE 1##########################################

######################
CORRELAÇÃO PONTO-BISSERIAL
#####################


## correlação ponto-bisserial entre o escore total e o item 1:
biserial.cor(rowSums(altura.itens), altura.itens[[1]]) 

## Por padrão, a função biserial.cor() utiliza o valor 0 como referência. 
## Para utilizar o valor 1, utilize o argumento level=2:
rho.PB_1 <-  biserial.cor(rowSums(altura.itens), altura.itens[[1]],level=2) #VALOR PARA O ITEM 1

## Os valores da correlacao ponto bisserial para todos os itens está em altura.desc$bisCorr:
rho_PB <- altura.desc$bisCorr #VALORES PRA TODOS OS ITENS

#OBS: altura.itens = itens do questionario



######################
CORRELAÇÃO BISSERIAL
#####################

## correlação bisserial:
rho.B.vec <- rep(0,14)
for (i in 1:14) {
  pp <- colSums(altura.itens)[i]/nrow(altura.itens)
  rho.B <- sqrt(pp*(1-pp))*altura.desc$bisCorr[i]/dnorm(qnorm(pp,0,1),0,1)
  rho.B.vec[i] <- rho.B
}

rho.B.vec #VALORES DE TODOS OS ITENS

## exemplo da correlação bisserial para o item 1:
p_p <- colSums(altura.itens)[1]/nrow(altura.itens) #VALOR PARA O ITEM 1
rho.B_1 <- sqrt(p_p*(1-p_p))*rho.PB_1/dnorm(qnorm(p_p,0,1),0,1) #VALOR PARA O ITEM 1



######################
RELAÇÃO ENTRE PB X B
#####################


## relação entre a correlação ponto bisserial e a correlação bisserial:
plot(altura.desc$bisCorr, rho.B.vec, xlab=c("Correlação Ponto Bisserial"),
     ylab=c("Correlação Bisserial"),xlim=c(0,1),ylim=c(0,1))
abline(0,1)



######################
COEFICIENTE ALPHA DE CRONBACH
#####################

## coeficiente "alpha" de Cronbach:
cronbach.alpha(altura.itens)
cronbach.alpha(altura.itens[-1]) #excluindo o item 1

## para todos os itens:
altura.desc$alpha



######################
ÍNDICE DE DIFICULDADE DOS ITENS
#####################

## indice de dificuldade dos itens:
1-apply(altura.itens,2,sum)/nrow(altura.itens)



######################
COEFICIENTE DE DISCRIMINAÇÃO DOS ITENS
#####################

## coeficiente de discriminacao dos itens:
escore <- apply(altura.itens,1,sum)
aux <- ceiling(0.27*nrow(altura.itens))
escore.inf <- sort(escore)[aux]
escore.sup <- sort(escore)[nrow(altura.itens)-aux]

altura.inf <- altura.itens[escore<=escore.inf,]
altura.sup <- altura.itens[escore>=escore.sup,]

apply(altura.sup,2,sum)/nrow(altura.sup)-apply(altura.inf,2,sum)/nrow(altura.inf)



######################
OBSERVAÇÃO
#####################

## alternativamente, algumas estatisticas podem ser obtidas utilizando o pacote CTT:
library(CTT)

#ex: altura.reliab <- reliability(altura.itens)
altura.reliab <- itemAnalysis(altura.itens)
names(altura.reliab)
altura.reliab$pBis

str(altura.reliab)



#############################UNIDADE 2##########################################

######################
OBSERVAÇÃO
#####################

## antes de tudo, é necessário definir algumas variaveis:
#exemplo:
mat.par.1 <- matrix(c(1.8, .7, 1.8, 1.2, 1.2, .5, 1, 1, 1, -.5, .5, 0, 0.2, 0.2, .25, .2, 0.25, .25),nrow=6) #matrix do primeiro coeficiente
mat.par.2 <- matrix(c(2, .5, 1.5, 1.3, 1.1, .7, -1, 1, -1.5, .5, 1.5, 2, 0.2, 0.2, .25, .2, 0.25, .25),nrow=6) #matrix do segundo coeficiente

theta <- seq(-4,4,0.01) #amplitude do theta

mat.prob <- matrix(0,nrow(mat.par.1),length(theta)) #matriz de probabilidade



######################
GRÁFICO CCI
#####################

## Gráfico das CCI's para o teste 1:

for (i in 1:nrow(mat.par.1)) {
  for (j in 1:length(theta))
    mat.prob[i,j] <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))
}

plot(theta,mat.prob[1,],type="l",ylim=c(0,1), col = 2)

for (i in 2:nrow(mat.par.1))
  lines(theta,mat.prob[i,],lty=i, col = 2)
legend(-4,1,c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6"), lty = c(1:6), col = 2)

## Gráfico das CCI's para o teste 2:

for (i in 1:nrow(mat.par.2)) {
  for (j in 1:length(theta))
    mat.prob[i,j] <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))
}

plot(theta,mat.prob[1,],type="l",ylim=c(0,1), col = 2)

for (i in 2:nrow(mat.par.2))
  lines(theta,mat.prob[i,],lty=i, col = 2)
legend(-4,1,c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6"), lty=c(1:6), col = 2)



######################
PROBAB DE ACERTO
#####################

## para o teste 1:
vec.prob.1 <- mat.par.1[,3] + (1-mat.par.1[,3])/(1+exp(-mat.par.1[,1]*(0-mat.par.1[,2])))
vec.prob.1

## para o teste 2:
vec.prob.2 <- mat.par.2[,3] + (1-mat.par.2[,3])/(1+exp(-mat.par.2[,1]*(0-mat.par.2[,2])))
vec.prob.2 



######################
GRÁFICO FIT DOS ITENS
#####################

## grafico FIT do teste 1:
for (i in 1:nrow(mat.par.1)) {
  for (j in 1:length(theta)) {
    mat.prob[i,j] <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))
    mat.prob.dif[i,j] <- mat.par.1[i,1]*(1-mat.par.1[i,3])*exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2]))/
      ((1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

info.1 <- apply(mat.info,2,sum)

plot(theta,info.1,type="l", col = 2)

## grafico FIT do teste 2:
for (i in 1:nrow(mat.par.2)) {
  for (j in 1:length(theta)) {
    mat.prob[i,j] <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))
    mat.prob.dif[i,j] <- mat.par.2[i,1]*(1-mat.par.2[i,3])*exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2]))/
      ((1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

info.2 <- apply(mat.info,2,sum)

lines(theta,info.2,lty=2, col = 2)
legend(-4,1.5,c("Teste 1", "Teste 2"), lty=c(1,2), col = 2)



#############################UNIDADE 3##########################################

######################
AJUSTE DO MODELO LOGÍSTICO
#####################

no.item <- ncol(altura[,3:16]) # ITENS DO QUESTIONARIO
altura.tpm <- tpm(altura[,3:16],constraint=cbind(1:no.item,1,0))

## OBS: a função "tpm" do pacote "ltm" ajusta o modelo logístico de 1, 2 ou 3 parâmetros.
### No nosso caso, queremos ajustar o ML2. Isto é feito com a restrição "constraint=cbind(1:no.item,1,0)"
### que impõe o valor 0 para o primeiro parâmetro (o pacote considera a ordem c,b,a, isto é,
### o primeiro parâmetro é o de acerto ao acaso (c), o segundo é o de dificuldade (b)
### e o terceiro é o de discriminação (a).



######################
ESTIMATIVA DOS PARÂMETROS
#####################

par.est <- coef(altura.tpm) #cc, bb, aa



######################
GRÁFICO CCI DE UM ITEM
#####################

## Gráfico da CCI do item 5 (por exemplo)
theta.vec <- seq(-4,4,0.01)
prob <- 1/(1+exp(-par.est[5,3]*(theta.vec-par.est[5,2])))
plot(theta.vec,prob,type="l",xlab=c("Proficiência)"),ylab=c("Probabilidade de resposta positiva"),ylim=c(0,1))




######################
GRÁFICO FIT DOS ITENS E DO TESTE
#####################

## Gráfico das funções de informação dos itens:

mat.prob <- mat.prob.dif <- mat.info <- matrix(0,no.item,length(theta.vec))

for (i in 1:no.item) {
  for (j in 1:length(theta.vec)) {
    mat.prob[i,j] <- par.est[i,1] + (1-par.est[i,1])/(1+exp(-par.est[i,3]*(theta.vec[j]-par.est[i,2])))
    mat.prob.dif[i,j] <- par.est[i,3]*(1-par.est[i,1])*exp(-par.est[i,3]*(theta.vec[j]-par.est[i,2]))/
      ((1+exp(-par.est[i,3]*(theta.vec[j]-par.est[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

plot(theta.vec,mat.info[1,],type="l",ylim=c(0,max(mat.info)),xlab=c("Proficiência)"),ylab=c("Informação"),
     main=c("Fun???oes de informação dos itens"))
for (i in 2:no.item) 
  lines(theta.vec,mat.info[i,])


## Gráfico da função de informação do teste:

plot(theta.vec,apply(mat.info,2,sum),type="l",ylim=c(0,max(apply(mat.info,2,sum))),xlab=c("Proficiência)"),
     ylab=c("Informação"),main=c("Função de Informação do Teste"))



######################
ESTIMATIVA DA PROFICIÊNCIA E ERRO PADRÃO
#####################

## matriz com a estimativa da proficiência e o erro-padrão (1/sqrt(informacao(theta.est))):

theta.est.eap <- eap(altura[3:16], cbind(par.est[,3],par.est[,2],par.est[,1]), qu=normal.qu())



######################
RELAÇÃO COM A TRASNFORMAÇÃO LINEAR
#####################

## Transformação linear da altura estimada com média e variância iguais a altura real:

theta.est.eap <- eap(altura[3:16], cbind(par.est[,3],par.est[,2],par.est[,1]), qu=normal.qu())

theta.est <- mean(altura[,2]) + sd(altura[,2])*theta.est.eap[,1]

plot(altura[,2],theta.est)
abline(0,1)

cor(altura[,2],theta.est)



######################
ESTIMAÇÃO VIA TCT
#####################

escore <- apply(altura[,3:16],1,sum)
escore.padr <- (escore-mean(escore))/sd(escore)
theta.est.tct <- mean(altura[,2]) + sd(altura[,2])*escore.padr

plot(theta.est.tct,theta.est,xlab=c("Altura estimada via TCT"),ylab=c("Altura estimada via TRI"))
abline(0,1)

cor(theta.est.tct,theta.est)