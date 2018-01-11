#exeleg - Buchbeitrag "Aus der Balance? Das Verhältnis von Parlament und Regierung im internationalen Vergleich"

#Julian Bernauer und Martina Flick
#Version 11. Januar 2018

library(foreign)
library(R2jags)
library(tidyverse)
library(psych)

setwd("...")

#Data for 22 countries from multiple sources 
#(Fish and Kroenig 2009, Russo and Wiberg 2010, Siaroff 2003, Seebaldt 2009, Z'Graggen 2009)
#see text for sources 
load("exeleg.Rdata")
attach(exeleg)

N <- length(appoint_pm)

#factors! 
elect_pm_fact2 <- as.numeric(elect_pm_fact)
elect_pm_fact2[elect_pm_fact2==1] <- NA
elect_pm_fact2[elect_pm_fact2==2] <- 1 
elect_pm_fact2[elect_pm_fact2==3] <- 2 
elect_pm_fact2[elect_pm_fact2==4] <- 3 
elect_pm_fact2[elect_pm_fact2==5] <- 3 

elect_min_fact2 <- as.numeric(elect_min_fact)
elect_min_fact2[elect_min_fact2==1] <- NA
elect_min_fact2[elect_min_fact2==2] <- 0
elect_min_fact2[elect_min_fact2==3] <- 1
elect_min_fact2[elect_min_fact2==4] <- 1

leg_init_fact2 <- as.numeric(leg_init_fact)
leg_init_fact2[leg_init_fact2==1] <- NA
leg_init_fact2[leg_init_fact2==2] <- 0
leg_init_fact2[leg_init_fact2==3] <- 1
leg_init_fact2[leg_init_fact2==4] <- 1

quest2 <- as.numeric(quest)
leg_field_fact2 <- as.numeric(leg_field_fact)
leg_field_fact2[leg_field_fact2==1] <- NA
leg_field_fact2[leg_field_fact2==2] <- 1
leg_field_fact2[leg_field_fact2==3] <- 2
leg_field_fact2[leg_field_fact2==4] <- 3

leg_auto_fact2 <- as.numeric(leg_auto_fact)
leg_auto_fact2[leg_auto_fact2==1] <- NA 
leg_auto_fact2[leg_auto_fact2==2] <- 1
leg_auto_fact2[leg_auto_fact2==3] <- 2
leg_auto_fact2[leg_auto_fact2==4] <- 3
leg_auto_fact2[leg_auto_fact2==5] <- 3

deselect_gov_fact2 <- as.numeric(deselect_gov_fact)
deselect_gov_fact2[deselect_gov_fact2==1] <- NA 
deselect_gov_fact2[deselect_gov_fact2==2] <- 1
deselect_gov_fact2[deselect_gov_fact2==3] <- 2
deselect_gov_fact2[deselect_gov_fact2==4] <- 3
deselect_gov_fact2[deselect_gov_fact2==5] <- 4
deselect_gov_fact2[deselect_gov_fact2==6] <- 5

contr_gov_fact2 <- as.numeric(contr_gov_fact)
contr_gov_fact2[contr_gov_fact2==1] <- NA 
contr_gov_fact2[contr_gov_fact2==2] <- 1
contr_gov_fact2[contr_gov_fact2==3] <- 2
contr_gov_fact2[contr_gov_fact2==4] <- 3
contr_gov_fact2[contr_gov_fact2==5] <- 4

exeleg.data <- list(N=N, pmfa_see=elect_pm_fact2, minfa_see=elect_min_fact2,quest=quest2,linfa=leg_init_fact2,lfifa=leg_field_fact2,laufa=leg_auto_fact2,degfa=deselect_gov_fact2,cogfa=contr_gov_fact2,
                    staff=staff, 
                    secr=secretary,
                    fin=inc_parl, cost=cost_parl, proft=time_session, profc=time_comm)


###
#Measurement model 
model <- "model{

for(j in 1:N){
minfa_see[j] ~ dbern(p.minfa_see[j])
p.minfa_see[j] <- 1/(1+exp(-z.minfa_see[j]))
z.minfa_see[j] <- alpha.minfa_see + gamma.minfa_see1*x1[j] 

linfa[j] ~ dbern(p.linfa[j])
p.linfa[j] <- 1/(1+exp(-z.linfa[j]))
z.linfa[j] <- alpha.linfa + gamma.linfa1*x1[j] 

quest[j] ~ dcat(p.quest[j,1:4])
    mu.quest[j] <- beta.quest1*x1[j] 
    logit(Q.quest[j,1]) <- tau.quest[1]-mu.quest[j]
    p.quest[j,1] <- Q.quest[j,1]
    logit(Q.quest[j,2]) <- tau.quest[2] - mu.quest[j]
    p.quest[j,2] <- Q.quest[j,2] - Q.quest[j,1]
    logit(Q.quest[j,3]) <- tau.quest[3] - mu.quest[j]
    p.quest[j,3] <- Q.quest[j,3] - Q.quest[j,2]
    p.quest[j,4] <- 1 - Q.quest[j,3]

	pmfa_see[j] ~ dcat(p.pmfa_see[j,1:3])
    mu.pmfa_see[j] <- beta.pmfa_see1*x1[j]
logit(Q.pmfa_see[j,1]) <- tau.pmfa_see[1] - mu.pmfa_see[j]
p.pmfa_see[j,1] <- Q.pmfa_see[j,1]
logit(Q.pmfa_see[j,2]) <- tau.pmfa_see[2] - mu.pmfa_see[j]
p.pmfa_see[j,2] <- Q.pmfa_see[j,2] - Q.pmfa_see[j,1] 
p.pmfa_see[j,3] <- 1 - Q.pmfa_see[j,2]

	lfifa[j] ~ dcat(p.lfifa[j,1:3])
    mu.lfifa[j] <- beta.lfifa1*x1[j]
logit(Q.lfifa[j,1]) <- tau.lfifa[1] - mu.lfifa[j]
p.lfifa[j,1] <- Q.lfifa[j,1]
logit(Q.lfifa[j,2]) <- tau.lfifa[2] - mu.lfifa[j]
p.lfifa[j,2] <- Q.lfifa[j,2] - Q.lfifa[j,1] 
p.lfifa[j,3] <- 1 - Q.lfifa[j,2]

	laufa[j] ~ dcat(p.laufa[j,1:3])
    mu.laufa[j] <- beta.laufa1*x1[j]
logit(Q.laufa[j,1]) <- tau.laufa[1] - mu.laufa[j]
p.laufa[j,1] <- Q.laufa[j,1]
logit(Q.laufa[j,2]) <- tau.laufa[2] - mu.laufa[j]
p.laufa[j,2] <- Q.laufa[j,2] - Q.laufa[j,1] 
p.laufa[j,3] <- 1 - Q.laufa[j,2]


 cogfa[j] ~ dcat(p.cogfa[j,1:4])
    mu.cogfa[j] <- beta.cogfa1*x1[j] 
    logit(Q.cogfa[j,1]) <- tau.cogfa[1]-mu.cogfa[j]
    p.cogfa[j,1] <- Q.cogfa[j,1]
    logit(Q.cogfa[j,2]) <- tau.cogfa[2] - mu.cogfa[j]
    p.cogfa[j,2] <- Q.cogfa[j,2] - Q.cogfa[j,1]
    logit(Q.cogfa[j,3]) <- tau.cogfa[3] - mu.cogfa[j]
    p.cogfa[j,3] <- Q.cogfa[j,3] - Q.cogfa[j,2]
    p.cogfa[j,4] <- 1 - Q.cogfa[j,3]


 degfa[j] ~ dcat(p.degfa[j,1:5])
    mu.degfa[j] <- beta.degfa1*x1[j] 
    logit(Q.degfa[j,1]) <- tau.degfa[1]-mu.degfa[j]
    p.degfa[j,1] <- Q.degfa[j,1]
    logit(Q.degfa[j,2]) <- tau.degfa[2] - mu.degfa[j]
    p.degfa[j,2] <- Q.degfa[j,2] - Q.degfa[j,1]
    logit(Q.degfa[j,3]) <- tau.degfa[3] - mu.degfa[j]
    p.degfa[j,3] <- Q.degfa[j,3] - Q.degfa[j,2]
    logit(Q.degfa[j,4]) <- tau.degfa[4] - mu.degfa[j]
    p.degfa[j,4] <- Q.degfa[j,4] - Q.degfa[j,3]
    p.degfa[j,5] <- 1 - Q.degfa[j,4]

}

alpha.linfa ~ dnorm(0,.001)
alpha.minfa_see ~ dnorm(0,.001)

gamma.minfa_see1 ~ dnorm(0,.001) 
gamma.linfa1 ~ dnorm(0,.001)

beta.pmfa_see1  ~ dnorm(0,.001) I(0,)
beta.lfifa1  ~ dnorm(0,.001)
beta.laufa1  ~ dnorm(0,.001)
beta.cogfa1  ~ dnorm(0,.001)
beta.degfa1  ~ dnorm(0,.001)

tau.quest[1] ~ dnorm(0,.01)
  for (j in 1:2){
delta.quest[j] ~ dexp(2)
tau.quest[j+1] <- tau.quest[j] + delta.quest[j]
}

beta.quest1 ~ dnorm(0, .001) 

 tau.pmfa_see[1] ~ dnorm(0,.01)
  for (j in 1:1){
    delta.pmfa_see[j] ~ dexp(2)
    tau.pmfa_see[j+1] <- tau.pmfa_see[j] + delta.pmfa_see[j]
  }

 tau.lfifa[1] ~ dnorm(0,.01)
  for (j in 1:1){
    delta.lfifa[j] ~ dexp(2)
    tau.lfifa[j+1] <- tau.lfifa[j] + delta.lfifa[j]
  }

 tau.laufa[1] ~ dnorm(0,.01)
  for (j in 1:1){
    delta.laufa[j] ~ dexp(2)
    tau.laufa[j+1] <- tau.laufa[j] + delta.laufa[j]
  }

  
  tau.cogfa[1] ~ dnorm(0,.01)
  for (j in 1:2){
    delta.cogfa[j] ~ dexp(2)
    tau.cogfa[j+1] <- tau.cogfa[j] + delta.cogfa[j]
  }

  tau.degfa[1] ~ dnorm(0,.01)
  for (j in 1:3){
delta.degfa[j] ~ dexp(2)
tau.degfa[j+1] <- tau.degfa[j] + delta.degfa[j]
}


for(j in 1:N){

staff[j] ~ dbern(p.staff[j])
p.staff[j] <- 1/(1+exp(-z.staff[j]))
z.staff[j] <- alpha.staff + gamma.staff2*x2[j] 

secr[j] ~ dbern(p.secr[j])
p.secr[j] <- 1/(1+exp(-z.secr[j]))
z.secr[j] <- alpha.secr + gamma.secr2*x2[j] 

fin[j] ~ dnorm(mu.fin[j],tau.fin)
mu.fin[j] <- alpha.fin + gamma.fin2*x2[j]

cost[j] ~ dnorm(mu.cost[j],tau.cost)
mu.cost[j] <- alpha.cost + gamma.cost2*x2[j]

proft[j] ~ dnorm(mu.proft[j],tau.proft)
mu.proft[j] <- alpha.proft + gamma.proft2*x2[j]

profc[j] ~ dnorm(mu.profc[j],tau.profc)
mu.profc[j] <- alpha.profc + gamma.profc2*x2[j]

}

alpha.staff ~ dnorm(0,.001)
alpha.secr ~ dnorm(0,.001)

gamma.staff2 ~ dnorm(0,.001)
gamma.secr2 ~ dnorm(0,.001)

alpha.fin ~ dnorm(0, .001)
gamma.fin2 ~ dnorm(0, .001) I(0,)
tau.fin <- pow(sigma.fin, -2)
sigma.fin ~ dunif(0, 50)

alpha.cost ~ dnorm(0, .001)
gamma.cost2 ~ dnorm(0, .001) 
tau.cost <- pow(sigma.cost, -2)
sigma.cost ~ dunif(0, 50)

alpha.proft ~ dnorm(0, .001)
gamma.proft2 ~ dnorm(0, .001) I(0,)
tau.proft <- pow(sigma.proft, -2)
sigma.proft ~ dunif(0, 50)

alpha.profc ~ dnorm(0, .001)
gamma.profc2 ~ dnorm(0, .001) 
tau.profc <- pow(sigma.profc, -2)
sigma.profc ~ dunif(0, 50)

for(j in 1:3){
x1[j] ~ dnorm(0,1) 
x2[j] ~ dnorm(0,1)  
}

#D bei Institutionen und Ressourcen stark 
x1[4] ~ dnorm(0,1) I(0,)
x2[4] ~ dnorm(0,1) I(0,)

for(j in 5:22){
x1[j] ~ dnorm(0,1) 
x2[j] ~ dnorm(0,1)  
}

}"
  
write(model, file="exelegmodel.jags")

exeleg.parameters <- c("gamma.minfa_see1",
                       "gamma.linfa1",
                       "beta.pmfa_see1",
                       "beta.lfifa1",
                       "beta.laufa1",
                       "beta.cogfa1",
                       "beta.degfa1",
                       "beta.quest1",
                       "gamma.staff2",
                       "gamma.secr2",
                       "gamma.fin2",
                       "gamma.cost2",
                       "gamma.proft2",
                       "gamma.profc2",
                       "x1","x2")

jags.exeleg <- jags.model(file="exelegmodel.jags", data = exeleg.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.exeleg, exeleg.parameters, n.iter=100, thin=1)
samplesburn <- coda.samples(jags.exeleg, exeleg.parameters, n.iter=9800, thin=98)
samples <- coda.samples(jags.exeleg, exeleg.parameters, n.iter=10000, thin=100)

#plot(sampleshelp, ask=TRUE) 
plot(samples, ask=TRUE)

#parameters 
kette <- as.matrix(samples)

###
#extract parameters and scores 

output <- as.data.frame(kette) 
x1 <- 0
for(i in 15:36){x1[i] <- mean(output[,i])}
x1 <- x1[15:36]
x1 
x2 <- 0
for(i in 37:58){x2[i] <- mean(output[,i])}
x2 <- x2[37:58]
x2 

#plot dimensions 
plot(x1,x2, main="", xlab="Institutionen", ylab="Ressourcen", type="n")
text(x1,x2,krzel,cex=1)
abline(lm(x2 ~ x1))

cor(x1,x2)

mcoeffs <- 0
for(i in 1:14){mcoeffs[i] <- mean(output[,i])}
mcoeffs <- mcoeffs[1:14]
mcoeffs
m.v <- c(mcoeffs[5],mcoeffs[10]/5,mcoeffs[9],mcoeffs[4],mcoeffs[3],mcoeffs[2],mcoeffs[1],mcoeffs[6],mcoeffs[14],mcoeffs[13]/5,mcoeffs[8],mcoeffs[7],mcoeffs[12],mcoeffs[11])

scoeffs <- 0
for(i in 1:14){scoeffs[i] <- sd(output[,i])}
scoeffs <- scoeffs[1:14]
scoeffs
sd.v <- c(scoeffs[5],scoeffs[10]/5,scoeffs[9],scoeffs[4],scoeffs[3],scoeffs[2],scoeffs[1],scoeffs[6],scoeffs[14],scoeffs[13]/5,scoeffs[8],scoeffs[7],scoeffs[12],scoeffs[11])

var.names <- c("Wahl Regierungschef","Wahl Minister (/5)","Initiativhoheit","Gestaltungsfeld","Gestaltungsautonomie","Abwahl","Kontrolle","Befragung",
               "Wiss. Mitarbeiter","Parl. Sekretariat (/5)","Einkommen","Kosten","Zeitaufwand Plenum","Zeitaufwand Komm.")

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-6,14), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-6,14, by = 2), label = seq(-6,14, by = 2), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .7 
segments(left.side,14,left.side,7) 
segments(left.side,14,left.side+.1,14) 
segments(left.side,7,left.side+.1,7)
text(.5, 10.5, "Institutionen", srt = 90, cex=1)
segments(left.side,6,left.side,1) 
segments(left.side,6,left.side+.1,6) 
segments(left.side,1,left.side+.1,1)
text(.5, 3.5, "Ressourcen", srt = 90, cex=1)



#Consequences - ohne Italien 

qog <- read.csv("qog_std_cs_jan17.csv",sep=",",header=TRUE) 
qog <- subset(qog, select=c(cname,icrg_qog))
icrg <- c(.889,.889,.972,.889,.972,.757,.861,.889,.944,.806,.569,.861,.917,.944,.944,.972,.917,.722,.972,.889,.750,.817)
icrg2 <- c(.889,.889,.972,.889,.972,.757,.861,.889,.944,.806,.861,.917,.944,.944,.972,.917,.722,.972,.889,.750,.817)
krzel2 <- c("AU","BE","DK","DE","FI","FR","UK","IE","IS","IL","JP","CA","NZ","NE","NO","AT","PT","SE","CH","ES","US")
x1b <- c(-0.095519315,1.206157338,0.492008339,0.259076073,-0.619681312,-0.904316229,-0.147825239,0.237449039,0.563038363,1.253617105,
        -0.892850856, -0.007220409,  0.292682738 , 0.408304946,  0.419707435,  0.306568846,  0.104203771,  0.548378317,
        0.077537068, 0.333276441, -1.001426162)
x2b <- c(0.35278871 , 0.07643348  ,0.12870211 , 0.82633331, -0.57630454 , 0.95752414,  0.76993697,  0.20884520, -1.21965438, -0.75972865,
         -0.65432795,  1.04079347, -0.23561040,  0.69372983, -0.01595273,  0.38840804, -0.70170013, -0.74357567, -1.03114488,
         -1.05869737,  1.52953301)

par(mfrow=c(1,2))
plot(x1b, icrg2, xlab="Institutionen", ylab="Regierungsqualität", type="n",xlim=c(-1,1.5), ylim=c(.75,1))
text(x1b, icrg2, krzel2, cex=1)
abline(rega <- lm(icrg2~x1b))
plot(x2b, icrg2, xlab="Ressourcen", ylab="Regierungsqualität", type="n",xlim=c(-1.5,2), ylim=c(.75,1))
text(x2b, icrg2, krzel2, cex=1)
abline(regb <- lm(icrg2~x2b))

cor(x1b,icrg2)
cor(x2b,icrg2)

summary(regc <- lm(icrg2~x1b + x2b))
summary(rega)
