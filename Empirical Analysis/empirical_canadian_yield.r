## Description: Simulation codes for eigenvalue ratio estimators and variance ratio tests.
## The code can produce the results given in Tables 1 and 2 of the paper.

## Loading packages

library(fda);library(tseries);library(sandwich);library(sde);
library(variables);library(basefun);library(polynom);
library(fracdiff);library(arfima)
setwd("C:/Users/wseo2199/Dropbox/Shang_Seo/Integer_integration/code")
load("CV1.Rdata")
inner = dget("inprod.R")
lrvar = dget("lr_var_v2_for_fractional.R")

setwd("C:/Users/wseo2199/Dropbox/Shang_Seo/Integer_integration")
data_raw   = read.csv("Canadian_daily_yields2.csv")
data_raw=as.matrix(data_raw[,2:ncol(data_raw)])



uband=1/5 #1/3 and 1/4


ql=0.04467116
qu=2.12588475

T_sample=nrow(data_raw)
TTT=T_sample

x_mat=t(data_raw)
nt=nrow(x_mat)

# figure

grid_point = seq(0.25, 30, by = 0.25)
savefig("yield", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(grid_point, x_mat), xlab = "Maturity", ylab = "Zero-coupon bond")
dev.off()

lbnumber2=26;  t = (0:(nt-1))/(nt-1)
LBF = matrix(NA,nrow = nt , ncol = lbnumber2)
for (i in 1:(lbnumber2/2)){
  LBF[,2*i-1] = sqrt(2)*sin(2*pi*i*t) /sqrt(inner(sqrt(2)*sin(2*pi*i*t),sqrt(2)*sin(2*pi*i*t),t))
  LBF[,2*i] = sqrt(2)*cos(2*pi*i*t)/sqrt(inner(sqrt(2)*cos(2*pi*i*t),sqrt(2)*cos(2*pi*i*t),t))
}



### V_0 test##
hh=t(LBF[1:nt,])%*%x_mat[1:nt,]*(t[2]-t[1])
xcoef=hh; xcoef=xcoef[,2:ncol(xcoef)]-xcoef[,1]
xcoef=t(xcoef)

T_sample=nrow(xcoef); TTT=T_sample

bandw=floor(TTT^(uband))
zz0=t(xcoef[1:TTT,])
zz0=zz0 #-rowMeans(zz0)
zz0=t(zz0)
zz=zz0

zz2=apply(zz0,2, cumsum)
v0=lrvar(zz,kernel=2)$omega / TTT
vx1 = crossprod(zz2) / (TTT^2)
vx0 = lrvar(zz,kernel=2)$omega / TTT

eelements=eigen(vx1)
ev0=eelements$vectors[,1]

eval_vx0= t(ev0)%*%vx0%*%ev0
eval_vx1= t(ev0)%*%vx1%*%ev0
teststat=(eval_vx1/eval_vx0)

teststat
if (teststat > qu) {print("Reject at upper tail")}
if (teststat < ql) {print("Reject at lower tail")}
if (teststat > ql & teststat <qu) {print("Accept")}



### V_1 test##
hh=t(LBF[1:nt,])%*%x_mat[1:nt,]*(t[2]-t[1])
xcoef=hh;
xcoef=t(xcoef)
T_sample=nrow(xcoef); TTT=T_sample
bandw=floor(TTT^(uband))

zz0=t(xcoef[1:TTT,])
zz0=zz0 #-rowMeans(zz0)
zz0=t(zz0)
zz=zz0

zz=zz[2:TTT,]-zz[1:(TTT-1),];zz0=zz0[2:TTT,]-zz0[1:(TTT-1),]
zz2=apply(zz0,2, cumsum)
v0=lrvar(zz,kernel=2)$omega / TTT
vx1 = crossprod(zz2) / (TTT^2)
vx0 = lrvar(zz,kernel=2)$omega / TTT

eval_vx0= t(ev0)%*%vx0%*%ev0
eval_vx1= t(ev0)%*%vx1%*%ev0

teststat2=(eval_vx1/eval_vx0)
teststat2
if (teststat2 > qu) {print("Reject at upper tail")}
if (teststat2 < ql) {print("Reject at lower tail")}
if (teststat2 > ql & teststat2 <qu) {print("Accept")}




### V_2 test##
hh=t(LBF[1:nt,])%*%x_mat[1:nt,]*(t[2]-t[1])
xcoef=hh;
xcoef=t(xcoef)
T_sample=nrow(xcoef); TTT=T_sample
bandw=floor(TTT^(uband))

zz0=t(xcoef[1:TTT,])
zz0=zz0 #-rowMeans(zz0)
zz0=t(zz0)
zz=zz0

## second difference ##
zz=zz[2:TTT,]-zz[1:(TTT-1),];zz0=zz0[2:TTT,]-zz0[1:(TTT-1),]
TTT=TTT-1
zz=zz[2:TTT,]-zz[1:(TTT-1),];zz0=zz0[2:TTT,]-zz0[1:(TTT-1),]
######################################################

zz2=apply(zz0,2, cumsum)
v0=lrvar(zz,kernel=2)$omega / TTT
vx1 = crossprod(zz2) / (TTT^2)
vx0 = lrvar(zz,kernel=2)$omega / TTT

eval_vx0= t(ev0)%*%vx0%*%ev0
eval_vx1= t(ev0)%*%vx1%*%ev0

teststat3=(eval_vx1/eval_vx0)
teststat3
if (teststat3 > qu) {print("Reject at upper tail")}
if (teststat3 < ql) {print("Reject at lower tail")}
if (teststat3 > ql & teststat3 <qu) {print("Accept")}

seqbase=seq(0,1,0.001)
quantile_grid=as.vector(quantile(CV1,seqbase))

QQ=abs(quantile_grid-rep(teststat,length(quantile_grid)))
seqbase[which(QQ==min(QQ))]

QQ=abs(quantile_grid-rep(teststat2,length(quantile_grid)))
seqbase[which(QQ==min(QQ))]

QQ=abs(quantile_grid-rep(teststat3,length(quantile_grid)))
seqbase[which(QQ==min(QQ))]




