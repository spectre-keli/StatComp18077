## ------------------------------------------------------------------------
x<-rnorm(50)
x

## ------------------------------------------------------------------------
name<-c("A","B","C")
age<-c(20,20,21)
w<-c(120,130,125)
s.info<-data.frame(name=name,age=age,weight=w)
knitr::kable(s.info, format = "markdown") 

## ------------------------------------------------------------------------
plot(x,type="l")

## ------------------------------------------------------------------------
x <- as.integer(seq(0,4)) 
p <- c(0.1,0.2,0.2,0.2,0.3) 
cp <- cumsum(p)
m <- 1e3 
emp <- numeric(m) 
emp <- x[findInterval(runif(m),cp)+1] 
emp.p <- as.vector(table(emp)) 
the <- sample(x,1000,replace = T,prob = p)
the.p <- as.vector(table(the)) 
rft<-data.frame(x=x,emp.p=emp.p/sum(emp.p)/p,the.p=the.p/sum(the.p)/p)
knitr::kable(rft, format = "markdown")

## ------------------------------------------------------------------------
library(ggplot2)
beta<-function(x){
 -(x^(a-1)*(1-x)^(b-1)*gamma(a+b)/gamma(a)/gamma(b))
}

my.rbeta<-function(n,alpha,beta,fn){
if(alpha<=1||beta<=1){
print("Unreasonable parameters")
break 
}else{
assign("a",alpha,pos=1)
assign("b",beta,pos=1)
myfit<-optim(0.5,fn,method="L-BFGS-B",lower=0,upper=1)
c<-ceiling(-myfit$value)
j<-k<-0
y<-numeric(n) 
while(k < n){ 
u <- runif(1) 
j <- j + 1 
x <- runif(1)   
f.cg<-gamma(alpha+beta)/gamma(alpha)/gamma(beta)*x^(alpha-1)*(1-x)^(beta-1)/c    
if(f.cg>u){  
k<-k + 1 
y[k]<-x 
}
}
}
y
}
x<-my.rbeta(3000,3,2,beta)
y<-rbeta(3000,3,2)
value<-c(x,y)                
type<-rep(c("sample","theoretical"),each=3000)
plotset<-data.frame(x=value,type=type)
figure<-ggplot(plotset,aes(x=x,fill=type,group=type))+
geom_histogram(aes(y=..density..),stat="bin",binwidth=bw.SJ(y,method="ste"),position=position_dodge(0.04))+
labs(x="x",y="density",title="Beta(3,2)")+theme(plot.title = element_text(size=14,hjust = 0.5))
figure

## ------------------------------------------------------------------------
n<-1e4
r<-4
beta<-2 
lambda<-rgamma(n,r,beta) 
x<-rexp(n,lambda) 
x[1:100]                   ##Only part of the data is shown

## ------------------------------------------------------------------------
MC.pbeta<-function(x,a,b){
m <- 1e4 
u <- runif(m, min=0, max=x)
g<-function(u) {gamma(a+b)/gamma(a)/gamma(b)*u^(a-1)*(1-u)^(b-1)}
theta.hat <- mean(g(u)) * x
theta.hat
}
p1<-p2<-numeric(9)
for(i in 1:9){
p1[i]<-MC.pbeta(0.1*i,3,3)
p2[i]<-pbeta(0.1*i,3,3)
}
res<-rbind(MC.p=round(p1,3), pbeta.p=round(p2,3))
colnames(res) <- paste0('x=',seq(0.1,0.9,by=0.1))
knitr::kable(res, format = "markdown",align="c")

## ------------------------------------------------------------------------
MC.Ray<-function(num,sigma,antithetic = TRUE){
u<-runif(num)
if(!antithetic) v<-runif(num) else v<-1-u
u<-c(u,v)
x<-numeric(num*2)
for(i in 1:num){
x[i]<-sqrt(-2*sigma^2*log(1-u[i]))
x[i+num]<-sqrt(-2*sigma^2*log(1-u[i+num]))
}
x
}
m<-1000
sigma<-1                                      #sigma???څ<U+03AA>1
MC1<-MC.Ray(m,sigma,antithetic=FALSE)
MC2<-MC.Ray(m,sigma)
percent<-(var((MC1[(1+m):(m+m)]+MC1[1:m])/2)-var((MC2[(1+m):(m+m)]+MC2[1:m])/2))/
var((MC1[(1+m):(m+m)]+MC1[1:m])/2)
print(sprintf("the percent reduction in variance is %f",percent))

## ------------------------------------------------------------------------
m<-1e4
theta.hat<-se<-numeric(2)
g<-function(x){
x^2*exp(-x^2/2)/sqrt(2*pi)*(x > 1)
}
x<-rnorm(m)   #using f1
fg<-g(x)/(exp(-x^2/2)/sqrt(2*pi))
theta.hat[1]<-mean(fg)
se[1]<-sd(fg)
x<-rexp(m,rate=0.5) #using f2
fg<-g(x)/(exp(-x/2)/2)
theta.hat[2]<-mean(fg)
se[2]<-sd(fg)
res<-rbind(theta=round(theta.hat,3), se=round(se,3))
colnames(res)<-paste0('f',1:2)

## ------------------------------------------------------------------------
knitr::kable(res, format = "markdown",align="c")

## ----echo=FALSE,fig.width=10---------------------------------------------
g<-function(x) (x^2)*(exp((-x^2)/2))/sqrt(2*pi)
f1<-function(x) 1/sqrt(2*pi)*exp(-x^2/2)
f2<-function(x) 1/2*exp(-x/2)
x<-seq(1,5,0.01)
gs1<-c(expression(g^2/f1),expression(g^2/f2))
plot(x,g(x)^2/f1(x),type="l",ylab="",lwd = 0.25,col=1)
lines(x,g(x)^2/f2(x), lty = 2, lwd = 0.25,col=2)
legend("topright", legend = gs1,lty = 1:2, lwd = 0.25, inset=0.1,col=1:2) 

## ------------------------------------------------------------------------
m<-1e4
g<-function(x){
x^2*exp(-x^2/2)/sqrt(2*pi)*(x > 1)
}
x<-rexp(m,rate=0.5) #using f2
fg<-g(x)/(exp(-x/2)/2)
theta.hat1<-mean(fg)
print(sprintf("the Monte Carlo estimate of integral value is %f",theta.hat1))

## ------------------------------------------------------------------------
library(ggplot2)
G.estimate<-function(n,dist){             ##estimate the G
if(dist=="sl"){               
diffmethod<-paste("r","lnorm",sep="")
}else if(dist=="unif"){       
diffmethod<-paste("r","unif",sep="")
}else if(dist=="bin"){         
diffmethod<-paste("r","binom",sep="")
}else{
stop("error in your distribution")
}
if(dist!="bin"){  
x<-sort(mapply(diffmethod,n))
}else{
x<-sort(mapply(diffmethod,n,1,0.1))  
}
mu=mean(x)
G.hat<-sum(sapply(seq(1,n),function(i){(2*i-n-1)*x[i]}))/mu/n^2
G.hat
}  
dfplot<-function(x){                      ##construct density histograms                                      
plotset<-data.frame(Gini=x)                           
ggplot(plotset,aes(x=Gini))+
geom_histogram(aes(y=..density..),stat="bin",binwidth=bw.SJ(x,method="ste"),fill="#FF9999")+
labs(y="density")+theme(plot.title=element_text(size=14,hjust = 0.5))
}

G.sl<-sapply(seq(1,1e4),function(ol){G.estimate(1000,"sl")})       ##sl:standard lognormal
res<-data.frame(mean=mean(G.sl),median=median(G.sl),deciles=as.numeric(quantile(G.sl,0.1)))
knitr::kable(res, format = "markdown",align="c")
dfplot(G.sl)

G.unif<-sapply(seq(1,1e4),function(ol){G.estimate(1000,"unif")})   ##unif:uniform distribution 
res<-data.frame(mean=mean(G.unif),median=median(G.unif),deciles=as.numeric(quantile(G.unif,0.1)))
knitr::kable(res, format = "markdown",align="c")
dfplot(G.unif)

G.bin<-sapply(seq(1,1e4),function(ol){G.estimate(1000,"bin")})    ##bin:Bernoulli 
res<-data.frame(mean=mean(G.bin),median=median(G.bin),deciles=as.numeric(quantile(G.bin,0.1)))
knitr::kable(res, format = "markdown",align="c")
dfplot(G.bin)

## ------------------------------------------------------------------------
G.estimate<-function(n,a,b){
x<-rlnorm(n,a,b)
mu=mean(x)
G.hat<-sum(sapply(seq(1,n),function(i){(2*i-n-1)*x[i]}))/mu/n^2
G.hat
}
G.sl<-sapply(seq(1,1e4),function(ol){G.estimate(1e3,1,4)})        ##lognormal(1,4)
CI<-c(mean(G.sl)-qt(0.975,1e4-2)*sd(G.sl),mean(G.sl)+qt(0.975,1e4-2)*sd(G.sl))
mean(G.sl>=CI[1]&G.sl<=CI[2])

## ------------------------------------------------------------------------
library(mvtnorm)
power.test<-function(method,cor){
n1<-500
n2<-1e4
mean<-c(0,0)
sigma<-matrix(c(1,cor,cor,1),nrow=2)
p<-mean(replicate(n2,expr={
x<-rmvnorm(n1,mean,sigma)
cor.test(x[,1],x[,2],method=method)$p.value<=0.05
}))
p
}
test.alter<-function(method){             ##(X,Y)~(X,v*X),  X~N(0,1),P(V=1)=P(V=-1)=0.5
n1<-500
n2<-1e4
p<-mean(replicate(n2,expr={
x<-rnorm(n1)
a<-rbinom(n1,1,0.5)
v<-2*a-1
y<-data.frame(x=x,y=v*x)
cor.test(y[,1],y[,2],method="pearson")$p.value<=0.05
}))
p
}
power1<-data.frame(method=c("pearson","kendall","spearman"),power=c(power.test("pearson",0.1),power.test("kendall",0.1),power.test("spearman",0.1)))
knitr::kable(power1,format="markdown",align="c")     ##bivariate normal
power2<-data.frame(method=c("pearson","kendall","spearman"),power=c(test.alter("pearson"),test.alter("kendall"),test.alter("spearman")))
knitr::kable(power2,format="markdown",align="c")     ##alternative

## ----echo=FALSE----------------------------------------------------------
library(bootstrap)
library(boot)
library(lattice)
library(DAAG)
set.seed(1)

## ------------------------------------------------------------------------
n<-nrow(law)
b.cor<-function(x,i){
cor(x[i,1],x[i,2])
}
theta.hat<-b.cor(law,1:n)
theta.jack<-numeric(n)
for(i in 1:n){
theta.jack[i]<-b.cor(law,(1:n)[-i])
}
bias.jack<-(n-1)*(mean(theta.jack)-theta.hat)
se.jack<-sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias=bias.jack,se=se.jack),3)

## ------------------------------------------------------------------------
m<-1e3
boot.mean<-function(x,i) mean(x[,1][i])
de<-boot(data=aircondit,statistic=boot.mean,R=m)
ci<-boot.ci(de,type=c("norm","basic","perc","bca"))
cat(' norm.ci:',ci$norm[2:3],"\n",
    'basic.ci:',ci$basic[4:5],"\n",
    'perc.ci:',ci$percent[4:5],"\n",
    'BCa.ci:',ci$bca[4:5],"\n","Because they use the quantiles of different distributions.")

## ------------------------------------------------------------------------
n<-nrow(scor)
theta.jack<-numeric(n)
sigma.hat<-cov(scor)
lambda.hat<-eigen(sigma.hat)$values       #the default is descending
theta.hat<-lambda.hat[1]/sum(lambda.hat)
j.theta<-function(x,i){
j.l<-eigen(cov(x[-i,]))$values
j.t<-j.l[1]/sum(j.l)
}
for(i in 1:n){
theta.jack[i]<-j.theta(scor,i)
}
bias.jack<-(n-1)*(mean(theta.jack)-theta.hat)
se.jack<-sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias=bias.jack,se=se.jack),3)

## ------------------------------------------------------------------------
attach(ironslag)        #in DAAG ironslag
n<-length(magnetic)     # for n-fold cross validation
cb<-t(combn(n,2))       # fit models on leave-two-out samples
e1<-e2<-e3<-e4<-matrix(NA,nrow(cb),2) 
for(k in 1:nrow(cb)) {
y<-magnetic[-cb[k,]]
x<-chemical[-cb[k,]]

J1<-lm(y ~ x)
yhat1<-J1$coef[1]+J1$coef[2]*chemical[cb[k,]]
e1[k,]<-magnetic[cb[k,]]-yhat1

J2<-lm(y ~ x + I(x^2))
yhat2<-J2$coef[1]+J2$coef[2]*chemical[cb[k,]] +J2$coef[3]*chemical[cb[k,]]^2
e2[k,]<-magnetic[cb[k,]]-yhat2

J3<-lm(log(y) ~ x)
logyhat3<-J3$coef[1]+J3$coef[2]*chemical[cb[k,]]
yhat3<-exp(logyhat3)
e3[k,]<-magnetic[cb[k,]]-yhat3

J4<-lm(log(y) ~ log(x))
logyhat4<-J4$coef[1]+J4$coef[2]*log(chemical[cb[k,]])
yhat4<-exp(logyhat4)
e4[k,]<-magnetic[cb[k,]]-yhat4
}

c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ----echo=FALSE----------------------------------------------------------
print("Model 2 is best.")

## ----echo=FALSE----------------------------------------------------------
library(survival)
library(mvtnorm)
library(gam)
library(splines)
library(foreach)
library(RANN)
library(energy)
library(boot)
library(Ball)

## ------------------------------------------------------------------------
cvm.test<-function(x,y){
F<-ecdf(x);G<-ecdf(y)
n<-length(x);m<-length(y)
w2<-m*n/(m+n)^2*(sum((F(x)-G(x))^2)+sum((F(y)-G(y))^2))
w2
}
attach(chickwts)                    
x <- sort(as.vector(chickwts[chickwts[,2]=="soybean",1]))
y <- sort(as.vector(chickwts[chickwts[,2]=="linseed",1]))
detach(chickwts)
R<-999;z<-c(x,y);K<-seq(1:26);n<-length(x);set.seed(12345)
reps<-numeric(R);t0<-cvm.test(x,y)
for(i in 1:R){
k<-sample(K,size=n,replace=FALSE)
x1<-z[k];y1<-z[-k]                 
reps[i]<-cvm.test(x1,y1)
}
p<-mean(abs(c(t0,reps))>=abs(t0))
sprintf("example 8.1,8.2 p.value=%f",p)

## ------------------------------------------------------------------------
m<-1e3;k<-3;p<-2;mu<-0.5;R<-999;set.seed(12345)
p.values <- matrix(NA,m,3)

Tn<-function(z,ix,sizes,k){
n1<-sizes[1];n2<-sizes[2];n<-n1+n2
if(is.vector(z)) z<-data.frame(z,0)
z<-z[ix, ]
NN<-nn2(data=z,k=k+1)
block1<-NN$nn.idx[1:n1,-1] 
block2<-NN$nn.idx[(n1+1):n,-1] 
i1<-sum(block1 <= n1)
i2<-sum(block2 >= n1) 
(i1+i2)/(k*n)
}

eqdist.nn<-function(z,sizes,k){
boot.obj<-boot(data=z,statistic=Tn,R=R,sim="permutation",sizes=sizes,k=k)
ts<-c(boot.obj$t0,boot.obj$t)
p.value<-mean(ts>=ts[1])
list(statistic=ts[1],p.value=p.value)
}

## ----echo=FALSE----------------------------------------------------------
n1<-n2<-50;n<-n1+n2;N=c(n1,n2)  
for(i in 1:m){
x<-rnorm(n1,0,1)
y<-rnorm(n2,0,2)
z<-c(x,y)
p.values[i,1]<-eqdist.nn(z,N,k)$p.value
p.values[i,2]<-eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3]<-bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha<-0.1
pow<-colMeans(p.values<alpha)
pow

## ----echo=FALSE----------------------------------------------------------
n1<-n2<-50;n<-n1+n2;N=c(n1,n2)  
for(i in 1:m){
x<-rnorm(n1,0,1)
y<-rnorm(n2,1,2)
z<-c(x,y)
p.values[i,1]<-eqdist.nn(z,N,k)$p.value
p.values[i,2]<-eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3]<-bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha<-0.1
pow<-colMeans(p.values<alpha)
pow

## ----echo=FALSE----------------------------------------------------------
n1<-n2<-50;n<-n1+n2;N=c(n1,n2)  
for(i in 1:m){
x<-rt(n1,1)
y<-c(rnorm(n2/2,5,1),rnorm(n2/2))
z<-c(x,y)
p.values[i,1]<-eqdist.nn(z,N,k)$p.value
p.values[i,2]<-eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3]<-bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha<-0.1
pow<-colMeans(p.values<alpha)
pow

## ----echo=FALSE----------------------------------------------------------
n1<-10;n2<-100;n<-n1+n2;N=c(n1,n2) 
for(i in 1:m){
x<-rnorm(10)
y<-rnorm(100)
z<-c(x,y)
p.values[i,1]<-eqdist.nn(z,N,k)$p.value
p.values[i,2]<-eqdist.etest(z,sizes=N,R=R)$p.value
p.values[i,3]<-bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha<-0.1
pow<-colMeans(p.values<alpha) 
pow

## ------------------------------------------------------------------------
set.seed(12345)
m<-10000
sigma<-2
x<-numeric(m)
x[1]<-rnorm(1)
k<-0
u<-runif(m)
for(i in 2:m){
xt<-x[i-1]
y<-rnorm(1,xt,sigma)
num<-dcauchy(y)*dnorm(xt,y,sigma)
den<-dcauchy(xt)*dnorm(y,xt,sigma)
if(u[i]<=num/den){
x[i]<-y
}else{
x[i]<-xt
k<-k+1     
}
}
b<-1001      #discard the burnin sample
y<-x[b:m]
a<-ppoints(200)
QC<-qcauchy(a)  #quantiles of Cauchy
Q<-quantile(y, a)
qc<-qcauchy(0.1) #deciles of Cauchy
q<-quantile(y,0.1)
qqplot(QC,Q,main="QQ-plot",xlab="Cauchy Quantiles",ylab="Sample Quantiles")
abline(0,1,col='steelblue',lwd=2)
points(q,qc,cex=1.5,pch=16,col="tomato")  #The tomato dots represent the deciles 

## ------------------------------------------------------------------------
set.seed(12345)
w<-.5 #width of the uniform support set 
m<-10000 #length of the chain 
burn<-1000 #burn-in time 
x<-numeric(m) #the chain  
ob<-c(125,18,20,34) #the observed sample
prob<-function(y,ob){ # computes the target density 
if(y < 0 || y >= 1){
return(0)
}else{
return((0.5+y/4)^ob[1]*((1-y)/4)^ob[2]*
((1-y)/4)^ob[3]*(y/4)^ob[4])
}
}
u<-runif(m) #for accept/reject step 
v<-runif(m,-w,w) #proposal distribution 
x[1] <-.5 
for(i in 2:m) { 
y<-x[i-1]+v[i] 
if(u[i]<=prob(y,ob)/prob(x[i-1],ob)){
x[i]<-y
}else{ 
x[i]<-x[i-1]} 
}
mean(x[1001:m])

## ------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
psi<-as.matrix(psi)        # psi[i,j] is the statistic psi(X[i,1:j])
n<-ncol(psi)               # for chain in i-th row of X
k<-nrow(psi)
psi.means<-rowMeans(psi)   #row means
B<-n*var(psi.means)        #between variance est.
psi.w<-apply(psi,1,"var")  #within variances
W<-mean(psi.w)             #within est.
v.hat<-W*(n-1)/n+(B/n)     #upper variance est.
r.hat<-v.hat/W             #G-R statistic
return(r.hat)
}       

prob<-function(y,ob){ # computes the target density 
if(y < 0 || y >= 1){
return(0)
}else{
return((0.5+y/4)^ob[1]*((1-y)/4)^ob[2]*
((1-y)/4)^ob[3]*(y/4)^ob[4])
}
}

mult.chain<-function(ob,w,m,x1){
x<-numeric(m)      #the chain
u<-runif(m)        #for accept/reject step 
v<-runif(m,-w,w)   #proposal distribution 
x[1]<-x1 
for(i in 2:m) { 
y<-x[i-1]+v[i] 
if(u[i]<=prob(y,ob)/prob(x[i-1],ob)){
x[i]<-y
}else{ 
x[i]<-x[i-1]} 
}
return(x)
}

w<-.5               #width of the uniform support set 
ob<-c(125,18,20,34) #the observed sample
m<-10000            #length of the chain 
burn<-1000          #burn-in time 
k<-4                #number of chains to generate
x0<-c(0.5,0.9,0.1,0.75)  #choose overdispersed initial values

set.seed(12345)     #generate the chains
X<-matrix(0,nrow=k,ncol=m)
for(i in 1:k){
X[i, ]<-mult.chain(ob,w,m,x0[i])
}
    
psi<-t(apply(X,1,cumsum)) #compute diagnostic statistics
for(i in 1:nrow(psi)){
psi[i,]<-psi[i,]/(1:ncol(psi))
}
    
par(mfrow=c(2,2),mar=c(2,2,2,2))  #plot psi for the four chains
for(i in 1:k)
plot(psi[i,(burn+1):m],type="l",xlab=i,ylab=bquote(psi))
    
par(mfrow=c(1,1)) #restore default
rhat<-rep(0,m)
for(j in (burn+1):m)
rhat[j]<-Gelman.Rubin(psi[,1:j])
plot(rhat[(burn+1):m],type="l",xlab="length",ylab="R")
abline(h=1.2,lty=2)

## ------------------------------------------------------------------------
s.k.1<-function(a){
f1<-1-pt(sqrt(a^2*(k-1)/(k-a^2)),df=k-1)
f2<-1-pt(sqrt(a^2*k/(k+1-a^2)),df=k)
return(f1-f2)
}

s.k.2<-function(a){
f1<-1-pt(sqrt(a^2*(k-1)/(k-a^2)),df=k-1)
f2<-1-pt(sqrt(a^2*k/(k+1-a^2)),df=k)
abs(f1-f2)
}

solution<-function(x){
assign("k",x,pos=1)
if(k<=21){
myfit<-uniroot(s.k.1,c(1e-4,sqrt(x)-1e-4))$root
}else{
myfit<-optimize(s.k.2,c(0,3))$minimum        ##while k>=22,upper=3
}
return(myfit)
}

A.k<-sapply(c(4:25,100,500,1000),solution)
result<-data.frame(k=c(4:25,100,500,1000),ponits=A.k)
t(result)

## ------------------------------------------------------------------------
f<-function(x,theta,eta){
1/(theta*pi*(1+((x-eta)/theta)^2))
}
mycauchy<-function(x,theta,eta){
res<-integrate(f,lower=-Inf,upper=x,rel.tol=.Machine$double.eps^0.25,
theta=theta,eta=eta)
res$value
}
upper<-seq(-10,10,2)
result<-rbind(x=as.integer(upper),mycauchy=mapply(mycauchy,upper,1,0),pcauchy=pcauchy(upper)) #default theta=1, eta=0
options(warn=-1)
knitr::kable(result, format = "markdown")

## ------------------------------------------------------------------------
n.A<-28;n.B<-24;n.OO<-41;n.AB<-70 
N<-10000 #max. number of iterations 
tol<-.Machine$double.eps 
L.old<-c(.2,.35)  #initial est. for p and q 
M.value<-0
L.list<-data.frame(p=0,q=0)

mlogL<-function(l,l.old){
r<-1-sum(l)
r.old<-1-sum(l.old)
n.AA<-n.A*l.old[1]^2/(l.old[1]^2+2*l.old[1]*r.old)
n.BB<-n.B*l.old[2]^2/(l.old[2]^2+2*l.old[2]*r.old)
llh<-2*n.OO*log(r)+n.AB*log(2*l[1]*l[2])+
2*n.AA*log(l[1])+2*n.BB*log(l[2])+
(n.A-n.AA)*log(2*l[1]*r)+(n.B-n.BB)*log(2*l[2]*r)
-llh
}

for(j in 1:N){
res<-optim(c(0.3,0.2),mlogL,l.old=L.old)
L<-res$par
L.list[j,]<-L
M.value[j]<- -res$value
if(sum(abs(L-L.old)/L.old)<tol) break 
L.old<-L
}
L.list  #p,q
M.value  ##the max likelihood values,no increase !

## ------------------------------------------------------------------------
attach(mtcars)
formulas<-list(mpg ~ disp, mpg ~ I(1 / disp), mpg ~ disp + wt, mpg ~ I(1 / disp) + wt)
myresult.1<-vector("list", length(formulas))
for(i in 1:length(formulas)) myresult.1[[i]]<-lm(formulas[[i]],data=mtcars)
myresult.2<-lapply(formulas,lm)
myresult.1           #the result of for loops
myresult.2           #the result of lapply

## ------------------------------------------------------------------------
set.seed(12345)
bootstraps<-lapply(1:10,function(i){
rows<-sample(1:nrow(mtcars),rep = TRUE)
mtcars[rows, ]
})
myresult.1<-vector("list",10)
for(i in 1:10){
myresult.1[[i]]<-lm(mpg ~ disp,data=bootstraps[[i]])
}
myresult.2<-lapply(bootstraps,lm,formula=mpg~disp)

## ------------------------------------------------------------------------
rsq<-function(mod) summary.lm(mod)$r.squared   
lapply(lapply(formulas,lm),rsq)                      #exercise 3
lapply(lapply(bootstraps,lm,formula=mpg~disp),rsq)   #exercise 4

## ------------------------------------------------------------------------
set.seed(12345)
trials<-replicate(100,t.test(rpois(10,10),rpois(7, 10)),simplify = FALSE)
my.pvalue<-function(x){         #using anonymous function
x$p.value
}
sapply(trials, my.pvalue)
sapply(trials,"[[",3)           #using [[ directly

## ------------------------------------------------------------------------
v.lapply<-function (f,n,type, ...){             #an lapply() variant
f<-match.fun(f)
tt=Map(f, ...)
if(type=="numeric")  y=vapply(tt,cbind,numeric(n))
else if(type=="character") y=vapply(tt,cbind,character(n))
else if(type=="complex") y=vapply(tt,cbind,complex(n))
else if(type=="logical") y=vapply(tt,cbind,logical(n))
return(y)
}
my.f<-function(x1,x2){   #example
t.test(x1,x2)$p.value
}
x1<-replicate(100,rpois(10,10),simplify = FALSE)
x2<-replicate(100,rpois(7,10),simplify = FALSE)
v.lapply(my.f,1,"numeric",x1,x2)

## ----echo=FALSE----------------------------------------------------------
library(microbenchmark)

## ------------------------------------------------------------------------
my.chisq.test<-function(x,y){
if(!is.vector(x) && !is.vector(y))
stop("at least one of 'x' and 'y' is not a vector")
if(typeof(x)=="character" || typeof(y)=="character")
stop("at least one of 'x' and 'y' is not a numeric vector")
if(any(x<0) || anyNA(x)) 
stop("all entries of 'x' must be nonnegative and finite")
if(any(y<0) || anyNA(y)) 
stop("all entries of 'y' must be nonnegative and finite")
if((n<-sum(x))==0) 
stop("at least one entry of 'x' must be positive")
if((n<-sum(x))==0) 
stop("at least one entry of 'x' must be positive")
if(length(x)!=length(y)) 
stop("'x' and 'y' must have the same length")
DNAME<-paste(deparse(substitute(x)),"and",deparse(substitute(y)))
METHOD<-"Pearson's Chi-squared test"
x<-rbind(x,y)
nr<-as.integer(nrow(x));nc<-as.integer(ncol(x))
sr<-rowSums(x);sc<-colSums(x);n<-sum(x)
E<-outer(sr,sc,"*")/n
STATISTIC<-sum((x - E)^2/E)
names(STATISTIC)<-"X-squared"
structure(list(statistic=STATISTIC,method=METHOD,data.name=DNAME),class="htest")
}

mya<-c(762,327,468);myb<-c(484,239,477)    #example
my.chisq.test(mya,myb)        
chisq.test(rbind(mya,myb))
microbenchmark(t1=my.chisq.test(mya,myb),t2=chisq.test(rbind(mya,myb)))  

## ------------------------------------------------------------------------
my.table<-function(...,dnn = list.names(...),deparse.level = 1){
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm)) 
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level + 
            1, "", if (is.symbol(x)) as.character(x) else "", 
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm)) 
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    args <- list(...)
    if (!length(args)) 
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args)) 
            dnn <- if (!is.null(argn <- names(args))) 
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens)) 
            lens <- length(a)
        else if (length(a) != lens) 
            stop("all arguments must have the same length")
        fact.a <- is.factor(a)
        if (!fact.a) {
            a0 <- a
            a <- factor(a)
        }
        ll <- levels(a)
        a <- as.integer(a)
        nl <- length(ll)
        dims <- c(dims, nl)
        dn <- c(dn, list(ll))
        bin <- bin + pd * (a - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin)) 
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}

mya<-myb<-c(1,seq(1,4))            #example
my.table(mya,myb)
table(mya,myb)
microbenchmark(t1=my.table(mya,myb),t2=table(mya,myb))    

