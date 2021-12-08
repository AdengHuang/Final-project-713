library(survival)
library(survminer)
library(MASS)
data <- read.table("D:\\grad\\1\\713 survival\\pbc.txt")
attach(data)
colnames(data)<- c("id","time","status","trt","age","sex","ascites","hepato","spiders","edema","bili","chol","albumin","copper","alk.phos","ast","trig","platelet","protime","stage")
data$trt = ifelse(trt==1,"DPCA","Placebo")
# Balanced or not?
# categorical
p <- aggregate(sex~trt,data = data, FUN = table)
p
chisq.test(p$sex,correct = F)

p2 <- aggregate(ascites~trt,data = data, FUN = table)
p2
chisq.test(p2$ascites,correct = F)

p3 <- aggregate(hepato~trt,data = data, FUN = table)
p3
chisq.test(p3$hepato,correct = F)

p4 <- aggregate(spiders~trt,data = data, FUN = table)
p4
chisq.test(p4$spiders,correct = F)

p5 <- aggregate(edema~trt,data = data, FUN = table)
p5
chisq.test(p5$edema,correct = F)

p6 <- aggregate(stage~trt,data = data, FUN = table)
p6
chisq.test(p6$stage,correct = F)

# skewness check
boxplot(bili~trt)
boxplot(chol~trt)
boxplot(albumin~trt)
boxplot(copper~trt)
boxplot(alk.phos~trt)
boxplot(ast~trt)
boxplot(trig~trt)
boxplot(platelet~trt)
boxplot(protime~trt)

#mann whitney
aggregate(bili~trt, data = data,FUN = median)
wilcox.test(bili~trt,correct =F)

aggregate(chol~trt, data = data,FUN = median)
wilcox.test(chol~trt,correct =F)

aggregate(copper~trt, data = data,FUN = median)
wilcox.test(copper~trt,correct =F)

aggregate(alk.phos~trt, data = data,FUN = median)
wilcox.test(alk.phos~trt,correct =F)

aggregate(ast~trt, data = data,FUN = median)
wilcox.test(ast~trt,correct =F)

aggregate(trig~trt, data = data,FUN = median)
wilcox.test(trig~trt,correct =F)

aggregate(protime~trt, data = data,FUN = median)
wilcox.test(protime~trt,correct =F)

aggregate(albumin~trt,data = data ,FUN = mean)
t.test(albumin~trt)

aggregate(platelet~trt,data = data ,FUN = mean)
t.test(platelet~trt)

# univariate
data$status = ifelse(data$status==2,1,0)
survdata <- Surv(data$time,data$status)

summary(coxph(survdata~as.factor(trt)))
summary(coxph(survdata~as.factor(sex)))
summary(coxph(survdata~as.factor(ascites)))
summary(coxph(survdata~as.factor(hepato)))
summary(coxph(survdata~as.factor(spiders)))
summary(coxph(survdata~as.factor(edema)))
summary(coxph(survdata~as.factor(stage)))
summary(coxph(survdata~bili))
summary(coxph(survdata~chol))
summary(coxph(survdata~albumin))
summary(coxph(survdata~copper))
summary(coxph(survdata~alk.phos))
summary(coxph(survdata~ast))
summary(coxph(survdata~trig))
summary(coxph(survdata~platelet))
summary(coxph(survdata~protime))

ggsurvplot(survfit(survdata~sex),data = data)
ggsurvplot(survfit(survdata~stage),data = data)
for(i in 1:20)
{
  print(sum(is.na(data[[i]])))
}
zz<-na.omit(data)
suur <- Surv(zz$time,zz$status)
s <- list(lower = ~trt, upper = ~ trt+age+sex+ascites+hepato+spiders+factor(edema)+log(bili)+log(chol)+log(albumin)+log(copper)+log(alk.phos)+log(ast)+log(trig)+log(platelet)+log(protime)+factor(stage))
coxmd <- coxph(suur~trt,data = zz)
p<- stepAIC(coxmd, scope = s ,direction = "forward")

st1<-cox.zph(p,transform = "identity")
st2<-cox.zph(p,transform = "log")
st3<-cox.zph(p,transform = "rank")
st4<-cox.zph(p,transform = "km")
st1
st2
st3
st4
plot(p$residuals)
