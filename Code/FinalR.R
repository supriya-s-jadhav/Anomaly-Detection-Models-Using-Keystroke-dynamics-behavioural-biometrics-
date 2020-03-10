
## NOTE: I decided to include only 2 graphs in each section (histogram, boxplot, bar graph, ridge density plot, scatter plot) to reduce the length of the report. While doing analysis we looked at all the plots and code for all plots is included in rmd file.


####1.Dataset Introduction

#read known
known.data <- read.csv("known.csv", header = TRUE)
#saving original data
#original.known <- known
#known <- known[,-c(1,3,4)]
known<-known.data
dataset<-known.data

c<-sort(table(known$subject))
par(las=2)
barplot1<-barplot(c,main="Frequency Distribution of subjects",xlab = "Subject", 
                  col = rainbow(20),cex.names = 0.7,space=0,ylim = c(0,60),font=2)
text(barplot1,c,as.character(c),pos=3,cex=0.7,font = 2)

attach(known)
par(mfrow=c(2,1))
c07<-table(known$subject[sessionIndex==7])
par(las=2)
barplot2<-barplot(c07,main="Frequency Distribution of subjects for sessionIndex 7",xlab = "Subject", 
                  col = rainbow(20),cex.names = 0.7,space=0,ylim = c(0,30),font=2)
text(barplot2,c07,as.character(c07),pos=3,cex=0.7,font = 2)
c08<-table(known$subject[sessionIndex==8])
par(las=2)
barplot3<-barplot(c08,main="Frequency Distribution of subjects for sessionIndex 8",xlab = "Subject", 
                  col = rainbow(20),cex.names = 0.7,space=0,ylim = c(0,60),font=2)
text(barplot3,c08,as.character(c08),pos=3,cex=0.7,font = 2)

##Looking at independent variables

####X 


####Histogram for H variables from known and unknown data:


library(ggplot2)
known.data.hist = known.data[,-c(1,2,3,4)]
#Histogrsm for H variable
#getting H predictors

H.pred = known.data.hist[,c(1,4,7,10,13,16,19,22,25,28,31)]
#length(H.pred) 
for(i in 1:2){
  print(ggplot(data=H.pred, aes(H.pred[,i])) + geom_histogram(col = "red",fill="green") 
        + labs(title=  paste("Known Data: Histogram for ", names(H.pred)[i],sep = ""   )) + labs(x=names(H.pred)[i], y="Frequency"))
  
}

unknown.data <- read.csv("unknown.csv")
unknown.h.pred<-unknown.data[,c(3,6,9,12,15,18,21,24,27,30,31)]
for(i in 1:2){
  print(ggplot(data=unknown.h.pred, aes(unknown.h.pred[,i])) + geom_histogram(col = "green",fill="red") 
        + labs(title=  paste("UnKnown Data: Histogram for ", names(unknown.h.pred)[i],sep = ""   )) + labs(x=names(H.pred)[i], y="Frequency"))
  
}



#### Histogram for UD variables from known and unknown data:

UD.pred<-known.data[,c(7,10,13,16,19,22,25,28,31,34)]
for(i in 1:2){
  print(ggplot(data=UD.pred, aes(H.pred[,i])) + geom_histogram(col = "red",fill="green") 
        + labs(title=  paste("Known Data: Histogram for ", names(UD.pred)[i],sep = ""   )) + labs(x=names(UD.pred)[i], y="Frequency"))
  
}

unknown.h.pred<-unknown.data[,c(5,8,11,14,17,20,23,26,29,32)]
for(i in 1:2){
  print(ggplot(data=unknown.h.pred, aes(unknown.h.pred[,i])) + geom_histogram(col = "green",fill="red") 
        + labs(title=  paste("UnKnown Data: Histogram for ", names(unknown.h.pred)[i],sep = ""   )) + labs(x=names(H.pred)[i], y="Frequency"))
  
}


####Distribution of DD variables:

DD.pred<-known.data[,c(6,9,12,15,18,21,24,27,30,33)]
for(i in 1:2){
  print(ggplot(data=DD.pred, aes(DD.pred[,i])) + geom_histogram(col = "red",fill="green") 
        + labs(title=  paste("Known Data: Histogram for ", names(DD.pred)[i],sep = ""   )) + labs(x=names(DD.pred)[i], y="Frequency"))
  
}

unknown.DD.pred<-unknown.data[,c(4,7,10,13,16,19,22,25,28,31)]
for(i in 1:2){
  print(ggplot(data=unknown.DD.pred, aes(unknown.DD.pred[,i])) + geom_histogram(col = "green",fill="red") 
        + labs(title=  paste("UnKnown Data: Histogram for ", names(unknown.DD.pred)[i],sep = ""   )) + labs(x=names(unknown.DD.pred)[i], y="Frequency"))
  
}


####rep

#scatter plot of H.t vs H.period with all 51 classes labeled
ggplot(dataset, aes(dataset$H.period, dataset$H.t, colour = subject)) +
  geom_point()+xlab("Hold time for period key")+ylab("Hold time for t key")
ggplot(dataset, aes(dataset$H.five, dataset$H.Shift.r, colour = subject)) +
  geom_point()+xlab("Hold time for five key")+ylab("Hold time for shift+r key")
ggplot(dataset, aes(dataset$H.l, dataset$H.Return, colour = subject)) +
  geom_point()+xlab("Hold time for l key")+ylab("Hold time for Return key") 

#subset with observations of 3 users s002,s003,s004
sMix<-known[(known$subject %in% c("s002", "s003", "s004")), ]
#sMix<-subset(dataset,subject==c("s002","s003","s004"))
sMix<-droplevels(sMix)
#View(sMix)
#summary(sMix$subject)
#looking at the differences in distribution of 3 users
par(mfrow=c(2,2))
ggplot(sMix,aes(x=sMix$H.period,group=subject,fill=subject))+
  geom_histogram(position="identity",bins=20)+geom_density(alpha=0.3)+
  labs(xlab="Hold time of period key", ylab="Frequency count",
       title="Comparing H.period distribution for user s002,s003,s004")

#ggplot(sMix,aes(x=sMix$H.t,fill=subject))+geom_histogram(position="identity",bins=20)+geom_density(alpha=0.3)+labs(xlab="Hold time of t key", ylab="Frequency count", title="Comparing H.t distribution for user s002,s003,s004")

ggplot(sMix,aes(x=sMix$H.Shift.r,fill=subject))+
  geom_histogram(position="identity",bins=20)+geom_density(alpha=0.3)+
  labs(xlab="Hold time of shift+r key", ylab="Frequency count",
       title="Comparing H.Shift.r distribution for user s002,s003,s004")

#ggplot(sMix,aes(x=sMix$H.five,group=subject,fill=subject))+
#  geom_histogram(position="identity",bins=20)+geom_density(alpha=0.3)+
#  labs(xlab="Hold time of five key", ylab="Frequency count",
#       title="Comparing H.five distribution for user s002,s003,s004")

#ggplot(sMix,aes(x=sMix$H.a,fill=subject))+
#  geom_histogram(position="identity",bins=20)+geom_density(alpha=0.3)+
#  labs(xlab="Hold time of a key", ylab="Frequency count",
#       title="Comparing H.a distribution for user s002,s003,s004")

#ggplot(sMix,aes(x=sMix$H.Return,fill=subject))+
#  geom_histogram(position="identity",bins=20)+geom_density(alpha=0.3)+
#  labs(xlab="Hold time of Return key", ylab="Frequency count",
#       title="Comparing H.Return distribution for user s002,s003,s004")

####NOTE : We looked at the distribution plots of all keys in passcode for 51 different users and decided to include only 2 plots in the report. Code for all plots are in rmd file.

#looking at the differences in distribution of all 51 users
#The ridgeline plots below shows the hold time difference of passcode .tieRonal(Return key) of 51 users 
library(ggplot2)
library(ggridges)
theme_set(theme_ridges())

ggplot(dataset, aes(x = H.period, y = subject)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
                       name = "H.period")+labs(title = 'Hold time of 51 users for key H.period') 

#ggplot(dataset, aes(x = H.t, y = subject)) +
#geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
#scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.t")+labs(title = 'Hold time of 51 users for key H.t') 

#ggplot(dataset, aes(x = H.i, y = subject)) +
#geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
#scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),name = "H.i")+labs(title = 'Hold time of 51 users for key H.i') 

#ggplot(dataset, aes(x = H.e, y = subject)) + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) + scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.e")+labs(title = 'Hold time of 51 users for key H.e') 

#ggplot(dataset, aes(x = H.five, y = subject)) + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) + scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.five")+labs(title = 'Hold time of 51 users for key H.five') 

#ggplot(dataset, aes(x = H.Shift.r, y = subject)) + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) + scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.Shift.r")+labs(title = 'Hold time of 51 users for key H.Shift.r') 

#ggplot(dataset, aes(x = H.o, y = subject)) + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) + scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.o")+labs(title = 'Hold time of 51 users for key H.o') 

#ggplot(dataset, aes(x = H.a, y = subject)) + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) + scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.a")+labs(title = 'Hold time of 51 users for key H.a') 

#ggplot(dataset, aes(x = H.l, y = subject)) + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) + scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), name = "H.l")+labs(title = 'Hold time of 51 users for key H.l') 

ggplot(dataset, aes(x = H.Return, y = subject)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
  scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
                       name = "H.Return")+labs(title = 'Hold time of 51 users for key H.Return') 

#ref: http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/


Next we want to see if the sessionIndex makes any difference in the typing speed of 51 different users. We performed t tests of H variables (since they have normal distribution, we assumed equal variance between the two groups, reps in each group are small) to see if the hold time of keys in passcode is different for a user in session 7 and session 8. As explained before, out of 51 users only 22 users have information in both the sessions hence we will look for only these 22 users. Below is the hypothesis testing stated:
  
  Null hypothesis: The true mean of a user in session 7 and session 8 are same.


####Alternative hypethesis: The true mean of a user in session 7 and session 8 are different.

s005<-known.data[subject=="s005",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s007<-known.data[subject=="s007",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s008<-known.data[subject=="s008",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s012<-known.data[subject=="s012",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s020<-known.data[subject=="s020",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s021<-known.data[subject=="s021",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s024<-known.data[subject=="s024",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s026<-known.data[subject=="s026",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s028<-known.data[subject=="s028",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s029<-known.data[subject=="s029",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s034<-known.data[subject=="s034",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s036<-known.data[subject=="s036",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s039<-known.data[subject=="s039",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s042<-known.data[subject=="s042",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s043<-known.data[subject=="s043",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s049<-known.data[subject=="s049",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s050<-known.data[subject=="s050",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s051<-known.data[subject=="s051",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s052<-known.data[subject=="s052",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s053<-known.data[subject=="s053",c(3,5,8,11,14,17,20,23,26,29,32,35)]
s056<-known.data[subject=="s056",c(3,5,8,11,14,17,20,23,26,29,32,35)]

#for user s005
t1<-t.test(H.period~sessionIndex,data=s005)
t2<-t.test(H.t~sessionIndex,data=s005)
t3<-t.test(H.i~sessionIndex,data=s005)
t4<-t.test(H.e~sessionIndex,data=s005)
t5<-t.test(H.five~sessionIndex,data=s005)
t6<-t.test(H.Shift.r~sessionIndex,data=s005)
t7<-t.test(H.o~sessionIndex,data=s005)
t8<-t.test(H.a~sessionIndex,data=s005)
t9<-t.test(H.n~sessionIndex,data=s005)
t10<-t.test(H.l~sessionIndex,data=s005)
t11<-t.test(H.Return~sessionIndex,data=s005)

#for user s007
t12<-t.test(H.period~sessionIndex,data=s007)
t13<-t.test(H.t~sessionIndex,data=s007)
t14<-t.test(H.i~sessionIndex,data=s007)
t15<-t.test(H.e~sessionIndex,data=s007)
t16<-t.test(H.five~sessionIndex,data=s007)
t17<-t.test(H.Shift.r~sessionIndex,data=s007)
t18<-t.test(H.o~sessionIndex,data=s007)
t19<-t.test(H.a~sessionIndex,data=s007)
t20<-t.test(H.n~sessionIndex,data=s007)
t21<-t.test(H.l~sessionIndex,data=s007)
t22<-t.test(H.Return~sessionIndex,data=s007)

#for user s008
t23<-t.test(H.period~sessionIndex,data=s008)
t24<-t.test(H.t~sessionIndex,data=s008)
t25<-t.test(H.i~sessionIndex,data=s008)
t26<-t.test(H.e~sessionIndex,data=s008)
t27<-t.test(H.five~sessionIndex,data=s008)
t28<-t.test(H.Shift.r~sessionIndex,data=s008)
t29<-t.test(H.o~sessionIndex,data=s008)
t30<-t.test(H.a~sessionIndex,data=s008)
t31<-t.test(H.n~sessionIndex,data=s008)
t32<-t.test(H.l~sessionIndex,data=s008)
t33<-t.test(H.Return~sessionIndex,data=s008)

#for user s012
t34<-t.test(H.period~sessionIndex,data=s012)
t35<-t.test(H.t~sessionIndex,data=s012)
t36<-t.test(H.i~sessionIndex,data=s012)
t37<-t.test(H.e~sessionIndex,data=s012)
t38<-t.test(H.five~sessionIndex,data=s012)
t39<-t.test(H.Shift.r~sessionIndex,data=s012)
t40<-t.test(H.o~sessionIndex,data=s012)
t41<-t.test(H.a~sessionIndex,data=s012)
t42<-t.test(H.n~sessionIndex,data=s012)
t43<-t.test(H.l~sessionIndex,data=s012)
t44<-t.test(H.Return~sessionIndex,data=s012)

#for user s020
t45<-t.test(H.period~sessionIndex,data=s020)
t46<-t.test(H.t~sessionIndex,data=s020)
t47<-t.test(H.i~sessionIndex,data=s020)
t48<-t.test(H.e~sessionIndex,data=s020)
t49<-t.test(H.five~sessionIndex,data=s020)
t50<-t.test(H.Shift.r~sessionIndex,data=s020)
t51<-t.test(H.o~sessionIndex,data=s020)
t52<-t.test(H.a~sessionIndex,data=s020)
t53<-t.test(H.n~sessionIndex,data=s020)
t54<-t.test(H.l~sessionIndex,data=s020)
t55<-t.test(H.Return~sessionIndex,data=s020)

#for user s021
t56<-t.test(H.period~sessionIndex,data=s021)
t57<-t.test(H.t~sessionIndex,data=s021)
t58<-t.test(H.i~sessionIndex,data=s021)
t59<-t.test(H.e~sessionIndex,data=s021)
t60<-t.test(H.five~sessionIndex,data=s021)
t61<-t.test(H.Shift.r~sessionIndex,data=s021)
t62<-t.test(H.o~sessionIndex,data=s021)
t63<-t.test(H.a~sessionIndex,data=s021)
t64<-t.test(H.n~sessionIndex,data=s021)
t65<-t.test(H.l~sessionIndex,data=s021)
t66<-t.test(H.Return~sessionIndex,data=s021)

#for user s024
t67<-t.test(H.period~sessionIndex,data=s024)
t68<-t.test(H.t~sessionIndex,data=s024)
t69<-t.test(H.i~sessionIndex,data=s024)
t70<-t.test(H.e~sessionIndex,data=s024)
t71<-t.test(H.five~sessionIndex,data=s024)
t72<-t.test(H.Shift.r~sessionIndex,data=s024)
t73<-t.test(H.o~sessionIndex,data=s024)
t74<-t.test(H.a~sessionIndex,data=s024)
t75<-t.test(H.n~sessionIndex,data=s024)
t76<-t.test(H.l~sessionIndex,data=s024)
t77<-t.test(H.Return~sessionIndex,data=s024)

#for user s026
t78<-t.test(H.period~sessionIndex,data=s026)
t79<-t.test(H.t~sessionIndex,data=s026)
t80<-t.test(H.i~sessionIndex,data=s026)
t81<-t.test(H.e~sessionIndex,data=s026)
t82<-t.test(H.five~sessionIndex,data=s026)
t83<-t.test(H.Shift.r~sessionIndex,data=s026)
t84<-t.test(H.o~sessionIndex,data=s026)
t85<-t.test(H.a~sessionIndex,data=s026)
t86<-t.test(H.n~sessionIndex,data=s026)
t87<-t.test(H.l~sessionIndex,data=s026)
t88<-t.test(H.Return~sessionIndex,data=s026)

#for user s028
t89<-t.test(H.period~sessionIndex,data=s028)
t90<-t.test(H.t~sessionIndex,data=s028)
t91<-t.test(H.i~sessionIndex,data=s028)
t92<-t.test(H.e~sessionIndex,data=s028)
t93<-t.test(H.five~sessionIndex,data=s028)
t94<-t.test(H.Shift.r~sessionIndex,data=s028)
t95<-t.test(H.o~sessionIndex,data=s028)
t96<-t.test(H.a~sessionIndex,data=s028)
t97<-t.test(H.n~sessionIndex,data=s028)
t98<-t.test(H.l~sessionIndex,data=s028)
t99<-t.test(H.Return~sessionIndex,data=s028)

#for user s029
t101<-t.test(H.period~sessionIndex,data=s029)
t102<-t.test(H.t~sessionIndex,data=s029)
t103<-t.test(H.i~sessionIndex,data=s029)
t104<-t.test(H.e~sessionIndex,data=s029)
t105<-t.test(H.five~sessionIndex,data=s029)
t106<-t.test(H.Shift.r~sessionIndex,data=s029)
t107<-t.test(H.o~sessionIndex,data=s029)
t108<-t.test(H.a~sessionIndex,data=s029)
t109<-t.test(H.n~sessionIndex,data=s029)
t110<-t.test(H.l~sessionIndex,data=s029)
t111<-t.test(H.Return~sessionIndex,data=s029)

#for user s034
t112<-t.test(H.period~sessionIndex,data=s034)
t113<-t.test(H.t~sessionIndex,data=s034)
t114<-t.test(H.i~sessionIndex,data=s034)
t115<-t.test(H.e~sessionIndex,data=s034)
t116<-t.test(H.five~sessionIndex,data=s034)
t117<-t.test(H.Shift.r~sessionIndex,data=s034)
t118<-t.test(H.o~sessionIndex,data=s034)
t119<-t.test(H.a~sessionIndex,data=s034)
t120<-t.test(H.n~sessionIndex,data=s034)
t121<-t.test(H.l~sessionIndex,data=s034)
t122<-t.test(H.Return~sessionIndex,data=s034)

#for user s036
t123<-t.test(H.period~sessionIndex,data=s036)
t124<-t.test(H.t~sessionIndex,data=s036)
t125<-t.test(H.i~sessionIndex,data=s036)
t126<-t.test(H.e~sessionIndex,data=s036)
t127<-t.test(H.five~sessionIndex,data=s036)
t128<-t.test(H.Shift.r~sessionIndex,data=s036)
t129<-t.test(H.o~sessionIndex,data=s036)
t130<-t.test(H.a~sessionIndex,data=s036)
t131<-t.test(H.n~sessionIndex,data=s036)
t132<-t.test(H.l~sessionIndex,data=s036)
t133<-t.test(H.Return~sessionIndex,data=s036)

#for user s039
t134<-t.test(H.period~sessionIndex,data=s039)
t135<-t.test(H.t~sessionIndex,data=s039)
t136<-t.test(H.i~sessionIndex,data=s039)
t137<-t.test(H.e~sessionIndex,data=s039)
t138<-t.test(H.five~sessionIndex,data=s039)
t139<-t.test(H.Shift.r~sessionIndex,data=s039)
t140<-t.test(H.o~sessionIndex,data=s039)
t141<-t.test(H.a~sessionIndex,data=s039)
t142<-t.test(H.n~sessionIndex,data=s039)
t143<-t.test(H.l~sessionIndex,data=s039)
t144<-t.test(H.Return~sessionIndex,data=s039)

#for user s042
t145<-t.test(H.period~sessionIndex,data=s042)
t146<-t.test(H.t~sessionIndex,data=s042)
t147<-t.test(H.i~sessionIndex,data=s042)
t148<-t.test(H.e~sessionIndex,data=s042)
t149<-t.test(H.five~sessionIndex,data=s042)
t150<-t.test(H.Shift.r~sessionIndex,data=s042)
t151<-t.test(H.o~sessionIndex,data=s042)
t152<-t.test(H.a~sessionIndex,data=s042)
t153<-t.test(H.n~sessionIndex,data=s042)
t154<-t.test(H.l~sessionIndex,data=s042)
t155<-t.test(H.Return~sessionIndex,data=s042)

#for user s043
t156<-t.test(H.period~sessionIndex,data=s043)
t157<-t.test(H.t~sessionIndex,data=s043)
t158<-t.test(H.i~sessionIndex,data=s043)
t159<-t.test(H.e~sessionIndex,data=s043)
t160<-t.test(H.five~sessionIndex,data=s043)
t161<-t.test(H.Shift.r~sessionIndex,data=s043)
t162<-t.test(H.o~sessionIndex,data=s043)
t163<-t.test(H.a~sessionIndex,data=s043)
t164<-t.test(H.n~sessionIndex,data=s043)
t165<-t.test(H.l~sessionIndex,data=s043)
t166<-t.test(H.Return~sessionIndex,data=s043)

#for user s049
t167<-t.test(H.period~sessionIndex,data=s049)
t168<-t.test(H.t~sessionIndex,data=s049)
t169<-t.test(H.i~sessionIndex,data=s049)
t170<-t.test(H.e~sessionIndex,data=s049)
t171<-t.test(H.five~sessionIndex,data=s049)
t172<-t.test(H.Shift.r~sessionIndex,data=s049)
t173<-t.test(H.o~sessionIndex,data=s049)
t174<-t.test(H.a~sessionIndex,data=s049)
t175<-t.test(H.n~sessionIndex,data=s049)
t176<-t.test(H.l~sessionIndex,data=s049)
t177<-t.test(H.Return~sessionIndex,data=s049)

#for user s050
t178<-t.test(H.period~sessionIndex,data=s050)
t179<-t.test(H.t~sessionIndex,data=s050)
t180<-t.test(H.i~sessionIndex,data=s050)
t181<-t.test(H.e~sessionIndex,data=s050)
t182<-t.test(H.five~sessionIndex,data=s050)
t183<-t.test(H.Shift.r~sessionIndex,data=s050)
t184<-t.test(H.o~sessionIndex,data=s050)
t185<-t.test(H.a~sessionIndex,data=s050)
t186<-t.test(H.n~sessionIndex,data=s050)
t187<-t.test(H.l~sessionIndex,data=s050)
t188<-t.test(H.Return~sessionIndex,data=s050)

#for user s051
t189<-t.test(H.period~sessionIndex,data=s051)
t190<-t.test(H.t~sessionIndex,data=s051)
t191<-t.test(H.i~sessionIndex,data=s051)
t192<-t.test(H.e~sessionIndex,data=s051)
t193<-t.test(H.five~sessionIndex,data=s051)
t194<-t.test(H.Shift.r~sessionIndex,data=s051)
t195<-t.test(H.o~sessionIndex,data=s051)
t196<-t.test(H.a~sessionIndex,data=s051)
t197<-t.test(H.n~sessionIndex,data=s051)
t198<-t.test(H.l~sessionIndex,data=s051)
t199<-t.test(H.Return~sessionIndex,data=s051)

#for user s052
t200<-t.test(H.period~sessionIndex,data=s052)
t201<-t.test(H.t~sessionIndex,data=s052)
t202<-t.test(H.i~sessionIndex,data=s052)
t203<-t.test(H.e~sessionIndex,data=s052)
t204<-t.test(H.five~sessionIndex,data=s052)
t205<-t.test(H.Shift.r~sessionIndex,data=s052)
t206<-t.test(H.o~sessionIndex,data=s052)
t207<-t.test(H.a~sessionIndex,data=s052)
t208<-t.test(H.n~sessionIndex,data=s052)
t209<-t.test(H.l~sessionIndex,data=s052)
t210<-t.test(H.Return~sessionIndex,data=s052)

#for user s053
t211<-t.test(H.period~sessionIndex,data=s053)
t212<-t.test(H.t~sessionIndex,data=s053)
t213<-t.test(H.i~sessionIndex,data=s053)
t214<-t.test(H.e~sessionIndex,data=s053)
t215<-t.test(H.five~sessionIndex,data=s053)
t216<-t.test(H.Shift.r~sessionIndex,data=s053)
t217<-t.test(H.o~sessionIndex,data=s053)
t218<-t.test(H.a~sessionIndex,data=s053)
t219<-t.test(H.n~sessionIndex,data=s053)
t220<-t.test(H.l~sessionIndex,data=s053)
t221<-t.test(H.Return~sessionIndex,data=s053)

#for user s056
t222<-t.test(H.period~sessionIndex,data=s056)
t223<-t.test(H.t~sessionIndex,data=s056)
t224<-t.test(H.i~sessionIndex,data=s056)
t225<-t.test(H.e~sessionIndex,data=s056)
t226<-t.test(H.five~sessionIndex,data=s056)
t227<-t.test(H.Shift.r~sessionIndex,data=s056)
t228<-t.test(H.o~sessionIndex,data=s056)
t229<-t.test(H.a~sessionIndex,data=s056)
t230<-t.test(H.n~sessionIndex,data=s056)
t231<-t.test(H.l~sessionIndex,data=s056)
t232<-t.test(H.Return~sessionIndex,data=s056)

independent.test7n8<-cbind(user005=c(t1$p.value,t2$p.value,t3$p.value,t4$p.value,t5$p.value,t6$p.value,t7$p.value,t8$p.value,t9$p.value,t10$p.value,t11$p.value),
                           user7=c(t12$p.value,t13$p.value,t14$p.value,t15$p.value,t16$p.value,t17$p.value,t18$p.value,t19$p.value,t20$p.value,t21$p.value,t22$p.value),
                           user8=c(t23$p.value,t24$p.value,t25$p.value,t26$p.value,t27$p.value,t28$p.value,t29$p.value,t30$p.value,t31$p.value,t32$p.value,t33$p.value),
                           user12=c(t34$p.value,t35$p.value,t36$p.value,t37$p.value,t38$p.value,t39$p.value,t40$p.value,t41$p.value,t42$p.value,t43$p.value,t44$p.value),
                           user20=c(t45$p.value,t46$p.value,t47$p.value,t48$p.value,t49$p.value,t50$p.value,t51$p.value,t52$p.value,t53$p.value,t54$p.value,t55$p.value),
                           user21=c(t56$p.value,t57$p.value,t58$p.value,t59$p.value,t60$p.value,t61$p.value,t62$p.value,t63$p.value,t64$p.value,t65$p.value,t66$p.value),
                           user24=c(t67$p.value,t68$p.value,t69$p.value,t70$p.value,t71$p.value,t72$p.value,t73$p.value,t74$p.value,t75$p.value,t76$p.value,t77$p.value),
                           user26=c(t78$p.value,t79$p.value,t80$p.value,t81$p.value,t82$p.value,t83$p.value,t84$p.value,t85$p.value,t86$p.value,t87$p.value,t88$p.value),
                           user28=c(t89$p.value,t90$p.value,t91$p.value,t92$p.value,t93$p.value,t94$p.value,t95$p.value,t96$p.value,t97$p.value,t98$p.value,t99$p.value),
                           
                           user29=c(t101$p.value,t102$p.value,t103$p.value,t104$p.value,t105$p.value,t106$p.value,t107$p.value,t108$p.value,t109$p.value,t110$p.value,t111$p.value),
                           user34=c(t112$p.value,t113$p.value,t114$p.value,t115$p.value,t116$p.value,t117$p.value,t118$p.value,t119$p.value,t120$p.value,t121$p.value,t122$p.value),
                           user36=c(t123$p.value,t124$p.value,t125$p.value,t126$p.value,t127$p.value,t128$p.value,t129$p.value,t130$p.value,t131$p.value,t132$p.value,t133$p.value),
                           user39=c(t134$p.value,t135$p.value,t136$p.value,t137$p.value,t138$p.value,t139$p.value,t140$p.value,t141$p.value,t142$p.value,t143$p.value,t144$p.value),
                           
                           user42=c(t145$p.value,t146$p.value,t147$p.value,t148$p.value,t149$p.value,t150$p.value,t151$p.value,t152$p.value,t153$p.value,t154$p.value,t155$p.value),
                           user43=c(t156$p.value,t157$p.value,t158$p.value,t159$p.value,t160$p.value,t161$p.value,t162$p.value,t163$p.value,t164$p.value,t165$p.value,t166$p.value),
                           user49=c(t167$p.value,t168$p.value,t169$p.value,t170$p.value,t171$p.value,t172$p.value,t173$p.value,t174$p.value,t175$p.value,t176$p.value,t177$p.value),
                           user50=c(t178$p.value,t179$p.value,t180$p.value,t181$p.value,t182$p.value,t183$p.value,t184$p.value,t185$p.value,t186$p.value,t187$p.value,t188$p.value),
                           
                           user51=c(t189$p.value,t190$p.value,t191$p.value,t192$p.value,t193$p.value,t194$p.value,t195$p.value,t196$p.value,t197$p.value,t198$p.value,t199$p.value),
                           user52=c(t200$p.value,t201$p.value,t202$p.value,t203$p.value,t204$p.value,t205$p.value,t206$p.value,t207$p.value,t208$p.value,t209$p.value,t210$p.value),
                           user53=c(t211$p.value,t212$p.value,t213$p.value,t214$p.value,t215$p.value,t216$p.value,t217$p.value,t218$p.value,t219$p.value,t220$p.value,t221$p.value),
                           user56=c(t222$p.value,t223$p.value,t224$p.value,t225$p.value,t226$p.value,t227$p.value,t228$p.value,t229$p.value,t230$p.value,t231$p.value,t232$p.value))

sessionTest<-cbind(c("H.period","H.t","H.i","H.e","H.five","H.Shift.R","H.o","H.a","H.n","H.l","H.Return"),independent.test7n8,4)
sessionTesting<-sessionTest[,-1]
rownames(sessionTesting)<-sessionTest[,1]
sessionTesting


###1: Our main goal was to design a good classifier. So, before jumping into directly fitting all the predictors into different classifier methods. We decided to check the correlation between predictors. And, hence the following chart obtained.

#correlation chart
#read known
known <- read.csv("known.csv", header = TRUE)
#savung original data
original.known <- known
#removing unwanted varibales
known <- known[,-c(1,3,4)]
#cor.predictors = known[,-c(1, seq(2,ncol(known),3))]
library(ggplot2)
library(ggcorrplot)
ggcorrplot(cor(data.frame(known[,-1])),hc.order = TRUE, type = "lower",lab = TRUE, title = '',tl.cex = 25,tl.srt = 45,show.legend = FALSE)
#ggcorrplot(cor(cor.predictors), hc.order = TRUE, outline.col = "white",tl.cex = 25,title = 'Correlation chart for higly correlated predictor variables')

###2: Another method we deployed to obtain importance of predictors is we produced box plots between one predictor versus all the classes on a single plot. And, hence we produced 31 those plots. As to show those 31 boxplots in this report we felt unnecessary, and hence we decided to show following 4 plots.

#predictor vs class

for(i in 2:5){
  library(ggplot2)
  print(ggplot(known, aes(x=subject, y=known[,i], color=subject)) +
          geom_boxplot() + labs(x = "Subject",y = names(known)[i]) + theme(axis.title.x = element_text(face="bold", size=6), 
                                                                           axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none")) 
}


##3 Obtaining Classifiers

#spliting training and test data: spliting is done in a such way that train and test data were splitted according to proportion of the data in each class
set.seed(702)
library(caret)

indx.known = createDataPartition(original.known$subject, p = 3/5, list = FALSE)
train.known.raw = original.known[indx.known ,]#preserve repeation (rep) on train data
test.known.raw = original.known[-indx.known ,]#preserve repeation (rep) on test data
train.known = train.known.raw[,-c(1,3,4)]
names(train.known)
test.known = test.known.raw[,-c(1,3,4)]

#checking the number of observations in 51 classes of subjects in orignal,train and test dataset
percentageOrig <-prop.table(table(known$subject))*100
co<-cbind(Original.freq=table(original.known$subject),Original.Percentage=percentageOrig)

percentage <-prop.table(table(train.known$subject))*100
ctrain<-cbind(Train.Freq =table(train.known$subject),Train.Percentage=percentage)

percentagetest<-prop.table(table(test.known$subject))*100
ctest<-cbind(Test.Freq=table(test.known$subject),Test.Percentage=percentagetest)

cbind(co,ctrain,ctest)

#repetition of each class in train data
temp <- as.data.frame(table(train.known$subject))
colnames(temp) <- c('subject','Number of repetitions')
trn.cnt <- temp
#repetition of each class in test data
temp <- as.data.frame(table(test.known$subject))
colnames(temp) <- c('subject','Number of repetitions')
tst.cnt <- temp

####i:  Multinomial logistics regression

#using only H varibales
library(MASS)
library(nnet)
set.seed(702)

#fit multinom
###1 : GLM multinom() fucntion to obtain a classifier: Only H Predictors.

fit.multinom <- multinom(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o +H.a + H.l + H.n + H.Return, data = train.known)

set.seed(702)
#use multinom to predict class train data
pred.multi.trn = predict(fit.multinom, data = train.known)
cbind(table(pred.multi.trn))
#confusion matrix
c.m <- cbind(table(pred.multi.trn,train.known$subject))

#validation Approach train accuracy:
mean(pred.multi.trn == train.known$subject)

# 5-fold validation accuarcy
multi.split <- split(1:dim(known)[1], sample(1:dim(known)[1], 5, replace=F))
#Put the for loop together for 5 folds
multi.err.known <- rep(0,5)
for(i in 1:5)
{
  # Fit a mutinom model to training data.
  multinom.k.known <- multinom(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o +H.a + H.l + H.n + H.Return, data = known[-multi.split[[i]],]) 
  #Prediction for the multinom model
  multinom.pred.k.known <- predict(multinom.k.known , newdata = known[multi.split[[i]],])
  #Create a vector of accuracy with if prediction is incorrect
  multi.err.known[i] <- mean(multinom.pred.k.known == known[multi.split[[i]],]$subject)
}
#5 fold CV: Calculate the accuracy
test.error.multinom.k.known <- round(sum(multi.err.known)/5,6)
#78.2113

##Validation Approach: Multinomial prediction on test data
pred.multi.tst = predict(fit.multinom, test.known)
#confusion matrix on test data
c.m.tst <- cbind(table(pred.multi.tst,test.known$subject))

trn.glm <- mean(pred.multi.trn == train.known$subject)

vsa.glm <- mean(pred.multi.tst == test.known$subject)

glm.5 <- test.error.multinom.k.known 

#### Table 1: Accuracy for GLM Model with H predictors only:

library(kableExtra)
library(knitr)
Model <- c('Glm using multinom function')
Methods <- c('Train','Test','5-fold')
Accuracy <- c(trn.glm, vsa.glm, glm.5)
temp <- as.data.frame(cbind(Model,Methods,Accuracy))
kable(temp, align="r")


#### ii : LDA

####Test accuracy for LDA model with **H** predictors only:

###ii. Test accuracy for LDA model with **H** predictors only:
attach(known)
set.seed(702)
lda.small = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return, data = train.known)
#predict on train
lda.pred.small = predict(lda.small, train.known)
c.m <- cbind(table(lda.pred.small$class,train.known$subject))
write.csv(c.m,'Confusion matrix on training data using LDA for small model.csv')
image(c.m)
#train error
trn.e.s <- mean(lda.pred.small$class == train.known$subject)
#prediction on test data
lda.pred.small.tst = predict(lda.small, test.known)
c.m.t.h <- cbind(table(lda.pred.small.tst$class,test.known$subject))
write.csv(c.m.t.h,'Confusion matrix on test data using LDA for small model.csv')
image(c.m.t.h)
#Test accuracy
v.s <- mean(lda.pred.small.tst$class == test.known$subject)#0.7826087

### Test accuracy using loocv for LDA model with H predictor only
#test accuracy using loocv
library(boot)
set.seed(702)
cnt.small = rep(0, dim(known)[1])
for (i in 1:(dim(known)[1])) {
  small.lda = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return , data = known[-i,])
  small.pred = predict(small.lda, known[i,])
  if (small.pred$class != known$subject[i]) 
    cnt.small[i] = 1
}
loocv.lda.small <- 1-sum(cnt.small)/dim(known)[1]
#loocv.lda.small#0.8113739

###  5-fold CV for LDA model with H only is given by:
set.seed(900)

indx.known = createDataPartition(known$subject, p = 1/5, list = FALSE)

train.known1 = known[indx.known ,]
test.known1 = known[-indx.known ,]

set.seed(901)
indx.known2 = createDataPartition(test.known1$subject, p=1/4, list = FALSE)

train.known2 = test.known1[indx.known2,]
test.known2 = test.known1[-indx.known2,]

set.seed(902)
indx.known3 = createDataPartition(test.known2$subject, p=1/3, list = FALSE)

train.known3 = test.known2[indx.known3,]
test.known3 = test.known2[-indx.known3,]

set.seed(903)
indx.known4 = createDataPartition(test.known3$subject, p=1/2, list = FALSE)

train.known4 = test.known3[indx.known4,]
final.train = test.known3[-indx.known4,]


#1st test error

train1 <- rbind(train.known2,train.known3,train.known4,final.train)
test1 = train.known1

full.model.lda = lda(subject ~. , data = train1)
full.pred = predict(full.model.lda, test1)
lda.error.5.1 <- mean(full.pred$class != test1$subject)

#2nd test error

train2 <- rbind(train.known3,train.known4,final.train,train.known1)
test2 = train.known2

full.model.lda = lda(subject ~. , data = train2)
full.pred = predict(full.model.lda, test2)
lda.error.5.2 <- mean(full.pred$class != test2$subject)

#3rd test error

train3 <- rbind(train.known4,final.train,train.known,train.known2)
test3 = train.known3

full.model.lda = lda(subject ~. , data = train3)
full.pred = predict(full.model.lda, test3)
lda.error.5.3 <- mean(full.pred$class != test3$subject)

#4th test error

train4 <- rbind(final.train,train.known,train.known2,train.known3)
test4 = train.known4

full.model.lda = lda(subject ~. , data = train4)
full.pred = predict(full.model.lda, test4)
lda.error.5.4 <- mean(full.pred$class != test4$subject)

#5th test error

train5 <- rbind(train.known,train.known2,train.known3,train.known4)
test5 = final.train

full.model.lda = lda(subject ~. ,data = train5)
full.pred = predict(full.model.lda, test5)
lda.error.5.5 <- mean(full.pred$class != test5$subject)

#5-fold cross-validation error is given by:
sum(lda.error.5.1,lda.error.5.2,lda.error.5.3,lda.error.5.4,lda.error.5.5)/5


#1st test error

train1 <- rbind(train.known2,train.known3,train.known4,final.train)
test1 = train.known1

small.model.lda = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return , data = train1)
small.pred = predict(small.model.lda, test1)
lda.error.5.1 <- mean(small.pred$class != test1$subject)

#2nd test error

train2 <- rbind(train.known3,train.known4,final.train,train.known1)

small.model.lda = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return , data = train2)
small.pred = predict(small.model.lda, test2)
lda.error.5.2 <- mean(small.pred$class != test2$subject)

#3rd test error

train3 <- rbind(train.known4,final.train,train.known,train.known2)
test3 = train.known3

small.model.lda = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return , data = train3)
small.pred = predict(small.model.lda, test3)
lda.error.5.3 <- mean(small.pred$class != test3$subject)

#4th test error

train4 <- rbind(final.train,train.known,train.known2,train.known3)
test4 = train.known4

small.model.lda = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return , data = train4)
small.pred = predict(small.model.lda, test4)
lda.error.5.4 <- mean(small.pred$class != test4$subject)

#5th test error

train5 <- rbind(train.known,train.known2,train.known3,train.known4)
test5 = final.train

small.model.lda = lda(subject ~ H.period + H.t + H.i + H.e + H.five + H.Shift.r + H.o + H.a + H.l + H.n + H.Return,data = train5)
small.pred = predict(small.model.lda, test5)
lda.error.5.5 <- mean(small.pred$class != test5$subject)

#5-fold model accuarcy is given by:
s.5 <- 1-sum(lda.error.5.1,lda.error.5.2,lda.error.5.3,lda.error.5.4,lda.error.5.5)/5


####Table 2: Accuracy Table for LDA having only H predictors:

library(kableExtra)
library(knitr)
Model <- c('LDA having only H predictors')
Method <- c('Train','Test','LOOCV','5-fold')
Accuracy <- c(trn.e.s,v.s,s.5,loocv.lda.small)
temp <- as.data.frame(cbind(Model,Method,Accuracy))
kable(temp, align="r")


####iii: LDA Model with **H** and **UD** predictors

set.seed(702)
lda.ud.hh = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
                + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return , data = train.known)
#predict on train
lda.pred.ud.hh = predict(lda.ud.hh, train.known)
c.m.ud.hh <- cbind(table(lda.pred.ud.hh$class,train.known$subject))
write.csv(c.m.ud.hh,'Confusion matrix on training data using LDA for the model having H and UD predictors only.csv')
#image(c.m)
#train error
trn.e.ud <- mean(lda.pred.ud.hh$class == train.known$subject)
#prediction on test data
lda.pred.ud.tst = predict(lda.ud.hh, test.known)
c.m.t.ud <- cbind(table(lda.pred.ud.tst$class,test.known$subject))
write.csv(c.m.t.ud,'Confusion matrix on test data using LDA for the model having H and UD predictors only.csv')
image(c.m.t.ud)
#Test accuracy
v.s.ud <-  mean(lda.pred.ud.tst$class == test.known$subject)##0.8695652



#test accuracy using loocv LDA for the model having H and UD predictors only
library(boot)
set.seed(702)
cnt.lda.ud = rep(0, dim(known)[1])
for (i in 1:(dim(known)[1])) {
  lda.ud.hh = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
                  + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return, data = known[-i,])
  lda.hh.pred = predict(lda.ud.hh, known[i,])
  if (lda.hh.pred$class != known$subject[i]) 
    cnt.lda.ud[i] = 1
}
loocv.lda.ud.hh <- 1-sum(cnt.lda.ud)/dim(known)[1]
##0.8738739

####Model for LDA having H and UD predictors only using 5-fold CV method:
set.seed(702) 
#1st test error

train1 <- rbind(train.known2,train.known3,train.known4,final.train)
test1 = train.known1

lda1 = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
           + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return , data = train1)
pred1 = predict(lda1, test1)
err1 <- mean(pred1$class != test1$subject)


#2nd test error

train2 <- rbind(train.known3,train.known4,final.train,train.known1)
test2 = train.known2

lda2 = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
           + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return , data = train2)
pred2 = predict(lda2, test2)
err2 <- mean(pred2$class != test2$subject)

#3rd test error

train3 <- rbind(train.known4,final.train,train.known,train.known2)
test3 = train.known3

lda3 = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
           + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return , data = train3)
pred3 = predict(lda3, test3)
err3 <- mean(pred3$class != test3$subject)


#4th test error

train4 <- rbind(final.train,train.known,train.known2,train.known3)
test4 = train.known4

lda4 = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
           + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return, data = train4)
pred4 = predict(lda4, test4)
err4 <- mean(pred4$class != test4$subject)


#5th test error

train5 <- rbind(train.known,train.known2,train.known3,train.known4)
test5 = final.train

lda5 = lda(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r 
           + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return, data = train5)
pred5 = predict(lda5, test5)
err5 <- mean(pred5$class != test5$subject)


#5-fold cross-validation error is given by:
mean.err1 <- 1-sum(err1,err2,err3,err4,err5)/5 ##0.8806738

s.5.ud <- mean.err1
#s.5.ud



#### Table 3: Accuracy for LDA having only H and UD predictors:

library(kableExtra)
library(knitr)
Model <- c('LDA having only H and UD predictors')
Method <- c('Train','Test','LOOCV','5-fold')
Accuracy <- c(trn.e.ud,v.s.ud, s.5.ud,loocv.lda.ud.hh)
temp <- as.data.frame(cbind(Model, Method,Accuracy))
kable(temp, align="r")


                                                                                                                                                               ####iv: Implementation of PCA on LDA
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE,include=FALSE}
                                                                                                                                                               #ref:https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
                                                                                                                                                               
                                                                                                                                                               #try to implement LDA + PCR
                                                                                                                                                               
                                                                                                                                                               #read known
                                                                                                                                                               known.pca <- read.csv("known.csv", header = TRUE)
                                                                                                                                                               #savung original data
                                                                                                                                                               original.known.pca <- known.pca[,-1]
                                                                                                                                                               #removing response classes and unwanted varibales
                                                                                                                                                               known.pca <- known.pca[,-c(1,2,3,4)]
                                                                                                                                                               head(known.pca)
                                                                                                                                                               
                                                                                                                                                               #converting fetaures
                                                                                                                                                               known.pca.data = as.matrix(known.pca)
                                                                                                                                                               head(known.pca.data)
                                                                                                                                                               
                                                                                                                                                               #create subject vector
                                                                                                                                                               subjt = original.known.pca$subject
                                                                                                                                                               
                                                                                                                                                               ##PCA
                                                                                                                                                               
                                                                                                                                                               pca.known.pca = prcomp(known.pca, scale. = TRUE)#pca on predictors
                                                                                                                                                               names(pca.known.pca)
                                                                                                                                                               pca.known.pca$center
                                                                                                                                                               pca.known.pca$scale
                                                                                                                                                               
                                                                                                                                                               #most imp
                                                                                                                                                               pca.known.pca$rotation
                                                                                                                                                               dim(pca.known.pca$x)
                                                                                                                                                               biplot(pca.known.pca, scale = 0)
                                                                                                                                                               
                                                                                                                                                               #We aim to find the components which explain the maximum variance. This is because, we want to retain as much 
                                                                                                                                                               #information as possible using these components. So, higher is the explained variance, higher will be the information contained in those components.
                                                                                                                                                               
                                                                                                                                                               #compute the standard deviation of each principal component
                                                                                                                                                               std.dev = pca.known.pca$sdev
                                                                                                                                                               #compute variance
                                                                                                                                                               var = std.dev^2
                                                                                                                                                               var[1:21]
                                                                                                                                                               #proportion of variance
                                                                                                                                                               prop.var = var/sum(var)
                                                                                                                                                               sum(prop.var[1:20])#to see the 1st 10 variance explained by 10 principle component
                                                                                                                                                               
                                                                                                                                                               #scree plot
                                                                                                                                                               plot(prop.var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")
                                                                                                                                                               #by looking at the plot we can see that 21 principal components are enough to explain the variabilities in the data
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               ####The scree plot for PCA can be obtained as:
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               library(ggplot2)
                                                                                                                                                               x= c(1:31)
                                                                                                                                                               qplot(x,prop.var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "l")
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               By looking at the scree plot we can say that 21 principle componests have explained almost 100% variablity on the data. Hence using 21 principle components for further analysis.
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               known.pca.pca = pca.known.pca$x[,1:21] #choosing 21 principle components
                                                                                                                                                               #dim(known.pca.pca)
                                                                                                                                                               
                                                                                                                                                               known.pca.pca.sub = data.frame(cbind(known.pca.pca,subjt))
                                                                                                                                                               #dim(known.pca.pca.sub)
                                                                                                                                                               #head(known.pca.pca.sub)
                                                                                                                                                               
                                                                                                                                                               #split the data into train and test set
                                                                                                                                                               set.seed(702)
                                                                                                                                                               library(caret)
                                                                                                                                                               indx.known.pca = createDataPartition(known.pca.pca.sub$subjt, p = 3/5, list = FALSE)
                                                                                                                                                               train.known.pca.pca = known.pca.pca.sub[indx.known.pca,]
                                                                                                                                                               test.known.pca.pca = known.pca.pca.sub[-indx.known.pca,]
                                                                                                                                                               
                                                                                                                                                               #perform LDA
                                                                                                                                                               attach(known.pca.pca.sub)
                                                                                                                                                               library(MASS)
                                                                                                                                                               lda.pca = lda(subjt ~. , data = train.known.pca.pca)
                                                                                                                                                               
                                                                                                                                                               #predict on train
                                                                                                                                                               lda.pred.trn = predict(lda.pca, train.known.pca.pca)
                                                                                                                                                               #train error
                                                                                                                                                               trn.e.pca <- 1 - mean(lda.pred.trn$class == train.known.pca.pca$subjt)
                                                                                                                                                               #prediction on test data
                                                                                                                                                               lda.pred.tst = predict(lda.pca, test.known.pca.pca)
                                                                                                                                                               c.m.tst.pca <- cbind(table(lda.pred.tst$class,test.known.pca.pca$subjt))
                                                                                                                                                               
                                                                                                                                                               colnames(c.m.tst.pca) = paste(unique(original.known.pca$subject))
                                                                                                                                                               rownames(c.m.tst.pca) = paste(unique(original.known.pca$subject))
                                                                                                                                                               write.csv(c.m.tst.pca,'Confusion matrix on test data using PCA and LDA.csv')
                                                                                                                                                               
                                                                                                                                                               #image(c.m.t)
                                                                                                                                                               #validation error
                                                                                                                                                               tst.e.pca = 1-mean(lda.pred.tst$class == test.known.pca.pca$subjt)
                                                                                                                                                               
                                                                                                                                                               #repetition of each class in train data
                                                                                                                                                               temp <- as.data.frame(table(train.known.pca.pca$subjt))
                                                                                                                                                               colnames(temp) <- c('subject','Number of repetitions')
                                                                                                                                                               trn.cnt.pca <- temp
                                                                                                                                                               #repetition of each class in test data
                                                                                                                                                               temp <- as.data.frame(table(test.known.pca.pca$subjt))
                                                                                                                                                               colnames(temp) <- c('subject','Number of repetitions')
                                                                                                                                                               tst.cnt.pca <- temp
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,message=FALSE,include=FALSE}
                                                                                                                                                               #### The training error for PCA + LDA is given by:
                                                                                                                                                               trn.e.pca 
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,include=FALSE}
                                                                                                                                                               #### The test error for PCA + LDA is given by:
                                                                                                                                                               tst.e.pca 
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ##Table 4: Test accuracy for PCA on LDA model
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               
                                                                                                                                                               library(kableExtra)
                                                                                                                                                               library(knitr)
                                                                                                                                                               Model <- c('Applying PCA on LDA')
                                                                                                                                                               Method <- c('Train','Test')
                                                                                                                                                               Accuracy <- c(1-trn.e.pca, 1-tst.e.pca )
                                                                                                                                                               temp <- as.data.frame(cbind(Model, Method, Accuracy))
                                                                                                                                                               kable(temp, align="r")
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               #### V : Bagging and Random Forest using all predictors
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               library(randomForest)
                                                                                                                                                               set.seed(900)
                                                                                                                                                               ####Bagging
                                                                                                                                                               
                                                                                                                                                               bagging <- randomForest(subject ~.,data = train.known, mtry = 31, ntree = 500, importance = TRUE)
                                                                                                                                                               #predict trn
                                                                                                                                                               yhat.pred.trn = predict(bagging, train.known)
                                                                                                                                                               c.m.bag.trn <- table(yhat.pred.trn,train.known$subject)
                                                                                                                                                               accuracy <- (sum(diag(c.m.bag.trn)))/sum(c.m.bag.trn)
                                                                                                                                                               #train accuracy
                                                                                                                                                               bag.trn <- accuracy
                                                                                                                                                               #predict test
                                                                                                                                                               yhat.bagging = predict(bagging,test.known)
                                                                                                                                                               c.m.bag.tst <- table(yhat.bagging,test.known$subject)
                                                                                                                                                               accuracy1 <- (sum(diag(c.m.bag.tst)))/sum(c.m.bag.tst)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               bag.err <- accuracy1
                                                                                                                                                               #confusionMatrix(data = yhat.bagging, reference = test.known$subject)
                                                                                                                                                               #save csv
                                                                                                                                                               write.csv(c.m.bag.tst,'Confusion matrix on test data using Bagging.csv')
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ###Random Forest
                                                                                                                                                               library(randomForest)
                                                                                                                                                               rf <- randomForest(subject ~.,data = train.known, mtry = 5, ntree = 500, importance = TRUE)
                                                                                                                                                               rf.model <- rf
                                                                                                                                                               #predict trn
                                                                                                                                                               yhat.pred.trn = predict(rf, train.known)
                                                                                                                                                               c.m.rf.trn <- table(yhat.pred.trn,train.known$subject)
                                                                                                                                                               accuracy <- (sum(diag(c.m.rf.trn)))/sum(c.m.rf.trn)
                                                                                                                                                               #train accuracy
                                                                                                                                                               rf.trn <- accuracy
                                                                                                                                                               #predict test
                                                                                                                                                               yhat.rf = predict(rf,test.known)
                                                                                                                                                               c.m.rf.tst <- table(yhat.rf,test.known$subject)
                                                                                                                                                               accuracy <-(sum(diag(c.m.rf.tst)))/sum(c.m.rf.tst)
                                                                                                                                                               
                                                                                                                                                               #train accuracy
                                                                                                                                                               
                                                                                                                                                               rf.err <- accuracy
                                                                                                                                                               #confusionMatrix(data = yhat.rf, reference = test.known$subject)
                                                                                                                                                               #save csv
                                                                                                                                                               #write.csv(c.m.rf.tst,'Confusion matrix on test data using RandomForest.csv')
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE,include=FALSE}
                                                                                                                                                               #### 5-fold CV error for Bagging and RandomForest can be given by,respectively: 
                                                                                                                                                               library(caret)
                                                                                                                                                               library(randomForest)
                                                                                                                                                               set.seed(900)
                                                                                                                                                               
                                                                                                                                                               indx.known = createDataPartition(known$subject, p = 1/5, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known1 = known[indx.known ,]
                                                                                                                                                               test.known1 = known[-indx.known ,]
                                                                                                                                                               
                                                                                                                                                               set.seed(901)
                                                                                                                                                               indx.known2 = createDataPartition(test.known1$subject, p = 1/4, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known2 = test.known1[indx.known2,]
                                                                                                                                                               test.known2 = test.known1[-indx.known2,]
                                                                                                                                                               
                                                                                                                                                               set.seed(902)
                                                                                                                                                               indx.known3 = createDataPartition(test.known2$subject, p = 1/3, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known3 = test.known2[indx.known3,]
                                                                                                                                                               test.known3 = test.known2[-indx.known3,]
                                                                                                                                                               
                                                                                                                                                               set.seed(903)
                                                                                                                                                               indx.known4 = createDataPartition(test.known3$subject, p = 1/2, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known4 = test.known3[indx.known4,]
                                                                                                                                                               final.train = test.known3[-indx.known4,]
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               #1st test accuracy
                                                                                                                                                               
                                                                                                                                                               train1 <- rbind(train.known2,train.known3,train.known4,final.train)
                                                                                                                                                               test1 = train.known1
                                                                                                                                                               
                                                                                                                                                               full.model.bag = randomForest(subject ~. ,data = train1, mtry = 31, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.bagging = predict(full.model.bag,test1)
                                                                                                                                                               c.m.bag <- table(yhat.bagging,test1$subject)
                                                                                                                                                               accuracy1 <- (sum(diag(c.m.bag)))/sum(c.m.bag)
                                                                                                                                                               
                                                                                                                                                               #Test accuracy
                                                                                                                                                               bag.err1 <- accuracy1 
                                                                                                                                                               
                                                                                                                                                               #2nd test accuracy
                                                                                                                                                               
                                                                                                                                                               train2 <- rbind(train.known3,train.known4,final.train,train.known1)
                                                                                                                                                               test2 = train.known2
                                                                                                                                                               
                                                                                                                                                               full.model.bag = randomForest(subject ~. ,data = train2, mtry = 31, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.bagging = predict(full.model.bag,test2)
                                                                                                                                                               c.m.bag <- table(yhat.bagging,test2$subject)
                                                                                                                                                               accuracy2 <- (sum(diag(c.m.bag)))/sum(c.m.bag)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               bag.err2 <- accuracy2
                                                                                                                                                               
                                                                                                                                                               #3rd test accuracy
                                                                                                                                                               
                                                                                                                                                               train3 <- rbind(train.known4,final.train,train.known1,train.known2)
                                                                                                                                                               test3 = train.known3
                                                                                                                                                               
                                                                                                                                                               full.model.bag = randomForest(subject ~. ,data = train3, mtry = 31, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.bagging = predict(full.model.bag,test3)
                                                                                                                                                               c.m.bag <- table(yhat.bagging,test3$subject)
                                                                                                                                                               accuracy3 <- (sum(diag(c.m.bag)))/sum(c.m.bag)
                                                                                                                                                               
                                                                                                                                                               #Test accuracy
                                                                                                                                                               bag.err3 <- accuracy3
                                                                                                                                                               
                                                                                                                                                               #4th test accuracy
                                                                                                                                                               
                                                                                                                                                               train4 <- rbind(final.train,train.known1,train.known2,train.known3)
                                                                                                                                                               test4 = train.known4
                                                                                                                                                               
                                                                                                                                                               full.model.bag = randomForest(subject ~. ,data = train4, mtry = 31, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.bagging = predict(full.model.bag,test4)
                                                                                                                                                               c.m.bag <- table(yhat.bagging,test4$subject)
                                                                                                                                                               accuracy4 <- (sum(diag(c.m.bag)))/sum(c.m.bag)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               bag.err4 <- accuracy4
                                                                                                                                                               
                                                                                                                                                               #5th test accuracy
                                                                                                                                                               
                                                                                                                                                               train5 <- rbind(train.known1,train.known2,train.known3,train.known4)
                                                                                                                                                               test5 = final.train
                                                                                                                                                               
                                                                                                                                                               full.model.bag = randomForest(subject ~. ,data = train5, mtry = 31, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.bagging = predict(full.model.bag,test5)
                                                                                                                                                               c.m.bag <- table(yhat.bagging,test5$subject)
                                                                                                                                                               accuracy5 <- (sum(diag(c.m.bag)))/sum(c.m.bag)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               bag.err5 <- accuracy5
                                                                                                                                                               
                                                                                                                                                               #5-fold Model accuracyr is given by:
                                                                                                                                                               bag.k5 <- sum(bag.err1,bag.err2,bag.err3,bag.err4,bag.err5)/5
                                                                                                                                                               
                                                                                                                                                               ### For RandomForest
                                                                                                                                                               set.seed(900)
                                                                                                                                                               
                                                                                                                                                               indx.known = createDataPartition(known$subject, p = 1/5, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known1 = known[indx.known ,]
                                                                                                                                                               test.known1 = known[-indx.known ,]
                                                                                                                                                               
                                                                                                                                                               set.seed(901)
                                                                                                                                                               indx.known2 = createDataPartition(test.known1$subject, p = 1/4, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known2 = test.known1[indx.known2,]
                                                                                                                                                               test.known2 = test.known1[-indx.known2,]
                                                                                                                                                               
                                                                                                                                                               set.seed(902)
                                                                                                                                                               indx.known3 = createDataPartition(test.known2$subject, p = 1/3, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known3 = test.known2[indx.known3,]
                                                                                                                                                               test.known3 = test.known2[-indx.known3,]
                                                                                                                                                               
                                                                                                                                                               set.seed(903)
                                                                                                                                                               indx.known4 = createDataPartition(test.known3$subject, p = 1/2, list = FALSE)
                                                                                                                                                               
                                                                                                                                                               train.known4 = test.known3[indx.known4,]
                                                                                                                                                               final.train = test.known3[-indx.known4,]
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               #fold1
                                                                                                                                                               full.model.rf = randomForest(subject ~. ,data = train1, mtry = 5, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.rf = predict(full.model.rf,test1)
                                                                                                                                                               c.m.rf <- table(yhat.rf,test1$subject)
                                                                                                                                                               accuracy1 <- (sum(diag(c.m.rf)))/sum(c.m.rf)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               rf.err1 <- accuracy1 
                                                                                                                                                               #fold2
                                                                                                                                                               full.model.rf = randomForest(subject ~. ,data = train2, mtry = 5, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.rf = predict(full.model.rf,test2)
                                                                                                                                                               c.m.rf <- table(yhat.rf,test2$subject)
                                                                                                                                                               accuracy2 <- (sum(diag(c.m.rf)))/sum(c.m.rf)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               rf.err2 <- accuracy2 
                                                                                                                                                               #fold3
                                                                                                                                                               full.model.rf = randomForest(subject ~. ,data = train3, mtry = 5, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.rf = predict(full.model.rf,test3)
                                                                                                                                                               c.m.rf <- table(yhat.rf,test3$subject)
                                                                                                                                                               accuracy3 <- (sum(diag(c.m.rf)))/sum(c.m.rf)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               rf.err3 <- accuracy3
                                                                                                                                                               #fold4
                                                                                                                                                               full.model.rf = randomForest(subject ~. ,data = train4, mtry = 5, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.rf = predict(full.model.rf,test4)
                                                                                                                                                               c.m.rf <- table(yhat.rf,test4$subject)
                                                                                                                                                               accuracy4 <- (sum(diag(c.m.rf)))/sum(c.m.rf)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               rf.err4 <- accuracy4
                                                                                                                                                               #fold5
                                                                                                                                                               full.model.rf = randomForest(subject ~. ,data = train5, mtry = 5, ntree = 500, importance = TRUE)
                                                                                                                                                               yhat.rf = predict(full.model.rf,test5)
                                                                                                                                                               c.m.rf <- table(yhat.rf,test5$subject)
                                                                                                                                                               accuracy5 <- (sum(diag(c.m.rf)))/sum(c.m.rf)
                                                                                                                                                               #Test accuracy
                                                                                                                                                               rf.err5 <- accuracy5
                                                                                                                                                               
                                                                                                                                                               #5-fold cross-validation error is given by:
                                                                                                                                                               rf.k5 <- sum(rf.err1,rf.err2,rf.err3,rf.err4,rf.err5)/5
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE,eval=FALSE}
                                                                                                                                                               
                                                                                                                                                               #### LOOCV accuracy for Bagging and RandomForest can be given by,respectively:
                                                                                                                                                               #test error using loocv
                                                                                                                                                               library(boot)
                                                                                                                                                               set.seed(4)
                                                                                                                                                               cnt.1 = rep(0, dim(known)[1])
                                                                                                                                                               for (i in 1:(dim(known)[1])) {
                                                                                                                                                                 full.model.bag = randomForest(subject ~. , data = known[-i,], mtry = 31,ntree = 500, imporatnce = TRUE)
                                                                                                                                                                 yhat.bagging = predict(full.model.bag, newdata = known[i,])
                                                                                                                                                                 if (yhat.bagging != known$subject[i]) 
                                                                                                                                                                   cnt.1[i] = 1
                                                                                                                                                               }
                                                                                                                                                               loocv.bag.2 <- sum(cnt.1)/dim(known)[1]
                                                                                                                                                               loocv.bag.1 <- 1- 0.08558559
                                                                                                                                                               #0.08558559
                                                                                                                                                               cnt.2 = rep(0, dim(known)[1])
                                                                                                                                                               for (i in 1:(dim(known)[1])) {
                                                                                                                                                                 full.model.rf = randomForest(subject ~. , data = known[-i,], mtry = 5,ntree = 500, imporatnce = TRUE)
                                                                                                                                                                 yhat.rf = predict(full.model.rf, newdata = known[i,])
                                                                                                                                                                 if (yhat.rf != known$subject[i]) 
                                                                                                                                                                   cnt.2[i] = 1
                                                                                                                                                               }
                                                                                                                                                               loocv.r <- sum(cnt.2)/dim(known)[1]
                                                                                                                                                               loocv.rf <- 1- 0.03828829
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               loocv.bag.1 <- 1- 0.08558559
                                                                                                                                                               loocv.rf <- 1- 0.03828829
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ####Vi: Bagging 
                                                                                                                                                               Bagging involves creating multiple copies of the original training data set using the bootstrap, fitting a separate decision tree on individual copy, and then combining all the trees in order to create a single predictive model. In bagging,each tree is built on a bootstrap data set which is independent of the other trees. Generally, it considers m = p (number of predictors in the model). Implementing a bagging model on the train dataset and assessing the training and testing accuracy, k-fold and loocv yields the output shown in Table 4 below.
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               #### Table 5: Accuracy for bagging can be given as:
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               library(kableExtra)
                                                                                                                                                               library(knitr)
                                                                                                                                                               Model <- c('Bagging having all predictors')
                                                                                                                                                               Method <- c('Train','Test','LOOCV','5-fold')
                                                                                                                                                               Accuracy <- c(bag.trn,bag.err,loocv.bag.1, bag.k5)
                                                                                                                                                               temp <- as.data.frame(cbind(Model, Method, Accuracy))
                                                                                                                                                               kable(temp, align="r")
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               Table 4 displayed the accuracy rate of model with bagging. The k-fold approach appears to be performing well in term of overall accuracy (93.8%) as compared to test accuracy (92.17%) and LOOCV approach (91.44%). The subject specific accuracy in the bagging model seems to be performing better than the LD model, with greater proportion of the subject specific accuracy being 100%. This model performs better than LDA model with H and UD predictors.
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          ####Vi: Random Forest
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          Random Forest is an ensemble model that constructs a number of decision trees at training time and output the class that is the mode of the classes output by individual trees. It basically averages the predictions made by tree models to make predictions. Random Forest gives more improved prediction compared to boosting. It considers the important variables first by building randomly different tree models. It considers m = sqrt(p) in the place of mtry for random forests of classification trees, where mtry is number of predictors in the model. Implementing a random forest model on the train dataset and assessing the training and testing accuracy of the model yields the output shown in Table below. 
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          #### Table 6: Accuracy for Random forest model:
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                                                                          library(kableExtra)
                                                                                                                                                                                                                                                                                                                                                                          library(knitr)
                                                                                                                                                                                                                                                                                                                                                                          Model <- c('Rf having all predictors')
                                                                                                                                                                                                                                                                                                                                                                          Method <- c('Train','Test','K-fold','LOOCV')
                                                                                                                                                                                                                                                                                                                                                                          Accuracy <- c(rf.trn,rf.err, rf.k5,loocv.rf)
                                                                                                                                                                                                                                                                                                                                                                          temp <- as.data.frame(cbind(Model, Method,Accuracy))
                                                                                                                                                                                                                                                                                                                                                                          kable(temp, align="r")
                                                                                                                                                                                                                                                                                                                                                                          ```
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                          In Table 5 above, it can be observed that loocv method records the highest accuracy rate of 96.2% compared to test accuracy (96%). K-fold performed below test and loocv with 78.2% accuracy rate. The subject specific accuracy in the random forest model seems to be performing well with majority of the subjects correctly predicting the reps in each subject.
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ####Vii: Boosting
                                                                                                                                                               
                                                                                                                                                               Unlike bagging, in boosting the subset creation is not random and depends upon the performance of the previous models: every new subsets contains the elements that were (likely to be) misclassified by previous models. Hence, in boosting, the trees are grown sequentially: that is each tree is grown using information from previously grown trees. Boosting does not involve bootstrap sampling; instead each tree is fit on a modified version of the original data set.
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE,include=FALSE}
                                                                                                                                                               ###Boosting
                                                                                                                                                               set.seed(701)
                                                                                                                                                               library(adabag) # for adaboost
                                                                                                                                                               
                                                                                                                                                               boost.model <- boosting(subject ~., data = train.known, boos = TRUE, mfinal = 20, coeflearn = 'Breiman')
                                                                                                                                                               
                                                                                                                                                               #predict train
                                                                                                                                                               pred.boost.trn = predict.boosting(boost.model, train.known)
                                                                                                                                                               #trn error
                                                                                                                                                               pred.boost.trn$error
                                                                                                                                                               
                                                                                                                                                               #predict test
                                                                                                                                                               
                                                                                                                                                               boost.pred.tst <- predict.boosting(boost.model,newdata = test.known)
                                                                                                                                                               
                                                                                                                                                               #test error
                                                                                                                                                               boost.pred.tst$error
                                                                                                                                                               c.m.boost <- boost.pred.tst$confusion
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               #### The training error for boosting can be given by:
                                                                                                                                                               b.t = pred.boost.trn$error
                                                                                                                                                               1-b.t
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               #### The testerror for boosting can be given by:
                                                                                                                                                               b.tst = boost.pred.tst$error
                                                                                                                                                               1-b.tst
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ##Table 7: Test accuracy for boosting model
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               library(kableExtra)
                                                                                                                                                               library(knitr)
                                                                                                                                                               Model <- c('Bagging having all predictors')
                                                                                                                                                               Method <- c('Train','Test')
                                                                                                                                                               Accuracy <- c(1-b.t, 1-b.tst)
                                                                                                                                                               temp <- as.data.frame(cbind(Model, Method, Accuracy))
                                                                                                                                                               kable(temp, align="r")
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               In Table 6, overall accuracy for test data is 78%, which is quite lower than random forest model and bagging model. The class S054 have the lowest class specific accuracy of 42.86%, originally it had 14 reps and it predicted only 6. Moreover, we can see most of the class specific accuracy is low. Hence we can conclude that multinomial logistics model performance tends to decrease for the response variable with more than 2 classes. Next we will look how LDA performs, since it is a popular method for multi-class classification problems with more than 2 classes.
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ####Viii :SVM Full model
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE,include=FALSE}
                                                                                                                                                               library(e1071)
                                                                                                                                                               ## linear
                                                                                                                                                               train.dataset=train.known
                                                                                                                                                               test.dataset= test.known
                                                                                                                                                               set.seed(702)
                                                                                                                                                               #tune function to select best cost value
                                                                                                                                                               #tune.out1 = tune(svm, subject ~ .,
                                                                                                                                                               #                data = train.dataset,
                                                                                                                                                               #                kernel = "linear",
                                                                                                                                                               #                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
                                                                                                                                                               #summary(tune.out1)#best cost parameter obtained is 0.1
                                                                                                                                                               svm.linear = svm(subject ~ ., data = train.dataset,kernel = "linear",cost = 0.1)
                                                                                                                                                               #train.dataset
                                                                                                                                                               pTrain4.linear = predict(svm.linear, train.dataset)
                                                                                                                                                               tTrain4.linear <- table(pTrain4.linear,train.dataset$subject)
                                                                                                                                                               aTrain4.linear<- (sum(diag(tTrain4.linear)))/sum(tTrain4.linear)
                                                                                                                                                               #aTrain4.linear#=0.974234
                                                                                                                                                               #confusionMatrix(data=pTrain4.linear, reference = train.dataset$subject)
                                                                                                                                                               #test.dataset
                                                                                                                                                               pTest4.linear = predict(svm.linear, test.dataset)
                                                                                                                                                               tTest4.linear <- table(pTest4.linear,test.dataset$subject)
                                                                                                                                                               aTest4.linear<- (sum(diag(tTest4.linear)))/sum(tTest4.linear)
                                                                                                                                                               #aTest4.linear#=0.8529412
                                                                                                                                                               #confusionMatrix(data=pTest4.linear, reference = test.dataset$subject)
                                                                                                                                                               
                                                                                                                                                               ## polynomial
                                                                                                                                                               set.seed(702)
                                                                                                                                                               #tune.out2 = tune(svm, subject ~ .,
                                                                                                                                                               #                data = train.dataset, kernel = "polynomial",
                                                                                                                                                               #                ranges = list(cost = 10^seq(-2, 1, by = 0.25),
                                                                                                                                                               #                              degree = c(1,2, 3, 4)))
                                                                                                                                                               #summary(tune.out2)#best cost value obtained 3.162278 and degree 1
                                                                                                                                                               svm.poly = svm(subject ~ ., data = train.dataset, kernel = "polynomial",degree = 1,cost = 3.1622)
                                                                                                                                                               #train.dataset
                                                                                                                                                               pTrain4.poly<-predict(svm.poly,train.dataset)
                                                                                                                                                               tTrain4.poly<-table(pTrain4.poly,train.dataset$subject)
                                                                                                                                                               aTrain4.poly<-(sum(diag(tTrain4.poly)))/sum(tTrain4.poly)
                                                                                                                                                               #aTrain4.poly#=0.974234
                                                                                                                                                               #confusionMatrix(data=pTrain4.poly,reference = train.dataset$subject)
                                                                                                                                                               #test.dataset
                                                                                                                                                               pTest4.poly<-predict(svm.poly,test.dataset)
                                                                                                                                                               tTest4.poly<-table(pTest4.poly,test.dataset$subject)
                                                                                                                                                               aTest4.poly<-(sum(diag(tTest4.poly)))/sum(tTest4.poly)
                                                                                                                                                               #aTest4.poly#=0.8529412
                                                                                                                                                               #confusionMatrix(data=pTest4.poly,reference = test.dataset$subject)
                                                                                                                                                               
                                                                                                                                                               ## radial
                                                                                                                                                               set.seed(702)
                                                                                                                                                               #tune.out3 = tune(svm, subject ~ .,
                                                                                                                                                               #                data = train.dataset, kernel = "radial",
                                                                                                                                                               #                ranges = list(cost = 10^seq(-2, 1, by = 0.25),
                                                                                                                                                               #                              gamma = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
                                                                                                                                                               #summary(tune.out3)#best cost value obtained is 10 and gamma value 0.01
                                                                                                                                                               set.seed(702)
                                                                                                                                                               svm.radial = svm(subject ~ .,data = train.dataset,kernel = "radial",gamma =0.01, cost = 10)
                                                                                                                                                               #train.dataset
                                                                                                                                                               pTrain4.radial<-predict(svm.radial,train.dataset)
                                                                                                                                                               tTrain4.radial<-table(pTrain4.radial,train.dataset$subject)
                                                                                                                                                               aTrain4.radial<-(sum(diag(tTrain4.radial)))/sum(tTrain4.radial)
                                                                                                                                                               #aTrain4.radial#=0.993036
                                                                                                                                                               #confusionMatrix(data=pTrain4.radial,reference = train.dataset$subject)
                                                                                                                                                               #test.dataset
                                                                                                                                                               pTest4.radial<-predict(svm.radial,test.dataset)
                                                                                                                                                               tTest4.radial<-table(pTest4.radial,test.dataset$subject)
                                                                                                                                                               aTest4.radial<-(sum(diag(tTest4.radial)))/sum(tTest4.radial)
                                                                                                                                                               #aTest4.radial#=0.8882353
                                                                                                                                                               svm.best<-confusionMatrix(data=pTest4.radial,reference = test.dataset$subject)
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               ####Table 8: Test accuracy for SVM full model
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               library(kableExtra)
                                                                                                                                                               library(knitr)
                                                                                                                                                               Models <- c('SVM using linear kernel','SVM using linear kernel','SVM using poly kernel','SVM using poly kernel','SVM using radial kernel','SVM using radial kernel')
                                                                                                                                                               Methods <- c('Train Accuray','Test Accuracy')
                                                                                                                                                               Accuracy <- c(aTrain4.linear,aTest4.linear,aTrain4.poly,aTest4.poly,aTrain4.radial,aTest4.radial)
                                                                                                                                                               temp <- as.data.frame(cbind(Models,Methods,Accuracy))
                                                                                                                                                               kable(temp, align="r")
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ####Ix: SVM having H and UD:
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE,include=FALSE}
                                                                                                                                                               
                                                                                                                                                               ## radial
                                                                                                                                                               set.seed(702)
                                                                                                                                                               #tune.outHUD3 = tune(svm,subject ~ H.period + DD.period.t + H.t + DD.t.i + H.i + DD.i.e + H.e + DD.e.five + H.five + DD.five.Shift.r + H.Shift.r + DD.Shift.r.o + H.o + DD.o.a + H.a + DD.a.n + H.n + DD.n.l + H.l + DD.l.Return + H.Return,
                                                                                                                                                               #                data = train.dataset, kernel = "radial",
                                                                                                                                                               #                ranges = list(cost = 10^seq(-2, 1, by = 0.25),
                                                                                                                                                               #                              gamma = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
                                                                                                                                                               #summary(tune.outHUD3)#best cost value obtained is 10 and gamma value 0.01
                                                                                                                                                               set.seed(702)
                                                                                                                                                               svm.radialHUD = svm(subject ~ H.period + UD.period.t + H.t + UD.t.i + H.i + UD.i.e + H.e + UD.e.five + H.five + UD.five.Shift.r + H.Shift.r + UD.Shift.r.o + H.o + UD.o.a + H.a + UD.a.n + H.n + UD.n.l + H.l + UD.l.Return + H.Return, data = train.dataset, kernel = "radial",
                                                                                                                                                                                   gamma = 0.01,
                                                                                                                                                                                   cost = 10)
                                                                                                                                                               #train.dataset
                                                                                                                                                               pTrain4.radialHUD<-predict(svm.radialHUD,train.dataset)
                                                                                                                                                               tTrain4.radialHUD<-table(pTrain4.radialHUD,train.dataset$subject)
                                                                                                                                                               aTrain4.radialHUD<-(sum(diag(tTrain4.radialHUD)))/sum(tTrain4.radialHUD)
                                                                                                                                                               #aTrain4.radialHUD#0.9895543
                                                                                                                                                               #confusionMatrix(data=pTrain4.radialHUD,reference = train.dataset$subject)
                                                                                                                                                               #test.dataset
                                                                                                                                                               pTest4.radialHUD<-predict(svm.radialHUD,test.dataset)
                                                                                                                                                               tTest4.radialHUD<-table(pTest4.radialHUD,test.dataset$subject)
                                                                                                                                                               aTest4.radialHUD<-(sum(diag(tTest4.radialHUD)))/sum(tTest4.radialHUD)
                                                                                                                                                               #aTest4.radialHUD#0.8823529
                                                                                                                                                               svm.small <- confusionMatrix(data=pTest4.radialHUD,reference = test.dataset$subject)
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               SVM with kernel value radial performed well compared to SVM kernel values linear and poly. There is not much difference between model accuracy of SVM linear and SVM radial, hence let us look at the class specific accuracy of predicted data using both model.
                                                                                                                                                               
                                                                                                                                                               ####Table 9: Test accuracy for SVM having H and UD:
                                                                                                                                                               
                                                                                                                                                               ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                               library(kableExtra)
                                                                                                                                                               library(knitr)
                                                                                                                                                               Models <- c('SVM using radial kernel')
                                                                                                                                                               Accuracy <- c('Train','Test set')
                                                                                                                                                               Methods <- c(aTrain4.radialHUD,aTest4.radialHUD)
                                                                                                                                                               temp <- as.data.frame(cbind(Models,Methods,Accuracy))
                                                                                                                                                               kable(temp, align="r")
                                                                                                                                                               
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               ###Selection of model:
                                                                                                                                                               Base on model performance (accuracy rate), our recommended model is the random forest. This is because the model possesses high overall predictive accuracy (96.2%) and subject specific prediction of individual reps are very high. The subject specific accuracy in the random forest model have majority predicting 100% of the reps in each subject. Also, considering subjects with smaller reps, the model predicted greater proportion for subjects with smaller reps. It can be summarized infollowing points:
                                                                                                                                                                                                                                                                                                                              .Random Forest is intrinsically suited for multiclass problems
                                                                                                                                                                                                                                                                                                                            .Over all model accuracy is good.
                                                                                                                                                                                                                                                                                                                            .Class specific accuracy is good.
                                                                                                                                                                                                                                                                                                                            .Class specific accuracy for classes with fewer reps is good.
                                                                                                                                                                                                                                                                                                                            .We can use huge predictors as they reduces overfitting and is therefore more accurate.
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            #predictions on unknown data using LDA having H and UD predictors:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            unknown = read.csv('unknown.csv')
                                                                                                                                                                                                                                                                                                                            original.unknown <- unknown
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            unknown = unknown[,-2]#remove rep
                                                                                                                                                                                                                                                                                                                            #make unknown data having only H and DD predictors to predict using best LDA model we obtained above
                                                                                                                                                                                                                                                                                                                            unknown = unknown[,-c(1)]
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            #head(unknown)
                                                                                                                                                                                                                                                                                                                            predict.unknown = (predict(lda.ud.hh, unknown))$class
                                                                                                                                                                                                                                                                                                                            table(predict.unknown)
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            #### The maximum reps on unknown data
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r}
                                                                                                                                                                                                                                                                                                                            which.max(table(predict.unknown))
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            #Appendice
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Class specific performances on test data using Multinom() function i.e GLM metthod:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.multi <- cbind(tst.cnt[,-1],diag(c.m.tst))
                                                                                                                                                                                                                                                                                                                            colnames(comp.multi) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.multi <- data.frame(comp.multi)
                                                                                                                                                                                                                                                                                                                            t <- comp.multi$Correctly.predicted.by.the.model/comp.multi$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.multi <- data.frame(comp.multi,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.multi) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.multi, align="r")
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for GLM
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.glm = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.glm, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Class specific performances on test data for LDA having only H predictors:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.lda <- cbind(tst.cnt[,-1],diag(c.m.t.h))
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda)
                                                                                                                                                                                                                                                                                                                            t <- comp.lda$Correctly.predicted.by.the.model/comp.lda$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.lda, align="r")
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for LDA only H predictors
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.glm = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.glm, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Class specific performances on test data for LDA having only H and UD predictors:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.lda <- cbind(tst.cnt[,-1],diag(c.m.t.ud))
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda)
                                                                                                                                                                                                                                                                                                                            t <- comp.lda$Correctly.predicted.by.the.model/comp.lda$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.lda, align="r")
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for LDA having only H and UD predictors:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.glm = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.glm, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Class specific performances on test data for bagging can be given by:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.bag <- cbind(tst.cnt[,-1],diag(c.m.bag.tst))
                                                                                                                                                                                                                                                                                                                            colnames(comp.bag) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.bag <- data.frame(comp.bag)
                                                                                                                                                                                                                                                                                                                            t <- comp.bag$Correctly.predicted.by.the.model/comp.bag$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.bag <- data.frame(comp.bag,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.bag) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.bag, align="r")
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for bagging:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.glm = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.glm, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Class specific performances on test data for random forest can be given by:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.rf <- cbind(tst.cnt[,-1],diag(c.m.rf.tst))
                                                                                                                                                                                                                                                                                                                            colnames(comp.rf) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.rf <- data.frame(comp.rf)
                                                                                                                                                                                                                                                                                                                            t <- comp.rf$Correctly.predicted.by.the.model/comp.rf$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.rf <- data.frame(comp.rf,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.rf) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.rf, align="r")
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for random forest can be given by:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.glm = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.glm, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Class specific performances on test data for boosting can be given by:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.boost <- cbind(tst.cnt[,-1],diag(c.m.boost))
                                                                                                                                                                                                                                                                                                                            colnames(comp.boost) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.boost <- data.frame(comp.boost)
                                                                                                                                                                                                                                                                                                                            t <- comp.boost$Correctly.predicted.by.the.model/comp.boost$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.boost <- data.frame(comp.boost,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.boost) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.boost, align="r")
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for boosting can be given by:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.glm = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.glm, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ##class cpecific performance of PCA on LDA
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ##class cpecific performance of PCA + LDA
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.lda <- cbind(tst.cnt.pca[,-1],diag(c.m.tst.pca))
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda)
                                                                                                                                                                                                                                                                                                                            t <- comp.lda$Correctly.predicted.by.the.model/comp.lda$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.lda, align="r")
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for pca + lda
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.pca = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.pca, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ##class cpecific performance of full model SVM
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ##class cpecific performance of PCA + LDA
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.lda <- cbind(tst.cnt[,-1],diag(svm.best$table))
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda)
                                                                                                                                                                                                                                                                                                                            t <- comp.lda$Correctly.predicted.by.the.model/comp.lda$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.lda, align="r")
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for full model SVM
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.pca = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.pca, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ##class cpecific performance of small model (H + UD) SVM
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE}
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ##class cpecific performance of PCA + LDA
                                                                                                                                                                                                                                                                                                                            library(knitr)
                                                                                                                                                                                                                                                                                                                            library(kableExtra)
                                                                                                                                                                                                                                                                                                                            comp.lda <- cbind(tst.cnt[,-1],diag(svm.small$table))
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) <- c('Number of repetitions in each class on test data','Correctly predicted by the model')
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda)
                                                                                                                                                                                                                                                                                                                            t <- comp.lda$Correctly.predicted.by.the.model/comp.lda$Number.of.repetitions.in.each.class.on.test.data*100
                                                                                                                                                                                                                                                                                                                            comp.lda <- data.frame(comp.lda,t)
                                                                                                                                                                                                                                                                                                                            colnames(comp.lda) = c('Number of repetitions in each class on test data','Correctly predicted by the model','Accuracy in each class')
                                                                                                                                                                                                                                                                                                                            kable(comp.lda, align="r")
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ####Accuracy in each class vs subject for small (H + UD)  model SVM
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```{r,echo=FALSE,warning=FALSE,message=FALSE,fig.width=10}
                                                                                                                                                                                                                                                                                                                            #class specific accuracy vs subject graph
                                                                                                                                                                                                                                                                                                                            library(ggplot2)
                                                                                                                                                                                                                                                                                                                            library('reshape2')
                                                                                                                                                                                                                                                                                                                            temp.pca = as.data.frame(cbind(t,unique(known$subject)))
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            melted = melt(temp.pca, id.vars="t")
                                                                                                                                                                                                                                                                                                                            melted$value = paste(unique(known$subject))
                                                                                                                                                                                                                                                                                                                            ggplot(data=melted, aes(x=reorder(value,-t), y=t, group=variable)) + geom_point() + labs(x = "Subject",y = "Accuracy in each class")+ theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=06))+ theme(legend.position="none") 
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ```
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            ###References:
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            http://www.biometric-solutions.com/keystroke-dynamics.html
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                              https://en.wikipedia.org/wiki/Keystroke_dynamics
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            http://www.cs.cmu.edu/~keystroke/
                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                              https://www.computereconomics.com/article.cfm?id=1181
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                              https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                              http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                              Text books: 
                                                                                                                                                                                                                                                                                                                              Introduction to statistical Learning in R
                                                                                                                                                                                                                                                                                                                            A Handbook of Statisticsl Analysis using R
                                                                                                                                                                                                                                                                                                                            R graphics Cookbook
                                                                                                                                                                                                                                                                                                                            SDSU SD stat 702 online course material
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                            