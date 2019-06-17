## Example 2: based on an example from the wikipedia page:
# https://en.wikipedia.org/wiki/Confusion_matrix
install.packages("boot")
library(boot)
library(simpleboot)
library(ggplot2)
pval<-numeric()
cil<-numeric()
ciu<-numeric()
x<-factor()
y<-factor()


obs<-148
trp1<-62
fap1<-0
fan1<-86
trp2<-141
fap2<-0
fan2<-7

tp1<-c(rep(1,trp1),rep(0,(obs-trp1)))
fp1<-c(rep(1,(fap1+trp1)),rep(0,(obs-(trp1+fap1))))
fn1<-c(rep(1,(fan1+trp1)),rep(0,(obs-(trp1+fan1))))
tp2<-c(rep(1,trp2),rep(0,(obs-trp2)))
fp2<-c(rep(1,(fap2+trp2)),rep(0,(obs-(trp2+fap2))))
fn2<-c(rep(1,(fan2+trp2)),rep(0,(obs-(trp2+fan2))))
data<-data.frame(tp1,fp1,fn1,tp2,fp2,fn2)

computeFscorediff<-function(data,indices){
  sample<-data[indices,]                
  p1<-(sum(sample$tp1)/sum(sample$fp1))
  p2<-(sum(sample$tp2)/sum(sample$fp2))
  r1<-(sum(sample$tp1)/sum(sample$fn1))
  r2<-(sum(sample$tp2)/sum(sample$fn2))
  fscore1<-(2*p1*r1/(p1+r1))
  fscore2<-(2*p2*r2/(p2+r2))
  (fscore1-fscore2)}

b<-boot(data,computeFscorediff,10000)
print(b)

# Scores

p1<-(sum(data$tp1)/sum(data$fp1))
p2<-(sum(data$tp2)/sum(data$fp2))
r1<-(sum(data$tp1)/sum(data$fn1))
r2<-(sum(data$tp2)/sum(data$fn2))
fscore1<-(2*p1*r1/(p1+r1))
fscore2<-(2*p2*r2/(p2+r2))
(sum(data$tp1)/sum(data$fp1))
(sum(data$tp2)/sum(data$fp2))
(sum(data$tp1)/sum(data$fn1))
(sum(data$tp2)/sum(data$fn2))
(2*p1*r1/(p1+r1))
(2*p2*r2/(p2+r2))
# Nonparametric Bootstrap Confidence Intervals -  the basic bootstrap interval

conf<-boot.ci(boot.out=b,type=c("basic"))
print(conf)

b.under.H0 <- b$t - mean(b$t)
p<-mean(abs(b.under.H0) >= abs(b$t0))

conf$basic

print(p)


pval<-c(pval,p)
cil<-c(cil,conf$basic[4])
ciu<-c(ciu,conf$basic[5])
remove(b,b.under.H0)

plot<-data.frame(pval,cil,ciu)


write.csv(plot,file="3.bootstrap_pvalue_Compare.csv")

##################################################################
##PLots and graphs from here
# Plot pairwise comparisons block and color by significance level
# https://stackoverflow.com/questions/8391783/visualize-critical-values-pairwise-comparisons-from-posthoc-tukey-in-r
# https://stackoverflow.com/questions/33644034/how-to-visualize-pairwise-comparisons-with-ggplot2 
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
plot<-read.csv("pvalue_combined_excluded_c02.csv",header=TRUE,sep=",")

#Simple matrix image

image1<-ggplot(plot, aes(y=x, x=y, label=pval, fill=pval)) + geom_tile() + geom_text()+ labs(x="ATC Class", y="") + theme( axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank()) 
image1
ggsave(filename="4.Plot.png", plot=image1)

#Using the Bonferroni correction for 15 comparisons from 6 runs, the p value has to be below 0.05/15 = 0.0167

plot$significance<-cut(plot$pval,c(0,0.0167,1),include.lowest = TRUE,labels=c("significant","not significant"))
image2<-ggplot(plot, aes(y=x, x=y, fill=significance)) + theme( axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank())  +geom_tile()+scale_fill_manual(values = c("orange", "green")) + labs(x="ATC Class", y="")
image2
ggsave(filename="5.Plot_2colored.png", plot=image2)


# The below graph will look something like below - https://i.stack.imgur.com/vs5yx.png 
# Here the diff i refer to represent the original difference between F measures, i think it should be in basic$ else just use the original compute Fscore function. 
plot$RunXvsRunY<-rownames(plot)
plot
image4 <-ggplot(plot, aes(colour=cut(plot$pval,c(0,0.05,1),include.lowest = TRUE, label=c("Sig","Non-Sig")))) + 
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(plot$y, ymin=cil, ymax=ciu), width=0.2) + 
  geom_point(aes(plot$y, plot$point)) + labs(colour="Significance", x="ATC Group", y="Difference in F1-measure")
image4
ggsave(filename="7.Plot.errorbar.png", plot=image4)
