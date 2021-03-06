## Example 2: based on an example from the wikipedia page:
# https://en.wikipedia.org/wiki/Confusion_matrix
library(boot)
library(ggplot2)

pval<-numeric()
cil<-numeric()
ciu<-numeric()
x<-factor()
y<-factor()

computeFscorediff<-function(data,indices){
  sample<-data[indices,]                
  p1<-(sum(sample$Predicted1)/sum(sample$Predicted1))
  p2<-(sum(sample$Predicted2)/sum(sample$Predicted2))
  r1<-(sum(sample$Predicted1)/sum(sample$Actual))
  r2<-(sum(sample$Predicted2)/sum(sample$Actual))
  fscore1<-(2*p1*r1/(p1+r1))
  fscore2<-(2*p2*r2/(p2+r2))
  (fscore1-fscore2)}


#Enter results into here or load file csv in this format as sample.

input<-read.csv("Input.csv",header=TRUE,sep=",")
for(i in 1:4)
{
  for (j in (i+1):4)
  {
    x<-c(x,i)
    y<-c(y,j)
    TP1 <-input$TP[i]
    FN1 <-input$FN[i]
    TP2 <-input$TP[j]
    FN2 <-input$FN[j]
    AP1 <-input$Actual[i]
    AP2 <-input$Actual[j]
sample <-
  data.frame(Predicted1 = c(rep(1,    TP1 + 0),
                           rep(0,   FN1 + 0)),
             Actual1    = c(rep(c(1, 0), times = c(AP1, 0)),
                           rep(c(1, 0), times = c(0, 0))),
             Predicted2 = c(rep(1,   TP2  + 0),
                           rep(0,   FN2 + 0)),
             Actual2    = c(rep(c(1, 0), times = c(AP2, 0)),
                           rep(c(1, 0), times = c(0, 0))),
             stringsAsFactors = FALSE)




b<-boot(sample,computeFscorediff,10000)
print(b)
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

  }
}

x<-c(sample$Predicted1)
y<-c(sample$Predicted2)
plot<-data.frame(x,y,pval,cil,ciu)
plot$x<-as.factor(plot$x)
plot$y<-as.factor(plot$y)

write.csv(plot,file="3.bootstrap_pvalue.csv")


# Plot pairwise comparisons block and color by significance level
# https://stackoverflow.com/questions/8391783/visualize-critical-values-pairwise-comparisons-from-posthoc-tukey-in-r
# https://stackoverflow.com/questions/33644034/how-to-visualize-pairwise-comparisons-with-ggplot2 
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf


#Simple matrix image

image1<-ggplot(plot, aes(y=x, x=y, label=pval, fill=pval)) + geom_tile() + geom_text()
image1
ggsave(filename="4.Plot.png", plot=image1)

#Using the Bonferroni correction for 15 comparisons from 6 runs, the p value has to be below 0.05/15 = 0.0167

plot$significance<-cut(plot$pval,c(0,0.0167,1),include.lowest = TRUE,labels=c("significant","not significant"))
image2<-ggplot(plot, aes(y=x, x=y, fill=significance))  +geom_tile()+scale_fill_manual(values = c("orange", "green"))
image2
ggsave(filename="5.Plot_2colored.png", plot=image2)

#Better colored image# this is supposed to look like this - https://i.stack.imgur.com/i0IsQ.png of course with only two colors
# Calculate grid line positions manually.

x_grid_lines = seq(0.5, length(levels(plot$x)), 1)
y_grid_lines = seq(0.5, length(levels(plot$y)), 1)

image3<-ggplot(plot, aes(xmin=as.integer(x) - 0.5,
                         xmax=as.integer(x) + 0.5,
                         ymin=as.integer(y) - 0.5,
                         ymax=as.integer(y) + 0.5,
                         fill=significance)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  coord_cartesian(xlim=c(min(x_grid_lines), max(x_grid_lines)),
                  ylim=c(min(y_grid_lines), max(y_grid_lines))) +
  scale_x_continuous(breaks=seq(1, length(levels(plot$x))),
                     labels=levels(plot$x)) +
  scale_y_continuous(breaks=seq(1, length(levels(plot$x))),
                     labels=levels(dplot$y)) +
  scale_fill_manual(name="Critical Values", values=c("orange", "green")) +
  geom_rect() +
  geom_hline(yintercept=y_grid_lines, colour="grey40", size=0.15) +
  geom_vline(xintercept=x_grid_lines, colour="grey40", size=0.15) +
  theme(axis.text.y=theme_text(size=9)) +
  theme(axis.text.x=theme_text(size=9, angle=90)) +
  theme(title="Critical Values Matrix")
image3
ggsave(filename="6.Plot.png", plot=image3)

# The below graph will look something like below - https://i.stack.imgur.com/vs5yx.png 
# Here the diff i refer to represent the original difference between F measures, i think it should be in basic$ else just use the original compute Fscore function. 

plot$RunXvsRunY<-rownames(plot)
plot
image4 <-ggplot(plot, aes(colour=cut(plot$pval,c(0,0.05,1),include.lowest = TRUE, label=c("sig","Non-Sig")))) + 
  geom_hline(yintercept=0.05, lty="11", colour="grey30") +
  geom_errorbar(aes(plot$RunXvsRunY, ymin=cil, ymax=ciu), width=0.2) + 
  geom_point(aes(plot$RunXvsRunY, diff)) + labs(colour="Signif")
image4
ggsave(filename="7.Plot.errorbar.png", plot=image4)
