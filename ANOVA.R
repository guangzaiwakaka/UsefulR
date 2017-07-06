
mydata <- EarlyPSS
##Method1 analysis by groups first then do the ANOVA
##aggregate
##We can use the aggregate() function to apply a specified command for groups
##Baisic structure of the command is aggregate(Variable, by=list(Grouping1,..), Function)

##mean for each genotype
aggregate(mydata$PSS,by=list(Genotype=mydata$genotype),mean)

##range for each genotype
aggregate(mydata$PSS,by=list(Genotype=mydata$genotype),range)

##summary for each genotype
aggregate(mydata$PSS,by=list(Genotype=mydata$genotype),summary)

##sd for each genotype
aggregate(mydata$PSS,by=list(Genotype=mydata$genotype),sd)

##help of anova
help(aov)

##help of boxplot
help("boxplot")
## Default S3 method:
boxplot(x, ..., range = 1.5, width = NULL, varwidth = FALSE,
        notch = FALSE, outline = TRUE, names, plot = TRUE,
        border = par("fg"), col = NULL, log = "",
        pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
        horizontal = FALSE, add = FALSE, at = NULL)

##boxplot of the data
boxplot(mydata$PSS~mydata$genotype,xlab = "Genotype", ylab ="PSS")


##Ho: Mean PSS is the same for all genotypes

ANOVA1 = aov(mydata$PSS~mydata$genotype)
ANOVA1
summary(ANOVA1)

##know all letter stored in ANOVA1
attributes(ANOVA1)

ANOVA1$coefficients

#help of TukeyHSD
help("TukeyHSD")

##ALL possible pairwise post-hoc analysis
TukeyHSD(ANOVA1)
plot(TukeyHSD(ANOVA1))


##Method2 Using a package called agricolae to do ANOVA

mydata <- EarlyPSS
View(mydata)
##install a package called agricolae
library(agricolae)

data(mydata)
View(mydata)

model<-aov(PSS~genotype, data=mydata)
out <- HSD.test(model,"genotype", group=TRUE,console=TRUE,
                main="PSS for different genotypes")
#stargraph
bar.group(out$groups,ylim=c(0,0.4),density=4,border="black")
help("bar.err")


#endgraph
out<-HSD.test(model,variation="SD","genotype", group=FALSE)
means<-out$means
means



##help of HSDtest
help("HSD.test")

##Example of HSD.test
##Example start
library(agricolae)
data(sweetpotato)
View(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
out <- HSD.test(model,"virus", group=TRUE,console=TRUE,
                main="Yield of sweetpotato\nDealt with different virus")
#stargraph
bar.group(out$groups,ylim=c(0,45),density=4,border="blue")
#endgraph
out<-HSD.test(model,"virus", group=FALSE)
means<-out$means
# Old version HSD.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
with(sweetpotato,HSD.test(yield,virus,df,MSerror, group=TRUE,console=TRUE,
                          main="Yield of sweetpotato. Dealt with different virus"))
##Example end


