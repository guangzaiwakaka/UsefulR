# library
library(multcompView)
library(agricolae)
data <- Pplot

model <- aov(PT~GT, data=data)
Results <- HSD.test(model,"GT", group=TRUE,console=TRUE,
                main="PSS for different GTnotyPTs")
Vmin = Results$means$Min
Vmax = Results$means$Max

label = data.frame(Results$groups)
order = label[order(label$trt),]


# A panel of colors to draw each group with the same color :
my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255),
             rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) ,
             rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),
             rgb(165,103,40,maxColorValue = 255))

#Draw the basic boxplot
a=boxplot(data$PT ~ data$GT, ylim=c(min(Vmin) , 1.1*max(Vmax)),
          col=my_colors[as.numeric(order[,3])] , 
          ylab="PSS" , xlab="GTnotyPT", main="PSS after 22d of inoculation")

#Write the letter over each box. Over is how high I want to write it.
over=0.04*max( a$stats[nrow(a$stats),] )

b = c(nlevels(data$GT):8)
b = b[-1]

#Add the labels
text( b, a$stats[nrow(a$stats),]+over, order[,3], 
      col=my_colors[as.numeric(order[,3])], cex = 1.1)





