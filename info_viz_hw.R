library(ggplot2)
library(ggrepel)
filename <- file.choose()
filename 
crime<- read.csv(filename, header = T)
View(crime)

#Create scatterplot matrix of all variables
g1 <- pairs(crime[,2:9], panel = panel.smooth)
g1


crime <- subset(crime, crime$state != 'District of Columbia')

#Create bubble chart
install.packages(('ggthemes'))
library(ggthemes)
p6 <- ggplot(crime, aes(x = crime$murder, y = crime$burglary, size = crime$population, fill = 'red')) +
  theme_economist() +
  scale_fill_economist() +
  geom_point(shape = 21) +
  ggtitle("MURDERS VERSUS BULRGLARIES IN THE UNITED STATES") +
  labs(x = "Murders (per 100.000 population)", y = "Burglaries (per 100.000 population)",
       size = "Population") +
  scale_x_continuous(breaks = seq(1, 10,2)) +
  scale_size(range = c(1, 40)) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 9))+
  geom_text(aes(label=crime$state), size=3)
p6
  
  
  
filename <- file.choose()
filename 
birth<- read.csv(filename, header = T)
 

#Create a histogram
hist(birth$X2008, breaks=5)
  
  
#Create density plot
birth2008 <- birth$X2008[!is.na(birth$X2008)]
x <- density(plot(birth2008))
plot(x)
polygon(x, col="#821122", border="#cccccc")


#extra visuals 
g2 <-ggplot(crime, aes(x=murder)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + ggtitle('Murder Distribution with Density')
g2


g4 <- ggplot(crime, aes(x=crime$state,y=crime$murder, col = 'red')) +
  geom_bar(stat = "identity") +
  xlab('State') +
  ylab('Murder Rate') +
  ggtitle('Murder Rate by State')+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g4



g6 <- ggplot(crime, aes(x=murder, y=robbery, fill = 'red', label = crime$state)) + 
  geom_point(color = 'red')+
  geom_smooth(method=lm, se=FALSE) + 
  geom_text_repel(label = crime$state)+ 
  theme_classic(base_size = 16)
g6




g7 <- ggplot(birth, aes(x=birth$X1960, y=birth$X1961, fill = 'red', label = birth$Country)) + 
  geom_point(color = 'red')+
  geom_smooth(method=lm, se=FALSE) + 
  geom_text_repel(label = birth$Country)+ 
  theme_classic(base_size = 16)
g7




  