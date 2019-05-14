library(dplyr)
library(ggplot2)

filename <- file.choose()
filename 
mydata <- read.csv(filename, header = T)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#1) What are the distributions of total sales for water color and drawing papers? Show this as a grouped boxplot (two boxplots in the same plot
d1 <- ggplot(mydata, aes(x = mydata$paper, y = mydata$total.sale, color = mydata$paper )) + geom_boxplot()+
  ggtitle('Total Sales Distribution by Paper Type')
d1


#2) Are total sales growing for each store over the years covered in the dataset? Show as a muli-line plot.
df1 <- data.frame(tapply(mydata$year, list(mydata$year,mydata$store), sum))
df1 <- add_rownames(df1, "year")
df1
d2 <-ggplot(df1, aes(df1$year, group=1)) +
  geom_line(aes(y = df1$Davenport, colour = "red"))+
  geom_line(aes(y = df1$Dublin, colour = "green"))+
  geom_line(aes(y = df1$Portland, colour = "yellow"))+
  geom_line(aes(y = df1$Syracuse, colour = "orange"))+
  xlab('Year ')+
  ylab('Total Sales')+
  ggtitle('Sales Growth Per Store')
d2



#3) 
d3 <- ggplot(mydata, aes(x=mydata$store,y=mydata$total.sale, fill = mydata$paper)) +
  geom_bar(stat = "identity") +
  xlab('Store Region') +
  ylab('Total Sales') +
  ggtitle('Sales By Region') +
  labs(fill='Type of Paper')
d3


#4) Each paper (watercolor and drawing) has different subtypes. For watercolor only, how are the total sales of the different paper types (column is paper.type) similar or different for each store?

watercolor <- data.frame(subset(mydata, mydata$paper =='watercolor'))
d4 <- ggplot(watercolor, aes(x=watercolor$store,y=watercolor$total.sale, fill = watercolor$paper.type)) +
  geom_bar(stat = "identity") +
  xlab('Store Region') +
  ylab('Total Sales') +
  ggtitle('Total Sales by Paper Type - Watercolor') +
  labs(fill='Type of Paper')
d4

#5) In the Davenport store, do the sales representatives (column is 'rep') tend to sell the same amounts (units) of water color and drawing paper?

Davenport <- data.frame(subset(mydata, mydata$store =='Davenport'))
d5 <- ggplot(Davenport, aes(x=Davenport$rep,y=Davenport$total.sale, fill = Davenport$paper)) +
  geom_bar(stat = "identity") +
  xlab('Sales Rep') +
  ylab('Total Sales') +
  ggtitle('Total Sales by Sales Rep in Davenport') +
  labs(fill='Type of Paper')
d5


#6) Over the years, does the ratio of units sold for water color and drawing paper stay the same? Is one growing while the other stays constant?
df2 <- data.frame(tapply(mydata$year, list(mydata$year,mydata$paper), sum))
df2 <- add_rownames(df2, "year")
df2$ratio_drawing <- with(df2, df2$drawing/(df2$drawing+df2$watercolor))
df2$ratio_watercolor <- with(df2, df2$watercolor/(df2$drawing+df2$watercolor))

d6 <- ggplot(df2, aes(df2$year, group=1)) +
  geom_line(aes(y = df2$ratio_watercolor, colour = "watercolor"))+
  geom_line(aes(y = df2$ratio_drawing, colour = "drawing"))+
  xlab('Year')+
  ylab('Total Sales')+
  ggtitle('Sales Ratio by Paper Type')
d6



multiplot(d1, d2, d3, d4, d5, d6, cols =2)

