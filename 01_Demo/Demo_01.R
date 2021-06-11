x <- 3
numlist <- c(0,1,2,3,4,5,6,7,8,9)

# 15.2.3: Read and Write Using R ----------------------------------------------

demo_table <- read.csv(file='Data/demo.csv',check.names=F,stringsAsFactors = F)

# install.packages("package") requires parentheses but install.library does not
install.library(jsonlite)
library(jsonlite)

demo_table2 <- fromJSON(txt='Data/demo.json')
demo_table3 <- read.csv(file='Data/demo2.csv',check.names=F,stringsAsFactors = F)
demo_table2 <- fromJSON(txt='Data/demo.json')nrow()

# 15.2.4: Select Data in R -----------------------------------------------------
x <- c(3,3,2,2,5,5,8,8,9)
x[3]
demo_table[3,"Year"]
demo_table[3,3]
demo_table$"Vehicle_Class"
demo_table$"Vehicle_Class"[2]

# filter ----------------------------------------------------------------------
filter_table <- demo_table2[demo_table2$price > 10000,]
filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) #filter by price and drivetrain
filter_table3 <- demo_table2[("clean" %in% demo_table2$title_status) & (demo_table2$price > 10000) & (demo_table2$drive == "4wd"),]

# sample function --------------------------------------------------------------
sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"),4)
num_rows <- 1:nrow(demo_table)
sample_rows <- sample(num_rows, 3)
demo_table[sample_rows,]
# combine all three
demo_table[sample(1:nrow(demo_table), 3),]

# 15.2.5: Transform, Group, Reshape --------------------------------------------
# tidyverse package contains dplyr (need this to use pipe operator %>%), tidyr, ggplot2
library(tidyverse)

# pipe operator chains together functions
# mutate
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) #add columns to original data frame

# group_by and summarize
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer), .groups = 'keep') #create summary table
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n(), .groups = 'keep') #create summary table with multiple columns

# gather to reshape dataset (shrink in)
demo_table3 <- read.csv('01_Demo/demo2.csv',check.names = F,stringsAsFactors = F)
long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

# spread to expand back out
wide_table <- long_table %>% spread(key="Metric",value="Score")
all.equal(wide_table,table)
table <- demo_table3[,order(colnames(wide_table))]
table <- demo_table3[,(colnames(wide_table))]

# ------------------------------------------------------------------------------
# 15.3.1: ggplot2
# mpg is a built in R dataset
head(mpg)

# 15.3.2:  bar plots ----------------------------------------------
plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot

mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n(), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") #plot bar plot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") + #plot a boxplot with labels
  theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

# 15.3.4: line plots -----------------------------------------------------------------
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2
plt + geom_line()
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) #add line plot with labels

# point plots ------------------------------------------------------------------
plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv,size=cty)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive", size="City Fuel-Efficiency (MPG)") #add scatter plot with multiple aesthetics
plt + geom_point(aes(size=cty)) + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive",size="City Fuel-Efficiency (MPG)") #add scatter plot with multiple aesthetics

# 15.3.5:  box plots --------------------------------------------------------------------
plt <- ggplot(mpg,aes(y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot(fill="lightblue", color = "darkgreen", lty="dashed") + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot and rotate x-axis labels 45 degrees
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot(fill="lightblue") + #add boxplot and fill color 
  theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
  geom_point() #overlay scatter plot on top
plt + geom_boxplot(fill="white",aes(color=manufacturer),linetype="dashed") + theme(axis.text.x=element_text(angle=45,hjust=1))  #add boxplot and rotate x-axis labels 45 degrees

# 15.3.6:  heat maps --------------------------------------------------------------------
mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels
mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) #import dataset into ggplot2
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + #add heatmap with labels > 
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) #rotate x-axis labels 90 degrees

# 15.3.7:  Add Layers to Plots ---------------------------------------------------------
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot(fill="white",aes(color=manufacturer)) + theme(axis.text.x=element_text(angle=45,hjust=1)) + #add boxplot and rotate x-axis labels 45 degrees
geom_point() #overlay scatter plot on top

# mapping argument uses aes() to identify variables, data argument can be used to provide new input data-------
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") #add scatter plot

# overlay with error bars ------------------------------------------------------------
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ), .groups = 'keep')
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + #add scatter plot with labels
  geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars

# Faceting ---- added to the end of plotting statement -------------------------------
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) #convert to long format
head(mpg_long)
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot with labels rotated 45 degrees

# now add facet_wrap ------
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

# Skill Drill --- use a different variable for the facet_wrap -----------------------------------------------
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(class)) + #create multiple boxplots, one for each class
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(year)) + #create multiple boxplots, one for each year
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

# Skill Drill --- use two different variables for the facet_wrap -------------------------------------------
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(year,MPG_Type)) + #create multiple boxplots, one for each year & MPG type
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

# 15.4.4: Qualitative Test for Normality --------------------------------------------------------------------
head(mtcars)
ggplot(mtcars,aes(x=wt)) + geom_density() #visualize distribution using density plot

# 15.6.1: random sample from population ---- sample_n() function --------------------------------------------
population_table <- read.csv(file='Data/used_car_data.csv',check.names=F,stringsAsFactors = F)
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table, aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

# 15.6.2: One-Sample t-Test -----------------------------------------------------------------------------------
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven)))  #compare sample versus population means

# 15.6.3: Two-sample t-test -----------------------------------------------------------------------------------
sample_table <- population_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50) #generate another 50 randomly sampled data points

t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) #compare means of two samples

# 15.6.4: Two-sample t-test (paired) --------------------------------------------------------------------------
mpg_data <- read.csv('Data/mpg_modified.csv') #import dataset
mpg_1999 <- mpg_data %>% filter(year==1999) #select only data points where the year is 1999
mpg_2008 <- mpg_data %>% filter(year==2008) #select only data points where the year is 2008

t.test(mpg_1999$hwy,mpg_2008$hwy,paired=T) #compare the mean difference between two samples

# 15.6.5 ANOVA test -------------------------------------------------------------------------------------------
# clean data - convert numeric column to factor
head(mtcars)
mtcars_filt <- mtcars[,c("hp","cyl")] #filter columns from mtcars dataset
head(mtcars_filt)
# use factor() to convert numeric to categorical
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric column to factor

# compare means
aov(hp ~ cyl,data=mtcars_filt) #compare means across multiple levels

# compare means w p-values - use summary()
summary(aov(hp ~ cyl,data=mtcars_filt))  #compare means across multiple levels

# 15.7.1 ------ correlation, variance, covariance (matrices) -------------------------------------------------
# example 1:
head(mtcars)
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() #create scatter plot
cor(mtcars$hp,mtcars$qsec) #calculate correlation coefficient, "r"

# example 2:
used_cars <- read.csv('Data/used_car_data.csv', stringsAsFactors = F) #read in dataset
head(used_cars)
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price)) #import dataset into ggplot2
plt + geom_point() #create a scatter plot
cor(used_cars$Miles_Driven,used_cars$Selling_Price) #calculate correlation coefficient

# example 3: correlation matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) #convert data frame into numeric matrix
cor(used_matrix)

# 15.7.2: Single Linear Regression ---------------------------------------------------------------
lm(qsec ~ hp, mtcars) #create linear model lm (y ~ x, data)
summary(lm(qsec~hp,mtcars)) #summarize linear model

# Create linear model and plot
model <- lm(qsec ~ hp, mtcars) # Create linear model
yvals <- model$coefficients['hp']*mtcars$hp +
  model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

# 15.7.3:  Multiple Linear Regression ---------------------------------------------------------------
lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) #generate multiple linear regression model
summary( lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) #generate summary statistics

# 15.8.1:  chi-square test to compare distribution of frequencies across two groups -----------------
# First generate a contingency (frequency) table
head(mpg)
table(mpg$class,mpg$year) #generate contingency table
tbl <- table(mpg$class,mpg$year) #generate contingency table
chisq.test(tbl) #compare categorical distributions
# practice
head(mtcars)
tbl2 <- table(mtcars$cyl,mtcars$qsec) #generate contingency table
chisq.test(tbl2) #compare categorical distributions
