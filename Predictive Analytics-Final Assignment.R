#import the data and the libraries
library(dplyr)
library(psych)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
data = read.csv("fish.csv", header = TRUE)
View(data)

#The structure of the dataset
str(data)
#Analysing the str of the dataset, we can see that "Species" is the only categorical variable, while the other 6 are continuous

#Now we rename the cloumns for better understanding
names(data)<- c("Species", "Weight","Length1", "Length2","Length3", "Height", "Width")

#Let us summarize the dataset
summary(data)

#Check if there are any missing values in the dataset
miss<-sum(is.na(data))
print(paste("Number of  missing values:-",miss))
#The answer is false, so there are no missing values in our dataset

#We again filter the values 0 from the dataset
fish<- data%>% filter(Weight>0,Length1>0,Length2>0,Length3>0,Height>0,Width>0)
fish$Species<-as.factor(fish$Species)
#Now we have a clean dataset with no NAN and 0 values

#The variable to be predicted(Dependent Variable) is Weight
#The independent variables(Predictors) are Length1,Length2, Length3, Height and Width

#Distribution of the dependent variable Weight
ggplot(fish,aes(x=Weight,fill=Species))+
  geom_histogram(alpha=0.5,col="red",bins=20)+scale_color_brewer(palette = "Dark2")+scale_fill_brewer(palette = "Dark2")+
  theme(panel.background = element_rect(fill="grey"))
  #Looking at the histogram, we can see that there are some outliers for the species PIKE.


#Diving further into data exploration
#We now check each independent and dependent variable according to the species
bp1<-ggplot(fish,aes(x=Species,y=Length1))+geom_boxplot()
bp2<-ggplot(fish,aes(x=Species,y=Length2))+geom_boxplot()
bp3<-ggplot(fish,aes(x=Species,y=Length3))+geom_boxplot()
bp4<-ggplot(fish,aes(x=Species,y=Height))+geom_boxplot()
bp5<-ggplot(fish,aes(x=Species,y=Width))+geom_boxplot()
bp6<-ggplot(fish,aes(x=Species,y=Weight))+geom_boxplot()

#Using ggarange function to visualize all boxplots in one window
ggarrange(bp1,bp2,bp3,bp4,bp5,bp6,labels=c("Length1","Length2","Length3","Height","Width","Weight"),ncol=2,nrow = 3)
#we can observe that the data is not normally distributed.
#Another important point is that each species has different mean "WEIGHT".

#Now we use density plot to visualize further the distribution of weight by species
#The vertical lines represent the mean of the weight of each species

#First we calculate the means of each fish species
fish.means <- fish %>%
  group_by(Species)%>%
  summarise(Mean_weight=mean(log(Weight)))

#Now we use density plot to visualize the data
ggplot(fish,aes(x=log(Weight),fill=Species,color=Species,group=Species))+
  geom_density(alpha=0.4,lwd=1)+
  geom_vline(data=fish.means,aes(xintercept = Mean_weight,color=Species),lwd=2,lty="dashed")+
  scale_x_continuous(limits = c(1.5,8.5))
#As the original values of "WEIGHT" are highly variable, the density graph could not be properly visualized.
#Thus, we have used the log of WEIGHT, in order to visualize the data through density graph
#The graph shows the different species-wise density and distribution of our dependent variable WEIGHT
#It further supports our claim that the variables are not normally distributed

#Checking global relationship between our depenent variable(Weight) and other independent variables
l1<-ggplot( data = fish,mapping = aes(x = Length1, y = Weight,col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")
l2<-ggplot( data = fish,mapping = aes(x = Length2, y = Weight,col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")
l3<-ggplot( data = fish,mapping = aes(x = Length3, y = Weight,col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")
h<-ggplot( data = fish,mapping = aes(x = Height, y = Weight,col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")

w<-ggplot( data = fish,mapping = aes(x = Width, y = Weight,col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")

l1
l2
l3
h
w

#We can see that between the three lengths and weight, there is a linear relationship
#However, for height and width, there is a relationship but not linear.
#It will be very hard to run linear regression on non-linear relationships
#So, we have two choices:-either we can use a non-linear model or we can use transformations to linearize the data
#As we are using linear regression on this model,
#We can use log transformations on the dataset and use the obtained dataset in our regression analysis

l1_log<-ggplot( data = fish,mapping = aes(x = log(Length1), y =log(Weight),col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")
l2_log<-ggplot( data = fish,mapping = aes(x = log(Length2), y = log(Weight),col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")
l3_log<-ggplot( data = fish,mapping = aes(x = log(Length3), y = log(Weight),col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")
h_log<-ggplot( data = fish,mapping = aes(x = log(Height), y = log(Weight),col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")

w_log<-ggplot( data = fish,mapping = aes(x = log(Width), y = log(Weight),col=Species)) +geom_point(size=3,alpha=0.5) +geom_smooth(col="black",method = "lm",se=F,lty="dashed",lwd=2,formula="y~x")

l1_log
l2_log
l3_log
h_log
w_log

#We can observe that now, the relationships between "HEIGHT"and "WIDTH" with "WEIGHT" is linear.
#Now we can use linear regression to analyse and predict the data

#We first check correlations between the dependent variable and independent variables
correlation_fish<-cor(fish[c("Length1","Length2","Length3","Height","Width","Weight")])
library(corrplot)
corrplot(correlation_fish,method = "number")
#As evident from the correlation matrix, Length1,Length2 and Length are highly correlated with one another
#This correlation between independent variables is called multicollinearity.
#It is advisable to use only one of those correlated independent variable in order to improve the linear regression

# REGRESSION ANALYSIS
#Building the full linear with all available variables
fish_reg <- lm(Weight ~ Species + Length1 + Length2 + Length3 + Height + Width,data = fish)
summary(fish_reg)
#The summary shows us that R-Squared is 0.93 which means our regression explains 93% of the variance of the dataset
#This is good
#However, the high p-values of "Species", "Length1","Length2" and "Length3" suggest that these variables are not really significant
#That also means the regression can be further optimized to obtain better results

#OPTIMIZATION OF REGRESSION
#As previously shown by the correlation matrix, "Length1", "Length2" and "Length3" are correlated to one another, so we use only one of them "Length1"
#We remove "Species" entirely, as we have seen from the density graph that species wise, the density is not normally distributed.
#Buidling the new regression model
fish_reg_improved <- lm(Weight ~ Length1 + Height + Width,data = fish)
summary(fish_reg_improved)
#Although the R-Squared has decreased, we can see that the p-values are all close to 0.000
#WHich means that all the variables we have used in the new regression model are highly significant

#TRAINNING AND TESTING ON THE IMPROVED REGRESSION MODEL
#Shuffling the data
fish_shuffled<-fish%>%sample_n(158)
#Setting the trainning and testing set
#Train set is 84.81% while Test set is the rest of the whole dataset
fish_train<-fish_shuffled[1:134,]
fish_test<-fish_shuffled[135:158,]

#Trainning the improved model
fish_reg_improved<-lm(Weight~ Length1 + Height +Width, data = fish_train)
#Predicting the model
fish_predict<-predict(fish_reg_improved,fish_test)

#Cross checking the observed values and predicted values
head(fish_test)
head(fish_predict)

#Checking the performance of the model
cor_reg_fish<-cor(fish_test$Weight,fish_predict)
cor_reg_fish
#The correlation is 0.9498 which is very good for our model


#Plotting and comparing our predictions with the observed values
pred_plot <- data.frame(pred_lin=fish_predict,real_values=fish_test$Weight,unit = seq(1,length(fish_test$Weight),by=1))

ggplot(pred_plot,aes(x=unit,y=real_values))+
  geom_point(col="red",size=6,alpha=0.7)+
  geom_point(aes(x=unit,y=pred_lin),col="blue",size=4,alpha=0.7)+
  scale_x_continuous(breaks=seq(1,25,by=1),labels  =seq(1,25,by=1),minor_breaks = NULL)
#From the graph, we can infer that our regression is close to predicting most of the values but not yet perfect
#We can further imporve our model by using a log-log model where we take the log values of our dependent and independent variables

#Trainning the new improved log model
fish_reg_log<-lm(log(Weight)~log(Length1)+log(Height)+log(Width),data=fish_train)
summary(fish_reg_log)
#it is clear from the summary that the log-log multiple regression is way better than the previously imporved model
#The R-Squared has improved from 0.89 to 0.99 which is great 
#In addition the p-values of all variables are 0.000 which means all the variables are significant

#predicting using log-log multiple regression
fish_predict_log<-predict(fish_reg_log,fish_test)
#Reversing the log using the exponential function
fish_final_predict_log<-exp(fish_predict_log)

#Comparing the heads of the real and predicted values
head(fish_test)
head(fish_final_predict_log)

#Plotting and comparing the real values with the values predicted
pred_plot_log <- data.frame(pred_log=fish_final_predict_log,real_values=fish_test$Weight,unit = seq(1,length(fish_test$Weight),by=1))

ggplot(pred_plot_log,aes(x=unit,y=real_values))+
  geom_point(col="red",size=6,alpha=0.7)+
  geom_point(aes(x=unit,y=pred_log),col="green",size=4,alpha=0.7)+
  scale_x_continuous(breaks=seq(1,25,by=1),labels  =seq(1,25,by=1),minor_breaks = NULL)
#As we can clearly see, the predictions are way way better than the improved model
#The new log-log multiple linear regression model pretty much 

#Summarizing the predicting power of the two regression models
pred_plot_final <- data.frame(pred_log=fish_final_predict_log,pred_lin=fish_predict,real_values=fish_test$Weight,unit = seq(1,length(fish_test$Weight),by=1))

ggplot(pred_plot_final,aes(x=unit,y=real_values))+
  geom_point(col="red",size=6,alpha=0.7)+
  geom_point(aes(x=unit,y=pred_lin),col="blue",size=4,alpha=0.7)+
  geom_point(aes(x=unit,y=pred_log),col="green",size=4,alpha=0.7)+
  scale_x_continuous(breaks=seq(1,25,by=1),labels  =seq(1,25,by=1),minor_breaks = NULL)

#RED -> REAL VALUES
#BLUE -> PREDICTED VALUES USING THE IMPROVED LINEAR REGRESSION MODEL
#GREEN -> PREDICTED VALUES USING THE LOG-LOG REGRESSION MODEL
