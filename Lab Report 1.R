#relevant packages
library(tidyverse) #ggplot
library(dplyr) #filtering and summarizing
library(ggplot2) #graphs 
library(modelsummary) #regression summary table
library(texreg) #html summary table
library(equatiomatic) #formula illustration in LaTeX

#setting working directory and loading data

setwd("~/Desktop/Lab Reports")

load("titanic.Rdata")

#Task 1

Titanicdf<-data.frame(dt) #creating data frame

view(Titanicdf) 

#summarize data for table of descriptives (idea inspired by Paula Nilsson's feedback)
#sd= standard deviation -> na.rm removes the NA's 


summary(Titanicdf$Survived) #survived
sd(Titanicdf$Survived, na.rm = TRUE)
table(Titanicdf$Survived)

summary(Titanicdf$Age) #age 
sd(Titanicdf$Age, na.rm = TRUE) 

summary(Titanicdf$Fare) #fare
sd(Titanicdf$Fare, na.rm = TRUE)

summary(Titanicdf$Pclass) #ticket class 
sd(Titanicdf$Pclass, na.rm = TRUE)

table(Titanicdf$Sex) #sex 
sd(Titanicdf$Sex, na.rm = TRUE) #warning message due to double

#other variables, e.g.,Passenger ID  and habour are not relevant


#searching for NA's

sum(is.na(Titanicdf))
colSums(is.na(Titanicdf))


#recoding data into either numeric or factor variables

Titanicdf_mutated<-Titanicdf %>% mutate(Pclass=as_factor(Pclass),
                                        Sex = as_factor(Sex),
                                        Age = as.numeric(Age),
                                        Survived = as_factor(Survived), 
                                        Fare=as.numeric(Fare))

#visualization of main data with Boxplot's

ggplot(Titanicdf, aes(x = Sex, y = Age)) +
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Age')+
  xlab('Sex')+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white")

ggplot(Titanicdf, aes(x = Pclass, y = Age)) +
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Age')+
  xlab('Class')+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white")

#visualization of main data with histogramm's

ggplot(Titanicdf_mutated, aes(x=Age, fill = Survived))+ 
  geom_histogram()+
  ylab('Count')+
  xlab('Age')+
  theme_classic()+
  labs(fill= 'Survived')+
  scale_fill_discrete(labels = c("No", "Yes"))
  
ggplot(Titanicdf_mutated, aes(x=Age, fill= Pclass))+ 
  geom_histogram()+
  ylab('Count')+
  xlab('Age')+
  theme_classic()+
  labs(fill= 'Class')

ggplot(Titanicdf_mutated, aes(x=Age, color= Sex, fill= Sex))+ 
  geom_histogram()+
  ylab('Count')+
  xlab('Age')+
  theme_classic()+
  labs(fill= 'Sex')


#Task 2 

# Convert variable to a factor

Titanicdf$Pclass <- as.factor(Titanicdf$Pclass)

# Changing the reference level of "Pclass" to the third class

Titanicdf$Pclass <- relevel(Titanicdf$Pclass, ref = 3)

#creating a model

model1<-glm(Survived ~ Pclass, 
    family = binomial(link="logit"),
    data = Titanicdf)	

summary(model1) #summary of the model
plot(model1) #plotting the model

4/891 #rule of thumb for cook's distance

# Task 3 

modelsummary(model1, stars = TRUE) #model summary table 

modellist<- list(model1) #Creating a list

htmlreg(modellist) #creating html model summary table 

#equation in LaTeX 
equatiomatic::extract_eq(model1)
extract_eq(model1, wrap = TRUE, intercept = 'beta')


#Task 4 and 5

#In general, a model with more predictors will be more complex and will 
#be able to capture more patterns in the data than a model with fewer predictors. 
#However, adding more predictors can also lead to over-fitting, where the model p
#erforms well on the training data but does not generalize well to new data. 
#Therefore, it is important to evaluate the performance of the model on a separate test 
#set or using cross-validation to ensure that the model is not over-fitting while losing DF.



model2<-glm(Survived ~ Pclass + Sex + Age,  # Task 4, creating a model adding Sex and Age as predictors-> control variables
            family = binomial(link="logit"),
            data = Titanicdf)	

summary(model2) #summarizing the model

plot(model2) #plotting the model

#Task 6

# using logit2prob

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)}
#Formulas applied

#Intercept(1.2) + Pclass1(2.58)*(1)+Pclass2(1.27)*(0)+-0,037*(age=30)+(-2.52).*(1) = 0.15 

#Intercept(1.2) + Pclass1(2.58)*(0)+Pclass2(1.27)*(0)+-0,037*(age=30)+(-2.52)*(1)  = (-2.42) 

Male1<-1.2+2.58-1.11-2.52
logit2prob(Male1)

#The probability of 1st Class Passenger to survive is 0.537 (54%)

Male3<-1.2-1.11-2.52
logit2prob(Male3)

#The probability of 3rd Class Passenger Making it is 0.08(8%)



#Task 7 

pscl::pR2(model1)["McFadden"] #using McFadden R^2 
pscl::pR2(model2)["McFadden"]

#Task 8 threshold & sensitivity 

NoNA<-na.omit(Titanicdf) #removing/bassing by NA's of the data frame
view(NoNA)


model3<-glm(Survived ~ Pclass + Sex + Age, #creating model with data from NoNA
            family = binomial(link="logit"),
            data = NoNA)	

summary(model3) #summarizing


#logit2prob

logit2prob <- function(logit){ #using Tutor's formula for log odds
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)}

NoNA = NoNA %>% 	#Threshold -1.99
  mutate(pred_mod1 = predict(model3)) %>% 	
  mutate(pred_mod1 = case_when(pred_mod1 <= (-1.99) ~ "0",	
                               pred_mod1 >= (-1.99) ~ "1"))	

NoNA = NoNA %>%	#creating correct_prediction
  mutate(correct_prediction = case_when(pred_mod1 == Survived ~ "0",	
                                        pred_mod1 != Survived ~ "1"))


# coding correct guesses	
NoNA = NoNA %>%	
  mutate(correct_prediction = case_when(pred_mod1 == Survived ~ "correct",	
                                        pred_mod1 != Survived ~ "incorrect"))	


#Confusion matrix

NoNA %>% 	
  group_by(Survived, pred_mod1) %>% 	
  summarize(n = n()) %>% 	
  spread(Survived, n)	

# correct categorization rate overall	

NoNA %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))



#Model 3 

model3<-glm(Survived ~ Pclass + Sex + Fare, #without removing NA's but by using Fares instead of Age
            family = binomial(link="logit"),
            data = Titanicdf)	

#Same procedure

logit2prob <- function(logit){ #logit2prob
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)}


Predictionmodelnew = Titanicdf_mutated %>% 	#Threshold 0.1
  mutate(pred_mod1 = logit2prob(predict(model3))) %>% 	
  mutate(pred_mod1 = case_when(pred_mod1 <= 0.1 ~ "0",	
                               pred_mod1 > 0.1  ~ "1"))



Predictionmodelnew1 = Predictionmodelnew %>%	#creating correct_prediction
  mutate(correct_prediction = case_when(pred_mod1 == Survived ~ "1",	
                                        pred_mod1 != Survived ~ "0"))


#Confusion matrix

Predictionmodelnew1 %>% 	
  group_by(Survived, pred_mod1) %>% 	
  summarize(n = n()) %>% 	
  spread(Survived, n)	

# correct categorization rate overall

Predictionmodelnew1 %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))



#Task 9

#Same procedure

Predictionmodel = NoNA %>% 	
  mutate(pred_mod1 = logit2prob(predict(model4))) %>% 	#Threshold of 0
  mutate(pred_mod1 = case_when(pred_mod1 <= 0 ~ "0",	
                               pred_mod1 > 0  ~ "1"))

Predictionmodel1 = Predictionmodel %>%	
  mutate(correct_prediction = case_when(pred_mod1 == Survived ~ "1",	
                                        pred_mod1 != Survived ~ "0"))


Predictionmodel1 %>% 	
  group_by(Survived, pred_mod1) %>% 	
  summarize(n = n()) %>% 	
  spread(Survived, n)	

Predictionmodel1 %>%	
  filter(Survived == "1") %>% 	
  group_by(correct_prediction) %>%	
  summarise(count = n()) %>%	
  mutate(freq = count / sum(count))

#how many actual survived
testing<-filter(Titanicdf, Survived== '1')
nrow(testing)


