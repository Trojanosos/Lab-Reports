#Lab Report III

#step one is setting working direction and loading data


#relevant packages
library(tidyverse) #ggplot
library(dplyr) #filtering and summarizing
library(ggplot2) #graphs
library(modelsummary) #regression summary table
library(texreg) #html summary table
library(sjPlot) #plotting models and estimates
library(lme4)	#lmer functions
library(lmerTest) #testing significance of lmer
library(ordinal) #for ordinal regression models
library(MASS) #for fitting and using models


#Loading and taking a look at the data

setwd("~/Desktop/Lab Reports")

studentLab3<-read_csv("studentLab3.csv") #saving data in an object

view(studentLab3) #viewing data


#categorizing data s numeric or factor and filtering age NA's
studentLab3<-studentLab3 %>%
  mutate(age = na_if(age,0),
         school = as_factor(school),
         homeworks = as.numeric(homeworks),
         grade = as_factor(grade),
         paid = as_factor(paid),
         traveltime = as.numeric(traveltime),
         sex = as_factor(sex),
         studytime = as.numeric(studytime))

studentLab3<- studentLab3 %>% #combining other and female
  mutate(sex = fct_collapse(sex, O= c('O', 'F')))

studentLab3<- na.omit(studentLab3) #filtering and passing by NA's

#Task 1
#illustration of control variable descriptives with boxplot

ggplot(studentLab3, aes(y = age, x = sex, fill = school)) +
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Age')+
  xlab('Sex')+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white")+
  facet_wrap(~school)+
  ggtitle("Figure 1. Boxplot: Age~Sex by school")

#illustration of descriptives with bar charts

ggplot(studentLab3, aes(x = studytime, fill = grade))+
  geom_bar(position = 'dodge')+
  ggtitle("Figure 2. Barchart: count~studytime by grade")


ggplot(studentLab3, aes(x = grade, fill = paid)) +
  geom_bar()+
  ggtitle("Figure 3. Barchart: count~grade by paid")


#Descriptives table (inspired by Paula Nilsson feedback)
#sd= standard deviation -> na.rm removes the NA's 


summary(studentLab3$grade) #grades

summary(studentLab3$homeworks) #homeworks
table(studentLab3$homeworks)
sd(studentLab3$homeworks, na.rm = TRUE)

summary(studentLab3$school) #school

summary(studentLab3$sex) #sex

summary(studentLab3$age) #age
table(studentLab3$age) 
sd(studentLab3$age, na.rm = TRUE)

summary(studentLab3$studytime) #studytime 
table(studentLab3$studytime) 
sd(studentLab3$studytime, na.rm = TRUE)

summary(studentLab3$traveltime) #traveltime
table(studentLab3$traveltime) 
sd(studentLab3$traveltime, na.rm = TRUE)

summary(studentLab3$paid) #paid

#searching for NA's

sum(is.na(studentLab3))
colSums(is.na(studentLab3))

#Task 2

#converting the variable 'grade' to numeric
studentLab3$grade <- as.numeric(studentLab3$grade)

studentLab3 %>% #illustrating the difference with ggplot
  ggplot(aes(x = studytime, y = grade, color = paid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ paid)+
  ggtitle("Figure 5. Graph of grade~studyime by paid")

studentLab3 %>% #illustrating the difference with ggplot
  ggplot(aes(x = studytime, y = grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Figure 4. Graph of grade~studyime")


#Task 3

#visualizing the influence of paid teaching in one graph, graphical visualization is not enough for explaining causality, also consider school
ggplot(data = studentLab3, aes(x = grade, y = age, fill = paid)) +
  geom_point() +
  geom_smooth(method = "lm")+
  ggtitle("Figure 6. Graph of age~grade by paid")

#Task 4
#turning paid into numeric variable

studentLab3$paid <- as.numeric(studentLab3$paid)
view(studentLab3)

#creating a correlation matrix
cor <- cor(studentLab3[c("grade", "paid")])

view(cor)

#creating a linear model

model <- lm(grade ~ paid + age + sex + homeworks + studytime, data = studentLab3)

summary(model) #checking for statistical significance of grade~paid relationship while using control variables


#Task 5

#setting the variables back to the above created preferences
studentLab3<-studentLab3 %>%
  mutate(age = na_if(age,0),
         school = as_factor(school),
         homeworks = as.numeric(homeworks),
         grade = as_factor(grade),
         paid = as_factor(paid),
         traveltime = as.numeric(traveltime),
         sex = as_factor(sex),
         studytime = as.numeric(studytime))

#Model1
Modell<- clm(grade~studytime + sex + age + traveltime, data = studentLab3) #creating model without school

#Model 2
Model<- clm(grade~studytime + sex + age + school + traveltime, data = studentLab3) #creating model with school variables

#model 3
ModelMixed<- clmm(grade~studytime + sex + age  + traveltime + (1|school), data = studentLab3) #creating intercept model with school as mixed effect

summary(Model) #summarizing

summary(Modell) #summarizing

summary(ModelMixed) #summarizing

anova(Modell, Model, ModelMixed) #comparing the models

#DF
ranef(ModelMixed)

#Therefore, The number of estimated parameters is equal to the number of fixed effects coefficients +
#the number of random effects coefficients + 1 (residual variance) -> unexplained variation in the outcome 
#unaccountable by the fixed and random effects in the model
# This equals: number of observations - number of estimated parameters + 1
#643-7+1 = 637


#Task 7
ranef(ModelMixed) #looking at the intercepts of the different schools

mean(studentLab3$traveltime) #looking at means for formula)
mean(studentLab3$studytime) #looking at means for formula

#Task 8 

summary(ModelMixed)

#creating log odds and storing in object

logit2prob <- function(logit){ 
  odds <- exp(logit)
  prob <- odds / (1 + odds)*100
  return(prob)}

#Formula for male 20 years old 
#Filtering for the mean of studytime and traveltime of every male

library(dplyr)

averageM<-studentLab3 %>%
  dplyr::select(sex, studytime, traveltime) #selecting relevant variables, sex, travel- and studytime

averageM<-filter(averageM,sex == "M") #filtering for male

#looking at the mean of travel- and studytime
mean1<-mean(averageM$traveltime)
mean2<-mean(averageM$studytime)

#reversing the clmm formula and generating log odds to receive probabilities

#ModelMixed
#grade = u|g(-4.7) - studytime(0.5)*(1.72) - sexM(-0.4)*1 +
#age(-0.24)*20 - traveltime (-0.28)*(1.61 mean) + ε 

-4.7-(0.5*mean2)-(-0.4)-(-0.24*20)-(-0.28*mean1)

#grade = g|vg(-1.57)- studytime(0.5)*(1.72) -sexM(-0.4)*1 +
#age(-0.24)*20 - traveltime (-0.28)*(1.61 mean) + ε 

-1.57-(0.5*mean2)-(-0.4)-(-0.24*20)-(-0.28*mean1)



#Travetime (do they all live in the school building? 1-4 minutes/hours is unrealistic!)

#from log odds to probability
logit2prob(0.09)
logit2prob(3.22) 

#Failing 52.25%, probability of failing and passing is 96.16%. 
96.16-52.25
100-96.16
#Therefore, the probability of an average male to pass is 43.91%


#Task 9

glm1<- glmer(homeworks~studytime + age + traveltime+ sex + (1|school),  #creating a poisson model that allows prediction of 
             data = studentLab3, family = poisson) #homework with random effect of school, while controlling for age, traveltime and sex

summary(glm1) #summarize

glm2<-glm(homeworks~studytime + age + traveltime + school + sex,  #creating a poisson model that allows prediction of homework 
     data = studentLab3, family = poisson) #without random effect while controlling for age,traveltime, school, and sex

summary(glm2)



anova(glm1, glm2) #comparing the models


#Task 10

df.residual(glm1)

#Task 11
#model for  age 20, male 

summary(glm1)
summary(averageM)

#age= 20

#means
#studytime= 1.72
#traveltime= 1.61

#inserting the values of the summary table into the glm formula-> reversing effect from log to odds

-3.85-0.38*(1.72)+(0.22)*(20)+(0.09)*(1.61)+(0.24)*(1)



exp(0.2813) #exponential number of formula result to derive the poisson estimation

#1.33 homeworks in average

#Without traveltime because it is unrealistic. 1-4 minutes is not a real world factor if it is not a bording school.

-3.85-0.38*(1.72)+(0.22)*(20)+(0.24)*(1)


exp(0.1364)

#Without traveltime the average is 1.15 of not submitted homeworks

#Task 12 

ggplot(studentLab3, aes(x=studytime, y=homeworks)) + 
  geom_point() + 
  geom_smooth(method="glm", formula=y~x, se=FALSE, method.args=list(family="poisson")) +
  xlab("studytime") + 
  ylab("Number of not submitted homeworks") + 
  ggtitle("Figure 7. Poisson Scatterplot: studytime~Number of not submitted homeworks")

ggplot(studentLab3, aes(x=studytime, y=homeworks)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("studytime") + 
  ylab("Number of not submitted homeworks") + 
  ggtitle("Figure 8. Linear Scatterplot: studytime~Number of not submitted homeworks")


