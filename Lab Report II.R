#relevant packages

library(tidyverse) #ggplot
library(dplyr) #filtering and summarizing
library(ggplot2) #graphs
library(modelsummary) #regression summary table
library(texreg) #html summary table
library(sjPlot) #plotting models and estimates
library(lme4)	#lmer functions
library(lmerTest) #testing significance of lmer
library(Hmisc) #analysis and visualization
library(base) #basic functions
library(glmmTMB) #functions for fitting glmm using normal distribution
library(car) #checking for multicollinarity
library(equatiomatic) #formula illustration in LaTeX

#setting working directory and loading data 
setwd("~/Desktop/Lab Reports")
load("dataLab2.Rdata")

#viewing and combining data sets
view(dtB)
view(dtA)
dataset<- rbind(dtA, dtB)
View(dataset)

#Task 1

#Looking at the data and creating a table  for descriptives (inspired by Paula Nilsson feedback)
#sd= standard deviation -> na.rm removes the NA's 

summary(dataset$pain) #pain
sd(dataset$pain, na.rm = TRUE)

summary(dataset$STAI_trait) #trait_anxiety
sd(dataset$STAI_trait, na.rm = TRUE)

summary(dataset$pain_cat) #pain_catastrophizing
sd(dataset$pain_cat, na.rm = TRUE)

summary(dataset$mindfulness) #mindfulness
sd(dataset$mindfulness, na.rm = TRUE)

summary(dataset$cortisol_serum) #cortisol_serum
sd(dataset$cortisol_serum, na.rm = TRUE)

summary(dataset$cortisol_saliva) #cortisol_saliva
sd(dataset$cortisol_saliva, na.rm = TRUE)

summary(dataset$age) #age
sd(dataset$pain, na.rm = TRUE)

summary(dataset$weight) #weight
sd(dataset$weight, na.rm = TRUE)

summary(dataset$household_income) #income
sd(dataset$household_income, na.rm = TRUE)

summary(dataset$IQ) #IQ
sd(dataset$IQ, na.rm = TRUE)

table(dataset$sex) #sex

#searching for NA's

sum(is.na(dataset))
colSums(is.na(dataset))

#visualizing the data with graphs 

dataset %>% 		 #STAI_trait~age
  ggplot() +		
  aes(y = STAI_trait, x = age) +		
  geom_point(aes(color = pain), size = 4)	+
  ggtitle("Figure 1. scatterplot: STAI_trait~AGE ")

dataset %>% 		#Crtisol_serum~Cortisol_salvia
  ggplot() +		
  aes(y = cortisol_serum, x = cortisol_saliva) +		
  geom_point(aes(color = pain), size = 4)	+
  ggtitle("Figure 2. scatterplot: Cortisol_serum~Cortisol_saliva")

dataset %>% 		#mindfulness~STAI_trait
  ggplot() +		
  aes(y = mindfulness, x = STAI_trait) +		
  geom_point(aes(color = weight), size = 4)	+
  ggtitle("Figure 3. scatterplot: Mindfulness~STAI_trait")

#changing names for illustration

datanew <- transform(dataset, 
                     hospital = as.numeric(gsub("hospital_", "", hospital)))


view(datanew)
#changing categorization in order to illustrate variables

datanew1<-datanew %>% mutate(household_income=as.numeric(household_income))


#recode sex and include woman as female

data1 <- datanew1 %>% 	
  mutate(sex = ifelse(sex == "male", "male", 
                      ifelse(sex == "female" | sex == "woman", "female", sex)))

view(data1)

#more graphs

data1 %>% 		#household_income~hospital
  ggplot() +		
  aes(y = household_income, x = hospital) +
  scale_x_continuous(breaks = seq(0,20)) +
  geom_point(aes(color = pain_cat), size = 4) +
  ggtitle("Figure 4. scatterplot: Household income~Hospital")

data1 %>% 		#mindfulness~hospital
  ggplot() +		
  aes(y = mindfulness, x = hospital) +	
  scale_x_continuous(breaks = seq(0,20)) +
  geom_point(aes(color = cortisol_serum), size = 4)	+
  ggtitle("Figure 4. scatterplot: Mindfulness~Hospital ")

data1 %>% 		#pain_cat~hospital
  ggplot() +		
  aes(y = pain_cat, x = hospital) +		
  scale_x_continuous(breaks = seq(0,20)) +
  geom_point(aes(color = pain), size = 4) +
ggtitle("Figure 5. scatterplot: Pain_cat~Hospital ")

data1 %>% 		#IQ~hospital
  ggplot() +		
  aes(y = IQ, x = hospital) +		
  scale_x_continuous(breaks = seq(0,20)) +
  geom_point(aes(color = pain), size = 4)	+
  ggtitle("Figure 6. scatterplot: IQ~Hospital ")
  

#Boxplots

ggplot(data1, aes(y = age, x = sex)) +  #age~sex
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Age') +
  xlab('Sex') +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white") +
  ggtitle("Figure 7. Boxplot: Age~Sex ")

ggplot(data1, aes(y = household_income, x = sex)) + #household_income~sex
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Income') +
  xlab('Sex') +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white") +
  ggtitle("Figure 8. Boxplot: Household income~Sex")

ggplot(data1, aes(y = cortisol_serum, x = sex)) + #coritaol_serum`sex`
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Cortisol Serum') +
  xlab('Sex') +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white") +
  ggtitle("Figure 9. Boxplot: Cortisol serum~Sex")

ggplot(data1, aes(y = cortisol_saliva, x = sex)) + #pain_cat~sex
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Cortisol Saliva') +
  xlab('Sex') +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white") +
  ggtitle("Figure 10. Boxplot: Cortisol saliva~Sex")

ggplot(data1, aes(y = pain, x = sex)) + #pain~sex
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Pain') +
  xlab('Sex') +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white") +
  ggtitle("Figure 11. Boxplot: Pain~Sex")


ggplot(data1, aes(y = pain_cat, x = sex)) + #pain_cat~sex
  geom_boxplot(outlier.size=1, outlier.shape=1, outlier.colour="black") + 
  ylab('Pain Cat') +
  xlab('Sex') +
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 3, fill = "white") +
  ggtitle("Figure 12. Boxplot: Pain catastrophizing~Sex")


#Task 3

#numerical variables
summary(dataset) #looking at data again

numeric<-select(dataset,STAI_trait, #selecting numerical variables
                age, pain, pain_cat, 
                cortisol_serum, cortisol_saliva, 
                mindfulness, weight, IQ, 
                household_income) 

correlationtable<-cor(numeric) #creating correlation table
view(correlationtable) #looking at correlation table


#Task 4 

#Intercept model 1.1 with all control variables

model1.1 <- lmer(pain ~ mindfulness +
                   cortisol_saliva + 
                   cortisol_serum  + 
                   household_income + 
                   STAI_trait + 
                   weight + 
                   IQ + 
                   hospital + 
                   age+ 
                   sex+
                   pain_cat + 
                   (1|hospital), data = data1)

#R recommends rescalaing as control variables are not on the same scale

summary(model1.1) #looking at significance


#Intercept model 1

model1 <- lmer(pain ~ mindfulness + #Creating a random intercept model with all 
                 cortisol_saliva + #established findings, sex, age and hospital as random effect
                 cortisol_serum + 
                 STAI_trait+ 
                 age + 
                 sex+
                 pain_cat + 
                 (1|hospital), data = data1)

summary(model1) #looking at the model

ranef(model1) #looking at the intercept for each hospital slope

library(gridExtra) #combining plots

grid.arrange( plot_model(model1, type = "est"),
              plot_model(model1, type = "re"),
              ncol = 2)
  

plot(model1) #plotting the model

#creating a new variable for prediction, not shown in paper
dataframe<- data1
dataframe$newm<-predict(model1)

#visualizing different intercept models, not shown in paper
ggplot(dataframe, aes(y = mindfulness, x = pain, group = hospital))+	
  geom_point(size = 1)+	
  geom_smooth(color='red', aes(y= newm, x= pain), method = "lm")+	
  facet_wrap( ~ hospital, ncol = 5)	


#Figure 15
dataframe$newm1.1<-predict(model1.1)

ggplot(dataframe, aes(y = mindfulness, x = pain, group = hospital))+	
  geom_point(size = 1)+	
  geom_smooth(color='red', aes(y= newm1.1, x= pain), method = "lm")+	
  facet_wrap( ~ hospital, ncol = 5)	+
  ggtitle("Figure 15. Prediction of model 1.1 including all variables")
  

summary(model1.1)


#Models 2-6 are different models for every established model 
#also to check for hospital as random effect and to look at the influence of predictors


#pain~STAI_trait
model2 <- lmer(pain ~ STAI_trait + #Creating a random  intercept model with hospital as random effect
                 cortisol_saliva + cortisol_serum + cortisol_saliva + age + sex+
                 mindfulness + (1|hospital), data = data1)

grid.arrange( plot_model(model2, type = "est"),
              plot_model(model2, type = "re"),
              ncol = 2)

summary(model2)

#pain~pain_cat
model3 <- lmer(pain ~ pain_cat + #Creating a random intercept model with hospital as random effect
                 cortisol_saliva + cortisol_serum + STAI_trait+ age + sex+
                 mindfulness + (1|hospital), data = data1)

grid.arrange( plot_model(model3, type = "est"),
              plot_model(model3, type = "re"),
              ncol = 2)

summary(model3)

#Spain~mindfulness
model4 <- lmer(pain ~ mindfulness + #Creating a random intercept model with hospital as random effect 
                 cortisol_saliva + cortisol_serum + pain_cat+ age + sex+
                 STAI_trait+ (1|hospital), data = data1)

grid.arrange( plot_model(model4, type = "est"),
              plot_model(model4, type = "re"),
              ncol = 2)

summary(model4)

#pain~cortisol_serum
model5 <- lmer(pain ~ cortisol_serum + #Creating a random intercept model with hospital as random effect
                 mindfulness + cortisol_saliva + STAI_trait+ age + sex+
                 pain_cat + (1|hospital), data = data1)

grid.arrange( plot_model(model5, type = "est"),
              plot_model(model5, type = "re"),
              ncol = 2)

summary(model5)

#pain~cortisol_saliva
model6 <- lmer(pain ~ cortisol_saliva + #Creating a random intercept model with hospital as random effect
                 mindfulness + cortisol_serum + STAI_trait+ age + sex+
                 pain_cat + (1|hospital), data = data1)

grid.arrange( plot_model(model6, type = "est"),
              plot_model(model6, type = "re"),
              ncol = 2)

summary(model6)


#Creating a slope model with established findings as random effect

model7 <- lmer(pain~STAI_trait+ 
                 pain_cat+
                 mindfulness+
                 cortisol_saliva + 
                 cortisol_serum + 
                 sex+
                 age+
                   (STAI_trait|hospital)+
                   (pain_cat|hospital)+
                   (mindfulness|hospital)+
                   (cortisol_serum|hospital),
               control = lmerControl(optimizer = "Nelder_Mead"), 
               data = data1)

#comparing models with anova
anova(model1, model1.1, model2, model3, model4, model5, model6, model7)

#comparing only intercept models
anova(model1, model1.1, model2, model3, model4, model5, model6)

#Using the slope model for illustration, Figure 14
plot(model7) #plotting model


dataframe$NY<-predict(model7) #creating new variable for prediction


ggplot(dataframe, aes(y = mindfulness, x = pain, group = hospital))+	
  geom_point(size = 1)+	
  geom_smooth(color='red', aes(y= NY, x= pain), method = "lm")+	
  facet_wrap( ~ hospital, ncol = 5) +
  ggtitle("Figure 14. Prediction of slope model 7")


#Task 5 DF

#looking at the degrees of freedom for all models

df.residual(model1) #model with all 'selected variables'
df.residual(model1.1)#model with all variables
df.residual(model2) #pain~STAI_tait
df.residual(model3) #pain~pain_cat
df.residual(model4) #pain~mindfulness
df.residual(model5) #pain~cortisol_serum
df.residual(model6) #pain~cortisol_saliva
df.residual(model7) #slope model


#finding out about influential outliers of selected model
cooksD <- cooks.distance(model1) #cooks distance

#creating function to find significant outliers 
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))] 

influential #viewing significant outliers


#homoscedasticity

plot_residuals(model1) #looking at plot to find out about homoscedasticity 
plot(model1) #plotting model 1 and looking at residuals

#vif check for multicollinearity

vif(model1)

#looking at residuals with 'basic' plot function 

plot(model1) #plotting model to identify normal distribution

#summary
msummary(model1)

#Task 6

qqnorm(residuals(model1)) #looking at Q-Q Plot to identify normal distribution

#Task 7

ranef(model1) #looking at slope intercepts of hospitals

#Task 8 vizualizing the model wit LaTeX

equatiomatic::extract_eq(model1)
extract_eq(model1, wrap = TRUE, intercept = 'beta')

ranef(model1) #determining hospital intercepts

#Task 9

model3 <- lmer(pain ~ age + #creating a model in order to calculate the pain of a 30 year old male
             sex + (1|hospital), data = data1)

summary(model3) #looking at coefficients for the formula

ranef(model3) #looking at slope intercepts of hospitals

