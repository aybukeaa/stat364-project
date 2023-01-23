library(readxl)
library(car)
library(dplyr)
library(MASS)
library(corrplot)
library(e1071)
library(ggplot2)
library(rstatix)
library(lmtest)
library(lmtest)
library(sandwich)
library(ggpubr)
library(WRS2)
library(moments)

projectdata <- read_excel("nhanes_systolic_new.xlsx")
head(projectdata)
summary(projectdata)
str(projectdata) #We have 3 categorical variable.(gender, pulse_regular_or_irregular, smoke_everyday_or_not)
#View(projectdata)
projectdata$SEQN <- NULL

#Transforming categorical variables into a factor variables
#Gender
#0: female, 1: male
projectdata$gender <- ifelse(projectdata$gender == "1", 1,0)
projectdata$gender <- as.factor(projectdata$gender)

#Smoked at least 100 cigarettes in life
#1:Yes 2:No 7:Refused 9:Don't know
projectdata$smoke_everyday_or_not <- as.factor(projectdata$smoke_everyday_or_not)

#Pulse rate is regular or irregular
#1:regular 0:irregular
projectdata$pulse_regular_or_irregular <- ifelse(projectdata$pulse_regular_or_irregular == "1", 1,0)
projectdata$pulse_regular_or_irregular <- as.factor(projectdata$pulse_regular_or_irregular)

#After the transformation on categorical variables
str(projectdata)


#RESEARCH 1
#According to H.Karppanen (1991), the mineral elements sodium, potassium, calcium and magnesium 
#play a central role in the normal regulation of blood pressure. 
#In particular, these mineral elements have important interrelationships in the control of 
#arterial resistance. These elements, especially sodium and potassium, also regulate the fluid 
#balance of the body and, hence, influence blood pressure. 
#Evidence shows that the present levels of intake of mineral elements are not optimum for 
#maintaining normal blood pressure and might cause development of arterial hypertension. 
#So we wanted to investigate the relationship of these minerals with blood pressure in our data.
data_mineral <- data.frame(systolic_blood_pressure =projectdata$systolic_blood_pressure,
                           sodium = projectdata$sodium,
                           dietary_potassium = projectdata$dietary_potassium,
                           dietary_calcium = projectdata$dietary_calcium,
                           magnesium = projectdata$magnesium)

#Building multiple linear regression:
#model.1 <- lm(systolic_blood_pressure ~ sodium+dietary_potassium+dietary_calcium+magnesium, data = projectdata)
#summary(model.1) #Do not forget to write the function of regression equation.
#par(mfrow = c(2, 2))
#plot(model.1)

plot(density(projectdata$systolic_blood_pressure)) #right-skewed outcome
#Compute the skewness. It is positive. Recall right-skewed=positively-skewed.
skewness(projectdata$systolic_blood_pressure)
shapiro.test(projectdata$systolic_blood_pressure)


################################
#Generalized linear model with and log link
#Alternative 1: gamma family log link
gamma_log <- glm(systolic_blood_pressure ~ sodium+dietary_potassium+dietary_calcium+magnesium, data = projectdata, family = Gamma(link = "log"))
summary(gamma_log)
par(mfrow = c(2, 2))
plot(gamma_log)

#Alternative 2: gamma family identity link
gamma_log1 <- glm(log(systolic_blood_pressure) ~ sodium+dietary_potassium+dietary_calcium+magnesium, data = projectdata, family = Gamma(link = "identity"))
summary(gamma_log1)
par(mfrow = c(2, 2))
plot(gamma_log1)

#Alternative 3:
#Log link normal model- gaussian family log link
gaussian <- glm(systolic_blood_pressure ~ sodium+dietary_potassium+dietary_calcium+magnesium, family = gaussian(link="log"), data = projectdata)
summary(gaussian)
par(mfrow = c(2, 2))
plot(gaussian)

#Alternative 4:
#inverse gaussian family identity link
inverse.gaussian <- glm(systolic_blood_pressure ~ sodium+dietary_potassium+dietary_calcium+magnesium, family = inverse.gaussian(link="identity"), data = projectdata)
summary(inverse.gaussian)
par(mfrow = c(2, 2))
plot(inverse.gaussian)


predicted.gamma_log <- predict(gamma_log, projectdata)
predicted.gamma_log1 <- predict(gamma_log1, projectdata)
predicted.gaussian <- predict(gaussian, projectdata)
predicted.inverse.gaussian <- predict(inverse.gaussian)

rmse.gamma_log <- caret::RMSE(predicted.gamma_log, projectdata$systolic_blood_pressure)
rmse.gamma_log1 <- caret::RMSE(predicted.gamma_log1, projectdata$systolic_blood_pressure)
rmse.gaussian <- caret::RMSE(predicted.gaussian, projectdata$systolic_blood_pressure)
rmse.inverse.gaussian <- caret::RMSE(predicted.inverse.gaussian, projectdata$systolic_blood_pressure)

rmse.gamma_log
rmse.gamma_log1
rmse.gaussian
rmse.inverse.gaussian

vif(gamma_log)
vif(gamma_log1)
vif(gaussian)
vif(inverse.gaussian)

#Investigating the source of misfit
#Checking the multicollinearity problem

acf(rstandard(gamma_log))#Residuals are independent.
acf(rstandard(gamma_log1))
acf(rstandard(gaussian))
acf(rstandard(inverse.gaussian))

bc <- boxcox(model.1)
lambda <- bc$x[which.max(bc$y)]
lambda

#box-cox transformation
projectdata$systolic_blood_pressure_trans <- projectdata$systolic_blood_pressure^lambda

gamma_log_trans <- glm(systolic_blood_pressure_trans ~ sodium+dietary_potassium+dietary_calcium+magnesium, data = projectdata, family = Gamma(link = "log"))
summary(gamma_log_trans)

par(mfrow = c(2, 2))
plot(gamma_log_trans)

acf(rstandard(gamma_log1))#Residuals are independent.

bc1 <- boxcox(model.1)
lambda1 <- bc1$x[which.max(bc1$y)]
lambda1

projectdata$systolic_blood_pressure_trans1 <- projectdata$systolic_blood_pressure^lambda1
gamma_log1_trans <- glm(abs(log(systolic_blood_pressure_trans1)) ~ sodium+dietary_potassium+dietary_calcium+magnesium, data = projectdata, family = Gamma(link = "identity"))
summary(gamma_log1_trans)
par(mfrow = c(2, 2))
plot(gamma_log1_trans)


#Alternative 3:
#Log link normal model- gaussian family log link
gaussian <- glm(systolic_blood_pressure ~ sodium+dietary_potassium+dietary_calcium+magnesium, family = gaussian(link="log"), data = projectdata)
summary(gaussian)
par(mfrow = c(2, 2))
plot(gaussian)

#1493001/4879 = 306 not a good fit

#Alternative 4:
#inverse gaussian family identity link
inverse.gaussian <- glm(systolic_blood_pressure ~ sodium+dietary_potassium+dietary_calcium+magnesium, family = inverse.gaussian(link="identity"), data = projectdata)
summary(inverse.gaussian)
par(mfrow = c(2, 2))
plot(inverse.gaussian)
vif(inverse.gaussian)

#FIRST DATASET QUESTION-2
data_gender <- data.frame(systolic_blood_pressure = projectdata$systolic_blood_pressure,
                          gender = projectdata$gender)

levels(projectdata$gender) #There are two level of gender which are female:0 and male:1

###Model Building
model_gender  <- lm(systolic_blood_pressure ~ gender, data = projectdata)
summary(model_gender) 

projectdata %>%
  group_by(gender) %>%
  get_summary_stats(systolic_blood_pressure, type = "mean_sd") #summary statistics

###Visualization
ggplot(projectdata, aes(systolic_blood_pressure, color = gender)) +
  geom_density() +
  scale_color_viridis_d()

ggplot(projectdata, aes(y =systolic_blood_pressure , color = gender)) +
  geom_boxplot()

###Checking Assumptions###
#Outliers Check
data_gender %>% 
  group_by(gender) %>%
  identify_outliers(systolic_blood_pressure) #There are extreme outliers.

#If the result will be substantially affected or not without outliers.
#This can be evaluated by comparing the result of the ANOVA test with and without the outlier.

#med1way(systolic_blood_pressure ~ gender, projectdata)
#t1way(systolic_blood_pressure ~ gender, projectdata)
#Qanova(systolic_blood_pressure ~ gender, projectdata, q = 0.5)


#Checking normality assumption by groups
shapiro_test(residuals(model_gender)) #Shapiro-wilk test indicates that the residuals are not normally distributed.
projectdata %>%
  group_by(gender) %>%
  shapiro_test(systolic_blood_pressure)
ggqqplot(projectdata, "systolic_blood_pressure", facet.by = "gender")
#The residuals from both female and male group are not normally distributed.


#Homogeneity of variance assumption
plot(model_gender, 1)
projectdata %>% levene_test(systolic_blood_pressure ~ gender)
#Null Hypothesis: All populations variances are equal
#Alternative Hypothesis: At least two of them differ
#Levene's test with one independent variable
leveneTest(systolic_blood_pressure ~ gender, data = projectdata)
#From the output above, we can see that the p-value is < 0.05, which is significant.
#This means that, there is significant difference between variances across groups.
#Therefore, we cannot assume the homogeneity of variances in the different treatment groups(genders).


#Since ANOVA assumptions are not met, we have used Kruskal-Wallis Test
#Kruskal-Wallis test, which is the non-parametric alternative to one-way ANOVA test.
kruskal.test(systolic_blood_pressure ~ gender, data = projectdata)
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

##############################
#SECOND-DATASET
########## RESEARCH QUESTION ##########
# Danish Business Authority suggests that entrepreneurs come to entrepreneurship with different levels of skills and therefore each entrepreneur requires a different ‘game plan’ for developing his or her skills. 
# As a result of this, main concern here is to investigate the skills levels of founders whether they have an impact on the success of startup or not. 

library(olsrr)
library(MASS)
data <- read.csv("startup.csv",header=T,na.strings=c(""))
datanew <- subset(data,select=c(2,29:38))
colnames(datanew) <- c("Dependent", "Entrepreneurship.skills","Operations.skills","Engineering.skills","Marketing.skills" ,
                       "Leadership.skills","Data.Science.skills","Business.Strategy.skills" , "Product.Management.skills",
                       "Sales.skills","Domain.skills" )
View(datanew)
set.seed(364)
sample <- sample(c(TRUE, FALSE), nrow(datanew), replace=TRUE, prob=c(0.7,0.3))
train  <- datanew[sample, ]
test   <- datanew[!sample, ]

################# LOGISTIC REG ASSUMPTION CHECK ####################################

######### Linearity assumption ###########
library(tidyverse)
library(broom)
theme_set(theme_classic())

full.model1 <- glm(Dependent ~.,family=binomial, data=datanew)
summary(full.model1)
probabilities <- predict(full.model1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# Select only numeric predictors
mydata <- datanew %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# As seen in the above plots, there is no linear relationship between the logit of the outcome and each predictor variables.
# Only Data Science skills score, Domain skills score, Leadership skills score and Engineering skills score have linear realtionship with the response.

########## Influential Points ############

plot(full.model1, which = 4, id.n = 3)

# The Cook’s distance plot shows 3 possible outlier values. However, not all outliers are influential observations.
# To check whether the data contains potential influential observations, the standardized residual error can be inspected.
# Data points with an absolute standardized residuals above 3 represent possible outliers.

model.data <- augment(full.model1) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Dependent), alpha = .5) +
  theme_bw()

# As seen in the standardized residuals plot  there is no influential values

model.data %>% 
  filter(abs(.std.resid) > 3)

############ Multicollinearity ###########3

car::vif(full.model1)

# As seen in the table above, there is no multicollinearity among the predictors.
# All variables have a value of  VIF well below 5.

################### VARIABLE SELECTION ####################


######## STEPWISE SELECTION ###########
full.model <- glm(Dependent ~.,family=binomial,data=train)
step(full.model)

########## FORWARD SELECTION
null.model <- glm(Dependent ~1, family = binomial, train)
forward.model <- step(object = null.model, scope = list(upper = full.model), direction = "forward")

# For variable selection stepwise and forward selection methods are used. 
# From stepwise selection, best model is constructed with the variables: Founders Engineering skills score  Founders Leadership skills score, Founders Data Science skills score, Founders Domain skills scores. 
# The same result is obtained from forward selection too.

library(MASS)
library(caret)
args(stepAIC)

model.with.caret <- caret::train(as.factor(Dependent)~., data = train, method="glmStepAIC", trControl=trainControl(method="cv"), direction="both")

summary(model.with.caret)

### FINAL MODEL #####
model.final <- glm(Dependent ~ Data.Science.skills+Domain.skills+Leadership.skills+Engineering.skills, family = binomial, train)
summary(model.final)

######### Linearity assumption ###########
mydata <- datanew %>%
  dplyr::select(c(1,4,6,7,11)) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
# Create the Scatter Plots:
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
########## Influential Points ############

plot(model.final, which = 4, id.n = 3)

# The Cook’s distance plot shows 3 possible outlier values. However, not all outliers are influential observations.
# To check whether the data contains potential influential observations, the standardized residual error can be inspected.
# Data points with an absolute standardized residuals above 3 represent possible outliers.

model.data <- augment(model.final) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Dependent), alpha = .5) +
  theme_bw()

# As seen in the standardized residuals plot  there is no influential values

model.data %>% 
  filter(abs(.std.resid) > 3)

############ Multicollinearity ###########3

car::vif(model.final)

# As seen in the table above, there is no multicollinearity among the predictors.
# All variables have a value of  VIF well below 5.


##### MODEL VALIDATION ####

# H0: model is a good fit
# H1: model is not a good fit
library(ResourceSelection) 
hoslem.test(model.final$y, fitted(model.final), g=10) # g is number of groups. since we don't have any groups we take it as 10 which is default.
# Since p-value is larger than 0.05 we cant reject null hyp and conclude that the model seems to fit well.

####### accuracy check
library(caret)
probs <- predict(model.final, test, type="response")
predicted <- ifelse(probs > 0.5, 1, 0)
t <- table(predicted, actual=test$Dependent)
t
confusionMatrix(t, positive = "1") 
# since 95% CI for accuracy contains no info rate we shouldn't use this model for further estimations.
# Also, we desire other levels such as sensitivity, specifity, etc. to be higher than 0.5; but in our example we have 0.4 for sensitivity. we shouldn't use this model.


library(pROC)
roc <- roc(test$Dependent ~ probs, plot = TRUE, print.auc = TRUE, legacy.axes=T)
# Again from the ROC curve, it assess that the models predictive power , 0.481, is also low.










