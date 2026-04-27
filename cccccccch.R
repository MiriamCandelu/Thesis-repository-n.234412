#ECONOMIC EMP AND RIGHTS !!

install.packages("psych")
install.packages("ggplot2") #ggplot is used for high-quality graphs
install.packages("lmtest") # to test for homo/heteroskedasticity
install.packages("sandwich") # to use robust standard errors 
install.packages("modelsummary", dependencies = TRUE) # this package depends on some other packages
install.packages("corrplot") # Install the corrplot library, for a nice-looking correlation matrix
install.packages("stargazer")
install.packages("readxl")
install.packages("e1071")
install.packages("modelsummary")


######## Call the aforementioned packages:
library(ggplot2)
library(lmtest)
library(sandwich)
library(modelsummary)
library(corrplot)
library(e1071)

#RESTART by loading packages
library(psych)
library(readxl)
library(haven)
library(questionr)
library(tidyverse)
library(descr)
library(table1)
library(gtsummary)
library(sjPlot)
library(ggplot2)
library(readr)
library(modelsummary)
library(dplyr)
library(corrplot)
library(ggdag)
library(curl)
library(labelled)
library(dagitty)
library(ggrepel)
library(readxl)


#load dataset
getwd()
setwd("C:/Users/miria/Miriam/HERTIE/TESI/R BUONAFORTUNA")
data <- read_excel("C:/Users/miria/Miriam/HERTIE/TESI/Tentative.Dataset.xlsx")
head(data)
str(data)

#now create clean dataset for political IV so I don't override 



subset3 <- data %>%
  select(Country,
         WEmp3_women_in_parliament,
         Health_mortality_under5, 
         Stunted_under5, 
         Water_access, 
         Mean_year_schooling_total,
         Government_effectiveness, 
         GDP_percapita_PPP, 
         Urban_population, 
         Conflict_Fragility, 
         Net_ODAreceived, 
         Region)


clean3 <- subset3 %>%
  filter(
         !is.na(WEmp3_women_in_parliament),
         !is.na(Health_mortality_under5), 
         !is.na(Stunted_under5),
         !is.na(Water_access),
         !is.na(Mean_year_schooling_total),
         !is.na(Government_effectiveness),
         !is.na(GDP_percapita_PPP),
         !is.na(Urban_population),
         !is.na(Conflict_Fragility),
         !is.na(Net_ODAreceived))



clean3 <- clean3 %>%
  rename(
    WomeninParliament = WEmp3_women_in_parliament,
    Under5Mortality = Health_mortality_under5,
    Stunted = Stunted_under5,
    Water = Water_access, 
    Education = Mean_year_schooling_total,
    Quality = Government_effectiveness, 
    GDPpc = GDP_percapita_PPP,
    UrbanPop = Urban_population,
    ConflictFragility = Conflict_Fragility, 
    NetODA = Net_ODAreceived
  )

summary(clean3)


describe(clean3)

clean3$ConflictFragility <- factor(clean3$ConflictFragility,
                                   levels = c(0, 1),
                                   labels = c("None", "Conflict or Fragile"))

clean3$Region <- factor(clean3$Region,
                        levels = c("EUCA", "SSA", "SA", "MENA", "LAC", "EAP"))

table(clean3$Region)
table(clean3$ConflictFragility)


descriptive_data3 <- clean3 %>%
  select(
    WomeninParliament,
    Under5Mortality,
    Stunted,
    Water,
    Education,
    Quality,
    GDPpc,
    UrbanPop,
    NetODA
  ) %>%
  rename(
    'Women in Parliament' = WomeninParliament,
    'Under-5 Mortality' = Under5Mortality,
    'Stunting Rate' = Stunted,
    'Access to Water' = Water,
    'Education' = Education,
    'Government Effectiveness' = Quality,
    'GDP per Capita' = GDPpc,
    'Urbanization' = UrbanPop,
    'Net ODA' = NetODA
  )


datasummary_skim(descriptive_data3, fmt = 2)



hist(clean3$WomeninParliament)
hist(clean3$WomeninParliament, 
     main="Histogram of Women in Parliament", 
     xlab="% of seat held by women in parliament (lower chamber)", 
     ylab="Frequency", 
     col="lightblue")

hist(clean3$Under5Mortality, 
     main="Histogram of Under 5 Mortality", 
     xlab="Under5 Mortality per 1000 live births", 
     ylab="Frequency", 
     col="lightblue")

#Under5Moortality is skewed

hist(clean3$Stunted, 
     main="Histogram of Stunting Under 5", 
     xlab="% Stunted Under5", 
     ylab="Frequency", 
     col="lightblue")
#also kinda skewedits already a percentage so we don't log it anyway

#can also use this for skewness to decided if log or not
skewness(clean3$Under5Mortality, na.rm = TRUE)
#if bigger than 1 strongly skewed, 0.5 moderately skewed
#now it's 1.174594 so we can log 

hist(clean3$Water, 
     main="Histogram of Water Access", 
     xlab="Basic drinking water services access % of population", 
     ylab="Frequency", 
     col="lightblue")
#skewed but already a percentage

hist(clean3$Education, 
     main="Histogram of Mean Years of Schooling", 
     xlab="Education", 
     ylab="Frequency", 
     col="lightblue")


hist(clean3$Quality, 
     main="Histogram of Administrative Quality", 
     xlab="Government Effectiveness", 
     ylab="Frequency", 
     col="lightblue")

hist(clean3$GDPpc, 
     main="Histogram of GDP per capita", 
     xlab="GDP per capita PPP", 
     ylab="Frequency", 
     col="lightblue")
#skewed and will be logged

hist(clean3$UrbanPop, 
     main="Histogram of Urban Population", 
     xlab="Urban Population % of total population", 
     ylab="Frequency", 
     col="lightblue")


#for this hist formula need numeric variables so can't do it for Conflict Fragility

hist(clean3$NetODA, 
     main="Histogram of Net Development Asssitance", 
     xlab="Conflict or institutional fragility", 
     ylab="Frequency", 
     col="lightblue")

#CONSIDER LOGS

#will log only under5 mortality and GDPpc

#do correlation and scatterplot
cor(clean3$Under5Mortality, clean3$WomeninParliament)
#can't use cor(clean1) becuase x should be numeric
cor(clean3[, !names(clean3) %in% c("Country","ConflictFragility", "Region")], use = "complete.obs")   #exclude variables non numeric 
#here look out for 
# 1 Multicollinearity - correlations among IVs and controls - if coefficient is bigger than 0.7 warning, bigger than 0.8 serious problem
# 2 DV-IV relationship expected sign and reasonable magnitude - strong correlation is good signal eg. 0.5-0.7, very weal around 0 relationship might be weak or non linear
# 3 DV-controls - controls should actually relate to the DV so we want it NOT to be 0, but even moderate is fine - close to 0 means control may not be doing much but can keep if theory justifies is

# in this case looks good
# No Multicollinearity - all IV and controls have coefficients under 0.7
# DV-IV all above 0.5
# DV-controls - all good but NetODA is a bit low 0.15 something 

datasummary_correlation(clean3)


plot(clean3$WomeninParliament, clean3$Under5Mortality)
text(clean3$WomeninParliament, clean3$Under5Mortality, labels = clean3$Country, cex = 0.7, pos = 4) #allows me to put country names so can check outlier

# PLOT THE REGRESSION
ggplot(clean3, aes(x=WomeninParliament, y=Under5Mortality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Women in Parliament and Child Mortality") +
  labs(y="Under 5 Child Mortality", x = "% of women in parliament") +
  theme_minimal() # changes default theme


options(scipen=999) 
reg16 <- lm(Under5Mortality ~ WomeninParliament, data= clean3)
summary(reg16)

# intercept is the value of child mortality if WPEA was zero, under 5 mortality would be 54.78 per 1000 live birth
# coefficient of WPEA is -0.2744
# it's significant but at the 0.5 level mhh



#LOG
clean3$log_Under5Mortality <- log(clean3$Under5Mortality)

hist(clean3$log_Under5Mortality, 
     main="Histogram of Under 5 Mortality log", 
     xlab="Under5 Mortality per 1000 live births %", 
     ylab="Frequency", 
     col="lightblue")


#we log GDP too
clean3$log_GDPpc <- log(clean3$GDPpc)
plot(clean3$log_GDPpc, clean3$log_Under5Mortality)

cor(clean3$WomeninParliament, log(clean3$log_GDPpc), use = "complete.obs")

datasummary_correlation(clean3)
datasummary_correlation(
  clean3 %>%
    select(
      WomeninParliament,
      log_Under5Mortality,
      Stunted,
      Water,
      Education,
      Quality,
      log_GDPpc,
      UrbanPop,
      NetODA
    ) %>%
    rename(
      "Women in Parliament" = WomeninParliament,
      "Log Under-5 Mortality" = log_Under5Mortality,
      "Stunting Rate" = Stunted,
      "Access to Water" = Water,
      "Education" = Education,
      "Government Effectiveness" = Quality,
      "Log GDP Per Capita" = log_GDPpc,
      "Urbanization" = UrbanPop,
      "Net ODA" = NetODA  
    )
)




plot(clean3$WomeninParliament, clean3$log_Under5Mortality)
ggplot(clean3, aes(x=WomeninParliament, y=log_Under5Mortality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Women in Parliament and Under-5 Mortality") +
  labs(y="Under-5 Mortality (log)", x = "Women Seats in National Parliament (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

#now run another regression with log you can check hot it changes the effect and interpretation
reg17 <- lm(log_Under5Mortality ~ WomeninParliament, data= clean3)
summary(reg17)
#it's not significant anymore

# but its not necessary, the important thing is the log

#now we move to the complete regression model
# MULTIPLE REGRESSION



model1.20 <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc, data = clean3)
summary(model1.20)

(exp(-0.001301) - 1) * 100
#“The coefficient is interpreted using the transformation (e  β  −1)×100, which yields the exact percentage change in the dependent variable.”
#with the exact formula, the effect is  -0.1300154. 
#only when DV is logged
#ma comunque sta interpretazione in questo caso non vale niente perchè non è significant


model2.20 <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc + UrbanPop, data = clean3)
summary(model2.20)

model3.20 <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility, data = clean3)
summary(model3.20)

model4.20 <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean3)
summary(model4.20)

model5.20 <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3)
summary(model5.20) 

#overall, it is not significamt amd there is not a big effect whatsoever anyway
(exp(-0.005137) - 1) * 100

#OLS assumptions check

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.20) 
#Residual vs. Fitted, we want to see random scatter around 0 and no clear pattern; but what we see here is that there is a U shape pattern
#which means that residuals are not randomly distributed and there is violation of 
#linearity assumption, the model is likely misspecified, we may need log transofrmation (or a non linear term like suqared, but log more probable)
#Q-Q plot, we want points along the straight line. We have points mostly aligned at the center but deviations in the tails (here not bad but left tail deviates)
#which means residuals are not perfectly normal. It's a mild/moderate issue and common in cross country data but confirms presence of outliers/heavy tails
#Scale-Location, we want a flat red line and equal spread of point; IF we have a red line that is increasing and spread grows with fitted values
#this means heteroskedasticity is present (variance increases with predicted values) - even if not totally clear tbh - always better to do bptest
#Residuals vs Leverage, we want no extreme leverage points but we have a few points on the right (111,97,45)what are potential influential observations
#SO overall diagnosis: linearity , heteroskedasticity actually not according to bp test, some outliers; 
#if you don't meet assumptions what should do? try log transformation. Use robust standard errors, check influencial points, and re run diagnostic after fixes:) 

# how to write it in thesis: Diagnostic plots indicate deviations from OLS assumptions, including evidence of non-linearity and heteroskedasticity. 
#To address these issues, variables were transformed and robust standard errors were employed 

# HOMOSCEDASTICITY
plot(model5.20, 3) 
# does the variance of the residuals increase as the predicted values (y-hat) increase? 
# this would indicate heteroskedasticity# a horizontal line indicates homoscedasticity 
# In general, if the distribution of the residuals looks shaped like a funnel (like this < or this >),then heteroskedasticity is also likely

# Bonus: Robust Standard Errors:
# Easy fix for presence of heteroscedasticity: Robust Standard Errors:
# From plot it looks like there is heteroscedasticity, but is there a way to test this? 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.20)

?bptest

# The null hypothesis for the Breusch-Pagan test is that the variance of 
# the error term is constant, i.e., homoscedasticity
# Pvalue < 0.05, therefore we to reject the null of homoscedasticity! bad news!
#The Breusch–Pagan test indicates the presence of heteroskedasticity if (p < 0.05)
#because it is 0.1518 for model 5.16, we fail to reject the H0, we find no strong evidence of heteroscedasticity



#MULTICOLLINEARITY
#can account for it with correlation tables; with VIF; with regression and R2 by regressig each coefficient
#on the rest and exhamining R2; and with tolerance - the proportion in the variation of the indep.var not explained by other indep variables
library(car)
vif(model5.20)
#while correlation between the IV and the two control GDP is a bit high, around 7, the VIF test indicates that the coefficient variance 
#is inflated by a factor of 2.14 which is totally acceptable and under 5-10 or more. 



#INFLUENTIAL OBS/OUTLIERS
#Are there influential cases that are affecting my substantive results?”
?avPlots
avPlots(model5.20)


##
outlier_check8 <- data.frame(
  std_resid = rstandard(model5.20),
  stud_resid = rstudent(model5.20),
  cooks_d = cooks.distance(model5.20),
  leverage = hatvalues(model5.20)
)

outlier_check8$case <- 1:nrow(outlier_check8)

flagged <- subset(
  outlier_check8,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.20)) |
    leverage > 2 * (length(coef(model5.20)) / nrow(model.frame(model5.20)))
)

flagged
model.frame(model5.20)[flagged$case, ]

#if just cooks

cooks.distance(model5.20)
4/116
#0.03448276
#so it is observation 8, 45, 65, 73, 96, 108

clean3_no_outliers8 <- clean3[-c(8, 45, 65, 73, 96, 108), ]

model5.20_no_outliers <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_outliers8)
summary(model5.20_no_outliers) 

(exp(-0.00383230) - 1) * 100
#-0.3824966
#A compact way to write this in the thesis is:
#I assessed influential observations using Cook’s distance, focusing on whether specific cases materially altered the regression 
#estimates. Given the study’s aim, this was more relevant than identifying outliers per se.
#or Influential observations were assessed using Cook’s distance (threshold: 4/n). Models were re-estimated excluding flagged cases
#to verify that the main results were not driven by a small number of observations.
#the coefficient becomes slightly smaller and still non significant

dfb20 <- dfbetas(model5.20)

n <- nrow(clean3)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb20)

which(abs(dfb20[, "WomeninParliament"]) > threshold) 

dfb20[which(apply(abs(dfb20) > threshold, 1, any)), ]

plot(dfb20)
abline(h = c(-threshold, threshold), lty = 2)

clean3_no_dfbetas20 <- clean3[-c(8, 18,  65,  80,  81,  96, 103, 113 ), ]

model5.20_no_dfbetas <- lm(log_Under5Mortality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_dfbetas20)
summary(model5.20_no_dfbetas) 



#MOVING TO STUNTING 

reg18 <- lm(Stunted ~ WomeninParliament, data= clean3)
summary(reg18)
#not significant from the beginning

plot(clean3$WomeninParliament, clean3$Stunted)
ggplot(clean3, aes(x=WomeninParliament, y=Stunted)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Women in Parliament and Stunting Rate") +
  labs(y ="Stunting (% of Children Under-5)" , x = "Women's Seats in National Parliament (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )



model1.21 <- lm(Stunted ~ WomeninParliament + log_GDPpc, data = clean3)
summary(model1.21)

model5.21 <- lm(Stunted ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3)
summary(model5.21) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.21) 
#Residual vs. Fitted, we want to see random scatter around 0 and no clear pattern; but what we see here is that there is a U shape pattern
#which means that residuals are not randomly distributed and there is violation of 
#linearity assumption, the model is likely misspecified, we may need log transofrmation (or a non linear term like suqared, but log more probable)
#Q-Q plot, we want points along the straight line. We have points mostly aligned at the center but deviations in the tails (here not bad but left tail deviates)
#which means residuals are not perfectly normal. It's a mild/moderate issue and common in cross country data but confirms presence of outliers/heavy tails
#Scale-Location, we want a flat red line and equal spread of point; IF we have a red line that is increasing and spread grows with fitted values
#this means heteroskedasticity is present (variance increases with predicted values) - even if not totally clear tbh - always better to do bptest
#Residuals vs Leverage, we want no extreme leverage points but we have a few points on the right (111,97,45)what are potential influential observations
#SO overall diagnosis: linearity , heteroskedasticity actually not according to bp test, some outliers; 
#if you don't meet assumptions what should do? try log transformation. Use robust standard errors, check influencial points, and re run diagnostic after fixes:) 

# how to write it in thesis: Diagnostic plots indicate deviations from OLS assumptions, including evidence of non-linearity and heteroskedasticity. 
#To address these issues, variables were transformed and robust standard errors were employed 


# HOMOSCEDASTICITY 
plot(model5.21, 3) 
# does the variance of the residuals increase as the predicted values (y-hat) increase? 
# this would indicate heteroskedasticity# a horizontal line indicates homoscedasticity 
# In general, if the distribution of the residuals looks shaped like a funnel (like this < or this >),then heteroskedasticity is also likely

# Bonus: Robust Standard Errors:
# Easy fix for presence of heteroscedasticity: Robust Standard Errors:
# From plot it looks like there is heteroscedasticity, but is there a way to test this? 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.21)

# the Breusch-Pagan test indicates no heteroskedasticity (0.8962 > 0.05)

#MULTICOLLINEARITY
#can account for it with correlation tables; with VIF; with regression and R2 by regressig each coefficient
#on the rest and exhamining R2; and with tolerance - the proportion in the variation of the indep.var not explained by other indep variables
library(car)
vif(model5.21)
#while correlation between the IV and the two control GDP is a bit high, around 7, the VIF test indicates that the coefficient variance 
#is inflated by a factor of 2.14 which is totally acceptable and under 5-10 or more. 


#OUTLIERS 
?avPlots
avPlots(model5.21)

# If you want R to identify the extreme observations on the plot, you can use dfbetas

##
outlier_check9 <- data.frame(
  std_resid = rstandard(model5.21),
  stud_resid = rstudent(model5.21),
  cooks_d = cooks.distance(model5.21),
  leverage = hatvalues(model5.21)
)

outlier_check9$case <- 1:nrow(outlier_check9)

flagged <- subset(
  outlier_check9,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.21)) |
    leverage > 2 * (length(coef(model5.21)) / nrow(model.frame(model5.21)))
)

flagged
model.frame(model5.21)[flagged$case, ]

#if just cooks

cooks.distance(model5.5)
4/116
#0.03448276

#1, 4, 42, 47, 64, 67, 82, 83, 96, 103, 105

clean3_no_outliers9 <- clean3[-c(1, 4, 42, 47, 64, 67, 82, 83, 96, 103, 105), ]

model5.21_no_outliers <- lm(Stunted ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_outliers9)
summary(model5.21_no_outliers) 

#slightly bigger, not significant

dfb21 <- dfbetas(model5.21)

n <- nrow(clean3)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb21)

which(abs(dfb21[, "WomeninParliament"]) > threshold) 

dfb21[which(apply(abs(dfb21) > threshold, 1, any)), ]

plot(dfb21)
abline(h = c(-threshold, threshold), lty = 2)

clean3_no_dfbetas21 <- clean3[-c(4,  17,  18,  42,  80,  83,  96, 103 ), ]

model5.21_no_dfbetas <- lm(Stunted ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_dfbetas21)
summary(model5.21_no_dfbetas) 




#WATER ACCESS

reg19 <- lm(Water ~ WomeninParliament, data= clean3)
summary(reg19)
#not significant from the beginning


ggplot(clean3, aes(x=WomeninParliament, y=Water)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Women in Parliament and Water Access") +
  labs(y ="Water Access (% of population)" , x = "Women's Seats in National Parliament (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

model1.22 <- lm(Water ~ WomeninParliament + log_GDPpc, data = clean3)
summary(model1.22)

model5.22 <- lm(Water ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3)
summary(model5.22) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.22) 
#Residual vs. Fitted, we want to see random scatter around 0 and no clear pattern; but what we see here is that there is a U shape pattern
#which means that residuals are not randomly distributed and there is violation of 
#linearity assumption, the model is likely misspecified, we may need log transofrmation (or a non linear term like suqared, but log more probable)
#Q-Q plot, we want points along the straight line. We have points mostly aligned at the center but deviations in the tails (here not bad but left tail deviates)
#which means residuals are not perfectly normal. It's a mild/moderate issue and common in cross country data but confirms presence of outliers/heavy tails
#Scale-Location, we want a flat red line and equal spread of point; IF we have a red line that is increasing and spread grows with fitted values
#this means heteroskedasticity is present (variance increases with predicted values) - even if not totally clear tbh - always better to do bptest
#Residuals vs Leverage, we want no extreme leverage points but we have a few points on the right (111,97,45)what are potential influential observations
#SO overall diagnosis: linearity , heteroskedasticity actually not according to bp test, some outliers; 
#if you don't meet assumptions what should do? try log transformation. Use robust standard errors, check influencial points, and re run diagnostic after fixes:) 

# how to write it in thesis: Diagnostic plots indicate deviations from OLS assumptions, including evidence of non-linearity and heteroskedasticity. 
#To address these issues, variables were transformed and robust standard errors were employed 


# HOMOSCEDASTICITY 
plot(model5.7, 3) 
# does the variance of the residuals increase as the predicted values (y-hat) increase? 
# this would indicate heteroskedasticity# a horizontal line indicates homoscedasticity 
# In general, if the distribution of the residuals looks shaped like a funnel (like this < or this >),then heteroskedasticity is also likely

# Bonus: Robust Standard Errors:
# Easy fix for presence of heteroscedasticity: Robust Standard Errors:
# From plot it looks like there is heteroscedasticity, but is there a way to test this? 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.22)

# the Breusch-Pagan test indicates heteroskedasticity (0.0009797 > 0.05)
library(sandwich)
corrected_errors5.22 <- coeftest(model5.22, vcov = vcovHC(model5.22, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.22


#MULTICOLLINEARITY
#can account for it with correlation tables; with VIF; with regression and R2 by regressig each coefficient
#on the rest and exhamining R2; and with tolerance - the proportion in the variation of the indep.var not explained by other indep variables
library(car)
vif(model5.22)
#while correlation between the IV and the two control GDP is a bit high, around 7, the VIF test indicates that the coefficient variance 
#is inflated by a factor of 2.14 which is totally acceptable and under 5-10 or more. 



#OUTLIERS 

##
outlier_check10 <- data.frame(
  std_resid = rstandard(model5.22),
  stud_resid = rstudent(model5.22),
  cooks_d = cooks.distance(model5.22),
  leverage = hatvalues(model5.22)
)

outlier_check10$case <- 1:nrow(outlier_check10)

flagged <- subset(
  outlier_check10,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.22)) |
    leverage > 2 * (length(coef(model5.22)) / nrow(model.frame(model5.22)))
)


flagged
model.frame(model5.7)[flagged$case, ]

#if just cooks

cooks.distance(model5.7)
4/116
#0.03448276

#21, 26, 45, 66, 83, 87, 109

clean3_no_outliers10 <- clean3[-c(21, 26, 45, 66, 83, 87, 109), ]

model5.22_no_outliers <- lm(Water ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_outliers10)
summary(model5.22_no_outliers) 

#effect changes sign but still not significant and changed sign what 

dfb22 <- dfbetas(model5.22)

n <- nrow(clean3)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb22)

which(abs(dfb22[, "WomeninParliament"]) > threshold) 

dfb22[which(apply(abs(dfb22) > threshold, 1, any)), ]

plot(dfb22)
abline(h = c(-threshold, threshold), lty = 2)

clean3_no_dfbetas22 <- clean3[-c(21,  26,  39,  80,  83,  87,  90,  96, 103  ), ]

model5.22_no_dfbetas <- lm(Water ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_dfbetas22)
summary(model5.22_no_dfbetas) 





#EDUCATION

reg20 <- lm(Education ~ WomeninParliament, data= clean3)
summary(reg20)
#significant at * level


ggplot(clean3, aes(x=WomeninParliament, y=Education)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Women in Parliament and Education") +
  labs(y ="Education (Mean Years of Schooling)" , x = "Women's Seats in National Parliament (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

model1.23 <- lm(Education ~ WomeninParliament + log_GDPpc, data = clean3)
summary(model1.23)

#not significant anymoree

model5.23 <- lm(Education ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3)
summary(model5.23) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.23) 
#Residual vs. Fitted, we want to see random scatter around 0 and no clear pattern; but what we see here is that there is a U shape pattern
#which means that residuals are not randomly distributed and there is violation of 
#linearity assumption, the model is likely misspecified, we may need log transofrmation (or a non linear term like suqared, but log more probable)
#Q-Q plot, we want points along the straight line. We have points mostly aligned at the center but deviations in the tails (here not bad but left tail deviates)
#which means residuals are not perfectly normal. It's a mild/moderate issue and common in cross country data but confirms presence of outliers/heavy tails
#Scale-Location, we want a flat red line and equal spread of point; IF we have a red line that is increasing and spread grows with fitted values
#this means heteroskedasticity is present (variance increases with predicted values) - even if not totally clear tbh - always better to do bptest
#Residuals vs Leverage, we want no extreme leverage points but we have a few points on the right (111,97,45)what are potential influential observations
#SO overall diagnosis: linearity , heteroskedasticity actually not according to bp test, some outliers; 
#if you don't meet assumptions what should do? try log transformation. Use robust standard errors, check influencial points, and re run diagnostic after fixes:) 

# how to write it in thesis: Diagnostic plots indicate deviations from OLS assumptions, including evidence of non-linearity and heteroskedasticity. 
#To address these issues, variables were transformed and robust standard errors were employed 


# HOMOSCEDASTICITY 
plot(model5.8, 3) 
# does the variance of the residuals increase as the predicted values (y-hat) increase? 
# this would indicate heteroskedasticity# a horizontal line indicates homoscedasticity 
# In general, if the distribution of the residuals looks shaped like a funnel (like this < or this >),then heteroskedasticity is also likely

# Bonus: Robust Standard Errors:
# Easy fix for presence of heteroscedasticity: Robust Standard Errors:
# From plot it looks like there is heteroscedasticity, but is there a way to test this? 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.23)

# the Breusch-Pagan test indicates not heteroskedasticity (p > 0.05)
#0.6849 p bigger than 0.05 so no heteroscedasticity



#MULTICOLLINEARITY
#can account for it with correlation tables; with VIF; with regression and R2 by regressig each coefficient
#on the rest and exhamining R2; and with tolerance - the proportion in the variation of the indep.var not explained by other indep variables
library(car)
vif(model5.23)
#while correlation between the IV and the two control GDP is a bit high, around 7, the VIF test indicates that the coefficient variance 
#is inflated by a factor of 2.14 which is totally acceptable and under 5-10 or more. 


#OUTLIERS 

##
outlier_check11 <- data.frame(
  std_resid = rstandard(model5.23),
  stud_resid = rstudent(model5.23),
  cooks_d = cooks.distance(model5.23),
  leverage = hatvalues(model5.23)
)

outlier_check11$case <- 1:nrow(outlier_check11)

flagged <- subset(
  outlier_check11,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.23)) |
    leverage > 2 * (length(coef(model5.23)) / nrow(model.frame(model5.23)))
)

flagged
model.frame(model5.8)[flagged$case, ]

#if just cooks

cooks.distance(model5.8)
4/116
#0.03448276

#26, 45, 58, 67, 95, 96, 107, 109

clean3_no_outliers11 <- clean3[-c(26, 45, 58, 67, 95, 96, 107, 109), ]

model5.23_no_outliers <- lm(Education ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_outliers11)
summary(model5.23_no_outliers) 

#the effect is slightly bigger and slightly significant (.) but still would not consider significant 

dfb23 <- dfbetas(model5.23)

n <- nrow(clean3)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb23)

which(abs(dfb23[, "WomeninParliament"]) > threshold) 

dfb23[which(apply(abs(dfb23) > threshold, 1, any)), ]

plot(dfb23)
abline(h = c(-threshold, threshold), lty = 2)

clean3_no_dfbetas23 <- clean3[-c(26, 90, 95, 96 ), ]

model5.23_no_dfbetas <- lm(Education ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_dfbetas23)
summary(model5.23_no_dfbetas) 


#ADMINISTRATIVE QUALITY

reg21 <- lm(Quality ~ WomeninParliament, data= clean3)
summary(reg21)
#significant at 0.05 level

ggplot(clean3, aes(x=WomeninParliament, y=Quality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Women in Parliament and Government Effectiveness") +
  labs(y ="Government Effectiveness" , x = "Women's Seats in National Parliament (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )



model1.24 <- lm(Quality ~ WomeninParliament + log_GDPpc, data = clean3)
summary(model1.24)

model2.24 <- lm(Quality ~ WomeninParliament + log_GDPpc + UrbanPop, data = clean3)
summary(model2.24)

#lost significance

model3.24 <- lm(Quality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility, data = clean3)
summary(model3.24)

model4.24 <- lm(Quality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean3)
summary(model4.24)

model5.24 <- lm(Quality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3)
summary(model5.24) 
#yoooo its significant again at 0.05 slay queen 



#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.24) 
#Residual vs. Fitted, we want to see random scatter around 0 and no clear pattern; but what we see here is that there is a U shape pattern
#which means that residuals are not randomly distributed and there is violation of 
#linearity assumption, the model is likely misspecified, we may need log transofrmation (or a non linear term like suqared, but log more probable)
#Q-Q plot, we want points along the straight line. We have points mostly aligned at the center but deviations in the tails (here not bad but left tail deviates)
#which means residuals are not perfectly normal. It's a mild/moderate issue and common in cross country data but confirms presence of outliers/heavy tails
#Scale-Location, we want a flat red line and equal spread of point; IF we have a red line that is increasing and spread grows with fitted values
#this means heteroskedasticity is present (variance increases with predicted values) - even if not totally clear tbh - always better to do bptest
#Residuals vs Leverage, we want no extreme leverage points but we have a few points on the right (111,97,45)what are potential influential observations
#SO overall diagnosis: linearity , heteroskedasticity actually not according to bp test, some outliers; 
#if you don't meet assumptions what should do? try log transformation. Use robust standard errors, check influencial points, and re run diagnostic after fixes:) 

# how to write it in thesis: Diagnostic plots indicate deviations from OLS assumptions, including evidence of non-linearity and heteroskedasticity. 
#To address these issues, variables were transformed and robust standard errors were employed 


# HOMOSCEDASTICITY 
plot(model5.24, 3) 
# does the variance of the residuals increase as the predicted values (y-hat) increase? 
# this would indicate heteroskedasticity# a horizontal line indicates homoscedasticity 
# In general, if the distribution of the residuals looks shaped like a funnel (like this < or this >),then heteroskedasticity is also likely

# Bonus: Robust Standard Errors:
# Easy fix for presence of heteroscedasticity: Robust Standard Errors:
# From plot it looks like there is heteroscedasticity, but is there a way to test this? 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.24)

# the Breusch-Pagan test indicates no heteroskedasticity if (p > 0.05)
#0.7019 p bigger than 0.05 so no heteroscedasticity


#MULTICOLLINEARITY
#can account for it with correlation tables; with VIF; with regression and R2 by regressig each coefficient
#on the rest and exhamining R2; and with tolerance - the proportion in the variation of the indep.var not explained by other indep variables
library(car)
vif(model5.24)
#while correlation between the IV and the two control GDP is a bit high, around 7, the VIF test indicates that the coefficient variance 
#is inflated by a factor of 2.14 which is totally acceptable and under 5-10 or more. 


#OUTLIERS 

##
outlier_check12 <- data.frame(
  std_resid = rstandard(model5.24),
  stud_resid = rstudent(model5.24),
  cooks_d = cooks.distance(model5.24),
  leverage = hatvalues(model5.24)
)

outlier_check12$case <- 1:nrow(outlier_check12)

flagged <- subset(
  outlier_check12,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.24)) |
    leverage > 2 * (length(coef(model5.24)) / nrow(model.frame(model5.24)))
)

flagged
model.frame(model5.24)[flagged$case, ]

#if just cooks

cooks.distance(model5.9)
4/116
#0.03448276

#1, 11, 38, 45, 47, 52, 76, 87, 96, 109

clean3_no_outliers12 <- clean3[-c(1, 11, 38, 45, 47, 52, 76, 87, 96, 109), ]

model5.24_no_outliers <- lm(Quality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_outliers12)
summary(model5.24_no_outliers) 

#the effect is slightly bigger really little tho; but it becomes significant at the 0.01 level! 

dfb24 <- dfbetas(model5.24)

n <- nrow(clean3)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb24)

which(abs(dfb24[, "WomeninParliament"]) > threshold) 

dfb24[which(apply(abs(dfb24) > threshold, 1, any)), ]

plot(dfb24)
abline(h = c(-threshold, threshold), lty = 2)

clean3_no_dfbetas24 <- clean3[-c(8, 47, 75, 87, 90, 96, 97), ]

model5.24_no_dfbetas <- lm(Quality ~ WomeninParliament + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean3_no_dfbetas24)
summary(model5.24_no_dfbetas) 






library(modelsummary)
library(sandwich)

modelsummary(
  list(
    "Under 5 Mortality" = model5.20,
    "Stunting" = model5.21,
    "Water Access" = model5.22,
    "Education" = model5.23,
    "Government Effectiveness" = model5.24
  ),
  vcov = list(
    NULL,
    NULL,
    vcovHC(model5.22, type = "HC1"),
    NULL,
    NULL
  ),
  coef_rename = c(
    "WomeninParliament" = "Women in Parliament (% of seats)",
    "log_GDPpc" = "Log GDP per capita",
    "UrbanPop" = "Urban population (%)",
    "ConflictFragilityConflict or Fragile" = "Conflict or Fragile",
    "NetODA" = "Net ODA (% GNI)",
    "RegionSSA" = "Sub-Saharan Africa",
    "RegionSA" = "South Asia",
    "RegionMENA" = "Middle East & North Africa",
    "RegionLAC" = "Latin America & Caribbean",
    "RegionEAP" = "East Asia & Pacific"
  ),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  statistic = "({std.error})",
  output = "WomenParliament1.png"
)





