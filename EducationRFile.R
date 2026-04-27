#EDUCATION !!

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
data <- read_excel("C:/Users/miria/Miriam/HERTIE/TESI/Dataset.xlsx")
head(data)
str(data)

#now create clean dataset for education IV so I don't override 

subset1 <- data %>%
  select(Country,
         Wemp1_edu_female, 
         Health_mortality_under5, 
         Stunted_under5, 
         Water_access, 
         Government_effectiveness, 
         GDP_percapita_PPP, 
         Urban_population, 
         Conflict_Fragility, 
         Net_ODAreceived, 
         Region)


clean1 <- subset1 %>%
  filter(!is.na(Wemp1_edu_female),
         !is.na(Health_mortality_under5), 
         !is.na(Stunted_under5),
         !is.na(Water_access),
         !is.na(Government_effectiveness),
         !is.na(GDP_percapita_PPP),
         !is.na(Urban_population),
         !is.na(Conflict_Fragility),
         !is.na(Net_ODAreceived))


clean1 <- clean1 %>%
  rename(
    FEducation = Wemp1_edu_female,
    Under5Mortality = Health_mortality_under5,
    Stunted = Stunted_under5,
    Water = Water_access, 
    Quality = Government_effectiveness, 
    GDPpc = GDP_percapita_PPP,
    UrbanPop = Urban_population,
    ConflictFragility = Conflict_Fragility, 
    NetODA = Net_ODAreceived
  )

summary(clean1)


describe(clean1)

clean1$ConflictFragility <- factor(clean1$ConflictFragility,
                                   levels = c(0, 1),
                                   labels = c("None", "Conflict or Fragile"))

clean1$Region <- factor(clean1$Region,
                        levels = c("EUCA", "SSA", "SA", "MENA", "LAC", "EAP"))


table(clean1$Region)
table(clean1$ConflictFragility)


descriptive_data1 <- clean1 %>%
  select(
    FEducation,
    Under5Mortality,
    Stunted,
    Water,
    Quality,
    GDPpc,
    UrbanPop,
    NetODA
  ) %>%
  rename(
    'Female Schooling' = FEducation,
    'Under-5 Mortality' = Under5Mortality,
    'Stunting Rate' = Stunted,
    'Access to Water' = Water,
    'Government Effectiveness' = Quality,
    'GDP per Capita' = GDPpc,
    'Urbanization' = UrbanPop,
    'Net ODA' = NetODA
  )


datasummary_skim(descriptive_data1, fmt = 2)





table(clean1$Region)
table(clean1$ConflictFragility)

hist(clean1$FEducation)
hist(clean1$FEducation, 
     main="Histogram of Female Education", 
     xlab="Mean Years Female Education", 
     ylab="Frequency", 
     col="lightblue")

hist(clean1$Under5Mortality, 
     main="Histogram of Under 5 Mortality", 
     xlab="Under5 Mortality per 1000 live births", 
     ylab="Frequency", 
     col="lightblue")

#Under5Moortality is skewed
hist(clean1$Stunted, 
     main="Histogram of Stunting Under 5", 
     xlab="% Stunted Under5", 
     ylab="Frequency", 
     col="lightblue")

skewness(clean1$Under5Mortality, na.rm = TRUE)
#if bigger than 1 strongly skewed, 0.5 moderately skewed

hist(clean1$Water, 
     main="Histogram of Water Access", 
     xlab="Basic drinking water services access % of population", 
     ylab="Frequency", 
     col="lightblue")

hist(clean1$Quality, 
     main="Histogram of Administrative Quality", 
     xlab="Government Effectiveness", 
     ylab="Frequency", 
     col="lightblue")

hist(clean1$GDPpc, 
     main="Histogram of GDP per capita", 
     xlab="GDP per capita PPP", 
     ylab="Frequency", 
     col="lightblue")
#skewed and will be logged

hist(clean1$UrbanPop, 
     main="Histogram of Urban Population", 
     xlab="Urban Population % of total population", 
     ylab="Frequency", 
     col="lightblue")

hist(clean1$NetODA, 
     main="Histogram of Net Development Asssitance", 
     xlab="Conflict or institutional fragility", 
     ylab="Frequency", 
     col="lightblue")

#CONSIDER LOGS

#will log only under5 mortality and GDPpc

#do correlation and scatterplot
cor(clean1$Under5Mortality, clean1$FEducation)
#can't use cor(clean1) becuase x should be numeric
cor(clean1[, !names(clean1) %in% c("Country","ConflictFragility", "Region")], use = "complete.obs")   #exclude variables non numeric 

datasummary_correlation(clean1)

# PLOT THE REGRESSION
ggplot(clean1, aes(x=FEducation, y=Under5Mortality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Female Education and Child Mortality") +
  labs(y="Under5 Child Mortality (per 1000 live births)", x = "Female Education (years)") +
  theme_minimal() # changes default theme


options(scipen=999) 
firstreg <- lm(Under5Mortality ~ FEducation, data= clean1)
summary(firstreg)

#LOG
clean1$log_Under5Mortality <- log(clean1$Under5Mortality)

hist(clean1$Under5Mortality, 
     main="Histogram of Under 5 Mortality log", 
     xlab="Under5 Mortality per 1000 live births", 
     ylab="Frequency", 
     col="lightblue")


#now run another regression with log you can check hot it changes the effect and interpretation
firstreg2 <- lm(log_Under5Mortality ~ FEducation, data= clean1)
summary(firstreg2)


plot(clean1$FEducation, clean1$log_Under5Mortality)
ggplot(clean1, aes(x=FEducation, y=log_Under5Mortality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Female Education and Under-5 Mortality") +
  labs(y="Under-5 Mortality (log)", x = "Female Education (Mean Years of Schooling)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )


plot(clean1$FEducation, clean1$Under5Mortality)
plot(clean1$FEducation, clean1$log_Under5Mortality)


# MULTIPLE REGRESSION

#we log GDP too
clean1$log_GDPpc <- log(clean1$GDPpc)
plot(clean1$log_GDPpc, clean1$log_Under5Mortality)

model1 <- lm(log_Under5Mortality ~ FEducation + log_GDPpc, data = clean1)
summary(model1)

(exp(-0.08430) - 1) * 100
#only when DV is logged

cor(clean1$FEducation, log(clean1$log_GDPpc), use = "complete.obs")
datasummary_correlation(clean1)
datasummary_correlation(
  clean1 %>%
    select(
      FEducation,
      log_Under5Mortality,
      Stunted,
      Water,
      Quality,
      log_GDPpc,
      UrbanPop,
      NetODA
  ) %>%
    rename(
      "Female Schooling" = FEducation,
      "Log Under-5 Mortality" = log_Under5Mortality,
      "Stunting Rate" = Stunted,
      "Access to Water" = Water,
      "Government Effectiveness" = Quality,
      "Log GDP Per Capita" = log_GDPpc,
      "Urbanization" = UrbanPop,
      "Net ODA" = NetODA  
    )
)


cor(clean1[, !names(clean1) %in% c("Country","ConflictFragility", "Region")], use = "complete.obs")   #exclude variables non numeric 


model2 <- lm(log_Under5Mortality ~ FEducation + log_GDPpc + UrbanPop, data = clean1)
summary(model2)

model3 <- lm(log_Under5Mortality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility, data = clean1)
summary(model3)

model4 <- lm(log_Under5Mortality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean1)
summary(model4)

model5 <- lm(log_Under5Mortality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1)
summary(model5) 

(exp(-0.041470) - 1) * 100 


#OLS assumptions check

# NORMAL DISTURBANCE
plot(firstreg, 2)

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) 
plot(model5) 


# HOMOSCEDASTICITY

# Breusch-Pagan test!
library(lmtest)
bptest(model5)

?bptest

# The null hypothesis for the Breusch-Pagan test is that the variance of 
# the error term is constant, i.e., homoscedasticity
# Pvalue < 0.05, we fail to reject the null/ presence of heteroskedasticity if (p < 0.05) 


#MULTICOLLINEARITY
library(car)
vif(model5)



library(stargazer)
stargazer(model1, model2, model3, model4, model5, type = "text")
stargazer(model1, model2, model3, model4, model5, type = "html", out = "EduMortality.html")

#INFLUENTIAL OBSERVATIONS 
?avPlots
avPlots(model5)

##
outlier_check1.1 <- data.frame(
  std_resid = rstandard(model5),
  stud_resid = rstudent(model5),
  cooks_d = cooks.distance(model5),
  leverage = hatvalues(model5)
)

outlier_check1.1$case <- 1:nrow(outlier_check1.1)

flagged <- subset(
  outlier_check1.1,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5)) |
    leverage > 2 * (length(coef(model5)) / nrow(model.frame(model5)))
)

flagged
model.frame(model5)[flagged$case, ]

4/119
#0.03361345

#8, 45, 56, 66, 82, 97, 110, 111

clean1_no_outliers1 <- clean1[-c(8, 45, 56, 66, 82, 97, 110, 111), ]

model5_no_outliers <- lm(log_Under5Mortality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_outliers1)
summary(model5_no_outliers) 

(exp(-0.0348413 ) - 1) * 100

influence.measures(model5)

dfb <- dfbetas(model5)

n <- nrow(clean1)
threshold <- 2 / sqrt(n)

colnames(dfb)

which(abs(dfb[, "FEducation"]) > threshold) 

dfb[which(apply(abs(dfb) > threshold, 1, any)), ]

plot(dfb)
abline(h = c(-threshold, threshold), lty = 2)

clean1_no_dfbetas1 <- clean1[-c(23, 30,  43,  45,  56,  82,  97, 102, 109), ]

model5_no_dfbetas <- lm(log_Under5Mortality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_dfbetas1)
summary(model5_no_dfbetas) 



#MOVING TO STUNTING 

secondreg <- lm(Stunted ~ FEducation, data= clean1)
summary(secondreg)


model1.2 <- lm(Stunted ~ FEducation + log_GDPpc, data = clean1)
summary(model1.2)


# PLOT THE REGRESSION
ggplot(clean1, aes(x=FEducation, y=Stunted)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Female Education and Stunting") +
  labs(y="Stunting (% of Children Under-5)", x = "Female Education (Mean Years of Schooling)") +
  theme_minimal() +
  theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  panel.grid.minor = element_blank()
)




model2.2 <- lm(Stunted ~ FEducation + log_GDPpc + UrbanPop, data = clean1)
summary(model2.2)

model3.2 <- lm(Stunted ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility, data = clean1)
summary(model3.2)

model4.2 <- lm(Stunted ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean1)
summary(model4.2)

model5.2 <- lm(Stunted ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1)
summary(model5.2) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.2) 


# HOMOSCEDASTICITY 
plot(model5.2, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.2)



#MULTICOLLINEARITY
library(car)
vif(model5.2)


library(stargazer)
stargazer(model1.2, model2.2, model3.2, model4.2, model5.2, type = "text")
stargazer(model1.2, model2.2, model3.2, model4.2, model5.2, type = "html", out = "EduStunted.html")

#INFLUENTIAL OBS 
?avPlots
avPlots(model5.2)

##
outlier_check1.2 <- data.frame(
  std_resid = rstandard(model5.2),
  stud_resid = rstudent(model5.2),
  cooks_d = cooks.distance(model5.2),
  leverage = hatvalues(model5.2)
)

outlier_check1.2$case <- 1:nrow(outlier_check1.2)

flagged <- subset(
  outlier_check1.2,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.2)) |
    leverage > 2 * (length(coef(model5.2)) / nrow(model.frame(model5.2)))
)

flagged
model.frame(model5.2)[flagged$case, ]

4/119
#0.03361345

#4, 42, 68, 84, 107

clean1_no_outliers2 <- clean1[-c(4, 42, 68, 84, 107), ]

model5.2_no_outliers <- lm(Stunted ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_outliers2)
summary(model5.2_no_outliers) 


dfb2 <- dfbetas(model5.2)

n <- nrow(clean1)
threshold <- 2 / sqrt(n)

colnames(dfb2)

which(abs(dfb2[, "FEducation"]) > threshold) 

dfb2[which(apply(abs(dfb2) > threshold, 1, any)), ]

plot(dfb2)
abline(h = c(-threshold, threshold), lty = 2)

clean1_no_dfbetas2 <- clean1[-c(1, 4, 16, 26, 42, 60, 67, 68, 89, 95, 96, 97, 107), ]

model5.2_no_dfbetas <- lm(Stunted ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_dfbetas2)
summary(model5.2_no_dfbetas) 



#MOVING TO WATER ACCESS 

thirdreg <- lm(Water ~ FEducation, data= clean1)
summary(thirdreg)
#one additional year of female education is associated with 3% increase in population with water access


ggplot(clean1, aes(x=FEducation, y=Water)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Female Education and Water Access") +
  labs(y="Water Access (% of population)", x = "Female Education (Mean Years of Schooling)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )




model1.3 <- lm(Water ~ FEducation + log_GDPpc, data = clean1)
summary(model1.3)



model2.3 <- lm(Water ~ FEducation + log_GDPpc + UrbanPop, data = clean1)
summary(model2.3)

model3.3 <- lm(Water ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility, data = clean1)
summary(model3.3)

model4.3 <- lm(Water ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean1)
summary(model4.3)

model5.3 <- lm(Water ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1)
summary(model5.3) 


#OLS checks

# NORMAL DISTURBANCE
plot(thirdreg, 2)


# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
plot(model5.3) 

# HOMOSCEDASTICITY 
plot(model5.3, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.3)

#in case of heteroscedasticity, fix:
library(sandwich)
corrected_errors5.3 <- coeftest(model5.3, vcov = vcovHC(model5.3, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.3


#MULTICOLLINEARITY
library(car)
vif(model5.3)



library(stargazer)
stargazer(model1.3, model2.3, model3.3, model4.3, corrected_errors5.3, type = "text")
stargazer(model1.3, model2.3, model3.3, model4.3, corrected_errors5.3, type = "html", out = "EduWater.html")


#INFLUENTIAL OBS
?avPlots
avPlots(model5.3)

##
outlier_check1.3 <- data.frame(
  std_resid = rstandard(model5.3),
  stud_resid = rstudent(model5.3),
  cooks_d = cooks.distance(model5.3),
  leverage = hatvalues(model5.3)
)

outlier_check1.3$case <- 1:nrow(outlier_check1.3)

flagged <- subset(
  outlier_check1.3,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.3)) |
    leverage > 2 * (length(coef(model5.3)) / nrow(model.frame(model5.3)))
)

flagged
model.frame(model5.3)[flagged$case, ]

4/119
#0.03361345

#21, 26, 30, 67, 84, 97

clean1_no_outliers3 <- clean1[-c(21, 26, 30, 67, 84, 97), ]

model5.3_no_outliers <- lm(Water ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_outliers3)
summary(model5.3_no_outliers) 




dfb3 <- dfbetas(model5.3)

n <- nrow(clean1)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb3)

which(abs(dfb3[, "FEducation"]) > threshold) 

dfb3[which(apply(abs(dfb3) > threshold, 1, any)), ]

plot(dfb3)
abline(h = c(-threshold, threshold), lty = 2)

clean1_no_dfbetas3 <- clean1[-c(26, 30, 55, 67, 84, 89, 91, 95, 97, 109, 117), ]

model5.3_no_dfbetas <- lm(Water ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_dfbetas3)
summary(model5.3_no_dfbetas) 




#GOVERNMENT EFFECTIVENESS

fourthreg <- lm(Quality ~ FEducation, data= clean1)
summary(fourthreg)


ggplot(clean1, aes(x=FEducation, y=Quality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Female Education and Government Effectiveness") +
  labs(y="Government Effectiveness", x = "Female Education (Mean Years of Schooling)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

model1.4 <- lm(Quality ~ FEducation + log_GDPpc, data = clean1)
summary(model1.4)


model2.4 <- lm(Quality ~ FEducation + log_GDPpc + UrbanPop, data = clean1)
summary(model2.4)

model3.4 <- lm(Quality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility, data = clean1)
summary(model3.4)


model4.4 <- lm(Quality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean1)
summary(model4.4)

model5.4 <- lm(Quality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1)
summary(model5.4) 



#OLS checks

# NORMAL DISTURBANCE
plot(fourthreg, 2)

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
# Now, four plots will show up in the same window:
plot(model5.4) 


# HOMOSCEDASTICITY 
plot(model5.4, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.4)



#MULTICOLLINEARITY
library(car)
vif(model5.4)


library(stargazer)
stargazer(model1.3, model2.3, model3.3, model4.3, corrected_errors5.3, type = "text")
stargazer(model1.3, model2.3, model3.3, model4.3, corrected_errors5.3, type = "html", out = "EduWater.html")



#INFLUENTIAL OBS 
?avPlots
avPlots(model5.3)

##
outlier_check1.4 <- data.frame(
  std_resid = rstandard(model5.4),
  stud_resid = rstudent(model5.4),
  cooks_d = cooks.distance(model5.4),
  leverage = hatvalues(model5.4)
)

outlier_check1.4$case <- 1:nrow(outlier_check1.4)

flagged <- subset(
  outlier_check1.4,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.4)) |
    leverage > 2 * (length(coef(model5.4)) / nrow(model.frame(model5.4)))
)

flagged
model.frame(model5.4)[flagged$case, ]

4/119
#0.03361345

#11, 35, 38, 45, 48, 53, 111

clean1_no_outliers4 <- clean1[-c(11, 35, 38, 45, 48, 53, 111), ]

model5.4_no_outliers <- lm(Quality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_outliers4)
summary(model5.4_no_outliers) 


dfb4 <- dfbetas(model5.4)

n <- nrow(clean1)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb4)

which(abs(dfb4[, "FEducation"]) > threshold) 

dfb4[which(apply(abs(dfb4) > threshold, 1, any)), ]

plot(dfb4)
abline(h = c(-threshold, threshold), lty = 2)

clean1_no_dfbetas4 <- clean1[-c(1, 4, 11, 38, 42, 53, 57, 58, 89, 91, 97 ), ]

model5.4_no_dfbetas <- lm(Quality ~ FEducation + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean1_no_dfbetas4)
summary(model5.4_no_dfbetas) 






library(modelsummary)
library(sandwich)

modelsummary(
  list(
    "Under-5 Mortality (log)" = model5,
    "Stunting" = model5.2,
    "Water Access" = model5.3,
    "Government Effectiveness" = model5.4
  ),
  vcov = list(
    NULL,
    NULL,
    vcovHC(model5.3, type = "HC1"),
    NULL
  ),
  coef_rename = c(
    "FEducation" = "Female Education (years)",
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
  output = "FemaleEducationRegressionTables1.png"
)







