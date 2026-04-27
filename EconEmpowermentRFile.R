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
data <- read_excel("C:/Users/miria/Miriam/HERTIE/TESI/Dataset.xlsx")
head(data)
str(data)


#now create clean dataset for education IV so I don't override 



subset2 <- data %>%
  select(Country,
         WEmp2_WBL, 
         WEmp2_WBL_WPEA,
         WEmp2_WBL_Work, 
         WEmp2_WBL_Pay, 
         WEmp2_WBL_Enterpreneurship,
         WEmp2_WBL_Assets, 
         WEmp2_Laborforce_participation,
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


clean2 <- subset2 %>%
  filter(!is.na(WEmp2_WBL),
         !is.na(WEmp2_WBL_WPEA),
         !is.na(WEmp2_WBL_Work),
         !is.na(WEmp2_WBL_Pay),
         !is.na(WEmp2_WBL_Enterpreneurship),
         !is.na(WEmp2_WBL_Assets),
         !is.na(WEmp2_Laborforce_participation),
         !is.na(Health_mortality_under5), 
         !is.na(Stunted_under5),
         !is.na(Water_access),
         !is.na(Mean_year_schooling_total),
         !is.na(Government_effectiveness),
         !is.na(GDP_percapita_PPP),
         !is.na(Urban_population),
         !is.na(Conflict_Fragility),
         !is.na(Net_ODAreceived))



clean2 <- clean2 %>%
  rename(
    WPEA = WEmp2_WBL_WPEA,
    WBL = WEmp2_WBL,
    Work = WEmp2_WBL_Work,
    Pay = WEmp2_WBL_Pay, 
    Enterpreneurship = WEmp2_WBL_Enterpreneurship,
    Assets = WEmp2_WBL_Assets,
    LaborForce = WEmp2_Laborforce_participation,
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

summary(clean2)


describe(clean2)

clean2$ConflictFragility <- factor(clean2$ConflictFragility,
                                   levels = c(0, 1),
                                   labels = c("None", "Conflict or Fragile"))

clean2$Region <- factor(clean2$Region,
                        levels = c("EUCA", "SSA", "SA", "MENA", "LAC", "EAP"))

table(clean2$Region)
table(clean2$ConflictFragility)



descriptive_data2 <- clean2 %>%
  select(
    WPEA,
    LaborForce,
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
    'WPEA' = WPEA,
    'FLFP' = LaborForce,
    'Under-5 Mortality' = Under5Mortality,
    'Stunting Rate' = Stunted,
    'Access to Water' = Water,
    'Education' = Education, 
    'Government Effectiveness' = Quality,
    'GDP per Capita' = GDPpc,
    'Urbanization' = UrbanPop,
    'Net ODA' = NetODA
  )


datasummary_skim(descriptive_data2, fmt = 2)


hist(clean2$WPEA)
hist(clean2$WPEA, 
     main="Histogram of WPEA", 
     xlab="WPEA", 
     ylab="Frequency", 
     col="lightblue")


hist(clean2$WBL, 
     main="Histogram of WBL", 
     xlab="WPEA", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$Work, 
     main="Histogram of Equality in Work", 
     xlab="Work", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$Pay, 
     main="Histogram of Equality in Pay", 
     xlab="Pay", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$Enterpreneurship, 
     main="Histogram of Equality in Enterpreneurship", 
     xlab="Enterpreneurship", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$Assets, 
     main="Histogram of Equality in Assets", 
     xlab="Assets", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$LaborForce, 
     main="Histogram of Female Labor Force Participation", 
     xlab="FLabor Force Participation", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$Under5Mortality, 
     main="Histogram of Under 5 Mortality", 
     xlab="Under5 Mortality per 1000 live births", 
     ylab="Frequency", 
     col="lightblue")

#Under5Moortality is skewed

hist(clean2$Stunted, 
     main="Histogram of Stunting Under 5", 
     xlab="% Stunted Under5", 
     ylab="Frequency", 
     col="lightblue")


#can also use this for skewness to decided if log or not
skewness(clean2$Under5Mortality, na.rm = TRUE)
#if bigger than 1 strongly skewed, 0.5 moderately skewed


hist(clean2$Water, 
     main="Histogram of Water Access", 
     xlab="Basic drinking water services access % of population", 
     ylab="Frequency", 
     col="lightblue")


hist(clean2$Education, 
     main="Histogram of Mean Years of Schooling", 
     xlab="Education", 
     ylab="Frequency", 
     col="lightblue")


hist(clean2$Quality, 
     main="Histogram of Administrative Quality", 
     xlab="Government Effectiveness", 
     ylab="Frequency", 
     col="lightblue")

hist(clean2$GDPpc, 
     main="Histogram of GDP per capita", 
     xlab="GDP per capita PPP", 
     ylab="Frequency", 
     col="lightblue")
#skewed and will be logged

hist(clean2$UrbanPop, 
     main="Histogram of Urban Population", 
     xlab="Urban Population % of total population", 
     ylab="Frequency", 
     col="lightblue")


hist(clean2$NetODA, 
     main="Histogram of Net Development Asssitance", 
     xlab="Conflict or institutional fragility", 
     ylab="Frequency", 
     col="lightblue")

#CONSIDER LOGS

#will log only under5 mortality and GDPpc

#do correlation and scatterplot
cor(clean2$Under5Mortality, clean2$WPEA)
#can't use cor(clean1) becuase x should be numeric
cor(clean2[, !names(clean2) %in% c("Country","ConflictFragility", "Region")], use = "complete.obs")   #exclude variables non numeric 

datasummary_correlation(clean2)
plot(clean2$WPEA, clean2$Under5Mortality)
text(clean2$WPEA, clean2$Under5Mortality, labels = clean2$Country, cex = 0.7, pos = 4) #allows me to put country names so can check outlier

# PLOT THE REGRESSION
ggplot(clean2, aes(x=WPEA, y=Under5Mortality)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("WPEA and Child Mortality") +
  labs(y="Under 5 Child Mortality", x = "WPEA Index") +
  theme_minimal()  +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )



options(scipen=999) 
fifthreg <- lm(Under5Mortality ~ WPEA, data= clean2)
summary(fifthreg)


#LOG
clean2$log_Under5Mortality <- log(clean2$Under5Mortality)

hist(clean2$log_Under5Mortality, 
     main="Histogram of Under 5 Mortality log", 
     xlab="Under5 Mortality per 1000 live births %", 
     ylab="Frequency", 
     col="lightblue")


fifthreg2 <- lm(log_Under5Mortality ~ WPEA, data= clean2)
summary(fifthreg2)


# MULTIPLE REGRESSION

#we log GDP too
clean2$log_GDPpc <- log(clean2$GDPpc)
plot(clean2$log_GDPpc, clean2$log_Under5Mortality)


cor(clean2$WPEA, log(clean2$log_GDPpc), use = "complete.obs")
datasummary_correlation(clean2)
datasummary_correlation(
  clean2 %>%
    select(
      WPEA,
      LaborForce,
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
      "WPEA" = WPEA,
      "FLFP" = LaborForce,
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



model1.5 <- lm(log_Under5Mortality ~ WPEA + log_GDPpc, data = clean2)
summary(model1.5)

(exp(0.001246) - 1) * 100
#only when DV is logged


model2.5 <- lm(log_Under5Mortality ~ WPEA + log_GDPpc + UrbanPop, data = clean2)
summary(model2.5)

model3.5 <- lm(log_Under5Mortality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility, data = clean2)
summary(model3.5)

model4.5 <- lm(log_Under5Mortality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean2)
summary(model4.5)

model5.5 <- lm(log_Under5Mortality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.5) 


(exp(0.001390) - 1) * 100


#OLS assumptions check

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
plot(model5.5) 
 

# HOMOSCEDASTICITY
plot(model5.5, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.5)


#MULTICOLLINEARITY
library(car)
vif(model5)


library(stargazer)
stargazer(model1.5, model2.5, model3.5, model4.5, model5.5, type = "text")
stargazer(model1.5, model2.5, model3.5, model4.5, model5.5, type = "html", out = "WPEAMortality.html")



#INFLUENTIAL OBS/OUTLIERS
#Are there influential cases that are affecting my substantive results?”
?avPlots
avPlots(model5)


##
outlier_check <- data.frame(
  std_resid = rstandard(model5.5),
  stud_resid = rstudent(model5.5),
  cooks_d = cooks.distance(model5.5),
  leverage = hatvalues(model5.5)
)

outlier_check$case <- 1:nrow(outlier_check)

flagged <- subset(
  outlier_check,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.5)) |
    leverage > 2 * (length(coef(model5.5)) / nrow(model.frame(model5.5)))
)

flagged
model.frame(model5.5)[flagged$case, ]

#if just cooks

cooks.distance(model5.5)
4/115
#0.03478261
#so it is observation 8, 11, 45, 65,95 


clean2_no_outliers <- clean2[-c(8, 11, 45, 65,95), ]

model5.5_no_outliers <- lm(log_Under5Mortality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers)
summary(model5.5_no_outliers) 



dfb5 <- dfbetas(model5.5)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb5)

which(abs(dfb5[, "WPEA"]) > threshold) 

dfb5[which(apply(abs(dfb5) > threshold, 1, any)), ]

plot(dfb5)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas5 <- clean2[-c( 7, 8, 14, 35, 45, 97), ]

model5.5_no_dfbetas <- lm(log_Under5Mortality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas5)
summary(model5.5_no_dfbetas) 



#MOVING TO STUNTING 

reg6 <- lm(Stunted ~ WPEA, data= clean2)
summary(reg6)
#not significant from the beginning

model1.6 <- lm(Stunted ~ WPEA + log_GDPpc, data = clean2)
summary(model1.6)

model5.6 <- lm(Stunted ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.6) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
plot(model5.6) 

# HOMOSCEDASTICITY 
plot(model5.6, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.6)

# the Breusch-Pagan test indicates no heteroskedasticity (0.8343 > 0.05)

#MULTICOLLINEARITY
library(car)
vif(model5.6)

library(stargazer)
stargazer(model1.2, model2.2, model3.2, model4.2, model5.2, type = "text")
stargazer(model1.2, model2.2, model3.2, model4.2, model5.2, type = "html", out = "EduStunted.html")

#INFLUENTIAL OBS
?avPlots
avPlots(model5.2)


which(abs(rstandard(model5.6)) > 2)
outliers2 <- which(abs(rstandard(model5.6)) > 2)
clean2$Country[outliers2]


##
outlier_check1 <- data.frame(
  std_resid = rstandard(model5.6),
  stud_resid = rstudent(model5.6),
  cooks_d = cooks.distance(model5.6),
  leverage = hatvalues(model5.6)
)

outlier_check1$case <- 1:nrow(outlier_check1)

flagged <- subset(
  outlier_check1,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.6)) |
    leverage > 2 * (length(coef(model5.6)) / nrow(model.frame(model5.6)))
)

flagged
model.frame(model5.6)[flagged$case, ]

#if just cooks

cooks.distance(model5.6)
4/115
#0.03478261

#1, 4, 17, 42, 64, 82, 95, 99, 103, 105

clean2_no_outliers1 <- clean2[-c(1, 4, 17, 42, 64, 82, 95, 99, 103, 105), ]

model5.6_no_outliers <- lm(Stunted ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers1)
summary(model5.6_no_outliers) 



dfb6 <- dfbetas(model5.6)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb6)

which(abs(dfb6[, "WPEA"]) > threshold) 

dfb6[which(apply(abs(dfb6) > threshold, 1, any)), ]

plot(dfb6)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas6 <- clean2[-c( 14,  17, 42, 64, 78, 103, 105 ), ]

model5.6_no_dfbetas <- lm(Stunted ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas6)
summary(model5.6_no_dfbetas) 


#WATER ACCESS

reg7 <- lm(Water ~ WPEA, data= clean2)
summary(reg7)


model1.7 <- lm(Water ~ WPEA + log_GDPpc, data = clean2)
summary(model1.7)

model5.7 <- lm(Water ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.7) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
plot(model5.7) 


# HOMOSCEDASTICITY 
plot(model5.7, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.7)

# the Breusch-Pagan test indicates heteroskedasticity (0.005253 > 0.05)
library(sandwich)
corrected_errors5.7 <- coeftest(model5.7, vcov = vcovHC(model5.7, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.7


#MULTICOLLINEARITY
library(car)
vif(model5.7)


#INFLUENTIAL OBS 

##
outlier_check2 <- data.frame(
  std_resid = rstandard(model5.7),
  stud_resid = rstudent(model5.7),
  cooks_d = cooks.distance(model5.7),
  leverage = hatvalues(model5.7)
)

outlier_check2$case <- 1:nrow(outlier_check2)

flagged <- subset(
  outlier_check2,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.7)) |
    leverage > 2 * (length(coef(model5.7)) / nrow(model.frame(model5.7)))
)


flagged
model.frame(model5.7)[flagged$case, ]

#if just cooks

cooks.distance(model5.7)
4/115
#0.03478261

#21, 26, 66, 68, 82

clean2_no_outliers2 <- clean2[-c(21, 26, 66, 68, 82), ]

model5.7_no_outliers <- lm(Water ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers2)
summary(model5.7_no_outliers) 



dfb7 <- dfbetas(model5.7)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb7)

which(abs(dfb7[, "WPEA"]) > threshold) 

dfb7[which(apply(abs(dfb7) > threshold, 1, any)), ]

plot(dfb7)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas7 <- clean2[-c( 21, 26, 67, 68 ), ]

model5.7_no_dfbetas <- lm(Water ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas7)
summary(model5.7_no_dfbetas) 



#EDUCATION

reg8 <- lm(Education ~ WPEA, data= clean2)
summary(reg8)


model1.8 <- lm(Education ~ WPEA + log_GDPpc, data = clean2)
summary(model1.8)


model5.8 <- lm(Education ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.8) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
plot(model5.8) 

# HOMOSCEDASTICITY 
plot(model5.8, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.8)


#MULTICOLLINEARITY
library(car)
vif(model5.8)


#INFLUENTIAL OBS 

##
outlier_check3 <- data.frame(
  std_resid = rstandard(model5.8),
  stud_resid = rstudent(model5.8),
  cooks_d = cooks.distance(model5.8),
  leverage = hatvalues(model5.8)
)

outlier_check3$case <- 1:nrow(outlier_check3)

flagged <- subset(
  outlier_check3,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.8)) |
    leverage > 2 * (length(coef(model5.8)) / nrow(model.frame(model5.8)))
)

flagged
model.frame(model5.8)[flagged$case, ]

#if just cooks

cooks.distance(model5.8)
4/115
#0.03478261

#26, 30, 45, 87, 95, 100, 105, 107, 113, 115

clean2_no_outliers3 <- clean2[-c(26, 30, 45, 87, 95, 100, 105, 107, 113, 115), ]

model5.8_no_outliers <- lm(Education ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers3)
summary(model5.8_no_outliers) 


dfb8 <- dfbetas(model5.8)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb8)

which(abs(dfb8[, "WPEA"]) > threshold) 

dfb8[which(apply(abs(dfb8) > threshold, 1, any)), ]

plot(dfb8)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas8 <- clean2[-c( 35, 42,  67,  78,  94, 105, 113, 115), ]

model5.8_no_dfbetas <- lm(Education ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas8)
summary(model5.8_no_dfbetas) 





#GOVERNMENT EFFECTIVENESS

reg9 <- lm(Quality ~ WPEA, data= clean2)
summary(reg9)

model1.9 <- lm(Quality ~ WPEA + log_GDPpc, data = clean2)
summary(model1.9)

model2.9 <- lm(Quality ~ WPEA + log_GDPpc + UrbanPop, data = clean2)
summary(model2.9)

model3.9 <- lm(Quality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility, data = clean2)
summary(model3.9)

model4.9 <- lm(Quality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA, data = clean2)
summary(model4.9)

model5.9 <- lm(Quality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.9) 


#OLS checks

# NORMAL DISTURBANCE
plot(firstreg, 2)
# the closer the residuals are to the diagonal line, the more normal the residuals 

# You can see them all at once by running this line first:
par(mfrow = c(2, 2)) # Create 2x2 grid in plot window
plot(model5.9) 


# HOMOSCEDASTICITY 
plot(model5.9, 3) 

# Breusch-Pagan test!
library(lmtest)
bptest(model5.9)

#MULTICOLLINEARITY
library(car)
vif(model5.9)


#OUTLIERS 

##
outlier_check4 <- data.frame(
  std_resid = rstandard(model5.9),
  stud_resid = rstudent(model5.9),
  cooks_d = cooks.distance(model5.9),
  leverage = hatvalues(model5.9)
)

outlier_check4$case <- 1:nrow(outlier_check4)

flagged <- subset(
  outlier_check4,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.9)) |
    leverage > 2 * (length(coef(model5.9)) / nrow(model.frame(model5.9)))
)

flagged
model.frame(model5.9)[flagged$case, ]

#if just cooks

cooks.distance(model5.9)
4/115
#0.03478261

#11, 35, 38, 45, 48, 53, 65, 78, 99

clean2_no_outliers4 <- clean2[-c(11, 35, 38, 45, 48, 53, 65, 78, 99), ]

model5.9_no_outliers <- lm(Quality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers4)
summary(model5.9_no_outliers) 



dfb9 <- dfbetas(model5.9)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb9)

which(abs(dfb9[, "WPEA"]) > threshold) 

dfb9[which(apply(abs(dfb9) > threshold, 1, any)), ]

plot(dfb9)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas9 <- clean2[-c(14, 35, 38, 42, 45, 52, 64, 67, 74, 78, 86), ]

model5.9_no_dfbetas <- lm(Quality ~ WPEA + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas9)
summary(model5.9_no_dfbetas) 






#TRYING WITH LABOR FORCE PARTICIPATION


model5.11 <- lm(log_Under5Mortality ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.11) 

##
outlier_check5.1 <- data.frame(
  std_resid = rstandard(model5.11),
  stud_resid = rstudent(model5.11),
  cooks_d = cooks.distance(model5.11),
  leverage = hatvalues(model5.11)
)

outlier_check5.1$case <- 1:nrow(outlier_check5.1)

flagged <- subset(
  outlier_check5.1,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.11)) |
    leverage > 2 * (length(coef(model5.11)) / nrow(model.frame(model5.11)))
)

flagged
model.frame(model5.11)[flagged$case, ]

#if just cooks

cooks.distance(model5.11)
4/115
#0.03478261

#8, 11, 45, 65, 72, 95, 107

clean2_no_outliers5.1 <- clean2[-c(8, 11, 45, 65, 72, 95, 107), ]

model5.11_no_outliers <- lm(log_Under5Mortality ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers5.1)
summary(model5.11_no_outliers) 



dfb11 <- dfbetas(model5.11)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb11)

which(abs(dfb11[, "LaborForce"]) > threshold) 

dfb11[which(apply(abs(dfb11) > threshold, 1, any)), ]

plot(dfb11)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas11 <- clean2[-c(11,  17,  37,  79,  88,  92,  93,  97, 100, 107 ), ]

model5.11_no_dfbetas <- lm(log_Under5Mortality ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas11)
summary(model5.11_no_dfbetas) 




model5.12 <- lm(Stunted ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.12) 


par(mfrow = c(2, 2))
plot(model5.12) 

# HOMOSCEDASTICITY 
# Breusch-Pagan test!
library(lmtest)
bptest(model5.12)


#MULTICOLLINEARITY
library(car)
vif(model5.12)


#INFLUENTIAL OBS 

##
outlier_check5 <- data.frame(
  std_resid = rstandard(model5.12),
  stud_resid = rstudent(model5.12),
  cooks_d = cooks.distance(model5.12),
  leverage = hatvalues(model5.12)
)

outlier_check5$case <- 1:nrow(outlier_check5)

flagged <- subset(
  outlier_check5,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.12)) |
    leverage > 2 * (length(coef(model5.12)) / nrow(model.frame(model5.12)))
)

flagged
model.frame(model5.12)[flagged$case, ]

#if just cooks

cooks.distance(model5.12)
4/115
#0.03478261

#1, 4, 17, 42, 48, 64, 82, 95, 99, 103, 105

clean2_no_outliers5 <- clean2[-c(1, 4, 17, 42, 48, 64, 82, 95, 99, 103, 105), ]

model5.12_no_outliers <- lm(Stunted ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers5)
summary(model5.12_no_outliers) 



dfb12 <- dfbetas(model5.12)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb12)

which(abs(dfb12[, "LaborForce"]) > threshold) 

dfb12[which(apply(abs(dfb12) > threshold, 1, any)), ]

plot(dfb12)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas12 <- clean2[-c(1,  4, 17, 42, 87, 97  ), ]

model5.12_no_dfbetas <- lm(Stunted ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas12)
summary(model5.12_no_dfbetas) 





model5.13 <- lm(Water ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.13) 



ggplot(clean2, aes(x=LaborForce, y=Water)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)+  # Add linear regression line (by default includes 95% confidence region)
  ggtitle("Female Labor Force Participation and Water Access") +
  labs(y="Water Access (% population)", x = "Female Labor Force Participation (% of female population 15+)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )



#OLS

par(mfrow = c(2, 2)) 
plot(model5.13) 

# HOMOSCEDASTICITY 
# Breusch-Pagan test!
library(lmtest)
bptest(model5.13)


library(sandwich)
corrected_errors5.13 <- coeftest(model5.13, vcov = vcovHC(model5.13, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.13


#MULTICOLLINEARITY
library(car)
vif(model5.13)


##
outlier_check6 <- data.frame(
  std_resid = rstandard(model5.13),
  stud_resid = rstudent(model5.13),
  cooks_d = cooks.distance(model5.13),
  leverage = hatvalues(model5.13)
)

outlier_check6$case <- 1:nrow(outlier_check6)

flagged <- subset(
  outlier_check6,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.13)) |
    leverage > 2 * (length(coef(model5.13)) / nrow(model.frame(model5.13)))
)

flagged
model.frame(model5.13)[flagged$case, ]

4/115
#0.03478261

#21, 26, 45, 66, 82

clean2_no_outliers6 <- clean2[-c(21, 26, 45, 66, 82), ]

model5.13_no_outliers <- lm(Water ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers6)
summary(model5.13_no_outliers) 



library(sandwich)
corrected_errors5.13_no_outliers <- coeftest(model5.13_no_outliers, vcov = vcovHC(model5.13_no_outliers, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.13_no_outliers


dfb13 <- dfbetas(model5.13)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb13)

which(abs(dfb13[, "LaborForce"]) > threshold) 

dfb13[which(apply(abs(dfb13) > threshold, 1, any)), ]

plot(dfb13)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas13 <- clean2[-c(17, 21, 60, 67, 79, 82, 88 ), ]

model5.13_no_dfbetas <- lm(Water ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas13)
summary(model5.13_no_dfbetas) 






model5.14 <- lm(Education ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.14) 


outlier_check6.1 <- data.frame(
  std_resid = rstandard(model5.14),
  stud_resid = rstudent(model5.14),
  cooks_d = cooks.distance(model5.14),
  leverage = hatvalues(model5.14)
)

outlier_check6.1$case <- 1:nrow(outlier_check6.1)

flagged <- subset(
  outlier_check6.1,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.14)) |
    leverage > 2 * (length(coef(model5.14)) / nrow(model.frame(model5.14)))
)

flagged
model.frame(model5.14)[flagged$case, ]

4/115
#0.03478261

#11, 26, 45, 87, 95, 100, 105, 107, 113

clean2_no_outliers6.1 <- clean2[-c(11, 26, 45, 87, 95, 100, 105, 107, 113), ]

model5.14_no_outliers <- lm(Education ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers6.1)
summary(model5.14_no_outliers) 




dfb14 <- dfbetas(model5.14)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb14)

which(abs(dfb14[, "LaborForce"]) > threshold) 

dfb14[which(apply(abs(dfb14) > threshold, 1, any)), ]

plot(dfb14)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas14 <- clean2[-c(10,  11,  19,  43,  67,  87,  89,  93, 100, 107 ), ]

model5.14_no_dfbetas <- lm(Education ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas14)
summary(model5.14_no_dfbetas) 






model5.15 <- lm(Quality ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.15) 



#OLS

par(mfrow = c(2, 2)) 
plot(model5.15) 

# HOMOSCEDASTICITY 
# Breusch-Pagan test!
library(lmtest)
bptest(model5.15)

#MULTICOLLINEARITY
library(car)
vif(model5.15)

##
outlier_check7 <- data.frame(
  std_resid = rstandard(model5.15),
  stud_resid = rstudent(model5.15),
  cooks_d = cooks.distance(model5.15),
  leverage = hatvalues(model5.15)
)

outlier_check7$case <- 1:nrow(outlier_check7)

flagged <- subset(
  outlier_check7,
  abs(stud_resid) > 2 |
    cooks_d > 4 / nrow(model.frame(model5.15)) |
    leverage > 2 * (length(coef(model5.15)) / nrow(model.frame(model5.15)))
)

flagged
model.frame(model5.15)[flagged$case, ]

4/115
#0.03478261

#35, 45, 48, 53, 65, 87, 99

clean2_no_outliers7 <- clean2[-c(35, 45, 48, 53, 65, 87, 99), ]

model5.15_no_outliers <- lm(Quality ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_outliers7)
summary(model5.15_no_outliers) 


dfb15 <- dfbetas(model5.15)

n <- nrow(clean2)
threshold <- 2 / sqrt(n)
threshold

colnames(dfb15)

which(abs(dfb15[, "LaborForce"]) > threshold) 

dfb15[which(apply(abs(dfb15) > threshold, 1, any)), ]

plot(dfb15)
abline(h = c(-threshold, threshold), lty = 2)

clean2_no_dfbetas15 <- clean2[-c(4, 37, 60, 62, 67, 74, 78, 87, 89 ), ]

model5.15_no_dfbetas <- lm(Quality ~ LaborForce + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2_no_dfbetas15)
summary(model5.15_no_dfbetas) 



library(stargazer)
stargazer(model5.5, model5.6, corrected_errors5.7, model5.8, model5.9, model5.12, corrected_errors5.13, type = "text")
stargazer(model5.5, model5.6, corrected_errors5.7, model5.8, model5.9, model5.12, corrected_errors5.13, type = "html", out = "Economic Empowerment")
#non funziona con i corrected errors


library(modelsummary)
library(sandwich)

modelsummary(
  list(
    "Under-5 Mortality (log)" = model5.5,
    "Stunting" = model5.6,
    "Water Access" = model5.7,
    "Education" = model5.8,
    "Government Effectiveness" = model5.9
  ),
  vcov = list(
    NULL,
    NULL,
    vcovHC(model5.7, type = "HC1"),
    NULL,
    NULL
  ),
  coef_rename = c(
    "WPEA" = "WPEA Index",
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
  output = "Economic Empowerment1.png"
)




modelsummary(
  list(
    "Under-5 Mortality (log)" = model5.11,
    "Stunting" = model5.12,
    "Water Access" = model5.13,
    "Education" = model5.14,
    "Government Effectiveness" = model5.15
  ),
  vcov = list(
    NULL,
    NULL,
    vcovHC(model5.13, type = "HC1"),
    NULL,
    NULL
  ),
  coef_rename = c(
    "LaborForce" = "FLFP (% of female population ages 15+)",
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
  output = "LaborForceRegressionTables1.png"
)


#so I tried to run the analysis also with WBL and the single components of my subindex WPEA and the following appear to have a significant effect
model5.16 <- lm(Water ~ WBL + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.16) 

library(lmtest)
bptest(model5.16)

library(sandwich)
corrected_errors5.16 <- coeftest(model5.16, vcov = vcovHC(model5.16, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.16

model5.17 <- lm(Water ~ Work + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.17)

# Breusch-Pagan test!
library(lmtest)
bptest(model5.17)

library(sandwich)
corrected_errors5.17 <- coeftest(model5.17, vcov = vcovHC(model5.17, type = "HC1")) #I should run this coeftest for every regression I will run later (if needed ofc but to say that this does not make standard errors robust forever but just for that regression)
corrected_errors5.17



model5.18 <- lm(Quality ~ WBL + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.18)

model5.19 <- lm(Quality ~ Work + log_GDPpc + UrbanPop + ConflictFragility + NetODA + Region, data = clean2)
summary(model5.19) 


modelsummary(
  list(
    "Water Access" = model5.16,
    "Government Effectiveness" = model5.18,
    "Water Access" = model5.17,
    "Government Effectiveness" = model5.19
  ),
  vcov = list(
    vcovHC(model5.16, type = "HC1"),
    NULL,
    vcovHC(model5.17, type = "HC1"),
    NULL
  ),
  coef_rename = c(
    "WBL" = "WBL",
    "Work" = "Work",
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
  output = "WBLWorkRegressionTables1.png"
)

