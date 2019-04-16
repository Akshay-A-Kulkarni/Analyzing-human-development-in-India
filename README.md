# Analyzing-human-development-in-India
Data Science Project conducted under the supervision of Prof. Kylie Bemis at Northeastern

---
title: "Untitled"
author: "Akshay Kulkarni 001445074"
date: "November 21, 2018"
output:
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}

# Loading all the required libraries 

library(tidyverse)
library(RODBC)
library(ineq)     # for Lc and Gini
library(ggplot2)
library(scales)
library(showtext) # for fonts
library(stringr)  # for str_wrap
library(grid)
library(ROCR)
library(ggcorrplot)
library(modelr)
library(caret)
library(leaps)
library(nnet)
library(randomForest)
library(dplyr)


```




```{r warning=FALSE}


load("C:/Users/Akshay/Desktop/ICPSR_36151/DS0003/36151-0003-Data.rda")



women <- da36151.0003

women <- filter(women, EW6 >= 25 & EW6 <= 59)
women <- filter(women, RO6 == "(1) Married 1" | RO6 == "(0) married, spouse absent")
women <- women %>% filter(!is.na(GR46), GR46 != '-')

women <- dplyr::select(women, HHID, PERSONID, IDHH, IDPERSON, EW5, EW6, EW8, EW9, EW10, GR46, ID11, ID13, 
                HHEDUC, HHEDUCM, HHEDUCF, NCHILDM, NCHILDF, SPED6, SPED2, SPED3, SPRO5, INCCROP, INCAGPROP, 
                INCANIMAL, INCAG, INCBUS, INCBUSCALC, INCOTHER, INCOME, INCOMEPC, INCNONAG, INCAGLAB, INCSALARY, 
                INCNREGA,RSUNEARN, GR48, GR46B,GR46A)

women <- mutate(women, education = ifelse(EW8 == "(00) none 0", "illiterate", 
                                          ifelse(EW8 == "(01) 1st class 1" | EW8 == "(02) 2nd class 2" | EW8 == "(03) 3rd class 3" | EW8 == "(04) 4th class 4", "preprimary", 
                                                 ifelse(EW8 == "(05) 5th class 5" | EW8 == "(06) 6th class 6" | EW8 == "(07) 7th class 7" | EW8 == "(08) 8th class 8" | EW8 == "(09) 9th class 9", "primary & postprimary", 
                                                        ifelse(EW8 == "(10) Secondary 10" | EW8 == "(11) 11th Class 11", "secondary", 
                                                               ifelse(EW8 == "(12) High Secondary 12" | EW8 == "(13) 1 year post-secondary 13" | EW8 == "(14) 2 years post-secondary 14", "higher secondary", 
                                                                      ifelse(EW8 == "(15) Bachelors 15" | EW8 == "(16) Above Bachelors 16", "college graduate or higher", "-")))))))
View(head(women, 20))

women$education <- factor(women$education, levels = c("illiterate","preprimary" ,"primary & postprimary" , "secondary", "higher secondary", "college graduate or higher"))
ggplot(data=women %>% filter(!is.na(education), !is.na(GR46B), GR46B == '(1) Yes 1'), aes(x = education)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Education level", y = "Women employed in labor force")


summary <- women %>%
  filter(!is.na(education), !is.na(RSUNEARN)) %>%
  group_by(education) %>%
  summarise(other_household_income = mean(RSUNEARN))

summary



ed_prop <- prop.table(table(women$education))

ggplot(data = summary) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_histogram(aes(x= education, y = other_household_income,group = 1),stat="identity") +
  geom_line(aes(x= education, y = other_household_income,group = 1),stat="identity",color="red",lwd=2) +
  labs(x = "Education level", y = "Other Income")

ggplot(data = women %>% filter(!is.na(education), !is.na(GR48))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_bar(aes(x= education, fill= GR48), position="fill") +
  labs(x = "Education level", y = "Willingness")


ggplot(data = women %>% filter(!is.na(education), !is.na(GR46))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_bar(aes(x= education, fill= GR46), position="dodge") +
  labs(x = "Education level", y = "Ever worked in their life")

ggplot(data = women %>% filter(!is.na(education), !is.na(ID13), !is.na(GR46B), GR46B == '(1) Yes 1')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_bar(aes(x= education, fill = ID13),show.legend = FALSE) + 
  facet_wrap(~ID13) +
  labs(x = "Education level", y = "Caste")





# ````````````````````````````````````````````````````````````````````````````````````



set.seed(1)
women_par <- resample_partition(women, c(train = 0.8, test = 0.2))



# Converting partitions into tibbles

train_partition <-  as_tibble(women_par$train)
test_partition <- as_tibble(women_par$test)



# Checking for factors with single/ 1 levels ( DROP = Factor has only one level)

(l <- sapply(women, function(x) is.factor(x)))
m <- women[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

# No factors in dataset with single levels which mightve caused contrast error while running the model.



# GLM / Logistic Regression

fit <- glm(GR46 ~(education), data = women_par$train, family = binomial(link= "logit")) # Single predictor variable

fit1 <- glm(GR46 ~ EW8+ID11+ID13+RSUNEARN+INCOME, data = women_par$train, family = binomial(link= "logit"))


summary(fit)
summary(fit1)




# ````````````````````````````````````````````````````````````````````````````````````

# FITTING DIFFERENT GLM MODELS

#fit1 <- glm(GR46 ~ EW8+SPED2+INCOME+ID11+ID13, data = women_par$train, family = binomial(link = "logit"))
#
# fit2 <- glm(GR46 ~ (education), data = women_par$train, family = binomial(link="logit"))
# 
# fit3 <- glm(GR46 ~ education + INCOME, data = women, family = binomial())
# 
# fit4 <- glm(GR46 ~ education + RSUNEARN + EW6 + SPED6 , data = women, family = binomial())
# 
# fit5 <- glm(GR46 ~ education + RSUNEARN + EW6 + SPED6 + ID13, data = women, family = binomial())
#
# fit6 <- glm(GR46 ~ EW8 + RSUNEARN + EW6 + SPED6 + ID13 + ID11, data = women_par$train, family = binomial(link = "logit"))

# ````````````````````````````````````````````````````````````````````````````````````




# Stepwise generalized logistic regression linear regression to check if if adding diff variables improves the model or not.

lm.one <- glm(GR46 ~ education,data=train_partition,family = binomial(link= "logit"),na.action = na.roughfix)

lm.all <- glm(GR46 ~ EW5+ EW6+ education+ EW9+ EW10+ID11+ ID13+HHEDUC+ HHEDUCM+ HHEDUCF+SPED6+ SPED2+ SPED3+ INCOME+RSUNEARN,data=train_partition,family = binomial(link= "logit"),na.action = na.roughfix)

step(lm.one,scope=list(upper=lm.all, lower= lm.one), direction = "forward",trace = 1)




# Trying multinomial regression 

fit1m <- multinom(GR46 ~ EW8+EW9+EW10+SPED2+INCOME+ID11+ID13, data = train_partition,hess=TRUE)




# Caluculating prediction values ( to be run everytime after running the model)
 
pred <- as_tibble(predict.glm(fit1, test_partition,na.action = na.pass))


# Converting predictions to match the levels of the response variable

pred_fc <- mutate(pred, value = ifelse(pred > 0.5,"(1) Yes 1","(0) No 0"))
p_class <- factor(pred_fc$value, levels = levels(women$GR46))



# Displaying Confusion Matrix 

Cmatrix <- confusionMatrix( p_class, test_partition$GR46, dnn = c("Prediction", "Reference"))
Cmatrix
cat("The F Score is",Cmatrix$byClass["F1"])


cdplot(GR46 ~ education, data=women)

```



```{r}
# Running randomForest on "women" data.

women.rf1=randomForest(GR46 ~EW6+ education+ SPED6+ SPRO5+ INCOME+ RSUNEARN, data = train_partition, na.action = na.roughfix, mtry=6)
women.rf1
plot(women.rf1)

  

# Calculating predictions for randomForest

cl <- as_tibble(predict(women.rf1, test_partition))
p_cl <- factor(cl$value, levels = levels(women$GR46))


# Displaying COnfusion Matrix

Cmatrix1 <- confusionMatrix( p_cl,test_partition$GR46, dnn = c("Prediction", "Reference"))
Cmatrix1
cat("The F Score is",Cmatrix1$byClass["F1"])

importance(women.rf1)


# ````````````````````````````````````````````````````````````````````````````````````

# Plotting Precision , Recall and  TPR/FPR graph (ROC) 

OOB.votes <- predict (women.rf1,test_partition,type="prob")
OOB.pred <- OOB.votes[,2]

pred.obj <- prediction (OOB.pred,test_partition$GR46)

RP.perf <- performance(pred.obj, "rec","prec")
plot (RP.perf)

ROC.perf <- performance(pred.obj, "tpr","fpr")
plot (ROC.perf)


```



```{r}
library(tidyverse)
library(dplyr)

# Loading DS1 Dataframe 

load("C:/Users/Akshay/Desktop/ICPSR_36151/DS0001/36151-0001-Data.rda")
load("C:/Users/Akshay/Desktop/ICPSR_36151/DS0010/36151-0010-Data.rda")

ds1 <- as_tibble(da36151.0001)

ds1 <- dplyr::select(ds1, HHID, NF5, NF25, NF45, INCCROP, INCAGPROP, INCANIMAL, INCAG, INCBUS, 
                     INCOTHER, INCEARN, INCBENEFITS, INCOME, INCREMIT, INCOMEPC, WS3NM, WS4, WS5, 
                     WS7MONTHS, WSEARN, WS12, WSEARNAGLAB, WSEARNNONAG, WSEARNSALARY, WSEARNNREGA, 
                     RSUNEARN, INCNONAG, INCAGLAB, INCSALARY, INCNREGA, INCNONNREGA, HHEDUC, HHEDUCM, 
                     HHEDUCF, MG10)

ds10 <- as_tibble(da36151.0010)

# Loading IHDS 2005 data from tsv files (no .rda available)

ihds2005 <- as_tibble(read.delim('C:/DMP/22626-0001-Data.tsv',sep = "\t"))



# preliminary LC and GIni Plots 

x <- ineq(ds1$INCOME,type= "Gini")

y <- Lc(ds1$INCOME)

plot(y)




# Plotting Gini Curves for 2004-05 and 2011-12 IHDS Data.


font.add.google("Poppins", "myfont")
showtext.auto()


Income05 <- ihds2005$income
Income <- ds1$INCOME

lorenz05 <- Lc(Income05)
lorenz_df1 <- data.frame(prop_pop = lorenz05$p, income = lorenz05$L) %>%
   mutate(prop_equality = prop_pop)

ineq(Income05,type="Gini")
ineq(Income,type="Gini")

lorenz11 <- Lc(Income)
lorenz_df2 <- data.frame(prop_pop = lorenz11$p, income = lorenz11$L) %>%
   mutate(prop_equality = prop_pop)

p1 <- ggplot(lorenz_df1, aes(x = prop_pop, y = income)) +
   geom_ribbon(data=lorenz_df2,aes(ymax = prop_equality, ymin = income), fill = "yellow",alpha=0.5)+
   geom_line() +
   geom_abline(slope = 1, xintercept = 0, type="l", lty=2,lwd=2) +
   scale_x_continuous("Cumulative proportion of population", label = percent) +
   scale_y_continuous("Cumulative proportion of income", label = percent) +
   theme_minimal(base_family = "myfont") +
   coord_equal() +
   annotate("text",0.53, 0.32, label = "Inequality Gap", family = "myfont") +
   annotate("text", 0.5 , 0.6, label = "Complete equality line", angle = 45, family = "myfont") + 
   ggtitle (
      str_wrap("Cumulative distribution of income from all sources", 200))

print(p1)

grid.text("Source: IHDS 2011-12", 0.8, 0.23, 
       gp = gpar(fontfamily = "myfont", fontsize = 15))

ggsave("IncomeEquality.jpeg")



font.add.google("Poppins", "myfont")
showtext.auto()


p2 <- ggplot(lorenz_df1, aes(x = prop_pop, y = income)) +
   geom_ribbon(aes(ymax = prop_equality, ymin = income), fill = "red",alpha=0.5) +
   geom_line() +
   geom_abline(slope = 1, xintercept = 0, type="l", lty=2,lwd=2) +
   scale_x_continuous("Cumulative proportion of population", label = percent) +
   scale_y_continuous("Cumulative proportion of income", label = percent) +
   theme_minimal(base_family = "myfont") +
   coord_equal() +
   annotate("text",0.53, 0.32, label = "Inequality Gap", family = "myfont") +
   annotate("text", 0.5 , 0.6, label = "Complete equality line", angle = 45, family = "myfont") + 
   ggtitle (
      str_wrap("Cumulative distribution of income from all sources", 200))

print(p2)

grid.text("Source: IHDS 2004-05", 0.8, 0.23, 
       gp = gpar(fontfamily = "myfont", fontsize = 10))




```



```{r}


# loading Agrarian Data 

load("C:/Users/Akshay/Desktop/ICPSR_36151/DS0002/36151-0002-Data.rda")


household_data <- da36151.0002
View(head(household_data,20))

# Sorting and Selecting relevant variables for the model

agrarian <- dplyr::select(household_data, HHID, ID14, FM2, FM3,FM4A,FM5A,FM6A,FM4B,FM5B, FM6B,FM4C,FM26A, FM26B,
                   FM5C, FM6C, FM40E, FM11A, FM11B, FM11C, FM27B, FM29, FM30, FM31, FM32, FM33, FM34, FM29RS, FM30RS, FM40E)

# FM29RS - "Rupees spent last year on fertilizer and manure"
# FM30RS - "Rs last year on herbicides and pesticides"
# FM27A -  "Hired farm labour days"
# ID14 -   "Main income source"
# FM2 - "Local area unit name"
# FM3 - "Local units/acre"
# FM4A - "Owned kharif"
# FM5A - "Rented in kharif"
# FM6A - "Rented out kharif"
# FM4B - "Owned rabi"
# FM5B - "Rented in rabi"
# FM6B - "Rented out rabi"
# FM4C - "Owned summer"
# FM5C - "Rented in summer"
# FM6C - "Rented out summer"
# FM40E - "Tractors/Tillers"
# FM11A - "Cultivated kharif"
# FM11B - "Cultivated rabi"
# FM11C - "Cultivated summer"
# FM26A - "Crop residue total value (rupees)"
# FM26B - "Crop residue sold (rupees)"
# FM27B - "Hired farm labour Rs"
# FM29: - "Fertilizers Rs"
# FM30: - "Pesticides Rs"
# FM31: - "Irrigation water Rs"
# FM32: - "Hired Equipment/Animals Rs"
# FM33: - "Agriculture loan repayment Rs"
# FM34: - "Farm miscellaneous Rs"


agrarian <- agrarian %>%
  mutate(cultivated_land = (FM11A + FM11B + FM11C) /FM3)

#agrarian <- na.omit(agrarian)

agrarian <-  filter(agrarian, cultivated_land <= 30)

summary(agrarian$cultivated_land)

ggplot(agrarian) + geom_histogram(aes(cultivated_land), binwidth = 5) + coord_cartesian(xlim = c(0, 75), ylim = c(0,5000))


#Partitioning data for training and Testing

agrarian1 <-  resample_partition(agrarian, c(train = 0.8,test = 0.1, valid = 0.1))

agrarian_train <- as_tibble(agrarian1$train)
agrarian_valid <- as_tibble(agrarian1$valid)
agrarian_test <- as_tibble(agrarian1$test)


train=sample(1:nrow(agrarian_train),nrow(agrarian_train))


#test = sample(1:nrow(agrarian_test),nrow(agrarian_test)) 
#valid = sample(1:nrow(agrarian_valid),nrow(agrarian_valid)) 
#agri.rf1=randomForest(cultivated_land ~ FM27B+FM29+FM30+FM31+FM32+FM33+FM34, data = agrarian, subset = train,na.action = na.roughfix)



# Running randomForest 

agri.rf1=randomForest(cultivated_land ~ FM27B+FM29RS+FM30RS+FM31+FM32+FM34+FM26A, data = agrarian_train, 
                      na.action = na.roughfix, ntree = 200)

ggplot(agrarian) + geom_line(mapping = aes( x = log(FM32) , y = cultivated_land))


agri.rf1

plot(agri.rf1)
importance(agri.rf1) 


# Plotting Actual vs Predicted cultvated land values to visually test the model fit 


cl <- as_tibble(predict(agri.rf1, na.omit(agrarian_test)))
test_omit <- na.omit(agrarian_test)
x1 <- seq(1:nrow(test_omit))
df <- mutate(cl,arpita = row_number())
df1 <- mutate(test_omit, no_rows = row_number())
ggplot() + geom_smooth(data = df, mapping = aes(x = arpita, y = value,color='black')) + 
geom_smooth(data = df1, mapping = aes(x= no_rows, y = df1$cultivated_land,color='red')) +
  scale_colour_manual(name = 'cultivated land', 
                      values =c('black'='black','red'='red'), labels = c('predicted','actual')) + 
  labs(x = "observation number", y = "cultivated land")



# Plotting Residuals against predictor variables

agrarian_test %>% add_residuals(agri.rf1) %>%
  ggplot(aes(x=FM27B, y=resid)) + geom_point() + scale_x_continuous(limits = c(0,20000)) + 
  labs(x = "Hired farm labour (Rs)")

agrarian_test %>% add_residuals(agri.rf1) %>%
  ggplot(aes(x=FM29RS, y=resid)) + geom_point() + scale_x_continuous(limits = c(0,20000)) + 
  labs(x = "Rupees spent last year on fertilizer and manure")

agrarian_test %>% add_residuals(agri.rf1) %>%
  ggplot(aes(x=FM30RS, y=resid)) + geom_point() + scale_x_continuous(limits = c(0,20000)) + 
  labs(x = "Rs last year on herbicides and pesticides")

agrarian_test %>% add_residuals(agri.rf1) %>%
  ggplot(aes(x=FM31, y=resid)) + geom_point() + scale_x_continuous(limits = c(0,10000)) + 
  labs(x = "Rupees spent on irrigation")

agrarian_test %>% add_residuals(agri.rf1) %>%
  ggplot(aes(x=FM32, y=resid)) + geom_point() + scale_x_continuous(limits = c(0,20000)) + 
  labs(x = "Hired Equipment/Animals Rs")

agrarian_test %>% add_residuals(agri.rf1) %>%
  ggplot(aes(x=FM34, y=resid)) + geom_point() + scale_x_continuous(limits = c(0,10000)) + 
  labs(x = "Farm miscellaneous Rs")



# total costs evaluations

df_test1 <- mutate(agrarian, money_invested = FM29 + FM32 + 
                     FM31 + FM30 + FM27B + FM34, unsold_residue = FM26A - FM26B)%>%  
  filter(ID14=="(01) Cultivation 1") %>%
  summarise(total_money = sum(money_invested, na.rm = TRUE), 
            Crop_residue = sum(FM26A, na.rm = TRUE), 
            Residue_sold = sum(FM26B, na.rm = TRUE),
            unsold_residue = sum(unsold_residue, na.rm = TRUE)) 
  



```
