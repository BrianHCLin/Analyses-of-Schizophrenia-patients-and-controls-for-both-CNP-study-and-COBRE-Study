set.seed(4321)


#imputation 

Data_logi <- Data[,c("Study",names(Data[,c(1:5,139:152)]), subset)]

Data_logi <- impute.rfsrc(Study~., data = Data_logi)

for(i in 5:6){
  Data_logi[i] <- as.integer(Data_logi[,i])
}


#check correlation

high_cor <- findCorrelation(cor(Data_logi[,-c(1,3:4)]),cutoff = 0.75) + 3

#Remove variables to prevent multicollinearity problem
Data_logi <- Data_logi %>%
  select(-c(high_cor))

index <- sample(1:nrow(Data_logi), size = round(nrow(Data_logi)*0.7,0),replace = FALSE)

Data_train <- Data_logi[index,]
Data_test <- Data_logi[-index,]
logi_m5 <-glm(Study~. + Subject_Type*Age , data = Data_train, family = "binomial") 
summary(logi_m5)
round(exp(coef(logi_m5)),3)
anova(logi_m5, test = "Chisq")


#R-squared

R_squared <- 1 - (summary(logi_m5)[[4]]/summary(logi_m5)[[8]])
R_squared

#Effect plot
plot(Effect(c("Subject_Type", "Age"), logi_m5),ask = FALSE)


#70/30 CV check

#Train
Data_train$prediction <- predict(logi_m5, Data_train, type = "response")


#Test
Data_test$prediction <- predict(logi_m5, Data_test, type = "response")

prop.table(table(COBRE$Subject_Type))


accuracy_info <- AccuracyCutoffInfo( train = Data_train, test = Data_test, 
                                     predict = "prediction", actual = "Study" )

accuracy_info$plot


Classify(Data_train, Data_train$prediction,"Study", 0.5)

Classify(Data_test, Data_test$prediction,"Study", 0.5)


#Combine data model k fold CV check
set.seed(4321)

Accuracy.k <- cv.error(Data_logi, "Study", cut_off = 0.5)
Accuracy.k
mean(Accuracy.k)

