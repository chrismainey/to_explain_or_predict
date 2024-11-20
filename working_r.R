library(readr)
heart_failure_clinical_records_dataset <- read_csv("data/heart_failure_clinical_records_dataset.csv")

Hmisc::describe(heart_failure_clinical_records_dataset)


heart_failure_clinical_records_dataset$anaemia <- factor(heart_failure_clinical_records_dataset$anaemia)
heart_failure_clinical_records_dataset$diabetes <- factor(heart_failure_clinical_records_dataset$diabetes)
heart_failure_clinical_records_dataset$high_blood_pressure <- factor(heart_failure_clinical_records_dataset$high_blood_pressure)
heart_failure_clinical_records_dataset$sex <- factor(heart_failure_clinical_records_dataset$sex)
heart_failure_clinical_records_dataset$smoking <- factor(heart_failure_clinical_records_dataset$smoking)

heart_failure_clinical_records_dataset$sc_age <- scale(heart_failure_clinical_records_dataset$age)
heart_failure_clinical_records_dataset$sc_serum_creatinin <- scale(heart_failure_clinical_records_dataset$serum_creatinine)
heart_failure_clinical_records_dataset$sc_ejection_fraction <- scale(heart_failure_clinical_records_dataset$ejection_fraction)

model1 <- glm(DEATH_EVENT ~ serum_creatinine + ejection_fraction + age,
  data=heart_failure_clinical_records_dataset, family = "binomial")


summary(model1)

library(ModelMetrics)

auc(model1)


library(caret)


library(caret)
set.seed(3456)
trainIndex <- createDataPartition(heart_failure_clinical_records_dataset$DEATH_EVENT, p = .8, 
                                  list = FALSE, 
                                  times = 1)

Train <- heart_failure_clinical_records_dataset[ trainIndex,]
Test  <- heart_failure_clinical_records_dataset[-trainIndex,]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 5)


ridge <- train(DEATH_EVENT ~ serum_creatinine + ejection_fraction, data = Train, 
                 method = 'ridge', 
                 #lambda1=,
                 #trControl = fitControl,
                 family = "binomial",
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
ridge



x <- model.matrix(DEATH_EVENT~sc_serum_creatinin+sc_ejection_fraction, Train)[,-1]
y <- Train$DEATH_EVENT

library(glmnet)
cv <- cv.glmnet(x, y, alpha = 0, family="binomial")
# Display the best lambda value


ridge1<-glmnet(x,y, alpha=0, lamda=cv$lambda.min, family="binomial")

summary(ridge1)

coef(ridge1)


# Make predictions on the test data
x.test <- model.matrix(DEATH_EVENT~sc_serum_creatinin+sc_ejection_fraction, Test)[,-1]

predictions <- predict(ridge1, newx=x.test, type="response") |> as.vector()
# Model performance metrics

ModelMetrics::auc(Test$DEATH_EVENT, predictions)





# logarithmic relationship tested

library(readr)
heart_failure_dt <- read_csv("data/heart_failure_clinical_records_dataset.csv")

r_model_exp2 <- glm(DEATH_EVENT ~ serum_creatinine + ejection_fraction
                   , data=heart_failure_dt
                   , family = "binomial")


r_model_exp3 <- glm(DEATH_EVENT ~ serum_creatinine + ejection_fraction
                    , data=heart_failure_dt
                    , family = binomial(link = "cloglog"))


summary(r_model_exp2)
summary(r_model_exp3)


library(ModelMetrics)

auc(r_model_exp2)
