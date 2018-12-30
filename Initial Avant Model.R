attach(avant_UW)
avant_UW$purpose_group <- ifelse(purpose=="moving___relocation" | purpose=="life_event" | purpose == "emergency_expense" | purpose =="installment" | is.null(purpose), "moving, life event, emergency expense, installment, no answer", "others")
                           
detach(avant_UW)
#Logistic regression Model
library(caTools)
split <- sample.split(avant_UW, SplitRatio = 0.7)
split
training <- subset(avant_UW, split=="TRUE")
testing <- subset(avant_UW,split=="FALSE")
library(usdm)
vif(training)

library(nnet)
model<- multinom(modified_status~`annual_income`:is_inc_v+`annual_income`:dti+annual_income:pti+`annual_income`+inquiries_last6_months+purpose_group+fico*int_rate+credit_age_months+open_acc+is_homeowner+term+log10(`loan_amount`)*int_rate+dti+pti,data=training,family="bionomial")
res <- predict(model, training)
tab <- table(res,training$modified_status)
tab
1-sum(diag(tab))/sum(tab)
check_model<- lm(modified_status~`annual_income`:is_inc_v+`annual_income`:dti+annual_income:pti+`annual_income`+inquiries_last6_months+purpose_group+fico*int_rate+credit_age_months+open_acc+is_homeowner+term+log10(`loan_amount`)*int_rate+dti+pti,data=training)
summary(check_model)
#ROC
library(ROCR)
pred <- predict(model, training, type = 'prob')
pred <- prediction(pred, training$modified_status)
eval <- performance(pred, "acc")
plot(eval)
abline(h=0.79, v=0.45)

pred<- prediction(pred, training$modified_status)
roc <- performance(pred, "tpr", "fpr")
plot(roc, colorize=T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0,b=1)

#AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round (auc,4)
legend(.6,.2,auc, title = "AUC", cex =1.2)
