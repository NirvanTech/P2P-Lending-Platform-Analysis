
#Logistic regression Model
library(caTools)
modified_split <- sample.split(modified_avant_UW, SplitRatio = 0.5)
modified_split
modified_training <- subset(modified_avant_UW, modified_split=="TRUE")
modified_testing <- subset(modified_avant_UW,modified_split=="FALSE")

library(nnet)
modified_model<- multinom(Modified_loan_status~log10(`Annual Income`)+`Credit Age Months`+`Fico`+`Inquiries Last6 Months`+`Is Homeowner`+`Is Inc V`+`Open Acc`+`Term`+`Loan Amount`+`Int Rate`,data=modified_training)
modified_res <- predict(modified_model, modified_training)
modified_tab <- table(modified_res,modified_training$Modified_loan_status)
modified_tab
1-sum(diag(modified_tab))/sum(modified_tab)

#ROC
library(ROCR)
modified_pred <- predict(modified_model, modified_training, type = 'prob')
modified_pred <- prediction(modified_pred, modified_training$Modified_loan_status)
modified_eval <- performance(modified_pred, "acc")
plot(modified_eval)
abline(h=0.79, v=0.45)

modified_pred<- prediction(modified_pred, modified_training$Modified_loan_status)
modified_roc <- performance(modified_pred, "tpr", "fpr")
plot(modified_roc, colorize=T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0,b=1)

#AUC
modified_auc <- performance(modified_pred,"auc")
modified_auc <- unlist(slot(modified_auc, "y.values"))
modified_auc <- round (modified_auc,4)
legend(.6,.2,modified_auc, title = "AUC", cex =1.2)
