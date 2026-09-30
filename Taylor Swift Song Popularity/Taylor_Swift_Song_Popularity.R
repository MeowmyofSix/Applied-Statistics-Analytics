##R Code Written by Lisa Platt for STA 691 Final Project####

##Load excel data file via Import Dataset Menu in Environment Tab####

##Load all pertinent libraries####

library(boot)
library(broom)
library(car)
library(caret)
library(caTools)
library(class)
library(corrplot)
library(dplyr)
library(e1071)
library(flextable)
library(gam)
library(gbm)
library(GGally)
library(glmnet)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(ISLR2)
library(knitr)
library(leaps)
library(MASS)
library(mgcv)
library(moments)
library(nortest)
library(officer)
library(pls)
library(pROC)
library(purrr)
library(randomForest)
library(readxl)
library(ROCR)
library(rpart.plot)
library(tidyr)
library(tree)

data <- taylor_swift_spotify_OG
data <- data.frame(data)
data <- subset(data, select = -c(release_date, id, uri))
#removing id and uri as they are not really meaningful, release_date is not as useful as album name.

##Exploratory Data Analysis####

####Summary of data####
summary(data)

####Skewness and Kurstosis Evaluation####
skewness_values <- skewness(data[, 5:16])
kurtosis_values <- kurtosis(data[, 5:16])
results <- data.frame(
  Variable = colnames(data[, 5:16]),
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)
ft <- flextable(results)
ft <- set_table_properties(ft, layout = "autofit") 
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
doc <- body_add_par(doc, "Skewness and Kurtosis Analysis", style = "heading 1")
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "AnalysisReport.docx")

graphics.off()
par(mfrow=c(1,3))
hist(data$instrumentalness,ylab="frequency",frequency=TRUE,main="")
hist(data$speechiness,ylab="frequency",frequency=TRUE,main="")
hist(data$liveness,ylab="frequency",frequency=TRUE,main="")
mtext("Distributions of Skewed Variables", side = 3, line = -2, cex = 1.5, outer = TRUE)

data$log_instrumentalness <- log(data$instrumentalness + 1E-7)
data$log_speechiness <- ifelse(data$speechiness <= 0, NA, log(data$speechiness))
data$log_liveness <- ifelse(data$liveness <= 0, NA, log(data$liveness))

skewness2<-skewness(data[,17:19])
kurtosis2<-kurtosis(data[,17:19])

results2 <- data.frame(
  Variable = colnames(data[, 17:19]),
  Skewness = skewness2,
  Kurtosis = kurtosis2
)
ft2 <- flextable(results2)
ft2 <- set_table_properties(ft2, layout = "autofit") 
doc2 <- read_docx()
doc2 <- body_add_flextable(doc2, value = ft2)
doc2 <- body_add_par(doc2, "Skewness and Kurtosis Analysis", style = "heading 1")
doc2 <- body_add_flextable(doc2, value = ft2)
print(doc2, target = "AnalysisReport2.docx")

####Boxplot of Popularity####
boxplot(data$popularity, main="Boxplot of Popularity")
points(mean(data$popularity), col="blue", pch=18)  
five_number_summary <- fivenum(data$popularity)
mean_popularity <- round(mean(data$popularity),0)
skewness_popularity <- round(skewness(data$popularity),3)
kurtosis_popularity <- round(kurtosis(data$popularity),3)
names(five_number_summary) <- c("Min", "1st Qu.", "Median", "3rd Qu.", "Max")
summary_with_stats <- c(five_number_summary, Mean=mean_popularity, 
                        Skewness=skewness_popularity, Kurtosis=kurtosis_popularity)
legend("topright", 
       legend = paste(names(summary_with_stats), summary_with_stats, sep=": "),
       title = "Summary",
       bty = "n",  
       col=c(rep("black", 5), "blue", "black", "black"), 
       pch=c(rep(NA, 5), 18, NA, NA),  
       cex = 0.8)  

####Bar Plot of Popularity_binary####
max_count <- max(table(data$popularity_binary))
data_stats <- data %>%
  group_by(popularity_binary) %>%
  summarise(
    Min = min(popularity),
    `1st Qu.` = quantile(popularity, 0.25),
    Median = median(popularity),
    `3rd Qu.` = quantile(popularity, 0.75),
    Max = max(popularity),
    Mean = round(mean(popularity),0),
    Skewness = round(skewness(popularity),3),
    Kurtosis = round(kurtosis(popularity),3)
  ) %>%
  mutate(Class = ifelse(popularity_binary == 0, "Flop", "Bop"))
p <- ggplot(data, aes(x = factor(popularity_binary), fill = factor(popularity_binary))) +
  geom_bar(stat = "count") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  scale_fill_manual(values = c("pink", "lightblue"), labels = c("Flop", "Bop")) +
  labs(fill = "Result", x = "Popularity Binary", y = "Count", title = "Binary Response Variable") +
  theme_minimal()
p + geom_text(data = data_stats, aes(x = as.numeric(popularity_binary)+1, y = 50, label = paste("Min:", Min, "\n1st Qu.:", `1st Qu.`, "\nMedian:", Median, "\n3rd Qu.:", `3rd Qu.`, "\nMax:", Max, "\nMean:", Mean, "\nSkew:", Skewness, "\nKurt:", Kurtosis, "\nClass:", Class)), size = 3, hjust = 0.5)

####Correlation matrix####
par(mfrow=c(1,1))
cor_matrix <- cor(data[,c(5:7, 10, 12:19)], use = "pairwise.complete.obs")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   order = "hclust", tl.col = "black", tl.srt = 45, 
                   addCoef.col = "black", tl.cex=0.8, cl.cex=0.8,number.cex=0.8)
#note that energy, loudness, and acousticness seem to have high correlation

####Histograms of Quantitative Variables####
long_data <- reshape2::melt(data[,c(5:7, 10, 12:14,16:19)])
graphics.off()
ggplot(long_data, aes(x = value)) +
  geom_histogram(bins = 15, fill = "blue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histograms of Quantitative Variables", x = "Value", y = "Frequency")

####Boxplots of Quantitative Variables####
graphics.off()
par(mfrow=c(2,2))  
boxplot(data$danceability, xlab="danceability")
boxplot(data$loudness, xlab="loudness")
boxplot(data$duration_ms,xlab="duration_ms")
boxplot(data$log_instrumentalness,xlab="log_instrumentalness")
boxplot(data$log_speechiness,xlab="log_speechiness")
boxplot(data$log_liveness,xlab="log_instrumentalness")
mtext("Boxplots of Quantitative Variables With Outliers", side = 3, line = -2, cex = 1, outer = TRUE)

graphics.off()
par(mfrow=c(1,3))
boxplot(acousticness~popularity_binary,data=data)
boxplot(danceability~popularity_binary,data=data)
boxplot(loudness~popularity_binary,data=data)
mtext("Boxplots of Quantitative Variables - Binary Response", side = 3, line = -2, cex = 1.5, outer = TRUE)

graphics.off()
par(mfrow=c(1,2))
boxplot(valence~popularity_binary,data=data)
boxplot(duration_ms~popularity_binary,data=data)
mtext("Boxplots of Quantitative Variables - Binary Response - Continued", side = 3, line = -2, cex = 1, outer = TRUE)

graphics.off()
par(mfrow=c(1,3))
boxplot(log_instrumentalness~popularity_binary,data=data)
boxplot(log_speechiness~popularity_binary,data=data)
boxplot(log_liveness~popularity_binary,data=data)
mtext("Boxplots of Quantitative Variables - Binary Response - Continued", side = 3, line = -2, cex = 1, outer = TRUE)

#Update data set to include key variables moving forward
updated_data<-data[,c(5:7, 10, 12,13:19)]

####Scatter plots of predictors vs popularity####
par(mfrow=c(4,4))
response <- "popularity"  # Replace with your actual response variable name
plot_list3 <- list()
for (predictor in names(updated_data)[c(1:6,9:13)]) {
  p <- ggplot(updated_data, aes_string(x = "popularity", y = predictor)) +
    geom_point() +
    labs(x = "popularity", y = predictor)
  plot_list3[[predictor]] <- p
}
do.call(grid.arrange, c(plot_list3, ncol = 4))

####Interaction Assessment####

#We will first create three linear models using the colon symbol to create interaction terms using the predictors with highest correlation values: acousticness, energy and loudness:
  
lm.fit.colon1<-lm(popularity~. -popularity_binary+acousticness:energy, data=updated_data)
lm.fit.colon2<-lm(popularity~. -popularity_binary+acousticness:loudness, data=updated_data)
lm.fit.colon3<-lm(popularity~. -popularity_binary+energy:loudness, data=updated_data)

model_summaries<-lapply(
  list(
    lm.fit.colon1,
    lm.fit.colon2,
    lm.fit.colon3), summary)

interaction_summary <- data.frame(
  Model = character(),
  Interaction = character(),
  Estimate = numeric(),
  StdError = numeric(),
  Pr = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:length(model_summaries)) {
  coefficients <- model_summaries[[i]]$coefficients
  interaction_term <- grep(":.*", rownames(coefficients), value = TRUE)
  for (term in interaction_term) {
    interaction_summary <- rbind(interaction_summary, data.frame(
      Model = paste("lm.fit", i, sep=""),
      Interaction = term,
      Estimate = round(coefficients[term, "Estimate"],6),
      StdError = round(coefficients[term, "Std. Error"],6),
      Pr = sprintf("%.2e",coefficients[term, "Pr(>|t|)"])
    ))
  }
}

interaction_summary

#The table above lists out the coefficient, standard error of the coefficient, and its associated p-value.

#Now adding interaction term and updating the whole model

updated_data$energy_loudness<-updated_data$energy*data$loudness   

lm.fit.2<-lm(popularity~. -popularity_binary, data=updated_data)
summary(lm.fit.2)

#duration_ms, the log of speechiness, and energy:loudness are significant

ad.test(lm.fit.2$residuals)
par(mfrow=c(2,2))
plot(lm.fit.2)

####Checking for non-linearity######

predictor_names <- names(updated_data)[c(1:6,9:13)]
model_summaries_df <- data.frame()  

for (predictor in predictor_names) {
  formula <- as.formula(sprintf("popularity ~ poly(%s, 8, raw=TRUE)", predictor))
  
  model <- lm(formula, data=updated_data)
  summary_model <- summary(model)
  coefs <- summary_model$coefficients

  beta_0 <- if (nrow(coefs) >= 1) round(coefs[1, "Estimate"], 3) else NA
  beta_1 <- if (nrow(coefs) >= 2) round(coefs[2, "Estimate"], 3) else NA
  beta_2 <- if (nrow(coefs) >= 3) round(coefs[3, "Estimate"], 3) else NA
  beta_3 <- if (nrow(coefs) >= 4) round(coefs[4, "Estimate"], 3) else NA
  beta_4 <- if (nrow(coefs) >= 5) round(coefs[5, "Estimate"], 3) else NA
  beta_5 <- if (nrow(coefs) >= 6) round(coefs[6, "Estimate"], 3) else NA
  beta_6 <- if (nrow(coefs) >= 7) round(coefs[7, "Estimate"], 3) else NA
  beta_7 <- if (nrow(coefs) >= 8) round(coefs[8, "Estimate"], 3) else NA
  beta_8 <- if (nrow(coefs) >= 9) round(coefs[9, "Estimate"], 3) else NA
  p_value_beta_1 <- if (nrow(coefs) >= 2) round(coefs[2, "Pr(>|t|)"], 4) else NA
  p_value_beta_2 <- if (nrow(coefs) >= 3) round(coefs[3, "Pr(>|t|)"], 4) else NA
  p_value_beta_3 <- if (nrow(coefs) >= 4) round(coefs[4, "Pr(>|t|)"], 4) else NA
  p_value_beta_4 <- if (nrow(coefs) >= 5) round(coefs[5, "Pr(>|t|)"], 4) else NA
  p_value_beta_5 <- if (nrow(coefs) >= 6) round(coefs[6, "Pr(>|t|)"], 4) else NA
  p_value_beta_6 <- if (nrow(coefs) >= 7) round(coefs[7, "Pr(>|t|)"], 4) else NA
  p_value_beta_7 <- if (nrow(coefs) >= 8) round(coefs[8, "Pr(>|t|)"], 4) else NA
  p_value_beta_8 <- if (nrow(coefs) >= 9) round(coefs[9, "Pr(>|t|)"], 4) else NA
  model_summaries_df <- rbind(model_summaries_df, data.frame(
    Predictor = predictor,
    P_B2 = p_value_beta_2,
    P_B3 = p_value_beta_3,
    P_B4 = p_value_beta_4,
    P_B5 = p_value_beta_5,
    P_B6 = p_value_beta_6,
    P_B7 = p_value_beta_7,
    P_B8 = p_value_beta_8
    #R_Squared = summary_model$r.squared,
    #Adjusted_R_Squared = summary_model$adj.r.squared,
    #F_Statistic = summary_model$fstatistic[1]
  ))
}
# View the summary table
model_summaries_df

##Chapter 6######

####Split updated data into train and test####
set.seed(1)
x <- model.matrix(popularity ~ .-popularity_binary, updated_data)[, -1]
y <- updated_data$popularity
train <- sample(1:nrow(x), nrow(x) / 2)
test <- setdiff(1:nrow(x),train)
y.test <- y[test]
y.train<-y[train]

####Basic MLR####
##Set up model
set.seed(1)
lin.mod<-lm(popularity~.-popularity_binary,data=updated_data, subset=train)
summary(lin.mod)
#Assess residuals
shapiro.test(lin.mod$residuals)
graphics.off()
par(mfrow=c(2,2))
plot(lin.mod)
data[c(42, 89, 102, 149, 160),2]
#Use model to predict
predictions.train<-predict(lin.mod,newdata=updated_data[train,])
train_error.lm<-mean((y.train - predictions.train)^2)
train_error.lm
predictions.test<-predict(lin.mod,newdata=updated_data[test,])
test_error.lm<-mean((y.test - predictions.test)^2)
test_error.lm

####Best subset selection#####

#####Exhaustive#####
#Set up model
regfit.full<-regsubsets(popularity~.-popularity_binary,data=updated_data,subset=train,method="exhaustive",nvmax=12)
summary(regfit.full)
reg.summary<-summary(regfit.full)
#Assess criteria for selection and number of variables
which.max(reg.summary$adjr2) #6
which.min(reg.summary$cp) #3
which.min(reg.summary$bic) #2
coef(regfit.full, 7) #Stuck using 7 because of hierarchy principle (force loudness in)
#Plot criteria for selection and number of variables
par(oma = c(0, 0, 3, 0))
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
points(6, reg.summary$adjr2[6], col = "red", cex = 2, 
       pch = 20)
points(7, reg.summary$adjr2[7], col = "blue", cex = 2, 
       pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
points(3, reg.summary$cp[3], col = "red", cex = 2,
       pch = 20)
points(7, reg.summary$cp[7], col = "blue", cex = 2,
       pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(2, reg.summary$bic[2], col = "red", cex = 2,
       pch = 20)
points(7, reg.summary$bic[7], col = "blue", cex = 2,
       pch = 20)
mtext("Best Subset Selection", line = 1, outer = TRUE)
legend("topright", inset = c(0, -3.5), legend = c("Best Subset", "With Forced Variables"), 
       col = c("red", "blue"), pch = 20, horiz = TRUE, cex = 0.8, bty = "n", 
       xpd = NA)

#Build new model with appropriate predictors
regfit.best<-lm(popularity~.-acousticness -danceability -valence -popularity_binary - log_liveness, data=updated_data[train,])
summary(regfit.best)
#Check new model residuals
shapiro.test(regfit.best$residuals)
par(mfrow=c(2,2))
plot(regfit.best)
#Use new model to predict
predictions.best.train<-predict(regfit.best,newdata=updated_data[train,])
train.error.best<-mean((updated_data$popularity[train] - predictions.best.train)^2)
train.error.best
predictions.best.test<-predict(regfit.best,newdata=updated_data[test,])
test.error.best<-mean((updated_data$popularity[test] - predictions.best.test)^2)
test.error.best

#####Forward#####
#Build model using forward selection
regfit.fwd <- regsubsets(popularity~.-popularity_binary,data=updated_data,subset=train,
                         nvmax = 12, method = "forward")
summary(regfit.fwd)
#Identify selection criteria and number of variables
regfit.fwd.summary<-summary(regfit.fwd)
which.max(regfit.fwd.summary$adjr2) #7
which.min(regfit.fwd.summary$cp) #3
which.min(regfit.fwd.summary$bic) #2
coef(regfit.fwd, 7) #Stuck using 7, same as best
#Plot selection criteria and number of variables
par(mfrow = c(2, 2))
plot(regfit.fwd.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(regfit.fwd.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
points(7, regfit.fwd.summary$adjr2[7], col = "red", cex = 2, 
       pch = 20)
plot(regfit.fwd.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
points(3, regfit.fwd.summary$cp[3], col = "red", cex = 2,
       pch = 20)
plot(regfit.fwd.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(2, regfit.fwd.summary$bic[2], col = "red", cex = 2,
       pch = 20)

#####Backward#####
#Build model using backward selection
regfit.bwd <- regsubsets(popularity~.-popularity_binary,data=updated_data,subset=train,
                         nvmax = 12, method = "backward")
summary(regfit.bwd)
#Identify selection criteria and number of variables
regfit.bwd.summary<-summary(regfit.bwd)
which.max(regfit.bwd.summary$adjr2) #5
which.min(regfit.bwd.summary$cp) #4
which.min(regfit.bwd.summary$bic) #4
coef(regfit.bwd, 7) #Stuck using 7 because of hierarchy principle
#Plot selection criteria and number of variables
par(mfrow = c(2, 2))
plot(regfit.bwd.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(regfit.bwd.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
points(5, regfit.bwd.summary$adjr2[5], col = "red", cex = 2, 
       pch = 20)
plot(regfit.bwd.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
points(4, regfit.bwd.summary$cp[4], col = "red", cex = 2,
       pch = 20)
plot(regfit.bwd.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(4, regfit.bwd.summary$bic[4], col = "red", cex = 2,
       pch = 20)

####Shrinkage Methods####

#####Ridge Regression####

#Build and train model
set.seed(1)
grid <- 10^seq(10, -2, length = 100)
ridge.mod.train <- glmnet(x[train,], y.train, alpha = 0, lambda = grid)

#Find best lambda via cross validation
set.seed(1)
cv.ridge <- cv.glmnet(x[train,], y[train], alpha=0)
summary(cv.ridge)

#Plot lambdas and identify best lambda and associated coefficients
par(mfrow = c(1, 1))
plot(cv.ridge)
title(main="Ridge Regression - CV Lambda", line=2.5)
best_lambda_rr <- cv.ridge$lambda.min
best_lambda_rr
best_coeffs.rr <- coef(cv.ridge, s = "lambda.min")
best_coeffs.rr

#Use best lambda to predict
predictions.rr.train <- predict(cv.ridge, newx=x[train,], s=best_lambda_rr)
train_error.rr <- mean((y.train - predictions.rr.train)^2)
train_error.rr
predictions.rr.test <- predict(cv.ridge, newx=x[test,], s=best_lambda_rr)
test_error.rr <- mean((y.test - predictions.rr.test)^2)
test_error.rr

#Plot coefficients versus lambda and versus L2 norm ratio
par(mfrow=c(1,1))
lambda_sequence.tr <- ridge.mod.train$lambda
coefs.train <- predict(ridge.mod.train, type="coefficients", s=lambda_sequence.tr)[-1, ] 
plot(ridge.mod.train, xlab="L2 Norm")
legend("bottomleft", legend=colnames(x), col=1:nrow(coefs.train), lty=1, cex=0.7)
title(main="Ridge Regression - L2 Norm", line=2.5)
lambda_sequence.tr <- ridge.mod.train$lambda
coefs.train <- predict(ridge.mod.train, type="coefficients", s=lambda_sequence.tr)[-1, ] 
plot(log(lambda_sequence.tr), rep(0, length(lambda_sequence.tr)), type="n", ylim=range(coefs.train),xlim=c(log(0.006737946999),log(22026.4657948)), xlab=expression(log(lambda)), ylab="Coefficients",main="Ridge Regression - Best Lambda")
for (i in 1:nrow(coefs.train)) {
  lines(log(lambda_sequence.tr), coefs.train[i, ], col=i)
}
abline(v=log(best_lambda_rr), col="blue")
legend("bottomright", legend=colnames(x), col=1:nrow(coefs.train), lty=1, cex=0.7)
text_x <- log(best_lambda_rr) 
text_y <- par("usr")[3] + 0.05 * (par("usr")[4] - par("usr")[3])  
label <- sprintf("Best log(λ): %.4f", log(best_lambda_rr))
text(text_x, text_y, label, pos=2, col="blue")  

#####Lasso Regression####

#Build and train model
set.seed(1)
grid <- 10^seq(10, -2, length = 100)
penalty_factors <- rep(1, ncol(x))
penalty_factors[3:4] <- 0  # No penalty on 'loudness'
lasso.mod.train <- glmnet(x[train,], y.train, alpha = 1, lambda = grid, penalty.factor = penalty_factors)

#Find best lambda via cross validation
set.seed(1)
cv.lasso <- cv.glmnet(x[train,], y[train], alpha=1, penalty.factor = penalty_factors)

#Plot lambdas and identify best lambda and associated coefficients
plot(cv.lasso)
title(main="Lasso Regression - CV Lambda", line=2.5)
bestlambda.la <- cv.lasso$lambda.min
predictions.la.train <- predict(cv.lasso, newx=x[train,], s=bestlambda.la)
train_error.la <- mean((predictions.la.train-y.train)^2)
train_error.la
predictions.la.test <- predict(cv.lasso, newx=x[test,], s=bestlambda.la)
test_error.la <- mean((predictions.la.test-y.test)^2)
test_error.la
lasso.coef <- predict(cv.lasso, type = "coefficients", s = bestlambda.la)
lasso.coef <- lasso.coef[-1, , drop = FALSE] 
print(length(lasso.coef[lasso.coef != 0]))

#Plot coefficients versus lambda and versus L1 norm ratio
plot(lasso.mod.train)
title(main="Lasso Regression - L1 Norm", line=2.5)
legend("bottomleft", legend=colnames(x), col=1:nrow(coefs.train), lty=1, cex=0.7)
lambda_sequence.trl <- lasso.mod.train$lambda
coefs.train.l <- predict(lasso.mod.train, type="coefficients", s=lambda_sequence.trl)[-1, ]
plot(log(lambda_sequence.trl), rep(0, length(lambda_sequence.trl)), type="n", ylim=range(coefs.train.l),xlim=c(log(0.006737946999),log(22026.4657948)), xlab=expression(log(lambda)), ylab="Coefficients",main="Lasso Regression - Best Lambda")
for (i in 1:nrow(coefs.train.l)) {
  lines(log(lambda_sequence.trl), coefs.train.l[i, ], col=i)
}
abline(v=log(bestlambda.la), col="blue")
legend("bottomright", legend=colnames(x), col=1:nrow(coefs.train), lty=1, cex=0.8)
text_x <- log(bestlambda.la)  # X coordinate for the text
text_y <- par("usr")[3] + 0.05 * (par("usr")[4] - par("usr")[3]) 
label <- sprintf("Best log(λ): %.4f", log(bestlambda.la)) 
text(text_x, text_y, label, pos=4, col="blue")  

####Dimension Reduction Methods####

#####PCR####
#Set up model and plot validation plot
set.seed(1)
pcr.fit <- pcr(popularity ~ .-popularity_binary, data = updated_data, subset = train,
                 scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="RMSEP",legendpos="bottomright")
abline(v = 7, col = "blue", lty = 1) 
text(x=7,y=13,labels="components = 7",pos=2,col="blue")
title(main="PCR Validation Plot", line=3)

#Use number of components to predict
pcr.pred.train<-predict(pcr.fit,x[train,], ncomp= 7)
train_error.pcr<-mean((pcr.pred.train - y.train)^2)
train_error.pcr
pcr.pred.test<-predict(pcr.fit,x[test,], ncomp= 7)
test_error.pcr<-mean((pcr.pred.test - y.test)^2)
test_error.pcr

# Extract the variance explained by the components for predictors (X) and response
var_explained_x_pcr <- c(26.886, 45.45, 59.50, 70.25, 77.73, 84.34, 89.50, 94.11, 97.87, 99.59, 100.00)
var_explained_pop_pcr<-c(2.482, 27.42, 27.42, 27.53, 27.54, 28.79, 29.27, 29.27, 29.49, 29.83, 31.39)

# Create scree plots
plot(var_explained_x_pcr, type = "b", xlab = "Number of Components", ylab = "Percentage of Variance in Predictors Explained",
     main = "Scree Plot for PCR - Predictors", xlim = c(1, length(var_explained_x_pcr)))
abline(h = 90, col = "blue", lty = 2) 
abline(h = 95, col = "red", lty = 2)
abline(v = 7, col = "blue", lty = 1) 
text(x=7,y=80,labels="components = 7",pos=4,col="blue")
plot(var_explained_pop_pcr, type = "b", xlab = "Number of Components", ylab = "Percentage of Variance in Popularity Explained",
     main = "Scree Plot for PCR - Popularity", xlim = c(1, length(var_explained_pop_pcr)))
abline(v = 7, col = "blue", lty = 1) 
text(x=7,y=20,labels="components = 7",pos=4,col="blue")


#####PLS####
#Set up model and plot validation plot
set.seed(1)
pls.model<-plsr(popularity~.-popularity_binary,data=updated_data,subset=train,scale=TRUE,validation="CV")
summary(pls.model)
validationplot(pls.model,val.type="RMSEP",legendpos="bottomright")
title(main="PLS Validation Plot", line=3)
abline(v = 9, col = "blue", lty = 1) 
text(x=9,y=13,labels="components = 9",pos=2,col="blue")

#Use number of components to predict
pls.pred.train <- predict(pls.model, x[train, ], ncomp = 9)
train_error.pls<-mean((pls.pred.train - y.train)^2)
train_error.pls
pls.pred.test <- predict(pls.model, x[test, ], ncomp = 9)
test_error.pls<-mean((pls.pred.test - y.test)^2)
test_error.pls

# Extract the variance explained by the components for predictors (X) and response
var_explained_x_pls <- c(19.90, 43.12, 51.46, 57.03, 63.81, 69.15, 79.44, 84.23, 88.25, 95.39, 100.00)
var_explained_pop_pls<-c(28.23, 29.23, 30.02, 30.51, 30.89, 31.16, 31.26, 31.37, 31.39, 31.39, 31.39)

# Create scree plots
plot(var_explained_x_pls, type = "b", xlab = "Number of Components", ylab = "Percentage of Variance in Predictors Explained",
     main = "Scree Plot for PLS - Predictors", xlim = c(1, length(var_explained_x_pls)))
abline(h = 90, col = "blue", lty = 2) # Adds a dashed blue line at y = 90
abline(h = 95, col = "red", lty = 2)  # Adds a dashed red line at y = 95
abline(v = 9, col = "blue", lty = 1) 
text(x=9,y=60,labels="components = 9",pos=2,col="blue")
plot(var_explained_pop_pls, type = "b", xlab = "Number of Components", ylab = "Percentage of Variance in Popularity Explained",
     main = "Scree Plot for PLS - Popularity", xlim = c(1, length(var_explained_pop_pls)))
abline(v = 9, col = "blue", lty = 1) 
text(x=9,y=31,labels="components = 9",pos=2,col="blue")


###Chapter 6 Summary Table####

errors_df <- data.frame(
  Model = c("Least Squares", "Best Subset/F/B", "Ridge Regression", "Lasso", "PCR", "PLS"),
  Train_Error=c(train_error.lm, train.error.best, train_error.rr, train_error.la, train_error.pcr,train_error.pls),
  Test_Error = c(test_error.lm, test.error.best, test_error.rr, test_error.la, test_error.pcr, test_error.pls)
)
errors_df_sorted <- errors_df[order(errors_df$Test_Error),]
errors_df_sorted

##Chapter 9#####

#####Support Vector Classifier#####

#Updating data setup to run SVM
updated_data$popularity_binary <- as.factor(updated_data$popularity_binary)
updated_data_2 <- subset(updated_data, select = -c(popularity))
set.seed(1)
x1 <- model.matrix(popularity_binary ~ ., updated_data_2)[, -1]
y2 <- updated_data_2$popularity_binary
train2 <- sample(1:nrow(x1), nrow(x1) / 2)
test2 <- setdiff(1:nrow(x1),train2)
y2.test <- y2[test2]

##Setting up linear kernel model
set.seed(1)
svmfit1 <- e1071::svm(popularity_binary ~ ., data = updated_data_2[train2,], kernel = "linear", 
              cost = 0.01, scale = FALSE)
summary(svmfit1)
svmfit1$index

#Using pre-tuned linear kernel model to predict
ypred.train1 <- predict(svmfit1, updated_data_2[train2,])
ypred.test1 <-predict(svmfit1, updated_data_2[test2,])
table(predict = ypred.train1, truth = updated_data_2[train2,]$popularity_binary)
table(predict = ypred.test1, truth =  updated_data_2[test2,]$popularity_binary)

#Tuning Linear Kernel Model
set.seed(1)
tune.out1 <- tune(svm, popularity_binary ~ ., data = updated_data_2[train2,], kernel = "linear", 
                 ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1,  5, 10)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model
summary(bestmod1)

##Using Tuned Linear Kernel Model
ypred.train2 <- predict(bestmod1, updated_data_2[train2,])
ypred.test2 <-predict(bestmod1, updated_data_2[test2,])
table(predict = ypred.train2, truth = updated_data_2[train2,]$popularity_binary)
table(predict = ypred.test2, truth = updated_data_2[test2,]$popularity_binary)

#####Support Vector Machine - Radial#####

##Setting up radial kernel model
set.seed(1)
svmfit3 <- svm(popularity_binary ~ ., data = updated_data_2[train2,], kernel = "radial", 
               cost = 0.05, gamma=1/11, scale = FALSE)
summary(svmfit3)

#Using pre-tuned radial kernel model to predict
ypred.train3 <- predict(svmfit3, updated_data_2[train2,])
ypred.test3 <-predict(svmfit3, updated_data_2[test2,])
table(predict = ypred.train3, truth = updated_data_2[train2,]$popularity_binary)
table(predict = ypred.test3, truth = updated_data_2[test2,]$popularity_binary)

##Tune Radial Kernel Model
set.seed(1)
tune.out2 <- tune(svm, popularity_binary ~ ., data = updated_data_2[train2,], kernel = "radial", 
                  ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1,  5, 10),
                  gamma=c(0.01,0.05,1/12,0.1,0.5,1,5,10)
                  )
                )
summary(tune.out2)
bestmod2 <- tune.out2$best.model
summary(bestmod2)

##Using Tuned Radial Kernel Model to Predict
ypred.train4 <- predict(bestmod2, updated_data_2[train2,])
ypred.test4 <-predict(bestmod2, updated_data_2[test2,])
table(predict = ypred.train4, truth = updated_data_2[train2,]$popularity_binary)
table(predict = ypred.test4, truth = updated_data_2[test2,]$popularity_binary)

#####Support Vector Machine - Poly#####

##Setting up Poly Kernel Model
set.seed(1)
svmfit5 <- svm(popularity_binary ~ ., data = updated_data_2[train2,], kernel = "polynomial", 
               cost = 0.5, gamma=0.05, degree=2, coef0=0, scale = FALSE)
summary(svmfit5)

##Using Pre-Tuned Poly Kernel Model to Predict
ypred.train5 <- predict(svmfit5, updated_data_2[train2,])
ypred.test5 <-predict(svmfit5, updated_data_2[test2,])
table(predict = ypred.train5, truth = updated_data_2[train2,]$popularity_binary)
table(predict = ypred.test5, truth = updated_data_2[test2,]$popularity_binary)

##Tuning Poly Kernel Model
set.seed(1)
tune.out3 <- tune(svm, popularity_binary ~ ., data = updated_data_2[train2,], kernel = "polynomial", 
                  ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1,  5, 10),
                  gamma=c(0.01,0.05,1/12,0.1,0.5,1,5,10),
                  degree=c(1,2,3,4,5),
                  coef0=0))
summary(tune.out3)
bestmod3 <- tune.out3$best.model
summary(bestmod3)

##Using Tuned Poly Kernel Model to Predict
ypred.train6 <- predict(bestmod3, updated_data_2[train2,])
ypred.test6 <-predict(bestmod3, updated_data_2[test2,])
table(predict = ypred.train6, truth = updated_data_2[train2,]$popularity_binary)
table(predict = ypred.test6, truth = updated_data_2[test2,]$popularity_binary)

#####ROC Curves#####
graphics.off()
par(mfrow=c(1,1))
svm_model1 <- svm(popularity_binary ~ ., data = updated_data_2[train2,], kernel='linear', cost=0.05, probability = TRUE)
svm_model2 <- svm(popularity_binary ~ ., data = updated_data_2[train2,], kernel='radial', cost=0.5, gamma=0.05, probability = TRUE)
svm_model3 <- svm(popularity_binary ~ ., data = updated_data_2[train2,], kernel='polynomial', cost=5, gamma=0.01, degree=1, coef0=0,probability = TRUE)

# Get probability predictions
prob_predictions1 <- predict(svm_model1, updated_data_2[test2,], probability = TRUE)
prob_predictions2 <- predict(svm_model2, updated_data_2[test2,], probability = TRUE)
prob_predictions3 <- predict(svm_model3, updated_data_2[test2,], probability = TRUE)

# Extract probabilities for the positive class
probabilities1 <- attr(prob_predictions1, "probabilities")[,2]
probabilities2 <- attr(prob_predictions2, "probabilities")[,2]
probabilities3 <- attr(prob_predictions3, "probabilities")[,2]

roc_curve1 <- roc(response = updated_data_2[test2,]$popularity_binary, predictor = probabilities1)
roc_curve2 <- roc(response = updated_data_2[test2,]$popularity_binary, predictor = probabilities2)
roc_curve3 <- roc(response = updated_data_2[test2,]$popularity_binary[test2], predictor = probabilities3)
auc1 <- auc(roc_curve1)
auc2 <- auc(roc_curve2)
auc3 <- auc(roc_curve3)

# Plot ROC curves
plot(roc_curve1, main="ROC Curves Comparison", col="red",xlim=c(1,0.1))
plot(roc_curve2, add=TRUE, col="blue",xlim=c(1,0.1))
plot(roc_curve3, add=TRUE, col="green",xlim=c(1,0.1))
legend("bottomright", 
       legend=c(paste("Linear: AUC =", round(auc1, 4)),
                paste("Radial: AUC =", round(auc2, 4)),
                paste("Poly: AUC =", round(auc3, 4))),
       col=c("red", "blue", "green"), 
       lwd=2,cex=0.6,
       bty="n")

