library(stargazer)


benchmark_data <- read.csv("~/Thesis/Analysis/regression_data_w_investor.csv", header=TRUE)
benchmark_data[, 7][benchmark_data[,7] == 0] <- 1
benchmark_data[, 6][benchmark_data[,6] == 0] <- 1

# Make EE1.3 student 5-point Likert
benchmark_data$EE1.3_1[benchmark_data$EE1.3_1==13] <- 1
benchmark_data$EE1.3_1[benchmark_data$EE1.3_1==14] <- 2
benchmark_data$EE1.3_1[benchmark_data$EE1.3_1==15] <- 3
benchmark_data$EE1.3_1[benchmark_data$EE1.3_1==16] <- 4
benchmark_data$EE1.3_1[benchmark_data$EE1.3_1==17] <- 5

benchmark_data$EE1.3_2[benchmark_data$EE1.3_2==13] <- 1
benchmark_data$EE1.3_2[benchmark_data$EE1.3_2==14] <- 2
benchmark_data$EE1.3_2[benchmark_data$EE1.3_2==15] <- 3
benchmark_data$EE1.3_2[benchmark_data$EE1.3_2==16] <- 4
benchmark_data$EE1.3_2[benchmark_data$EE1.3_2==17] <- 5

benchmark_data$EE1.3_3[benchmark_data$EE1.3_3==13] <- 1
benchmark_data$EE1.3_3[benchmark_data$EE1.3_3==14] <- 2
benchmark_data$EE1.3_3[benchmark_data$EE1.3_3==15] <- 3
benchmark_data$EE1.3_3[benchmark_data$EE1.3_3==16] <- 4
benchmark_data$EE1.3_3[benchmark_data$EE1.3_3==17] <- 5

# Make EE1.3 inv 5-point Likert
benchmark_data$EE1_1_inv[benchmark_data$EE1_1_inv==13] <- 1
benchmark_data$EE1_1_inv[benchmark_data$EE1_1_inv==14] <- 2
benchmark_data$EE1_1_inv[benchmark_data$EE1_1_inv==15] <- 3
benchmark_data$EE1_1_inv[benchmark_data$EE1_1_inv==16] <- 4
benchmark_data$EE1_1_inv[benchmark_data$EE1_1_inv==17] <- 5

benchmark_data$EE1_2_inv[benchmark_data$EE1_2_inv==13] <- 1
benchmark_data$EE1_2_inv[benchmark_data$EE1_2_inv==14] <- 2
benchmark_data$EE1_2_inv[benchmark_data$EE1_2_inv==15] <- 3
benchmark_data$EE1_2_inv[benchmark_data$EE1_2_inv==16] <- 4
benchmark_data$EE1_2_inv[benchmark_data$EE1_2_inv==17] <- 5

benchmark_data$EE1_3_inv[benchmark_data$EE1_3_inv==13] <- 1
benchmark_data$EE1_3_inv[benchmark_data$EE1_3_inv==14] <- 2
benchmark_data$EE1_3_inv[benchmark_data$EE1_3_inv==15] <- 3
benchmark_data$EE1_3_inv[benchmark_data$EE1_3_inv==16] <- 4
benchmark_data$EE1_3_inv[benchmark_data$EE1_3_inv==17] <- 5


# Make Gender Binary
benchmark_data$D2[benchmark_data$D2==2] <- 0
benchmark_data$D2_inv[benchmark_data$D2_inv==2] <- 0



plot(density(log(benchmark_data$probinv)))
plot(density(log(benchmark_data$probsuc)))



### LOG PROBINV MODELS ###

H1_model = lm(log(probinv) ~ Peak_Joy + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + Average_Negative_Emotions, 
              data = benchmark_data)

summary(H1_model)


H2_model = lm(log(probinv) ~ Peak_Joy_Duration + I(Peak_Joy_Duration**2) + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + 
                First_Second_Joy + Average_Negative_Emotions, data = benchmark_data)

summary(H2_model)

model_3 = lm(log(probinv) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + 
               First_Second_Joy + Average_Negative_Emotions, data = benchmark_data)


summary(model_3)



### LOG PROBSUC MODELS ###

H1_model_suc = lm(log(probsuc) ~ Peak_Joy + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + 
                    Average_Negative_Emotions, data = benchmark_data)

summary(H1_model_suc)


H2_model_suc = lm(log(probsuc) ~ Peak_Joy_Duration + I(Peak_Joy_Duration**2) + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + 
                    First_Second_Joy + Average_Negative_Emotions, data = benchmark_data)

summary(H2_model_suc)

model_3_suc = lm(log(probsuc) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) + EE1.3_1 +  EE1.3_3 + Average_Joy + Last_Second_Joy + 
                   First_Second_Joy + Average_Negative_Emotions, data = benchmark_data)


summary(model_3_suc)


### Control Variables ###

control_reg_suc <- lm(log(probsuc) ~ EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + Average_Negative_Emotions, 
                  data = benchmark_data)
control_reg <- lm(log(probinv) ~ EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + Average_Negative_Emotions, 
                  data = benchmark_data)


stargazer(control_reg, H1_model, H2_model, model_3, control_reg_suc, H1_model_suc, H2_model_suc, model_3_suc, title="Results Benchmark Model", align=TRUE)


######### WITH INVESTOR CONTROL ####################

### INCL INVESTOR CONTROL AND DEMOGRAPHICS STUDENT ###

inv_cont_data <- read.csv("~/Thesis/Analysis/excl_inv3.csv", header=TRUE)
inv_cont_data[, 7][inv_cont_data[,7] == 0] <- 1
inv_cont_data[, 6][inv_cont_data[,6] == 0] <- 1

inv_cont_data[, 18][inv_cont_data[,18] == 2] <- 0
inv_cont_data[, -1][inv_cont_data[,-1] == 2] <- 0

# Make EE1.3 student 5-point Likert
inv_cont_data$EE1.3_1[inv_cont_data$EE1.3_1==13] <- 1
inv_cont_data$EE1.3_1[inv_cont_data$EE1.3_1==14] <- 2
inv_cont_data$EE1.3_1[inv_cont_data$EE1.3_1==15] <- 3
inv_cont_data$EE1.3_1[inv_cont_data$EE1.3_1==16] <- 4
inv_cont_data$EE1.3_1[inv_cont_data$EE1.3_1==17] <- 5

inv_cont_data$EE1.3_2[inv_cont_data$EE1.3_2==13] <- 1
inv_cont_data$EE1.3_2[inv_cont_data$EE1.3_2==14] <- 2
inv_cont_data$EE1.3_2[inv_cont_data$EE1.3_2==15] <- 3
inv_cont_data$EE1.3_2[inv_cont_data$EE1.3_2==16] <- 4
inv_cont_data$EE1.3_2[inv_cont_data$EE1.3_2==17] <- 5

inv_cont_data$EE1.3_3[inv_cont_data$EE1.3_3==13] <- 1
inv_cont_data$EE1.3_3[inv_cont_data$EE1.3_3==14] <- 2
inv_cont_data$EE1.3_3[inv_cont_data$EE1.3_3==15] <- 3
inv_cont_data$EE1.3_3[inv_cont_data$EE1.3_3==16] <- 4
inv_cont_data$EE1.3_3[inv_cont_data$EE1.3_3==17] <- 5

# Make EE1.3 inv 5-point Likert
inv_cont_data$EE1_1_inv[inv_cont_data$EE1_1_inv==13] <- 1
inv_cont_data$EE1_1_inv[inv_cont_data$EE1_1_inv==14] <- 2
inv_cont_data$EE1_1_inv[inv_cont_data$EE1_1_inv==15] <- 3
inv_cont_data$EE1_1_inv[inv_cont_data$EE1_1_inv==16] <- 4
inv_cont_data$EE1_1_inv[inv_cont_data$EE1_1_inv==17] <- 5

inv_cont_data$EE1_2_inv[inv_cont_data$EE1_2_inv==13] <- 1
inv_cont_data$EE1_2_inv[inv_cont_data$EE1_2_inv==14] <- 2
inv_cont_data$EE1_2_inv[inv_cont_data$EE1_2_inv==15] <- 3
inv_cont_data$EE1_2_inv[inv_cont_data$EE1_2_inv==16] <- 4
inv_cont_data$EE1_2_inv[inv_cont_data$EE1_2_inv==17] <- 5

inv_cont_data$EE1_3_inv[inv_cont_data$EE1_3_inv==13] <- 1
inv_cont_data$EE1_3_inv[inv_cont_data$EE1_3_inv==14] <- 2
inv_cont_data$EE1_3_inv[inv_cont_data$EE1_3_inv==15] <- 3
inv_cont_data$EE1_3_inv[inv_cont_data$EE1_3_inv==16] <- 4
inv_cont_data$EE1_3_inv[inv_cont_data$EE1_3_inv==17] <- 5



scatter.smooth(x=inv_cont_data$Average_Negative_Emotions, y=log(inv_cont_data$probsuc), main="log(probsuc) ~ Average_Negative_Emotions")
scatter.smooth(x=inv_cont_data$quality, y=log(inv_cont_data$probsuc), main="log(probsuc) ~ quality")



inv_control_h1 <- lm(log(probinv) ~ Peak_Joy + quality + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 +
                       Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv , data = inv_cont_data)

summary(inv_control_h1)

inv_control_h2 <- lm(log(probinv) ~ Peak_Joy_Duration + I(Peak_Joy_Duration**2) + quality+ EE1.3_1 + EE1.3_3+ D1 + D2 + Average_Joy + Last_Second_Joy + First_Second_Joy + 
                       Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv , data = inv_cont_data)

summary(inv_control_h2)

inv_control_3 <- lm(log(probinv) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) + quality + EE1.3_1+ EE1.3_3 + D1 + D2 + Average_Joy + Last_Second_Joy + 
                      First_Second_Joy + Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv, data = inv_cont_data)

summary(inv_control_3)

qual_control <- lm(log(probinv) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) +quality + EE1.3_1 + EE1.3_3 + D1 + D2 + Average_Joy + Last_Second_Joy + 
                     First_Second_Joy + Average_Negative_Emotions + EE1_1_inv + IE2_inv + D1_inv + D2_inv, data = inv_cont_data)
summary(qual_control)



inv_control_h1_suc <- lm(log(probsuc) ~ Peak_Joy+ quality + EE1.3_1 + EE1.3_3+ Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 +
                       Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv , data = inv_cont_data)



inv_control_h2_suc <- lm(log(probsuc) ~ Peak_Joy_Duration + I(Peak_Joy_Duration**2)+ quality + EE1.3_1 + EE1.3_3+ D1 + D2 + Average_Joy + Last_Second_Joy + First_Second_Joy + 
                       Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv , data = inv_cont_data)



inv_control_3_suc <- lm(log(probsuc) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) + quality+ EE1.3_1 + EE1.3_3+ D1 + D2 + Average_Joy + Last_Second_Joy + 
                      First_Second_Joy + Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv, data = inv_cont_data)




inv_control <- lm(log(probinv) ~ quality + EE1.3_1+ EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 +
                       Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv , data = inv_cont_data)

inv_control_suc <- lm(log(probsuc) ~ quality + EE1.3_1 + EE1.3_3+ Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 +
                    Average_Negative_Emotions + EE1_1_inv + IE1_inv + D1_inv + D2_inv , data = inv_cont_data)


stargazer(inv_control, inv_control_h1, inv_control_h2, inv_control_3, inv_control_suc, inv_control_h1_suc, inv_control_h2_suc, inv_control_3_suc,
          title="Results Added Control Variables Model", align=TRUE)



#### EMOTIONAL CONTAGION ########

contagion_data <- read.csv("~/Thesis/Analysis/final_contagion_data.csv", header=TRUE)

contagion_data[, 10][contagion_data[,10] == 0] <- 1
contagion_data[, 9][contagion_data[,9] == 0] <- 1


plot(density(contagion_data$probinv))
plot(density(log(contagion_data$probinv)))



boxplot(probsuc~contagion_binary_strict, data=contagion_data)
boxplot(Peak_Joy~contagion_binary_strict, data=contagion_data)



contagion_data$contagion_binary_full <- as.integer(as.logical(contagion_data$contagion_binary_strict))

# Make EE1.3 student 5-point Likert
contagion_data$EE1.3_1[contagion_data$EE1.3_1==13] <- 1
contagion_data$EE1.3_1[contagion_data$EE1.3_1==14] <- 2
contagion_data$EE1.3_1[contagion_data$EE1.3_1==15] <- 3
contagion_data$EE1.3_1[contagion_data$EE1.3_1==16] <- 4
contagion_data$EE1.3_1[contagion_data$EE1.3_1==17] <- 5

contagion_data$EE1.3_2[contagion_data$EE1.3_2==13] <- 1
contagion_data$EE1.3_2[contagion_data$EE1.3_2==14] <- 2
contagion_data$EE1.3_2[contagion_data$EE1.3_2==15] <- 3
contagion_data$EE1.3_2[contagion_data$EE1.3_2==16] <- 4
contagion_data$EE1.3_2[contagion_data$EE1.3_2==17] <- 5

contagion_data$EE1.3_3[contagion_data$EE1.3_3==13] <- 1
contagion_data$EE1.3_3[contagion_data$EE1.3_3==14] <- 2
contagion_data$EE1.3_3[contagion_data$EE1.3_3==15] <- 3
contagion_data$EE1.3_3[contagion_data$EE1.3_3==16] <- 4
contagion_data$EE1.3_3[contagion_data$EE1.3_3==17] <- 5

# Make EE1.3 inv 5-point Likert
contagion_data$EE1_1_inv[contagion_data$EE1_1_inv==13] <- 1
contagion_data$EE1_1_inv[contagion_data$EE1_1_inv==14] <- 2
contagion_data$EE1_1_inv[contagion_data$EE1_1_inv==15] <- 3
contagion_data$EE1_1_inv[contagion_data$EE1_1_inv==16] <- 4
contagion_data$EE1_1_inv[contagion_data$EE1_1_inv==17] <- 5

contagion_data$EE1_2_inv[contagion_data$EE1_2_inv==13] <- 1
contagion_data$EE1_2_inv[contagion_data$EE1_2_inv==14] <- 2
contagion_data$EE1_2_inv[contagion_data$EE1_2_inv==15] <- 3
contagion_data$EE1_2_inv[contagion_data$EE1_2_inv==16] <- 4
contagion_data$EE1_2_inv[contagion_data$EE1_2_inv==17] <- 5

contagion_data$EE1_3_inv[contagion_data$EE1_3_inv==13] <- 1
contagion_data$EE1_3_inv[contagion_data$EE1_3_inv==14] <- 2
contagion_data$EE1_3_inv[contagion_data$EE1_3_inv==15] <- 3
contagion_data$EE1_3_inv[contagion_data$EE1_3_inv==16] <- 4
contagion_data$EE1_3_inv[contagion_data$EE1_3_inv==17] <- 5


# Make Gender Binary
contagion_data$D2[contagion_data$D2==2] <- 0
contagion_data$D2_inv[contagion_data$D2_inv==2] <- 0

write.csv(contagion_data,"correlation_data.csv", row.names = FALSE)


## LOG PROBINV WITH CONTAGION AS INDEPENDENT

model_1 = lm(log(probinv) ~ contagion_binary_full + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 +
               Average_Negative_Emotions + EE1_1_inv + IE2_inv + D1_inv + D2_inv, data = contagion_data)

summary(model_1)


## LOG PROBINV WITH MODERATION ONLY PEAK JOY 

model_2 = lm(log(probinv) ~ Peak_Joy + contagion_binary_full + I(Peak_Joy * contagion_binary_full) + EE1.3_1 + EE1.3_3 + Average_Joy + 
               Last_Second_Joy + First_Second_Joy + D1+ D2 + Average_Negative_Emotions + EE1_1_inv + IE2_inv + D1_inv + D2_inv, 
             data = contagion_data)

summary(model_2)


## LOG PROBINV WITH MODERATION PEAK JOY + DURATION

model_3 = lm(log(probinv) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) + contagion_binary_full + I(Peak_Joy * contagion_binary_full) + 
               + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 + Average_Negative_Emotions + EE1_1_inv + 
               IE2_inv + D1_inv + D2_inv, data = contagion_data)

summary(model_3)

## LOG PROBsuc WITH CONTAGION AS INDEPENDENT

model_1_suc = lm(log(probsuc) ~ contagion_binary_full + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 +
                   Average_Negative_Emotions + EE1_1_inv + IE2_inv + D1_inv + D2_inv, data = contagion_data)

summary(model_1_suc)


## LOG PROBsuc WITH MODERATION ONLY PEAK JOY 

model_2_suc = lm(log(probsuc) ~ Peak_Joy + contagion_binary_full + I(Peak_Joy * contagion_binary_full) + EE1.3_1 + EE1.3_3 + Average_Joy + 
                   Last_Second_Joy + First_Second_Joy + D1+ D2 + Average_Negative_Emotions + EE1_1_inv + IE2_inv + D1_inv + D2_inv, 
                 data = contagion_data)

summary(model_2_suc)


## LOG PROBINV WITH MODERATION PEAK JOY + DURATION

model_3_suc = lm(log(probsuc) ~ Peak_Joy + Peak_Joy_Duration + I(Peak_Joy_Duration**2) + contagion_binary_full + I(Peak_Joy * contagion_binary_full) + 
                   + EE1.3_1 + EE1.3_3 + Average_Joy + Last_Second_Joy + First_Second_Joy + D1+ D2 + Average_Negative_Emotions + EE1_1_inv + 
                   IE1_inv + D1_inv + D2_inv, data = contagion_data)

summary(model_3_suc)

stargazer(model_1, model_2, model_3, model_1_suc, model_2_suc, model_3_suc, title="Results Emotional Contagion Model", align=TRUE)


### T TEST

contagion_data <- read.csv("~/Thesis/Analysis/final_contagion_data.csv", header=TRUE)

contagion_data[, 10][contagion_data[,10] == 0] <- 1
contagion_data[, 9][contagion_data[,9] == 0] <- 1

contagion_data$inverse_contagion <- lapply(contagion_data$dtw_tail, function(x) 1/x)


plot(density(contagion_data$probinv))
plot(density(log(contagion_data$probinv)))



boxplot(probsuc~contagion_binary_strict, data=contagion_data)
boxplot(Peak_Joy ~ D2, data = contagion_data)

#ttest_data = contagion_data[complete.cases(contagion_data[ , 40]),]
ttest_data <- contagion_data

t.test(ttest_data$Peak_Joy~ttest_data$contagion_binary_strict, alternative='less') # where y is numeric and x is a binary factor
boxplot(Peak_Joy~contagion_binary_strict, data=ttest_data)