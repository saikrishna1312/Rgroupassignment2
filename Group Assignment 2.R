rm(list=ls())
set.seed(100)

#defining number of samples in control or treatment groups
n_control<-1000
n_treatment<-700

#defining treatment and control groups
y0_control<-100+rnorm(n_control,0,1)
y0_treatment<-91+rnorm(n_treatment,0,1)

y1_control<-y0_control+rnorm(n_control,3,2)
y1_treatment<-y0_treatment+rnorm(n_treatment,6,2)

#causal effects of treatment
baseline<-mean(y0_treatment)-mean(y0_control)
att<-mean(y1_treatment)-mean(y0_treatment)
atc<-mean(y1_control)-mean(y0_control)
acct_rate<-n_treatment/(n_treatment+n_control)
ate<-acct_rate*att+(1-acct_rate)*atc
naive_est<-mean(y1_treatment)-mean(y0_control)

#Printing results of observational study
cat("Baseline bias = ",baseline)
cat("Average treatment effect of the treatment group (ATT) = ",att)
cat("Average treatment effect of the control group (ATC) = ",atc)
cat("Average treatment effect of the population (ATE) = ",ate)
cat("Naive estimate = ",naive_est)

#combining contol group (y0_control, y0_treatment) and treatment group (y1_control, y1_treatment)
y0<-rbind(y0_control, y0_treatment)
y1<-rbind(y1_control, y1_treatment)


#sampling 200 sample indices from the 1700 samples from the observational study
indices_sample<-sample(length(y0), 200, replace=FALSE)

sample_y0<-y0[indices_sample]
sample_y1<-y1[indices_sample]

#number of samples from control group and treatment group in the randomized sample
num_control_samples <- sum(indices_sample <= length(y0_control))
num_treatment_samples <- sum(indices_sample > length(y0_control))

sample_y0_control <- sample_y0[1:num_control_samples]
sample_y0_treatment <- sample_y0[(num_control_samples + 1):(num_control_samples + num_treatment_samples)]

sample_y1_control <- sample_y1[1:num_control_samples]
sample_y1_treatment <- sample_y1[(num_control_samples + 1):(num_control_samples + num_treatment_samples)]

#causal effect of treatment in the randomized sample of 200 
acct_rate_random<-num_treatment_samples/(num_treatment_samples+num_control_samples)
att_random<-mean(sample_y1_treatment)-mean(sample_y0_treatment)
atc_random<-mean(sample_y1_control)-mean(sample_y0_control)
ate_random<-acct_rate_random*att_random+(1-acct_rate_random)*atc_random
naive_est_random<-mean(sample_y1_treatment)-mean(sample_y0_control)

#Printing results of randomized control trial
cat("Average treatment effect of the treatment group (ATT) = ",att_random)
cat("Average treatment effect of the control group (ATC) = ",atc_random)
cat("Average treatment effect of the population (ATE) = ",ate_random)
cat("Naive estimate = ",naive_est_random)
