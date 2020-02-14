library(dplyr)
library(dbplyr)
library(zoo)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(scales)
library(polychor)
library(rlist)

winpath <- data.frame(patient_number=integer(), gender=character(), age=double(), result_date=as.Date(character()), inr_abnormal=integer())

for(i in 1:8000) {
  
patient_number <- i
gender_options <- c("M", "F")
gender <- sample(gender_options, 1)

age_dist <- rnorm(10000, 65, 12)
age <- round(sample(age_dist, 1),0)

prob_inr_abnormal = 0.005*age+0.33

start_date <- as.Date('2014-01-01')
result_date <- c(start_date)

initial_inr_abnormal <- rbinom(n=1, size=1, prob=prob_inr_abnormal)
inr_abnormal <- c(initial_inr_abnormal)

abnormal_inr_dist <- rnorm(1000, 1.75, 0.28)
normal_inr_dist <- rnorm(1000, 1.15, 0.15)

rslt <- ifelse(initial_inr_abnormal==1, sample(abnormal_inr_dist, 1), sample(normal_inr_dist,1))
result <- c(rslt)

number_of_days_monitored <- c(7, 14, 21, 28)
pt_LOS <- sample(number_of_days_monitored, 1)

inr_monitoring_strategy <- c(0.4, 0.5, 0.7, 0.9)
pt_monitored <- sample(inr_monitoring_strategy, 1)




for (j in 1:pt_LOS) {
  
  inr_tested <- rbinom(n=1, size=1, prob=pt_monitored)
  
  if(inr_tested == 1) {
    result_date <- list.append(result_date,start_date+j)

    
    if(length(inr_abnormal) == 1){
      inr <- ifelse(initial_inr_abnormal == 0, rbinom(n=1, size=1, prob=prob_inr_abnormal), rbinom(n=1, size=1, prob=1-prob_inr_abnormal))
      inr_abnormal <- list.append(inr_abnormal,inr)
      }
      else{
    inr <- ifelse(inr_abnormal[length(inr_abnormal)-1] == 0, rbinom(n=1, size=1, prob=prob_inr_abnormal), rbinom(n=1, size=1, prob=1-prob_inr_abnormal))
    inr_abnormal <- list.append(inr_abnormal,inr)
    }

    rslt <- ifelse(inr==1, sample(abnormal_inr_dist, 1), sample(normal_inr_dist,1))
    result <- list.append(result, rslt)  
  
  
  }
  

}

patient_number <- rep(patient_number, length(result_date))
gender <- rep(gender, length(result_date))
age <- rep(age, length(result_date))

df <- data.frame(patient_number, gender, age, result_date, result, inr_abnormal)

winpath <- rbind(winpath, df)

}


winpath <- winpath %>% 
  select(-inr_abnormal) %>% 
  mutate(result = round(result,2))

write.csv(winpath, '/Users/timothymcmillan/Documents/IHI_projects/clotting/simulated_data2.csv', row.names = FALSE)

ggplot(winpath, aes(x=result)) + geom_histogram(binwidth=0.2)
