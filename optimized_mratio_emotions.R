rm(list=ls())
set.seed(123)

library(tidyverse)
library(broom)
library(viridis)
library(foreign)
library(stringr)
library(readxl)
library(dplyr)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)

options(encoding = "UTF-8")

rootpath="C:/Domain/Research/Kwok/Meta Emotion/Data Analysis/meta_emotion_dataset"
setwd(rootpath)

files <- list.files(pattern="*.csv")


# first let's define the functions that calculate meta d and dprime



trials2counts <- function(stimID, response, rating,nRatings, padAmount = 0,padCells=0){
  
  nR_S1 <- list()
  nR_S2 <- list()
  
  if (padAmount == 0){
    padAmount = 1/(2*nRatings)}
  # S1 responses
  for (r in nRatings:1){
    cs1 <- 0
    cs2 <- 0
    for (i in 1:length(stimID)){
      s = stimID[i]
      x = response[i]
      y = rating[i]
      
      if ((s==0) & (x==0) & (y==r)){
        (cs1 <- cs1+1)}
      if ((s==1) & (x==0) & (y==r)){
        (cs2 <- cs2+1)}
    }
    nR_S1 <- append(nR_S1,cs1)
    nR_S2 <- append(nR_S2,cs2)
  }
  
  # S2 responses
  for (r in 1:nRatings){
    cs1 <- 0
    cs2 <- 0
    for (i in 1:length(stimID)){
      s = stimID[i]
      x = response[i]
      y = rating[i]
      
      if ((s==0) & (x==1) & (y==r)){
        (cs1 <- cs1+1)}
      if ((s==1) & (x==1) & (y==r)){
        (cs2 <- cs2+1)}
    }
    nR_S1 <- append(nR_S1,cs1)
    nR_S2 <- append(nR_S2,cs2)
  }
  
  
  # pad response counts to avoid zeros
  nR_S1 <- as.numeric(nR_S1)
  nR_S2 <- as.numeric(nR_S2)
  if (padCells == 1){
    nR_S1 <- lapply(nR_S1,FUN= function(x) x+padAmount)
    nR_S2 <- lapply(nR_S2,FUN= function(x) x+padAmount)}
  
  # Combine into lists
  newlist <- list(nR_S1,nR_S2)
  return(newlist)
}



metad_indiv <- function (nR_S1, nR_S2) {
  
  Tol <- 1e-05
  nratings <- length(nR_S1)/2
  
  # Adjust to ensure non-zero counts for type 1 d' point estimate
  adj_f <- 1/((nratings)*2)
  nR_S1_adj = nR_S1 + adj_f
  nR_S2_adj = nR_S2 + adj_f
  
  ratingHR <- matrix()
  ratingFAR <- matrix()
  
  for (c in 2:(nratings*2)) {
    ratingHR[c-1] <- sum(nR_S2_adj[c:length(nR_S2_adj)]) / sum(nR_S2_adj)
    ratingFAR[c-1] <- sum(nR_S1_adj[c:length(nR_S1_adj)]) / sum(nR_S1_adj)
    
  }
  
  t1_index <- nratings
  d1 <<- qnorm(ratingHR[(t1_index)]) - qnorm(ratingFAR[(t1_index)])
  c1 <<- -0.5 * (qnorm(ratingHR[(t1_index)]) + qnorm(ratingFAR[(t1_index)]))
  
  counts <- t(nR_S1) %>% 
    cbind(t(nR_S2))
  counts <- as.vector(counts)
  
  # Data preparation for model
  data <- list(
    d1 = d1,
    c1 = c1,
    counts = counts,
    nratings = nratings,
    Tol = Tol
  )
  
  # Specify initial values for meta_d, cS1, and cS2
  initial_values <- list(
    meta_d = 0,
    cS1 = 0,
    cS2 = 0
  )
  ## Model using JAGS
  # Create and update model with initial values
  model <- jags.model(file = 'Bayes_metad_indiv_R.txt', data = data,
                      n.chains = 3, quiet=FALSE)
  update(model, inits = initial_values, n.iter=1000, seed = 123)
  
  # Sampling
  samples <- coda.samples( 
    model          = model,
    variable.names = c("meta_d", "cS1", "cS2"),
    n.iter         = 10000,
    thin           = 1,
    inits          = initial_values,
    seed = 123)
  
  output <- list(samples, data)
  
  return(output)
}


d_prime <- function (nR_S1, nR_S2) {
  
  Tol <- 1e-05
  nratings <- length(nR_S1)/2
  
  # Adjust to ensure non-zero counts for type 1 d' point estimate
  #adj_f <- 1/((nratings)*2)
  adj_f<-0.5
  nR_S1_adj = nR_S1 + adj_f
  nR_S2_adj = nR_S2 + adj_f
  
  ratingHR <- matrix()
  ratingFAR <- matrix()
  
  for (c in 2:(nratings*2)) {
    ratingHR[c-1] <- sum(nR_S2_adj[c:length(nR_S2_adj)]) / sum(nR_S2_adj)
    ratingFAR[c-1] <- sum(nR_S1_adj[c:length(nR_S1_adj)]) / sum(nR_S1_adj)
    
  }
  
  t1_index <- nratings
  d1 <- qnorm(ratingHR[(t1_index)]) - qnorm(ratingFAR[(t1_index)])
  
  return(d1)
}

if (FALSE){
  across_intensity_2 <- data.frame(name = character(0), ID = numeric(0), 
                                   metad_2 = numeric(0), dprime_2 = numeric(0), mratio_2=numeric(0))
  across_intensity_4 <- data.frame(name = character(0), ID = numeric(0), 
                                   metad_4 = numeric(0), dprime_4 = numeric(0), mratio_4=numeric(0))
  across_intensity_6 <- data.frame(name = character(0), ID = numeric(0), 
                                   metad_6 = numeric(0), dprime_6 = numeric(0), mratio_6=numeric(0))
  across_intensity_8 <- data.frame(name = character(0), ID = numeric(0), 
                                   metad_8 = numeric(0), dprime_8 = numeric(0), mratio_8=numeric(0))
}

# the functions below this segment are just validity checks 


# Remove id 26, 30, 40 without dti data
# Exclude subjects who have not completed the experiment
# Eliminate chance level<0.55 (need to compare response and Ans to calculate accuracy)
# Decision&confidence report bias, that is, the ratio of choosing the same option (left/right), confidence level, >0.9 (choose the same number over 90%)
# Excluded trials with RT <0.1, > mean + 3SD, < mean - 3SD. 
# Participants who had >20% trials excluded should also be excluded from the subsequent data analysis.


trials_complete <- function(numberrows){
  if (numberrows < 144){
    return(FALSE)
  } else{
    return(TRUE)
  }
}


# dataf is any data frame we are trying to check 
# the response and ans, normally acc should be 0
# when reponse does not equal answer

respon_acc_ans <- function(dataf){
  for (i in 1:nrow(dataf)){
    if (dataf$accuracy[i] == 0 & dataf$response[i] == dataf$Ans[i]){
      return(FALSE)}
    if (dataf$accuracy[i] == 1 & dataf$response[i] != dataf$Ans[i]){
      return(FALSE)}
    return(TRUE)}
}

# Req. 3

not_chance <- function(dataf){
  if (mean(dataf$accuracy)<0.55){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Req. 4 

not_biased <- function(dataf){
  number_R1 = sum(dataf$Confidence == 1)/nrow(dataf)
  number_R2 = sum(dataf$Confidence == 2)/nrow(dataf)
  number_R3 = sum(dataf$Confidence == 3)/nrow(dataf)
  number_R4 = sum(dataf$Confidence == 4)/nrow(dataf)
  
  if (number_R1>0.9 | number_R2>0.9| number_R3 >0.9 | number_R4 >0.9){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

# create an empty list for each emotion
disgustlist <- list()
happylist <- list()
fearlist <- list()



for (i in 1:length(files)){
  dat<- read.csv(file = files[i], header = TRUE, fileEncoding = "UTF-8")
  name <- dat$姓名[3]
  ID <- dat$ID[3]
  dat <- dat %>%
    subset(block!="prc") %>%
    mutate(Accuracy = if_else(response == Ans, 1, 0))
  ndat <- subset(dat, abs(Decision.RT - mean(Decision.RT)) < 3*sd(Decision.RT) )
  
  
  if (nrow(ndat) < 0.8*nrow(dat) | dat$ID[1] == "30" | dat$ID[1] == "40" | dat$ID[1] == "26"){
    cat("halt", name, dat$ID[1])
  } else if (not_biased(dat)& not_chance(dat)& trials_complete(nrow(dat)) & respon_acc_ans(dat)){
    cat("proceed", name, "\n")
  } else{
    print("HALT")
  }
  
  dat <- ndat
  
  # only if they pass the checks above do we run the code below
  # here is where we separate based on emotion
  
  
  disgust_df <- subset(dat, emotion == "Disgust")
  happy_df <- subset(dat, emotion == "Happy")
  fear_df <- subset(dat, emotion == "Fear")
  
  
  # after the end of each loop
  # we must compile our results (metad, mratio, dprime)
  
  # 
  # we iterate over emotions here and calculate metad dprime mratio 
  
  
  
  emotions <- list(disgust_df, happy_df, fear_df)
  for (j in 1:length(emotions)){
    dat <- emotions[[j]]
    
    ###############
    dat$emotion_CH <- ifelse(dat$emotion == "Disgust", "恶心", 
                             ifelse(dat$emotion == "Happy", "快乐", "恐惧"))
    
    stimID <- numeric(nrow(dat))
    for (k in 1:nrow(dat)){
      if (dat$choice1[k]==dat$emotion_CH[k]){
        stimID[k] <- 0
      }  
      if (dat$choice2[k]==dat$emotion_CH[k]){
        stimID[k] <- 1
      }
    }
    
    
    #newlist                      
    dat$response <- replace(dat$response,dat$response==1,0)
    dat$response <- replace(dat$response,dat$response==2,1)
    response <- as.numeric(dat$response)
    
    rating<-list(dat$Confidence)
    rating<-as.numeric(unlist(rating))
    nRatings<-4
    
    newlist<-trials2counts(stimID,response,rating,nRatings)
    nR_S1<-newlist[1]
    nR_S2<-newlist[2]
    
    
    nR_S1<-as.numeric(unlist(nR_S1))
    nR_S2<-as.numeric(unlist(nR_S2))
    
    fit <- metad_indiv(nR_S1 = nR_S1, nR_S2 = nR_S2)
    
    
    output = fit[[1]]
    
    # Mean values 
    Value <- summary(output)
    stat <- data.frame(mean = Value[["statistics"]][, "Mean"])
    stat %<>%
      rownames_to_column(var = "name")
    
    # Rhat 
    Value <- gelman.diag(output, confidence = 0.95)
    Rhat <- data.frame(conv = Value$psrf)
    
    # HDI 
    HDI <- data.frame(HPDinterval(output, prob = 0.95))
    HDI %<>%
      rownames_to_column(var = "name")
    
    # All values in the same dataframe
    Fit <- stat %>%
      cbind(lower = HDI$lower,
            upper = HDI$upper)
    
    metad<-Fit$mean[7]
    
    dprime <- d_prime(nR_S1, nR_S2)
    
    mratio <- metad/dprime
    
    
    # meta d, dprime, and mratio
    new_row <- data.frame(name = name, ID = ID, metad = metad, dprime = dprime, mratio = mratio)
    
    if (dat$emotion[1] == 'Disgust'){
      disgustlist[[i]] <- list(new_row)
    } else if (dat$emotion[1] == 'Happy'){
      happylist[[i]] <- list(new_row)
    }else if (dat$emotion[1] == 'Fear'){
      fearlist[[i]] <- list(new_row)
    }
    
  }
} 

disgust_bh <- bind_rows(disgustlist)
happy_bh <- bind_rows(happylist)
fear_bh <- bind_rows(fearlist)




write.csv(disgust_bh, file = "C:/Domain/Research/Kwok/Meta Emotion/Data Analysis/across_emotion_disgust.csv", row.names = FALSE)
write.csv(happy_bh, file = "C:/Domain/Research/Kwok/Meta Emotion/Data Analysis/across_emotion_happy.csv", row.names = FALSE)
write.csv(fear_bh, file = "C:/Domain/Research/Kwok/Meta Emotion/Data Analysis/across_emotion_fear.csv", row.names = FALSE)












