# Copyright: Han Wang, June 8th 2022.

# Edited on July 7th 2022 (removed the figure titles)
# Edited on June 14th 2022 (updated the figure numbers).


# Load packages and define some functions

library(car)
library(dplyr)
library(tidyr)

library(ggplot2)
library(ggsci)
library(ggeffects)
library(sjPlot)
library(forcats)

library(lme4)
library(lmerTest)

library(mgcv)
library(mgcViz)
library(itsadug)

`%notin%` <- Negate(`%in%`)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  y <- x
  y[abs(x-mean(x)) > 3*sd(x)] <- NA
  y
}


# Data cleaning

## Experiment 1

### Speech task performance

dat_dual_speech_n192<-read.csv("dat_exp1_maintask.csv")
dat_dual_speech_n192$word_percent<-dat_dual_speech_n192$count_correct/3*100
dat_dual_speech_n192$word_proportion<-dat_dual_speech_n192$count_correct/3
dat_dual_speech_n192$trial<-as.numeric(dat_dual_speech_n192$trial)
dat_dual_speech_n192$participant<-as.factor(dat_dual_speech_n192$participant)
dat_dual_speech_n192$sentence<-as.factor(dat_dual_speech_n192$sentence)
dat_dual_speech_n192$prompt<-as.factor(dat_dual_speech_n192$prompt)
dat_dual_speech_n192$task<-as.factor(dat_dual_speech_n192$task)
dat_dual_speech_n192$correctness<-as.numeric(dat_dual_speech_n192$correctness)
dat_dual_speech_n192$rt<-as.numeric(dat_dual_speech_n192$rt)


library(Rmisc)
dat_dual_speech_n192_se <- summarySE(dat_dual_speech_n192, measurevar="word_percent", groupvars=c("trial","task")) # aggregate the raw data for plotting
detach("package:Rmisc", unload=TRUE)


### Secondary task performance

dat_dual_n192<-dat_dual_speech_n192 %>% filter(task != 'speech_single')

dat_dual_n192$prompt<-gsub("_", "", dat_dual_n192$prompt)
dat_dual_n192$prompt<-gsub(".png", "", dat_dual_n192$prompt)
dat_dual_n192$prompt<-tolower(dat_dual_n192$prompt)
dat_dual_n192$prompt<-as.factor(dat_dual_n192$prompt)

dat_dual_n192_sndcorrect <- dat_dual_n192 %>% filter(correctness == 1) # The data with only correct secondary task responses. This is used in the RT model.

library(Rmisc)
dat_dual_n192_secondary_acc <- summarySE(dat_dual_n192, measurevar="correctness", groupvars=c("trial","task"))
dat_dual_n192_sndcorrect_rt_se<-summarySE(dat_dual_n192_sndcorrect, measurevar="rt", groupvars=c("trial","task"))
detach("package:Rmisc", unload=TRUE)

### Single task performance (current vs pilot experiments, figure D5)

dat_single_40t_60t_n78<-read.csv("dat_singlespeech_current_pilot.csv")
dat_single_40t_60t_n78$word_percent<-dat_single_40t_60t_n78$count_correct/3*100
dat_single_40t_60t_n78$word_proportion<-dat_single_40t_60t_n78$count_correct/3
dat_single_40t_60t_n78$trial<-as.numeric(dat_single_40t_60t_n78$trial)
dat_single_40t_60t_n78$participant<-as.factor(dat_single_40t_60t_n78$participant)
dat_single_40t_60t_n78$sentence<-as.factor(dat_single_40t_60t_n78$sentence)
dat_single_40t_60t_n78$task<-as.factor(dat_single_40t_60t_n78$task)

library(Rmisc)
dat_single_40t_60t_n78_se <- summarySE(dat_single_40t_60t_n78, measurevar="word_percent", groupvars=c("participant","task")) # aggregate dataa for plotting
detach("package:Rmisc", unload=TRUE)


### Effort and attention questionnaire

#### wide format (for analysis)

dat_exp1_question_n192<-read.csv("dat_exp1_question_n192_wide_raw.csv")

dat_exp1_question_n192$participant<-as.factor(dat_exp1_question_n192$participant)

dat_exp1_question_n192_wide_4measures<-data.frame("task.difficulty"= dat_exp1_question_n192$task,
                                                  "participant" = dat_exp1_question_n192$participant,
                                                  "effort.speech" = dat_exp1_question_n192$effort_speech,
                                                  "effort.visual" = dat_exp1_question_n192$effort_secondary,
                                                  "attention.speech" = dat_exp1_question_n192$attention_speech,
                                                  "attention.visual" = dat_exp1_question_n192$attention_secondary)


#### a wide format spreadsheet without outliers

dat_exp1_question_n192_wide_4measures_sdfiltered<-dat_exp1_question_n192_wide_4measures %>%
  group_by(task.difficulty) %>%
  mutate_at(vars(effort.speech,effort.visual,attention.speech,attention.visual), funs(remove_outliers)) %>%
  as.data.frame()


#### Long format (for visualisation)

dat_exp1_question_n192_long_sdfiltered<-read.csv("dat_exp1_question_n192_long_sdfiltered.csv") #Note that under task.difficulty, a=single speech, b = dual-easy, c = dual-intermediate, d = dual-hard. Here, we removed the responses larger than 3SDs of the group mean and removed the NA rows for visual responses in speech single condition.


## Experiment 2

### Speech task performance

dat_dual_speech_n192_exp2<-read.csv("dat_exp2_maintask.csv")
dat_dual_speech_n192_exp2$word_percent<-dat_dual_speech_n192_exp2$count_correct/3*100
dat_dual_speech_n192_exp2$word_proportion<-dat_dual_speech_n192_exp2$count_correct/3
dat_dual_speech_n192_exp2$trial<-as.numeric(dat_dual_speech_n192_exp2$trial)
dat_dual_speech_n192_exp2$participant<-as.factor(dat_dual_speech_n192_exp2$participant)
dat_dual_speech_n192_exp2$task<-as.factor(dat_dual_speech_n192_exp2$task)
dat_dual_speech_n192_exp2$sentence<-as.factor(dat_dual_speech_n192_exp2$sentence)
dat_dual_speech_n192_exp2$prompt<-as.factor(dat_dual_speech_n192_exp2$prompt)
dat_dual_speech_n192_exp2$correctness<-as.numeric(dat_dual_speech_n192_exp2$correctness)
dat_dual_speech_n192_exp2$rt<-as.numeric(dat_dual_speech_n192_exp2$rt)


dat_dual_speech_n192_nophonrep<-dat_dual_speech_n192_exp2[dat_dual_speech_n192_exp2$task!="phon_replication",] # No need to include the phonological replication task for the main analysis
library(Rmisc)
dat_dual_speech_n192_nophonrep_se <- summarySE(dat_dual_speech_n192_nophonrep, measurevar="word_percent", groupvars=c("trial","task")) # aggregate the raw data for plotting
detach("package:Rmisc", unload=TRUE)


### Secondary task performance

dat_dual_n192_nophonrep<-dat_dual_speech_n192_nophonrep %>% filter(task != 'speech_single')
dat_dual_n192_nophonrep_sndcorrect <- dat_dual_n192_nophonrep %>% filter(correctness == 1)

library(Rmisc)
dat_dual_n192_nophonrep_secondary_acc <- summarySE(dat_dual_n192_nophonrep, measurevar="correctness", groupvars=c("trial","task"))
dat_dual_n192_nophonrep_sndcorrect_se<-summarySE(dat_dual_n192_nophonrep_sndcorrect, measurevar="rt", groupvars=c("trial","task"))
detach("package:Rmisc", unload=TRUE)

### Speech task performance under phonological task and the replicated phonological task

dat_dual_speech_n96_onlyphonrep<-dat_dual_speech_n192_exp2[dat_dual_speech_n192_exp2$task %notin% c("speech_single","lexical","visual"),]
library(Rmisc)
dat_dual_speech_n96_onlyphonrep_se <- summarySE(dat_dual_speech_n96_onlyphonrep, measurevar="word_percent", groupvars=c("trial","task")) # aggregate the raw data for plotting
detach("package:Rmisc", unload=TRUE)

### Secondary task accuracy under phonological task and the replicated phonological task

library(Rmisc)
dat_dual_n96_onlyphonrep_secondary_acc <-summarySE(dat_dual_speech_n96_onlyphonrep, measurevar="correctness", groupvars=c("trial","task"))
detach("package:Rmisc", unload=TRUE)

### Effort and attention questionnaire

#### wide format (for analysis)

dat_exp2_question_n192<-read.csv("dat_exp2_question_n192_wide_raw.csv")

dat_exp2_question_n192_nophonrep<-dat_exp2_question_n192[dat_exp2_question_n192$task!="phon_replication",]

dat_exp2_question_n192_wide_4measures_nophonrep<-data.frame("task.type"= dat_exp2_question_n192_nophonrep$task,
                                                            "participant" = dat_exp2_question_n192_nophonrep$participant,
                                                            "effort.speech" = dat_exp2_question_n192_nophonrep$effort_speech,
                                                            "effort.tsecondary" = dat_exp2_question_n192_nophonrep$effort_secondary,
                                                            "attention.speech" = dat_exp2_question_n192_nophonrep$attention_speech,
                                                            "attention.tsecondary" = dat_exp2_question_n192_nophonrep$attention_secondary)

#### a wide format spreadsheet without outliers

dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep<-dat_exp2_question_n192_wide_4measures_nophonrep %>%
  group_by(task.type) %>%
  mutate_at(vars(effort.speech,effort.tsecondary,attention.speech,attention.tsecondary), funs(remove_outliers)) %>%
  as.data.frame()

#### Long format (for visualisation)

dat_exp2_question_n192_long_sdfiltered<-read.csv("dat_exp2_question_n192_long_sdfiltered.csv") #Note that under task.type, a=single speech, b = dual-visual, c = dual-phonological, d = dual-lexical. Here, we removed the responses larger than 3SDs of the group mean and removed the NA rows for visual responses in speech single condition.


# Figure 3: Trial-wise % correct in speech tasks (Experiment 1)

## Model

m_fig3_full<-gam(cbind(count_correct,3-count_correct) ~ relevel(as.factor(task), ref="speech_single") + s(trial, k = -1, bs = "tp", by=task)+
                          s(participant,bs='re')+s(participant,trial,bs='re')+s(sentence,bs='re')+s(sentence,task,bs='re'),
                        data = dat_dual_speech_n192,family = binomial(link="logit"),method = "ML") # the saturated model including all terms allowed by the experimental design

m_fig3_final<-gam(cbind(count_correct,3-count_correct) ~ relevel(as.factor(task), ref="speech_single") + s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(participant,trial,bs='re'),
                  data = dat_dual_speech_n192,family = binomial(link="logit"),method = "ML") # the final GAMM reported in the paper

summary(m_fig3_final) # model output can be found in Table 1 in the paper

## Figure

ilink<-family(m_fig3_final)$linkinv # get the link function

m_fig3_final_new_data <- tidyr::expand(dat_dual_speech_n192, nesting(participant,task),
                                       trial = unique(trial)) # prepare an empty data frame for feeding the prediction

m_fig3_final_prediction<-bind_cols(m_fig3_final_new_data, setNames(as_tibble(predict(m_fig3_final, m_fig3_final_new_data, 
                                                                                     exclude = c("s(participant)","s(participant,trial)"),
                                                                                     se.fit = TRUE)[1:2]), c('fit_link','se_link'))) # generate the prediction for responses based on the link function 

m_fig3_final_prediction <- mutate(m_fig3_final_prediction,
                                  fit_resp  = ilink(fit_link),
                                  right_upr = ilink(fit_link + (2 * se_link)),
                                  right_lwr = ilink(fit_link - (2 * se_link))) # now change the predicted responses back to its original scale, along with the 95% intervals

m_fig3_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_fig3_final_prediction,FUN=mean) # aggregate the data because we only care about the fixed effects


m_fig3_final_reorder<-c('speech_single','speech_td_48_60','speech_td_24_36','speech_td_0_12') # reorder the levels for plotting

facet_labels<-c(speech_single = "Single", speech_td_0_12 = "Dual Hard",speech_td_24_36 = 'Dual Intermediate', speech_td_48_60 = 'Dual Easy') # Set the lable for panels in the plot

dat_dual_speech_n192_se_reorder <- dat_dual_speech_n192_se %>% 
  mutate(task = fct_relevel(task, m_fig3_final_reorder)) # reorder the raw data per level of task

m_fig3_final_prediction_reorder<-m_fig3_final_prediction %>% 
  mutate(task = fct_relevel(task, m_fig3_final_reorder)) # reorder the prediction per level of task


plot_m_fig3_final<-ggplot(dat_dual_speech_n192_se_reorder,aes(x=trial, y=word_percent,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=word_percent-se, ymax=word_percent+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig3_final_prediction_reorder, aes(x=trial, y=fit_resp*100,color=task), size=.6) +
  geom_ribbon(data=m_fig3_final_prediction_reorder, aes(ymin=right_lwr*100, ymax=right_upr*100, x=trial, y=fit_resp*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits=c(0, 100))+
  labs(x="Trial number", y = "%Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels)) # main figure


plot_m_fig3_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # update the formatting for the plot


# Figure 4: First derivative analysis (speech task)

library(gratia)

newd <- tidyr::expand(dat_dual_speech_n192, nesting(participant,task),
                      trial = unique(trial)) # a data frame to hold the new data generated from the prediction


fd <- derivatives(m_fig3_final, type = "central", term = "s(trial)", newdata = newd, unconditional = TRUE,partial_match = TRUE,interval = "simultaneous", n_sim = 10000) # get the derivatives and generate the simultaneous confidence intervals
fd<-fd %>%
  filter(lower != 0 & upper != 0)
fd<-aggregate(cbind(derivative,se,crit,lower,upper)~smooth+data,data=fd,FUN=mean) # aggregate the data as we only care about the fix effects


fd$smooth <- gsub("\\(", "_", fd$smooth) # rename the names of smooths in the data frame
fd$smooth <- gsub("\\):", "_", fd$smooth)

fd_reorder_diffloads<-c('s_trial_taskspeech_single','s_trial_taskspeech_td_48_60','s_trial_taskspeech_td_24_36','s_trial_taskspeech_td_0_12') # reorder the levels of task
facet_labels_fd_diffloads<-c(s_trial_taskspeech_single = "Single", s_trial_taskspeech_td_0_12 = "Dual Hard",s_trial_taskspeech_td_24_36 = 'Dual Intermediate', s_trial_taskspeech_td_48_60 = 'Dual Easy') # relabel the levels of task for plotting


dat_fd_reorder_diffloads <- fd %>% 
  mutate(smooth = fct_relevel(smooth, fd_reorder_diffloads)) # reorder the data frame


ggplot(dat_fd_reorder_diffloads,
       aes(x = data, y = derivative,colour=smooth,fill=smooth)) +
  geom_hline(aes(yintercept = 0)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  scale_y_continuous(breaks = seq(-0.06, 0.14, by = 0.02),limits=c(-0.06, 0.14))+
  labs(y = "First derivative", x = "Trial number") +
  theme_minimal()+
  facet_wrap(~ smooth,
             labeller = labeller(smooth = facet_labels_fd_diffloads))+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # plot the derivatives


# Figure 5: Trial-wise accuracy in visual tasks (Experiment 1)

## Model: (also see comments on Figure 3 for how the code is structured)

m_fig5_full<-gam(correctness~task+s(trial, k = -1, bs = "tp", by=task)+
                   s(participant,bs='re')+s(participant,trial,bs='re')+s(prompt,bs='re')+s(prompt,task,bs='re'),
                 data=dat_dual_n192, family=binomial(link="logit"),method = "ML")

m_fig5_final<-gam(correctness~task+s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(prompt,task,bs='re'),
                  data=dat_dual_n192, family=binomial(link="logit"),method = "ML")
summary(m_fig5_final)

m_fig5_final_ref24_36<-gam(correctness~relevel(as.factor(task), ref="speech_td_24_36")+s(trial, k = -1, bs = "tp", by=task)+
                             s(participant,bs='re')+s(prompt,task,bs='re'),
                           data=dat_dual_n192, family=binomial(link="logit"),method = "ML") # Change the reference level in the model to the intermediate difficulty.
summary(m_fig5_final_ref24_36) # Model output can be found in Table C1 in the paper


## Figure (also see comments on Figure 3 for how the code is structured):

ilink<-family(m_fig5_final)$linkinv

m_fig5_final_new_data <- tidyr::expand(dat_dual_n192, nesting(participant,prompt,task),
                                       trial = unique(trial))

m_fig5_final_prediction<-bind_cols(m_fig5_final_new_data, setNames(as_tibble(predict(m_fig5_final, m_fig5_final_new_data,
                                                                                     exclude = c("s(participant)", "s(participant,trial)", "s(prompt)", "s(prompt,task)"),
                                                                                     se.fit = TRUE)[1:2]), c('fit_link','se_link')))



m_fig5_final_prediction <- mutate(m_fig5_final_prediction,
                                  fit_resp  = ilink(fit_link),
                                  right_upr = ilink(fit_link + (2 * se_link)),
                                  right_lwr = ilink(fit_link - (2 * se_link)))

m_fig5_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_fig5_final_prediction,FUN=mean)


m_fig5_final_reorder<-c('speech_td_48_60','speech_td_24_36','speech_td_0_12')

facet_labels<-c(speech_td_0_12 = "Dual Hard",speech_td_24_36 = 'Dual Intermediate', speech_td_48_60 = 'Dual Easy')

dat_dual_n192_secondary_acc_reorder <- dat_dual_n192_secondary_acc %>% 
  mutate(task = fct_relevel(task, m_fig5_final_reorder))

m_fig5_final_prediction_reorder<-m_fig5_final_prediction %>% 
  mutate(task = fct_relevel(task, m_fig5_final_reorder))


plot_m_fig5_final<-ggplot(dat_dual_n192_secondary_acc_reorder, aes(x=trial, y=correctness,color=task, shape=task)) +
  geom_errorbar(aes(ymin=correctness-se, ymax=correctness+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig5_final_prediction_reorder, aes(x=trial, y=fit_resp,color=task), size=.6) +
  geom_ribbon(data=m_fig5_final_prediction_reorder, aes(ymin=right_lwr, ymax=right_upr, x=trial, y=fit_resp,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),limits=c(0.4, 1))+
  labs(x="Trial number", y = "Accuracy")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))


plot_m_fig5_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")


# Figure 7: Trial-wise % correct in speech tasks (Experiment 2)

## Model (also see comments on Figure 3 for how the code is structured)

m_fig7_full<-gam(cbind(count_correct,3-count_correct) ~ relevel(as.factor(task), ref="speech_single") + s(trial, k = -1, bs = "tp", by=task)+
                   s(participant,bs='re')+s(participant,trial,bs='re')+s(sentence,bs='re')+s(sentence,task,bs='re'),
                 data = dat_dual_speech_n192_nophonrep,family = binomial(link="logit"),method = "ML")

m_fig7_final<-gam(cbind(count_correct,3-count_correct) ~ relevel(as.factor(task), ref="speech_single") + s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(participant,trial,bs='re'),
                  data = dat_dual_speech_n192_nophonrep,family = binomial(link="logit"),method = "ML") # Model output can be found in Table 2 in the paper


## Figure (also see comments on Figure 3 for how the code is structured)

ilink<-family(m_fig7_final)$linkinv

m_fig7_final_new_data <- tidyr::expand(dat_dual_speech_n192_nophonrep, nesting(participant,task),
                                       trial = unique(trial))

m_fig7_final_prediction<-bind_cols(m_fig7_final_new_data, setNames(as_tibble(predict(m_fig7_final, m_fig7_final_new_data, 
                                                                                     exclude = c("s(participant)","s(participant,trial)"),
                                                                                     se.fit = TRUE)[1:2]), c('fit_link','se_link')))

m_fig7_final_prediction <- mutate(m_fig7_final_prediction,
                                  fit_resp  = ilink(fit_link),
                                  right_upr = ilink(fit_link + (2 * se_link)),
                                  right_lwr = ilink(fit_link - (2 * se_link)))

m_fig7_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_fig7_final_prediction,FUN=mean)


m_fig7_final_reorder<-c('speech_single','visual','phonological','lexical')

facet_labels<-c(lexical = "Dual Lexical", phonological = "Dual Phonological",speech_single = 'Single', visual = 'Dual Visual')

dat_dual_speech_n192_nophonrep_se_reorder <- dat_dual_speech_n192_nophonrep_se %>% 
  mutate(task = fct_relevel(task, m_fig7_final_reorder))

m_fig7_final_prediction_reorder<-m_fig7_final_prediction %>% 
  mutate(task = fct_relevel(task, m_fig7_final_reorder))


plot_m_fig7_final<-ggplot(dat_dual_speech_n192_nophonrep_se_reorder,aes(x=trial, y=word_percent,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=word_percent-se, ymax=word_percent+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig7_final_prediction_reorder, aes(x=trial, y=fit_resp*100,color=task), size=.6) +
  geom_ribbon(data=m_fig7_final_prediction_reorder, aes(ymin=right_lwr*100, ymax=right_upr*100, x=trial, y=fit_resp*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits=c(0, 100))+
  labs(x="Trial number", y = "%Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))


plot_m_fig7_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")


# Figure 8: First derivative analysis for the speech task performance in Experiment 2 (also see comments on Figure 4 for how the code is structured):

library(gratia)

newd_exp2_speech <- tidyr::expand(dat_dual_speech_n192_nophonrep, nesting(participant,task),
                      trial = unique(trial))

fd_difftasks <- derivatives(m_fig7_final, type = "central", term = "s(trial)", newdata = newd_exp2_speech, unconditional = TRUE,partial_match = TRUE,interval = "simultaneous",n_sim = 10000)
fd_difftasks<-fd_difftasks %>%
  filter(lower != 0 & upper != 0)
fd_difftasks<-aggregate(cbind(derivative,se,crit,lower,upper)~smooth+data,data=fd_difftasks,FUN=mean)

fd_difftasks$smooth <- gsub("\\(", "_", fd_difftasks$smooth)
fd_difftasks$smooth <- gsub("\\):", "_", fd_difftasks$smooth)

fd_reorder_difftasks<-c('s_trial_taskspeech_single','s_trial_taskvisual','s_trial_taskphonological','s_trial_tasklexical')
facet_labels_fd_difftasks<-c(s_trial_taskspeech_single = "Single", s_trial_taskvisual = "Dual Visual",s_trial_taskphonological = 'Dual Phonological', s_trial_tasklexical = 'Dual Lexical')


dat_fd_reorder_difftasks <- fd_difftasks %>% 
  mutate(smooth = fct_relevel(smooth, fd_reorder_difftasks))


ggplot(dat_fd_reorder_difftasks,
       aes(x = data, y = derivative,colour=smooth,fill=smooth)) +
  geom_hline(aes(yintercept = 0)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  scale_y_continuous(breaks = seq(-0.08, 0.16, by = 0.02),limits=c(-0.08, 0.16))+
  labs(y = "First derivative", x = "Trial number") +
  theme_minimal()+
  facet_wrap(~ smooth,
             labeller = labeller(smooth = facet_labels_fd_difftasks))+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")

# Figure 9: Trial-wise accuracy in secondary tasks (Experiment 2)

## Model (also see comments on Figure 3 for how the code is structured):

m_fig9_full<-gam(correctness~relevel(as.factor(task), ref="visual")+s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(participant,trial,bs='re')+s(prompt,bs='re')+s(prompt,task,bs='re'),
                  data=dat_dual_n192_nophonrep, family=binomial(link="logit"),method = "ML")

m_fig9_final<-gam(correctness~relevel(as.factor(task), ref="visual")+s(trial, k = -1, bs = "tp", by=task)+
                     s(participant,bs='re')+s(participant,trial,bs='re'),
                   data=dat_dual_n192_nophonrep, family=binomial(link="logit"),method = "ML")
summary(m_fig9_final) # Model output can be found in Table C4 in the paper


## Figure (also see comments on Figure 3 for how the code is structured):

ilink<-family(m_fig9_final)$linkinv

m_fig9_final_new_data <- tidyr::expand(dat_dual_n192_nophonrep, nesting(participant,prompt,task),
                                        trial = unique(trial))

m_fig9_final_prediction<-bind_cols(m_fig9_final_new_data, setNames(as_tibble(predict(m_fig9_final, m_fig9_final_new_data,
                                                                                       exclude = c("s(participant)", "s(participant,trial)", "s(prompt)", "s(prompt,task)"),
                                                                                       se.fit = TRUE)[1:2]), c('fit_link','se_link')))



m_fig9_final_prediction <- mutate(m_fig9_final_prediction,
                                   fit_resp  = ilink(fit_link),
                                   right_upr = ilink(fit_link + (2 * se_link)),
                                   right_lwr = ilink(fit_link - (2 * se_link)))

m_fig9_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_fig9_final_prediction,FUN=mean)


m_fig9_final_reorder<-c('visual','phonological','lexical')

facet_labels<-c(lexical = "Lexical",phonological = 'Phonological', visual = 'Visual')

dat_dual_n192_nophonrep_secondary_acc_reorder <- dat_dual_n192_nophonrep_secondary_acc %>% 
  mutate(task = fct_relevel(task, m_fig9_final_reorder))

m_fig9_final_prediction_reorder<-m_fig9_final_prediction %>% 
  mutate(task = fct_relevel(task, m_fig9_final_reorder))


plot_m_fig9_final<-ggplot(dat_dual_n192_nophonrep_secondary_acc_reorder, aes(x=trial, y=correctness,color=task, shape=task)) +
  geom_errorbar(aes(ymin=correctness-se, ymax=correctness+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig9_final_prediction_reorder, aes(x=trial, y=fit_resp,color=task), size=.6) +
  geom_ribbon(data=m_fig9_final_prediction_reorder, aes(ymin=right_lwr, ymax=right_upr, x=trial, y=fit_resp,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),limits=c(0.4, 1))+
  labs(x="Trial number", y = "Accuracy")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))


plot_m_fig9_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")


# Figure D1: Difference in GAMM-estimated smooth functions between pairs of visual task conditions of the speech task

library(egg)
library(ggpubr)
library(gridExtra)

m_fig3_final_Viz<-getViz(m_fig3_final) # Vectorise the model for visualization purpose

task_single_easy<-plotDiff(s1 = sm(m_fig3_final_Viz, 4), s2 = sm(m_fig3_final_Viz, 1)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2) # pairwise comparisons between the smooths of Trial for the Single and Dual-easy conditions.


tsingleeasy_formatted<-task_single_easy + theme(axis.title = element_blank(),
                                                axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                panel.grid.major.x = element_line(color = "grey90"),
                                                panel.grid.minor.x = element_line(color = "grey90")) # Format the figure


task_single_interm<-plotDiff(s1 = sm(m_fig3_final_Viz, 3), s2 = sm(m_fig3_final_Viz, 1)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2) # pairwise comparisons between the smooths of Trial for the Single and Dual-intermediate conditions.

tsingleinterm_formatted<-task_single_interm + theme(axis.title = element_blank(),
                                                    axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                    axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                    panel.grid.major.x = element_line(color = "grey90"),
                                                    panel.grid.minor.x = element_line(color = "grey90"))

task_single_hard<-plotDiff(s1 = sm(m_fig3_final_Viz, 2), s2 = sm(m_fig3_final_Viz, 1)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2) # pairwise comparisons between the smooths of Trial for the Single and Dual-hard conditions.

tsinglehard_formatted<-task_single_hard + theme(axis.title = element_blank(),
                                                axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                panel.grid.major.x = element_line(color = "grey90"),
                                                panel.grid.minor.x = element_line(color = "grey90"))

task_easy_interm<-plotDiff(s1 = sm(m_fig3_final_Viz, 3), s2 = sm(m_fig3_final_Viz, 4)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2) # pairwise comparisons between the smooths of Trial for the Dual-easy and Dual-intermediate conditions.

teasyinterm_formatted<-task_easy_interm + theme(axis.title = element_blank(),
                                                axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                panel.grid.major.x = element_line(color = "grey90"),
                                                panel.grid.minor.x = element_line(color = "grey90"))

task_easy_hard<-plotDiff(s1 = sm(m_fig3_final_Viz, 2), s2 = sm(m_fig3_final_Viz, 4)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2) # pairwise comparisons between the smooths of Trial for the Dual-easy and Dual-hard conditions.

teasyhard_formatted<-task_easy_hard + theme(axis.title = element_blank(),
                                            axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                            axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                            panel.grid.major.x = element_line(color = "grey90"),
                                            panel.grid.minor.x = element_line(color = "grey90"))


task_interm_hard<-plotDiff(s1 = sm(m_fig3_final_Viz, 2), s2 = sm(m_fig3_final_Viz, 3)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2) # pairwise comparisons between the smooths of Trial for the Dual-intermediate and Dual-hard conditions.

tintermhard_formatted<-task_interm_hard + theme(axis.title = element_blank(),
                                                axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                panel.grid.major.x = element_line(color = "grey90"),
                                                panel.grid.minor.x = element_line(color = "grey90"))

task_singletohard <- ggarrange(tsingleeasy_formatted$ggObj, tsingleinterm_formatted$ggObj,tsinglehard_formatted$ggObj,teasyinterm_formatted$ggObj,teasyhard_formatted$ggObj,tintermhard_formatted$ggObj,
                               hjust =c(-1.2,-0.7,-1.2,-0.6,-0.95,-0.6), vjust = c(1.9,1.9,1.9,1.9,1.9,1.9),
                               labels = c("dual easy - single", "dual intermediate - single","dual hard - single","dual intermediate - dual easy","dual hard - dual easy","dual hard - dual intermediate"),
                               font.label = list(size = 12, face = "bold", color ="black"),ncol = 3, nrow = 2) # show all six figures together

annotate_figure(task_singletohard,
                bottom = text_grob("Trial number",
                                   hjust = 0.5, size = 13),
                left = text_grob("Difference in logit trend of probability", rot = 90,size = 13)
) # annotate the figure


# Figure D2: GAMM-estimated visual task RTs in millisecond at different task difficulty in Experiment 1

## Model:

m_figd2_full<-gam(rt~task+s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(participant,trial,bs='re')+s(prompt,bs='re')+s(prompt,task,bs='re'),
                  data=dat_dual_n192_sndcorrect, family=Gamma(link="log"),method = "ML")


m_figd2_final<-gam(rt~task+s(trial, k = -1, bs = "tp", by=task)+
                     s(participant,bs='re')+s(participant,trial,bs='re'),
                   data=dat_dual_n192_sndcorrect, family=Gamma(link="log"),method = "ML")
summary(m_figd2_final) # Model output can be found in Table C2 in the paper

## Figure:

ilink<-family(m_figd2_final)$linkinv

m_figd2_final_new_data <- tidyr::expand(dat_dual_n192_sndcorrect, nesting(participant,prompt,task),
                                        trial = unique(trial))

m_figd2_final_prediction<-bind_cols(m_figd2_final_new_data, setNames(as_tibble(predict(m_figd2_final, m_figd2_final_new_data, 
                                                                                       exclude = c("s(participant)","s(participant,trial)"),
                                                                                       se.fit = TRUE)[1:2]), c('fit_link','se_link')))

m_figd2_final_prediction <- mutate(m_figd2_final_prediction,
                                   fit_resp  = exp(fit_link),
                                   right_upr = exp(fit_link + (2 * se_link)),
                                   right_lwr = exp(fit_link - (2 * se_link)))

m_figd2_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_figd2_final_prediction,FUN=mean)



m_figd2_final_reorder<-c('speech_td_48_60','speech_td_24_36','speech_td_0_12')

facet_labels<-c(speech_td_0_12 = "Dual Hard",speech_td_24_36 = 'Dual Intermediate', speech_td_48_60 = 'Dual Easy')

dat_dual_n192_sndcorrect_rt_se_reorder <- dat_dual_n192_sndcorrect_rt_se %>% 
  mutate(task = fct_relevel(task, m_figd2_final_reorder))

m_figd2_final_prediction_reorder<-m_figd2_final_prediction %>% 
  mutate(task = fct_relevel(task, m_figd2_final_reorder))


plot_m_figd2_final<-ggplot(dat_dual_n192_sndcorrect_rt_se_reorder, aes(x=trial, y=rt,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), width=.1) +
  geom_point() +
  geom_line(data=m_figd2_final_prediction_reorder, aes(x=trial, y=fit_resp,color=task), size=.6) +
  geom_ribbon(data=m_figd2_final_prediction_reorder, aes(ymin=right_lwr, ymax=right_upr, x=trial, y=fit_resp,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 1400, by = 200),limits=c(0, 1400))+
  labs(x="Trial number", y = "RT (ms)")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))

plot_m_figd2_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")

# Figure D3: Difference in GAMM-estimated smooth functions between pairs of secondary task conditions of the speech task
## Also see the comments left under Fig D1 for what the code means.

library(egg)
library(ggpubr)
library(gridExtra)

m_fig7_final_Viz <- getViz(m_fig7_final)

task_single_visual<-plotDiff(s1 = sm(m_fig7_final_Viz, 4), s2 = sm(m_fig7_final_Viz, 3)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)


tsinglevisual_formatted<-task_single_visual + theme(axis.title = element_blank(),
                                                    axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                    axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                    panel.grid.major.x = element_line(color = "grey90"),
                                                    panel.grid.minor.x = element_line(color = "grey90"))


task_single_phonological<-plotDiff(s1 = sm(m_fig7_final_Viz, 2), s2 = sm(m_fig7_final_Viz, 3)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)

tsinglephonological_formatted<-task_single_phonological + theme(axis.title = element_blank(),
                                                                axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                                axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                                panel.grid.major.x = element_line(color = "grey90"),
                                                                panel.grid.minor.x = element_line(color = "grey90"))

task_single_lexical<-plotDiff(s1 = sm(m_fig7_final_Viz, 1), s2 = sm(m_fig7_final_Viz, 3)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)

tsinglelexical_formatted<-task_single_lexical + theme(axis.title = element_blank(),
                                                      axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                      axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                      panel.grid.major.x = element_line(color = "grey90"),
                                                      panel.grid.minor.x = element_line(color = "grey90"))

task_visual_phonological<-plotDiff(s1 = sm(m_fig7_final_Viz, 2), s2 = sm(m_fig7_final_Viz, 4)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)

tvisualphonological_formatted<-task_visual_phonological + theme(axis.title = element_blank(),
                                                                axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                                axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                                panel.grid.major.x = element_line(color = "grey90"),
                                                                panel.grid.minor.x = element_line(color = "grey90"))

task_visual_lexical<-plotDiff(s1 = sm(m_fig7_final_Viz, 1), s2 = sm(m_fig7_final_Viz, 4)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)

tvisuallexical_formatted<-task_visual_lexical + theme(axis.title = element_blank(),
                                                      axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                      axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                      panel.grid.major.x = element_line(color = "grey90"),
                                                      panel.grid.minor.x = element_line(color = "grey90"))


task_phonological_lexical<-plotDiff(s1 = sm(m_fig7_final_Viz, 1), s2 = sm(m_fig7_final_Viz, 2)) + l_ciPoly() + 
  l_fitLine() + geom_hline(yintercept = 0, linetype = 2)

tphonologicallexical_formatted<-task_phonological_lexical + theme(axis.title = element_blank(),
                                                                  axis.text.x = element_text(size=10,angle = 0, hjust = 0.5),
                                                                  axis.text.y = element_text(size=10,angle = 0, hjust = 0.5),
                                                                  panel.grid.major.x = element_line(color = "grey90"),
                                                                  panel.grid.minor.x = element_line(color = "grey90"))

task_singletolexical <- ggarrange(tsinglevisual_formatted$ggObj, tsinglephonological_formatted$ggObj,tsinglelexical_formatted$ggObj,tvisualphonological_formatted$ggObj,tvisuallexical_formatted$ggObj,tphonologicallexical_formatted$ggObj,
                                  hjust =c(-1,-0.6,-0.95,-0.45,-0.65,-0.45), vjust = c(1.9,1.9,1.9,1.9,1.9,1.9),
                                  labels = c("dual visual - single", "dual phonological - single","dual lexical - single","dual phonological - dual visual","dual lexical - dual visual","dual lexical - dual phonological"),
                                  font.label = list(size = 12, face = "bold", color ="black"),ncol = 3, nrow = 2)

annotate_figure(task_singletolexical,
                bottom = text_grob("Trial number",
                                   hjust = 0.5, size = 13),
                left = text_grob("Difference in logit trend of probability", rot = 90,size = 13)
)


# Figure D4: Trial-wise RTs in secondary tasks (Experiment 2)

## Model (also see comments for Figure 3 for what the code means below):

m_figd4_final_full<-gam(rt~relevel(as.factor(task), ref="visual")+s(trial, k = -1, bs = "tp", by=task)+
                          s(participant,bs='re')+s(participant,trial,bs='re')+s(prompt,bs='re')+s(prompt,task,bs='re'),
                        data=dat_dual_n192_nophonrep_sndcorrect, family=Gamma(link="log"),method = "ML")


m_figd4_final_final<-gam(rt~relevel(as.factor(task), ref="visual")+s(trial, k = 6, bs = "tp", by=task)+
                           s(participant,bs='re')+s(participant,trial,bs='re'),
                         data=dat_dual_n192_nophonrep_sndcorrect, family=Gamma(link="log"),method = "ML")
summary(m_figd4_final_final) # Model output can be found in Table C5 in the paper

## Plots (also see comments for Figure 3 for what the code means below)

ilink<-family(m_figd4_final_final)$linkinv

m_figd4_final_final_new_data <- tidyr::expand(dat_dual_n192_nophonrep_sndcorrect, nesting(participant,prompt,task),
                                              trial = unique(trial))

m_figd4_final_final_prediction<-bind_cols(m_figd4_final_final_new_data, setNames(as_tibble(predict(m_figd4_final_final, m_figd4_final_final_new_data, 
                                                                                                   exclude = c("s(participant)","s(participant,trial)"),
                                                                                                   se.fit = TRUE)[1:2]), c('fit_link','se_link')))

m_figd4_final_final_prediction <- mutate(m_figd4_final_final_prediction,
                                         fit_resp  = exp(fit_link),
                                         right_upr = exp(fit_link + (2 * se_link)),
                                         right_lwr = exp(fit_link - (2 * se_link)))

m_figd4_final_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_figd4_final_final_prediction,FUN=mean)



m_figd4_final_final_reorder<-c('visual','phonological','lexical')

facet_labels<-c(lexical = "Lexical",phonological = 'Phonological', visual = 'Visual')

dat_dual_n192_nophonrep_sndcorrect_se_reorder <- dat_dual_n192_nophonrep_sndcorrect_se %>% 
  mutate(task = fct_relevel(task, m_figd4_final_final_reorder))

m_figd4_final_final_prediction_reorder<-m_figd4_final_final_prediction %>% 
  mutate(task = fct_relevel(task, m_figd4_final_final_reorder))


plot_m_figd4_final_final<-ggplot(dat_dual_n192_nophonrep_sndcorrect_se_reorder, aes(x=trial, y=rt,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), width=.1) +
  geom_point() +
  geom_line(data=m_figd4_final_final_prediction_reorder, aes(x=trial, y=fit_resp,color=task), size=.6) +
  geom_ribbon(data=m_figd4_final_final_prediction_reorder, aes(ymin=right_lwr, ymax=right_upr, x=trial, y=fit_resp,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 1500, by = 200),limits=c(0, 1500))+
  labs(title="Trial-wise RTs in secondary tasks (Experiment 2)",
       x="Trial number", y = "RT (ms)")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))

plot_m_figd4_final_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")

# Figure D5: Overall performance in the single speech task (current vs pilot experiments)


## Figure:

dat_single_40t_60t_n78_se$task<-factor(dat_single_40t_60t_n78_se$task, levels=c("speech_single","speech_pilot_60t")) # reorder the factors for plot
dat_single_40t_60t_n78_task_label <- c("Current","Pilot") # Labels for the two task conditions

plot_single_40t_60t_n78<-ggplot(dat_single_40t_60t_n78_se, aes(x=task, y=word_percent,fill=task)) +
  geom_boxplot(outlier.shape=NA,width = 0.5)+
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "grey") +
  geom_point(aes(fill=task,group=participant),size=2,shape=21, position = position_dodge(0.2),alpha = 0.5)+
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  scale_x_discrete(labels= dat_single_40t_60t_n78_task_label)+
  scale_fill_npg()+
  coord_cartesian(ylim = c(0, 100))+
  labs(x="Experiment", y = "%Correct")+
  theme_minimal() # Generate a figure

plot_single_40t_60t_n78+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # change some formatting and show the figure

## Model:

m_figd5_full<-glmer(cbind(count_correct,3-count_correct)~1+task+(1|participant)+(1+task|sentence),
                    data=dat_single_40t_60t_n78, family = binomial(link = "logit"), 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10e6)))
summary(m_figd5_full)


m_figd5_final<-glmer(cbind(count_correct,3-count_correct)~1+task+(1|participant),
                 data=dat_single_40t_60t_n78, family = binomial(link = "logit"), 
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10e6)))
summary(m_figd5_final)


# Figure D6: Trial-wise % correct in speech tasks (phonological and phonological replication)

## Model (also see the comments for Figure 3 for what the code means below):

m_figd6_full<-gam(cbind(count_correct,3-count_correct) ~ relevel(as.factor(task), ref="phonological") + s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(participant,trial,bs='re')+s(sentence,bs='re')+s(sentence,task,bs='re'),
                  data = dat_dual_speech_n96_onlyphonrep,family = binomial(link="logit"),method = "ML")


m_figd6_final<-gam(cbind(count_correct,3-count_correct) ~ relevel(as.factor(task), ref="phonological") + s(trial, k = -1, bs = "tp", by=task)+
                     s(participant,bs='re')+s(participant,trial,bs='re'),
                   data = dat_dual_speech_n96_onlyphonrep,family = binomial(link="logit"),method = "ML")

summary(m_figd6_final)

## Figure (also see the comments for Figure 3 for what the code means below):


ilink<-family(m_figd6_final)$linkinv

m_figd6_final_new_data <- tidyr::expand(dat_dual_speech_n96_onlyphonrep, nesting(participant,task),
                                        trial = unique(trial))

m_figd6_final_prediction<-bind_cols(m_figd6_final_new_data, setNames(as_tibble(predict(m_figd6_final, m_figd6_final_new_data, 
                                                                                       exclude = c("s(participant)","s(participant,trial)"),
                                                                                       se.fit = TRUE)[1:2]), c('fit_link','se_link')))

m_figd6_final_prediction <- mutate(m_figd6_final_prediction,
                                   fit_resp  = ilink(fit_link),
                                   right_upr = ilink(fit_link + (2 * se_link)),
                                   right_lwr = ilink(fit_link - (2 * se_link)))

m_figd6_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_figd6_final_prediction,FUN=mean)


m_figd6_final_reorder<-c('phonological','phon_replication')

facet_labels<-c(phon_replication = "Phonological Replication", phonological = "Phonological")

dat_dual_speech_n96_onlyphonrep_se_reorder <- dat_dual_speech_n96_onlyphonrep_se %>% 
  mutate(task = fct_relevel(task, m_figd6_final_reorder))

m_figd6_final_prediction_reorder<-m_figd6_final_prediction %>% 
  mutate(task = fct_relevel(task, m_figd6_final_reorder))


plot_m_figd6_final<-ggplot(dat_dual_speech_n96_onlyphonrep_se_reorder,aes(x=trial, y=word_percent,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=word_percent-se, ymax=word_percent+se), width=.1) +
  geom_point() +
  geom_line(data=m_figd6_final_prediction_reorder, aes(x=trial, y=fit_resp*100,color=task), size=.6) +
  geom_ribbon(data=m_figd6_final_prediction_reorder, aes(ymin=right_lwr*100, ymax=right_upr*100, x=trial, y=fit_resp*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits=c(40, 100))+
  labs(x="Trial number", y = "%Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))


plot_m_figd6_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")


# Figure D7: Trial-wise accuracy in secondary tasks (phonological and phonological replication)

## Model (also see the comments for Figure 3 for what the code means below):

m_figd7_full<-gam(correctness~relevel(as.factor(task), ref="phonological")+s(trial, k = -1, bs = "tp", by=task)+
                    s(participant,bs='re')+s(participant,trial,bs='re')+s(prompt,bs='re')+s(prompt,task,bs='re'),
                  data=dat_dual_speech_n96_onlyphonrep, family=binomial(link="logit"),method = "ML")

m_figd7_final<-gam(correctness~relevel(as.factor(task), ref="phonological")+s(trial, k = -1, bs = "tp", by=task)+
                     s(participant,bs='re')+s(participant,trial,bs='re'),
                   data=dat_dual_speech_n96_onlyphonrep, family=binomial(link="logit"),method = "ML")
summary(m_figd7_final)

## Figure (also see the comments for Figure 3 for what the code means below):

ilink<-family(m_figd7_final)$linkinv

m_figd7_final_new_data <- tidyr::expand(dat_dual_speech_n96_onlyphonrep, nesting(participant,prompt,task),
                                        trial = unique(trial))

m_figd7_final_prediction<-bind_cols(m_figd7_final_new_data, setNames(as_tibble(predict(m_figd7_final, m_figd7_final_new_data,
                                                                                       exclude = c("s(participant)", "s(participant,trial)", "s(prompt)", "s(prompt,task)"),
                                                                                       se.fit = TRUE)[1:2]), c('fit_link','se_link')))



m_figd7_final_prediction <- mutate(m_figd7_final_prediction,
                                   fit_resp  = ilink(fit_link),
                                   right_upr = ilink(fit_link + (2 * se_link)),
                                   right_lwr = ilink(fit_link - (2 * se_link)))

m_figd7_final_prediction<-aggregate(cbind(fit_link,se_link,fit_resp,right_upr,right_lwr)~trial+task,data=m_figd7_final_prediction,FUN=mean)


m_figd7_final_reorder<-c('phonological','phon_replication')

facet_labels<-c(phon_replication = "Phonological Replication", phonological = "Phonological")

dat_dual_n96_onlyphonrep_secondary_acc_reorder <- dat_dual_n96_onlyphonrep_secondary_acc %>% 
  mutate(task = fct_relevel(task, m_figd7_final_reorder))

m_figd7_final_prediction_reorder<-m_figd7_final_prediction %>% 
  mutate(task = fct_relevel(task, m_figd7_final_reorder))


plot_m_figd7_final<-ggplot(dat_dual_n96_onlyphonrep_secondary_acc_reorder, aes(x=trial, y=correctness,color=task, shape=task)) +
  geom_errorbar(aes(ymin=correctness-se, ymax=correctness+se), width=.1) +
  geom_point() +
  geom_line(data=m_figd7_final_prediction_reorder, aes(x=trial, y=fit_resp,color=task), size=.6) +
  geom_ribbon(data=m_figd7_final_prediction_reorder, aes(ymin=right_lwr, ymax=right_upr, x=trial, y=fit_resp,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),limits=c(0.5, 1))+
  labs(x="Trial number", y = "Accuracy")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))


plot_m_figd7_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")


# Figure D8: Effort and attention questionnaire (Experiment 1)

## Figure: 

dat_exp1_question_n192_long_sdfiltered$measure <- factor(dat_exp1_question_n192_long_sdfiltered$measure, levels = c("effort","attention")) # Label the levels for the Measure variable
facet_labels<-c(speech = "Speech task response", visual = "Visual task response") # Labels for the facet grids in the figure


plot_effortques_speechvisual_sdfiltered<-ggplot(dat_exp1_question_n192_long_sdfiltered, aes(x=measure, y=rating,fill=task.difficulty)) +
  geom_boxplot(outlier.shape=NA,position=position_dodge(width=0.9))+
  stat_summary(fun.y = "mean", geom = "point", aes(group=task.difficulty), shape = 23, size = 3, fill = "grey",position=position_dodge(0.9)) +
  geom_point(aes(group=task.difficulty),size=2,shape=21, position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.15),alpha = 0.5)+
  scale_x_discrete(labels=c("effort" = "Effort", attention = "Attention"))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_npg(labels = c("Single", "Dual Easy","Dual Intermediate","Dual Hard"))+
  labs(x="Measure", y = "Participant estimate",
       fill = "Task")+
  facet_wrap(~task,labeller = labeller(task = facet_labels))+
  theme_minimal() # Generate a figure


plot_effortques_speechvisual_sdfiltered+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11.5),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11)) # Update the format of the figure

## Models:

### effort speech (no outlier)

ques_speechvisual_speecheffort_m_lm_nooutlier<-lm(effort.speech ~ relevel(as.factor(task.difficulty), ref="speech_single"), na.action = na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered) #simple linear regression

summary(ques_speechvisual_speecheffort_m_lm_nooutlier) # Model output can be found in Table C3


### attention speech (no outlier)

ques_speechvisual_speechattention_m_lm_nooutlier<-lm(attention.speech ~ relevel(as.factor(task.difficulty), ref="speech_single"), na.action = na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered) 

summary(ques_speechvisual_speechattention_m_lm_nooutlier) # Model output can be found in Table C3


### effort visual (no outlier)

ques_speechvisual_visualeffort_m_lm_nooutlier<-lm(effort.visual ~ relevel(as.factor(task.difficulty), ref="speech_td_48_60"), na.action = na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered)

summary(ques_speechvisual_visualeffort_m_lm_nooutlier) # Model output can be found in Table C3


### attention visual (no outlier)

ques_speechvisual_visualattention_m_lm_nooutlier<-lm(attention.visual ~ relevel(as.factor(task.difficulty), ref="speech_td_48_60"), na.action=na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered)

summary(ques_speechvisual_visualattention_m_lm_nooutlier) # Model output can be found in Table C3



# Figure D9: Effort and attention questionnaire (Experiment 2)

## Figure (see also the comments for Figure D8 for what the code means below):

dat_exp2_question_n192_long_sdfiltered$measure <- factor(dat_exp2_question_n192_long_sdfiltered$measure, levels = c("effort","attention"))
dat_exp2_question_n192_long_sdfiltered$task <- factor(dat_exp2_question_n192_long_sdfiltered$task, levels = c("speech","secondary"))


facet_labels<-c(speech = "Speech task response", secondary = "Secondary task response")


plot_effortques_speechvisual_sdfiltered<-ggplot(dat_exp2_question_n192_long_sdfiltered, aes(x=measure, y=rating, fill=task.type)) +
  geom_boxplot(outlier.shape=NA,position=position_dodge(width=0.9))+
  stat_summary(fun.y = "mean", geom = "point", aes(group=task.type), shape = 23, size = 3, fill = "grey",position=position_dodge(0.9)) +
  geom_point(aes(group=task.type),size=2,shape=21, position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.15), alpha = 0.55)+
  #geom_jitter()+
  scale_x_discrete(labels=c("effort" = "Effort", attention = "Attention"))+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_npg(labels = c("Single", "Visual","Phonological","Lexical"))+
  #scale_colour_npg(labels = c("Single", "Visual","Phonological","Lexical"))+
  labs(x="Measure", y = "Participant estimate",
       fill = "Task")+
  facet_wrap(~task,labeller = labeller(task = facet_labels))+
  theme_minimal()


plot_effortques_speechvisual_sdfiltered+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11.5),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))

## Models:

#### effort speech (no outlier)

ques_speechvisual_speecheffort_m_lm_nophonrep_nooutlier<-lm(effort.speech ~ relevel(as.factor(task.type), ref="speech_single"), na.action = na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechvisual_speecheffort_m_lm_nophonrep_nooutlier) # Model output can be found in Table C6


#### attention speech (no outlier)

ques_speechvisual_speechattention_m_lm_nophonrep_nooutlier<-lm(attention.speech ~ relevel(as.factor(task.type), ref="speech_single"), na.action = na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechvisual_speechattention_m_lm_nophonrep_nooutlier) # Model output can be found in Table C6


#### effort secondary (no outlier)

ques_speechtsecondary_tsecondaryeffort_m_lm_nophonrep_nooutlier<-lm(effort.tsecondary ~ relevel(as.factor(task.type), ref="visual"), na.action = na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechtsecondary_tsecondaryeffort_m_lm_nophonrep_nooutlier) # Model output can be found in Table C6


#### attention secondary (no outlier)

ques_speechtsecondary_tsecondaryattention_m_lm_nophonrep_nooutlier<-lm(attention.tsecondary ~ relevel(as.factor(task.type), ref="visual"), na.action=na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechtsecondary_tsecondaryattention_m_lm_nophonrep_nooutlier) # Model output can be found in Table C6
