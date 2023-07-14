# Copyright: Han Wang, June 8th 2022.

# Edited on July 3rd 2023 (added analysis on fixed-effect model fit comparisons)
# Edited on June 15th 2023 (updated the figure numbers)
# Edited on May 22nd 2023 (Added an analysis on the relationship between the slopes of the two tasks)
# Edited on January 24th 2023 (updated comments and fixed typos)
# Edited on August 16th 2022 (implementing GLMMs for the main analysis)
# Edited on July 14th 2022 (included Bayes factor analysis)
# Edited on July 7th 2022 (removed the figure titles)
# Edited on June 14th 2022 (updated the figure numbers)


# Load packages and define some functions

library(car)
library(dplyr)
library(tidyr)

library(ggplot2)
library(ggpubr)
library(ggsci)
library(ggeffects)
library(sjPlot)
library(forcats)

library(lme4)
library(lmerTest)

library(mgcv)
library(mgcViz)
library(itsadug)

library(rcompanion)

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

### Single task performance (current vs pilot experiments, Figure C3)

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


# Table B1: Exploratory analysis for fixed-effect model fit comparisons

## Experiment 1:

### Speech-task % correct model

m_exp1_sp_lm <- glm(cbind(count_correct,3-count_correct) ~ trial * task, family = binomial, data = dat_dual_speech_n192)
m_exp1_sp_log <- glm(cbind(count_correct,3-count_correct) ~ log(trial) * task, family = binomial, data = dat_dual_speech_n192)

compareGLM(m_exp1_sp_lm,m_exp1_sp_log)

### Visual-task correctness model

m_exp1_vis_acc_lm <- glm(correctness ~ poly(trial,2) * task, family = binomial, data = dat_dual_n192)
m_exp1_vis_acc_poly <- glm(correctness ~ poly(trial,1) * task, family = binomial, data = dat_dual_n192)

compareGLM(m_exp1_vis_acc_lm, m_exp1_vis_acc_poly)

### Visual-task RT model

m_exp1_vis_rt_lm <- glm(rt ~ trial * task, family = Gamma(link = "log"), data = dat_dual_n192_sndcorrect)
m_exp1_vis_rt_log <- glm(rt ~ log(trial) * task, family = Gamma(link = "log"), data = dat_dual_n192_sndcorrect)

compareGLM(m_exp1_vis_rt_lm, m_exp1_vis_rt_log)

## Experiment 2

### Speech-task % correct model

m_exp2_sp_lm <- glm(cbind(count_correct,3-count_correct) ~ trial * task, family = binomial, data = dat_dual_speech_n192_nophonrep)
m_exp2_sp_log <- glm(cbind(count_correct,3-count_correct) ~ log(trial) * task, family = binomial, data = dat_dual_speech_n192_nophonrep)

compareGLM(m_exp2_sp_lm,m_exp2_sp_log)


### Secondary-task correctness model

m_exp2_2nd_acc_lm <- glm(correctness ~ trial * task, family = binomial, data = dat_dual_n192_nophonrep)
m_exp2_2nd_acc_log <- glm(correctness ~ log(trial) * task, family = binomial, data = dat_dual_n192_nophonrep)

compareGLM(m_exp2_2nd_acc_lm, m_exp2_2nd_acc_log)

### Secondary-task RT model

m_exp2_2nd_rt_lm <- glm(rt ~ trial * task, family = Gamma(link = "log"), data = dat_dual_n192_nophonrep_sndcorrect)
m_exp2_2nd_rt_log <- glm(rt ~ log(trial) * task, family = Gamma(link = "log"), data = dat_dual_n192_nophonrep_sndcorrect)

compareGLM(m_exp2_2nd_rt_lm, m_exp2_2nd_rt_log)


# Figure 3: Trial-wise % correct in speech tasks (Experiment 1)

## Model

### glmer_full


m_fig3_log_glmer_full<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant)+(1+task|sentence),
                             data=dat_dual_speech_n192, family = binomial(link = "logit"), 
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig3_log_glmer_full)



### glmer_final (best fitting model)

m_fig3_log_glmer_final<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant),
                              data=dat_dual_speech_n192, family = binomial(link = "logit"), 
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig3_log_glmer_final)

m_fig3_log_glmer_final_relvl012<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="speech_td_0_12")+(1+log(trial)|participant),
                                       data=dat_dual_speech_n192, family = binomial(link = "logit"), 
                                       control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig3_log_glmer_final_relvl012)


m_fig3_log_glmer_final_relvl2436<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="speech_td_24_36")+(1+log(trial)|participant),
                                        data=dat_dual_speech_n192, family = binomial(link = "logit"), 
                                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig3_log_glmer_final_relvl2436)


m_fig3_log_glmer_final_relvl4860<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="speech_td_48_60")+(1+log(trial)|participant),
                                        data=dat_dual_speech_n192, family = binomial(link = "logit"), 
                                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig3_log_glmer_final_relvl4860)



gg_m_fig3_log_glmer_final<-ggpredict(m_fig3_log_glmer_final, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_fig3_log_glmer_final) # plot the model

gg_m_fig3_log_glmer_final$task<-gg_m_fig3_log_glmer_final$group


### plot

m_fig3_final_reorder<-c('speech_single','speech_td_48_60','speech_td_24_36','speech_td_0_12') # reorder the levels for plotting

facet_labels<-c(speech_single = "Single", speech_td_0_12 = "Dual Hard",speech_td_24_36 = 'Dual Intermediate', speech_td_48_60 = 'Dual Easy') # Set the label for panels in the plot

dat_dual_speech_n192_se_reorder <- dat_dual_speech_n192_se %>% 
  mutate(task = fct_relevel(task, m_fig3_final_reorder)) # reorder the raw data per level of task

m_fig3_final_prediction_reorder<-gg_m_fig3_log_glmer_final %>% 
  mutate(task = fct_relevel(task, m_fig3_final_reorder)) # reorder the prediction per level of task

plot_m_fig3_final<-ggplot(dat_dual_speech_n192_se_reorder,aes(x=trial, y=word_percent,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=word_percent-se, ymax=word_percent+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig3_final_prediction_reorder, aes(x=x, y=predicted*100,color=task), size=.6) +
  geom_ribbon(data=m_fig3_final_prediction_reorder, aes(ymin=conf.low*100, ymax=conf.high*100, x=x, y=predicted*100,fill=task,color=task), alpha = 0.2) +# error band
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



# Figure 4: Trial-wise accuracy in visual tasks (Experiment 1)

## Model: (also see comments on Figure 3 for how the code is structured)

### glmer_poly_full

m_fig4_poly_glmer_full<-glmer(correctness~1+poly(trial,2)*task+(1+poly(trial,2)|participant)+(1+task|prompt),
                              data=dat_dual_n192, family = binomial(link = "logit"), 
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig4_poly_glmer_full)


gg_m_fig4_poly_glmer_full<-ggpredict(m_fig4_poly_glmer_full, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_fig4_poly_glmer_full) # plot the model

gg_m_fig4_poly_glmer_full$task<-gg_m_fig4_poly_glmer_full$group

# Note the main function ggplot() and the geom_line() read different datasets. This allows us to draw the raw data and the fitted values from the model on the same graph
ggplot(dat_dual_n192_secondary_acc, aes(x=trial, y=correctness, color=task, shape = task)) + 
  geom_errorbar(aes(ymin=correctness-se, ymax=correctness+se), width=.1) +
  geom_point() +
  geom_line(data=gg_m_fig4_poly_glmer_full, aes(x=x, y=predicted, colour = task), size=.6) +
  geom_ribbon(data=gg_m_fig4_poly_glmer_full, aes(ymin=conf.low, ymax=conf.high, x=x, y=predicted, color = task, fill=task), alpha = 0.2) +# error band
  labs(x="Trial number", y = "%Correct")+
  facet_wrap(~ task) +
  theme_bw()


### glmer_poly_3 (best fitting model)


m_fig4_poly_glmer_3<-glmer(correctness~1+poly(trial,2)*task+(1|prompt)+(0+poly(trial,2)|participant),
                           data=dat_dual_n192, family = binomial(link = "logit"), 
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig4_poly_glmer_3)

m_fig4_poly_glmer_3_relvl_2436<-glmer(correctness~1+poly(trial,2)*relevel(as.factor(task), ref="speech_td_24_36")+(1|prompt)+(0+poly(trial,2)|participant),
                                      data=dat_dual_n192, family = binomial(link = "logit"), 
                                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig4_poly_glmer_3_relvl_2436)

m_fig4_poly_glmer_3_relvl_4860<-glmer(correctness~1+poly(trial,2)*relevel(as.factor(task), ref="speech_td_48_60")+(1|prompt)+(0+poly(trial,2)|participant),
                                      data=dat_dual_n192, family = binomial(link = "logit"), 
                                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig4_poly_glmer_3_relvl_4860)


gg_m_fig4_poly_glmer_3<-ggpredict(m_fig4_poly_glmer_3, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_fig4_poly_glmer_3) # plot the model

gg_m_fig4_poly_glmer_3$task<-gg_m_fig4_poly_glmer_3$group


### plot

m_fig4_final_reorder<-c('speech_td_48_60','speech_td_24_36','speech_td_0_12')

facet_labels<-c(speech_td_0_12 = "Dual Hard",speech_td_24_36 = 'Dual Intermediate', speech_td_48_60 = 'Dual Easy')

dat_dual_n192_secondary_acc_reorder <- dat_dual_n192_secondary_acc %>% 
  mutate(task = fct_relevel(task, m_fig4_final_reorder))

m_fig4_final_prediction_reorder<-gg_m_fig4_poly_glmer_3 %>% 
  mutate(task = fct_relevel(task, m_fig4_final_reorder))


plot_m_fig4_final<-ggplot(dat_dual_n192_secondary_acc_reorder, aes(x=trial, y=correctness*100,color=task, shape=task)) +
  geom_errorbar(aes(ymin=(correctness-se)*100, ymax=(correctness+se)*100), width=.1) +
  geom_point() +
  geom_line(data=m_fig4_final_prediction_reorder, aes(x=x, y=predicted*100,color=task), size=.6) +
  geom_ribbon(data=m_fig4_final_prediction_reorder, aes(ymin=conf.low*100, ymax=conf.high*100, x=x, y=predicted*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits=c(40, 100))+
  labs(x="Trial number", y = "% Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels))


plot_m_fig4_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none")


# Figure 5: GLMM-estimated visual task RTs in millisecond at different task difficulty in Experiment 1

## Model:

### full:

m_fig5_glmer_full<-glmer(rt~1+log(trial)*task+(1+log(trial)|participant)+(1+task|prompt),
                         data=dat_dual_n192_sndcorrect, family = Gamma(link = "log"), 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))

summary(m_fig5_glmer_full)

### glmer_1 (best fitting model)

m_fig5_glmer_1<-glmer(rt~1+log(trial)*task+(1|participant)+(1|prompt),
                      data=dat_dual_n192_sndcorrect, family = Gamma(link = "log"), 
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))

summary(m_fig5_glmer_1)

gg_m_fig5_glmer_1<-ggpredict(m_fig5_glmer_1, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_fig5_glmer_1) # plot the model

gg_m_fig5_glmer_1$task<-gg_m_fig5_glmer_1$group


### plot

m_fig5_final_reorder<-c('speech_td_48_60','speech_td_24_36','speech_td_0_12')

facet_labels<-c(speech_td_0_12 = "Dual Hard",speech_td_24_36 = 'Dual Intermediate', speech_td_48_60 = 'Dual Easy')

dat_dual_n192_sndcorrect_rt_se_reorder <- dat_dual_n192_sndcorrect_rt_se %>% 
  mutate(task = fct_relevel(task, m_fig5_final_reorder))

m_fig5_final_prediction_reorder<-gg_m_fig5_glmer_1 %>% 
  mutate(task = fct_relevel(task, m_fig5_final_reorder))


plot_m_fig5_final<-ggplot(dat_dual_n192_sndcorrect_rt_se_reorder, aes(x=trial, y=rt,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig5_final_prediction_reorder, aes(x=x, y=predicted,color=task), size=.6) +
  geom_ribbon(data=m_fig5_final_prediction_reorder, aes(ymin=conf.low, ymax=conf.high, x=x, y=predicted,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(300, 1400, by = 200),limits=c(300, 1400))+
  labs(x="Trial number", y = "RT (ms)")+
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



m_fig5_glmer_1_relvl2436<-glmer(rt~1+log(trial)*relevel(as.factor(task), ref="speech_td_24_36")+(1|participant)+(1|prompt),
                                data=dat_dual_n192_sndcorrect, family = Gamma(link = "log"), 
                                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))

summary(m_fig5_glmer_1_relvl2436)



# Figure 7: Trial-wise % correct in speech tasks (Experiment 2)


## Log-transformed GLMER models:

### glmer_full

m_fig7_log_glmer_full<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant)+(1+task|sentence),
                             data=dat_dual_speech_n192_nophonrep, family = binomial(link = "logit"), 
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig7_log_glmer_full)


### glmer_final (best fitting model)

m_fig7_log_glmer_final<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant),
                              data=dat_dual_speech_n192_nophonrep, family = binomial(link = "logit"), 
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig7_log_glmer_final)


m_fig7_log_glmer_final_phon<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="phonological")+(1+log(trial)|participant),
                                   data=dat_dual_speech_n192_nophonrep, family = binomial(link = "logit"), 
                                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig7_log_glmer_final_phon)

m_fig7_log_glmer_final_lexical<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="lexical")+(1+log(trial)|participant),
                                      data=dat_dual_speech_n192_nophonrep, family = binomial(link = "logit"), 
                                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig7_log_glmer_final_lexical)

m_fig7_log_glmer_final_single<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="speech_single")+(1+log(trial)|participant),
                                     data=dat_dual_speech_n192_nophonrep, family = binomial(link = "logit"), 
                                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig7_log_glmer_final_single)

m_fig7_log_glmer_final_visual<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref="visual")+(1+log(trial)|participant),
                                     data=dat_dual_speech_n192_nophonrep, family = binomial(link = "logit"), 
                                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_fig7_log_glmer_final_visual)


### plot

m_fig7_final_reorder<-c('speech_single','visual','phonological','lexical') # reorder the levels for plotting

facet_labels<-c(lexical = "Dual Lexical", phonological = "Dual Phonological",speech_single = 'Single', visual = 'Dual Visual') # Set the label for panels in the plot

dat_dual_speech_n192_nophonrep_se_reorder <- dat_dual_speech_n192_nophonrep_se %>% 
  mutate(task = fct_relevel(task, m_fig7_final_reorder)) # reorder the raw data per level of task

m_fig7_final_prediction_reorder<-gg_m_fig7_log_glmer_final %>% 
  mutate(task = fct_relevel(task, m_fig7_final_reorder)) # reorder the prediction per level of task

plot_m_fig7_final<-ggplot(dat_dual_speech_n192_nophonrep_se_reorder,aes(x=trial, y=word_percent,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=word_percent-se, ymax=word_percent+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig7_final_prediction_reorder, aes(x=x, y=predicted*100,color=task), size=.6) +
  geom_ribbon(data=m_fig7_final_prediction_reorder, aes(ymin=conf.low*100, ymax=conf.high*100, x=x, y=predicted*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits=c(0, 100))+
  labs(x="Trial number", y = "%Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels)) # main figure


plot_m_fig7_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # update the formatting for the plot


# Figure 8: Trial-wise accuracy in secondary tasks (Experiment 2)

## Model (also see comments on Figure 3 for how the code is structured):

### glmer_full

m_fig8_log_glmer_full<-glmer(correctness~1+log(trial)*task+(1+log(trial)|participant)+(1+task|prompt),
                             data=dat_dual_n192_nophonrep, family = binomial(link = "logit"), 
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig8_log_glmer_full)

### glmer_final (best fitting model)

m_fig8_log_glmer_final<-glmer(correctness~1+log(trial)*task+(1+log(trial)|participant),
                              data=dat_dual_n192_nophonrep, family = binomial(link = "logit"), 
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig8_log_glmer_final)


gg_m_fig8_log_glmer_final<-ggpredict(m_fig8_log_glmer_final, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_fig8_log_glmer_final) # plot the model

gg_m_fig8_log_glmer_final$task<-gg_m_fig8_log_glmer_final$group



m_fig8_log_glmer_final_phon<-glmer(correctness~1+log(trial)*relevel(as.factor(task), ref="phonological")+(1+log(trial)|participant),
                                   data=dat_dual_n192_nophonrep, family = binomial(link = "logit"), 
                                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig8_log_glmer_final_phon)

m_fig8_log_glmer_final_vis<-glmer(correctness~1+log(trial)*relevel(as.factor(task), ref="visual")+(1+log(trial)|participant),
                                  data=dat_dual_n192_nophonrep, family = binomial(link = "logit"), 
                                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_fig8_log_glmer_final_vis)

### plot

m_fig8_final_reorder<-c('visual','phonological','lexical') # reorder the levels for plotting

facet_labels<-c(lexical = "Dual Lexical",phonological = 'Dual Phonological', visual = 'Dual Visual') # Set the label for panels in the plot

dat_dual_n192_nophonrep_secondary_acc_reorder <- dat_dual_n192_nophonrep_secondary_acc %>% 
  mutate(task = fct_relevel(task, m_fig8_final_reorder)) # reorder the raw data per level of task

m_fig8_final_prediction_reorder<-gg_m_fig8_log_glmer_final %>% 
  mutate(task = fct_relevel(task, m_fig8_final_reorder)) # reorder the prediction per level of task

plot_m_fig8_final<-ggplot(dat_dual_n192_nophonrep_secondary_acc_reorder,aes(x=trial, y=correctness*100,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=(correctness-se)*100, ymax=(correctness+se)*100), width=.1) +
  geom_point() +
  geom_line(data=m_fig8_final_prediction_reorder, aes(x=x, y=predicted*100,color=task), size=.6) +
  geom_ribbon(data=m_fig8_final_prediction_reorder, aes(ymin=conf.low*100, ymax=conf.high*100, x=x, y=predicted*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(40, 100, by = 10),limits=c(40, 100))+
  labs(x="Trial number", y = "%Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels)) # main figure


plot_m_fig8_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # update the formatting for the plot



# Figure 9: Trial-wise RTs in secondary tasks (Experiment 2)

## Model (also see comments for Figure 3 for what the code means below):

### full:

m_fig9_glmer_full<-glmer(rt~1+log(trial)*task+(1+log(trial)|participant)+(1+task|prompt),
                         data=dat_dual_n192_nophonrep_sndcorrect, family = Gamma(link = "log"), 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))

summary(m_fig9_glmer_full)


### glmer_3 (best fitting model)

m_fig9_glmer_3<-glmer(rt~1+log(trial)*task+(1+task|prompt),
                      data=dat_dual_n192_nophonrep_sndcorrect, family = Gamma(link = "log"), 
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))

summary(m_fig9_glmer_3)

gg_m_fig9_glmer_3<-ggpredict(m_fig9_glmer_3, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_fig9_glmer_3) # plot the model

gg_m_fig9_glmer_3$task<-gg_m_fig9_glmer_3$group


### plot

m_fig9_final_reorder<-c('visual','phonological','lexical') # reorder the levels for plotting

facet_labels<-c(lexical = "Dual Lexical",phonological = 'Dual Phonological', visual = 'Dual Visual') # Set the label for panels in the plot

dat_dual_n192_nophonrep_sndcorrect_se_reorder <- dat_dual_n192_nophonrep_sndcorrect_se %>% 
  mutate(task = fct_relevel(task, m_fig9_final_reorder)) # reorder the raw data per level of task

m_fig9_final_prediction_reorder<-gg_m_fig9_glmer_3 %>% 
  mutate(task = fct_relevel(task, m_fig9_final_reorder)) # reorder the prediction per level of task

plot_m_fig9_final<-ggplot(dat_dual_n192_nophonrep_sndcorrect_se_reorder,aes(x=trial, y=rt,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=rt-se, ymax=rt+se), width=.1) +
  geom_point() +
  geom_line(data=m_fig9_final_prediction_reorder, aes(x=x, y=predicted,color=task), size=.6) +
  geom_ribbon(data=m_fig9_final_prediction_reorder, aes(ymin=conf.low, ymax=conf.high, x=x, y=predicted,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(400, 1500, by = 200),limits=c(400, 1500))+
  labs(x="Trial number", y = "RT (ms)")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels)) # main figure


plot_m_fig9_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # update the formatting for the plot


# Figure C1: Overall performance in the single speech task (current vs pilot experiments)


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

m_figc1_full<-glmer(cbind(count_correct,3-count_correct)~1+task+(1|participant)+(1+task|sentence),
                    data=dat_single_40t_60t_n78, family = binomial(link = "logit"), 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10e6)))
summary(m_figc1_full)


m_figc1_final<-glmer(cbind(count_correct,3-count_correct)~1+task+(1|participant),
                     data=dat_single_40t_60t_n78, family = binomial(link = "logit"), 
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10e6)))
summary(m_figc1_final)


# Figure C2: Trial-wise % correct in speech tasks (phonological and phonological replication)

## Model (also see the comments for Figure 3 for what the code means below):

### full

m_figc2_log_glmer_full<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant)+(1+task|sentence),
                              data=dat_dual_speech_n96_onlyphonrep, family = binomial(link = "logit"), 
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_figc2_log_glmer_full)

### glmer_1 (best fitting model)

m_figc2_log_glmer_1<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant),
                           data=dat_dual_speech_n96_onlyphonrep, family = binomial(link = "logit"), 
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_figc2_log_glmer_1)

gg_m_figc2_log_glmer_1<-ggpredict(m_figc2_log_glmer_1, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_figc2_log_glmer_1) # plot the model

gg_m_figc2_log_glmer_1$task<-gg_m_figc2_log_glmer_1$group

### plot

m_figc2_final_reorder<-c('phonological','phon_replication') # reorder the levels for plotting

facet_labels<-c(phon_replication = "Phonological Replication", phonological = "Phonological") # Set the label for panels in the plot

dat_dual_speech_n96_onlyphonrep_se_reorder <- dat_dual_speech_n96_onlyphonrep_se %>% 
  mutate(task = fct_relevel(task, m_figc2_final_reorder)) # reorder the raw data per level of task

m_figc2_final_prediction_reorder<-gg_m_figc2_log_glmer_1 %>% 
  mutate(task = fct_relevel(task, m_figc2_final_reorder)) # reorder the prediction per level of task

plot_m_figc2_final<-ggplot(dat_dual_speech_n96_onlyphonrep_se_reorder,aes(x=trial, y=word_percent,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=word_percent-se, ymax=word_percent+se), width=.1) +
  geom_point() +
  geom_line(data=m_figc2_final_prediction_reorder, aes(x=x, y=predicted*100,color=task), size=.6) +
  geom_ribbon(data=m_figc2_final_prediction_reorder, aes(ymin=conf.low*100, ymax=conf.high*100, x=x, y=predicted*100,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(40, 100, by = 10),limits=c(40, 100))+
  labs(x="Trial number", y = "%Correct")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels)) # main figure


plot_m_figc2_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # update the formatting for the plot



# Figure C3: Trial-wise accuracy in secondary tasks (phonological and phonological replication)

## Model (also see the comments for Figure 3 for what the code means below):

### full

m_figc3_log_glmer_full<-glmer(correctness~1+log(trial)*task+(1+log(trial)|participant)+(1+task|prompt),
                              data=dat_dual_speech_n96_onlyphonrep, family = binomial(link = "logit"), 
                              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_figc3_log_glmer_full)


gg_m_figc3_log_glmer_full<-ggpredict(m_figc3_log_glmer_full, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_figc3_log_glmer_full) # plot the model

gg_m_figc3_log_glmer_full$task<-gg_m_figc3_log_glmer_full$group

# Note the main function ggplot() and the geom_line() read different datasets. This allows us to draw the raw data and the fitted values from the model on the same graph
ggplot(dat_dual_n96_onlyphonrep_secondary_acc, aes(x=trial, y=correctness, color=task, shape = task)) + 
  geom_errorbar(aes(ymin=correctness-se, ymax=correctness+se), width=.1) +
  geom_point() +
  geom_line(data=gg_m_figc3_log_glmer_full, aes(x=x, y=predicted, colour = task), size=.6) +
  #geom_ribbon(data=gg_m_figc3_log_glmer_full, aes(ymin=conf.low, ymax=conf.high, x=x, y=predicted, color = task, fill=task), alpha = 0.2) +# error band
  labs(x="Trial number", y = "%Correct")+
  facet_wrap(~ task) +
  theme_bw()

### glmer_1 (best fitting model)


m_figc3_log_glmer_1<-glmer(correctness~1+log(trial)*task+(1+log(trial)|participant),
                           data=dat_dual_speech_n96_onlyphonrep, family = binomial(link = "logit"), 
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_figc3_log_glmer_1)

m_figc3_log_glmer_1_phon<-glmer(correctness~1+log(trial)*relevel(as.factor(task), ref="phonological")+(1+log(trial)|participant),
                                data=dat_dual_speech_n96_onlyphonrep, family = binomial(link = "logit"), 
                                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_figc3_log_glmer_1_phon)


gg_m_figc3_log_glmer_1<-ggpredict(m_figc3_log_glmer_1, terms = c("trial[all]","task")) # save the prediction from the model using ggpredict()
plot(gg_m_figc3_log_glmer_1) # plot the model

gg_m_figc3_log_glmer_1$task<-gg_m_figc3_log_glmer_1$group


### plot

m_figc3_final_reorder<-c('phonological','phon_replication') # reorder the levels for plotting

facet_labels<-c(phon_replication = "Phonological Replication", phonological = "Phonological") # Set the label for panels in the plot

dat_dual_n96_onlyphonrep_secondary_acc_reorder <- dat_dual_n96_onlyphonrep_secondary_acc %>% 
  mutate(task = fct_relevel(task, m_figc3_final_reorder)) # reorder the raw data per level of task

m_figc3_final_prediction_reorder<-gg_m_figc3_log_glmer_1 %>% 
  mutate(task = fct_relevel(task, m_figc3_final_reorder)) # reorder the prediction per level of task

plot_m_figc3_final<-ggplot(dat_dual_n96_onlyphonrep_secondary_acc_reorder,aes(x=trial, y=correctness,color=task, shape=task)) + 
  geom_errorbar(aes(ymin=correctness-se, ymax=correctness+se), width=.1) +
  geom_point() +
  geom_line(data=m_figc3_final_prediction_reorder, aes(x=x, y=predicted,color=task), size=.6) +
  geom_ribbon(data=m_figc3_final_prediction_reorder, aes(ymin=conf.low, ymax=conf.high, x=x, y=predicted,fill=task,color=task), alpha = 0.2) +# error band
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.1),limits=c(0.5, 1))+
  labs(x="Trial number", y = "Accuracy")+
  theme_minimal()+
  facet_wrap(~task,
             labeller = labeller(task = facet_labels)) # main figure


plot_m_figc3_final+
  theme(plot.title = element_text(size = 13,hjust=0.5), 
        axis.text.x = element_text(size=11.5,angle = 0, hjust = 0.5),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = "none") # update the formatting for the plot


# Figure C4: Effort and attention questionnaire (Experiment 1)

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

summary(ques_speechvisual_speecheffort_m_lm_nooutlier) # Model output can be found in Table B3


### attention speech (no outlier)

ques_speechvisual_speechattention_m_lm_nooutlier<-lm(attention.speech ~ relevel(as.factor(task.difficulty), ref="speech_single"), na.action = na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered) 

summary(ques_speechvisual_speechattention_m_lm_nooutlier) # Model output can be found in Table B3


### effort visual (no outlier)

ques_speechvisual_visualeffort_m_lm_nooutlier<-lm(effort.visual ~ relevel(as.factor(task.difficulty), ref="speech_td_48_60"), na.action = na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered)

summary(ques_speechvisual_visualeffort_m_lm_nooutlier) # Model output can be found in Table B3


### attention visual (no outlier)

ques_speechvisual_visualattention_m_lm_nooutlier<-lm(attention.visual ~ relevel(as.factor(task.difficulty), ref="speech_td_48_60"), na.action=na.omit, data=dat_exp1_question_n192_wide_4measures_sdfiltered)

summary(ques_speechvisual_visualattention_m_lm_nooutlier) # Model output can be found in Table B3



# Figure C5: Effort and attention questionnaire (Experiment 2)

## Figure (see also the comments for Figure C4 for what the code means below):

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

summary(ques_speechvisual_speecheffort_m_lm_nophonrep_nooutlier) # Model output can be found in Table B6


#### attention speech (no outlier)

ques_speechvisual_speechattention_m_lm_nophonrep_nooutlier<-lm(attention.speech ~ relevel(as.factor(task.type), ref="speech_single"), na.action = na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechvisual_speechattention_m_lm_nophonrep_nooutlier) # Model output can be found in Table B6


#### effort secondary (no outlier)

ques_speechtsecondary_tsecondaryeffort_m_lm_nophonrep_nooutlier<-lm(effort.tsecondary ~ relevel(as.factor(task.type), ref="visual"), na.action = na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechtsecondary_tsecondaryeffort_m_lm_nophonrep_nooutlier) # Model output can be found in Table B6


#### attention secondary (no outlier)

ques_speechtsecondary_tsecondaryattention_m_lm_nophonrep_nooutlier<-lm(attention.tsecondary ~ relevel(as.factor(task.type), ref="visual"), na.action=na.omit, data=dat_exp2_question_n192_wide_4measures_sdfiltered_nophonrep)

summary(ques_speechtsecondary_tsecondaryattention_m_lm_nophonrep_nooutlier) # Model output can be found in Table B6


# Figure C6: correlations between individual slopes of the speech and secondary task conditions

## data curation

dat_exp1_randslopes<-dat_dual_speech_n192 %>%
  filter(task != "speech_single" & trial <= 20)

dat_exp2_randslopes<-dat_dual_speech_n192_nophonrep %>%
  filter(task != "speech_single" & trial <= 20)

## models

m_exp1_sp_randslopes<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant),
                            data=dat_exp1_randslopes, family = binomial(link = "logit"), 
                            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_exp1_sp_randslopes)

gg_m_exp1_sp_randslopes<-ggpredict(m_exp1_sp_randslopes, terms = c("trial[all]","task"))
plot(gg_m_exp1_sp_randslopes)


m_exp1_2nd_randslopes<-glmer(correctness~1+log(trial)*task+(1+log(trial)|participant),
                             data=dat_exp1_randslopes, family = binomial(link = "logit"), 
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_exp1_2nd_randslopes)

gg_m_exp1_2nd_randslopes<-ggpredict(m_exp1_2nd_randslopes, terms = c("trial[all]","task"))
plot(gg_m_exp1_2nd_randslopes)


m_exp2_sp_randslopes<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*task+(1+log(trial)|participant),
                            data=dat_exp2_randslopes, family = binomial(link = "logit"), 
                            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=20e5)))
summary(m_exp2_sp_randslopes)

gg_m_exp2_sp_randslopes<-ggpredict(m_exp2_sp_randslopes, terms = c("trial[all]","task"))
plot(gg_m_exp2_sp_randslopes)


m_exp2_2nd_randslopes<-glmer(correctness~1+log(trial)*task+(1+log(trial)|participant),
                             data=dat_exp2_randslopes, family = binomial(link = "logit"), 
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
summary(m_exp2_2nd_randslopes)

gg_m_exp2_2nd_randslopes<-ggpredict(m_exp2_2nd_randslopes, terms = c("trial[all]","task"))
plot(gg_m_exp2_2nd_randslopes)


## extract random slopes per condition from the models and store them to a spreadsheet

coefs<-data.frame(matrix(,nrow=48,ncol=12))

exps<-c("exp1","exp2")
tasks<-c("sp","2nd")
conds<-c("speech_td_48_60","speech_td_24_36","speech_td_0_12","visual","phonological","lexical")

i<-1

for (exp in exps) {
  
  # dataset and conditions
  if (exp == "exp1") {
    current_dat<-dat_exp1_randslopes
    conds_lst<-conds[1:3]
  } else {
    current_dat<-dat_exp2_randslopes
    conds_lst<-conds[4:6]
  }
  
  # extract the random slopes
  
  for (cond in conds_lst) {
    
    # fit a model
    
    current_m_sp<-glmer(cbind(count_correct,3-count_correct)~1+log(trial)*relevel(as.factor(task), ref=cond)+(1+log(trial)|participant),
                        data=current_dat, family = binomial(link = "logit"), 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
    current_m_2nd<-glmer(correctness~1+log(trial)*relevel(as.factor(task), ref=cond)+(1+log(trial)|participant),
                         data=current_dat, family = binomial(link = "logit"), 
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=30e5)))
    
    current_m_sp_coef<-coef(current_m_sp)$participant
    current_m_2nd_coef<-coef(current_m_2nd)$participant
    
    dat_currentcond_subj_lst<-current_dat %>%
      filter(task == cond) %>%
      pull(participant) %>%
      as.character() %>%
      unique()
    
    dat_curretcond_subj_idx<-is.element(row.names(current_m_sp_coef),dat_currentcond_subj_lst)
    
    m_sp_coef<-current_m_sp_coef[dat_curretcond_subj_idx,]
    
    names(coefs)[i]<-paste("logtrial",exp,tasks[1],cond,sep = "_")
    coefs[i]<-m_sp_coef$`log(trial)`
    i<-i+1
    
    
    m_2nd_coef<-current_m_2nd_coef[dat_curretcond_subj_idx,]
    
    names(coefs)[i]<-paste("logtrial",exp,tasks[2],cond,sep = "_")
    coefs[i]<-m_2nd_coef$`log(trial)`
    i<-i+1
    
  } 
}

## Plot

spe_vise<-ggplot(data = coefs, aes(x=logtrial_exp1_sp_speech_td_48_60, y=logtrial_exp1_2nd_speech_td_48_60)) + 
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x="Speech task under easy visual task (Exp 1)", y = "Easy visual task (Exp 1)")+
  theme_bw()

spe_vise_formatted<-spe_vise+theme(axis.title.x = element_text(size=14,angle = 0, hjust = 0.5),
                                   axis.title.y = element_text(size=14,angle = 90, hjust = 0.5),
                                   axis.text.x = element_text(size=12,angle = 0, hjust = 0.5),
                                   axis.text.y = element_text(size=12,angle = 0, hjust = 0.5),
                                   panel.grid.major.x = element_line(color = "grey90"),
                                   panel.grid.minor.x = element_line(color = "grey90"))

spi_visi<-ggplot(data = coefs, aes(x=logtrial_exp1_sp_speech_td_24_36, y=logtrial_exp1_2nd_speech_td_24_36)) + 
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x="Speech task under intermediate visual task (Exp 1)", y = "Intermediate visual task (Exp 1)")+
  theme_bw()

spi_visi_formatted<-spi_visi+theme(axis.title.x = element_text(size=14,angle = 0, hjust = 0.5),
                                   axis.title.y = element_text(size=14,angle = 90, hjust = 0.5),
                                   axis.text.x = element_text(size=12,angle = 0, hjust = 0.5),
                                   axis.text.y = element_text(size=12,angle = 0, hjust = 0.5),
                                   panel.grid.major.x = element_line(color = "grey90"),
                                   panel.grid.minor.x = element_line(color = "grey90"))


sph_vish<-ggplot(data = coefs, aes(x=logtrial_exp1_sp_speech_td_0_12, y=logtrial_exp1_2nd_speech_td_0_12)) + 
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x="Speech task under hard visual task (Exp 1)", y = "Hard visual task (Exp 1)")+
  theme_bw()

sph_vish_formatted<-sph_vish+theme(axis.title.x = element_text(size=14,angle = 0, hjust = 0.5),
                                   axis.title.y = element_text(size=14,angle = 90, hjust = 0.5),
                                   axis.text.x = element_text(size=12,angle = 0, hjust = 0.5),
                                   axis.text.y = element_text(size=12,angle = 0, hjust = 0.5),
                                   panel.grid.major.x = element_line(color = "grey90"),
                                   panel.grid.minor.x = element_line(color = "grey90"))


sp_vis<-ggplot(data = coefs, aes(x=logtrial_exp2_sp_visual, y=logtrial_exp2_2nd_visual)) + 
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x="Speech task under visual task (Exp 2)", y = "Visual task (Exp 2)")+
  theme_bw()

sp_vis_formatted<-sp_vis+theme(axis.title.x = element_text(size=14,angle = 0, hjust = 0.5),
                               axis.title.y = element_text(size=14,angle = 90, hjust = 0.5),
                               axis.text.x = element_text(size=12,angle = 0, hjust = 0.5),
                               axis.text.y = element_text(size=12,angle = 0, hjust = 0.5),
                               panel.grid.major.x = element_line(color = "grey90"),
                               panel.grid.minor.x = element_line(color = "grey90"))


sp_phon<-ggplot(data = coefs, aes(x=logtrial_exp2_sp_phonological, y=logtrial_exp2_2nd_phonological)) + 
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x="Speech task under phonological task (Exp 2)", y = "Phonological task (Exp 2)")+
  theme_bw()

sp_phon_formatted<-sp_phon+theme(axis.title.x = element_text(size=14,angle = 0, hjust = 0.5),
                                 axis.title.y = element_text(size=14,angle = 90, hjust = 0.5),
                                 axis.text.x = element_text(size=12,angle = 0, hjust = 0.5),
                                 axis.text.y = element_text(size=12,angle = 0, hjust = 0.5),
                                 panel.grid.major.x = element_line(color = "grey90"),
                                 panel.grid.minor.x = element_line(color = "grey90"))



sp_lex<-ggplot(data = coefs, aes(x=logtrial_exp2_sp_lexical, y=logtrial_exp2_2nd_lexical)) + 
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(x="Speech task under lexical task (Exp 2)", y = "Lexical task (Exp 2)")+
  theme_bw()

sp_lex_formatted<-sp_lex+theme(axis.title.x = element_text(size=14,angle = 0, hjust = 0.5),
                               axis.title.y = element_text(size=14,angle = 90, hjust = 0.5),
                               axis.text.x = element_text(size=12,angle = 0, hjust = 0.5),
                               axis.text.y = element_text(size=12,angle = 0, hjust = 0.5),
                               panel.grid.major.x = element_line(color = "grey90"),
                               panel.grid.minor.x = element_line(color = "grey90"))


task_all <- ggarrange(spe_vise_formatted, spi_visi_formatted,sph_vish_formatted,sp_vis_formatted,sp_phon_formatted,sp_lex_formatted,
                      font.label = list(size = 12, face = "bold", color ="black"),ncol = 3, nrow = 2)


annotate_figure(task_all,
                bottom = text_grob("Beta estimate (Speech task)",
                                   hjust = 0.5, size = 15),
                left = text_grob("Beta estimate (Secondary task)", rot = 90,size = 15)
)
