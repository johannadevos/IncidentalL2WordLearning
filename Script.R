### This script contains the data analysis of the following article:
### De Vos, J. F., Schriefers, H., Ten Bosch, L., & Lemhöfer, K. (2018).
### Interactive L2 vocabulary acquisition in a lab-based immersion setting. Manuscript submitted for publication.

# Libraries
library(lme4); library(ggplot2); library(plyr); library(simpleboot); library(Hmisc); library(arm)

# Clear workspace
rm(list = ls())

# Set working directory to the location of this script

# Read in data and inspect
data <- read.table("Data.txt", na.strings = "missing", sep = "\t", header = TRUE)
str(data)

# Use this dataset for a visualisation that includes the pre-test
# Don't do other computations on this dataset
#data <- read.table(file = "DataPre.txt", na.string = "missing", sep = "\t", header = TRUE)

# Recode integer as factor
data$Participant <- as.factor(data$Participant)
data$RetentionInterval <- as.factor(data$RetentionInterval)


## FILTERING 

# Only participants for whom all words were unknown
data2 <- data[data$Participant != "4" & data$Participant != "5" & data$Participant != "10" & data$Participant != "12" & data$Participant != "36" & data$Participant != "50" & data$Participant != "51" & data$Participant != "53" & data$Participant != "55" & data$Participant != "56",]
str(data2)

# Exclude pre-test 5/6 (100 data points)
data3 <- data2[data2$PreTestRaw != "5" & data2$PreTestRaw != "6",]
table(data3$Participant[data3$TestingMoment=="Main2"])
table(table(data3$Participant[data3$TestingMoment=="Main2"]))
(1-(nrow(data3)/nrow(data2)))*100

# Exclude pre-test 3/4 (9 data points)
data4 <- data3[data3$PreTestRaw != "3" & data3$PreTestRaw != "4",]
table(data4$Participant[data4$TestingMoment=="Main2"])
table(table(data4$Participant[data4$TestingMoment=="Main2"]))
(1-(nrow(data4)/nrow(data3)))*100

# Exclude cognates for which the German name is unknown
data5 <- data4[data4$GermanKnown != "No",]
(1-(nrow(data5)/nrow(data4)))*100

# Exclude words from the follow-up test that were heard in the mean time
data6 <- data5[data5$MeanTime != "Yes",]
(1-(nrow(data6)/nrow(data5)))*100

# How many data points are removed from the total data set? (percentage)
(1-(nrow(data6)/nrow(data2)))*100
table(data6$Participant[data6$TestingMoment=="Main2"])
table(table(data6$Participant[data6$TestingMoment=="Main2"]))

data <- data6

# Remove variables that are no longer needed
rm(data2, data3, data4, data5)


## DESCRIPTIVES

# How many participants in each condition and testing moment?
results <- aggregate(Score~Condition+TestingMoment+Participant, data, mean)
count(results, c("Condition", "TestingMoment")) # Requires plyr package

# How many cognates and non-cognates per participant?
count(data[data$TestingMoment!="FollowUp",], c("Participant", "Cognate"))

# Optionally: run the following lines to obtain descriptives for the learning and retention phase separately
learn <- data6[data6$TestingMoment=="Main2" | data6$TestingMoment=="Main4",]; data <- learn
retention <- data6[data6$TestingMoment=="Post" | data6$TestingMoment=="FollowUp",]; data <- retention

# If you want to obtain descriptives for the complete data set, use:
data <- data6


## Average score for all combinations of factor levels

# Create a new column containing all info
data$Info <- paste0(data$Condition, data$Cognate, data$RetentionInterval, data$TestingMoment)

# Aggregate over participants
aggr <- aggregate(Score ~ Participant + Condition + Cognate + RetentionInterval + TestingMoment + Info, data, mean, na.action = NULL) 
aggr <- aggr[order(aggr[,1]),]

# Get descriptives
table(aggr$Info) # n
tapply(aggr$Score*100, aggr$Info, mean)
tapply(aggr$Score*100, aggr$Info, sd)
tapply(aggr$Score*100, aggr$Info, shapiro.test)
tapply(aggr$Score*100, aggr$Info, t.test) # 95% confidence intervals

# Alternatively
aggregate(aggr$Score*100, list(aggr$Info), mean)

## Average score for each factor separately (split by Condition)

# Cognate status
cogn <- aggregate(Score ~ Participant + Condition + Cognate, aggr, mean, na.action = NULL)
cogn$Info <- paste0(cogn$Condition, cogn$Cognate)

table(cogn$Info) # n
tapply(cogn$Score*100, cogn$Info, mean)
tapply(cogn$Score*100, cogn$Info, sd)
tapply(cogn$Score*100, cogn$Info, shapiro.test)
tapply(cogn$Score*100, cogn$Info, t.test) # 95% confidence intervals

# Testing moment
test <- aggregate(Score ~ Participant + Condition + TestingMoment, aggr, mean, na.action = NULL)
test$Info <- paste0(test$Condition, test$TestingMoment)

table(test$Info) # n
tapply(test$Score*100, test$Info, mean)
tapply(test$Score*100, test$Info, sd)
tapply(test$Score*100, test$Info, shapiro.test)
tapply(test$Score*100, test$Info, t.test) # 95% confidence intervals

# Retention interval
ri <- aggregate(Score ~ Participant + Condition + RetentionInterval, aggr, mean, na.action = NULL)
ri$Info <- paste0(ri$Condition, ri$RetentionInterval)

table(ri$Info) # n
tapply(ri$Score*100, ri$Info, mean)
tapply(ri$Score*100, ri$Info, sd)
tapply(ri$Score*100, ri$Info, shapiro.test)
tapply(ri$Score*100, ri$Info, t.test) # 95% confidence intervals

# Condition
con <- aggregate(Score ~ Participant + Condition, aggr[aggr$TestingMoment != "FollowUp",], mean, na.action = NULL)

table(con$Condition) # n
tapply(con$Score*100, con$Condition, mean)
tapply(con$Score*100, con$Condition, sd)
tapply(con$Score*100, con$Condition, shapiro.test)
tapply(con$Score*100, con$Condition, t.test) # 95% confidence intervals

## Which words have been answered correctly, partially correctly, or incorrectly?

# Create new column with this information
data$Correct <- data$Score
data$Correct[data$Correct==1] <- "Correct"
data$Correct[data$Correct==0] <- "Incorrect"
data$Correct[data$Correct > 0 & data$Correct < 1] <- "Partial"

# How are the three response types distributed over the conditions? Two approaches
count(data, c('Condition', 'TestingMoment', 'Cognate', 'Correct'))
count(data, c('Condition', 'Cognate', 'Correct'))

x <- xtabs(~ Correct + Cognate + Condition, data[data$TestingMoment!="FollowUp",]); x
#prop.table(x)

x[1]/sum(x[1:3])*100 # Manually calculate percentages within each factor level

# Go back to the complete data set
data <- data6


## DATA VISUALISATION

pd <- position_dodge(width = 0.1)
data$TestingMoment <- factor(data$TestingMoment, levels = c("Pre", "Main2", "Main4", "Post", "FollowUp"))
aggr$TestingMoment <- factor(aggr$TestingMoment, levels = c("Pre", "Main2", "Main4", "Post", "FollowUp"))
data$Condition <- factor(data$Condition, levels = c("Experimental", "Control"))

# Both groups
  ggplot(aggr, aes(x = TestingMoment, y = Score*100, colour = Cognate, shape = Condition, group = interaction(Cognate,RetentionInterval,Condition))) +
  stat_summary(fun.y = mean, geom = "point", size = 5, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", aes(linetype = RetentionInterval), size = 1, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = 0.15, position = pd) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(text = element_text(size = 23), axis.text.y = element_text(size = 21), axis.text.x = element_text(size = 21), strip.text = element_text(size=23)) +
  labs(x = "\nTesting moment", y = "Score (% correct)\n") +
  scale_linetype_discrete(name = "Lag", labels = c("3 trials", "7 trials")) +
  #scale_x_discrete(labels = c("Pre", "EF2", "EF4", "20min", "6mon")) + # Uncomment this line for the figure in the publication
  #scale_shape_manual(values=c(18, 16), name = "Condition") http://www.sthda.com/english/wiki/ggplot2-point-shapes#change-point-shapes-colors-and-sizes-manually
  scale_shape_discrete(name = "Condition") + 
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
  scale_color_manual(name = "Cognate status", values=c("#000000", "#56B4E9")) # Use this to get colours that are contrastive when printing in grayscale (add a + in the row above)

#ggsave("Figure 1.tiff", dpi = 300)


## Plot data as a function of trial nr. 

pd <- position_dodge(width = 1)
data$TrialNumberRaw <- as.factor(data$TrialNumberRaw)
aggr2 <- aggregate(Score ~ Participant + Condition + Cognate + RetentionInterval + TestingMoment + TrialNumberRaw + Info, data, mean, na.action = NULL) # na.action --> participants with missing values for AttentionForWords are not removed from the datafile
aggr2$TrialNumberRaw <- as.integer(aggr2$TrialNumberRaw)

# Experimental
graph_exp <- ggplot(aggr2[aggr2$Condition=="Experimental" & aggr2$TestingMoment != "Post" & aggr2$TestingMoment != "FollowUp",], aes(x = TrialNumberRaw, y = Score*100, colour = Cognate, shape = TestingMoment, group = interaction(Cognate,RetentionInterval,TestingMoment))) +
  stat_summary(fun.y = mean, geom = "point", size = 5, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", aes(linetype = RetentionInterval), size = 1, position = pd) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = 0.15, position = pd) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(text = element_text(size = 23), axis.text.y = element_text(size = 23), axis.text.x = element_text(size = 23), strip.text = element_text(size=23)) +
  labs(x = "\nTrial number", y = "Score (% correct)\n") +
  scale_linetype_discrete(name = "Retention interval") +
  scale_colour_discrete(name = "Cognate status") +
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
  ggtitle("Experimental condition\n") +
  scale_color_manual(values=c("#000000", "#56B4E9"))

# Control
graph_contr <- ggplot(aggr2[aggr2$Condition=="Control" & aggr2$TestingMoment != "Post" & aggr2$TestingMoment != "FollowUp",], aes(x = TrialNumberRaw, y = Score*100, colour = Cognate, shape = TestingMoment, group = interaction(Cognate,RetentionInterval,TestingMoment))) +
  stat_summary(fun.y = mean, geom = "point", size = 5, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", aes(linetype = RetentionInterval), size = 1, position = pd) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = 0.15, position = pd) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(text = element_text(size = 23), axis.text.y = element_text(size = 23), axis.text.x = element_text(size = 23), strip.text = element_text(size=23)) +
  labs(x = "\nTrial number", y = "Score (% correct)\n") +
  scale_linetype_discrete(name = "Retention interval") +
  scale_colour_discrete(name = "Cognate status") +
  guides(color = guide_legend(order = 2), shape = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
  ggtitle("Control condition\n") +
  scale_color_manual(values=c("#000000", "#56B4E9"))

# To plot the two graphs in one picture
library(gridExtra)
grid.arrange(graph_contr, graph_exp, nrow=1, ncol=2)


## PARTICIPANT ANALYSIS / COMPARE PARTICIPANTS ACROSS CONDITIONS

# Aggregregate and subset Main2
# (45 participants total)
part <- aggregate(Score~Participant+TestingMoment+Condition+Age+YearsOfDutch+Proficiency+Exposure+HowManyOtherLanguages+Lextale, data, mean, na.action=NULL)
part2 <- part[part$TestingMoment=="Main2",]
sort(part2$Participant)
part <- part2

# Make a different aggregation for Memory, because R discards entries with missing values
# (44 participants total)
part_mem <- aggregate(Score~Participant+TestingMoment+Condition+Age+YearsOfDutch+Proficiency+Exposure+HowManyOtherLanguages+Lextale+Memory, data, mean, na.action=NULL)
part_mem2 <- part_mem[part_mem$TestingMoment=="Main2",]
sort(part_mem2$Participant)
part_mem <- part_mem2

# Means
tapply(part$Age, part$Condition, mean); tapply(part$Age, part$Condition, sd)
tapply(part$YearsOfDutch, part$Condition, mean); tapply(part$YearsOfDutch, part$Condition, sd)
tapply(part$Proficiency, part$Condition, mean); tapply(part$Proficiency, part$Condition, sd)
tapply(part$Exposure, part$Condition, mean); tapply(part$Exposure, part$Condition, sd)
tapply(part$HowManyOtherLanguages, part$Condition, mean); tapply(part$HowManyOtherLanguages, part$Condition, sd)
tapply(part$Lextale, part$Condition, mean); tapply(part$Lextale, part$Condition, sd)
tapply(part_mem$Memory, part_mem$Condition, mean); tapply(part_mem$Memory, part_mem$Condition, sd)

# Normally distributed?
shapiro.test(part$Age[part$Condition=="Experimental"]); shapiro.test(part$Age[part$Condition=="Control"])
shapiro.test(part$YearsOfDutch[part$Condition=="Experimental"]); shapiro.test(part$YearsOfDutch[part$Condition=="Control"])
shapiro.test(part$Proficiency[part$Condition=="Experimental"]); shapiro.test(part$Proficiency[part$Condition=="Control"])
shapiro.test(part$Exposure[part$Condition=="Experimental"]); shapiro.test(part$Exposure[part$Condition=="Control"])
shapiro.test(part$HowManyOtherLanguages[part$Condition=="Experimental"]); shapiro.test(part$HowManyOtherLanguages[part$Condition=="Control"])
shapiro.test(part$Lextale[part$Condition=="Experimental"]); shapiro.test(part$Lextale[part$Condition=="Control"])
shapiro.test(part_mem$Memory[part_mem$Condition=="Experimental"]); shapiro.test(part_mem$Memory[part_mem$Condition=="Control"])
shapiro.test(part$Score[part$Condition=="Experimental"]); shapiro.test(part$Score[part$Condition=="Control"])

# Compare means
wilcox.test(part$Age[part$Condition=="Experimental"], part$Age[part$Condition=="Control"])
wilcox.test(part$YearsOfDutch[part$Condition=="Experimental"], part$YearsOfDutch[part$Condition=="Control"])
wilcox.test(part$Proficiency[part$Condition=="Experimental"], part$Proficiency[part$Condition=="Control"])
wilcox.test(part$Exposure[part$Condition=="Experimental"], part$Exposure[part$Condition=="Control"])
wilcox.test(part$HowManyOtherLanguages[part$Condition=="Experimental"], part$HowManyOtherLanguages[part$Condition=="Control"])
t.test(part$Lextale[part$Condition=="Experimental"], part$Lextale[part$Condition=="Control"])
t.test(part_mem$Memory[part_mem$Condition=="Experimental"], part_mem$Memory[part_mem$Condition=="Control"])

# Histogram of participant scores at Main2
ggplot(data=part, aes(part$Score*100, fill = Condition)) + 
  geom_histogram(alpha=1, breaks=seq(0,100, by=10), col="white") +
  labs(x = "\nScore at Main2", y = "Counts\n") +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 15), 
        axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15), 
        strip.text = element_text(size=15)) +
  scale_x_continuous(breaks=seq(0, 100, by = 10)) +
  scale_y_continuous(breaks=seq(0,14, by=1)) +
  scale_fill_manual(values=c("#000000", "#56B4E9"))


## CORRELATION MATRIX

rcorr(as.matrix(part[4:10][part$Condition=="Experimental",]), type = "spearman")
rcorr(as.matrix(part_mem[4:11][part$Condition=="Experimental",]), type = "spearman") # For correlations with Memory only
rcorr(as.matrix(part_mem[4:11][part$Condition=="Experimental",]), type = "pearson") # For correlations with Memory only --> Score and Lextale


## ITEM ANALYSIS
items <- data[data$TestingMoment=="Main2" | data$TestingMoment=="Main4",]

# Calculate average score per item
aggr_words <- aggregate(Score~Word+Cognate+TestingMoment, items[items$Condition=="Experimental",], mean)
count_words <- plyr::count(items[items$Condition=="Experimental" & items$TestingMoment=="Main2",], "Word")
data_words <- merge(aggr_words, count_words, by="Word")

# Replace zero scores with 0.005 so that a tiny bar gets plotted
data_words$Score <- mapvalues(data_words$Score, from=0, to=0.005) # Requires plyr

# Bar plot with words on y-axis and score on x-axis
library(dplyr); library(tidyr)

# Define orders that can be used for sorting the bars
Main2_order <- data_words %>%
  filter(TestingMoment == 'Main2') %>%
  arrange(Score,Word) %>%
  pull(Word) %>%
  as.character()

Main4_order <- data_words %>%
  filter(TestingMoment == 'Main4') %>%
  arrange(Score,Word) %>%
  pull(Word) %>%
  as.character()

Main2_Main4_order <- data_words %>% 
  select(Word,TestingMoment,Score) %>%
  spread(key = TestingMoment,value = Score) %>%
  arrange(Main4,Main2,Word) %>%
  pull(Word) %>%
  as.character()

# Plot
data_words %>% 
  group_by(Word, Cognate) %>% 
  arrange(desc(Score)) %>%
  ggplot(data = ., 
         aes(x = factor(Word,levels = rev(Main2_Main4_order)), 
             y = Score * 100, 
             fill = Cognate)) + 
  geom_bar(aes(group = TestingMoment, 
               colour = TestingMoment), 
           stat = "identity", 
           position = "identity", 
           alpha = 0.5) +
  geom_text(aes(label = freq), 
            size = 3, 
            position = position_nudge(x=0.1, y=3), 
            data = filter(data_words, TestingMoment == "Main4")) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10),
        text = element_text(size = 15)) +
  labs(x = "Word\n", y = "\nAverage score at Main2 and Main4") +
  scale_fill_manual(values=c("#000000", "#56B4E9")) +
  scale_colour_manual(values=c("white", "white"), guide=FALSE) +
  coord_flip()

# Analysis: how can the results be explained?
aggr_words2 <- aggregate(Score~Word+Cognate+Phonemes+Concreteness+L1Frequency, items[items$Condition=="Experimental",], mean)
compounds <- c("broodrooster", "tulband", "puntenslijper", "kapstok", "toverstaf", "wafelijzer", "reddingsvest", "kruiwagen", "sambabal", "zwemvleugel", "citruspers", "dienblad", "heggenschaar", "onderzetter", "stofzuiger", "stokpaard")
aggr_words2$Compound <- c("No", "Yes")[(aggr_words2$Word %in% compounds)+1]
aggr_words2$Compound <- as.factor(aggr_words2$Compound)

item_model <- lm(Score ~ Cognate + Compound + Phonemes + Concreteness + L1Frequency, data = aggr_words2)
summary(item_model)

library(QuantPsyc)
lm.beta(item_model)


### MODELLING

# Define function to calculate probability from logit
logit2per = function(X){return(exp(X)/(1+exp(X)))}

# Change factor levels for TestingMoment
data$TestingMoment <- factor(data$TestingMoment, levels = c("Main2", "Main4", "Post", "FollowUp"))
data$TestingMoment <- factor(data$TestingMoment, levels = c("Main4", "Main2", "Post", "FollowUp"))
data$TestingMoment <- factor(data$TestingMoment, levels = c("FollowUp", "Main2", "Main4", "Post"))
data$TestingMoment <- factor(data$TestingMoment, levels = c("Post", "Main2", "Main4", "FollowUp"))
data$Condition <- factor(data$Condition, levels = c("Experimental", "Control"))
data$Condition <- factor(data$Condition, levels = c("Control", "Experimental"))


## MODEL SELECTION

## Explore different random-effects (RE) structures following Bates et al. (2015)
## First, find the number of dimensions in the RE structure that are supported by the data

# Define rePCA function (Bates, 2015, retrieved from https://github.com/dmbates/RePsychLing/blob/master/R/rePCA.R)
rePCA <- function(x) UseMethod('rePCA')

rePCA.merMod <- function(x) {
  chfs <- getME(x,"Tlist") # list of lower Cholesky factors
  nms <- names(chfs)
  unms <- unique(nms)
  names(unms) <- unms
  svals <- function(m) {
    vv <- svd(m,nv=0L)
    names(vv) <- c("sdev","rotation")
    vv$center <- FALSE
    vv$scale <- FALSE
    class(vv) <- "prcomp"
    vv
  }
  structure(lapply(unms,function(m) svals(bdiag(chfs[which(nms == m)]))),
            class="prcomplist")
}

summary.prcomplist <- function(object,...) {
  lapply(object,summary)
}

# Model A
model_a <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                   1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                   (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                   (1+Condition+TestingMoment+RetentionInterval|Word), 
                 data = data, family = 'binomial', control = glmerControl(
                   optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(model_a, correlation = "false")

# Run PCA on model A
rePCA(model_a) 
summary(rePCA(model_a)) # All dimensions are supported by the data
# Six levels for Participant: Intercept + (Levels of TM-1) + (Levels of CS-1) + (Levels of RI-1) = 1 + 3 + 1 + 1
# Six levels for Word: Intercept + (Levels of TM-1) + (Levels of Condition-1) + (Levels of RI-1) = 1 + 3 + 1 + 1

# Model B
model_b <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                   1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                   (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                   (1+Condition+TestingMoment+RetentionInterval+Condition:TestingMoment|Word), 
                 data = data, family = 'binomial', control = glmerControl(
                   optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(model_b, correlation = "false")

# Run PCA on model B
rePCA(model_b)
summary(rePCA(model_b))
# Nine levels for Word: Intercept + (Levels of TM-1) + (Levels of Condition-1) + (Levels of RI-1) + ((Levels of TM-1) * (Levels of Condition-1)) = 1 + 3 + 1 + 1 + 3

# Model C
model_c <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                   1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                   (1+Cognate+TestingMoment+RetentionInterval+Cognate:RetentionInterval|Participant) + 
                   (1+Condition+TestingMoment+RetentionInterval|Word), 
                 data = data, family = 'binomial', control = glmerControl(
                   optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(model_c, correlation = "false")

# Run PCA on model C
rePCA(model_c)
summary(rePCA(model_c))

# Model D
model_d <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                   1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                   (1+Cognate+TestingMoment+RetentionInterval+Cognate:RetentionInterval|Participant) + 
                   (1+Condition+TestingMoment+RetentionInterval+TestingMoment:RetentionInterval|Word), 
                 data = data, family = 'binomial', control = glmerControl(
                   optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(model_d, correlation = "false")

# Run PCA on model D
rePCA(model_d)
summary(rePCA(model_d)) # All dimensions are supported.

## Then, investigate the contribution of the included variance components

# Test the added value of the random effect of Testingmoment:RetentionInterval over Word
anova(model_c, model_d) 

# Test the added value of the random effect of CognateStatus:RetentionInterval over Participant
model_d_2 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                     1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                     (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                     (1+Condition+TestingMoment+RetentionInterval+TestingMoment:RetentionInterval|Word), 
                   data = data, family = 'binomial', control = glmerControl(
                     optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
anova(model_d, model_d_2)

## The final model: Hypothesis-based (HB) model (equal to model_d)
model_hb <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                    1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                    (1+Cognate+TestingMoment+RetentionInterval+Cognate:RetentionInterval|Participant) + 
                    (1+Condition+TestingMoment+RetentionInterval+TestingMoment:RetentionInterval|Word), 
                  data = data, family = 'binomial', control = glmerControl(
                    optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(model_hb, correlation = "true")
# None of variance components is close to or at zero
# None of correlations between random effects is close to 1 (see Bates et al., 2015)
# Thus, model is not overparameterised

## MODEL FIT

# Inspect residuals
binnedplot(fitted(model_hb), resid(model_hb, type = "response"), cex.pts=1, col.int="black", xlab = "Estimated score (as probability)")
# fitted(model_hb) is identical to logit2per(predict(model_hb))
# Thus, 'fitted' gives probabilities, while 'predict' gives logit values

# Compare to model with no interactions in RE structure
model_hb2 <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + (1+Cognate+TestingMoment+RetentionInterval|Participant) + (1+Condition+TestingMoment+RetentionInterval|Word), data = data, family = 'binomial', control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(model_hb2)
binnedplot(fitted(model_hb2), resid(model_hb2, type = "response"), cex.pts=1, col.int="black", xlab = "Estimated score (as probability)")

# Save workspace
save.image(file="Workspace.RData")


## SEPARATE ANALYSES FOR LEARNING AND RETENTION

## Learning phase

# Subset data
data_main <- data[data$TestingMoment=="Main2" | data$TestingMoment=="Main4",]

# Models without Memory

# Model without random slopes
model_main <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                   1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval +
                   (1|Participant) + 
                   (1|Word), 
                 data = data_main, family = 'binomial', control = glmerControl(
                   optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main)

summary(rePCA(model_main))

# Adding random slope of Condition over Word
model_main_cond_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                             1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                             (1|Participant) + 
                             (1+Condition|Word), 
                           data = data_main, family = 'binomial', control = glmerControl(
                             optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cond_w)

anova(model_main, model_main_cond_w) # Significant improvement
summary(rePCA(model_main_cond_w))

# Adding random slope of Cognate over Participant
model_main_cogn_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                        1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval +
                        (1+Cognate|Participant) + 
                        (1+Condition|Word), 
                      data = data_main, family = 'binomial', control = glmerControl(
                        optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cogn_p)

anova(model_main_cond_w, model_main_cogn_p) # Significant improvement
summary(rePCA(model_main_cogn_p))

# Adding random slope of TestingMoment over Word
model_main_tm_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                           (1+Cognate|Participant) + 
                           (1+Condition+TestingMoment|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_tm_w)

anova(model_main_cogn_p, model_main_tm_w) # Significant improvement
summary(rePCA(model_main_tm_w))

# Adding random slope of TestingMoment over Participant
model_main_tm_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                             1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                             (1+Cognate+TestingMoment|Participant) + 
                             (1+Condition+TestingMoment|Word), 
                           data = data_main, family = 'binomial', control = glmerControl(
                             optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_tm_p)

anova(model_main_tm_w, model_main_tm_p) # Significant improvement
summary(rePCA(model_main_tm_p))

# Adding random slope of RetentionInterval over Word
model_main_ri_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                           (1+Cognate+TestingMoment|Participant) + 
                           (1+Condition+TestingMoment+RetentionInterval|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_ri_w)

anova(model_main_tm_p, model_main_ri_w) # Significant improvement

# Adding random slope of RetentionInterval over Participant
model_main_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                           (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                           (1+Condition+TestingMoment+RetentionInterval|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_ri_p)

anova(model_main_ri_w, model_main_ri_p) # Significant improvement
summary(rePCA(model_main_ri_p))

# Adding random slope of the interaction of Condition and TestingMoment over Word
model_main_cond_tm_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                                (1+Condition+TestingMoment+RetentionInterval+Condition:TestingMoment|Word), 
                              data = data_main, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cond_tm_w)

anova(model_main_ri_p, model_main_cond_tm_w) # Not significant
summary(rePCA(model_main_cond_tm_w))

# Adding random slope of the interaction of Cognate and TestingMoment over Participant
model_main_cogn_tm_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment|Participant) + 
                                (1+Condition+TestingMoment+RetentionInterval|Word), 
                              data = data_main, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cogn_tm_p)

anova(model_main_ri_p, model_main_cogn_tm_p) # Significant improvement
summary(rePCA(model_main_cogn_tm_p))

# Adding random slope of the interaction of Condition and RetentionInterval over Word
model_main_cond_ri_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment|Participant) + 
                                (1+Condition+TestingMoment+RetentionInterval+Condition:RetentionInterval|Word), 
                              data = data_main, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cond_ri_w)

anova(model_main_cogn_tm_p, model_main_cond_ri_w) # Significant improvement
summary(rePCA(model_main_cond_ri_w))

# Adding random slope of the interaction of Cognate and RetentionInterval over Participant
model_main_cogn_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment+Cognate:RetentionInterval|Participant) + 
                                (1+Condition+TestingMoment+RetentionInterval+Condition:RetentionInterval|Word), 
                              data = data_main, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cogn_ri_p)

anova(model_main_cond_ri_w, model_main_cogn_ri_p) # Significant improvement
summary(rePCA(model_main_cogn_ri_p))

# Adding random slope of the interaction of TestingMoment and RetentionInterval over Word
model_main_tm_ri_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment+Cognate:RetentionInterval|Participant) + 
                                (1+Condition+TestingMoment+RetentionInterval+Condition:RetentionInterval+TestingMoment:RetentionInterval|Word), 
                              data = data_main, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_tm_ri_w)

anova(model_main_cogn_ri_p, model_main_tm_ri_w) # Significant improvement
summary(rePCA(model_main_tm_ri_w))

# Adding random slope of the interaction of TestingMoment and RetentionInterval over Participant
model_main_tm_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                              1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                              (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment+Cognate:RetentionInterval+TestingMoment:RetentionInterval|Participant) + 
                              (1+Condition+TestingMoment+RetentionInterval+Condition:RetentionInterval+TestingMoment:RetentionInterval|Word), 
                            data = data_main, family = 'binomial', control = glmerControl(
                              optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_tm_ri_p)

anova(model_main_tm_ri_w, model_main_tm_ri_p) # Significant improvement
summary(rePCA(model_main_tm_ri_p))

# Adding random slope of the three-way interaction of Condition, TestingMoment and RetentionInterval over Participant
model_main_cond_tm_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                              1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                              (1+Cognate*TestingMoment*RetentionInterval|Participant) + 
                              (1+Condition+TestingMoment+RetentionInterval+Condition:RetentionInterval+TestingMoment:RetentionInterval|Word), 
                            data = data_main, family = 'binomial', control = glmerControl(
                              optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cond_tm_ri_p)

anova(model_main_tm_ri_p, model_main_cond_tm_ri_p) # Significant improvement
summary(rePCA(model_main_cond_tm_ri_p))


# Models with memory

# Model without random slopes
model_main_mem <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                      1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                      (1|Participant) + 
                      (1|Word), 
                    data = data_main, family = 'binomial', control = glmerControl(
                      optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_mem)

# Adding random slope of Condition over Word
# Failed to converge
model_main_mem_cond_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                             1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                             (1|Participant) + 
                             (1+Condition|Word), 
                           data = data_main, family = 'binomial', control = glmerControl(
                             optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_cond_w_mem)

# Adding random slope of Cognate over Participant
model_main_mem_cogn_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                             1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                             (1+Cognate|Participant) + 
                             (1|Word), 
                           data = data_main, family = 'binomial', control = glmerControl(
                             optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_mem_cogn_p)

anova(model_main_mem, model_main_mem_cogn_p) # Significant improvement

# Adding random slope of TestingMoment over Word
model_main_mem_tm_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                           (1+Cognate|Participant) + 
                           (1+TestingMoment|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_mem_tm_w)

anova(model_main_cogn_p, model_main_tm_w) # Significant improvement

# Adding random slope of TestingMoment over Participant
model_main_mem_tm_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                           (1+Cognate+TestingMoment|Participant) + 
                           (1+TestingMoment|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_mem_tm_p)

anova(model_main_mem_tm_w, model_main_mem_tm_p) # Significant improvement

# Adding random slope of RetentionInterval over Word
model_main_ri_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                           (1+Cognate+TestingMoment|Participant) + 
                           (1+Condition+TestingMoment+RetentionInterval|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_ri_w)

anova(model_main_tm_p, model_main_ri_w) # Significant improvement

# Adding random slope of RetentionInterval over Participant
model_main_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + 
                           (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                           (1+Condition+TestingMoment+RetentionInterval|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_ri_p)

anova(model_main_ri_w, model_main_ri_p) # Significant improvement




# Removing random slope of RetentionInterval over Word, adding fixed effect of Memory
model_main_mem <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                           (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                           (1+Condition+TestingMoment|Word), 
                         data = data_main, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_mem)

# Model failed to converge, therefore remove some random effects (TM over P)
model_main_mem <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                          1 + Condition*TestingMoment + Condition*Cognate + Condition*RetentionInterval + Memory +
                          (1+Cognate+RetentionInterval|Participant) + 
                          (1+Condition+TestingMoment|Word), 
                        data = data_main, family = 'binomial', control = glmerControl(
                          optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_main_mem)

anova(model_main_ri_p, model_main_mem) # Significant improvement


## Long-term retention

# Subset data
data_post <- data[data$TestingMoment!="Main2" & data$Condition=="Experimental",]

# Relevel
data$TestingMoment <- factor(data$TestingMoment, levels = c("Main4", "Post", "FollowUp"))

# No random slopes
model_post <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                      1 + TestingMoment + Cognate + RetentionInterval + 
                      (1|Participant) + 
                      (1|Word), 
                    data = data_post, family = 'binomial', control = glmerControl(
                      optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post)

summary(rePCA(model_post))

# Adding random slope of Cognate over Participant
model_post_cogn_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                      1 + Cognate + TestingMoment + RetentionInterval + 
                      (1+Cognate|Participant) + 
                      (1|Word), 
                    data = data_post, family = 'binomial', control = glmerControl(
                      optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_cogn_p)

anova(model_post, model_post_cogn_p)
summary(rePCA(model_post_cogn_p))

# Adding random slope of TestingMoment over Word
model_post_tm_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                      1 + Cognate + TestingMoment + RetentionInterval + 
                      (1+Cognate|Participant) + 
                      (1+TestingMoment|Word), 
                    data = data_post, family = 'binomial', control = glmerControl(
                      optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_tm_w)

anova(model_post_cogn_p, model_post_tm_w)
summary(rePCA(model_post_tm_w))

# Adding random slope of TestingMoment over Participant
model_post_tm_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                   1 + Cognate + TestingMoment + RetentionInterval + 
                   (1+Cognate+TestingMoment|Participant) + 
                   (1+TestingMoment|Word), 
                 data = data_post, family = 'binomial', control = glmerControl(
                   optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_tm_p)

anova(model_post_tm_w, model_post_tm_p)
summary(rePCA(model_post_tm_p))

# Adding random slope of RetentionInterval over Word
model_post_ri_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Cognate + TestingMoment + RetentionInterval + 
                           (1+Cognate+TestingMoment|Participant) + 
                           (1+TestingMoment+RetentionInterval|Word), 
                         data = data_post, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_ri_w)

anova(model_post_tm_p, model_post_ri_w)
summary(rePCA(model_post_ri_w))

# Adding random slope of RetentionInterval over Participant
model_post_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Cognate + TestingMoment + RetentionInterval + 
                           (1+Cognate+TestingMoment+RetentionInterval|Participant) + 
                           (1+TestingMoment+RetentionInterval|Word), 
                         data = data_post, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_ri_p)

anova(model_post_ri_w, model_post_ri_p)
summary(rePCA(model_post_ri_p))

# Adding random slope of Cognate:TestingMoment over Participant
model_post_cogn_tm_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                           1 + Cognate + TestingMoment + RetentionInterval + 
                           (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment|Participant) + 
                           (1+TestingMoment+RetentionInterval|Word), 
                         data = data_post, family = 'binomial', control = glmerControl(
                           optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_cogn_tm_p)

anova(model_post_ri_p, model_post_cogn_tm_p)
summary(rePCA(model_post_cogn_tm_p))

# Adding random slope of Cognate:RetentionInterval over Participant
model_post_cogn_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Cognate + TestingMoment + RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment+Cognate:RetentionInterval|Participant) + 
                                (1+TestingMoment+RetentionInterval|Word), 
                              data = data_post, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_cogn_ri_p)

anova(model_post_cogn_tm_p, model_post_cogn_ri_p)
summary(rePCA(model_post_cogn_ri_p)) # Not supported by the data

# Adding random slope of TestingMoment:RetentionInterval over Word
model_post_tm_ri_w <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                                1 + Cognate + TestingMoment + RetentionInterval + 
                                (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment|Participant) + 
                                (1+TestingMoment+RetentionInterval+TestingMoment:RetentionInterval|Word), 
                              data = data_post, family = 'binomial', control = glmerControl(
                                optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_tm_ri_w)

anova(model_post_cogn_tm_p, model_post_tm_ri_w)
summary(rePCA(model_post_tm_ri_w))
# This is the final retention model

# Adding random slope of TestingMoment:RetentionInterval over Participant
model_post_tm_ri_p <- glmer(cbind(PhonemesCorrectRelative,PhonemesIncorrectRelative) ~ 
                              1 + Cognate + TestingMoment + RetentionInterval + 
                              (1+Cognate+TestingMoment+RetentionInterval+Cognate:TestingMoment+TestingMoment:RetentionInterval|Participant) + 
                              (1+TestingMoment+RetentionInterval+TestingMoment:RetentionInterval|Word), 
                            data = data_post, family = 'binomial', control = glmerControl(
                              optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(model_post_tm_ri_p)

# Failed to converge