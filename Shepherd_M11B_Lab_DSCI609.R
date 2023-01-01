library(tidyverse)
library(haven)
library(sjmisc)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(gridExtra)

setwd("/Users/burrisfaculty/Desktop/DSCode/DSCI609/Module 11 Lab")
dta<- read.csv('Labdata.csv')
View(dta)

# ***** DATA WRANGLING ************

#Create dummy variables for race
dta <- dta %>%
  mutate(white = if_else(race == 'white', 1, 0),
         black = if_else(race == 'black', 1, 0),
         hispan = if_else(race =='hispan',1,0))
#Remove original race variable from data frame
dta <- subset(dta, select = -race)
View(dta)
 
# ***** PRELIMINARY DATA ANALYSIS *********

#Use some preliminary data visualization
boxplot(re78 ~ treat, data = dta,
        xlab = 'Treatment',
        ylab = 'Real Earnings in 1978')
#From the boxplot, the medians are about the same. Treatment 1 has more high outliers

#pre-analysis of non-matched data
dta %>%
  group_by(treat) %>%
  summarise(n_subjects = n(),
            mean_earn = mean(re78),
            std_err = sd(re78)/sqrt(n_subjects))
#Take a quick peak at the standard scores
dta %>% 
  mutate(test = (re78 - mean(re78))/sd(re78)) %>% 
  group_by(treat) %>%
  summarise(mean_earn = mean(test))
#Both mean z-scores are close to 0. Treatment 0 is a little above while treatment 1 is a little below.

# ********** T-TESTS **********************
#Test the difference in means
with(dta, t.test(re78 ~ treat))
# p-value = 0.3491, so we would fail to reject the null hypothesis, so there is not a statistically significant
# difference in the mean earnings for the two treatments.

#Test the difference in means: pre-treatment covariates (confounding variables) with treatment (variable)
dta_cov <- c('age', 'educ', 'black', 'hispan', 'married', 'nodegree')
dta %>%
  group_by(treat) %>%
  select(one_of(dta_cov)) %>%
  summarise_all(funs(mean(.,na.rm = T)))
lapply(dta_cov, function(v) {
  t.test(dta[, v] ~ dta[, 'treat'])
})
#After looking through the six t-tests, there is a statistically significant difference (p-value < 0.05) in the means
# for the following covariates: age, black, hispan, married, nodegree

# ********* PROPENSITY SCORES ***************
#Use glm to estimate propensity scores

m_ps <- glm(treat ~ age + educ + black + hispan + married + nodegree,
            family = binomial(), data = dta)
summary(m_ps)

#Use ggplot to plot histograms of the estimated propensity scores by treatment status

prs_df <- data.frame(pr_score = predict(m_ps, type = 'response'),
                     treat = m_ps$model$treat)
#first column is probability (propensity score) and second column is catholic status

labs <- paste("Actual treatment administered", c("Treatment 0", "Treatment 1"))
prs_df %>%
  mutate(treatment = ifelse(treat == 0, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) + 
  geom_histogram(color = 'white') +
  facet_wrap(~treat) +
  xlab("Probability of Treatment 0") + 
  theme_bw()
#Execute a matching algorithm
dta_nomiss <- dta %>%
  select(re78, treat, one_of(dta_cov)) %>%
  na.omit()

mod_match <- matchit(treat ~ age + educ + black + hispan + married + nodegree,
                     method = 'nearest', data = dta_nomiss)
#Plot the mean of each covariate data in the matched data
dta_m <- match.data(mod_match)


fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$treat <- as.factor(dta$treat)
  ggplot(dta, aes(x = distance, y = variable, color = treat)) +
    geom_point(alpha = 0.2, size = 1.5) +
    geom_smooth(method = 'loess', se = F) +
    xlab("Propensity Score") + 
    ylab(variable) +
    theme_bw()
}

grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "educ") + theme(legend.position = 'none'),
  fn_bal(dta_m, 'black'),
  fn_bal(dta_m, "hispan") + theme(legend.position = 'none'),
  fn_bal(dta_m, "married"),
  fn_bal(dta_m, "nodegree") + theme(legend.position = 'none'),
  nrow = 3, widths = c(1, 0.85)
)

#Use t-test to check difference in means based on matched data: real earnings with treatment
dta_m %>%
  group_by(treat) %>%
  summarise_all(funs(mean))
#estimating different treatment effects
with(dta_m, t.test(re78 ~ treat)) #p-value = 0.5063 so there is not a statistically significant
#difference in the means

lapply(dta_cov, function(v){t.test(dta_m[,v]~dta_m[,"treat"])})
#Statistically significant difference for the following confounding variables:
#black, hispan

lm_treat1 <- lm(re78 ~ treat, data = dta_m)
summary(lm_treat1)

lm_treat2 <- lm(re78 ~ treat + age + educ+ black + hispan + married + nodegree, data = dta_m)
summary(lm_treat2)

test_glm <- glm(re78 ~ treat, data = dta_m)
summary(test_glm)
summary(lm_treat1)
