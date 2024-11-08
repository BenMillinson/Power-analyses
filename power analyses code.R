#this code is used to gain an estimation of sample size based from intended statistics

library(tidyverse)
library(pwr)
library(WebPower)

#power analysis/sample calculation----
#power is usually 80% (0.8) (arbitary?)
#sig.level = 0.05


# one mean T-Test ---------------------------------------------------------
#d = effect size
pwr.t.test(d = , sig.level = , power = , type ="one.sample", alternative="two.sided")


# two means T-Test --------------------------------------------------------
#d = effect size
pwr.t.test(d = , sig.level = , power = , type = "two.sample", alternative="greater")


# Paired T-Test -----------------------------------------------------------
#d = effect size
pwr.t.test(d = , sig.level = , power = , type ="paired", alternative="greater")


# one way ANOVA -----------------------------------------------------------
#k = number of groups
#f = effect size
pwr.anova.test(k= , f= , sig.level=0.05, power =0.80)


# Single Proportion Test --------------------------------------------------
#h = effect size
pwr.p.test(h=, sig.level=0.05, power=0.80, alternative="two.sided")


# Two Proportions Test ----------------------------------------------------
#h = effect size
pwr.2p.test(h=, sig.level=0.05, power=.80, alternative="two.sided")


# Chi Squared Test --------------------------------------------------------
#w = effect size
#df = degress of freedom
pwr.chisq.test(w= , df=, sig.level=0.05, power=0.80)


# simple linear regression ------------------------------------------------
# u = numerator degrees of freedom
# f2 = denominator degrees of freedom
pwr.f2.test(u=, f2=, sig.level=0.05, power=0.80)


# multiple linear regression ----------------------------------------------
# u = numerator degrees of freedom
# f2 = denominator degrees of freedom
# output v = n of participants
pwr.f2.test(u=1, f2=1, sig.level=0.05, power=0.80)


# correlation -------------------------------------------------------------
#r = correlation
pwr.r.test(r=, sig.level=0.05, power=0.80)


# repeated measures ANOVA -------------------------------------------------
#ng = number of groups
#nm = number of measures (dependent variables)
#f = effect size
#nscor = nonsphericity correction coefficient
#alpha = significance level
#type=(0,1,2), 0 = between effect; 1 = within-effect; 2 = interaction effect
wp.rmanova(n=NULL, ng=1, nm=, f=, nscor=1, + alpha=0.05, power=0.80, type=1)


# multi-way ANOVA (1 category of interest) --------------------------------
#ndf = numerator degrees of freedom
#f = effect size
#ng = number of groups
wp.kanova(ndf=,f=, ng=, alpha=0.05, power=0.80)


# multi-way ANOVA (>1 category of interest) -------------------------------
#ndf = numerator degrees of freedom
#f = effect size
#ng = number of groups
wp.kanova(ndf=, f=, ng=, alpha=0.05, power=0.80)


# Logistic Regression -----------------------------------------------------
#p0 = p(y=1|x=0)
#p1 = p(y=1|x=1)
#alternative = direction of the alternative hypothesis ("two sided", "less", "greater")
wp.logistic(p0=, p1=, alpha=0.05, power=0.80, alternative="", family="normal")


# Poisson Regression ------------------------------------------------------
#exp0 = rate under the null hypothesis
#exp1 = the relative increase of the event rate (used to calculate effect size)
#alternative = direction of the alternative hypothesis ("two sided", "less", "greater")
#family = distribution of the predictor 
#parameter = 0.5 = bernoulli, 1 = exponential/poisson, (0,1) = lognormal/normal/uniform
wp.poisson(exp0=, exp1=, alpha=0.05, power=0.80, alternative ="less", family="uniform")


# multilevel modeling: cluster randomized trials -------------------------
#J = number of clusters/sides
#ICC = intra-class correlation
#alternative = direction of the alternative hypothesis ("two.sided", "less", "greater")
wp.crt2arm(f=, J=, icc=, alpha=0.05, power=0.80, alternative="two.sided")


# multilevel modeling: multisite randomized trials ------------------------
#J = number of clusters/sides
#tau00 = variance of cluster/site means
#tau11 = variance of treatment effects across sites
#sg2 = level-one error variance
#alternative = direction of the alternative hypothesis ("two.sided", "less", "greater")
#type = type of effect ("main", "site", "variance")
wp.mrt2arm(f=, J=, tau11=, sg2=, alpha=0.05, power=0.80, alternative="two.sided")
