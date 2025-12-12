
library(tidyverse)
library(haven)
library(estimatr)
library(margins)
library(sandwich)
library(lmtest)
library(clarify)

uav <- read_dta("UAV2014Datasetsup003.dta")

fit <- glm(armedprogram ~ terrdisputes + lnnterr5 + autocrat + democrat + lngdpcap + defense,
           data = uav, family = binomial(link = "probit"))

cl_vcov <- sandwich::vcovCL(fit, cluster = uav$cowcc)


# Clustered standard errors
cl_vcov <- sandwich::vcovCL(m2_probit, cluster = uav$cowcc)

# Coefficients with clustered SE
coeftest(m2_probit, vcov. = cl_vcov)


mfx_cluster <- margins(m2_probit, 
                       data = uav,
                       vcov = cl_vcov)
summary(mfx_cluster)


# supply a cluster-robust vcov if you like (you might need to compute it manually and pass it to sim())
csim <- sim(fit, n = 5000, vcov = cl_vcov)  

# define two scenarios (data frames) for the "setx" equivalent

baseline <- uav %>%
  summarise(
    across(c(terrdisputes, lnnterr5, lngdpcap), \(x) mean(x, na.rm = TRUE))
  )

newdata1 <- baseline %>% 
  mutate(autocrat=1, democrat=0, defense=0)
newdata2 <- baseline %>% 
  mutate(autocrat=0, democrat=0, defense=0)
newdata3 <- baseline %>% 
  mutate(autocrat=0, democrat=1, defense=0)


est1 <- sim_apply(csim, FUN = function(fit) predict(fit, newdata = newdata1, type = "response"))
est2 <- sim_apply(csim, FUN = function(fit) predict(fit, newdata = newdata2, type = "response"))
est3 <- sim_apply(csim, FUN = function(fit) predict(fit, newdata = newdata3, type = "response"))

p1 <- unclass(est1)[,1]  # scenario 1 simulated probs
p2 <- unclass(est2)[,1]  # scenario 2 simulated probs
p3 <- unclass(est3)[,1]  # scenario 3 simulated probs

diff <- p1 - p3
mean(diff)                       # point estimate for scenario difference
quantile(diff, c(.025, .5, .975))  # 95% simulation interval
sd(diff)  
hist(diff)

dat <-
  tibble(
    type = rep(c("autocracies", "mixed", "democracies"),each = 5000),
    prob = c(p1, p2, p3)
  )

summary_df <- 
  dat %>% 
  group_by(type) %>%
  summarise(
            conf.high = quantile(prob, 0.975),
            conf.low = quantile(prob, 0.025),
            prob = mean(prob))


ggplot(dat, aes(type, prob)) + geom_point(alpha = 0.01, position = position_jitter(width = 0.1)) +
  geom_point(data = summary_df, aes(type, prob), color = "red", size = 3) +
  geom_linerange(data = summary_df, aes(type, ymin = conf.low, ymax = conf.high), color = "red") +
  labs(y = "Predicted Probability of Armed UAV Program", x = "Regime Type") +
  theme_minimal()









# Probit model
m2_probit <- glm(
  armedprogram ~ terrdisputes + lnnterr5 + autocrat + democrat + lngdpcap + defense,
  data = uav,
  family = binomial(link = "probit")
)


