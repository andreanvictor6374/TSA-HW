require('MASS'); require(slipper)
data('Boston')
"
a) Based on this data set, provide an estimate for the population mean of medv. 
Call this estimate μ^.
"

mu <- mean(Boston$medv)

"
b) Provide an estimate of the standard error of μ^. Interpret this result.
"
 
se <- sqrt(var(Boston$medv)/nrow(Boston))

"
c) Now estimate the standard error of μ^ using the bootstrap. 
How does this compare to your answer from (b)?
"

Boston %>%
  slipper(mean(medv), B = 1000) %>%
  summarise(se = sd(value)) %>%
  .[['se']] -> se_boot


"
d) Based on your bootstrap estimate from (c), provide a 95 % confidence interval for the mean of medv. 
Compare it to the results obtained using t.test(Boston$medv).
"
Boston %>%
  summarise(lower = mean(medv) - 1.96*se_boot, 
            mean = mean(medv), 
            upper = mean(medv) + 1.96*se_boot)

t.test(Boston$medv)

"
e) Based on this data set, provide an estimate, μ^med, 
for the median value of medv in the population.
"

med <- median(Boston$medv)


"
f) We now would like to estimate the standard error of μ^med. 
Unfortunately, there is no simple formula for computing the standard error of the median. 
Instead, estimate the standard error of the median using the bootstrap. 
Comment on your findings.
"
Boston %>%
  slipper(median(medv), 1000) %>%
  summarise(se = sd(value)) %>%
  .[['se']] -> med_se

"
g) Based on this data set, provide an estimate for the tenth percentile of medv in Boston suburbs. 
Call this quantity μ^0.1.
"
perc_ten <- quantile(Boston$medv, 0.1)

"
h) Use the bootstrap to estimate the standard error of μ^0.1. Comment on your findings.
"

Boston %>%
  slipper(quantile(medv, 0.1), B = 1000) %>%
  summarise(se = sd(value)) %>%
  .[['se']] -> perc_ten_se


