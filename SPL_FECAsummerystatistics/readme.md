[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_FECAsummarystatistic** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: SPL_FECAsummarystatistic

Published in:     Statistical Programming Languages

Description:      'Calculates the most common summary statistics (sum, mean, median, min, max,
                  standard deviation) for the amount of money contributed to each candidate.'

Keywords:         data-analysis, summary-statistics, dplyr, fec, election

Author:           Lukas Moedl

Submitted:        Mon, March 13 2017

Datafile:         dat4.csv
```

| Candidate | Sum      | Mean     | Median | Std. Dev. | Min. | Max.  | Count  | Most frequent Zip Codes |
|-----------|----------|----------|--------|-----------|------|-------|--------|-------------------------|
| Clinton   | 95206228 | 139.6457 | 25     | 418.3855  | 0.01 | 10000 | 681770 | 94611, 94110, 94114     |
| Trump     | 14319639 | 171.4516 | 80     | 351.5690  | 0.80 | 5400  | 83520  | 92067, 92037, 92660     |
| Sanders   | 20684424 | 51.1507  | 27     | 119.7251  | 1.00 | 10000 | 404382 | 94117, 94114, 94110     |
| Cruz      | 7035984  | 125.5551 | 50     | 472.4062  | 1.00 | 10800 | 56039  | 90274, 92637, 92705     |
| Other     | 21349140 | 340.2416 | 50     | 760.0941  | 0.01 | 10800 | 62747  | 90272, 90049, 92660     |


## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)


# Load libraries
library("magrittr")
library("dplyr")



# Load the full prepared data set
dat = "dat4.csv" %>% read.csv()

# Define a function that returns the n largest values of a vector
maxn = function(x, n = 3) tail(sort(x), n)

# focus on the in our opinion most interesting candidates
ourcands = c("Clinton", "Trump", "Sanders", "Cruz")
# and refer to the other as "Other"
dat = dat %>% 
    mutate(cand = ifelse(cand %in% ourcands, as.character(cand), "Other"))

# Order the candidates
dat$cand = factor(dat$cand, levels = c(ourcands, "Other"), ordered = TRUE)

# Caluclate summary statistics: sum, mean, standard deviation, minimal and 
# maximal value of the donation amount, number of donations to each candidate 
# and the corresponding 3 most frequent zip codes among donations for each zip
summarystatistic = dat %>%
    group_by(cand) %>%
    summarise(sum = sum(amount), mean = mean(amount), median = median(amount),
        sd = sd(amount), min = min(amount), max = max(amount), count = n(),
        top3zip = paste0(names(maxn(table(zip))), collapse = ", "))

# View it
# View(summarystatistic)
```
