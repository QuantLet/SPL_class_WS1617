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
