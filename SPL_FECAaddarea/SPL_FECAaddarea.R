# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)

# Load libraries
library("magrittr")
library("dplyr")



# Load the data set prepared by addincome
dat3 = "dat2.csv" %>% read.csv()

# Create a new data.frame from the data.frame dat3 by summing up donations to 
# each candidate with equal zip codes and count the frequency for each zip
sdat3 = dat3 %>% 
    select(cand, party, zip, amount) %>%
    group_by(zip, cand, party) %>%
    summarise(total = sum(amount), f = n())
    
# Use dummy variables to determine which candidate got the largest total sum of 
# donations and the most frequent number of contributions, respecitvely in each 
# zip
sdat3 = sdat3 %>% 
    group_by(zip) %>%
    mutate(dummy1 = max(total), dummy2 = max(f))
    
# Now select only these candidates with the largest total sum of donations and 
# the most frequent number of contributions, respectively. Note that since the 
# candidates might differ two seperate data.frames are created
sdat31 = sdat3 %>% 
    filter(total - dummy1 == 0)
    
sdat32 = sdat3 %>% 
    filter(f - dummy2 == 0)

# Add the corresponding candidates name but restrict to the in our opinion most 
# interessting candidates (Clinton, Cruz, Sanders and Trump) and refer to the 
# others as "Other" but keep their party as variable. Select only the relevant
# variables
sdat31 = sdat31 %>% 
    mutate(area1_cand = ifelse(cand %in% c("Clinton", "Cruz", "Sanders", 
        "Trump"), as.character(cand), "Other"), area1_party = party) %>%
    select(zip, area1_cand, area1_party)
    
sdat32 = sdat32 %>% 
    mutate(area2_cand = ifelse(cand %in% c("Clinton", "Cruz", "Sanders", 
        "Trump"), as.character(cand), "Other"), area2_party = party) %>%
    select(zip, area2_cand, area2_party)

# and merge everything back the dat3 data.frame
dat3 = left_join(dat3, left_join(sdat31, sdat32))


# save the prepared data set to a file
write.csv(dat3, "dat3.csv", row.names = FALSE)
