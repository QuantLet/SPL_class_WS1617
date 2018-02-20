[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPLfidata** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: FECAaddgender

Published in:     Statistical Programming Languages

Description:      'Infers gender of contributors based on first names by using, census data of 
                  first names and adds the gender to the data.'

Keywords:         data-preparation, inference, feature-engineering, gender, election, fec

Author:           Andrii Zakharov

Submitted:        Mon, March 13 2017

Datafile:         dat0.csv, census-dist-female-first.csv, census-dist-male-first.csv
```

## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)



# Load the data set prepared by prepdata
dat1 = read.csv("dat0.csv")


# sort by contributor name
dat1 = dat1[order(dat1$name),] 

# load a list of female names and a list of male names
# (from https://deron.meranda.us/data/)
m_names_dat = read.csv("census-dist-male-first.csv", header = F)
f_names_dat = read.csv("census-dist-female-first.csv", header = F)

# save each list to a data set
m_names_list = m_names_dat[, 1]
f_names_list = f_names_dat[, 1]

# trim leading and trailing white spaces
dat1_names = trimws(as.vector(dat1$name)) 

# quick-n-dirty extract first names with a string split
dat1_first_names = sapply(dat1_names, strsplit, " ")
dat1_first_names = sapply(dat1_first_names, "[[", 2)

# produce two boolean vectors for name occurence in male and female lists
test_m = sapply(dat1_first_names, "%in%", as.vector(m_names_list))
test_f = sapply(dat1_first_names, "%in%", as.vector(f_names_list))

# produce a vector of genders based on booleans
contbr_gender = c()
contbr_gender[which(test_m & test_f)]   = "both"
contbr_gender[which(!test_m & !test_f)] = "neither"
contbr_gender[which(test_m & !test_f)]  = "m"
contbr_gender[which(!test_m & test_f)]  = "f"

# see how many "both" and "neither" we have
length(dat1_first_names[contbr_gender == "both"])
length(dat1_first_names[contbr_gender == "neither"])

# it seems there are too many "both"... let's fix that a little
fn_both_genders = dat1_first_names[which(contbr_gender == "both")]

# look at rank of name in male vs female lists (usage frequency), assign gender 
# based on that.
# couldn't figure out how to do it without a for-loop... have some patience
both_to_m_f = character(length(fn_both_genders))

for(i in 1:length(fn_both_genders))
{
    if(m_names_dat[which(m_names_dat[,1] == fn_both_genders[i]), 4] - 
        f_names_dat[which(f_names_dat[,1] == fn_both_genders[i]), 4] >= 0)
    {
        both_to_m_f[i] = "f"
    }
    else
    {
        both_to_m_f[i] = "m"
    }
}

# looks good, let's put them back into the whole gender vector
contbr_gender[contbr_gender == "both"] = both_to_m_f

# and the whole vector into the dataframe
dat1$gender = contbr_gender

### Let's also add gender for our candidates
dat1$cand_gender = "m"
dat1$cand_gender[which(dat1$cand %in% c("Clinton", "Fiorina", "Stein"))] = "f"
    
# save the prepared data set to a file
write.csv(dat1, "dat1.csv", row.names = FALSE)
```
