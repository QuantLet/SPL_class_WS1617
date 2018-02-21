[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_FECAaddincome** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: SPL_FECAaddincome

Published in:     Statistical Programming Languages

Description:      'Uses the IRS tax records for California to construct a proxy the wealth of a
                  a zip code.' 

Keywords:         data-preparation, feature-engineering, irs, proxy, election, fec

Author:           Janek Willeke

Submitted:        Mon, March 13 2017

Datafile:         dat1.csv, 14zp05ca.csv
```

## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)



# Load the data set prepared by addgender
dat2 = read.csv("dat1.csv")

# read in IRS tax data from California
irsdata = read.csv("14zp05ca.csv", na.strings = c("", " ", "NA")) 

# keep only adjusted gross income
irsgai = irsdata[c(1, 2, 13)] 

# transform to wide format
irsgai_wide = reshape(irsgai, idvar = "CALIFORNIA", timevar = "X", 
    direction = "wide")

# remove redundant rows and columns and drop NAs:
#     - the first row is the total sum of IRS records in CA but this row can be
#       easily obtained by the colSums function. Thus not necessary
#     - the first column is the zip code. Keep it
#     - the fith column is the number of IRS records with adjusted gross income 
#       of less then 25k. Keep it
#     - the columns six to ten are the number of IRS records with adjusted gross
#       income of more than 25k, 50k, 75k, 100k and 200k, respectively. Keep 'em
irsgai_wide = na.omit(irsgai_wide[, c(1, 5:10)])[-1, ]

# adjust the column names
names(irsgai_wide) = c("zip", "b25k", "a25k", "a50k", "a75k", "a100k", "a200k")

# "**" and "." are used in place of 0, replace it with 0 and remove commata as 
# they are not necessary in R

irsgai_wide = apply(irsgai_wide, 2, function(y) gsub(",", "", fixed = FALSE, y))
irsgai_wide = apply(irsgai_wide, 2, function(y) gsub("*", "0", fixed = TRUE, y))
irsgai_wide = apply(irsgai_wide, 2, function(y) gsub(".", "0", fixed = TRUE, y))

# for some reason the data.frame structure is removed... create it again
irsgai_wide = as.data.frame(irsgai_wide)

# transform the values to numeric
irsgai_wide = apply(irsgai_wide, 2, as.integer)

# calculate relative values
irsgai_wide_rel = irsgai_wide[, 2:7]/rowSums(irsgai_wide[, 2:7])

# create a weighted continous distribution, where higher income has higher 
# weights distribution ranges from 0 to 1

tmp = data.frame(zip = irsgai_wide[, 1], distr = (1 * irsgai_wide_rel[, 1] +
    2 * irsgai_wide_rel[, 2] + 3 * irsgai_wide_rel[, 3] + 
    4 * irsgai_wide_rel[, 4] + 5 * irsgai_wide_rel[, 5] +
    6 * irsgai_wide_rel[, 6])/6)

# finally, match the ranking to the donation data set by zip code
dat2 = merge(x = dat2, y = tmp, by = "zip", all.x = TRUE)

# save the prepared data set to a file
write.csv(dat2, "dat2.csv", row.names = FALSE)
```
