[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_FECAaddoccuclass** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: SPL_FECAaddoccuclass

Published in:     Statistical Programming Languages

Description:      'Classifies the occupation with keywords and classifications taken from
                  https://github.com/datasciencedojo/DataMiningFEC/blob/master/6%20Bucketing%20Occupation%20Groups.R'

Keywords:         data-preparation, feature-engineering, occupation, classification fec, election

Author:           Jonas Klein

Submitted:        Mon, March 13 2017

Datafile:         dat3.csv, occupationclassed.txt
```

## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)



# Load the data set prepared by addarea
dat4 = read.csv("dat3.csv")

# Add a new column for the classes and default the value to "OTHER"
dat4$class = "OTHER"

# Transform the values of occupation to uppercase
dat4$occu = toupper(dat4$occu)

# Load the occupation class+keyword data set
occuclassed = read.table("occupationclassed.txt", header = TRUE, sep = ";")

# save the classes into an extra vector to increase CPU efficiency
classes = as.character(occuclassed[, 1])

# Now classify the occupation with the keywords taken from the occupationclassed
# data set
for(i in 1:nrow(occuclassed))
{
    # Split the keywords into a vector
    keywords = strsplit(as.character(occuclassed[i,2]), ",")[[1]]
    
    # and paste them to a vector
    pattern = paste(keywords)
    
    # Assign the classes to the  matched values
    dat4$class[which(dat4$occu %in% pattern)] = classes[i]
}

# save the prepared data set to a file
write.csv(dat4, "dat4.csv", row.names = FALSE)
```
