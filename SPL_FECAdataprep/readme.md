[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_FECAdataprep** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: SPL_FECAdataprep

Published in:     Statistical Programming Languages

Description:      'Prepares the FEC data set for California, i.e. the broken data set 
                  P00000001-CA.csv is loaded and transformed into a proper data frame. 
                  Moreover, column names and variable types are adjusted, invalid or NA 
                  values deleted and some more data manipulation is done.' 

Keywords:         'data-preparation, data-cleaning, data-manipulation, election, fec'

Author:           Jonas Klein

Submitted:        Mon, March 13 2017

Datafile:         P00000001-CA.csv
```

## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)



# Load the raw data set for CA
rawdat = "P00000001-CA.csv"

# read data with header as the first row, fill the missing cell to produce a
# complete empty column on the right
dat0 = read.csv(rawdat, header = FALSE, fill = TRUE, stringsAsFactors = FALSE) 

# delete the empty column
dat0 = dat0[, -ncol(dat0)]

# make header out of the first row
names(dat0) = as.vector(unlist(dat0[1, ]))

# delete the first row
dat0 = dat0[-1,]

# fix the numbering for the rows, which would otherwise start from 2
row.names(dat0) = NULL

# Keep only relevant variables and drop the rest
dat0 = dat0[, c("cand_nm", "contbr_nm", "contbr_zip", "contbr_occupation", 
    "contb_receipt_amt", "contb_receipt_dt")]
    
# Better names for manipulation
names(dat0) = c("cand", "name", "zip", "occu", "amount", "date")

# Fix the zip codes as they are in 9 digit representation; we need 5 digits only
dat0$zip = substr(dat0$zip, 1, 5)

# No need for the candidates first name
cand      = strsplit(dat0$cand, ", ")
dat0$cand = sapply(cand, "[[", 1)

# Adjust variable types
dat0$cand   = as.factor(dat0$cand)
dat0$name   = as.factor(dat0$name)
dat0$zip    = as.numeric(dat0$zip)
dat0$occu   = as.factor(dat0$occu) 
dat0$amount = as.numeric(dat0$amount)
# dat0$date   = as.Date(dat0$date, "%d-%B-%y") 
# you might have to adjust your local time settings of your R session. Otherwise 
# you might end up with lots of NAs 
# Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Drop NA values
dat0 = na.omit(dat0)

# remove all zips that do not belong to CA
dat0 = dat0[dat0$zip >= 90000 & dat0$zip <= 96162,]

# Delete all negative donations, i.e. refunds
dat0 = dat0[dat0$amount > 0,]

# Add the party (D = democrats, R = republicans, TP = third party) of each 
# candidate to the data set
dat0$party = "TP"

dat0$party[dat0$cand %in% c("Bush", "Carson", "Christie", "Cruz", "Fiorina", 
    "Gilmore", "Graham", "Huckabee", "Jindal", "Kasich", "Pataki", "Paul", 
    "Perry", "Rubio", "Santorum", "Trump", "Walker")] = "R"

dat0$party[dat0$cand %in% c("Clinton", "Lessig", "O'Malley", "Sanders", 
    "Webb")] = "D"

# save the prepared data set to a file
write.csv(dat0, "dat0.csv", row.names = FALSE)
```
