[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPLfidata** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: FECAgenderphi

Published in:     Statistical Programming Languages

Description:      'Calculates correlation between candidate gender and contributor gender
                  and performs a Chisquare test of independence.'

Keywords:         data-analysis, phi-coefficient, correlation, categorial-correlation, fec, election, gender

Author:           Lukas Moedl

Submitted:        Mon, March 13 2017

Datafile:         dat4.csv
```

|                        | Candidate Female | Candidate Male |         |
|------------------------|------------------|----------------|---------|
| **Contributor Female** | 388978           | 231764         | 620742  |
| **Contributor Male**   | 225462           | 302054         | 527516  |
|                        | 614440           | 533818         | 2296516 | 

*phi* = 0.1990643

## R Code
```r
# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)



# Load the full prepared data set
dat = read.csv("dat4.csv")

# Calculate the Phi coefficient of contributor and candidate gender by
#     Phi = (n00*n11 - n01*n10)/sqrt((n00+n01)*(n10+n11)*(n00+n10)*(n01+n11))
# where 
#     n00 = number of female contributors donating to female candidates
#     n10 = number of male contributors donating to female candidates
#     n01 = number of female contributors donating to male candidates
#     n11 = number of male contributors donating to male candidates.
# Note that we omit gender = both from the analysis
N   = table(dat$gender, dat$cand_gender)[1:2,1:2]
Phi = det(N) * prod(colSums(N),rowSums(N))^(-0.5)

# View it
# View(Phi)
```
