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
