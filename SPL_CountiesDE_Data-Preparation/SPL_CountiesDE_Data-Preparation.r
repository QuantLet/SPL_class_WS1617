# ======================================================
# ==================== PREPARE DATA ====================
# ======================================================


dat0 = read.table("Datensatz.txt", header = TRUE, sep = "\t", dec = ",", na.strings = "-")



# ==================== CHECKING THE DATA ====================
print.table(colMeans(dat0[, 4:13], na.rm = TRUE))
print.table(colSums(dat0[, 4:13], na.rm = TRUE))



# ==================== VARIABLE TRANSFORMATION ====================
X1  = round(dat0$Einwohner/dat0$Flaeche, digits = 0)                               # Population density in 1/km^2
X2  = round(dat0$BIP.in.Tsd.2009/dat0$Einwohner, digits = 1)                       # GDP per Inhabitant in 1000 Euro
X3  = round( 100 * dat0$Erwerb_LaWi_Tsd/dat0$Erwerbstaetige_Tsd, digits = 2)       # Workers in Agriculture in %
X4  = round( 100 * (dat0$BIP.in.Tsd.2009 - dat0$BIP.in.Tsd.2000)/(9 * dat0$BIP.in.Tsd.2000), digits = 2)  # GDP growth rate in % per year
X5  = round(1000 * dat0$Geburten/dat0$Einwohner, digits = 2)                       # Natality (Livebirths per 1000 capita)
X6  = round( 100 * (dat0$Zuzug - dat0$Wegzug)/dat0$Einwohner, digits = 3)          # Net Migration in %
X7  = round( 100 * dat0$Arbeitslose/(1000 * dat0$Erwerbstaetige_Tsd), digits = 2)  # Unemployment in %
X8  = dat0$Einwohner
X9  = dat0$Erwerbstaetige_Tsd
X10 = dat0$BIP.in.Tsd.2000

dat = cbind.data.frame(dat0$X, dat0$Land, dat0$Kategorie, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)
colnames(dat) = c("Name", "Bundesland", "Category", "Population density", "GDP per inhabitant", "Workers in Agriculture", "GDP growth rate", "Natality", "Net Migration", 
    "Unemployment", "Population", "Employees", "GDP in Tsd. 2000")
dat$Category = gsub("Kreisfreie Stadt", "County-Level City", dat$Category)
dat$Category = gsub("Landkreis", "County", dat$Category)
rm(dat0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)  # cleaning the environment



# ==================== FILTERING NAS ====================
table(is.na.data.frame(dat))            # 12% of total values are missing
sum(complete.cases(dat))                # 391 of 470 observables are complete
View(dat[!complete.cases(dat), ])       # shows incomplete cases: almost complete Meck.-Vorp. and large parts of Sachsen and Sachsen-Anhalt
dat.final = dat[complete.cases(dat), ]


    # this function deletes all rows with more than n NAs:
    delete.na = function(df, n = 0) {
        selection = apply(df, 1, function(x) sum(is.na(x)) <= n)
        return(df[selection, ])
    }

    
dat2na = delete.na(dat, 2)
table(!complete.cases(dat2na))
dat3na = delete.na(dat, 3)
table(!complete.cases(dat3na))
dat4na = delete.na(dat, 4)
table(!complete.cases(dat4na))
dat5na = delete.na(dat, 5)
table(!complete.cases(dat5na))
dat6na = delete.na(dat, 6)
table(!complete.cases(dat6na))
rm(dat2na, dat3na, dat4na, dat5na, dat6na)

# Plotting the NA-Occurence
na_count = c(0, 0)
for(n in c(2:9)) na_count[n+1] = sum(!complete.cases(delete.na(dat, n))) - sum(!complete.cases(delete.na(dat, n-1)))
x = seq(0, 9, by = 1)
barplot(na_count, names.arg = x, xlab = "NAs per Observation", ylab = "Observations")


# Summary: there is only one city with two NAs (Hannover), none with three, four or five, but 18 with six.  
# Since 6 missings are too many for detailed analysis, and only Hannover lacks less two, 
# only complete cases were used for further analysis.



# ==================== OUTPUT ====================
# The following code ensures clean export and import of the data

# AS CSV
write.csv(dat.final, file = "data.csv")
# read in via:
# dat = read.csv(file = "data.csv", header = TRUE, row.names = 1, quote = "\"", dec = ".", check.names = FALSE)
# dat[, 4:10] = lapply(dat[, 4:10], function(x) as.numeric(as.character(x)))

# AS TAB-SEPARATED TXT (GOOD FOR EXCEL)
write.table(dat.final, file = "data.txt", sep = "\t", dec = ",", row.names = TRUE, col.names = TRUE)
# read in via:
# dat = read.table("data.txt", header = TRUE, sep = "\t", dec = ",", na.strings = "NA", check.names = FALSE)
# dat[, 4:10] = lapply(dat[, 4:10], function(x) as.numeric(as.character(x)))
