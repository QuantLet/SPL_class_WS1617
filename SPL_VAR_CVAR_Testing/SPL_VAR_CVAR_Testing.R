# -----------------------------------------------------------------------------
# Quantlet No. 3 : Testing Quantlet
#
# This quantlet covers three different kind of tests. Firstly, three 
# distribution tests, namely the Kolmogorow-Smirnov test, Kuiper´s test and the 
# Anderson-Darling test. The second type of test is an unconditional coverage 
# test, namely Kupiec´s test, to test the Values at Risk. Finally, the McNeil &
# Frey test is performed in order to test the Conditional Value at Risk
# -----------------------------------------------------------------------------

# The package 'CircStats' is needed for the Kuiper's test that 
# is already implemented in R The package
# 'ADGofTest' is needed for the Anderson-Darling test that 
# is already implemented in R The package
# 'rugarch' is needed for the Kupiec test that 
# is already implemented in R

libraries = c("formatR", "CircStats", "ADGofTest", "rugarch", "Dowd")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Remove everything that might be saved
rm(list = ls(all = TRUE))
# Remove all window adjustments for graphics
graphics.off()

# Source VaR_and_CVaR_Quantlet
source("Quantlet2/VaR_and_CVaR_Quantlet.R")

# ----------------------------------------------------------------------------- 
# FUNCTION FOR DISTRIBUTION TESTS 
# -----------------------------------------------------------------------------

# Define function for performing goodness of fit tests
# Arguments: a (gof test), b (distribution unter H0), 
# data1 (first data set), data2 (second data set; default = NULL), 
# alpha (significance level) 
# Output: matrix of test results

# Set data2 equals NULL for one-sample test
data2    = NULL

gof_test = function(a, b, data1, data2, alpha) {
  
  # Standardize data1 for one-sample distribution test for t-distribution
  if (b == 2 && is.null(data2)) {
    data1 = (data1 - mean(data1)) * sqrt(2) / sd(data1)
  }
  
  if (a != 1 && a != 2 && a != 3) {
    stop("Please choose 1 for the KS test, 2 for the K test 
         or 3 for the AD test.")
  } else if (b != 1 && b != 2) {
    stop("One-sample test: Please choose 1 to test for normality or 2 for 
         t-distirbution. Two-sample test: Please choose 1 or 2.")
  } else if (a == 1) {
    
    # ------------------------------------------------------------------------- 
    #  KOLMOGOROV-SMIRNOV TEST  
    # -------------------------------------------------------------------------       
    
    # Sort data1 in in ascending order
    x1 = sort(data1)
    # n1 is the number of elements in data1
    n1 = length(x1)
    # f1 is the empirical cumulative distribution function of data1
    f1 = ecdf(x1)
    
    # For b == 1 we test, whether our data is normal distributed
    if (b == 1) {
      # Calculate the absolute values of the difference between the 
      # normal distribution (with mu = mean(X1) and sigma = sd(x1))
      # and the ecdf of our data
      diff_up1 = max(f1(x1) - pnorm(x1, mean(x1), sd(x1)))
      diff_lo1 = max(pnorm(x1, mean(x1), sd(x1)) - (f1(x1) - (1 / n1)))
      # For b == 2 we test, whether our data is t-distributed
    } else if (b == 2) {
      diff_up1 = max(f1(x1) - pt(x1, df = 4))
      diff_lo1 = max(pt(x1, df = 4) - (f1(x1) - (1 / n1)))
    }
    # If length(data2) == 0 we use the one-sample KS test
    if (length(data2) == 0) {
      
      # D is the test statistic and equals the biggest distance of  
      # our data and the distribution that is tested
      D = max(diff_up1, diff_lo1)
      
      # Critical values for n < 36 for alpha = 0.1, alpha = 0.05, alpha = 0.01
      crit_table_010 = c(0.95, 0.776, 0.636, 0.565, 0.51, 0.468, 0.436, 
                         0.41, 0.387, 0.369, 0.352, 0.338, 0.325, 0.314, 
                         0.304, 0.295, 0.286, 0.279, 0.271, 0.265, 0.259, 
                         0.253, 0.247, 0.242, 0.238, 0.233, 0.229, 0.225, 
                         0.221, 0.218, 0.214, 0.211, 0.208, 0.205, 0.202)
      crit_table_005 = c(0.975, 0.842, 0.708, 0.624, 0.563, 0.519, 0.483, 
                         0.454, 0.43, 0.409, 0.391, 0.375, 0.361, 0.349, 
                         0.338, 0.327, 0.318, 0.309, 0.301, 0.294, 0.287, 
                         0.281, 0.275, 0.269, 0.264, 0.259, 0.254, 0.25, 
                         0.246, 0.242, 0.238, 0.234, 0.231, 0.227, 0.224)
      crit_table_001 = c(0.995, 0.929, 0.829, 0.734, 0.669, 0.617, 0.576, 
                         0.542, 0.513, 0.489, 0.468, 0.45, 0.432, 0.418, 
                         0.404, 0.392, 0.381, 0.371, 0.361, 0.352, 0.344, 
                         0.337, 0.33, 0.323, 0.317, 0.311, 0.305, 0.3, 
                         0.295, 0.29, 0.285, 0.281, 0.277, 0.273, 0.269)
      
      # We only have the critical values for n < 36 for alpha = 0.1, 
      # alpha = 0.05, alpha = 0.01 
      if (alpha != 0.1 && alpha != 0.05 && alpha != 0.01) {
        stop("Please choose 0.05, 0.10 or 0.01 for alpha.")
        # If n > 35 we can calculate the critical value 
        # with the following formula
      } else if (n1 > 35) {
        Dalpha = sqrt(-0.5 * log(alpha / 2)) / sqrt(n1)
        # If alpha is < 36 we choose the critical value from the table
      } else if (alpha == 0.1) {
        Dalpha = crit_table_010[n1]
      } else if (alpha == 0.05) {
        Dalpha = crit_table_005[n1]
      } else if (alpha == 0.01) {
        Dalpha = crit_table_001[n1]
      }
      
      # In R implemented one-sample KS test
      if (b == 1) {
        test1 = ks.test(data1, "pnorm", mean(data1), sd(data1))
      } else if (b == 2) {
        test1 = ks.test(data1, "pt", df = 4)
      }
      # data2 is not empty (not equal to NULL): 
      # We perform the two-sample KS test
    } else {
      # Sort data2 in in ascending order
      x2 = sort(data2)
      # n2 is the number of elements in data2
      n2 = length(x2)
      # f2 is the empirical cumulative distribution function of data2
      f2 = ecdf(x2)
      
      # Calculate the absolute values of the difference 
      # between the two ecdfs of data1
      diff_up1 = abs(f2(x1) - f1(x1))
      diff_lo1 = abs(f2(x1) - (f1(x1) - 1 / n1))
      
      # Calculate the absolute values of the difference 
      # between the two ecdfs of data2
      diff_up2 = abs(f2(x2) - f1(x2))
      diff_lo2 = abs(f2(x2) - (f1(x2) - 1 / n2))
      
      # D is the test statistic and equals the biggest 
      # distance of our two datasets
      D = max(diff_up1, diff_lo1, diff_up2, diff_lo2)
      
      if (n1 + n2 > 35) {
        # If n1 + n2 > 35 we can calculate the critical value 
        # with the formula for Dalpha by choosig the correct calpha
        if (alpha == 0.1) {
          calpha = 1.22
        } else if (alpha == 0.05) {
          calpha = 1.36
        } else if (alpha == 0.025) {
          calpha = 1.48
        } else if (alpha == 0.01) {
          calpha = 1.63
        } else if (alpha == 0.005) {
          calpha = 1.73
        } else if (alpha == 0.001) {
          calpha = 1.95
          # We only have calpha for alpha = 0.1, alpha = 0.05, alpha = 0.025, 
          # alpha = 0.01, alpha = 0.005, alpha = 0.001 
        } else {
          stop("Please choose 0.05, 0.025, 0.01, 0.005 or 0.001 for alpha.")
        }
        # Formula for calculating the critical value
        Dalpha = calpha / sqrt((n1 * n2) / (n1 + n2))
      } else {
        # We only have a formula for n1 + n2 > 35
        stop("This test only works, if the sum of the length of the two 
             datasets is greater than 35.")
      }
      
      # In R implemented two-sample KS test - do our two datasets 
      # follow the same distribution?
      test1 = ks.test(data1, data2)
    }
    
    # If the test statistic ist greater than the critical value we reject 
    # the Null Hypothesis otherwise we cannot reject the Null Hypothesis 
    # result1 is the result of our own KS test
    result1 = ifelse(D > Dalpha, "We reject the Null Hypothesis", 
                     "We cannot reject the Null Hypothesis")
    # If the p-value ist smaller than alpha we reject the Null Hypothesis 
    # otherwise we cannot reject the Null Hypothesis 
    # result2 is the result of the KS test that is implemented in R
    result2 = ifelse(test1$p.value < alpha, "We reject the Null Hypothesis", 
                     "We cannot reject the Null Hypothesis")
    r       = c("Test: ", "Kolmogorov-Smirnov test", "Our own result: ", 
                result1, "Test statistic: ", D, 
                "Critical Value: ", Dalpha, "R result: ", result2, 
                "Alpha: ", alpha, "P-Value: ", test1$p, "Statistic: ",
                test1$statistic)
    # result matrix contains the results of our own KS test 
    # and of the KS test that is implemented in R
    result = matrix(r, nrow = 8, byrow = TRUE)
    return(result)
  } else if (a == 2) {
    
    # ------------------------------------------------------------------------- 
    # KUIPERS TEST 
    # ------------------------------------------------------------------------- 
    
    x1 = sort(data1)
    n1 = length(x1)
    f1 = ecdf(x1)
    
    if (n1 < 36) {
      stop("This test only works, if the length of the data is greater than 35.")
    }
    if (b == 1) {
      # Calculate the maximum values of the difference between the 
      # normal distribution (with mu = mean(X1) and
      # sigma = sd(x1)) and the ecdf of our data
      diff_up1 = max(f1(x1) - pnorm(x1, mean(x1), sd(x1)))
      diff_lo1 = max(pnorm(x1, mean(x1), sd(x1)) - (f1(x1) - (1 / n1)))
      # For b == 2 we test, whether our data is t-distributed
    } else if (b == 2) {
      diff_up1 = max(f1(x1) - pt(x1, df = 4))
      diff_lo1 = max(pt(x1, df = 4) - (f1(x1) - (1 / n1)))
    }
    
    V = diff_up1 + diff_lo1
    # Test statistic
    V_star = V * (sqrt(n1) + 0.155 + (0.24 / sqrt(n1)))
    
    # Critical values Valpha for different alpha
    if (alpha != 0.15 && alpha != 0.1 && alpha != 0.05 && 
        alpha != 0.025 && alpha != 0.01) {
      stop("Please choose 0.15, 0.1, 0.05, 0.025 or 0.01 for alpha.")
    } else {
      if (alpha == 0.15) {
        Valpha = 1.537
      } else if (alpha == 0.1) {
        Valpha = 1.62
      } else if (alpha == 0.05) {
        Valpha = 1.747
      } else if (alpha == 0.025) {
        Valpha = 1.862
      } else if (alpha == 0.01) {
        Valpha = 2.001
      } 
      # Calculate the Kuiper test that is implemented in R.  
      # If the normal distribution function (t-distribution function) 
      # of our data is uniformally distributed, then our data is 
      # normally distributed (t-distributed)
      # The function kuiper() performs the one-sample Kuiper´s test
      # and the result is printed to the screen (without a print command)
      if (b == 1) {
        kuiper(pnorm(data1, mean(data1), sd(data1)) * 2 * pi)
      } else if (b == 2) {
        kuiper(pt(data1, df = 4) * 2 * pi)
      }
    }
    result1 = ifelse(V_star > Valpha, "We reject the Null Hypothesis", 
                     "We cannot reject the Null Hypothesis")
    r       = c("Test: ", "Kuiper´s test", "Our own result: ", result1, 
                "Test statistic: ", V_star, "Critical Value: ", Valpha, 
                "Alpha: ", alpha)
    result  = matrix(r, nrow = 5, byrow = TRUE)
    return(result)
    
  } else if (a == 3) {
    
    # ------------------------------------------------------------------------- 
    # ANDERSON-DARLING TEST 
    # -------------------------------------------------------------------------
    
    x1 = sort(data1)
    n1 = length(x1)
    f1 = ecdf(x1)
    if (n1 < 6) {
      stop("This test only works, if the length of the data is greater than 5.")
    }
    if (b == 1) {
      # Calculate the test statistic using the normal distribution 
      # (with mu = mean(X1) and sigma = sd(x1)) and the ecdf of our data
      z1 = log(1 - rev(pnorm(x1, mean(x1), sd(x1))))
      A  = -2 * sum((f1(x1) - (1 / (2 * n1))) * (log(pnorm(x1, mean(x1), 
           sd(x1))) + z1)) - n1
      
      # The function ad.test() tests for normality
      test3 = ad.test(data1, pnorm, mean(data1), sd(data1))
    } else if (b == 2) {
      # For b == 2 we test, whether our data is t-distributed
      z1   = log(1 - rev(pt(x1, df = 4)))
      A    = -2 * sum((f1(x1) - (1 / (2 * n1))) * (log(pt(x1, 
             df = 4)) + z1)) - n1
      
      # To test for t-distibution with the function ad.test() 
      test3 = ad.test(data1, pt, df = 4)
    }
    
    # Test statistic
    A_star = A
    
    if (alpha == 0.15) {
      Aalpha = 1.61
    } else if (alpha == 0.1) {
      Aalpha = 1.933
    } else if (alpha == 0.05) {
      Aalpha = 2.492
    } else if (alpha == 0.025) {
      Aalpha = 3.0201
    } else if (alpha == 0.01) {
      Aalpha = 3.857
    } else {
      stop("Please choose 0.15, 0.1, 0.05, 0.025 or 0.01 for alpha.")
    }
    
    result1 = ifelse(A_star > Aalpha, "We reject the Null Hypothesis", 
                     "We cannot reject the Null Hypothesis")
    result2 = ifelse(test3$p.value < alpha, "We reject the Null Hypothesis", 
                     "We cannot reject the Null Hypothesis")
    r       = c("Test: ", "Anderson-Darling test", "Our own result: ", result1, 
                "Test statistic: ", A_star, 
                "Critical Value: ", Aalpha, "R result: ", result2, "Alpha: ", 
                alpha, "P-Value: ", test3$p, "Statistic: ", test3$statistic)
    result  = matrix(r, nrow = 8, byrow = TRUE)
    return(result)
  }
}

# -----------------------------------------------------------------------------
# DISTRIBUTION TEST 
# -----------------------------------------------------------------------------

# Confidence level alpha
alpha1 = 0.05

# Converting the historical data to atomic vector
hist = t(as.data.frame(input.pf))

# Converting the Monte Carlo data to atomic vector
mc_data_norm = t(as.data.frame(MC.VaR[1]))
mc_data_t    = t(as.data.frame(MC.VaR[3]))

# Are the returns corresponding to z1 normally distributed?
z1        = list(hist, mc_data_norm, mc_data_t)
gof.norm  = lapply(1:length(z1), function(y) sapply(1:3, 
                   function(x) gof_test(x, 1, z1[[y]], data2, alpha1)))

# Are the returns corresponding to z2 t-distributed?
z2        = list(hist, mc_data_norm, mc_data_t)
gof.t     = lapply(1:length(z2), function(y) sapply(1:3, 
                   function(x) gof_test(x, 2, z2[[y]], data2, alpha1)))

# Do the Monte Carlo data (normal distribution) and the historical data 
# follow the same distribution?
gof.mcnorm.hist = gof_test(1, 1, mc_data_norm, hist, alpha1)

# Do the Monte Carlo data (t-distribution) and the historical data 
# follow the same distribution?
gof.mct.hist    = gof_test(1, 1, mc_data_t, hist, alpha1)

# -----------------------------------------------------------------------------
# BACKTEST: Coverage test  
# -----------------------------------------------------------------------------

PnL = input.pf[202:nrow(input.pf), ]

# Function to combine testresults into one data.frame
cTestData = function(VaR, CVaR, actPnL) {
  VaR_test          = VaRTest(alpha = 0.05, actual = actPnL, 
                              VaR = VaR, conf.level = 0.95)
  CVaR_test         = ESTest(alpha = 0.05, actual = actPnL, VaR = VaR, 
                             ES = CVaR, conf.level = 0.95)
  VarTest           = data.frame(Date = attributes(input.pf)$row.names[
                                 202:nrow(input.pf)], 
                                 VAR  = VaR, 
                                 CVAR = CVaR, PNL = actPnL, 
                                 FLaG = ifelse(actPnL < VaR, 1, 0))
  rownames(VarTest) = attributes(input.pf)$row.names[202:nrow(input.pf)]
  output            = list("VaR_test" = VaR_test, "CVaR_test" = CVaR_test, 
                           "Test.data" = VarTest)
  return(output)
}

# -----------------------------------------------------------------------------
# VaRTest and ESTest for VaR and CVaR from historical data (unweighted)
# -----------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function (i) Hist_Sim(a = 0.95, 
                                    input.pf = input.pf[i:(i + 200), ], 
                                    "VaR", lambda = 0.995)[1])

CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function (i) Hist_Sim(a = 0.95, 
                                    input.pf = input.pf[i:(i + 200), ], 
                                    "CVaR", lambda = 0.995)[1])

VarTest.dat.hist = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)

# -----------------------------------------------------------------------------
# VaRTest and ESTest for VaR and CVaR from historical data (weighted)
# -----------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Hist_Sim(a = 0.95, 
                                   input.pf = input.pf[i:(i + 200), ], 
                                   "VaR", lambda = 0.995)[2])

CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Hist_Sim(a = 0.95, 
                                   input.pf = input.pf[i:(i + 200), ], 
                                   "CVaR", lambda = 0.995)[2])

VarTest.dat.hist_w = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)

# -----------------------------------------------------------------------------
# VaRTest and ESTest for analytical VaR and CVaR with normal distribution 
# -----------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Analyt_Mod(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "VaR", df = 4)[1])

CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Analyt_Mod(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "CVaR", df = 4)[1])

VarTest.dat.ananorm = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)

# -----------------------------------------------------------------------------
# VaRTest and ESTest for analytical VaR and CVaR with t-distribution 
# -----------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Analyt_Mod(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "VaR", df = 4)[2])

CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Analyt_Mod(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "CVaR", df = 4)[2])

VarTest.dat.anat = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)

# -----------------------------------------------------------------------------
# VaRTest and ESTest for analytical VaR and CVaR with Cornish-Fisher expansion 
# -----------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Analyt_Mod(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "VaR", df = 4)[3])

CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) Analyt_Mod(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "CVaR", df = 4)[3])

VarTest.dat.anacf = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)

# -------------------------------------------------------------------------------
# VaRTest and ESTest for VaR and CVaR from Monte Carlo data (normal distribution)
# -------------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) MonteCarlo(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "VaR", sample.size = 10000,
                                     num.randWalk = 100, df = 4)$MC_VaR_norm)
 
CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) MonteCarlo(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "CVaR", sample.size = 10000,
                                     num.randWalk = 100, df = 4)$MC_CVaR_norm)

VarTest.dat.mcnorm = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)

# -----------------------------------------------------------------------------
# VaRTest and ESTest for VaR and CVaR from Monte Carlo data (t-distribution)
# -----------------------------------------------------------------------------

VaR  = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) MonteCarlo(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "VaR", sample.size = 10000,
                                     num.randWalk = 100, df = 4)$MC_VaR_t)

CVaR = sapply(1:(length(input.pf[, 1]) - 201), 
              function(i) MonteCarlo(a = 0.95, 
                                     input.pf = input.pf[i:(i + 200), ], 
                                     "CVaR", sample.size = 10000,
                                     num.randWalk = 100, df = 4)$MC_CVaR_t)

VarTest.dat.mct = cTestData(VaR = VaR, CVaR = CVaR, actPnL = PnL)
