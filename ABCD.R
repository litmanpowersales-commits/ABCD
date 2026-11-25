############################################################
# PRACTICAL 1
############################################################
# AIM: To construct and study the Xbar-chart and R-chart for the given data (with
# sample size 5 and 25 samples) and to examine whether the process mean and
# variability are under statistical control.
# PROBLEM: Construct control charts for mean and range for the following
# data on the basis of rules. Sample of size 5 is taken every hour. Comment
# whether the production process is under control or not.
# 
# 
#            Subgroup        X1           X2           X3           X4            X5
#                1           20           22           21           23            22
#                2           19           18           22           20            20
#                3           25           18           20           17            22
#                4           20           21           22           21            21
#                5           19           24           23           22            20
#                6           22           20           18           18            19
#                7           18           20           19           19            20
#                8           20           18           23           29            21
#                9           21           20           24           23            22
#               10           21           19           20           20            20
#               11           20           20           23           22            20
#               12           22           21           20           22            23
#               13           19           22           19           18            19
#               14           20           21           22           21            22
#               15           20           24           24           23            23
#               16           21           20           24           20            21
#               17           20           18           18           20            20
#               18           20           24           22           23            23
#               19           20           19           23           20            19
#               20           22           21           21           24            22
#               21           23           22           22           20            22
#               22           21           19           18           17            19
#               23           21           24           24           23            23
#               24           20           22           21           21            20
#               25           19           20           21           21            22
# 
# 
# 
# 
#   THEORY:
#   For the given data the mean and standard deviation of the process is not
#   given so it is to be estimated. The mean can be estimated by Xbar and
#   standard deviation can be estimated by Rbar.
#          Xbarbarbarbar = Sigma Xbarbar  / Total Number of Samples
#          Rbarbar = Sigma R  / Total Number of Samples
# 
# 1. The control limits for Xbar-chart are given by:
# 
#         Central Line (C.L.) = Xbarbarbarbar
#         Upper Control Limit = Xbarbarbarbar + A2 Rbarbar
#         Lower Control Limit = Xbarbarbarbar - A2 Rbarbar
# 2. The control limits for R-chart are given by:
# 
#        Central Line (C.L.) =
#        Rbarbar Upper Control Limit
#        = D4 Rbarbar Lower Control
#        Limit = D3 Rbarbar
# Here, A2, D3, D4 are constants that depend on sample size n (=5). It is given that for n=5, A2 =
# 0.577, D3 = 0, D4 =
# 2.114.

# R CODE

# Practical 1
data=matrix(c(20,22,21,23,22,19,18,22,20,20,25,18,20,17,22,20,21,22,21,21,19,24,23,22,20,22,20,18,18,19,18,20,19,19,20,20,18,23,29,21,21,20,24,23,22,21,19,20,20,20,20,20,23,22,20,22,21,20,22,23,19,22,19,18,19,20,21,22,21,22,20,24,24,23,23,21,20,24,20,21,20,18,18,20,20,20,24,22,23,23,20,19,23,20,19,22,21,21,24,22,23,22,22,20,22,21,19,18,17,19,21,24,24,23,23,20,22,21,21,20,19,20,21,21,22),ncol=5,byrow=TRUE)
# Calculate sample means and ranges
sample_means = rowMeans(data)
sample_ranges = apply(data, 1, function(x) diff(range(x)))
print(sample_means)
print(sample_ranges)

A2 = 0.577
D3 = 0
D4 = 2.114
Xbarbar = mean(sample_means)
Rbar = mean(sample_ranges)
UCL_X = Xbarbar + A2 * Rbar
LCL_X = Xbarbar - A2 * Rbar
UCL_R = D4 * Rbar
LCL_R = D3 * Rbar
print(c(Xbarbar=Xbarbar, Rbar=Rbar, UCL_X=UCL_X, LCL_X=LCL_X,
        UCL_R=UCL_R, LCL_R=LCL_R))

plot(sample_means, type="b", pch=19, main="X-bar Control Chart",
     xlab="Sample Number", ylab="Sample Mean",
     ylim=range(sample_means, LCL_X, UCL_X))
abline(h=Xbarbar, lwd=2)
abline(h=c(UCL_X,LCL_X), lwd=2, lty=2)

plot(sample_ranges, type="b", pch=19, main="R Control Chart",
     xlab="Sample Number", ylab="Sample Range",
     ylim=range(sample_ranges, LCL_R, UCL_R))
abline(h=Rbar, lwd=2)
abline(h=c(UCL_R,LCL_R), lwd=2, lty=2)

# Using QCC
if (requireNamespace("qcc", quietly = TRUE)) {
  r_chart = qcc::qcc(data, type = "R", plot = TRUE)
  xbar_chart = qcc::qcc(data, type = "xbar", plot = TRUE)
} else {
  message("Base R charts completed. Install qcc for the additional qcc charts.")
}


# Results:
# 1. The mean of sample means, Xbarbar = 20.928 and the mean of range of samples, Rbar =
#    3.72.
# 
# 2. The upper control limit and lower control limits for Xbar-chart and R-chart are as
#    follows:
# 
#      Control Charts               UCL                Central Line               LCL
#         Xbar-chart               23.07444                20.928                 18.78156
#         R-chart                 7.86408                 3.72                      0
# 
# 
# 
# Conclusions:
# 1. In Xbar-control chart, no sample mean point lies outside the upper and lower
# control limits. The points fluctuate around the central line without any
# systematic pattern. Hence, the process is under statistical control with respect
# to its mean.
# 
# 2. In R-control chart, 2 sample points lie outside the upper control limit. These
# are sample 3 and sample 8. Hence, the process variability is not under
# statistical control.
# 
# 3. Hence, we say that the process is out of control. The sample points that are
# out of control need to be removed and the charts should be plotted again until
# we get a process which is in statistical control.
# 
# Name: Shivangi Barolia
# 
# Group: I



############################################################
# PRACTICAL 2
############################################################
# AIM: To construct 3sigma modified control limits and draw the control charts.
# 
# PROBLEM: Construct the modified control charts for nitrogen where content of a certain
# fertilizer mix is a characteristic of interest. A random sample of size 5 are selected and a percentage
# of nitrogen content is found for each and by using that the sample mean X-bar and range are given
# for 20 samples. The specification limits allow values from 12-33%. A process non-conformance
# rate of more than 1% is unacceptable. Construct the the range chart and 3sigma modified control chart.
# Also draw the control chart and verify if the process mean is in statistical control or not. . A Type-I
# error rate of alpha = 0.0026 is specified .
# Xbar values: 14.8, 15.2, 16.7, 15.5, 18.4, 17.6, 21.4, 20.5, 22.8, 16.9, 25.0, 16.4, 18.6, 23.9, 17.2,
# 16.8, 21.1, 19.5, 18.3, 20.2
# 
# R values: 2.2, 1.6, 1.8, 2.0, 1.8, 1.9, 2.2, 2.3, 2.5, 1.8, 2.1, 1.8, 1.5, 2.3, 2.1, 1.6, 2.0, 2.2, 1.8, 2.3
# 
# THEORY:
# R chart: The R-Chart measures the variability within samples by plotting the range of each
# sample. The range is defined as the difference between the maximum and minimum observations
# in a sample.
# 
# Average Range:
# 
# Estimated Process Standard Deviation:
# 
# Control Limits:
# UCL= D4Rbar
# CL = Rbar
# LCL = D3Rbar
# 
# where D3, D4 and d2 are control chart constants that depend on the sample size.
# 
# If all plotted ranges lie within these limits and exhibit no unusual pattern, the process variability is
# considered to be under statistical control.
# 
# Modified X bar chart: The Xbar -Chart is used to monitor the process mean. Unlike the
# conventional Xbar -Chart, the Modified Xbar -Chart incorporates specification limits, the allowable non-
# conformance rate, and the Type-I error probability.
# 
# Let:
# USL = Upper Specification Limit
# LSL = Lower Specification Limit
# n = Sample Size
# sigma = Estimated Process Standard Deviation
# delta = Allowable Fraction Non-Conforming
# alpha = Type-I Error Probability
# 
# The standard normal deviates are:
# 
# 
# 
# 
# Modified Control Limits:
# 
# 
# 
# 
# These limits are designed so that the probability of accepting a process that does not meet the
# required conformance level is minimized while maintaining the specified Type-I error rate.
# 
# Interpretation:
# 
#    1. The R-Chart is examined first to ensure that process variability is in control.
#    2. If the variability is in control, the Modified Xbar -Chart is used to assess if the process mean
#       is in control.
#    3. Any point lying outside the control limits indicates a possible shift in the process and
#       requires investigation.
#    4. If all sample points lie within the control limits and no systematic pattern is observed, the
#       process is considered statistically in control.
# 
# Thus, the combined use of the R-Chart and Modified Xbar -Chart provides a method for monitoring
# both process variability and process centring while ensuring compliance with specified quality
# requirements.

# R CODE

# Practical 2
# Practical 2
# Sample Mean and Range
X_bar<- c(14.8, 15.2, 16.7, 15.5, 18.4 ,17.6, 21.4, 20.5, 22.8, 16.9, 25.0, 16.4, 18.6,23.9, 17.2 ,16.8, 21.1, 19.5, 18.3, 20.2)
R<-c(2.2,1.6,1.8,2.0,1.8,1.9,2.2,2.3,2.5,1.8,2.1,1.8,1.5,2.3,2.1,1.6,2,2.2,1.8,2.3)
# Mean of R
R_bar<-mean(R)
R_bar
# R Control Chart
D3=0
D4=2.114
UCL=D4*R_bar
UCL
CL=R_bar
CL
LCL=D3*R_bar
LCL
# Plotting R Chart
plot (R, type="o", pch=16, col="blue", ylim=c(min(LCL,R),max(UCL,R)), main="R Control Chart", xlab="Sample", ylab="Range")
abline(h=CL, col="green", lwd=2)
abline(h=UCL, col="red", lty=2, lwd=2)
abline(h=LCL, col="red", lty=2, lwd=2)

# Sigma
d2=2.326
sigma=R_bar/d2
sigma
# Modified Control Chart
alpha=0.0026
del=0.01
USL=33
LSL=12
z_alpha=2.79
z_del=2.33
n=20
USL_m=USL-(z_del-(z_alpha/sqrt(n))*sigma)
USL_m
LSL_m=LSL+(z_del-(z_alpha/sqrt(n))*sigma)
LSL_m
mu=mean(X_bar)
mu
# Plot
plot (X_bar, type="o", pch=16, col="blue", ylim=c(min(LSL_m,X_bar),max(USL_m,X_bar)), main="Modified Control Chart", xlab="Sample", ylab="Range")
abline(h=mu, col="green", lwd=2)
abline(h=USL_m, col="red", lty=2, lwd=2)
abline(h=LSL_m, col="red", lty=2, lwd=2)




# RESULTS:
# 
# The average range of the samples was found to be and the estimated process standard deviation
# was .
# 
# The standard normal values corresponding to the specified parameters were obtained as and
# 
# The control limits for the R-Chart were calculated as:
# 
#        UCL = 4.20686
# 
#        CL = 1.9900
# 
#        LCL = 0.0000
# 
# The modified control limits for the Xbar -Chart were found to be:
# 
#        UCL = 31.20374
# 
#        LCL = 13.79626
# 
# All sample ranges were within the R-Chart control limits, and all sample mean values were within
# the modified Xbar -Chart control limits. No sample was found to be outside the specified control
# boundaries.
# 
# CONCLUSION: The R-Chart indicated that the process variability was stable and under statistical
# control. The Modified Xbar -Chart showed that all sample means were within the calculated control
# limits. Therefore, there is no evidence of any assignable cause of variation affecting the process.
# Hence, it can be concluded that the fertilizer manufacturing process is operating under statistical
# control and satisfies the specified quality and conformance requirements. The process mean and
# variability are both stable, indicating consistent production quality



############################################################
# PRACTICAL 3
############################################################
# AIM: To construct an Acceptance Control Chart.
# PROBLEM: The nitrogen content of a certain fertiliser mix is the characteristic of interest. A random
# sample of size 5 is selected, and the percentage of nitrogen content is found for each; the sample mean and
# range are given for 20 samples in the table below. The specification limits for allowable nitrogen content
# are 12% (LSL) and 33% (USL). A process nonconformance rate above 1% is unacceptable. Suppose that
# the proportion of non-conforming at which the process must be rejected is 3% (i.e. ) and we want to detect
# an out-of-control condition with probability 0.95 (i.e. ). Construct the Acceptance Control Chart and also
# construct the range chart.
# Xbar values: 14.8, 15.2, 16.7, 15.5, 18.4, 17.6, 21.4, 20.5, 22.8, 16.9, 25.0, 16.4, 18.6, 23.9, 17.2, 16.8, 21.1,
# 19.5, 18.3, 20.2
# R values: 2.2, 1.6, 1.8, 2.0, 1.8, 1.9, 2.2, 2.3, 2.5, 1.8, 2.1, 1.8, 1.5, 2.3, 2.1, 1.6, 2.0, 2.2, 1.8, 2.3
# THEORY:
# R chart: The R-Chart measures the variability within samples by plotting the range of each sample. The
# range is the difference between the maximum and minimum observation in a sample.
# Average Range:
# Estimated Process Standard Deviation:
# Control Limits:
# UCL = D4Rbar
# CL = Rbar
# LCL = D3Rbar
# where D3, D4 and d2 are control chart constants that depend on the sample size.
# If all plotted ranges lie within these limits and exhibit no unusual pattern, the process variability is
# considered to be under statistical control.
# Acceptance Control Chart: An Acceptance Control Chart is a statistical quality control chart used to
# determine whether a process is operating at an acceptable level when specification limits and an allowable
# proportion of non-conforming products are specified. Unlike the modified Xbar -Chart, which is designed
# around the Type-I error probability alpha, the acceptance control chart is designed around the Type-II error
# probability beta, i.e. around the risk of failing to detect a process whose fraction non-conforming has become
# unacceptably large.
# 
# Let:
# USL = Upper Specification Limit
# LSL = Lower Specification Limit
# n = Sample Size
# sigma = Estimated Process Standard Deviation
# gamma = Rejectable Fraction Non-Conforming
# beta = Type-II Error Probability (probability of not detecting the shift)
# The standard normal deviates are:
# 
# 
# 
# 
# Acceptance Control Limits:
# 
# These limits are placed so that if the process fraction non-conforming reaches the rejectable level
# gamma, the chart will signal with probability . The subtraction of the term pulls the limits inward
# relative to the modified chart, making the chart more sensitive to a deteriorating process.
# Interpretation:
#     1. The R-Chart is examined first to ensure that process variability is in control, since the
#        estimate of sigma used in the acceptance limits depends on Rbar .
# 
#     2. If the variability is in control, the Acceptance Control Chart is used to assess whether the
#        process mean is positioned so that the fraction non-conforming stays within the
#        acceptable level.
# 
#     3. Any point lying outside the acceptance control limits indicates that the process fraction
#        non-conforming may have reached the rejectable level and requires investigation.
# 
#     4. If all sample points lie within the control limits and no systematic pattern is observed, the
#        process is considered to be operating at an acceptable level.
# 
# Thus, the combined use of the R-Chart and the Acceptance Control Chart provides a method for
# monitoring both process variability and process centring while controlling the risk of accepting a
# process that does not meet the specified conformance requirement.

# R CODE

# Practical 3
xbar = c(14.8,15.2,16.7,15.5,18.4,17.6,21.4,20.5,22.8,16.9,
         25.0,16.4,18.6,23.9,17.2,16.8,21.1,19.5,18.3,20.2)

R = c(2.2,1.6,1.8,2.0,1.8,1.9,2.2,2.3,2.5,1.8,
      2.1,1.8,1.5,2.3,2.1,1.6,2.0,2.2,1.8,2.3)

sample = 1:20

n = 5
LSL = 12
USL = 33

gamma = 0.03
beta  = 0.05

# Control chart constants for n = 5
d2 = 2.326
D3 = 0
D4 = 2.114
# Calculate average range
Rbar = mean(R)
cat("Average Range =", Rbar, "\n")
# Estimate sigma
sigma = Rbar/d2
cat("Estimated sigma =", sigma, "\n")
# Calculate z values
z_gamma = qnorm(1-gamma)
z_beta  = qnorm(1-beta)

cat("z_gamma =", z_gamma, "\n")
cat("z_beta  =", z_beta, "\n")
# R-chart limits
R_UCL = D4*Rbar
R_CL  = Rbar
R_LCL = D3*Rbar

cat("\nR-chart limits:\n")
cat("UCL =", R_UCL, "\n")
cat("CL  =", R_CL, "\n")
cat("LCL =", R_LCL, "\n")
# Plot R-chart
plot(sample, R, type="o", pch=19, col="blue", lwd=2,
     xlab="Sample Number", ylab="Range",
     main="R Control Chart", ylim=c(0,4.5))

abline(h=R_UCL, col="red",   lwd=2, lty=2)
abline(h=R_CL,  col="black", lwd=2)
abline(h=R_LCL, col="red",   lwd=2, lty=2)

legend("topright", legend=c("Sample R values","UCL/LCL","Center Line"),
       col=c("blue","red","black"), lty=c(1,2,1), pch=c(19,NA,NA), lwd=2)

# Acceptance control chart limits
A_UCL = USL - z_gamma*sigma - (z_beta*sigma/sqrt(n))
A_LCL = LSL + z_gamma*sigma + (z_beta*sigma/sqrt(n))
A_CL  = (A_UCL + A_LCL)/2

cat("\nAcceptance control chart limits:\n")
cat("UCL =", A_UCL, "\n")
cat("CL  =", A_CL, "\n")
cat("LCL =", A_LCL, "\n")
# Print all results
results = data.frame(
  Quantity = c("R-bar","Sigma","z-gamma","z-beta","R UCL","R CL","R LCL",
               "Acceptance UCL","Acceptance CL","Acceptance LCL"),
  Value = c(Rbar, sigma, z_gamma, z_beta, R_UCL, R_CL, R_LCL,
            A_UCL, A_CL, A_LCL))

print(results)
# Plot Acceptance Control Chart
plot(sample, xbar, type="o", pch=19, col="blue", lwd=2,
     xlab="Sample Number", ylab="Sample Mean Nitrogen (%)",
     main="Acceptance Control Chart", ylim=c(10,35))

# Acceptance control limits
abline(h=A_UCL, col="red", lwd=2, lty=2)
abline(h=A_LCL, col="red", lwd=2, lty=2)

# Specification limits
abline(h=USL, col="purple", lwd=2, lty=3)
abline(h=LSL, col="purple", lwd=2, lty=3)

# Centre line
abline(h=A_CL, col="black", lwd=2)

legend("topright",
       legend=c("Sample means","Acceptance UCL/LCL","Specification limits","Center Line"),
       col=c("blue","red","purple","black"), lty=c(1,2,3,1),
       pch=c(19,NA,NA,NA), lwd=2, cex=0.75)

# Identify points outside acceptance limits
out_of_control = which(xbar > A_UCL | xbar < A_LCL)

cat("\nSamples outside acceptance control limits:\n")
if (length(out_of_control)==0) {
  cat("None - all sample means are within the acceptance limits.\n")
} else {
  print(out_of_control)
}
# Identify points outside R-chart limits
R_out = which(R > R_UCL | R < R_LCL)

cat("\nSamples outside R-chart limits:\n")
if (length(R_out)==0) {
  cat("None - process variability is in control.\n")
} else {
  print(R_out)
}


# RESULTS:
# The average range of the samples was found to be 1.9900 and the estimated process standard deviation was
# 0.8555.
# The standard normal values corresponding to the specified parameters were obtained as = 1.8808 and =
# 1.6449.
# The control limits for the R-Chart were calculated as:
#     -   UCL = 4.2069
# 
#     -   CL = 1.9900
# 
#     -   LCL = 0.0000
# 
# The acceptance control limits for the Xbar -Chart were found to be:
#     -   UCL = 30.7614
# 
#     -   CL = 22.5000
# 
#     -   LCL = 14.2386
# 
# All sample ranges were within the R-Chart control limits, and all sample mean values were within
# the acceptance control limits. No sample was found to be outside the specified control
# boundaries.
# CONCLUSIONS:
# The R-Chart indicated that the process variability was stable and under statistical control, so the estimate
# of sigma obtained from Rbar is valid for constructing the acceptance limits. The Acceptance Control Chart showed
# that all 20 sample means lay comfortably within the calculated limits of 14.2386 and 30.7614.
# Hence, it can be concluded that the fertilizer manufacturing process is operating at an acceptable level.
# There is no evidence that the process fraction non-conforming has reached the rejectable level of 3%, and
# the chart as designed would detect such a shift with probability 0.95. The process mean and variability are
# both stable, indicating consistent production quality within the specification limits of 12% and 33%.



############################################################
# PRACTICAL 4
############################################################
# Anish Agarwal
# AIM: To construct a Chi-Square Control Chart for a multivariate process, to find the UCL, the
# inverse variance-covariance matrix, the eigen values and eigen vectors, and to plot the control
# chart and the control ellipse (contour plot).
# PROBLEM: We are monitoring a process with quality characteristics, temperature and pressure . The
# process has been established with the following in-control mean vector and variance-covariance matrix:
# 
# 
# Now the sample vector observations on and are recorded as:
# Case 1:
# Case 2:
# Compute the chi-square test statistic, the UCL, the inverse variance-covariance matrix, the eigen values and
# eigen vectors. Plot a single control chart for both cases and also plot the contour plot.
# THEORY:
# Need for a multivariate chart: When two or more quality characteristics are monitored simultaneously
# and are correlated, plotting a separate Shewhart chart for each characteristic is misleading. Separate charts
# ignore the correlation structure, and the joint Type-I error is no longer the nominal alpha. A point may lie inside
# both individual charts and still be far from the joint in-control region. The Chi-Square Control Chart handles
# this by reducing the correlated measurements to a single distance statistic.
# Chi-Square (Hotelling) statistic: Let be a vector of observations, the known in-control mean vector and
# the known in-control variance-covariance matrix. The statistic plotted is the squared statistical distance
# (Mahalanobis distance) of the observation from the target:
# 
# 
# When the process is in control and follows a -variate normal distribution, follows a chi-square
# distribution with degrees of freedom.
# Control Limits:
# 
# 
# The lower control limit is taken as zero because the statistic is a squared distance and cannot be
# negative; only large values indicate a departure from the target.
# Inverse of the variance-covariance matrix: For ,
# 
# 
# Eigen values and eigen vectors: The eigen values of and their corresponding eigen vectors
# determine the shape and orientation of the control ellipse. The ellipse
# 
# 
# has its axes along the directions and , with half-lengths . A large ratio means a highly elongated
# ellipse, which is the geometric expression of strong correlation between the characteristics.
# Contour plot: The contour plot of the chi-square statistic over the plane shows the family of concentric
# ellipses of constant statistical distance. The contour at the level UCL is the boundary of the in-control
# 
# region: a point plotting inside it is in control and a point plotting outside it is out of control. This gives the
# same decision as the control chart, but shows which characteristic combination caused the signal, which the
# control chart alone does not.
# Interpretation:
#     5. The chi-square statistic is computed for each sample vector.
# 
#     6. Each statistic is plotted on a single control chart against the UCL.
# 
#     7. A point above the UCL indicates that the process mean vector has shifted from the target,
#        and an assignable cause must be investigated.
# 
#     8. The contour plot confirms the decision geometrically and shows the direction of the shift
#        relative to the correlation structure.

# R CODE

# Practical 4
# Process parameters
p = 2

# In-control mean vector
mu = matrix(c(50,100), nrow=2, ncol=1)

# In-control variance-covariance matrix
Sigma = matrix(c(10,20,20,50), nrow=2, ncol=2, byrow=TRUE)

# Sample observation vectors
x1 = matrix(c(54,108), nrow=2, ncol=1)   # Case 1
x2 = matrix(c(54, 92), nrow=2, ncol=1)   # Case 2

alpha = 0.001

cat("Mean vector:\n"); print(mu)
cat("\nVariance-covariance matrix:\n"); print(Sigma)
# Determinant and inverse of the variance-covariance matrix
detSigma = det(Sigma)
Sigma_inv = solve(Sigma)

cat("Determinant of Sigma =", detSigma, "\n")
cat("\nInverse variance-covariance matrix:\n")
print(Sigma_inv)
# Eigen values and eigen vectors of Sigma
e = eigen(Sigma)

cat("Eigen values:\n"); print(e$values)
cat("\nEigen vectors (columns):\n"); print(e$vectors)
# Control limits
UCL = qchisq(1-alpha, df=p)
CL  = p
LCL = 0

cat("UCL = chi-square(", alpha, ",", p, ") =", UCL, "\n")
cat("CL  =", CL, "\n")
cat("LCL =", LCL, "\n")
# Function to compute the chi-square test statistic
chisq_stat = function(x, mu, Sigma_inv) {
  d = x - mu
  as.numeric(t(d) %*% Sigma_inv %*% d)
}

chi1 = chisq_stat(x1, mu, Sigma_inv)
chi2 = chisq_stat(x2, mu, Sigma_inv)

cat("Case 1: chi-square statistic =", chi1, "\n")
cat("Case 2: chi-square statistic =", chi2, "\n")
# Print all results
chi_values = c(chi1, chi2)
results = data.frame(
  Case = c("Case 1","Case 2"),
  x1 = c(x1[1], x2[1]),
  x2 = c(x1[2], x2[2]),
  ChiSquare = chi_values,
  UCL = UCL,
  Status = ifelse(chi_values > UCL, "Out of control", "In control"))

print(results)
# Single control chart for both cases
sample = 1:2

plot(sample, chi_values, type="o", pch=19, col="blue", lwd=2,
     xlab="Sample Number", ylab="Chi-square statistic",
     main="Chi-Square Control Chart",
     xaxt="n", ylim=c(0, max(chi_values, UCL)+5))
axis(1, at=sample, labels=c("Case 1","Case 2"))

abline(h=UCL, col="red",   lwd=2, lty=2)
abline(h=CL,  col="black", lwd=2)
abline(h=LCL, col="red",   lwd=2, lty=2)

# Highlight out-of-control points
out = which(chi_values > UCL)
if (length(out) > 0) points(sample[out], chi_values[out], pch=19, col="red", cex=1.5)

text(sample, chi_values, labels=round(chi_values,3), pos=4, cex=0.8)
text(1.5, UCL, paste("UCL =", round(UCL,4)), pos=3, cex=0.8, col="red")

legend("topleft", legend=c("Chi-square values","UCL/LCL","Center Line"),
       col=c("blue","red","black"), lty=c(1,2,1), pch=c(19,NA,NA), lwd=2, cex=0.8)

# Contour plot of the chi-square statistic
g1 = seq(mu[1]-15, mu[1]+15, length.out=200)
g2 = seq(mu[2]-30, mu[2]+30, length.out=200)

z = outer(g1, g2, Vectorize(function(a,b) {
  d = c(a,b) - as.vector(mu)
  as.numeric(t(d) %*% Sigma_inv %*% d)
}))

contour(g1, g2, z, nlevels=12, col="grey50",
        xlab="x1 (Temperature)", ylab="x2 (Pressure)",
        main="Contour Plot of the Chi-Square Statistic")

# Control ellipse at the UCL level
contour(g1, g2, z, levels=UCL, col="red", lwd=3, lty=1,
        drawlabels=FALSE, add=TRUE)

points(mu[1], mu[2], pch=3, col="black", cex=1.5, lwd=2)          # target
points(x1[1], x1[2], pch=19, col="blue", cex=1.5)                 # Case 1
points(x2[1], x2[2], pch=19, col="red",  cex=1.5)                 # Case 2
text(x1[1], x1[2], "Case 1", pos=4, cex=0.8)
text(x2[1], x2[2], "Case 2", pos=4, cex=0.8)

legend("topleft", legend=c("Control ellipse (UCL)","Target mean","Case 1","Case 2"),
       col=c("red","black","blue","red"), lty=c(1,NA,NA,NA),
       pch=c(NA,3,19,19), lwd=c(3,2,NA,NA), cex=0.75)

# Axes of the control ellipse from the eigen structure
half_lengths = sqrt(e$values * UCL)

axes = data.frame(
  Component  = c("Major axis","Minor axis"),
  EigenValue = e$values,
  HalfLength = half_lengths,
  e_x1 = e$vectors[1,],
  e_x2 = e$vectors[2,])

print(axes)
# Identify out-of-control cases
cat("\nCases outside the control limit:\n")
if (length(out)==0) {
  cat("None - the process mean vector is in control for both cases.\n")
} else {
  cat("Case(s)", out, "- the process mean vector has shifted.\n")
}


# RESULTS:
# The determinant of the variance-covariance matrix was found to be 100, and the inverse variance-
# covariance matrix was obtained as:
# 
# 
# The eigen values of Sigma were found to be lambda1 = 58.2843 and lambda2 = 1.7157, with corresponding eigen
# vectors approximately (0.3827, 0.9239) and (-0.9239, 0.3827).
# The control limits for the Chi-Square Control Chart at alpha = 0.001 with p = 2 degrees of freedom were
# calculated as:
#    -   UCL = 13.8155
# 
#    -   CL = 2.0000
# 
#    -   LCL = 0.0000
# 
# The chi-square test statistics were computed as:
#    -   Case 1: chi^2 = 1.6000
# 
#    -   Case 2: chi^2 = 27.2000
# 
# Case 1 plots well below the UCL, while Case 2 plots far above it. The contour plot shows Case 1
# lying inside the control ellipse and Case 2 lying well outside it.
# CONCLUSIONS:
# The two cases have exactly the same value of , and their deviations in are equal in magnitude (108 and 92
# are both 8 units from the target of 100). On individual Shewhart charts they would therefore look equally
# acceptable. The chi-square statistic separates them completely, because it accounts for the strong positive
# correlation between temperature and pressure.
# In Case 1 both characteristics moved upward together, which is consistent with the established correlation
# structure, so the statistical distance from the target is small (chi^2 = 1.6) and the process is in control. In Case
# 2 temperature increased while pressure decreased, which contradicts the positive correlation between the
# two variables. This gives a very large statistical distance (chi^2 = 27.2), exceeding the UCL of 13.8155, and
# the process is declared out of control.
# Hence, the Chi-Square Control Chart signals an assignable cause for Case 2 and requires investigation,
# while Case 1 requires no action. This demonstrates the essential advantage of multivariate control charting:
# an out-of-control condition arising purely from a breakdown in the relationship between correlated
# characteristics is detected here, and would have been missed entirely by separate univariate control charts.
# 
# Name: Anish Agarwal
# Group - P2



############################################################
# PRACTICAL 5
############################################################
# AIM: To construct a Chi-Square Control Chart for a multivariate process with three quality
# characteristics, calculate the UCL, inverse covariance matrix, eigenvalues and eigenvectors, and plot the
# control chart and control ellipse.
# Problem: We are monitoring a process with p = 3 quality characteristics: temperature x1, pressure x2 and
# flow rate x3. The process has been established with the following in-control mean vector and variance-
# covariance matrix:
# 
# 
# 
# Now the sample vector observations on x1, x2 and x3 are recorded as:
# 
# 
# 
# Compute the chi-square test statistic, the UCL, the inverse variance-covariance matrix, the eigenvalues
# and eigenvectors. Plot a single control chart for both cases and also plot the contour plot (control ellipse).
# Theory:
# Chi-Square Statistic
# Let be a vector of observations, the known in-control mean vector, and the known in-control variance-
# covariance matrix.
# 
# The statistic plotted is the squared statistical distance (Mahalanobis distance) of the observation from the
# target:
# 
# Here, is the deviation vector, its transpose is a row vector, and is the inverse variance-covariance matrix.
# The product gives a single non-negative scalar.
# When the process is in control and follows a -variate normal distribution,
# 
# For this problem, .
# Control Limits
# 
# 
# The lower control limit is zero because the statistic is a squared distance and cannot be negative. Only
# large values indicate a departure from the target.
# Eigenvalues and Eigenvectors
# The eigenvalues and their corresponding eigenvectors satisfy:
# 
# The eigenvalues and eigenvectors determine the shape and orientation of the control region.
# For , the boundary
# 
# defines a 3-dimensional ellipsoid rather than a 2-dimensional ellipse.
# Its principal axes lie along the eigenvectors , with half-lengths:
# 
# Contour Plot for
# A single 2-D contour cannot display a 3-dimensional ellipsoid. Therefore, the control region is visualised
# using three pairwise 2-D control ellipses:
# 
#     1. : Temperature and Pressure
#     2. : Temperature and Flow-rate
#     3. : Pressure and Flow-rate
# Each ellipse is constructed using the corresponding sub-matrix of .
# A sample point lying outside the control ellipse or having
# 
# indicates an out-of-control condition.

# R CODE

# Practical 5
mu <- c(50, 100, 20)

Sigma <- matrix(c(10, 20, 5,
                  20, 50, 12,
                  5, 12, 8), 
                nrow = 3, byrow = TRUE)

p <- 3
alpha <- 0.0027

# 2. Sample Observations
samples <- rbind(
  Case1 = c(54, 108, 22),
  Case2 = c(54, 92, 16)
)

print("Mean Vector:")
print(mu)
print("Covariance Matrix:")
print(Sigma)
print("Sample Observations:")
print(samples)
# 3. Chi-Square Statistic

S_inv <- solve(Sigma)

chi_square <- function(x) {
  d <- x - mu
  t(d) %*% S_inv %*% d
}

chi_values <- apply(samples, 1, chi_square)

print("Chi-Square Statistics:")
print(round(chi_values, 4))
# 4. Control Limits
UCL <- qchisq(1 - alpha, df = p)
CL <- p
LCL <- 0

cat("UCL =", round(UCL, 4), "\n")
cat("CL  =", CL, "\n")
cat("LCL =", LCL, "\n")
# 5. Verdict
verdict <- ifelse(chi_values > UCL,
                  "OUT OF CONTROL",
                  "IN CONTROL")

summary <- data.frame(
  Sample = rownames(samples),
  Chi_Square = round(chi_values, 4),
  UCL = round(UCL, 4),
  Verdict = verdict
)

print(summary)
# 6. CHI-SQUARE CONTROL CHART

plot(chi_values, type = "o", pch = 19,
     col = "blue",
     ylim = c(0, 30),
     xaxt = "n",
     xlab = "Sample",
     ylab = expression(chi[0]^2),
     main = "Chi-Square Control Chart")

axis(1, at = 1:2, labels = rownames(samples))

abline(h = UCL, col = "red", lty = 2, lwd = 2)
abline(h = CL, col = "darkgreen", lty = 3)
abline(h = LCL, col = "grey40", lty = 3)

legend("topleft",
       legend = c("Chi-Square", "UCL", "CL", "LCL"),
       col = c("blue", "red", "darkgreen", "grey40"),
       lty = c(1, 2, 3, 3),
       pch = c(19, NA, NA, NA),
       bty = "n")

# 7. CONFIDENCE ELLIPSES

draw_ellipse <- function(i, j) {
  
  # Select two variables
  S <- Sigma[c(i, j), c(i, j)]
  center <- mu[c(i, j)]
  
  # Eigenvalues and eigenvectors
  e <- eigen(S)
  
  # Semi-axis lengths
  axes <- sqrt(e$values * UCL)
  
  # Points on ellipse
  theta <- seq(0, 2*pi, length.out = 200)
  
  circle <- rbind(
    axes[1] * cos(theta),
    axes[2] * sin(theta)
  )
  
  ellipse <- e$vectors %*% circle
  
  x <- center[1] + ellipse[1, ]
  y <- center[2] + ellipse[2, ]
  
  # Plot
  plot(x, y, type = "l", col = "blue", lwd = 2,
       xlab = paste0("X", i),
       ylab = paste0("X", j),
       main = paste("Ellipse: X", i, "vs X", j),
       asp = 1)
  
  # Mean point
  points(center[1], center[2],
         pch = 3, col = "darkgreen", cex = 1.5)
  
  # Sample points
  points(samples[, i], samples[, j],
         pch = 19, col = "black")
  
  text(samples[, i], samples[, j],
       labels = rownames(samples),
       pos = 3)
}


# Draw all three ellipses
par(mfrow = c(1, 3))

draw_ellipse(1, 2)
draw_ellipse(1, 3)
draw_ellipse(2, 3)

par(mfrow = c(1, 1))


# RESULTS:
# 
# The determinant of the variance-covariance matrix was found to be , and the inverse variance-
# covariance matrix was obtained as:
# 
# 
# 
# The eigenvalues of were found to be , , and , with corresponding eigenvectors:
# 
# 
# 
# 
# Chi-Square Test Statistics
# 
# Case 1:
# 
# 
# 
# 
# Case 2:
# 
# Control Limits
# 
# For and degrees of freedom:
# 
# 
# 
# Summary of Chi-Square Test Statistics
# 
#    Case                                 Statistic    UCL         Status
# 
#    Case 1        54   108     22        1.6000      16.2662     In control
# 
#    Case 2        54   92      16        28.7373     16.2662   Out of control



############################################################
# PRACTICAL 6
############################################################
# AIM: Construct Hotelling's T2 Control Chart & Contour Plot
# 
# PROBLEM: Using the given data in which p=2, m=20, n=4, alpha= 0.0054, also plot hotelling T2
# control chart and contour plot.
# 
#                                   X1                                   X2
#   Sample
#      s            1           2         3      4         1         2         3         4
#      1           80          82        78     85        19        22        20        20
#      2           75          78        84     81        24        21        18        21
#      3           83          86        84     87        19        24        21        22
#      4           79          84        80     83        18        20        17        16
#      5           82          81        78     86        23        21        18        22
#      6           86          84        85     87        21        20        23        21
#      7           84          88        82     85        19        23        19        22
#      8           76          84        78     82        22        17        19        18
#      9           85          88        85     87        18        16        20        16
#     10           80          78        81     83        18        19        20        18
#     11           86          84        85     86        23        20        24        22
#     12           81          81        83     82        22        21        23        21
#     13           81          86        82     79        16        18        20        19
#     14           75          78        82     80        22        21        23        22
#     15           77          84        78     85        22        19        21        18
#     16           86          82        84     84        19        23        18        22
#     17           84          85        78     79        17        22        18        19
#     18           82          86        79     83        20        19        23        21
#     19           79          88        85     83        21        23        20        18
#     20           80          84        82     85        18        22        19        20
# 
# 
# THEORY:
# 
# 1. Hotelling's T2 Control Chart
# 
# Hotelling's T2 control chart monitors multiple correlated quality characteristics simultaneously
# and identifies subgroups whose mean vectors differ significantly from the overall process mean.
# 
# 2.Hotelling's T2 Statistic
# 
# Hotelling's T2 statistic measures the multivariate distance between a subgroup mean vector and
# the overall mean, considering both variability and correlation.
# 
# 3.Control Chart
# 
# A control chart plots the calculated T2 statistics against subgroup numbers along with the UCL
# to visually identify observations exceeding the control boundary.
# 
# 4. Contour Plot
# 
# The contour plot represents the multivariate control region for two variables. Its elliptical
# boundary shows combinations of variable means corresponding to the control limit.

# R CODE

# Practical 6
# Hotelling T^2 Control Chart
# p = 2, m = 20 subgroups, n = 4 observations/subgroup
# DATA 
X1 <- matrix(c(
  80,82,78,85, 75,78,84,81, 83,86,84,87, 79,84,80,83,
  82,81,78,86, 86,84,85,87, 84,88,82,85, 76,84,78,82,
  85,88,85,87, 80,78,81,83, 86,84,85,86, 81,81,83,82,
  81,86,82,79, 75,78,82,80, 77,84,78,85, 86,82,84,84,
  84,85,78,79, 82,86,79,83, 79,88,85,83, 80,84,82,85
), ncol=4, byrow=TRUE)

X2 <- matrix(c(
  19,22,20,20, 24,21,18,21, 19,24,21,22, 18,20,17,16,
  23,21,18,22, 21,20,23,21, 19,23,19,22, 22,17,19,18,
  18,16,20,16, 18,19,20,18, 23,20,24,22, 22,21,23,21,
  16,18,20,19, 22,21,23,22, 22,19,21,18, 19,23,18,22,
  17,22,18,19, 20,19,23,21, 21,23,20,18, 18,22,19,20
), ncol=4, byrow=TRUE)

# SUBGROUP MEANS 
xbar1 <- rowMeans(X1)
xbar2 <- rowMeans(X2)

xbar <- c(mean(xbar1), mean(xbar2))
xbar

#WITHIN-SUBGROUP COVARIANCE
S_list <- lapply(1:20, function(i) {
  cov(cbind(X1[i,], X2[i,]))
})

S <- Reduce("+", S_list) / 20
S

# HOTELLING T2 
T2 <- numeric(20)

for(i in 1:20) {
  d <- c(xbar1[i], xbar2[i]) - xbar
  T2[i] <- 4 * t(d) %*% solve(S) %*% d
}

T2 <- as.numeric(T2)

# CONTROL LIMIT 
p <- 2
m <- 20
n <- 4
alpha <- 0.0054

df1 <- p
df2 <- m*(n-1) - p + 1

UCL <- (p*(m-1)*(n-1)/df2) * qf(1-alpha, df1, df2)

cat("UCL =", UCL, "\n")

#  CONTROL CHART 
plot(1:m, T2, type="o", pch=19, col="blue",
     xlab="Sample Number",
     ylab=expression(T^2),
     main="Hotelling's T^2 Control Chart")

abline(h=UCL, col="red", lwd=2, lty=2)

text(1:m, T2, labels=round(T2,2),
     pos=3, cex=0.7)

legend("topright",
       legend=c("T^2 statistic","UCL"),
       col=c("blue","red"),
       lty=c(1,2), pch=c(19,NA))

#  CONTOUR PLOT
# Mahalanobis T^2 contours for subgroup means

x <- seq(min(xbar1)-3, max(xbar1)+3, length=200)
y <- seq(min(xbar2)-3, max(xbar2)+3, length=200)

grid <- expand.grid(X1=x, X2=y)

D <- as.matrix(grid) -
  matrix(xbar, nrow=nrow(grid), ncol=2, byrow=TRUE)

Z <- 4 * rowSums((D %*% solve(S)) * D)
Z <- matrix(Z, nrow=length(x), ncol=length(y))

contour(x, y, Z,
        levels=UCL,
        lwd=2, col="red",
        xlab="X1 subgroup mean",
        ylab="X2 subgroup mean",
        main="Hotelling's T^2 Contour Plot")

points(xbar1, xbar2, pch=19, col="blue")
points(xbar[1], xbar[2], pch=4, cex=2, lwd=2)

text(xbar1, xbar2, labels=1:20,
     pos=3, cex=0.7)
legend("topright",
       legend=c("Subgroup means","Overall mean","T^2 control contour"),
       col=c("blue","black","red"),
       pch=c(19,4,NA), lty=c(NA,NA,1))





# CONCLUSION: The Phase 1 Hotelling's T2 multivariate control chart was successfully
# constructed to analyze the correlated variables of break factor and weight. The resulting control
# 
# ellipse completely enclosed all 20 sample points, with no values breaching the calculated boundary
# limit alpha=0.0054. Therefore, it is concluded that the textile manufacturing process is stable and
# operating in a state of statistical control.



############################################################
# PRACTICAL 7
############################################################
# AIM: To construct the generalised variance (|S|) control chart and its contour plot for two quality
# characteristics using R.
# Problem: Twenty subgroups, each containing four observations, are taken on two quality characteristics:
# single strand break strength (X1) and weight of textile fibre (X2). Using the data below, construct the
# generalised variance chart and the corresponding contour plot. Given , where m is the number of
# subgroups, n the observations per subgroup and p the number of quality characteristics.
#                          Subgrou X1 X1 X1 X1 X2 X2 X2 X2
#                                p
#                                1 80 82 78 85 19 22 20 20
#                                2 75 79 84 81 24 21 18 21
#                                3 83 86 84 87 17 24 21 21
#                                4 79 84 80 83 18 20 17 16
#                                5 82 81 78 86 23 21 18 22
#                                6 86 84 85 97 21 20 22 21
#                                7 94 88 92 85 19 23 19 22
#                                8 76 84 78 82 22 17 19 18
#                                9 95 88 85 87 18 16 20 16
#                               10 80 78 81 83 19 19 20 18
#                               11 88 94 85 86 23 20 24 22
#                               12 91 91 85 82 22 21 23 21
#                               13 80 86 82 79 16 19 20 19
#                               14 75 78 82 80 22 21 22 22
# Theory:
#                               15 77 90 79 85 22 19 21 18
#                               16 86 82 94 84 19 23 16 22
# Generalised                   17 84 85 78 74 17 22 18 19                       variance chart.
#                               18 82 86 79 83 20 19 23 21
# The Hotelling                                                                  chart monitors the
#                               19 79 88 95 83 21 23 20 18
# process mean                                                                   vector. The
#                               20 90 84 82 85 18 22 19 20
# generalised                                                                    variance chart
# monitors the variability of the process, based on the variance-covariance matrix. A measure of
# the generalised variance is , the determinant of the sample variance-covariance matrix.
# 
# For :
# 
# 
# 
# Mean and variance of . If and denote the mean and variance of the determinant, then:
# 
# 
#                                                    V
# 
# where Sigma is the variance-covariance matrix, and
# 
# Control limits. Most of the distribution of |S| lies within the interval Following the 3sigma control
# chart idea, the parameters of the chart are:
# 
# 
#                                                   U
# 
# 
# 
# Since Sigma is usually unknown, an unbiased estimate of | is obtained from where is the average of
# the subgroup covariance matrices:
# 
# 
# 
# If the LCL comes out negative, it is taken as 0. For the j-th sample, is computed and plotted on
# the chart. A point outside the limits shows that the process variability is out of control.
# 
# Contour plot. For depends on and the correlation r. If r is fixed at its average value, setting |S|
# equal to a constant gives a curve in the plane:
# 
# 
# 
# Taking gives the contour curves of the centre line and the upper control limit. Each subgroup is
# plotted as the point Points beyond the UCL curve show excess variability.

# R CODE

# Practical 7
m=20
n=4
p=2
x1=matrix(c(80,82,78,85,75,78,84,81,83,86,84,87,79,84,80,83,82,81,78,86,86,84,85,87,84,88,82,85,76,84,78,82,85,88,85,87,80,78,81,83,86,84,85,86,81,81,83,82,81,86,82,79,75,78,82,80,77,84,78,85,86,82,84,84,84,85,78,79,82,86,79,83,79,88,85,83,80,84,82,85),nrow=m,byrow=TRUE)
x2=matrix(c(19,22,20,20,24,21,18,21,19,24,21,22,18,20,17,16,23,21,18,22,21,20,23,21,19,23,19,22,22,17,19,18,18,16,20,16,18,19,20,18,23,20,24,22,22,21,23,21,16,18,20,19,22,21,23,22,22,19,21,18,19,23,18,22,17,22,18,19,20,19,23,21,21,23,20,18,18,22,19,20),nrow=m,byrow=TRUE)

# |S_j| of each subgroup and average covariance matrix S
detS=numeric(m)
S=matrix(0,p,p)
for (j in 1:m) {
  Sj=cov(cbind(x1[j,],x2[j,]))
  detS[j]=det(Sj)
  S=S+Sj
}
S=S/m
detS
# Constants b1 and b2
b1=prod(n-1:p)/(n-1)^p
b2=prod(n-1:p)*(prod(n-1:p+2)-prod(n-1:p))/(n-1)^(2*p)

# Estimate of |Sigma| is |S|/b1, so:
detSigma=det(S)/b1
CL=b1*detSigma
UCL=detSigma*(b1+3*sqrt(b2))
LCL=max(0,detSigma*(b1-3*sqrt(b2)))
c(b1=b1,b2=b2,detSigma=detSigma,CL=CL,UCL=UCL,LCL=LCL)
# Generalized variance chart
plot(1:m,detS,type="b",pch=19,xlab="Subgroup",ylab="|S|",main="Generalized Variance Chart",ylim=c(0,max(detS,UCL)+10))
abline(h=c(CL,UCL,LCL),lty=c(1,2,3),lwd=2)
text(m,UCL,paste("UCL"))
text(m,CL,paste("CL"))
text(m,LCL,paste("LCL"))

# Contour plot for generalized variance
s11=apply(x1,1,var)
s22=apply(x2,1,var)
r=S[1,2]/sqrt(S[1,1]*S[2,2])

g1=seq(0,max(s11)*1.2,length=200)
g2=seq(0,max(s22)*1.2,length=200)
Z=outer(g1,g2,function(a,b) a*b*(1-r^2))

contour(g1,g2,Z,levels=CL,labels=paste("CL =",round(CL,2)),labcex=0.9,lwd=2,xlab="Variance of X1 (s11)",ylab="Variance of X2 (s22)",main="Generalized Variance Contour Plot")
contour(g1,g2,Z,levels=UCL,labels=paste("UCL =",round(UCL,2)),labcex=0.9,lwd=2,lty=2,add=TRUE)
points(s11,s22,pch=19)
text(s11,s22,labels=1:m,pos=3,cex=0.7)



# RESULTS AND CONCLUSION:
# 
# Results:
# 
# The average covariance matrix of the 20 subgroups and the quantities needed for the limits were
# obtained from the R output:
# 
# The control limits of the generalised variance chart are:
# 
# 
# 
# 
# The is negative, so it is taken as 0.
# 
# Conclusion
# 
#    -   The control limits are .
#    -   All 20 values of lie between the and the. The largest is 51.72 (subgroup 19), which is
#        well below the , and the smallest is 0.50 (subgroup 12). Subgroups 17 and 19 are the
#        highest points, but both are far from the limit.
#    -   In the contour plot, every subgroup point lies inside the curve, which agrees with the
#        control chart.
#    -   Hence, the process variability is in statistical control, and no subgroup needs to be
#        removed. The estimated covariance matrix can therefore be used for the Hotelling Tchart
#        of the process means.



############################################################
# PRACTICAL 8
############################################################
# Aim: To construct a chi^2 control chart and a contour plot (control ellipse) at alpha = 0.01 for the
# bivariate quality characteristics tensile strength and diameter (Table 7-28), and to check
# whether the process is in statistical control.
# 
# Problem: Twenty samples of size n = 4 are taken, and two correlated quality characteristics are
# measured on each item: tensile strength (1000 kg) and diameter (cm). Monitoring them with two
# separate Xbar charts ignores their correlation and inflates the overall false-alarm rate. We therefore
# need a single multivariate chart, plus a contour plot of the control region, to decide whether the
# process is in control.
# 
# Theory: Let xbar   = (xbar 1 , xbar 2 )' be the vector of sample means for sample i, with p = 2 variables.
# 
#     1. Target values. muhat is the grand mean vector. Sbar is the pooled covariance matrix, the average
#        of the 20 within-sample covariance matrices. Here these are estimated from the data
#        (Phase I).
#     2. Chi-square statistic
# 
# 
# 
#         When the process is in control this follows chi^2 with p degrees of freedom.
# 
#     3. Control limits.
# 
#         UCL = = = 9.210, and LCL = 0.
# 
#         A point with > UCL signals out-of-control.
# 
#     4. Control ellipse (contour). The set of points with
# 
#                                           n (xbar - muhat)' Sbar    (xbar - muhat) =
# 
#         is an ellipse centred at muhat. A sample mean outside this ellipse is out of control. The plot
#         also shows the 95% ellipse as a dashed line for reference.

# R CODE

# Practical 8
# Data
TS <- matrix(c(66,70,68,72, 75,60,70,75, 65,70,70,65, 72,70,75,65, 73,74,72,70,
               72,74,73,74, 63,62,65,66, 75,84,75,66, 65,69,77,71, 70,68,67,67,
               80,75,70,69, 68,65,80,50, 74,80,76,74, 76,74,75,73, 71,70,74,73,
               68,67,70,69, 72,76,75,77, 76,74,75,77, 72,74,73,75, 72,68,74,70),
             ncol = 4, byrow = TRUE)
DI <- matrix(c(16,18,15,20, 17,22,18,19, 20,18,15,18, 19,20,15,17, 21,21,23,19,
               21,19,20,18, 22,20,24,22, 22,20,20,22, 18,16,18,18, 18,17,19,18,
               24,18,20,22, 20,21,20,22, 19,17,20,21, 20,17,18,18, 18,16,17,18,
               18,16,19,20, 22,19,23,20, 19,23,20,21, 20,18,20,19, 21,19,18,20),
             ncol = 4, byrow = TRUE)

m <- 20; n <- 4; p <- 2; alpha <- 0.01

# Sample means and grand mean
xbar <- cbind(rowMeans(TS), rowMeans(DI))
mu   <- colMeans(xbar)

# Pooled covariance matrix
S <- matrix(0, 2, 2)
for (i in 1:m) S <- S + cov(cbind(TS[i, ], DI[i, ]))
S <- S / m
Sinv <- solve(S)

# Chi-square statistic and UCL
chi2 <- apply(xbar, 1, function(x) n * t(x - mu) %*% Sinv %*% (x - mu))
UCL  <- qchisq(1 - alpha, df = p)
print(mu); print(S); print(UCL)
print(data.frame(Sample = 1:m, Chi2 = round(chi2, 3), Out = chi2 > UCL))

# Chi-square control chart
plot(1:m, chi2, type = "o", pch = 16, xlab = "Sample", ylab = expression(chi[0]^2),
     main = "Chi-square control chart (alpha = 0.01)", ylim = c(0, max(chi2, UCL) + 1))
abline(h = UCL, col = "red", lty = 2)
points(which(chi2 > UCL), chi2[chi2 > UCL], col = "red", pch = 16, cex = 1.5)
text(m - 2, UCL + 0.5, paste("UCL =", round(UCL, 3)), col = "red")

# Contour plot (control ellipse)
x <- seq(min(xbar[,1]) - 3, max(xbar[,1]) + 3, length = 200)
y <- seq(min(xbar[,2]) - 2, max(xbar[,2]) + 2, length = 200)
z <- outer(x, y, Vectorize(function(a, b) {
  d <- c(a, b) - mu; n * t(d) %*% Sinv %*% d }))
contour(x, y, z, levels = c(qchisq(0.95, 2), UCL), labels = c("95%", "99%"),
        col = c("green", "red"), xlab = "Mean tensile strength", ylab = "Mean diameter",
        main = "Control ellipse at alpha = 0.01")
points(xbar, pch = 16, col = ifelse(chi2 > UCL, "red", "blue"))
text(xbar, labels = 1:m, pos = 3, cex = 0.7)
points(mu[1], mu[2], pch = 3)


# Conclusion
# 
#   1. At alpha = 0.01, the chi-square control chart and the control ellipse give the same result. The
#      process is not in statistical control, because sample 7 is out of control.
#   2. Sample 7 is unusual because its mean tensile strength is low (64) while its mean diameter
#      is high (22). This combination is inconsistent with the rest of the data.
#   3. Since 19 of the 20 samples are in control, the out-of-control signal looks like it comes
#      from a single assignable cause. Possible causes are a material problem or a machine
#      setting problem.
#   4. Corrective action: investigate and remove the cause for sample 7, then discard it and
#      recompute muhat, Sbar and the UCL from the remaining 19 samples. If all revised points then fall
#      inside the limits, the revised chart can be used for future monitoring (Phase II).
#   5. Samples 8 and 17 are borderline, so they should be watched in future monitoring.



############################################################
# PRACTICAL 9
############################################################
# AIM:
# 
# Problem:
# 
#  A sequential sampling plan is to be used. The producer's risk is alpha = 0.05 at AQL p1 = 0.008,
# and the consumer's risk is beta = 0.07 at LQL p2 = 0.082. Determine:
# 
#     -   the equations of the acceptance and rejection lines,
#     -   the first opportunity to reject.
#     -   the first opportunity to accept.
#     -   plot the graph.
# 
# Theory:
# 
# Items are inspected one at a time. After n items with d defectives, the Sequential Probability
# Ratio Test compares the likelihood ratio with two boundaries, and
# 
#     -   If the ratio is <= A, accept the lot.
#     -   If the ratio is >= B, reject the lot.
#     -   Otherwise, continue sampling.
# 
# This gives two parallel straight lines in the (n, d) plane:
# 
#     -   Acceptance line:
#     -   Rejection line
# 
# where
# 
#     -
#     -
#     -
#     -
# 
# The decision rule is:
# 
#     -   -> accept
#     -   -> reject
#     -   -> take another item

# R CODE

# SPRT sequential sampling plan (attributes)
alpha <- 0.05; beta <- 0.07
p1 <- 0.008; p2 <- 0.082

k <- log((p2*(1-p1)) / (p1*(1-p2)))
ha <- log((1-alpha)/beta) / k
hr <- log((1-beta)/alpha) / k
s <- log((1-p1)/(1-p2)) / k

cat("k =", k, "\nha =", ha, "\nhr =", hr, "\ns =", s, "\n")
cat(sprintf("Acceptance line: da = -%.4f + %.4f n\n", ha, s))
cat(sprintf("Rejection line: dr = %.4f + %.4f n\n", hr, s))

# first opportunity to reject (all defective: d = n) and to accept (d = 0)
n_rej <- ceiling(hr/(1 - s))
n_acc <- ceiling(ha/s)
cat("First reject at n =", n_rej, "\nFirst accept at n =", n_acc, "\n")

# Plot
n <- 0:60
da <- -ha + s*n
dr <- hr + s*n

plot(n, dr, type = "l", col = "red", lwd = 2,
   ylim = c(-2, 5), xlab = "Number of items inspected (n)",
   ylab = "Cumulative defectives (d)",
   main = "Sequential Sampling Plan (SPRT)")
lines(n, da, col = "blue", lwd = 2)
abline(h = 0, lty = 3)
polygon(c(n, rev(n)), c(da, rev(dr)), col = rgb(0.5,0.5,0.5,0.15), border = NA)
points(n_acc, 0, pch = 19, col = "blue")
points(n_rej, n_rej, pch = 19, col = "red")
text(45, 4.2, "REJECT region", col = "red")
text(45, -1.2, "ACCEPT region", col = "blue")
text(30, 1.2, "CONTINUE\nSAMPLING")
legend("topleft", c("Rejection line", "Acceptance line"),
     col = c("red", "blue"), lwd = 2)


# Conclusions:
# 
#    1. The sequential plan with alpha = 0.05 at AQL = 0.008 and beta = 0.07 at LQL = 0.082 gives two
#       parallel lines with slope 0.0322, separated by h  + h  = 2.3 defectives.
#    2. A very bad lot can be rejected after only 2 items, while a lot must pass at least 34 defect-
#       free items before it can be accepted.
#    3. The graph shows three regions: reject (above the red line), accept (below the blue line),
#       and continue sampling (the shaded band between them).
#    4. Compared with a single sampling plan with the same alpha and beta, the sequential plan usually
#       needs a smaller average sample size. Very good or very bad lots are decided early, and
#       only borderline lots need many inspections.



############################################################
# PRACTICAL 10
############################################################
# AIM: To determine the parameters of a variables sampling plan and construct its OC curve,
# showing AQL and LTPD, using R.
# 
# Problem: The LSL for the breaking strength of yarn is 25 g. The breaking strength is normally
# distributed with variance 6. It is desired that a lot with a mean such that 3% of the product is
# non-conforming be accepted 94% of the time, while a lot with a mean such that 8% of the
# product is non-conforming should be accepted 7% of the time. Find the parameters of a
# variables sampling plan, describe its operation, and plot the OC curve with AQL and LTPD.
# 
# Theory
# In variables acceptance sampling, a measurable quality characteristic is observed on a random
# sample, and the decision is made using the sample mean. Here, breaking strength X is assumed
# normally distributed with known variance sigma^2 = 6 and lower specification limit LSL = 25 g. A
# product is non-conforming if X < 25.
# 
# Let p be the fraction non-conforming. For a normal distribution, (mu - LSL)/sigma = z1   , where zq is
# the qth standard normal quantile. For a sample of size n, Xbar ~ N(mu, sigma^2/n). Use the lower-
# specification acceptance rule:
# 
#                Accept the lot if Xbar >= LSL + ksigma (equivalently, QL = (Xbar - LSL)/sigma >= k).
# 
# Thus the probability of lot acceptance is
# 
#                                       Pa(p) =  [sqrtn (z1   - k)].
# 
# The plan is designed using AQL p1 = 0.03 with Pa1 = 0.94 and LTPD p2 = 0.08 with Pa2 = 0.07. Let
# z1 =    (0.97), z2 =    (0.92), a1 =    (0.94), and a2 =    (0.07).
# 
#                               sqrtn = (a1 - a2)/(z1 - z2),   k = z1 - a1/sqrtn
# 
# Calculations
# -   z1 =    (0.97) = 1.88079; z2 =    (0.92) = 1.40507.
# -   a1 =    (0.94) = 1.55477; a2 =    (0.07) = -1.47579.
# -   n = [(1.55477 - (-1.47579))/(1.88079 - 1.40507)]^2 = 40.58.
# -   Taking the next integer, sample size n = 41.
# -   k = 1.88079 - 1.55477/sqrt40.58266 = 1.63673 (approximately 1.638 after rounding n to 41 and
#     retaining the AQL design).
# 
# For the final rounded plan, use n = 41 and k   1.638. The implied acceptance probability at p =
# 8% is approximately 0.0679 (about 6.8%), close to the required 7%; the AQL acceptance
# probability is 0.94 by construction.
# 
# Parameters and operation of the plan
# 1.   Sample size: n = 41 yarn specimens, selected randomly from the lot.
# 2.   Measure breaking strength for each specimen and calculate the sample mean Xbar .
# 3.   Calculate the quality statistic QL = (Xbar - 25)/sqrt6.
# 4.   Accept the lot if QL >= k   1.638; otherwise reject the lot.
# 5.   Equivalently, accept when Xbar >= 25 + 1.638sqrt6   29.01 g; reject if the sample mean is below this value.
# 
# Operating Characteristic (OC) curve
# The OC curve plots the probability of accepting a lot against the fraction non-conforming p. For
# this plan, Pa(p) =  {sqrt41[   (1 - p) - 1.638]}. Mark AQL = 3% with Pa = 0.94 and LTPD = 8% with
# target Pa = 0.07.

# R CODE

# Practical 10
LSL=25
sigma=sqrt(6)
p1=0.03
Pa1=0.94
p2=0.08
Pa2=0.07

# Calculation of parameters
z1=qnorm(1-p1)
z2=qnorm(1-p2)
a1=qnorm(Pa1)
a2=qnorm(Pa2)

n0=((a1-a2)/(z1-z2))^2
n=ceiling(n0)
k=z1-a1/sqrt(n)

n
k
# Acceptance limit
Xbar=LSL+k*sigma
Xbar
# OC Curve
p=seq(0.001,0.20,length=200)
Pa=pnorm(sqrt(n)*(qnorm(1-p)-k))

plot(100*p,Pa,type="l",lwd=2,
     xlab="Percent non-conforming",
     ylab="Probability of acceptance",
     main="OC Curve for Variables Sampling Plan",
     ylim=c(0,1))

# AQL and LTPD points
Pa.AQL=pnorm(sqrt(n)*(qnorm(1-p1)-k))
Pa.LTPD=pnorm(sqrt(n)*(qnorm(1-p2)-k))

points(c(3,8),c(Pa.AQL,Pa.LTPD),pch=19)

# Reference lines
abline(v=c(3,8),lty=2)
abline(h=c(Pa1,Pa2),lty=3)

# Labelling
text(3,Pa.AQL,"AQL",pos=4)
text(8,Pa.LTPD,"LTPD",pos=4)




# Results and conclusion
# The calculated sample size before rounding is 40.58, so a sample of 41 units is selected. The
# plan uses k   1.638. The lot is accepted if the sample mean breaking strength is at least
# approximately 29.01 g; otherwise, it is rejected. The OC curve is expected to pass through the
# design point (AQL = 3%, Pa = 0.94) and near the LTPD point (8%, Pa   0.068). Hence, the
# sampling plan provides a high probability of acceptance at the AQL and a low probability of
# acceptance at the LTPD.
# 
# Note: Since n must be an integer, the achieved LTPD acceptance probability is approximately
# 6.8%, rather than exactly 7%.



############################################################
# PRACTICAL 11
############################################################
# Aim
# To assess the capability of a dairy-processing step from subgroup averages and ranges: to
# estimate the indices Cp, Cpk, Cpm and Cpmk against a target of 23 mg, to find 95% confidence
# intervals for Cp and Cpk, and to test whether Cp is less than 0.6 at the 5% level of significance.
# 
# Problem Statement
# The amount of preservative added to a dairy product should not exceed certain levels, 23 +/- 6
# mg. Samples of size 5 of processed cheese were taken from the process, and the average level
# of preservative and the range of each sample are shown in the table.
#          Sample       Average          Range         Sample       Average          Range
#            1            22               5             14           22               7
#            2            26               4             15           20               5
#            3            26               6             16           24               8
#            4            24               7             17           25               6
#            5            22               3             18           23               8
#            6            21               5             19           20               5
#            7            29               7             20           22               4
#            8            25               8             21           22               6
#            9            22               4             22           23               7
#            10           25               6             23           24               5
#            11           25               9             24           22               6
#            12           22               3             25           25               5
#            13           21               5
# 
# 
# 
# (a) Assuming normality and a target value of 23 mg, determine the indices Cp, Cpk, Cpm and
# Cpmk.
# 
# (b) Find a 95% confidence interval for Cp and Cpk, assuming normality.
# 
# (c) Can we conclude that Cp is less than 0.6 when alpha = 0.05?
# 
# Theory
# The specification 23 +/- 6 mg gives LSL = 17, USL = 29 and target T = 23, so the specification width
# is USL - LSL = 12. With m = 25 samples of size n = 5, the process mean is estimated by the grand
# mean of the sample averages and the process standard deviation from the average range, using
# the control-chart constant d2 = 2.326 for n = 5:
# 
# The potential capability of the process, which ignores how well it is centred, is measured by Cp,
# and the actual capability, which penalises a mean that is off-centre, by Cpk:
# 
# 
# 
# Cpm and Cpmk also take the distance of the mean from the target T into account, through the
# factor   below. They equal Cp and Cpk when the mean is exactly on target and fall below them
# as the mean moves away from T:
# 
# 
# 
# For a normal process, with N = mn the total number of observations and chi^2(q,  ) the q-th
# quantile of the chi-square distribution with   degrees of freedom, a 100(1 - alpha)% confidence
# interval for Cp is
# 
# 
# 
# and an approximate interval for Cpk, based on the normal distribution, is
# 
# 
# 
# To test H0: Cp = 0.6 against H1: Cp < 0.6, H0 is rejected at level alpha if  p falls below the critical value
# 
# 
# 
# Equivalently, (N - 1)( p/Cp0)^2 is compared with the chi-square distribution on N - 1 degrees of
# freedom, and the p-value is the probability that  p is as small as the observed value when Cp =
# 0.6.
# 
# mean() gives the grand mean and Rbar from the vectors of averages and ranges, and min() picks
# the smaller of Cpu and Cpl for Cpk. qchisq() and qnorm() give the chi-square and normal
# quantiles for the intervals and the critical value, pchisq() gives the p-value, and data.frame()
# collects the estimates and limits in one table.

# R CODE

# Practical 11
# Data: 25 subgroups of size n = 5 (sample average and range)
avg <- c(22, 26, 26, 24, 22, 21, 29, 25, 22, 25, 25, 22, 21,
         22, 20, 24, 25, 23, 20, 22, 22, 23, 24, 22, 25)
 
rng <- c(5, 4, 6, 7, 3, 5, 7, 8, 4, 6, 9, 3, 5,
         7, 5, 8, 6, 8, 5, 4, 6, 7, 5, 6, 5)
 
n <- 5               # subgroup size
m <- length(avg)     # number of subgroups
N <- n * m           # total number of observations
d2 <- 2.326          # control-chart constant d2 for n = 5
alpha <- 0.05
 
# Specification: 23 +/- 6 mg, target = 23 mg
Target <- 23
LSL <- Target - 6
USL <- Target + 6
c(LSL = LSL, Target = Target, USL = USL)
 
# Process mean and standard deviation estimates
xbarbar <- mean(avg)
Rbar <- mean(rng)
sigma <- Rbar / d2
round(c(xbarbar = xbarbar, Rbar = Rbar, sigma = sigma), 4)
 
# a) Process capability indices
Cp <- (USL - LSL) / (6 * sigma)
Cpu <- (USL - xbarbar) / (3 * sigma)
Cpl <- (xbarbar - LSL) / (3 * sigma)
Cpk <- min(Cpu, Cpl)
tau <- sqrt(1 + ((xbarbar - Target) / sigma)^2)
Cpm <- Cp / tau
Cpmk <- Cpk / tau
 
round(c(Cp = Cp, Cpu = Cpu, Cpl = Cpl, Cpk = Cpk,
        Cpm = Cpm, Cpmk = Cpmk), 4)
 
# b) 95% confidence intervals for Cp and Cpk
Cp_L <- Cp * sqrt(qchisq(alpha / 2, N - 1) / (N - 1))
Cp_U <- Cp * sqrt(qchisq(1 - alpha / 2, N - 1) / (N - 1))
 
z <- qnorm(1 - alpha / 2)
se_Cpk <- sqrt(1 / (9 * N) + Cpk^2 / (2 * (N - 1)))
Cpk_L <- Cpk - z * se_Cpk
Cpk_U <- Cpk + z * se_Cpk
 
data.frame(Index = c("Cp", "Cpk"),
           Estimate = round(c(Cp, Cpk), 4),
           Lower = round(c(Cp_L, Cpk_L), 4),
           Upper = round(c(Cp_U, Cpk_U), 4))
 
# c) Test H0: Cp = 0.6 against H1: Cp < 0.6 at alpha = 0.05
Cp0 <- 0.6
crit <- Cp0 * sqrt(qchisq(alpha, N - 1) / (N - 1))
chi_stat <- (N - 1) * (Cp / Cp0)^2
p_val <- pchisq(chi_stat, N - 1)
 
round(c(Cp_hat = Cp, Critical = crit, Chi_sq = chi_stat, p_value = p_val), 4)
 
if (Cp < crit) "Reject H0" else "Fail to reject H0"


# Result and Conclusion
# The sample averages and ranges gave the grand mean muhat = 23.28 and Rbar = 5.76, so the process
# standard deviation is estimated as sigmahat = 5.76/2.326 = 2.4764. With LSL = 17 and USL = 29, the
# indices are
# 
# All four indices are below 1, so the process is not capable of meeting the specification 23 +/- 6
# mg: the natural spread of the process, 6sigmahat = 14.86 mg, is wider than the specification width of 12
# mg. Cpk (0.7699) is slightly smaller than Cp (0.8076) because the mean 23.28 lies a little above
# the target 23, nearer to the USL, which is also why Cpu < Cpl. Cpm (0.8025) is almost equal to Cp
# since the mean is very close to the target, so the poor capability is caused by the excess
# variation and not by the centring.
# 
# The 95% confidence intervals are (0.7072, 0.9079) for Cp and (0.6577, 0.8822) for Cpk. Both
# intervals lie entirely below 1, so the process is significantly not capable at the 5% level.
# 
# For the test of H0: Cp = 0.6 against H1: Cp < 0.6, the critical value is 0.5369 and the observed  p
# = 0.8076 is greater than it (p-value = 1), so H0 is not rejected. We cannot conclude that Cp is less
# than 0.6. In fact the lower confidence limit 0.7072 is above 0.6, so Cp is at least 0.6 with 95%
# confidence, but it is still well short of the value 1 needed for a capable process. The variation of
# the process has to be reduced to bring it within the specification limits.
# 
#                                                                                         Gurbaksh Singh
#                                                                                          SQM Group 2



############################################################
# PRACTICAL 12
############################################################
# Aim
# To develop and compare two Continuous Sampling Plan-1 (CSP-1) plans for a manufacturing
# process when the specified Average Outgoing Quality Limit (AOQL) is 1.90%; to calculate and
# compare their Average Fraction Inspected (AFI) and Operating Characteristic (OC) values; and to
# select the preferred plan when the incoming fraction defective is p = 0.0375.
# 
# Problem Statement
# Suppose CSP-1 is used for a manufacturing process for which an AOQL of 1.90% is desired.
# Develop two CSP-1 plans that satisfy this AOQL requirement. Compare the plans in terms of
# their Average Fraction Inspected and Operating Characteristic curves. Decide which plan is
# preferable when the incoming fraction defective is 0.0375.
# 
# Theory and Formulae
# In a CSP-1 plan, all units are inspected initially. After i consecutive conforming (defect-free) units
# are obtained, inspection is reduced to a sampling inspection in which only a fraction f of the
# units is inspected. If a defective unit is found during sampling inspection, 100% inspection is
# resumed.
# Let p be the incoming fraction defective, q = 1 - p be the fraction non-defective, i be the
# clearance number, and f be the sampling fraction.
# The Average Outgoing Quality (AOQ) for a CSP-1 plan is:
#                                  AOQ(p) = p(1 - f)q  / [f + (1 - f)q ]
# 
# The Average Outgoing Quality Limit is the maximum value of AOQ over the range of incoming
# quality:
#                                         AOQL = max AOQ(p)
# 
# The Average Fraction Inspected (AFI) is:
#                                       AFI(p) = f / [f + (1 - f)q ]
# 
# The Operating Characteristic (OC), or probability of acceptance, is:
#                                       P (p) = q  / [f + (1 - f)q ]
# 
# For comparison, two sampling fractions are considered: f = 0.05 and f = 0.10. The required AOQL
# is 0.019, and the comparison is made at p = 0.0375.

# R CODE

# Practical 12
# CSP-1

p <- seq(0.001, 0.15, 0.001)
p0 <- 0.0375
f <- c(0.05, 0.10)
AOQL <- 0.019

# Plan 1
i1 <- 1
q <- 1-p

while (max(p*(1-f[1])*q^i1 /
           (f[1]+(1-f[1])*q^i1)) > AOQL) {
  i1 <- i1+1
}

# Plan 2
i2 <- 1

while (max(p*(1-f[2])*q^i2 /
           (f[2]+(1-f[2])*q^i2)) > AOQL) {
  i2 <- i2+1
}

# AOQ
AOQ1 <- p*(1-f[1])*(1-p)^i1 /
  (f[1]+(1-f[1])*(1-p)^i1)

AOQ2 <- p*(1-f[2])*(1-p)^i2 /
  (f[2]+(1-f[2])*(1-p)^i2)

# AFI
AFI1 <- f[1]/(f[1]+(1-f[1])*(1-p0)^i1)
AFI2 <- f[2]/(f[2]+(1-f[2])*(1-p0)^i2)

# OC
OC1 <- (1-p0)^i1/(f[1]+(1-f[1])*(1-p0)^i1)
OC2 <- (1-p0)^i2/(f[2]+(1-f[2])*(1-p0)^i2)

# Results
i1
i2
max(AOQ1)
max(AOQ2)
AFI1
AFI2
OC1
OC2
# AOQ curves
plot(p, AOQ1, type="l", col="blue", lwd=2,
     ylim=c(0,0.025), xlab="p", ylab="AOQ")
lines(p, AOQ2, col="red", lwd=2)
abline(h=AOQL, lty=2)
 
# AFI curves
plot(p, f[1]/(f[1]+(1-f[1])*(1-p)^i1),
     type="l", col="blue", lwd=2,
     ylim=c(0,1), xlab="p", ylab="AFI")
lines(p, f[2]/(f[2]+(1-f[2])*(1-p)^i2),
      col="red", lwd=2)
 
# OC curves
plot(p, (1-p)^i1/(f[1]+(1-f[1])*(1-p)^i1),
     type="l", col="blue", lwd=2,
     ylim=c(0,1), xlab="p", ylab="OC")
lines(p, (1-p)^i2/(f[2]+(1-f[2])*(1-p)^i2),
      col="red", lwd=2)





# Results
#  Measure                         Plan 1 (f = 0.05)                Plan 2 (f = 0.10)
#  Clearance number (i)            79                               57
#  AOQL                            0.01897899 (1.8979%)             0.01896547 (1.8965%)
#  AFI at p = 0.0375               0.5187504 (51.8750%)             0.4953463 (49.5346%)
#  OC at p = 0.0375                0.5065785                        0.5607264
# Both plans satisfy the specified AOQL of 1.90%. Plan 1 has a clearance number of 79, while Plan
# 2 has a clearance number of 57. At p = 0.0375, the AFI values are 0.5187504 and 0.4953463 for
# Plans 1 and 2, respectively. The OC values are 0.5065785 and 0.5607264, respectively.
# 
# 
# Interpretation of Curves
# The AOQ curves show that the maximum outgoing quality for both plans remains below the
# specified AOQL of 1.90%. The AFI curves show how the average proportion inspected changes
# with the incoming fraction defective. The OC curves show that the probability of acceptance
# decreases as the incoming fraction defective increases.
# At p = 0.0375, Plan 2 has a lower AFI than Plan 1. Therefore, Plan 2 requires a smaller average
# inspection effort. Plan 2 also has a higher probability of acceptance at this incoming fraction
# defective.
# 
# Conclusion
# Two CSP-1 plans were developed for the specified AOQL of 1.90%, using sampling fractions of
# 0.05 and 0.10. The corresponding clearance numbers were found to be 79 and 57. The
# maximum AOQ values were 0.01897899 and 0.01896547, respectively, so both plans satisfy the
# required AOQL.
# At an incoming fraction defective of 0.0375, Plan 1 has an AFI of 0.5187504 (51.8750%),
# whereas Plan 2 has an AFI of 0.4953463 (49.5346%). Plan 2 therefore requires less inspection.
# Its OC value is 0.5607264, compared with 0.5065785 for Plan 1.
# Hence, Plan 2 with clearance number i = 57 and sampling fraction f = 0.10 is preferred because
# it satisfies the AOQL requirement while having a lower Average Fraction Inspected at the
# specified incoming fraction defective.



