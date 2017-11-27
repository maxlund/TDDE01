library(fastICA)
library(pls)

set.seed(12345)
data = read.csv("NIRSpectra.csv", header = TRUE, 
                sep = ";", dec = ",", fill = TRUE)
# ************************ TODO:
# Should we remove the 'Viscosity' column when doing PCA etc??
# ************************
features = princomp(data[, -ncol(data)])
summary(features)
# First 3 components:
# Proportion of Variance: 0.9463514 0.05008087 0.003372371
# Proportion of variance in PC1 + PC2 = 0.9333233 + 0.06263367 = 0.9959569699999999 > 0.99
# -- we choose PC1 and PC2.

plot(features, main="PCA of different spectras")

# plot of PC1 vs PC2
x_lim = c(min(features$scores[ ,1]), max(features$scores[ ,1]))
y_lim = c(min(features$scores[ ,2]), max(features$scores[ ,2]))
plot(features$scores[ ,1], features$scores[ ,2], xlab="PC1", ylab="PC2", xlim=x_lim, ylim=y_lim)

# for comparison, if we look at ex. PC1 vs PC5:
# plot(features$scores[ ,1], features$scores[ ,16], xlab="PC1", ylab="PC2", xlim=x_lim, ylim=y_lim)
# .. all the variance is explained by PC1

loads = loadings(features)
plot(loads[, 1], type="b")
plot(loads[, 2], type="b")
# ************************ TODO:
# Is there any principle component that is explained by mainly a few original features?
# ************************

res = fastICA(data[ ,-ncol(data)], 2, fun = "logcosh", alpha = 1.0,
                   row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

# K = pre-whitening matrix that projects data onto the first n.comp principal components.
# W = estimated un-mixing matrix
# ************************ TODO:
# What is W_prime??
# ************************
W_prime = res$K %*% res$W

plot(W_prime[, 1])
plot(W_prime[, 2])

# ************************ TODO:
# Make a plot of the scores of the first two latent features and compare
# it with the score plot from step 1
# ***********************

pcr.fit = pcr(formula = Viscosity ~ ., 
              data = data,
              validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
# it seems after choosing more than 6 components, the MSEP doesn't go down much
validationplot(pcr.fit, val.type = "MSEP", xlim=c(0,10))
# TODO: Correct to say # components to select = 6??
