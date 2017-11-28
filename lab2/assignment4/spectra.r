library(fastICA)
library(pls)

set.seed(12345)
data = read.csv("NIRSpectra.csv", header = TRUE, 
                sep = ";", dec = ",", fill = TRUE)
features = princomp(data[, -ncol(data)])
plot(features, main="PCA of different spectras")
# Proportion of variance in PC1 + PC2 = 0.933 + 0.062 = 0.996 > 0.99
# -- we choose PC1 and PC2.

# plot of PC1 vs PC2
x_lim = c(min(features$scores[ ,1]), max(features$scores[ ,1]))
y_lim = c(min(features$scores[ ,2]), max(features$scores[ ,2]))
plot(features$scores[ ,1], features$scores[ ,2], xlim=x_lim, ylim=c(-1.5, 0.15))

# loads, how much each feature contribute to the variance seen in a PC
loads = loadings(features)
par(mfrow=c(1,1))
plot(loads[, 1], type="b", ylab="Loadings PC1")
plot(loads[, 2], type="b", ylab="Loadings PC2")

# which has 0 load? those contribute nothing to the linear transformation,
# LOAD = how much a feature contribute to the variance within a principal component
# i.e. how much does one dimension contribute to the variance along the dimension of the PC.
# ************************ TODO:
# Is there any principle component that is explained by mainly a few original features?
# YES PC2, there are a lot of features that are around 0
# ************************

res = fastICA(data[ ,-ncol(data)], 2, fun = "logcosh", alpha = 1.0,
              row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

#str(res)
# K = pre-whitening matrix that projects data onto the first n.comp principal components.
# W = estimated un-mixing matrix
# ************************ TODO:
# What is W_prime??
# ************************
Wp = res$K %*% res$W

plot(Wp[, 1])
plot(Wp[, 2])

# ************************ TODO:
# Make a plot of the scores of the first two latent features and compare
# it with the score plot from step 1
# lookup str(res) -- S ?
# ***********************

plot(res$S[, 1], res$S[, 2], main = "Scores of first two latent features in fastICA")

pcr.fit = pcr(formula = Viscosity ~ ., 
              data = data,
              validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
