
# check generalised gamma priors
# for checkmate analysis

library(flexsurv)
library(scales)

m <- 100   # number of event times
n <- 1000   # parameter sample size

mu <- rnorm(n, 0.3, 0.5)
Q <- rnorm(n, -0.7, 0.6)
scale <- rlnorm(n, log(0.9), 0.1)


t <- rgengamma(m, mu = mu[1], sigma = scale[1], Q = Q[1])
survfit(Surv(t, rep(1, m)) ~ 1) |> plot(conf.int = FALSE, xlim = c(0,100))

pred_samples <- NULL

for (i in 1:n) {
  t <- rgengamma(m, mu = mu[i], sigma = scale[i], Q = Q[i])
  pred_samples <- cbind(pred_samples, t)
  survfit(Surv(t, rep(1, m)) ~ 1) |> lines(conf.int = FALSE, col = alpha("grey", 0.1))
}

# one time per parameter sample
data <- rgengamma(n, mu = 0.1, sigma = 0.5, Q = -0.7)

plot_hist_retro(data, pred_samples, "Time", breaks = seq(0,150,1), xlim = c(0,10))


##################
# mike betancourt
# https://betanalpha.github.io/assets/case_studies/survival_modeling.html


plot_hist_retro <- function(data, pred_samples, name, breaks, xlim) {

  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  B <- length(breaks) - 1
  idx <- rep(1:B, each=2)
  xs <- sapply(1:length(idx),
               function(b) if(b %% 2 == 1) breaks[idx[b]] else breaks[idx[b] + 1])

  obs <- hist(data, breaks=breaks, plot=FALSE)$counts
  pad_obs <- do.call(cbind, lapply(idx, function(n) obs[n]))

  post_pred <- sapply(1:nrow(pred_samples),
                      function(n) hist(pred_samples[n,], breaks=breaks, plot=FALSE)$counts)

  probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  cred <- sapply(1:B, function(b) quantile(post_pred[b,], probs=probs))
  pad_cred <- do.call(cbind, lapply(idx, function(n) cred[1:9, n]))

  plot(1, type="n", main="Posterior Retrodictive Check",
       xlim=xlim, xlab=name,
       ylim=c(0, max(c(obs, cred[9,]))), ylab="")

  polygon(c(xs, rev(xs)), c(pad_cred[1,], rev(pad_cred[9,])),
          col = c_light, border = NA)
  polygon(c(xs, rev(xs)), c(pad_cred[2,], rev(pad_cred[8,])),
          col = c_light_highlight, border = NA)
  polygon(c(xs, rev(xs)), c(pad_cred[3,], rev(pad_cred[7,])),
          col = c_mid, border = NA)
  polygon(c(xs, rev(xs)), c(pad_cred[4,], rev(pad_cred[6,])),
          col = c_mid_highlight, border = NA)
  for (b in 1:B)
    lines(xs[(2 * b - 1):(2 * b)], pad_cred[5,(2 * b - 1):(2 * b)],
          col=c_dark, lwd=2)

  lines(xs, pad_obs, col="white", lty=1, lw=2.5)
  lines(xs, pad_obs, col="black", lty=1, lw=2)
}

