  set.seed(0)
   x = rcauchy(5e4, 0, sqrt(2)/2)
cuts = quantile(x, c(.025, .975))
 cut = x[x >= cuts[1] & x <= cuts[2]]
   h = hist(cut, breaks = 80, plot = FALSE)
cuts = cut(h$breaks, c(-1.2, .8))

plot(h, col = rgb(0, 0, 1, .3)[cuts], axes = FALSE, ylab = NA, main = NA, xlab = "Alternative Effect Sizes", font.lab = 2)

axis(1, at = -9:9, font = 2, xpd = NA)
abline(v = c(-1, 1), col = "red", lty = 2)
arrows(.8, 3e3, 3, 3500, code = 2, length = .15, angle = 20, lwd = 2)
text(6, 3700, "Concentration of Plausible \n Effect Sizes in L2 Research (d)", font = 2, xpf = NA)