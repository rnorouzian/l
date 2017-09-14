H = curve(dcauchy(x, 0, sqrt(2)/2), -6, 6, n = 1e4, yaxt = "n", ty = "n", yaxs = "i",
          bty = "n", ylab = NA, xlab = "Effect Sizes (d)", font.lab = 2)

x = seq(-1, 1, l = 1e4)
y = dcauchy(x, 0, sqrt(2)/2)

polygon(c(-1, x, 1), c(0, y, 0), col = "gold", border = NA)
lines(H, lwd = 2)
segments(c(-1, 0, 1), rep(0, 3), x <- c(-1, 0, 1), dcauchy(x, 0, sqrt(2)/2), lty = 3, col = 4)
axis(1, at = c(-1, 1), col.ticks = 2, col.axis = 2, font = 2)
arrows(.3, max(H$y)/2, 2, max(H$y)/1.3, code = 2)
points(.3, max(H$y)/2, pch = 19, cex = .9)
text(2, max(H$y)/1.3, "Concentration of \nfrequently found\n effect sizes in L2\n Research", pos = 3, font = 2)
