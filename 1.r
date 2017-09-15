hyp.test = function(N = 30, obs.d = .1){
  
  options(warn = -1)  ;  df = N - 1  ;  d.SE = 1/sqrt(N)
min.d = qt(1e-6, df)*d.SE  ;  max.d = qt(0.999999, df)*d.SE

`d|H0` = curve(dt(x/d.SE, df)/d.SE, min.d, max.d, n = 1e4, xlab = "Effect Size (d)", 
               ylab = NA, font.lab = 2, ty = "n", axes = FALSE, bty = "n", yaxs = "i")
  
  polygon(`d|H0`, col = 8, border = 8)  ;  axis(1)
  
    arrow = qt(.999, df)*d.SE
  p.value = signif(2*(1-pt(abs(obs.d)/d.SE, df)), 4)
  
   x = seq(min.d, -obs.d, l = 1e4) ;  y = dt(x/ d.SE, df)/d.SE
  xx = seq(max.d,  obs.d, l = 1e4) ; yy = dt(xx/d.SE, df)/d.SE
  
  polygon(c(min.d,  x, -obs.d), c( y[1],  y, rev( y[1])), col = 2, border = NA)  
  polygon(c(max.d, xx,  obs.d), c(yy[1], yy, rev(yy[1])), col = 2, border = NA)  
  
  segments(0, 0, 0, max(`d|H0`$y), col = 2, xpd = FALSE, lty = 2) 
  axis(1, at = obs.d, font = 2, col = 2, col.axis = 2)
  points(obs.d, 0, pch = 23, bg = 3, cex = 1.4, xpd = TRUE)
  legend("topleft", "Obtained Effect Size (d)", pch = 23, pt.bg = 3, pt.cex = 1.2, bty = "n", text.font = 2)   
  
  arrows((obs.d+max.d)/3, dt(((obs.d+max.d)/3)/d.SE , df)/d.SE, arrow, max(`d|H0`$y)/2, code = 1, length = .1, angle = 20, lwd = 2)
  arrows((-obs.d+min.d)/3, dt(((-obs.d+min.d)/3)/d.SE , df)/d.SE, arrow, max(`d|H0`$y)/2, code = 1, length = .1, angle = 20, lwd = 2)
  text(arrow, max(`d|H0`$y)/2, bquote(bold("p-value" == .(p.value))), font = 2, pos = 3)
}
#Example of use:
hyp.test(N = 30, obs.d = .3)
