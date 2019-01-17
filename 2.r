source("https://raw.githubusercontent.com/rnorouzian/i/master/i.r")

Figure.2 = function(N, d){
  
  options(warn = -1) ; d = sort(d)
  min.d = qcohen(1e-5, min(d), N)  ;  max.d = qcohen(.99999, max(d), N)  
  
  for(i in 1:length(d)){      
    H = curve(dcohen(x, d[i], N), min.d, max.d, n = 1e3, xlab = "Effect Size (d)", axes = FALSE,
              ylab = NA, type = "n", add = i!= 1, bty = "n", font.lab = 2, yaxs = "i")
    
    polygon(H, col = adjustcolor(i, .7), border = NA)
    text(d[i], max(H$y), bquote(bolditalic(H[.(i-1)])), pos = 3, xpd = NA)
    axis(1, at = d[i], col = i, col.axis = i, font = 2)
    segments(d[i], 0, d[i], max(H$y), lty = 3)
  }
}
# Example of use:
Figure.2(N = 30, d = seq(0, 2, .5))
