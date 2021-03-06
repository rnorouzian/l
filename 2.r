Figure.2 = function(N, d){
  
  options(warn = -1) ; d = sort(d)
  df = N - 1  ;  d.SE = 1/sqrt(N)  ;  ncp.min = min(d)*sqrt(N)  ;  ncp.max = max(d)*sqrt(N)
  min.d = d.SE*qt(1e-5, df, ncp.min)  ;  max.d = d.SE*qt(0.99999, df, ncp.max)  
  
  for(i in 1:length(d)){      
    H = curve(dt(x*sqrt(N), df, d[i]*sqrt(N)), min.d, max.d, n = 1e3, xlab = "Effect Size (Cohen's d)", 
              ylab = NA, ty = "n", add = i!= 1, bty = "n", yaxt = "n", font.lab = 2, yaxs = "i")
    
    polygon(H, col = adjustcolor(i, .7), border = NA)
    text(d[i], max(H$y), bquote(bolditalic(H[.(i-1)])), pos = 3, xpd = NA)
    axis(1, at = d[i], col = i, col.axis = i, font = 2)
    segments(d[i], 0, d[i], max(H$y), lty = 3)
  }
}
# Example of use:
Figure.2(N = 30, d = seq(0, 2, .5))
