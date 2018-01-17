BF.d.pvalue = Vectorize(function(t, n1, n2 = NA, scale = 1, log.BF = FALSE){
  
   options(warn = -1)  
      t = abs(t)
      N = ifelse(is.na(n2), n1, (n1 * n2)/(n1 + n2))
     df = ifelse(is.na(n2), n1 - 1, (n1 + n2) - 2)
      d = t / sqrt(N)
  
     H1 = integrate(function(delta) dcauchy(delta, 0, scale) * dt(t, df, delta*sqrt(N)), -Inf, Inf)[[1]]
     H0 = dt(t, df)
   BF10 = ifelse(log.BF, log(H1/H0), H1/H0)
p.value = 2*(1-pt(t, df))
  
cbind(BF10 = BF10, p.value = p.value, d = d, H0 = H0, H1 = H1, n1 = n1, n2 = n2, t = t)
  
}, c("t", "n1", "n2", "scale", "log.BF"))



decimal <- function(x, k){
  
  if(is.character(x)) {
    return(x)
  }
  
  format(round(x, k), nsmall = k, scientific = 
         ifelse(x >= 1e5 || x <= -1e5 || x <= 1e-5 & x >= -1e-5, TRUE, FALSE) )
}
