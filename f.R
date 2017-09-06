######### Histogram Panel function for pair ###########

panel.hist = function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}

#pairs(USJudgeRatings[1:5], panel = panel.smooth,
#     cex = 1.5, pch = 24, bg = "light blue",
#    diag.panel = panel.hist, cex.labels = 2, font.labels = 2)

######### Correlation Panel Function for pairs() #########

panel.cor = function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor) #cex = cex.cor*r
}
#pairs(USJudgeRatings, lower.panel = panel.smooth, upper.panel = panel.cor)


####################################################################################
## Function to economically compute BF10, Cohen's d, and p.value from the literature
####################################################################################


BF.d.pvalue = Vectorize(function(t, n1, n2 = NA, scale = sqrt(2)/2, log.BF = FALSE){
  
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
  
}, vectorize.args = c("t", "n1", "n2", "scale", "log.BF"))



decimal <- function(x, k){
  
  if(is.character(x)) {
    return(x)
  }
  
  format(round(x, k), nsmall = k, scientific = 
         ifelse(x >= 1e5 || x <= -1e5 || x <= 1e-5 & x >= -1e-5, TRUE, FALSE) )
}



vec.nrow = Vectorize(function(x)nrow(x), "x")

vec.ncol = Vectorize(function(x)ncol(x), "x")

vec.length = Vectorize(function(x)length(x), "x")

vec.max = Vectorize(function(x)max(x), "x")

vec.min = Vectorize(function(x)min(x), "x")

vec.rt = Vectorize(function(n, df, ncp)rt(n, df, ncp), vectorize.args = c("n", "df", "ncp"))

