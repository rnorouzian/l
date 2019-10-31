Break = "\n<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n"

notice = "   \"bayesL2\", a suite of R functions for Bayesian estimation in L2 research.
    Copyright (C) 2017-present  Reza Norouzian, rnorouzian@gmail.com
    This set of programs is free software: you can redistribute it under the 
    terms of the GNU General Public License as published by the Free 
    Software Foundation, either version 3 of the License, or any later 
    version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>."

message(Break, notice, Break)

Break = "\n*****************************************************************************\n"

cite = "To cite the package use:\n\nNorouzian, R., de Miranda, M. A., & Plonsky, L. (2018). The Bayesian \nrevolution in second language research: An applied approach. Language Learning, 64, 1032-1075.
\nNorouzian, R., de Miranda, M. A., & Plonsky, L. (2019). A Bayesian approach to measuring evidence \nin L2 research: An empirical investigation. Modern Language Journal, 103, 248-263."

cat(Break, cite, Break)

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
  
})

#============================

decimal <- function(x, k){
  
  if(is.character(x)) {
    return(x)
  }
  
  format(round(x, k), nsmall = k, scientific = 
         ifelse(x >= 1e5 || x <= -1e5 || x <= 1e-5 & x >= -1e-5, TRUE, FALSE) )
}
  
  
#============================  
  
 BF.t <- function(t, n1, n2 = NA, scale = sqrt(2)/2, log.BF = FALSE){
  
f <- Vectorize(function(t, n1, n2, scale, log.BF){
  
  options(warn = -1)  
  t = abs(t)
  N = ifelse(is.na(n2), n1, (n1 * n2)/(n1 + n2))
  df = ifelse(is.na(n2), n1 - 1, (n1 + n2) - 2)
  d = t / sqrt(N)
  
  H1 = integrate(function(delta) dcauchy(delta, 0, scale) * dt(t, df, delta*sqrt(N)), -Inf, Inf)[[1]]
  H0 = dt(t, df)
  BF10 = ifelse(log.BF, log(H1/H0), H1/H0)
  p.value = 2*(1-pt(t, df))
  
  return(c(t = t, BF10 = BF10, H0 = H0, H1 = H1, p.value = p.value, d = d,  n1 = n1, n2 = n2))
})
  data.frame(t(f(t = t, n1 = n1, n2 = n2, scale = scale, log.BF = log.BF)))
} 
