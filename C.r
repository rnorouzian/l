source("https://raw.githubusercontent.com/izeh/l/master/d.r")

pre_analysis()

p.dist()

p.sig.dist()

p.value_BF()

cat("Sig Ps that remain sig after .005 =",(nrow(subset(all.data, p < .05))-nrow(subset(all.data, p < .005)) )/nrow(subset(all.data, p < .05))*1e2,'%')

tt = subset(pval.bf, (p > .01 & p <= .05))

noquote(paste0("p.values(.01-.05)_BFs(Anecdotal[H1]) = ", round(length(which(tt$BF <= 3 & tt$BF >= 1)) / nrow(tt)*1e2, 2), "%"))

noquote(paste0("p.values(.01-.05)_BFs(Anecdotal[H0]) = ", round(length(which(tt$BF >= 1/3 & tt$BF <= 1)) / nrow(tt)*1e2, 2), "%"))



