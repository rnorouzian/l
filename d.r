D = read.csv("https://raw.githubusercontent.com/izeh/l/master/l.csv")

source("https://raw.githubusercontent.com/izeh/l/master/f.r")

t.value = D$t.value ; n1 = D$n1 ; n2 = D$n2 ; t.type = D$t.type ; p.value = D$p.value ; Data.size = nrow(D)

## Pre-analysis: 
pre_analysis = function(){
list(Data.size = Data.size,
paired.samples = sum(t.type == 0)-3,
one.sample = 3,
independent.samples = sum(t.type == 1))
}

## Main Analysis:
b = BF.d.pvalue(t = t.value, n1 = n1, n2 = n2, scale = 1)
BF = b[1, ]  ;  p.value = b[2, ]   ;  d = b[3, ] ; n1 = b[6, ]  ; n2 = b[7, ] ; t = b[8, ]

all.data = data.frame(p = p.value, BF = BF, d = d, n1 = n1, n2 = n2, t = t)
pval.bf = data.frame(p = p.value, BF = BF)
d.bf = data.frame(BF = BF, d = d)  
pval.d = data.frame(p = p.value, d = d)

## all p.values distribution
p.dist = function(){
original_par = par(no.readonly = TRUE)
on.exit(par(original_par))  
par(mgp = c(2.5, .6, 0), las = 1, yaxt = "n", mgp = c(2.5, .6, 0))  
h = hist(all.data$p, breaks = 15, xlab = bquote(bolditalic("p")*bold("-value")), font.lab = 2, main = NA)
rect(0, 0, .05, max(h$counts), col = rgb(0, 0, 1, .4), border = 2)
axis(1, at = .05, lab = ".05", col = 2, col.axis = 2, font = 2)
axis(2, at = c(seq(0, 2e2, l = 5), max(h$counts)))
  }

## Significant p.values distribution
p.sig.dist = function(){
original_par = par(no.readonly = TRUE)
on.exit(par(original_par))  
par(mgp = c(2.5, .75, 0), las = 1)      
sig.p = unname(unlist(subset(all.data, p <= .05)[1]))
h = hist(sig.p, xlab = bquote(bolditalic("p")*bold("-value")), font.lab = 2, main = NA, yaxt = "n", mgp = c(2.5, .6, 0))
rect(0, 0, .005, max(h$counts), col = rgb(0, 0, 1, .4), border = 2)
axis(1, at = .005, lab = ".005", col = 2, col.axis = 2, font = 2)
axis(2, at = c(seq(0, 100, l = 3), max(h$counts)))
cat("Percentage of sig. p to all ps = ", length(sig.p)/length(all.data$p)*1e2, "%", "\n\tSig Ps that remain sig after .005 =",(nrow(subset(all.data, p < .05))-nrow(subset(all.data, p < .005)) )/nrow(subset(all.data, p < .05))*1e2,'%')
 } 

### Ready the subsets:
p.value_BF = function(){
## Subsetting for a 7x4 plot p.value against BF:
p.b7.4 = subset(pval.bf, (BF > .1 & BF <= 1/3) & (p > .05)) 
p.b6.4 = subset(pval.bf, (BF > 1/3 & BF <= 1) & (p > .05))
p.b5.4 = subset(pval.bf, (BF > 1 & BF <= 3) & (p > .05)) 
p.b4.4 = subset(pval.bf, (BF > 3 & BF <= 10) & (p > .05)) 
p.b3.4 = subset(pval.bf, (BF > 10 & BF <= 30) & (p > .05))  
p.b2.4 = subset(pval.bf, (BF > 30 & BF <= 100) & (p > .05)) 
p.b1.4 = subset(pval.bf, (BF > 100) & (p > .05)) 

p.b7.3 = subset(pval.bf, (BF > .1 & BF <= 1/3) & (p > .01 & p <= .05)) 
p.b6.3 = subset(pval.bf, (BF > 1/3 & BF <= 1) & (p > .01 & p <= .05))
p.b5.3 = subset(pval.bf, (BF > 1 & BF <= 3) & (p > .01 & p <= .05)) 
p.b4.3 = subset(pval.bf, (BF > 3 & BF <= 10) & (p > .01 & p <= .05))
p.b3.3 = subset(pval.bf, (BF > 10 & BF <= 30) & (p > .01 & p <= .05))
p.b2.3 = subset(pval.bf, (BF > 30 & BF <= 100) & (p > .01 & p <= .05))
p.b1.3 = subset(pval.bf, (BF > 100) & (p > .01 & p <= .05))

p.b7.2 = subset(pval.bf, (BF > .1 & BF <= 1/3) & (p > .001 & p <= .01))
p.b6.2 = subset(pval.bf, (BF > 1/3 & BF <= 1) & (p > .001 & p <= .01))
p.b5.2 = subset(pval.bf, (BF > 1 & BF <= 3) & (p > .001 & p <= .01))
p.b4.2 = subset(pval.bf, (BF > 3 & BF <= 10) & (p > .001 & p <= .01))
p.b3.2 = subset(pval.bf, (BF > 10 & BF <= 30) & (p > .001 & p <= .01))
p.b2.2 = subset(pval.bf, (BF > 30 & BF <= 100) & (p > .001 & p <= .01))
p.b1.2 = subset(pval.bf, (BF > 100) & (p > .001 & p <= .01))

p.b7.1 = subset(pval.bf, (BF > .1 & BF <= 1/3) & (p <= .001))
p.b6.1 = subset(pval.bf, (BF > 1/3 & BF <= 1) & (p <= .001))
p.b5.1 = subset(pval.bf, (BF > 1 & BF <= 3) & (p <= .001))
p.b4.1 = subset(pval.bf, (BF > 3 & BF <= 10) & (p <= .001))
p.b3.1 = subset(pval.bf, (BF > 10 & BF <= 30) & (p <= .001))
p.b2.1 = subset(pval.bf, (BF > 30 & BF <= 100) & (p <= .001))
p.b1.1 = subset(pval.bf, (BF > 100) & (p <= .001))

pb.cat.sizes = lapply(mget(ls(pattern = "p\\.b\\d")), nrow)
total.pb.cat.sizes = Data.size

bf.Decisive = nrow(subset(pval.bf, (BF > 100)))
bf.very.strong = nrow(subset(pval.bf, (BF > 30 & BF <= 100)))
bf.strong = nrow(subset(pval.bf, (BF > 10 & BF <= 30)))
bf.substan.H1 = nrow(subset(pval.bf, (BF > 3 & BF <= 10)))
bf.Anecd.H1 = nrow(subset(pval.bf, (BF > 1 & BF <= 3)))
bf.Anecd.H0 = nrow(subset(pval.bf, (BF > 1/3 & BF <= 1)))
bf.substan.H0 = nrow(subset(pval.bf, (BF > .1 & BF <= 1/3)))

bf.cat.check = sum(bf.Decisive, bf.very.strong, bf.strong, bf.substan.H1, bf.Anecd.H1, bf.Anecd.H0, bf.substan.H0)

p.Decisive = nrow(subset(pval.bf, p <= .001)) 
p.substan = nrow(subset(pval.bf, (p > .001 & p <= .01)))
p.positive = nrow(subset(pval.bf, (p > .01 & p <= .05)))
p.no = nrow(subset(pval.bf, p > .05))

p.cat.check = sum(p.Decisive, p.substan, p.positive, p.no)
  
result = matrix(unlist(pb.cat.sizes), ncol = 4, byrow = TRUE)
dimnames(result) = list(BF = c("Decisive", "Very Strong", "Strong", "Substantial", "Anecdotal", "Anecdotal(H0)", "Substantial(H0)")
                             ,p.value = c("Decisive", "Substantial", "Positive", "None"))

p.marginal = matrix(c(p.Decisive, p.substan, p.positive, p.no), nrow = 1)
bf.marginal = matrix(c(bf.Decisive, bf.very.strong, bf.strong, bf.substan.H1,
                       bf.Anecd.H1, bf.Anecd.H0, bf.substan.H0, bf.cat.check), ncol = 1)

X.sq = chisq.test(result, correct = FALSE)
CramerV = unname(X.sq[[1]]) / (Data.size * 3)

chisq.table = cbind(rbind(result, p.marginal), bf.marginal)
dimnames(chisq.table) = list(BF = c("Decisive", "Very Strong", "Strong", "Substantial", "Anecdotal", "Anecdotal(H0)", "Substantial(H0)", "Marginal.p")
                             ,p.value = c("Decisive", "Substantial", "Positive", "None", "Marginal.BF"))

# Regular expression to create all 28 plot names:
plot.names = noquote(sprintf("p.b%d.%d", 1:7, rep(1:4, each = 7)))

# Ready the device:
original_par = par(no.readonly = TRUE)
on.exit(par(original_par))

par(mfcol = c(7, 4), mar = rep(.08, 4), oma = rep(9, 4))

## Multiple plotting:
invisible(lapply(mget(plot.names), function(pb) 
  if(nrow(pb) == 0){
    plot.new(); box()
  }else{
    plot(pb, pch = 21, bg = 3, cex = 1.5, xaxt = "n", yaxt = "n")
  }))

## Advanced Labeling:
par(xpd = NA)
mid.x = seq(grconvertX(0 + (1 /  8), "nic"), grconvertX(1 - (1 /  8), "nic"), l = 4)
mid.y = seq(grconvertY(0 + (1 / 14), "nic"), grconvertY(1 - (1 / 14), "nic"), l = 7)

gap.x = sort(c(mid.x[-length(mid.x)] + diff(mid.x)[1L] / 2, grconvertX(0:1, "nic")))
gap.y = sort(c(mid.y[-length(mid.y)] + diff(mid.y)[1L] / 2, grconvertY(0:1, "nic")))

xlim = grconvertX(0:1, "nic")
ylim = grconvertY(0:1, "nic")

arrows(rep(gap.x[1]-.57, 2), c(mid.y[3], mid.y[2]), rep(gap.x[1]-.57, 2), c(mid.y[4], mid.y[1]), code = 2, length = .075, angle = 20)

text(rep(gap.x[1]-.62, 2),  c(gap.y[2], gap.y[4]), c(expression(bolditalic("Support for H"[0])), expression(bolditalic("Support for H"[1]))), srt = 90, pos = 3)

v = c(Substantial = bf.substan.H0, Anecdotal = bf.Anecd.H0, Anecdotal = bf.Anecd.H1, Substantial = bf.substan.H1, 
      Strong=bf.strong, "Very Strong" = bf.very.strong, Decisive = bf.Decisive)/Data.size*1e2
l = paste0(names(v), "\n", decimal(v, 2), "%")       

mtext(l, side = 4, at = mid.y, las = 1, line = .2, cex = .7, font = 2)

v = c(Decisive= p.Decisive, Substantial= p.substan, Positive= p.positive, None= p.no)/Data.size*1e2
l = paste0(names(v), "\n", decimal(v, 2), "%")

text(mid.x, ylim[2] + .05, l, cex = 1.1, font = 2)

l = c('0', '.001', '.01', '.05', '1') 
mtext(l, side = 1, at = gap.x, line = .55, cex = .9, font = 2)

abline(h = gap.y[3], col = 2, lty = 2) # line of indecision

arrows(mid.x[3], gap.y[3], mid.x[3], gap.y[5]-.01, code = 3, col = rgb(1,0,0, .65), lwd = 2, angle = 20, length = .1)

l = c('1/10', '1/3', '1', '3', '10', '30', '100', '> 1000000')
text(xlim[1], gap.y, l, adj = 1.05, cex = 1.3, font = 2)

mtext(bquote(bolditalic("p")*bold("-value")), side = 1, at = mean(gap.x), line = 3.5, cex = 1.2)
text(xlim[1], mean(gap.y), "Bayes Factor", adj = c(.5, -2.2), cex = 1.8, font = 2, srt = 90)
text(mean(gap.x), ylim[2] + .16, bquote(bold("Evidence against"~bolditalic(H)[0])), cex = 1.8)
 
tt = subset(pval.bf, (p > .01 & p <= .05))

list(chisq.table = chisq.table, X.sq = X.sq, CramerV = CramerV, "p.values(.01-.05)_BFs(Anecdotal[H1]):" = 
paste0(round(length(which(tt$BF <= 3 & tt$BF >= 1)) / nrow(tt)*1e2, 2), "%"), "p.values(.01-.05)_BFs(Anecdotal[H0]):" = 
paste0(round(length(which(tt$BF >= 1/3 & tt$BF <= 1)) / nrow(tt)*1e2, 2), "%"))
}
