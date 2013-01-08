"show.colors" <-
function (kaj=1:length(colors()),cex=3) 
{
n_length(kaj)
nc_sqrt(n)+1
colid_ matrix(kaj,ncol=nc)
y_outer(nc:1,rep(1,nc))
x_outer(rep(1,nc),1:nc)
kajc_c(kaj,rep(1,nc*nc-n))

plot(x,y,col=colors()[kajc],pch=15,cex=cex,axes=F,xlab="",ylab="")
kajc_c(rev(kaj),rep(1,nc*nc-n))
text(x,y,kaj,col=colors()[kajc],cex=cex/6)
}
"show.pch" <-
function()
{
win.graph()
oldpar <- par(mar = c(1, 1, 1, 1) * 0.5)
on.exit(par(oldpar))
plot(c(0, 16), c(0, 32), type = "n", axes = F, xlab = "", ylab = "")
for(i in 0:255) {
points(i %% 16, 32 - (i %/% 16) * 2, pch = i, cex = 1.5)
text(i %% 16, 32 - (i %/% 16) * 2 - 0.75, as.character(i), cex = 
0.5)
}
win.graph()
plot(c(0, 16), c(0, 34), type = "n", axes = F, xlab = "", ylab = "")
text(0.7, 33 , "lwd", adj = 1)
text(8.3,33,"lty",adj=0)
for(i in 1:32) {
segments(1, 33 - i, 8, 33 - i, col = i, lwd = i/2, lty = i)
text(0.7, 33 - i, i/2, adj = 1, col = i)
text(8.3,33-i,(i-1)%%6+1,adj=0,col=i)
}
text(9.7, 34, "col")
text(15.5,34,"cex")
for(i in 0:16) {
rect(10, 32 - 2 * i, 15, 32 - 2 * i + 2, col = i, border = T)
rect(10, 32 - 2 * i, 15, 32 - 2 * i + 2, col = (i == 0), density
 = 0, border = T)
text(9.7, 32 - 2 * i + 1, i)
text(15.5,32 - 2 * i+1,i/5,cex=i/5)
}
}
