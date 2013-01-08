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
for(i in 1:32) {
segments(1, 33 - i, 8, 33 - i, col = i, lwd = i/2, lty = i)
text(0.7, 33 - i, i, adj = 1, col = i)
}
for(i in 0:16) {
rect(10, 32 - 2 * i, 15, 32 - 2 * i + 2, col = i, border = T)
rect(10, 32 - 2 * i, 15, 32 - 2 * i + 2, col = (i == 0), density
 = 0, border = T)
text(9.7, 32 - 2 * i + 1, i)
}
}
