"scat.2"<-
function(m = 5, n = 20, vx = 1, err = 1, a = 0, b = 1, conf = 0.95, xlim = c(-3 *
	sqrt(vx), 3 * sqrt(vx)), ylim = a + b * xlim, time = 1, npoints = 50, 
	pch = 1, pcex = 1.2, plwd = 2, lty = 4, llwd = 5)
{
	ab <- function(x, y)
	{
		b <- var(x, y)/var(x)
		a <- mean(y) - b * mean(x)
		as.vector(c(a, b))
	}
	conf.reg <- function(x, y, se, ab, n, npoints, conf, col = 4)
	{
		t.p <- 1 - (1 - conf)/2
		x1 <- seq(min(x) * 1.5, max(x) * 1.5, length = npoints)
		x2 <- x1 - mean(x)
		low <- ab[1] + ab[2] * x1 - qt(t.p, n - 2) * se * sqrt(1/n + x2^
			2/(n * var(x)))
		high <- ab[1] + ab[2] * x1 + qt(t.p, n - 2) * se * sqrt(1/n + 
			x2^2/(n * var(x)))
		lines(x1, low, col = col, lwd = 4)
		lines(x1, high, col = col, lwd = 4)
	}
	oldopt <- options(digits = 3)
	on.exit(options(oldopt))	#
	graphics.off()	#win.graph()
#win.graph()
	par(mfrow = c(1, 2))
	print(c(m = m, n = n, vx = vx, err = err, a = a, b = b, conf = conf, 
		xlim = xlim, ylim = ylim))
	vy <- b^2 * vx + err
	cat(b^2 * vx, " + ", err, " = ", vy, "= var(y)\n")
	cat("r^2 = ", format((b^2 * vx)/vy), "\n")
	cat("y = ", a, " + ", b, " * N(0,", (vx), ") + N(0,", (err), ")\n\n")
	mtit <- paste("y = ", a, "+", b, "*N(0,", vx, ")+N(0,", err, ")")
	stit <- paste("n=", n, "  Conf. level =", conf, "  r^2 = ", format((b^2 *
		vx)/vy))
	mtit <- paste(mtit, "\n", stit)
	x <- rnorm(n, sd = sqrt(vx))
	y <- a + b * x + rnorm(n, sd = sqrt(err))
	print(deparse(y))
	print((length(xlim) == 1) & (length(ylim) == 1))
	if((length(xlim) == 1) & (length(ylim) == 1)) {
		plot(x, y, type = "n")
		par1 <- par()
		dev.set(dev.prev())
		plot(x, y, type = "n")
		par2 <- par()
		dev.set(dev.next())
		ab0 <- ab(x, a + b * x)
	}
	else {
		plot(x, y, type = "n", xlim = xlim, ylim = ylim)
		par1 <- par()
		dev.set(dev.prev())
		plot(x, y, type = "n", xlim = xlim, ylim = ylim)
		par2 <- par()
		dev.set(dev.next())
		ab0 <- ab(x, a + b * x)
	}
	title(mtit)
	par(par1)	## dev.set(dev.prev())
	title(mtit)	# , stit, cex = 0.75)
	par(par2)	##	
	dev.set(dev.next())
	abline(ab0, lwd = llwd, lty = lty, col = 2)	#
#
	conf.reg(x, a + b * x, sqrt(err), ab0, length(x), npoints, conf)	#
#
#	pointwise(fit.se, coverage = 0.95)
	for(i in 1:m) {
		x <- rnorm(n, sd = sqrt(vx))
		y <- a + b * x + rnorm(n, sd = sqrt(err))
		cat("\n")
		cat(i, " r^2 = ", format(cor(x, y)^2), "    ")
		bla <- ab(x, y)
		cat(txt <- paste("y =", format(bla[1]), "+", format(bla[2]), 
			"* X\nr^2=", round(cor(x, y)^2, 3)))
		if(i == m) {
			par(par1)	## dev.set(dev.prev())		
			points(x, y, pch = pch, lwd = plwd, cex = pcex)
			abline(bla)	#
#
			text(-2.5, 2.9, txt, adj = 0)
			err.0 <- var(y) * (1 - cor(x, y)^2)
			conf.reg(x, a + b * x, sqrt(err.0), bla, length(x), 
				npoints, conf, col = 4)	#
			abline(ab0, lwd = llwd, lty = lty, col = 2)
			if(i < m) {
#		rnorm(10000)	# pause
#
#  brisi
#		
				if(i < 5) wait(time)
				points(x, y, pch = pch, lwd = plwd, cex = pcex, 
				  col = 0)
				abline(bla, col = 0)	#
#
				conf.reg(x, a + b * x, sqrt(err.0), bla, length(
				  x), npoints, conf, col = 0)
				abline(ab0, lwd = llwd, lty = lty, col = 0)
				text(-2.5, 2.9, txt, col = 0, adj = 0)
				box()
			}
		}
		par(par2)
		dev.set(dev.next())
		abline(bla)
	}
	par(par1)	###
	dev.set(dev.prev())
	points(x, y, pch = pch, lwd = plwd, cex = pcex)
	abline(bla)
	abline(ab0, lwd = llwd, lty = lty, col = 8)
	par(par2)
	abline(ab0, lwd = llwd, col = 0)
	abline(ab0, lwd = llwd, lty = lty, col = 8)	#
#
	par(par1)
	conf.reg(x, a + b * x, sqrt(err.0), bla, length(x), npoints, conf, col
		 = 4)
	bla
}
