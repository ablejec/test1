
showUV=function(a=5,b=5,m=2,n=2,r=2,s=2,p=1,plt=T,test=F,corel=T){
## pokaže relacijo spremenljivk U in V
##
        if(length(a)>1){
            est=a
            b=a[2]
            m=a[3]
            n=a[4]
            r=a[5]
            s=a[6]
            p=a[7]
            a=a[1]
            }
        K=(a^r)/(b^s)
        V=(x^r)/(y^s)
        U=x^r+K*y^s
        #plot(U,V)
        #my.title(est)
        #title(paste("\n\nr=",round(cor(U,V),3)))
        #abline(lsfit(U,V))
        #return(cor(U,V))
    
    lineV=lowess(U,V)
varV=var(lineV$y)/(var(V))
lineU=lowess(V,U)
varU=var(lineU$y)/(var(U))
if(plt){
plot(U,V)
if(is.null(K)) tpar=paste(": a=",round(a,3),", b=",round(b,3),", r=",round(r,3),", s=",round(s,3))
else tpar=paste(": K=",round(K,3),", r=",round(r,3),", s=",round(s,3))
if(corel){
abline(lsfit(U,V),col="red",lwd=3)
title(paste(vir,tpar,"\ncor=",round(cor(U,V),9)),cex=.75)
}
else
{
lines(lineV,col="red",lwd=3)
lines(lineU$y,lineU$x,col="blue",lwd=3)
abline(v=mean(lineU$y))
# abline(v=median(U),col="blue")
abline(h=mean(lineV$y))
# abline(h=median(V),col="blue")
title( paste(vir,tpar,"\nvar lowess: V=",round(varV,7)," U=",round(varU,7)," U+V=",round(varV+varU,7)),cex=.75)
}
}
if(corel)
return((cor(U,V))^2)
else
return(varV+varU)
 
    }    
########################  



llZ <- function(a=5,b=5,m=2,n=2,r=2,s=2,p=1){
## log likelihood za gostoto Z
##
        if(length(a)>1){
            est=a
            b=a[2]
            m=a[3]
            n=a[4]
            r=a[5]
            s=a[6]
            p=a[7]
            a=a[1]
            }
        CC=r*s*gamma(m+n)/(p*a*b*gamma(p*(m+n))*gamma(m)*gamma(n))
        z=CC*(x/a)^(m*r-1)*(y/b)^(n*s-1)*exp(-((x/a)^r+(y/b)^s)^(1/p))
        return(-sum(log(z)))
    }

llZ()
llZ(c(5,5,2,2,2,2,1))

llZ4=function(m=2,n=2,r=2,p=1){
## log likelihood za gostoto Z
##
        if(length(m)>1){
            est=m
            n=m[2]
            r=m[3]
            p=m[4]
            m=m[1]
            }
        pp=(p^p*(m+n-1/r-1/s)^(p-1))
        s=r/alpha
        a=xM/(pp*(m-1/r))^(1/r)
        b=yM/(pp*(m-1/s))^(1/s)
        CC=r*s*gamma(m+n)/(p*a*b*gamma(p*(m+n))*gamma(m)*gamma(n))
        z=CC*(x/a)^(m*r-1)*(y/b)^(n*s-1)*exp(-((x/a)^r+(y/b)^s)^(1/p))
        return(-sum(log(z)))
    }

llZ4()
llZ4(c(2,2,2,1))

#######
my.filled.contour <- 
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
    z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), 
    zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels), 
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
        1), plot.title, plot.axes, key.title, key.axes, asp = NA, 
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, 
    data=NULL,...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
        col = col))
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    if(!is.null(data)) points(data)
    invisible()
}
