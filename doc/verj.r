"verjetnostni" <-
function (kje=c((1:5)/10,.05,.01,.001))
{     plot(c(0,10),c(-3,3),type="n",axes=F,xlab="",ylab="")
abline(h=qnorm(1-kje))
abline(h=qnorm(kje))
axis(2,qnorm(kje),labels=kje,las=1)
axis(2,qnorm(1-kje),labels=1-kje,las=1)
abline(v=seq(0,10,.5) )
box()


axis(1,at=seq(0,10,.5),labels=rep("",21))
}
