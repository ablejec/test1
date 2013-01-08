X=matrix(1:20,ncol=4)
X
X=matrix(1:20,4,5)
X
X[2,]
X[,3]
X[,2:3]
dimnames(X)
list(paste("V",1:4),paste("S,1:5))
)
list(paste("V",1:4),paste("S",1:5))
list(paste("V",1:4,coll=""),paste("S",1:5))
list(paste("V",1:4,sep=""),paste("S",1:5))
list(paste("V",1:4,sep=""),paste("S",1:5,sep="-"))
list(paste(c("V","v"),1:4,sep=""),paste("S",1:5,sep="-"))
"a":"z"
letters()
letter()
apropos("letter")
lettersd
letters
Letters
LETTERS
list(paste("V",1:4,sep=""),paste("S",1:5,sep="-"))
dimnames(X)=list(paste("V",1:4,sep=""),paste("S",1:5,sep="-"))
X
dimnames(X)
dimnames(X)[[1]]
dimnames(X)[[1]]=NULL
dimnames(X)[[1]]=1:4
X
dimnames(X)=list(paste("V",1:4,sep=""),paste("S",1:5,sep="-"))
X
write.table("bla.xls",col.names=F,row.names=F,sep="\t")
getwd()
unlink("bla.xls")
write.table("bla.xls",col.names=F,row.names=F,sep="\t")
write.table("bla.xls",sep="\t")
write.table(X,file="bla.xls",col.names=F,row.names=F,sep="\t")
write.table(X,file="bla.xls",sep="\t")
write.table(X,file="bla.xls",sep="\t")
write.table(X,file="bla.xls",row.names=NA,sep="\t")
write.table(X,file="bla.xls",col.names=NA,sep="\t")
X
list("Tole je poskusna datoteka),dimenzija=dim(X),podatki=X)
)
list("Tole je poskusna datoteka"),dimenzija=dim(X),podatki=X)
list("Tole je poskusna datoteka",dimenzija=dim(X),podatki=X)
Y=X
Y
Y=matrix(round(runif(20)),4,5)
Y
dimnames(Y)=dimnames(X)
Y
i=1
Y[1,]
vrste=dimnames(Y)[[2]]
vrste
vrste[Y[i,]]
vrste[c(T,F,T)]
which(Y[i,]==1)
Y[i,]<>1
Y[i,]>1
Y[i,]<1
Y[i,]==1
vrste[Y[i,]==1]
prisotne=(Y[i,]==1)
prisotne
vrste[prisotne]
for(i=1:4) prisotne=(Y[i,]==1);vrste[prisotne]
for(i in 1:4) prisotne=(Y[i,]==1);vrste[prisotne]
Y
for(i in 1:4) 
{print(i)
prisotne=(Y[i,]==1)
vrste[prisotne]
}
for(i in 1:4) 
{print(i)
prisotne=(Y[i,]==1)
print(vrste[prisotne])
}
l[[1]]
l
rm(l)
l[[1]]
l=list()
l[[1]]
?read.table
seznam=list(4)
seznam
?list

##
## naredi seznam vrst iz datoteke prisotnosti
seznamVrst <-
function(Y){
regije=dimnames(Y)[[1]]
vrste=dimnames(Y)[[2]]
nregij=dim(Y)[1]
nvrst=dim(Y)[2]
seznam=list()
for(i in 1:nregij) 
{print(regije[i])
prisotne=(Y[i,]==1)
seznam[[i]]=vrste[prisotne]
}
seznam$regije=regije
seznam
}

V=seznamVrst(Y)
V