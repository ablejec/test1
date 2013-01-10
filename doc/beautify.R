##
## beautify
##
grda <- function(x=1,y=2,z){ # pa se kaj
#
#testna funkcija
#
print(x)
if(x>0){ #pa se kaj
print(x+y)} # izpise vsoto
else
{catln(x,"Manj�i od 0")} #
z=x+y #sestej
return(z)}

dput(grda)
sf=attributes(grda)
sf=sf[[1]]
n=length(sf)
kjeP=grep("[\{]",sf)
ntabs=apply(outer(1:n,kjeP,">="),1,sum)
kjeM=grep("[\}]",sf)
tabsM=apply(outer(1:n,kje,">="),1,sum)

gsub("[\{]","\{**",sf)
gsub("[\{]","\{\n",sf)



s=as.list(dput(grda))
m=length(s)
arg=unlist(s)[-m]
arg
paste("function(",paste(names(arg),arg,sep=" = ",collapse=" , "),")",collapse="")
