demo.bmi_function(){
dbmi_read.table("bmi.txt",header=T) # preberi podatke
print(names(dbmi)   )   # seznam spremenljivk
#edit(dbmi)     # pokazi podatke v data editorju
print(dim(dbmi)     )# dimenzije podatkov
print(summary(dbmi) )   # pregled vseh spremenljivk v tabeli
print(summary(dbmi[,-1]))   # izpusti identifikacijsko stevilko
print(apply(dbmi[,c("starost","visina","teza")],2,summary)) # izbira spremenljivk
print(apply(dbmi[,c("starost","visina","teza")],2,mean))
print(apply(dbmi[,c("starost","visina","teza")],2,var))
##
attach(dbmi)        # privzemi spremenljivke iz bmi
print(attributes(spol)) # lastnosti
print(levels(spol))     # nivoji za faktor
print(mean(spol))       # opisne spremenljivke nimajo povpre�ja
boxplot(split(visina,spol)) 
boxplot(split(visina,spol),col="lightblue") # pobarvana skatla
boxplot(split(visina,spol),col="lightblue",boxwex=.2) # o�je �katle
boxplot(split(teza,as.numeric(spol)*1000+starost),col="lightgreen",boxwex=.5)
boxplot(split(visina,spol),col="pink",boxwex=.2,ylim=c(100,200))
#
#
boxplot(split(teza,as.numeric(spol)*1000+starost),col="lightgreen",boxwex=.5,main="Te�a")
win.graph()
boxplot(split(teza,as.numeric(spol)*1000+starost),col="pink",boxwex=.5,main="Vi�ina")
#

vt_lsfit(visina,teza)   # metoda najmanjsih kvadratov (LS)
print(names(vt))
print(vt$c  )       # koeficienti
#
## korelacijski grafikon na razne nacine
plot(visina,teza)   
plot(visina,teza,pch=as.character(spol))
plot(visina,teza,col=as.numeric(spol))
plot(visina,teza,pch=as.character(spol),col=as.numeric(spol))
s.id_as.numeric(spol)
plot(visina,teza,col=c(4,5)[s.id])
plot(visina,teza,col=c(4,"red")[s.id],cex=1.2,pch=16)
plot(visina,teza,col=c(4,"red")[s.id],cex=1.2,pch=c(1,16)[s.id])
# dodaj debelo modro regresijsko premico
abline(lsfit(visina,teza),lwd=5,col="blue") 
# pa se tanko rdeco :)
abline(lsfit(visina,teza),col="red")        
#
#
bmi_teza/(visina^2/10000)   # izra�unam bmi
print(summary(bmi))
boxplot(split(bmi,spol),col="green")    # bmi glede na spol
print(quantiles(bmi,c(0.05,0.25,0.75,0.95)))    # kvantili
#
 bmic_cut(bmi,c(0,13,18,25,30,70))  # "recode"

print(levels(bmic))         # nivoji fakotrja
print(as.numeric(bmic))     # kot stevilke      
# nova imena nivojev
levels(bmic)_c("S","s","N","d","D")         
# naj bo urejen (ordinalen) 
bmic_factor(bmic,levels=c("S","s","N","d","D"),ordered=T)   
print(is.ordered(bmic))     # preveri, ce je res
print(table(spol,bmic))     # tabela kategorij glede na spol

dbmi_cbind(dbmi,bmi,bmic)   # zlepimo stolpce osnovnih podatkov in bmi

coplot(teza~visina|spol)
pairs(dbmi[,4:7])
pairs(dbmi[,4:7],col=s.id)
pairs(dbmi[,4:7],col=c("red","blue")[s.id])
#
#
coplot(bmi~visina|spol+starost)
#
}
