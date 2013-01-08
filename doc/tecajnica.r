"tecajnica" <-
function (CC="USD",d.low = "01.05.2003",
d.high = "31.10.2003") 
{
# Use of daily packed file of exchange rates of the Bank of Slovenia on internet # Matjaz Jeran 01.10.2003 #

# use packed exchange rates on http://www.bsi.si/_data/tecajnice/dtec-l.zip
download.file(url = "http://www.bsi.si/_data/tecajnice/dtec-l.zip",
destfile = paste(tempdir(), "dtec-l.zip", sep = "/"), 
method = "internal", quiet = FALSE, mode="wb", cacheOK = FALSE) # the file should be downloaded in some temp directory (whatever OS)

# unpack zip file on Windows xxxx
zip.unpack(zipname = paste(tempdir(), "Dtec-l.zip", sep = "/"), dest=tempdir()) # get file Dtec-l.txt from packed zip # end of Windows part

# unpack on Linux (How? Probably some administration of R on Linux needed) # zip.file.extract(zipname = "Dtec-l.zip", file = "Dtec-l.txt") # get file Dtec-l.txt from packed zip # end of Linux part

# The rest works on all OSs
exc <- read.table (file = paste(tempdir(), "Dtec-l.txt", sep = "/"), 
dec = ",", skip = 1, header = TRUE, comment.char = "",col.names=c("Num", "Dat", "CCu", "NCu", "Buy", "Mid", "Sel"))

exc$Num <- factor(exc$Num)
exc$NCu <- factor(exc$NCu)
exc$Dat <- strptime (as.character(exc$Dat), format = "%Y%m%d")
print(levels(exc$CCu))

# select desired currency: USD, EUR, CHF, etc.
selected.CCu <- CC

# select low and high date


# get filtered exchange rates
exc.filt_exc[exc$CCu == selected.CCu &
                exc$Dat >= strptime (d.low,  format = "%d.%m.%Y") & 
                exc$Dat <= strptime (d.high, format = "%d.%m.%Y"),]

# make a graph of filtered exchange rates
plot(exc.filt$Dat, exc.filt$Mid, type = "s",
xlab = "Dnevi", ylab = "Vrednost (SIT)")

title (paste ("Dnevna tecajnica", selected.CCu))
invisible(tecaji)

}
