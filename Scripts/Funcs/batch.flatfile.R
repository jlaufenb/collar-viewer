
batch.flatfile <- function(accounts){
    ## Create directory paths to GPS data from different metOcean accounts
    for(i in 1:length(accounts)){
        accounts[i] = paste0("../Data/GPS/Telonics/",accounts[i],"/Reports")
    }
    ## load GPS data file names
    files = unlist(lapply(accounts, function(x)list.files(x, pattern="Condensed", full=TRUE)))
    ## number of GPS data files
    nfiles = length(files)
    ## extract all CTNs
    fs = lengths(regmatches(files,gregexpr("/",files)))[1]
    ctns = substr(read.table(text=files,sep="/",as.is=TRUE)[,fs+1],1,7)
    ## process and aggregate all GPS data
    out <- do.call("rbind", lapply(files, flat.format))
    return(out)
}
