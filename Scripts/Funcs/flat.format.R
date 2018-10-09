
flat.format <- function(myfile){
    require(moveHMM)
    ## Create some bookkeeping objects
    levs1 = c("Resolved QFP", "Resolved QFP (Uncertain)",
              "Unresolved QFP", "Missing") # types of GPS fixes
    nlevs1 = length(levs1)
    levs2 = c("Primary","Auxiliary 1","Auxiliary 2","Auxiliary 3") # fix-rate schedule names
    nlevs2 = length(levs2)
    levs3 = c("P","A1","A2","A3")
    ##
    x = readLines(myfile) # read in data as text
    ctn = substr(x[8],5,nchar(x[8]))
    x = read.csv(textConnection(paste0(x[-(1:23)],collapse="\n")),stringsAsFactors=FALSE)
    ## retain location data and convert to dataframe
    attr(x,"CTN") = ctn # assign CTN# as attribute
    x = cbind(rep(ctn,nrow(x)),
              x[,c("GPS.Fix.Time","GPS.Fix.Attempt","GPS.Latitude","GPS.Longitude","Schedule.Set",
                   "GPS.Horizontal.Dilution","GPS.Satellite.Count")])
    dimnames(x)[[2]] = c("id","fixtime","fixtype","lat","lon","fixsched","hdop","nsats")
    x$fixtype[x$fixtype==""] = NA # replace empty values w/ NA
    ## Subset out location data based on desired fix rate(s)
    x = x[x$fixtype %in% levs1 & x$fixsched %in% levs2,] #exclude 'Succeeded' fix type and missing fix schedules
    x$fixtype = factor(x$fixtype,levels=levs1) # convert GPS fix type to factor
    x$fixsched = factor(x$fixsched,levels=levs2) # convert fix-rate schedule names to factor
    ## Format time variables
    x$fixtime = as.POSIXct(round(as.POSIXct(x$fixtime, tz="UTC", format="%Y.%m.%d %H:%M:%S"),"mins"))
    ## Create fixrate variable
    x$fixrate = scheds$Fix.Interval[as.numeric(collprogs[which(collprogs$CTN==ctn),2:5][as.numeric(x$fixsched)])]
    if(nrow(x)==0){
        x = list(NULL)
    } else {
        x = split(x,cumsum(c(1,diff(x$fixrate)!=0)))
        for(i in 1:length(x)){
            names(x)[[i]] = paste0("FixPeriod",i)
            attr(x[[i]],"FixPeriod") = x[[i]]$fixrate[1]
            allft = seq(x[[i]]$fixtime[1],
                        x[[i]]$fixtime[nrow(x[[i]])],
                        by=x[[i]]$fixrate[1]*3600)
            miss = c(0,which(!allft %in% x[[i]]$fixtime))
            rownames(x[[i]]) = 1:nrow(x[[i]])
            if(length(miss)>1){
                n = length(miss)-1
                x[[i]] = rbind(x[[i]],data.frame(id=rep(ctn,n), fixtime=allft[miss],
                                                 fixtype=rep(levs1[5],n), lat=rep(NA,n),
                                                 lon=rep(NA,n), fixsched=rep(NA,n), fixrate=rep(NA,n),
                                                 hdop=rep(NA,n),nsats=rep(NA,n)))
                x[[i]] = x[[i]][order(x[[i]]$fixtime),]
                rownames(x[[i]]) = 1:nrow(x[[i]])
                ind = 1 + cumsum(is.na(x[[i]]$lat))
                not.na = !is.na(x[[i]]$lat)
                xlist = split(x[[i]][not.na,], ind[not.na])
                x[[i]]$fixtype[which(is.na(x[[i]]$fixtype))] <- levels(x[[i]]$fixtype)[4]
            } else {
                n = 0
            } #ifelse
            x[[i]] = cbind(x[[i]],
                           prepData(data.frame(ID=x[[i]]$id, x=x[[i]]$lon,
                                               y=x[[i]]$lat), type="LL")[,c("step","angle")])
        }
    }
    x = do.call("rbind",x)
    return(x)
}
