rose_stat_data <- function(data,petal='oooooooooo'){
    origion=data
    origion$i=1:nrow(origion)
    if (petal %in% colnames(data)){
        ratio=abs(data[,petal]/max(data[,petal]))
    }else{
        ratio=rep(1,nrow(data))
    }
    center=c(0,0)
    n=1000
    #generate square coordinate
    dd1=lapply(1:n, function(i) c(cos((i*360/n)/180*pi),
                                  sin((i*360/n)/180*pi)))
    dd2=do.call(rbind,dd1)
    colnames(dd2)=c('x','y')
    # distribute coord for each class
    cut=round(seq(1, n, by = n/nrow(data)))
    for (i in 1:length(cut)) {
        if (i==1) ply=NULL
        a=cut[i]
        b=cut[i+1]
        cut
        a
        b
        if (i==length(cut)) b=n
        ddi=dd2[a:(b),]
        if (i==1) ddi=rbind(dd2[nrow(dd2),],ddi)
        ddi=ddi*ratio[i]
        ddi=rbind(center,ddi,center)
        plyi=cbind(i=i,ddi)
        ply=rbind(ply,plyi)
    }
    rownames(ply)=NULL
    data=merge(x=origion,y=ply,by = 'i')
    data
}
