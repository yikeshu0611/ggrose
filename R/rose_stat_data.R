rose_stat_data <- function(data,petal='oooooooooo',
                           n,center,
                           translate.1,
                           trans_h.2,
                           trans_v.3,
                           trans_al.4,
                           rotate.5,
                           zoom.6,
                           mirror.7,
                           mirror_h.8,
                           mirror_v.9,
                           round,
                           inner.r,
                           outer.r,
                           inner.petal,start_angle
                           ){
    origion=data
    origion$i=1:nrow(origion)
    if (petal %in% colnames(data)){
        string=format(round(abs(data[,petal])),scientific = FALSE)
        string=unique(string)
        if (length(string) ==1 &
            string[1] == 0){
            index=0
        }else{
            index=max(nchar(string))
        }
        ratio=data[,petal]/10^index
    }else{
        ratio=rep(1,nrow(data))
    }
    #generate square coordinate
    dd1=lapply(1:n, function(i) c(cos((i*round/n)/180*pi)*outer.r+center[1],
                                  sin((i*round/n)/180*pi)*outer.r+center[2]))
    dd2=do.call(rbind,dd1)
    colnames(dd2)=c('x','y')
    #inner cricle
    dd3=lapply(1:n, function(i) c(cos((i*round/n)/180*pi)*inner.r+center[1],
                                  sin((i*round/n)/180*pi)*inner.r+center[2]))
    dd4=do.call(rbind,dd3)
    # distribute coord for each class
    cut=round(seq(1, n, by = n/nrow(data)))
    for (i in 2:(length(cut)+1)) {
        # generate ddo
        if (i==2) ply=NULL
        a=cut[i-1]
        b=cut[i]
        if (i==(length(cut)+1)) b=n
        cut;a;b
        ddo=dd2[a:b,]
        if (i==2){
            if (round==360) ddo=rbind(dd2[nrow(dd2),],ddo)
        }
        # inner
        ddi=dd4[a:b,]
        if (i==2){
            if (round==360) ddi=rbind(dd4[nrow(dd4),],ddi)
        }
        #rev ddi
        ddi[,1]=rev(ddi[,1])
        ddi[,2]=rev(ddi[,2])
        rownames(ddi)=paste0('ddi',a:(a+nrow(ddi)-1))
        rownames(ddo)=paste0('ddo',a:(a+nrow(ddo)-1))
        #rotate by start_angle
        ddi=rotate(df=ddi,center,start_angle)
        ddo=rotate(df=ddo,center,start_angle)
        # stuff
        ddoi=rbind(center,ddo,ddi,center)
        # ratio each part
        x=(ddoi[,1]-center[1])*ratio[i-1]+center[1]
        y=(ddoi[,2]-center[2])*ratio[i-1]+center[2]
        # inner.petal means whether we do petal for inner
        if (!inner.petal){
            judge=left(names(x),3)=='ddi'
            x[judge]=ddi[,1]
            y[judge]=ddi[,2]
        }
        plyi=cbind(i=i-1,x=x,y=y)
        ply=rbind(ply,plyi)
        #if (i==(length(cut)+1)) rownames(ply)=NULL
    }
    ply_xy=ply[,c('x','y')]
    # center=c(ply_xy[1,1],ply_xy[1,2])
    #translate.1 trans_h.2 trans_v.3 trans_al.4
    if (!is.null(translate.1)) ply_xy=translate(ply_xy,translate.1)
    if (!is.null(trans_h.2))   ply_xy=trans_h(ply_xy,trans_h.2)
    if (!is.null(trans_v.3))   ply_xy=trans_v(ply_xy,trans_v.3)
    if (!is.null(trans_al.4))  ply_xy=trans_al(ply_xy,trans_al.4)
    #rotate.5
    center=c(ply_xy[1,1],ply_xy[1,2])
    if (rotate.5 != 0)         ply_xy=rotate(df=ply_xy,center,rotate.5)
    #zoom.6
    center=c(ply_xy[1,1],ply_xy[1,2])
    if (!is.null(zoom.6))      ply_xy=zoom(df=ply_xy,center,zoom.6)
    #mirror.7
    if (!is.null(mirror.7))    ply_xy=mirror(ply_xy,mirror.7)
    if (!is.null(mirror_h.8))  ply_xy=mirror_h(ply_xy,mirror_h.8)
    if (!is.null(mirror_v.9))  ply_xy=mirror_v(ply_xy,mirror_v.9)
    ply[,c('x','y')]=ply_xy
    ply=ply[rownames(ply)!='center',]
    data=merge(x=origion,y=ply,by = 'i')
    data
}
