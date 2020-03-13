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
                           round
                           ){
    origion=data
    origion$i=1:nrow(origion)
    if (petal %in% colnames(data)){
        ratio=abs(data[,petal]/max(data[,petal]))
    }else{
        ratio=rep(1,nrow(data))
    }
    #generate square coordinate
    dd1=lapply(1:n, function(i) c(cos((i*round/n)/180*pi)+center[1],
                                  sin((i*round/n)/180*pi)+center[2]))
    dd2=do.call(rbind,dd1)
    colnames(dd2)=c('x','y')
    # distribute coord for each class
    cut=round(seq(1, n, by = n/nrow(data)))
    for (i in 2:(length(cut)+1)) {
        # generate ddi
        if (i==2) ply=NULL
        a=cut[i-1]
        b=cut[i]
        if (i==(length(cut)+1)) b=n
        cut;a;b
        ddi=dd2[a:b,]
        if (i==2){
            if (round==360) ddi=rbind(dd2[nrow(dd2),],ddi)
        }
        ddi=rbind(center,ddi,center)
        # ratio each part
        x=(ddi[,1]-center[1])*ratio[i-1]+center[1]
        y=(ddi[,2]-center[2])*ratio[i-1]+center[2]
        ddi=data.frame(x=x,y=y)
        plyi=cbind(i=i-1,ddi)
        ply=rbind(ply,plyi)
        if (i==(length(cut)+1)) rownames(ply)=NULL
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
    data=merge(x=origion,y=ply,by = 'i')
    data
}
