rose_text_data <- function(data,petal='oooooooooo',
                           n=1000,center=c(0,0),
                           outer.r=1.1,round=360,
                           translate.1=NULL,trans_h.2=NULL,trans_v.3=NULL,
                           trans_al.4=NULL,
                           rotate.5=0,
                           zoom.6=NULL,
                           mirror.7=NULL,
                           mirror_h.8=NULL,
                           mirror_v.9=NULL){
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
    dd1=lapply(1:n, function(i) c(RoseAngle=i/n*round,
                                  cos((i/n*round)/180*pi)*outer.r+center[1],
                                  sin((i/n*round)/180*pi)*outer.r+center[2]))
    dd2=do.call(rbind,dd1)
    colnames(dd2)[2:3]=c('x','y')
    cut=round(seq(1, n, by = n/nrow(data)))
    cut=c(cut,n)
    idrow=sapply(1:(length(cut)-1), function(i) round(median(cut[i]:cut[i+1])))
    coord=dd2[idrow,]
    coord[,2]=(coord[,2]-center[1])*ratio+center[1]
    coord[,3]=(coord[,3]-center[2])*ratio+center[2]
    ply_xy=coord[,2:3]
    ply_xy=rbind(center,ply_xy)
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
    coord[,2:3]=ply_xy[-1,]
    data2=cbind(data,coord)
    data.frame(data2)
}
