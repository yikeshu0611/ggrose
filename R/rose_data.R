#' Prepare Data for Rose
#'
#' @param data data
#' @importFrom sf666 sf_polygon
#' @return sf
#' @export
#'
rose_data <- function(data){
    center=c(0,0)
    r=1
    n=1000
    #generate square coordinate
    dd1=lapply(1:n, function(i) c(r*cos((i*360/n)/180*pi)+center[1],
                                  r*sin((i*360/n)/180*pi)+center[2]))
    dd2=do.call(rbind,dd1)
    colnames(dd2)=c('x','y')
    # distribute coord for each class
    cut=round(seq(1, n, by = n/nrow(data)))
    for (i in 2:length(cut)) {
        if (i==2) ply=NULL
        ddi=dd2[cut[i-1]:cut[i],]
        if (i==2) ddi=rbind(dd2[nrow(dd2),],ddi)
        ddi=rbind(center,ddi,center)
        plyi=sf_polygon(ddi)
        ply=rbind(ply,plyi)
        if (i==length(cut)){
            ddi=dd2[cut[i]:nrow(dd2),]
            ddi=rbind(center,ddi,dd2[nrow(dd2),],center)
            plyi=sf_polygon(ddi)
            ply=rbind(ply,plyi)
        }
    }
    if (ncol(data)>=2){
        for (i in 1:ncol(data)) {
            ply=cbind(data[,i],ply)
        }
        colnames(ply)[1:ncol(data)]=rev(colnames(data))
    }
    ply
}
