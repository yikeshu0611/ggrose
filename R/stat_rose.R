library(ggplot2)
#' Rose Plot
#'
#' @param mapping mapping
#' @param data data
#' @param na.rm na rm, defalut is FALSE
#' @param show.legend show legend
#' @param inherit.aes inherit aes
#' @param ... passed parameters
#' @name stat-rose
#' @return ggplot2 style picture
#' @export
#'
#' @examples
#' set.seed(2020)
#' df=data.frame(r=rnorm(30))
#' ggplot(df)+
#'     stat_rose()
#'
#' ggplot(df)+
#'     stat_rose(color='black',fill=NA)
#'
#'
#' ggplot(df)+
#'     stat_rose(aes(petal=r,fill=r))
stat_rose <- function (mapping = aes(),
                       data = NULL,
                       center=c(0,0),
                       translate.1=NULL,trans_h.2=NULL,trans_v.3=NULL,
                       trans_al.4=NULL,
                       rotate.5=0,
                       zoom.6=NULL,
                       mirror.7=NULL,
                       mirror_h.8=NULL,
                       mirror_v.9=NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       n=1000,round=360,
                       ...){
    layer(data = data, mapping = mapping, stat = StatRose, geom = GeomPolygon,
          position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, rule = "evenodd",
                        n=n,center=center,round=round,
                        translate.1=translate.1,trans_h.2=trans_h.2,trans_v.3=trans_v.3,
                        rotate.5=rotate.5,zoom.6=zoom.6,
                        mirror.7=mirror.7,mirror_h.8=mirror_h.8,mirror_v.9=mirror_v.9,
                        trans_al.4=trans_al.4,
                        ...))
}
#' @export
#' @rdname stat-rose
StatRose <- ggproto("StatRose", Stat,
                    default_aes = aes(x = after_stat(x),
                                       y = after_stat(y),
                                       petal=NULL),
                    compute_group = function(scales, data,n,center,round,
                                             translate.1,trans_h.2,trans_v.3,trans_al.4,
                                             rotate.5,zoom.6,
                                             mirror.7,mirror_h.8,mirror_v.9){
                        data2=rose_stat_data(data,'petal',
                                             n=n,center=center,round=round,
                                             translate.1=translate.1,trans_h.2=trans_h.2,trans_v.3=trans_v.3,trans_al.4=trans_al.4,
                                             rotate.5=rotate.5,zoom.6=zoom.6,
                                             mirror.7=mirror.7,mirror_h.8=mirror_h.8,mirror_v.9=mirror_v.9)
                        data2$group=data2$i
                        data2
                    }
                    )
