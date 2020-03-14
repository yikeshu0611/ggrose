#' Rose Plot
#'
#' @param mapping mapping
#' @param data data, must be a dataframe
#' @param center center coordinate, must be two numbers
#' @param n the number of points, which will build a circle to help locate the text
#' @param round the round of map, which should be 0-360, default is 360.
#' @param translate.1 Operation to whole text, which is should be two numbers for x and y values
#' @param trans_h.2 Operation to whole text, which is should be one number
#' @param trans_v.3 Operation to whole text, which is should be one number
#' @param trans_al.4 Operation to whole text, which is should be two numbers for slope and intercept values
#' @param rotate.5 Operation to whole text, which is should be one number for angle
#' @param zoom.6 Operation to whole text, which is should be one number for ratio
#' @param mirror.7 Operation to whole text, which is should be two numbers for slope and intercept values
#' @param mirror_h.8 Operation to whole text, which is should be one number
#' @param mirror_v.9 Operation to whole text, which is should be one number
#' @param ... passed to parameters
#' @param na.rm default is FALSE
#' @param show.legend default is FALSE
#' @param inherit.aes default is TRUE
#' @param outer.r the radius of outer circle, default is 1
#' @param inner.r the radius of inner circle, default is 0
#' @param inner.petal a logical object. Defalut is FLASE. Whether do petal on inner circle.
#' @param start_angle start angle for rose, defalut is 0
#'
#' @importFrom do left
#' @return ggplot2 style picture
#' @export
#'
#' @examples
#' library(ggrose)
#' set.seed(2020)
#' df=data.frame(r=rnorm(30))
#'
#'
#' ggplot(df)+
#'     stat_circle(color='black',fill=NA)+
#'     stat_circle(r=1,color='black',fill=NA)+
#'     stat_circle(r=2,color='black',fill=NA)
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
                       outer.r=1,inner.r=0,inner.petal=FALSE,
                       start_angle=0,
                       ...){
    layer(data = data, mapping = mapping, stat = StatRose, geom = GeomPolygon,
          position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, rule = "evenodd",
                        n=n,center=center,round=round,inner.r=inner.r,start_angle=start_angle,
                        translate.1=translate.1,trans_h.2=trans_h.2,trans_v.3=trans_v.3,
                        rotate.5=rotate.5,zoom.6=zoom.6,
                        mirror.7=mirror.7,mirror_h.8=mirror_h.8,mirror_v.9=mirror_v.9,
                        trans_al.4=trans_al.4,
                        inner.petal=inner.petal,outer.r=outer.r,
                        ...))
}
StatRose <- ggproto("StatRose", Stat,
                    default_aes = aes(x = after_stat(x),
                                       y = after_stat(y),
                                       petal=NULL,
                                      group=after_stat(i)),
                    compute_group = function(scales, data,
                                             n,center,round,inner.r,outer.r,start_angle,
                                             translate.1,trans_h.2,trans_v.3,trans_al.4,
                                             rotate.5,zoom.6,
                                             mirror.7,mirror_h.8,mirror_v.9,
                                             inner.petal){
                        data2=rose_stat_data(data,'petal',
                                             n=n,center=center,start_angle=start_angle,
                                             round=round,inner.r=inner.r,outer.r=outer.r,
                                             translate.1=translate.1,trans_h.2=trans_h.2,trans_v.3=trans_v.3,trans_al.4=trans_al.4,
                                             rotate.5=rotate.5,zoom.6=zoom.6,
                                             mirror.7=mirror.7,mirror_h.8=mirror_h.8,mirror_v.9=mirror_v.9,
                                             inner.petal=inner.petal)
                        data2$group=data2$i
                        data2
                    }
                    )
