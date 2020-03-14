#' Plot Circle
#'
#' @param mapping mapping
#' @param data data
#' @param na.rm na rm, defalut is FALSE
#' @param center center coordinate, must be two numbers, default is 0,0
#' @param n the number of points, which will build a circle to help locate the text
#' @param round the round of map, which should be 0-360, default is 360.
#' @param r radius, default is 1
#' @param show.legend show legend
#' @param inherit.aes inherit aes
#' @param ... passed parameters
#' @importFrom do left
#' @return ggplot2 style picture
#' @export
#'
#' @examples
#' library(ggrose)
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
stat_circle <- function (mapping = aes(),
                       data = NULL,
                       center=c(0,0),r=0.5,n=1000,round=360,
                       na.rm=FALSE,
                       show.legend = NA,inherit.aes=TRUE,
                       ...){
    layer(data = data, mapping = mapping, stat = StatCircle,
          geom = GeomPolygon,
          position = "identity", show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, rule = "evenodd",
                        n=n,center=center,r=r,round=round,
                        ...))
}
StatCircle <- ggproto("StatCircle", Stat,
                    default_aes = aes(x = after_stat(x),
                                      y = after_stat(y)),
                    compute_group = function(scales,data,
                                             n,center,r,round){

                        dd1=lapply(1:n, function(i) c(cos((i*round/n)/180*pi)*r+center[1],
                                                      sin((i*round/n)/180*pi)*r+center[2]))
                        dd2=do.call(rbind,dd1)
                        colnames(dd2)=c('x','y')
                        dd2
                    }
)
