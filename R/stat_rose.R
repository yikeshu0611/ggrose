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
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...){
    layer(data = data, mapping = mapping, stat = StatRose, geom = GeomPolygon,
          position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, rule = "evenodd", ...))
}
#' @export
#' @rdname stat-rose
StatRose <- ggproto("StatRose", Stat,
                     default_aes = aes(x = after_stat(x),
                                       y = after_stat(y),
                                       petal=NULL),
                    compute_group = function(scales, data){
                        data2=rose_stat_data(data,'petal')
                        data2$group=data2$i
                        data2
                    }
                    )
