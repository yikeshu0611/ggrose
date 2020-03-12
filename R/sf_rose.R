#' @title Simple feature style Rose Map
#'
#' @param mapping mapping
#' @param data data
#' @param translate translate, must be two numbers. The first number is given to move along x-axis, the other is y-axis
#' @param rotate rotate
#' @param mirror mirror, must be two numbers. The first number is slope, the other is intercept
#' @param mirror_h one number, mirror refelct by horizontal line
#' @param mirror_v one number, mirror refelct by vertical line
#' @param zoom one number
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... passed to params
#' @importFrom ggplot2 ggproto aes Stat layer_sf GeomSf coord_sf
#' @importFrom sf666 zoom %txy% mirror mh mv
#' @importFrom sf st_as_sf st_coordinates st_bbox
#' @name sfROse
#' @return Simple feature picture
#' @export
#'
#' @examples
#'
#' librar(ggrose)
#' dd=nCov2019()
#' dd2=rose_data(dd)
#'
#' # original data
#' ggplot(data = dd2)+
#'     sf_rose()
#'
#'
#' # add petal
#' ggplot(data = dd2)+
#'     sf_rose(aes(petal=diagnose))
#'
#' # add petal
#' # transform diagnose to log10
#' ggplot(data = dd2)+
#'     sf_rose(aes(petal=log10(diagnose)))
#'
#'
#' # add color
#' ggplot(data = dd2)+
#'     sf_rose(aes(fill=log10(diagnose)))
#'
#' # use color and petal together
#' ggplot(data = dd2)+
#'     sf_rose(aes(fill=log10(diagnose),
#'                 petal=log10(diagnose)))
#'
#' # plot two roses and translate the second 1.6 anlong x-axis
#' ggplot(data = dd2)+
#'     sf_rose(aes(petal=log10(diagnose)))+
#'     sf_rose(aes(petal=log10(diagnose)),
#'             translate = c(1.6,0))
#'
#' # rotate
#' ggplot(data = dd2)+
#'     sf_rose(aes(fill=log10(diagnose),
#'                 petal=log10(diagnose)),
#'             rotate = 90)
#'
#'
#' # mirror reflecte
#' ggplot(data = dd2)+
#'     sf_rose(aes(petal=log10(diagnose)))+
#'     sf_rose(aes(petal=log10(diagnose)),
#'             translate = c(1.6,0),fill='blue',
#'             mirror_v = 2)
#'
#' # zoom
#' ggplot(data = dd2)+
#'     sf_rose(aes(petal=log10(diagnose)))+
#'     sf_rose(aes(petal=log10(diagnose)),
#'             translate = c(1.6,0),fill='blue',
#'             zoom=0.5
#'             )
#'
#' # mirror reflecte and regulate start angle
#' ggplot(data = dd2)+
#'     sf_rose(aes(petal=log10(diagnose)),rotate = -30)+
#'     sf_rose(aes(petal=log10(diagnose)),rotate = -30,
#'             translate = c(2,0),fill='blue',
#'             mirror_v = 2)
sf_rose <- function (mapping = aes(petal=NULL), data = NULL,
                     translate=c(0,0),
                     rotate=0,
                     mirror=NULL,mirror_h=NULL,mirror_v=NULL,
                     zoom=1,
                     position = "identity",
                     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
{
    c(layer_sf(geom = GeomSf, data = data, mapping = mapping,
               stat = StatSfRose, position = position, show.legend = if (is.character(show.legend)) TRUE else show.legend,
               inherit.aes = inherit.aes,
               params = list(na.rm = na.rm,
                             legend = if (is.character(show.legend)) show.legend else "polygon",
                             translate=translate,rotate=rotate,zoom=zoom,
                             mirror_h=mirror_h,mirror=mirror,mirror_v=mirror_v,
                             ...)),
      coord_sf(default = TRUE))
}
#' @export
#' @rdname sfROse
StatSfRose <- ggproto("StatSf", Stat,
                      compute_group = function(data, scales,translate,
                                               rotate,mirror,mirror_h,mirror_v,
                                               zoom) {
                          #data enter as dataframe class
                          if ('petal' %in% colnames(data)){
                              ratio=data[,'petal']/max(data[,'petal'])
                              data=st_as_sf(data)
                              center=c(st_coordinates(data)[1,1],st_coordinates(data)[1,2])
                              data=sf666::zoom(sf = data,ratio = ratio,center = center)
                              #translate
                              data = data %txy% translate
                              #ratate
                              center=c(st_coordinates(data)[1,1],st_coordinates(data)[1,2])
                              data = sf666::rotate(sf = data,center = center,angel = rotate)
                              #mirror reflect
                              if (!is.null(mirror)) data=mirror(sf = data,k = mirror[1],b = mirror[2])
                              if (!is.null(mirror_h)) data=mh(sf = data,h = mirror_h)
                              if (!is.null(mirror_v)) data=mv(sf = data,v = mirror_v)
                              #zoom
                              center=c(st_coordinates(data)[1,1],st_coordinates(data)[1,2])
                              data=sf666::zoom(sf = data,ratio = zoom,center = center)
                          }
                          data=st_as_sf(data)
                          bbox <- st_bbox(data)
                          data$xmin <- bbox[["xmin"]]
                          data$xmax <- bbox[["xmax"]]
                          data$ymin <- bbox[["ymin"]]
                          data$ymax <- bbox[["ymax"]]
                          as.data.frame(data)
                      },
                      default_aes = aes(petal=NULL),
                      required_aes = c("geometry")
)
