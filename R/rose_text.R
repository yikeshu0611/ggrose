#' Add Text to Rose map
#'
#' @param mapping mapping
#' @param data data, must be a dataframe
#' @param center center coordinate, must be two numbers
#' @param outer.r the radius of text circle, default is 1.05
#' @param n the number of points, which will build a circle to help locate the text
#' @param round the round of map, which should be 0-360, default is 360.
#' @param angle_string change angle by string, which should be s1;s2:angle
#' @param angle_id change angle by id, which can be 1:2;3:angle
#' @param coord_string change coordinate by string, which can be s1;s2:0.1;0.2
#' @param coord_id change coordinate by id, which can be 1:2;3:0.1;0.2
#' @param disappear_string disappear by string, which can be s1;s2
#' @param disappear_id disappear by id, which can be 1:2;3
#' @param translate.1 Operation to whole text, which is should be two numbers for x and y values
#' @param trans_h.2 Operation to whole text, which is should be one number
#' @param trans_v.3 Operation to whole text, which is should be one number
#' @param trans_al.4 Operation to whole text, which is should be two numbers for slope and intercept values
#' @param rotate.5 Operation to whole text, which is should be one number for angle
#' @param zoom.6 Operation to whole text, which is should be one number for ratio
#' @param mirror.7 Operation to whole text, which is should be two numbers for slope and intercept values
#' @param mirror_h.8 Operation to whole text, which is should be one number
#' @param mirror_v.9 Operation to whole text, which is should be one number
#' @param position position
#' @param ... passed to parameters
#' @param parse default is FALSE
#' @param check_overlap default is FALSE
#' @param na.rm default is FALSE
#' @param show.legend default is FALSE
#' @param angle default is NA
#' @param inherit.aes default is TRUE
#' @importFrom do left
#' @importFrom ggplot2 Geom GeomPolygon aes layer
#' @importFrom stats median
#' @importFrom grid textGrob gpar
#' @return text
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
#'     stat_rose(aes(petal=r,fill=r))+
#'     rose_text(aes(label=round(r)))
#'
#' ggplot(df)+
#'     stat_rose(aes(petal=r,fill=r))+
#'     rose_text(aes(petal=r,label=round(r)))
rose_text <- function (mapping = NULL, data = NULL,
                        center=c(0,0),outer.r=1.05,n=1000,round=360,
                       angle_string=NULL,
                       angle_id=NULL,
                       coord_string=NULL,
                       coord_id=NULL,
                       disappear_string=NULL,
                       disappear_id=NULL,

                       translate.1=NULL,trans_h.2=NULL,trans_v.3=NULL,
                       trans_al.4=NULL,
                       rotate.5=0,
                       zoom.6=NULL,
                       mirror.7=NULL,
                       mirror_h.8=NULL,
                       mirror_v.9=NULL,

                       position = "identity", ..., parse = FALSE,
                       check_overlap = FALSE, na.rm = FALSE,
                       show.legend = NA,angle=NA,
                       inherit.aes = TRUE){
        layer(data = data, mapping = mapping, stat = StatRoseText,
              geom = GeomRoseText,
              position = position, show.legend = show.legend, inherit.aes = inherit.aes,
              params = list(parse = parse, check_overlap = check_overlap,
                            na.rm = na.rm,n=n,center=center,outer.r=outer.r,
                            round=round,angle=angle,
                            angle_string=angle_string,
                            angle_id=angle_id,
                            coord_string=coord_string,
                            coord_id=coord_id,
                            disappear_string=disappear_string,
                            disappear_id=disappear_id,
                            translate.1=translate.1,
                            trans_h.2=trans_h.2,
                            trans_v.3=trans_v.3,
                            trans_al.4=trans_al.4,
                            rotate.5=0,
                            zoom.6=zoom.6,
                            mirror.7=mirror.7,
                            mirror_h.8=mirror_h.8,
                            mirror_v.9=mirror_v.9,
                            ...))
}
GeomRoseText <- ggproto("GeomRoseText", Geom,
                    required_aes = c("label"),
                    default_aes = aes(x=after_stat(x),y=after_stat(y),
                        colour = "black", size = 3.88, angle = NA, hjust = 0.5,
                        vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                    ),
                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                          na.rm = FALSE,
                                          check_overlap = FALSE,
                                          angle,
                                          angle_string,
                                          angle_id,
                                          coord_string,
                                          coord_id,
                                          disappear_string,
                                          disappear_id) {
                        if (all(is.na(angle))){
                            data$angle=data$RoseAngle
                            data$angle=ifelse(data$angle>180,data$angle-180,data$angle)
                            data$angle=ifelse(data$angle>90,data$angle-90,data$angle+90+180)
                        }else{
                            data$angle=angle
                        }
                        # angle_string
                        if (!is.null(angle_string)){
                            for (i in 1:length(angle_string)) {
                                string_i=angle_string[i]
                                string_i=do::reverse(sub(':','stringsplitsub',do::reverse(string_i)))
                                string_2=strsplit(string_i,'bustilpsgnirts')[[1]]
                                string=strsplit(string_2,';')[[1]]
                                anglie_2=string_2[2]
                                for (j in string) {
                                    if (any(data$label==j)){
                                        data[data$label==j,'angle']=anglie_2
                                    }else{
                                        message(j,' is not an included text')
                                    }
                                }
                            }
                        }
                        # angle_id
                        if (!is.null(angle_id)){
                            for (i in 1:length(angle_id)) {
                                string_i=angle_id[i]
                                string_i=do::reverse(sub(':','stringsplitsub',do::reverse(string_i)))
                                string_2=strsplit(string_i,'bustilpsgnirts')[[1]]
                                idrow=string_2[1]
                                if (grepl(';',idrow)){
                                    text=strsplit(idrow,';')[[1]]
                                    ID=NULL
                                    for (j in 1:length(text)) {
                                        ID=c(ID,eval(parse(text = text[j])))
                                    }
                                    idrow=ID
                                }else{
                                    text = idrow
                                    ID=NULL
                                    for (j in 1:length(text)) {
                                        ID=c(ID,eval(parse(text = text[j])))
                                    }
                                    idrow=ID
                                }
                                anglie_2=string_2[2]
                                for (j in idrow) {
                                    if (any(nrow(data) >= j)){
                                        data[j,'angle']=anglie_2
                                    }else{
                                        message(j,' is byond row number')
                                    }
                                }
                            }
                        }
                        # coord_string
                        if (!is.null(coord_string)){
                            for (i in 1:length(coord_string)) {
                                string_i=coord_string[i]
                                string_i=do::reverse(sub(':','stringsplitsub',do::reverse(string_i)))
                                string_2=strsplit(string_i,'bustilpsgnirts')[[1]]
                                string=strsplit(string_2,';')[[1]]
                                anglie_2=as.numeric(as.character(strsplit(string_2[2],';')[[1]]))
                                for (j in string) {
                                    if (any(data$label==j)){
                                        data[data$label==j,'x']=anglie_2[1]
                                        data[data$label==j,'y']=anglie_2[2]
                                    }else{
                                        message(j,' is not an included text')
                                    }
                                }
                            }
                        }
                        # coord_id
                        if (!is.null(coord_id)){
                            for (i in 1:length(coord_id)) {
                                string_i=coord_id[i]
                                string_i=do::reverse(sub(':','stringsplitsub',do::reverse(string_i)))
                                string_2=strsplit(string_i,'bustilpsgnirts')[[1]]
                                string=as.numeric(as.character(strsplit(string_2,';')[[1]]))
                                anglie_2=as.numeric(as.character(strsplit(string_2[2],';')[[1]]))
                                for (j in string) {
                                    if (nrow(data)>=j){
                                        data[j,'x']=anglie_2[1]
                                        data[j,'y']=anglie_2[2]
                                    }else{
                                        message(j,' is not an included text')
                                    }
                                }
                            }
                        }
                        # disappear_string
                        if (!is.null(disappear_string)){
                            for (i in 1:length(disappear_string)) {
                                string_i=disappear_string[i]
                                string=strsplit(string_i,';')[[1]]
                                for (j in string) {
                                    if (any(data$label==j)){
                                        data[data$label==j,'label']=''
                                    }else{
                                        message(j,' is not an included text')
                                    }
                                }
                            }
                        }
                        # disappear_id
                        if (!is.null(disappear_id)){
                            for (i in 1:length(disappear_id)) {
                                idrow=disappear_id[i]
                                if (grepl(';',idrow)){
                                    text=strsplit(idrow,';')[[1]]
                                    ID=NULL
                                    for (j in 1:length(text)) {
                                        ID=c(ID,eval(parse(text = text[j])))
                                    }
                                    idrow=ID
                                }else{
                                    text = idrow
                                    ID=NULL
                                    for (j in 1:length(text)) {
                                        ID=c(ID,eval(parse(text = text[j])))
                                    }
                                    idrow=ID
                                }
                                for (j in idrow) {
                                    if (nrow(data) >=j){
                                        data[j,'label']=''
                                    }else{
                                        message(j,' is not an included text')
                                    }
                                }
                            }
                        }
                        ######################################################################
                        lab <- data$label
                        if (parse) {
                            lab <- parse_safe(as.character(lab))
                        }
                        data <- coord$transform(data, panel_params)
                        if (is.character(data$vjust)) {
                            data$vjust <- compute_just(data$vjust, data$y)
                        }
                        if (is.character(data$hjust)) {
                            data$hjust <- compute_just(data$hjust, data$x)
                        }
                        textGrob(
                            lab,
                            data$x, data$y, default.units = "native",
                            hjust = data$hjust, vjust = data$vjust,
                            rot = data$angle,
                            gp = gpar(
                                col = alpha(data$colour, data$alpha),
                                fontsize = data$size * .pt,
                                fontfamily = data$family,
                                fontface = data$fontface,
                                lineheight = data$lineheight
                            ),
                            check.overlap = check_overlap
                        )
                    },
                    draw_key = draw_key_text
)
StatRoseText <- ggproto("StatRoseText", Stat,
                        default_aes = aes(x = after_stat(x),
                                          y = after_stat(y),
                                          petal=NULL),
                        compute_group = function(scales, data,
                                                 n,center,outer.r,
                                                 round,
                                                 translate.1,
                                                 trans_h.2,
                                                 trans_v.3,
                                                 trans_al.4,
                                                 rotate.5,
                                                 zoom.6,
                                                 mirror.7,
                                                 mirror_h.8,
                                                 mirror_v.9){
                            data2=rose_text_data(data=data,petal='petal',
                                               n=n,center=center,outer.r=outer.r,
                                               round=round,
                                               translate.1=translate.1,
                                               trans_h.2=trans_h.2,
                                               trans_v.3=trans_v.3,
                                               trans_al.4=trans_al.4,
                                               rotate.5=0,
                                               zoom.6=zoom.6,
                                               mirror.7=mirror.7,
                                               mirror_h.8=mirror_h.8,
                                               mirror_v.9=mirror_v.9)
                            data2
                        }
)
