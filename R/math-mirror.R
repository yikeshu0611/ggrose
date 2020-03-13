mirror_h <- function(df,value){
    x=df[,1]
    expr_x=paste0(2*value,'-','x')
    df[,1]=eval(parse(text = expr_x))
    df
}
mirror_v <- function(df,value){
    y=df[,2]
    expr_y=paste0(2*value,'-','y')
    df[,2]=eval(parse(text = expr_y))
    df
}
mirror <- function(sf,slope_intercept){
    x=df[,1]
    y=df[,2]
    a=slope_intercept[1]
    b=-1
    c=slope_intercept[2]
    expr_x=paste0('(',b^2-a^2,'*','x','-(',2*a*b,')*','y','-',2*a*c,')/',a^2+b^2)
    expr_y=paste0('(',a^2-b^2,'*','y','-(',2*a*b,')*','x','-',2*b*c,')/',a^2+b^2)
    df[,1]=eval(parse(text = expr_x))
    df[,2]=eval(parse(text = expr_y))
    df
}
