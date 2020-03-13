zoom <- function(df,center,ratio){
    x=df[,1]
    y=df[,2]
    expr_x=paste0('(','x','-',center[1],')*',ratio,'+',center[1])
    expr_y=paste0('(','y','-',center[2],')*',ratio,'+',center[2])
    df[,1]=eval(parse(text = expr_x))
    df[,2]=eval(parse(text = expr_y))
    df
}
