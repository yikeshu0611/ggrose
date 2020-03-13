translate <- function(df,step){
    df[,1]=df[,1]+step[1]
    df[,2]=df[,2]+step[2]
    df
}
trans_h <- function(df,step){
    df[,1]=df[,1]+step[1]
    df
}
trans_v <- function(df,step){
    df[,2]=df[,2]+step[1]
    df
}
trans_al <- function(df,Angle_Length=c(30,1)){
    angle=Angle_Length[1]/180*pi
    step=c(Angle_Length[2]*cos(angle),Angle_Length[2]*sin(angle))
    df[,1]=df[,1]+step[1]
    df[,2]=df[,2]+step[2]
    df
}
