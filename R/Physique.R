
chute.libre <- function()

parabole <- function(v0,alpha,x,h,g,t){
  # t = scd
  # v0 = m/s
  # x = m
  # h = m
  # g = m/s/N

  args <- list(v0,alpha,h,x,g,y,t)
  res <- unlist(lapply(args,function(i){is.null(res)}))
  if(which(res==FALSE)>1) return(FALSE)

  if(is.null(v0)){
    if(x != 0){
      v0 = x/(t*(cos(alpha)))
    } else {
      v0 = (y-h-((-1/2)*g*t**2))/(sin(alpha)*t)
    }
    return(v0)
  }
  if(is.null(h)){
    return(h)
  }
  if(is.null(alpha)){
    return(alpha)
  }
  if(is.null(x)){
    x = v0*(cos(alpha)*t)
    return(x)
  }
  if(is.null(y)){
    y = ((-1/2)*g*t**2)+(v0*sin(alpha)*t)+h
    return(y)
  }
}

gravite <- function(M,R){
  G <- 6.674*10^-11
  return(G*(M/(R**2)))
}

M <- 5.972*10^24
R <- 6371000

v0 <- 10
t <- 3
x <- 3
h <- 8
y <- 12
g <- gravite(M,R)
