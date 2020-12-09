Physique_run <- function(){
  appDir <- system.file("shinyApp", package = "Physique")
  if (appDir == "")
  {
    stop("Could not find app directory. Try re-installing `Physique`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)

}

chute.libre <- function(){}

equation.newton <- function(v0=NULL,alpha=NULL,x=NULL,h=NULL,g=NULL,y=NULL,t=NULL){
  # t = scd
  # v0 = m/s
  # x = m
  # h = m
  # g = m/s/N

  args <- list(v0,alpha,h,x,g,y,t)
  res <- unlist(lapply(args,function(i){is.null(i)}))
  if(which(FALSE%in%res)>1) return(FALSE)

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

trajectoire <- function(v0,alpha,h,x,g,y,t){
  args <- list(v0,alpha,h,x,g,y,t)
  res1 <- unlist(lapply(args,function(i){length(i)>1}))
  res2 <- unlist(lapply(args,function(i){is.null(i)}))

  if(is.null(y) && length(t)>1){
    data <- lapply(t, function(i){
      x <- equation.newton(v0,alpha,h,x=NULL,g,y,t=i)
      y <- equation.newton(v0,alpha,h,x=0,g,y=NULL,t=i)
      return(c(x,y))
    })
    data <- do.call(rbind,data)
    data <- cbind(data,t)
    colnames(data) <- c("x","y","t")
  }
  return(data)
}

gravite <- function(M,R){
  G <- 6.674*10^-11
  return(G*(M/(R**2)))
}

demo <- function(){

  M <- 5.972*10^24
  R <- 6371000
  g <- gravite(M,R)

  v0 <- 10
  t <- seq(0,3,0.1)
  x <- 3
  h <- 8
  y <- NULL
  alpha <- 45

  data <- trajectoire(v0,alpha,h,x,g,y,t)

}
