library("TurtleGraphics")


oursin <- function(n, c, pos) {
  r <- n/10
  
  # --- Dessin des pointes en rouge ---
  turtle_param(col = "red")
  for(i in 1:n){
    turtle_forward(r)
    turtle_up()
    turtle_goto(pos[1], pos[2])
    turtle_down()
    turtle_turn(360/n)
  }
  
  turtle_col("green")
}





etoile <- function(n, col = "red", offset = 10) {
  pos <- turtle_getpos()
  cx <- pos[1]
  cy <- pos[2] + offset
  angles <- seq(0, 4) * 2 * pi / 5 + pi/2
  xs <- cx + n * cos(angles)
  ys <- cy + n * sin(angles)
  ordre <- c(1, 3, 5, 2, 4, 1)
  
  for (r in seq(n, 0, by = -1)) {
    xs_r <- cx + r * cos(angles)
    ys_r <- cy + r * sin(angles)
    turtle_up()
    turtle_goto(xs_r[ordre[1]], ys_r[ordre[1]])
    turtle_down()
    turtle_lwd(2)
    turtle_param(col = col)
    for (i in 2:length(ordre)) {
      turtle_goto(xs_r[ordre[i]], ys_r[ordre[i]])
    }
  }
}



tronc <- function(n) {
  turtle_init(width = 200, height = 250)
  
  turtle_do({
    turtle_setpos(105, 40)
    turtle_left(90)
    turtle_lwd(n)
    turtle_col("Brown")
    
    for (i in 1:2) {
      turtle_forward(15)
      turtle_left(90)
      turtle_forward(25)
      turtle_left(90)
    }
  })
}

branche <- function(n) {
  turtle_do({
    turtle_setpos(200, 40)
    turtle_col("Green")
    max_large <- 200
    reduction <- 2 * 5
    turtle_lwd(n)  # épaisseur des branches
    
    turtle_forward(max_large)
    max_large <- max_large - reduction
    turtle_left(4)
    turtle_backward(max_large)
    max_large <- max_large - reduction
    
    for (i in 1:6) {
      turtle_right(8)
      turtle_forward(max_large)
      
      # Dessiner oursin sur la branche avec rouge
      pos <- turtle_getpos()
      turtle_col("red")
      oursin(n*2, 10*n, pos)
      
      max_large <- max_large - reduction
      turtle_left(8)
      turtle_backward(max_large)
      
      pos<- turtle_getpos()
      turtle_col("red")
      oursin(n*2, 10*n, pos)
      max_large <- max_large - reduction
      turtle_col("Green")
    }
    
    turtle_right(8)
    turtle_forward(max_large)
    max_large <- max_large %/% 2
    turtle_left(25)
    turtle_backward(max_large)
    
    etoile(n*2, col = "red")
  })
}



sapin <- function(n) {
  turtle_init(width = 400, height = 400) 
  tronc(n*5)      
  branche(n*2)
  turtle_hide()
}






















etoile <- function(n, col = 'gold',sommet) {
  turtle_do({
    pos <- turtle_getpos()
    cx <- sommet[1]
    cy <- sommet[2]
    angles <- seq(0, 4) * 2 * pi / 5 + pi/2
    ordre <- c(1, 3, 5, 2, 4, 1)
    turtle_lwd(2)
    turtle_col(col)
    for (r in seq(n, 0, by = -1)) {
      xs_r <- cx + r * cos(angles)
      ys_r <- cy + r * sin(angles)
      turtle_up()
      turtle_goto(xs_r[ordre[1]], ys_r[ordre[1]])
      turtle_down()
      for (i in 2:length(ordre)) {
        turtle_goto(xs_r[ordre[i]], ys_r[ordre[i]])
      }
    }
  })
}


oursin <- function(n, couleur, x, y){
  
  turtle_up()
  turtle_setpos(x, y)
  turtle_down()
  
  for(i in 1:n){
    turtle_param(col = couleur, lwd=2, lty=2)
    turtle_forward(5)
    turtle_up()
    turtle_setpos(x, y)
    turtle_down()
    turtle_turn(360/n)
  }
}

sapin<-function(){
  turtle_init(width = 400,height = 400)
  turtle_hide()
  turtle_setpos(180,40) #pour faire la largeur de tronc=40
  turtle_do({turtle_forward(60)
    turtle_right(90)
    turtle_forward(40)
    turtle_right(90)
    turtle_forward(60)
    turtle_right(90)
    turtle_forward(40)
  })
  turtle_do({turtle_setpos(330,100)
    turtle_forward(260) # longueur base
    gauche<-turtle_getpos()
    turtle_goto(200,350)
    sommet<-turtle_getpos()
    turtle_goto(330,100)
    droit<-turtle_getpos()})
  x<-330
  y<-100
  base<-260
  positions<-list()
  positions<-append(positions,list(droit,gauche))
  for (i in 1:7) {
    x<-x-16
    y<-y+30
    base<-base-32
    turtle_setpos(x,y)
    positions<-append(positions,list(turtle_getpos()))
    turtle_forward(base)
    positions<-append(positions,list(turtle_getpos()))
  }
  couleur<-sample(rainbow(length(positions)))
  for (i in seq_along(positions)) {
    p <- positions[[i]]
    col <- couleur[i]
    oursin(100, col, p[1], p[2]) # 12 pointes par exemple
  }
  etoile(30, col = "gold",sommet)
}
