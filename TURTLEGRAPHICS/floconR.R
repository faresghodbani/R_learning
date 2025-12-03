library("TurtleGraphics")

VK <- function(n, T) {
  if (n == 0) turtle_forward(T)
  else {
    VK(n-1, T/3)
    turtle_left(60)
    VK(n-1, T/3)
    turtle_right(120)
    VK(n-1, T/3)
    turtle_left(60)
    VK(n-1, T/3)
  }
}

flocon_koch <- function(n, T) {
  turtle_init(width = 800, height = 600)  # plus grand pour éviter de sortir
  turtle_hide()
  turtle_up()
  turtle_col("red")          # couleur du trait
  turtle_lwd(3) 
  turtle_goto(400,200)   # position de départ sûre
  turtle_down()
  
  for (i in 1:3) {
    VK(n, T)
    turtle_right(120)
  }
}

flocon_koch(4, 100)
