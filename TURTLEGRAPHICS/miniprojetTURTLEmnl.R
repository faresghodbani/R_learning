library(TurtleGraphics)

oursin <- function(n,c,pos){
  turtle_hide() # cacher la tortue
  coul<-rainbow(n) #generer n couleur differents
  for( i in 1:n){
    r<-runif(1,min=0,max=c) # longueur aleatoire de l'epine 
    turtle_param(col = coul[i],lwd=1,lty=1)
    turtle_forward(r)
    turtle_up()
    turtle_goto(pos[1],pos[2])#revenir au centre
    turtle_down()
    turtle_turn(360/n)#tourne la direction de la tortue
  }
}


tronc <- function(){
  turtle_init(width = 200, height = 200)
  turtle_setpos(105, 40)   # centré
  turtle_left(90)          # orienté vers le haut
  
  turtle_col("Brown")
  for (i in 1:2) {
    turtle_forward(15)
    turtle_left(90)
    turtle_forward(25)
    turtle_left(90)
    
  }
}

branche<-function(){
  turtle_setpos(200,40)
  turtle_col("Green")
  max_large<-200           # 15(largeur tronc)+50+50(distance de chaque cote)
  reduction<-2*5
  
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(4)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(8)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(8)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(8)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(8)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(8)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large-reduction
  turtle_left(8)
  turtle_backward(max_large)
  max_large<-max_large-reduction
  turtle_right(8)
  turtle_forward(max_large)
  max_large<-max_large%/%2
  turtle_left(25)
  turtle_backward(max_large)
  pos=turtle_getpos()
  oursin(100,10,pos)
}

