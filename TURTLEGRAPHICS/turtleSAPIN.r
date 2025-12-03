library(TurtleGraphics)

# Fonction pour une branche horizontale en zigzag
branche <- function(longueur, reduction) {
  # droite-gauche-droite-gauche
  turtle_forward(longueur)
  longueur <- longueur - reduction
  turtle_backward(longueur)
  longueur <- longueur - reduction
  turtle_forward(longueur)
  longueur <- longueur - reduction
  turtle_backward(longueur)
}

# Fonction qui dessine le sapin
sapin <- function() {
  turtle_init(mode = "clip", width = 400, height = 400)
  turtle_hide()
  
  # départ en bas au centre
  turtle_setpos(0, -150)
  turtle_col("brown")
  
  # Tronc vertical
  turtle_left(90)       # pointe vers le haut
  turtle_forward(30)    # hauteur du tronc
  turtle_right(90)      # pointe vers la droite pour les branches
  
  turtle_col("green")
  
  # Première branche (la plus longue)
  branche(80, 10)
  
  # Remonter pour la deuxième branche
  turtle_left(90)
  turtle_forward(20)
  turtle_right(90)
  
  # Deuxième branche
  branche(60, 8)
  
  # Remonter pour la troisième branche
  turtle_left(90)
  turtle_forward(20)
  turtle_right(90)
  
  # Troisième branche (plus courte)
  branche(40, 5)
}

# Dessiner le sapin
sapin()



