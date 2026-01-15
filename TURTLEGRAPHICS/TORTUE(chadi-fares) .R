library("TurtleGraphics")

# ---------------------------------------------------------
#  FONCTION : background()
#  Rôle : Dessine un fond bleu nuit en remplissant l'écran
#         avec des lignes horizontales épaisses.
# ---------------------------------------------------------
background <- function() {
  turtle_init(width = 400, height = 400) # initialise la fenêtre graphique (400x400)
  turtle_hide()                          # cache la tortue
  turtle_lwd(5)                          # épaisseur importante pour remplir vite
  turtle_col("midnightblue")            # couleur du fond
  
  # Dessin du fond ligne par ligne
  for (y in seq(0, 400, by = 5)) {
    turtle_setpos(0, y)                  # début de ligne à gauche
    turtle_goto(400, y)                  # fin de ligne à droite
  }
  
  turtle_lwd(1)                          # remise à l'épaisseur normale
}

# ---------------------------------------------------------
#  FONCTION : tronc()
#  Rôle : Dessine le tronc du sapin :
#         - remplissage marron
#         - contour noir
# ---------------------------------------------------------
tronc <- function() {
  turtle_setpos(180,40)                  # coin bas gauche du tronc
  x <- 180
  turtle_col("brown")                   # couleur du tronc
  
  # Remplissage vertical du tronc
  for (i in 0:40) {
    turtle_setpos(x+i,40)
    turtle_forward(60)                   # hauteur du tronc
  }
  
  # Contour du tronc
  turtle_setpos(220,40)
  turtle_col("black")
  turtle_lwd(2)
  
  for (i in 1:2) {
    turtle_forward(60)
    turtle_left(90)
    turtle_forward(40)
    turtle_left(90)
  }
}

# ---------------------------------------------------------
#  FONCTION : etoile(n, col, sommet)
#  Rôle : Dessine une étoile à 5 branches au sommet du sapin
#         avec un effet lumineux (étoiles concentriques).
#  Paramètres :
#     n      : taille maximale de l'étoile
#     col    : couleur de l'étoile
#     sommet : coordonnées (x, y) du centre de l'étoile
# ---------------------------------------------------------
etoile <- function(n, col = "gold", sommet) {
  turtle_do({
    cx <- sommet[1]
    cy <- sommet[2]
    
    turtle_setangle(90)                  # orientation vers le haut
    
    angles <- (0:4) * 2 * pi / 5 + pi/2  # angles des sommets
    ordre <- c(1, 3, 5, 2, 4, 1)          # ordre pour tracer l'étoile
    
    turtle_lwd(2)
    turtle_col(col)
    
    # Etoiles concentriques
    for (r in seq(n, 0, by = -1)) {
      xs <- cx + r * cos(angles)
      ys <- cy + r * sin(angles)
      
      turtle_up()
      turtle_goto(xs[ordre[1]], ys[ordre[1]])
      turtle_down()
      
      for (i in ordre[-1]) turtle_goto(xs[i], ys[i])
    }
  })
}

# ---------------------------------------------------------
#  FONCTION : oursin(n, couleur, x, y)
#  Rôle : Dessine une décoration du sapin en forme d'oursin
#         (petits traits répartis en cercle).
#  Paramètres :
#     n       : nombre de traits
#     couleur : couleur de la décoration
#     x, y    : position de la boule
# ---------------------------------------------------------
oursin <- function(n, couleur, x, y) {
  turtle_setpos(x, y)
  turtle_param(col = couleur, lwd = 4)
  
  for(i in seq(1:n)) {
    turtle_forward(5)                    # longueur d'une pointe
    turtle_setpos(x, y)                  # retour au centre
    turtle_turn(360/n)                   # rotation régulière
  }
}

# ---------------------------------------------------------
#  FONCTION : herbe()
#  Rôle :
#   - Dessine de l’herbe stylisée au bas de l’image
#   - Utilise des traits disposés en cercle pour créer
#     un effet de touffe d’herbe
#   - Repose sur la même logique que la fonction oursin()
#     (répétition de traits avec rotation régulière)
# ---------------------------------------------------------
herbe <- function() {
  
  # Paramètres graphiques de l’herbe
  turtle_param(col = "green", lwd = 50)   # couleur verte et traits épais
  
  # Positions horizontales des touffes d’herbe
  xs <- seq(20, 380, by = 40)              # répartition régulière sur la largeur
  
  y <- 20                                 # hauteur fixe au bas de l’image
  
  # Boucle sur chaque position x
  for (x in xs) {
    
    turtle_setpos(x, y)                   # positionnement de la touffe
    turtle_setangle(0)                    # orientation initiale
    
    # Dessin de la touffe :
    # même principe que oursin() :
    # - un trait
    # - retour au centre
    # - rotation régulière
    for (i in 1:10) {
      turtle_forward(10)                  # longueur d’un brin d’herbe
      turtle_setpos(x, y)                 # retour au centre
      turtle_turn(360 / 10)               # rotation pour répartir les brins
    }
  }
}

# ---------------------------------------------------------
#  FONCTION : decoration(positions, couleurs, note)
#  Rôle :
#   - Place un certain nombre de boules de Noël sur le sapin
#   - Chaque boule est dessinée comme un petit oursin
#     (utilisation de la fonction oursin() pour la logique)
#  Paramètres :
#     positions : liste des coordonnées (x, y) possibles pour les boules
#     couleurs  : vecteur de couleurs correspondant à chaque position
#     note      : nombre de boules à afficher
# ---------------------------------------------------------
decoration <- function(positions, couleurs, note) {
  
  # Boucle sur le nombre de boules à afficher
  for (i in 1:note) {
    
    # Récupération de la position de la boule i
    p <- positions[[i]]  
    
    # Dessin de la boule (oursin) :
    # - taille : 30
    # - couleur : couleurs[i]
    # - position : p[1] (x), p[2] (y)
    oursin(30, couleurs[i], p[1], p[2])
  }
}

# ---------------------------------------------------------
#  FONCTION : arbre()
#  Rôle :
#   - Dessine le sapin sous forme de triangle rempli
#   - Calcule automatiquement les positions des décorations
#   - Génère des couleurs aléatoires pour les boules
#   - Ajoute l’étoile au sommet
#  Valeur retournée :
#     positions : liste des coordonnées possibles des décorations
#     couleurs  : couleurs associées aux décorations
# ---------------------------------------------------------
arbre <- function() {
  
  # Points clés du triangle du sapin
  A_gauche <- c(70, 100)    # coin inférieur gauche
  A_droite <- c(330, 100)   # coin inférieur droit
  C_sommet <- c(200, 350)   # sommet du sapin
  
  # Calcul des pentes des côtés
  # (utilisées pour déterminer les bords du sapin)
  m_g <- (C_sommet[1] - A_gauche[1]) / (C_sommet[2] - A_gauche[2])  # pente côté gauche
  m_d <- (C_sommet[1] - A_droite[1]) / (C_sommet[2] - A_droite[2])  # pente côté droit
  
  turtle_lwd(2)                     # épaisseur des lignes
  turtle_col("forestgreen")         # couleur verte du sapin
  

  # Remplissage du sapin ligne par ligne
  for (y in seq(100, 350, by = 2)) {   # balayage vertical du bas vers le haut
    
    # Calcul de la position du bord gauche à la hauteur y
    xg <- A_gauche[1] + m_g * (y - A_gauche[2])
    
    # Calcul de la position du bord droit à la hauteur y
    xd <- A_droite[1] + m_d * (y - A_droite[2])
    
    # Tracé de la ligne horizontale entre les deux bords
    turtle_setpos(xg, y)
    turtle_goto(xd, y)
  }
  
  # Contour du sapin pour plus de visibilité
  turtle_col("black")
  turtle_do({
    turtle_setpos(330,100)   # point de départ : bas droit
    turtle_left(90)
    turtle_forward(260)     # tracé du côté droit
    turtle_goto(200,350)    # tracé jusqu’au sommet
    turtle_goto(330,100)    # fermeture du triangle
  })
  
  # Initialisation des paramètres
  # pour le calcul des décorations
  x <- 330                 # position de départ à droite
  y <- 100                 # hauteur de départ
  base <- 260              # largeur initiale du sapin
  
  # Liste contenant les positions possibles des décorations
  positions <- list(c(330,100), c(70,100))  # coins de la base
  
  # Calcul des positions des décorations
  # (deux par étage)
  for (i in 1:7) {
    
    # Réduction progressive de la largeur du sapin
    x <- x - 16             # décalage vers la gauche
    y <- y + 30             # montée en hauteur
    base <- base - 32       # diminution de la largeur
    
    # Position gauche de l’étage
    turtle_setpos(x, y)
    positions <- append(positions, list(turtle_getpos()))
    
    # Position droite de l’étage
    turtle_forward(base)
    positions <- append(positions, list(turtle_getpos()))
  }
  
  # Génération de couleurs aléatoires
  # pour chaque décoration
  couleurs <- sample(rainbow(length(positions)))

  # Ajout de l’étoile au sommet
  etoile(30, "gold", C_sommet)
  
  # Valeur retournée par la fonction
  return(list(
    positions = positions,  # coordonnées des décorations
    couleurs  = couleurs    # couleurs associées
  ))
}


# ---------------------------------------------------------
#  FONCTION : auto(positions, couleurs, nb_vagues,
#                  nb_neige_par_vague, note)
#  Rôle :
#   - Anime la scène du sapin
#   - Fait tomber la neige
#   - Fait clignoter les boules de Noël
#  Paramètres :
#     positions           : liste des coordonnées des boules de Noël
#     couleurs            : couleurs possibles des boules
#     nb_vagues           : nombre de cycles d’animation
#     nb_neige_par_vague  : nombre de flocons tombant par vague
#     note                : nombre de boules à afficher (clignotantes)
# ---------------------------------------------------------
auto <- function(positions, couleurs, nb_vagues = 5, nb_neige_par_vague = 2, note) {
  
  # Boucle sur chaque vague d’animation
  for (vague in 1:nb_vagues) {
    
    # -----------------------------
    # Chute de neige
    # -----------------------------
    turtle_param(col="snow", lwd=5, lty=3)   # couleur blanche, traits épais, pointillés
    for (i in 1:nb_neige_par_vague) {
      x <- sample(0:400, 1)                   # position horizontale aléatoire
      long <- sample(30:300, 1)               # longueur du flocon (verticale)
      turtle_setpos(x, 400)                   # départ en haut de l’écran
      turtle_setangle(180)                     # orientation vers le bas
      turtle_forward(long)                     # tracé du flocon
    }
    
    # -----------------------------
    # Boules de Noël clignotantes
    # -----------------------------
    # Mélange aléatoire des couleurs pour simuler le clignotement
    couleurs <- sample(rainbow(length(positions)), note)
    
    # Dessin des boules si la note > 0
    if (note != 0) decoration(positions, couleurs, note)
    
    Sys.sleep(1)   # pause pour voir l’animation
  }
}

# ---------------------------------------------------------
#  FONCTION : noeuds(col_ruban, cx, cy)
#  Rôle :
#    - Dessine un nœud de ruban sur un cadeau
#    - Composé de deux "papillons" symétriques
#  Paramètres :
#    col_ruban : couleur du ruban/nœud
#    cx, cy    : coordonnées du centre du nœud
# ---------------------------------------------------------
noeuds <- function(col_ruban, cx, cy) {
  
  turtle_col(col_ruban)      # couleur du nœud
  turtle_lwd(3)              # épaisseur des traits
  turtle_setangle(0)         # orientation initiale de la tortue
  
  # -----------------------------
  # Premier papillon
  # -----------------------------
  turtle_right(90)           # orienter vers le bas
  turtle_do({
    turtle_forward(10)       # longueur du papillon
    turtle_left(90)
    turtle_forward(5)        # largeur du papillon
    turtle_goto(cx, cy)      # retour au centre du nœud
  })
  
  # -----------------------------
  # Second papillon symétrique
  # -----------------------------
  turtle_left(90)            # réorientation
  turtle_do({
    turtle_forward(10)
    turtle_right(90)
    turtle_forward(5)
    turtle_goto(cx, cy)      # retour au centre
  })
}


# ---------------------------------------------------------
#  FONCTION : cadeaux(x, y)
#  Rôle :
#    - Dessine un cadeau complet :
#        1) boîte
#        2) couvercle
#        3) ruban vertical
#        4) nœud sur le ruban
#  Paramètres :
#    x, y : coordonnées du coin bas gauche de la boîte
# ---------------------------------------------------------
cadeaux <- function(x, y) {
  
  # -----------------------------
  # Choix aléatoire des couleurs
  # -----------------------------
  col_ruban <- sample(colors(), 1)     # couleur du ruban
  col_cadeau <- sample(colors(), 1)    # couleur de la boîte
  
  turtle_setpos(x, y)                  # positionnement de départ
  
  lh <- 30                              # longueur et hauteur de la boîte
  
  # -----------------------------
  # Remplissage de la boîte
  # -----------------------------
  turtle_lwd(1)
  turtle_col(col_cadeau)
  for (i in x:(x + lh)) {              # lignes verticales successives
    turtle_setpos(i, y)
    turtle_forward(lh)                  # hauteur de la boîte
  }
  
  # -----------------------------
  # Contour de la boîte
  # -----------------------------
  turtle_left(90)
  turtle_col("black")
  for (i in 1:2) {
    turtle_forward(lh)                  # hauteur
    turtle_left(90)
    turtle_forward(lh)                  # largeur
    turtle_left(90)
  }
  
  # -----------------------------
  # Paramètres du couvercle
  # -----------------------------
  xc <- x + 35                          # coin bas gauche du couvercle
  yc <- y + 30                          # hauteur du couvercle
  h_couvercle <- 10                      # hauteur
  
  turtle_setpos(xc, yc)
  turtle_col(col_cadeau)
  
  # -----------------------------
  # Remplissage du couvercle
  # -----------------------------
  for (y_pos in yc:(yc + h_couvercle)) {
    turtle_setpos(xc, y_pos)
    turtle_forward(40)                   # largeur du couvercle
  }
  
  # -----------------------------
  # Contour du couvercle
  # -----------------------------
  turtle_col("black")
  turtle_left(90)
  for (i in 1:2) {
    turtle_forward(h_couvercle)         # hauteur
    turtle_left(90)
    turtle_forward(40)                   # largeur
    turtle_left(90)
  }
  
  # -----------------------------
  # Ruban vertical
  # -----------------------------
  turtle_col(col_ruban)
  xr_initial <- x + 10                   # bord gauche du ruban
  xr_final <- x + 20                     # bord droit
  hauteur_final <- yc + h_couvercle      # départ du ruban (haut du couvercle)
  
  for (i in xr_initial:xr_final) {
    turtle_setpos(i, hauteur_final)
    turtle_forward(lh + h_couvercle)    # hauteur totale du ruban
  }
  
  # -----------------------------
  # Nœud du ruban
  # -----------------------------
  m_ruban <- (xr_initial + 5)            # centre du ruban
  turtle_setpos(m_ruban, hauteur_final)
  noeuds(col_ruban, m_ruban, hauteur_final)  # dessin du nœud
}


# ---------------------------------------------------------
#  FONCTION : affichage_cadeau(n)
#  Rôle :
#    - Affiche plusieurs cadeaux aléatoirement au bas de l’écran
#  Paramètres :
#    n : nombre de cadeaux à dessiner (max 9)
# ---------------------------------------------------------
affichage_cadeau <- function(n) {
  y <- 5                                  # hauteur fixe pour tous les cadeaux
  
  if (n <= 0) return()                    # rien à afficher si n <= 0
  if (n > 9) {
    n <- 9 
    cat("Evitez d'en faire plus, le pere noel n'a plus que 9 cadeaux😉\n") 
  }
  # Positions horizontales possibles
  positions_possibles <- seq(20, 360, by = 40)
  x_random <- sample(positions_possibles, n)  # tirage aléatoire sans répétition
  
  # Dessin de chaque cadeau
  for (i in 1:n) {
    cadeaux(x_random[i], y)
  }
}

# ---------------------------------------------------------
#  FONCTION : projet_sapin(note, membre_famille)
#  Rôle :
#    - Fonction principale qui assemble toutes les parties du sapin de Noël
#    - Dessine le fond, le tronc, le sapin, l’herbe
#    - Affiche les cadeaux si le sapin est suffisamment décoré
#    - Anime la neige et le clignotement des boules
#  Paramètres :
#    note            : nombre de boules de Noël à afficher sur le sapin (max 16)
#    membre_famille  : nombre de cadeaux à afficher au pied du sapin
# ---------------------------------------------------------

# ---------------------------------------------------------
# Pour utiliser le projet_sapin :
# - Entrez notre note (0-20)
# - Indiquez le nombre de membres de votre famille
# Cela permettra à chacun de bénéficier d'un cadeau si la note nous plais sois : si le sapin est complet.
projet_sapin <- function(note, membre_famille) {
  
  # Vérification des paramètres :
  # - note et membre_famille doivent être des nombres entiers
  # - si ce n'est pas le cas, on réinitialise le dessin
  #   puis on arrête l'exécution avec un message d'erreur
  if (
    !is.numeric(note) || note %% 1 != 0 ||
    !is.numeric(membre_famille) || membre_famille %% 1 != 0
  ) {
    turtle_init(width = 400, height = 400)
    turtle_hide()
    stop("Erreur : note et membre_famille doivent être des nombres entiers")
  }
  if (note > 16) note <- 16
  if (note < 0)  note <- 0
  
  # Fond et tronc
  background()   # dessin du ciel ou fond
  tronc()        # dessin du tronc du sapin

  # Sapin et positions des boules
  tree <- arbre()                    # retourne positions et couleurs
  positions <- tree[["positions"]]   # liste des coordonnées des boules
  couleurs  <- tree[["couleurs"]]    # vecteur des couleurs
  
  # Herbe au pied du sapin
  herbe()
  
  # Affichage des cadeaux (si sapin bien décoré)
  if (note >= 16) {
    # nombre de cadeaux déterminé par le nombre de membres de la famille
    affichage_cadeau(membre_famille)
    cat("MERCI POUR CETTE NOTE ET PASSEZ DE JOYEUSES FÊTES !\n")
  }else {
    # note pas assez haute donc pas de cadeau :)
    cat("pas assez de boules pour remplir le sapin => pas de magie de noel => pas de cadeaux\n")
    cat("essayez avec une note plus haute....")
  }
  
  # -----------------------------
  # Animation de la scène en alternant :
  #   - neige qui tombe
  #   - boules qui clignotent
  # -----------------------------
  auto(positions, couleurs, note = note)
}
