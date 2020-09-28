
##Crea un objeto de matriz especial que puede almacenar en caché su inverso
makeCacheMatrix <- function( m = matrix() ) {
  
  ##Iniciar la propiedad inversa
  i <- NULL
  
  ##Metodo envio de matriz
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ##Metodo para obtener la matriz
  get <- function() {
    m
  }
  
  ##Envio de la matriz inversa
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##Obtener la matriz inversa
  getInverse <- function() {

    i
  }
  
  ##Lista de los metodos
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ##Obtener la matriz del propio objeto 
  data <- x$get()
  
  ##Calculo de la matriz usando multiplicion de matriz
  m <- solve(data) %*% data
  
  ##Envio inverso del objeto
  x$setInverse(m)
  
  ## Devuelve la matriz
  m
}