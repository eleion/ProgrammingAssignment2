## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   # Inicializamos la matrix.
   inversa <- NULL
   
   # Creamos las funciones 'set' y 'get'.
   set <- function(y) {
      x       <<- y
      inversa <<- NULL
   }
   get <- function() {
      x
   }
   # Creamos las funciones 'setInversa' y 'getInversa'.
   setInversa <- function(i) {
      inversa <<- i
   }
   getInversa <- function() {
      inversa
   }
   
   # Devolvemos una lista con los métodos 'gets' y 'sets'.
   list(
      set = set,
      get = get,
      setInversa = setInversa,
      getInversa = getInversa
   )
}


## Write a short comment describing this function
## Devuelve una matriz que es la inversa de 'x'.

cacheSolve <- function(x, ...) {
   # Obtenemos la inversa.
   inversa <- x$getInversa()
   
   # Si no es nula la inversa la calculamos.
   if(!is.null(inversa)) {
      message('Obteniendo la inversa de la matriz.')
      return(inversa)
   }
   
   # Obtenemos la matriz y calculamos la inversa.
   matriz <- x$get()
   inversa <- solve(matriz, ...)
   x$setInversa(inversa)
   
   # Devolvemos la matriz inversa.
   inversa
}
