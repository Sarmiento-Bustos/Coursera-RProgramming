#LUCAS SARMIENTO-BUSTOS

#The comments of this script are in spanish. Because is my maternal language hehehe

makeCacheMatrix<-function(x = matrix()) {
       inverse<-NULL      #Creación de variable
       set<-function(y) { #función de la matrix e inversa
       x<<-y              #Crear una nueva variable y que cabie su valor
       inverse<-NULL
       }
       get<-function(){x} #Devuelve(return) el valor de la matriz
       setinverse<-function(inversec) {inverse<<-inversec} #Da valor a la matriz y se iguala al valor ingresado anteriormente
       getinverse<-function() {inverse} #ver el valor de la inversa calculada
       
       list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
       }

x<<-makeCacheMatrix(matrix(1:4;nrow=2,ncol=2))  #crear la matriz como tal
x			 #x contiene todos los valores y todas las funciones

  # Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()  #adquiere el valor de la inversa de la matrix
        if(!is.null(inverse)) {
        message("getting cached data")  #si no hay valor asignado se muestra dicho mensaje
        return(inverse)
        }

        data <- x$get()      #generar la variable data e igualarla a la inversa de la matriz
        inverse<-solve(data, ...)#calcular la inversa
        x$setinverse(inverse)   #da el valor encontrado anteriormente
        inverse
}
