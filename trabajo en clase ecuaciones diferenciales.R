funcion <- function(x, y){
  return(-3*x^2*y+6*x^2)
}


solucion <- function(x){
  return(2+((exp(-x^3))*(exp(1)-2)))
}

h = 0.001
inicio = -0.1
final = 0.1
x = seq(inicio,final,by=h)
y = seq(inicio,final,by=h)
dif = seq(inicio,final,by=h)



for (i in x) {
  y[i] = solucion(x[i])  
}

for (j in y) {
  dif[j] = abs((solucion(x[j]+h)-solucion(x[j]))/h)
}




plot(x,y,type="l",asp=1,col="red")
plot(x,dif,type="l",asp=1,col="red")


print(dif)

cat("Cota superior: ", dif[(final/h)*2], "\n")
