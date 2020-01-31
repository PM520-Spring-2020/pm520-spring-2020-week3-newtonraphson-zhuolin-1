# a function we will work with
F1 <- function(x){
  return(c(x^2, 2*x)) # note that the function returns two numbers. The first is f(x); the second is the derivative, f'(x)
}

#define a function F2(x)=sin(x)
F2 <- function(x){
  return(c(sin(x), cos(x)))
}
#define F3(x)=(x-2)^3-6*x
F3 <- function(x){
  return(c((x-2)^3-6*x, 3*(x-2)^2-6))
}
#define F4(x)=cos(x)-x###
F4 <- function(x){
  return(c(cos(x)-x, -sin(x)-1))
}
# (All functions need to return f(x) and f’(x))

# Define your Newton-Raphson function
NewtonRaphson <- function(func, StartingValue, Tolerance, MaxNumberOfIterations){
  #initialize a variable, Deviation (say), to record |f(x)| so that you know how far away you are from 0.
  #(So initialize it to some arbitrary large number)
  Deviation <- abs(func(StartingValue)[1])
  #Set up a counter, i, to record how many iterations you have performed. Set it equal to 0
  i <- 0
  # Initialize the values of x and f(x)
  x <- StartingValue
  #Set up a while loop until we hit the required target accuracy or the max. number of steps
  while ((i < MaxNumberOfIterations) && (Deviation > Tolerance))
  {
    # Record the value of f(x) and f’(x), for the current x value.
    # I put them in a variable Z. Z[1]<-x; Z[2]<-f(x)
    Z <- func(x)
    # To be safe, check that the function and it's derivative are defined at X (either could be NaN if you are unlucky)
    if ((Z[1] == "NaN") || (Z[2] == "NaN")){
      cat("\nFunction or derivative not defined error.\n")
      break
    }

    #Find the next X-value using Newton-Raphson's formula. Let's call that value X
    X <- x - Z[1]/Z[2]
    # calculate Deviation<- |f(x)-0|
    Deviation <- abs(func(X)[1])
    # increase the value of your iteration counter
    i <- i + 1

    # if you like, have the program write out how it is getting on
    cat(paste("\nIteration ",i,":   X=",X,"  Deviation=",Deviation))

    # If you are feeling fancy, add some line segments to the screen to show where it just went
    # See the 'fixed points' code for a reminder of how to do that.
  }

  # output the result
  if (Deviation<Tolerance){
    cat(paste("\nFound the root point: ",X, " after ", i, "iterations"))
  }else{
    cat(paste("\nConvergence failure. Deviation: ",Deviation, "after ", i, 	"iterations"))}

  # have the function return the answer
  return(X)
}


pdf("Fig6.pdf")
curve(x^2, -1, 11, main = "y=x^2")
NewtonRaphson(F1, 10, 1e-3, 40)
abline(h = 0)
dev.off()
