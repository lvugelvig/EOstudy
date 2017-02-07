## PLOTTING FUNCTIONS
# SMD: show me the data!
histSMD.TF <- function(X, Y, ylm=c(0,1)){
  if( all(sort(unique(X)) == c(FALSE, TRUE)))
    xpos <- c(1,2)
  Y.F <- Y[X==FALSE]
  Y.T <- Y[X==TRUE]
  dx <- 0.2
  xxF <- runif(length(Y.F), min=xpos[1]-dx, max=xpos[1]+dx)
  xxT <- runif(length(Y.T), min=xpos[2]-dx, max=xpos[2]+dx)
  
  # Initiate the plot
  plot(0, type="n", xlim = c(xpos[1]-2*dx, xpos[2]+2*dx), ylim=ylm)
  # Plot FALSE values
  color <- rgb(0.5,0.1,0.1,0.5)
  thepch <- 21
  periph <- 1
  points(xxF, Y.F, bg=color, pch=thepch, col=periph)
  points(xxT, Y.T, bg=color, pch=thepch, col=periph)
  
}