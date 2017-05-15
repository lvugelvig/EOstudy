listnames <- c("Florence", "Line", "Nico", "Shermin", "Veronica", "Amaury", "Aleeza", "Robin", "Li", "Gavin", "Aparna", "Takahiro", "Kayce", "Bronwyn", "Erminda")

GuessName <- function(firstname){
  # Temporary file where the output of the website is stored
  tmphtml <- "guessname.html"
  
  # Query the website and save page
  cmd <- paste0('curl -s --compress "http://www.gpeters.com/names/baby-names.php?name=', firstname, '" | grep "Based on popular usage" > ', tmphtml)
  system(cmd)

  # Extract gender information
  cmd.gender <- paste0('./extractgender.sh ', tmphtml)
  gender <- system(cmd.gender, intern = TRUE)
  #  and replace boy and girl by adult qualifiers :-)
  if(gender=="girl") gender <- "Woman"
  if(gender=="boy") gender <- "Man"
  
  # Extract confidence information
  cmd.ratio <- paste0('./extractratio.sh ', tmphtml)
  ratio <- system(cmd.ratio, intern = TRUE)
  
  # Erase temporary file
  system(paste0("rm ", tmphtml))

  # Pause to avoid overloading the website
  Sys.sleep(1)
  
  # Output
  out <- c(gender, ratio)
  #  If no information, write Unknown
  if(is.na(out[1])){
    out <- c("Unknown", "0")
  }
  return(out)
}

res <- lapply(listnames, GuessName)
cbind(listnames, matrix(unlist(res), byrow = TRUE, ncol = 2))
