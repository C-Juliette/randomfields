#' charactervector_as_vector
#'
#' @param x Ex : x <- "(0, 1)" a string
#'
#' @return a vector Ex : c(0, 1)
#' @export
#'
#' @examples
#' charactervector_as_vector("(0, 1)")
#'
charactervector_as_vector <- function(x){
  # chaine_splitee <- strsplit(x,"")
  #
  # return(c(as.numeric(chaine_splitee[[1]][2]),as.numeric(chaine_splitee[[1]][5])))}
x <- strsplit(x,"")
x <- as.vector(x[[1]])
x <- as.numeric(x)

new <- c()

if (length(x) != 1){
for (i in (1:(length(x)-1))){
  if (is.na(x[i]) & is.na(x[i+1]) ){
     x[i] <- "temp"
   }
  }

remove <- c("temp")
sortie <- c()
for (i in 1:length(x)){
  if (! x[i] %in% remove){
    sortie <- append(sortie, x[i])
  }
}

sortie
for (i in 1:(length(sortie)-1)){
  while (! (is.na(sortie[i]) || is.na(sortie[i+1]))){
    sortie[i] <- stringr::str_c(sortie[i], sortie[i+1])
    sortie[i+1] <- NA
  }
}

# emplacementNa <- c()
# for (i in 1:(length(sortie)-1)){
#   if(is.na(sortie[i])){emplacementNa <- append(emplacementNa, i)}
# }


sortie


sortie <- sortie[!is.na(sortie)]
sortie <- as.numeric(sortie)}

else{sortie <- as.numeric(x)}

return(sortie)}
