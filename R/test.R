#' Multiply a Number by 2
#' 
#' @description
#' This function takes a numeric input and multiplies it by 2.
#' 
#' @param x A numeric value to be multiplied by 2
#' 
#' @return A numeric value equal to the input multiplied by 2
#' 
#' @examples
#' multiplication_2(5)  # Returns 10
#' multiplication_2(-3) # Returns -6
#' multiplication_2(0)  # Returns 0
#' 
#' # Works with vectors too
#' multiplication_2(c(1, 2, 3)) # Returns c(2, 4, 6)
#' 
#' @export
multiplication_2 <- function(x){
    2*x
}
