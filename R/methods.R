construct.cv.SplitSvm <- function(object, fn_call, x, y){
  class(object) <- append("cv.SplitSvm", class(object))
  num_betas <- dim(object$betas)[3]
  num_groups <- dim(object$betas)[2]
  object$intercept <- object$intercept#sapply(1:num_betas, function(k, betas, mux_train, muy_train){
 #   as.numeric(muy_train) - as.numeric(mux_train %*% betas[,,k])
#  }, object$betas, mux_train, muy_train, simplify = 'array')
  object$intercept <- array(object$intercept, dim = c(1, num_groups, num_betas))
  object$call <- fn_call
  return(object)
}

scalar_predict <- function(index, object, newx, type){
  if(type[1]=="response"){
    if (dim(object$betas)[2]>1){
      coef <- apply(object$betas[,,index], 1, mean)
    } else {
      coef <- object$betas[,,index]
    }
    output <- mean(object$intercept[,,index]) + as.numeric(newx %*% coef)
  } else {
    intercept <- mean(object$intercept[,,index])
    if (dim(object$betas)[2]>1){
      coef <- apply(object$betas[,,index], 1, mean)
    } else {
      coef <- object$betas[,,index]
    }
    output <- c(intercept, coef)  
  }  
  return(output)
}

#' @title Make predictions from a cv.SplitReg object.
#' @method predict cv.SplitReg
#' @param object Fitted cv.SplitReg object.
#' @param newx Matrix of new values of x at which prediction are to be made. Ignored if type is "coefficients".
#' @param index Indices indicating values of lambda_S at which to predict. Defaults to the optimal value.
#' @param type Either "response" for predicted values or "coefficients" for the estimated coefficients.
#' @param ... Additional arguments for compatibility.

#' @return Either a matrix with predictions or a vector of coefficients
#' 
#' @description 
#' Make predictions from a cv.SplitReg object, similar to other predict methods.
#' 
#' @seealso 
#' \code{\link{predict.cv.SplitReg}}
#' 
#' @examples 
#' library(MASS)
#' set.seed(1)
#' beta <- c(rep(5, 5), rep(0, 45))
#' Sigma <- matrix(0.5, 50, 50)
#' diag(Sigma) <- 1
#' x <- mvrnorm(50, mu = rep(0, 50), Sigma = Sigma)
#' y <- x %*% beta + rnorm(50)
#' fit <- cv.SplitReg(x, y, num_models=2)
#' x.new <- mvrnorm(50, mu = rep(0, 50), Sigma = Sigma)
#' split.predictions <- predict(fit, newx = x.new, type="response")
#' 
predict.cv.SplitSvm <- function(object, newx, index=object$index_opt, type = c("response", "coefficients"), ...){
  if (any(!is.numeric(index), index < 0, index > dim(object$betas)[3])){
    stop("index has to be vector of positive integers, the largest of which
         has to be smaller than or equal to the length of the grid for the sparsity penalties")
  }
  if(type[1]=="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if(p != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    out <- sapply(index, scalar_predict, object, newx, type)
    prob <- as.vector( 1/(1+exp(-out)))
    n    <- length(prob)
    output<- rbinom(n,1,prob)
    output[output==0]=-1
  } else {
    output <- sapply(index, scalar_predict, object, newx, type)
    output <- as.matrix(output)
  }
  return(output)
}

#' @title Extract coefficients from a cv.SplitReg object.
#' @method coef cv.SplitReg
#' @param object Fitted cv.SplitReg object.
#' @param index Indices indicating values of lambda_S at which to extract coefficients. Defaults to the optimal value.
#' @param ... Additional arguments for compatibility.

#' @return A vector of coefficients
#' 
#' @description 
#' Extract coefficients from a cv.SplitReg object.
#' 
#' @seealso 
#' \code{\link{cv.SplitReg}}
#' 
#' @examples 
#' library(MASS)
#' set.seed(1)
#' beta <- c(rep(5, 5), rep(0, 45))
#' Sigma <- matrix(0.5, 50, 50)
#' diag(Sigma) <- 1
#' x <- mvrnorm(50, mu = rep(0, 50), Sigma = Sigma)
#' y <- x %*% beta + rnorm(50)
#' fit <- cv.SplitReg(x, y, num_models=2)
#' split.coefs <- coef(fit)
#' 
coef.cv.SplitSvm <- function(object, index=object$index_opt,...){
  return(predict(object, index = index, type = "coefficients"))
}