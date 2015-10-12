my_lm_4b <- function (formula, data, subset, weights, na.action, method = "qr", 
                   model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
                   contrasts = NULL, offset, ...) 
{
  #print memory usage on every step
  memUsed<-quote(print(paste("Memory used:",round( mem_used()/10^6,1),"mb")))
  
  eval(memUsed)
  ret.x <- x
  
  
  
  eval(memUsed)
  ret.y <- y
  eval(memUsed)
  cl <- match.call()
  
  eval(memUsed)
  mf <- match.call(expand.dots = FALSE)
  eval(memUsed)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  
  
  
  eval(memUsed)
  mf <- mf[c(1L, m)]
  eval(memUsed)
  mf$drop.unused.levels <- TRUE
  eval(memUsed)
  mf[[1L]] <- quote(stats::model.frame)
  
  
  
  eval(memUsed)
  print("FIRST JUMP")
  print(paste("object mf size before eval =", object.size(mf)))
  mf <- eval(mf, parent.frame())
  print("HEAD OF MF")
  print(head(mf))
  print(paste("The dimensions of mf are 1 000 000 x 4"))
  print(paste("The typeof(mf) is", unlist(typeof(mf))))
  print(paste("The typeof(mf[1,1]) is", unlist(typeof(mf[1,1]))))
  print(paste("object mf size after eval =", object.size(mf)))
  eval(memUsed)
  print(paste("object size mf[1,1] after eval =", object.size(mf[1,1])))
  print("Comment FIRST JUMP: This is perfectly logic as mf is the observations (1*8mb) and the predictors (3*8mb)")
  
  
  
  
  if (method == "model.frame") {
    return(mf)
    eval(memUsed)
  }
  else if (method != "qr") {
    eval(memUsed)
    warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                     method), domain = NA)
  }
  eval(memUsed)
  mt <- attr(mf, "terms")
  
  
  
  
  
  
  cat("\n")
  print("SECOND JUMP")
  eval(memUsed)
  print(paste("object y size before eval =", object.size(y)))
  print("Objects in the environment ls() before y eval")
  print(ls())
  y <- model.response(mf, "numeric")  
  print("Objects in the environment ls() after y eval")
  print(ls())
  print("HEAD OF y")
  print(head(y))
  
  print(paste("The typeof(y) is", typeof(y)))
  print(paste("The typeof(y[1]) is", typeof(y[1])))
  print(paste("The length of y is", length(y)))
  print(paste("object y size after eval =", object.size(y)))
  eval(memUsed)
  print(paste("object size y[1] after eval =", object.size(y[1])))
  print("Comment SECOND JUMP: This is not logic, since y is a vector of 10^6 values it should only add 8mb extra. However the attributes associated with y take up the memory that is larger than 8mb.")
  print(ls())
  print("END SECOND JUMP")
  cat("\n")
  
  
  
  
  
  
  w <- as.vector(model.weights(mf))
  eval(memUsed)
  
  #### BEGIN: The function never enters here in our analysis
    if (!is.null(w) && !is.numeric(w)) {
      eval(memUsed)
      print("bbbb")
      stop("'weights' must be a numeric vector")
    }
  #### END: The function never enters here
  
  
  eval(memUsed)
  offset <- as.vector(model.offset(mf))
  eval(memUsed)
  
  
  #### BEGIN: The function never enters here in our analysis
    if (!is.null(offset)) {
      eval(memUsed)
      print("qqq2q")
      if (length(offset) != NROW(y)) {
        eval(memUsed)
        stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                      length(offset), NROW(y)), domain = NA)
      }
    }
    if (is.empty.model(mt)) {
      print("ccccc")
      eval(memUsed)
      x <- NULL
      eval(memUsed)
      z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
                                                        3) else numeric(), residuals = y, fitted.values = 0 * 
                  y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
                                                                                  0) else if (is.matrix(y)) nrow(y) else length(y))
      print("cccc")
      print(object.size(z))
      if (!is.null(offset)) {
        print("ddddd")
        eval(memUsed)
        print(object.size(z))
        z$fitted.values <- offset
        print(object.size(z))
        eval(memUsed)
        z$residuals <- y - offset
        print(object.size(z))
        eval(memUsed)
      }
    }
   
   
   
  else {
    
  #### END: The function never enters here in our analysis
    
    
  cat("\n")
  print("LAST JUMP")

  eval(memUsed)
  print(paste("object x size before eval =", object.size(x)))
  cat("\n")
  print("Objects in the environment ls() before x eval")
  print(ls())
  cat("\n")
  x <- model.matrix(mt, mf, contrasts)
  print("Objects in the environment ls() after x eval")
  print(ls())
  cat("\n")
  print(paste("object x size after eval =", object.size(x)))
  print("HEAD OF x")
  print(head(y))
  print(paste("The typeof(x) is", typeof(x)))
  print(paste("The typeof(x[1,1]) is", typeof(x[1,1])))
  print("The dim of x is (see below)")
  print(dim(x))
  print("Comment LAST JUMP: x is a [10^6 x 4] matrix with values for the intercept and the three covariates. It should only be 32mb, although the size is 88mb. The extra memory is taken up by the dimnames of the attributes.")
  cat("\n")
  eval(memUsed)
  print("END LAST JUMP")
  cat("\n")
  
  
  
  
  
  
  z <- if (is.null(w)) {
    ### LM.FIT IS CALLED HERE
    print(paste("Memory used before lm.fit:", round(mem_used()/10^6,1),"mb")) # FIRST PRINT
    lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
           ...)
  }
  else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
               ...)
  }
  class(z) <- c(if (is.matrix(y)) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- x
  if (ret.y) 
    z$y <- y
  if (!qr) 
    z$qr <- NULL
  z
}