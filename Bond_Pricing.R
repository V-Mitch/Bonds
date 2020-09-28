# Bond calculations

# Input percentage values for yield and couponrate
CalcBondPrice <-
  function(yield,
           couponrate = 0,
           couponsperyear = 0,
           duration = 1,
           facevalue = 100,
           printinfo = TRUE) {
    yield_deci <- yield / 100
    couponrate_deci <- couponrate / 100
    
    # Calculation of the bond price and other metrics
    
    # Calculation if there are no coupons at all
    coupons_present <- TRUE
    if (couponrate == 0 || couponsperyear == 0) {
      coupons_present <- FALSE
      Price <- facevalue / (1 + yield_deci)
    }
    
    # Calculation if there are coupons
    else{
      Price <-
        couponrate * 1 / yield_deci * (1 - 1 / (1 + yield_deci) ^ (duration * couponsperyear)) +
        facevalue / (1 + yield_deci) ^ (duration * couponsperyear)
    }
    coupon <- (couponrate_deci * facevalue) / (couponsperyear)
    
    if(printinfo == TRUE){
    # Additional information for the user about the bond
    if (Price > facevalue) {
      print(paste("Bond is trading at a premium. The price should be: ", Price))
      if (coupons_present == TRUE) {
        print(paste("The Coupons are worth ", coupon, "each"))
      }
    }
    else if (Price < facevalue) {
      print(paste("Bond is trading at a discount. The price should be: ", Price))
      if (coupons_present == TRUE) {
        print(paste("The Coupons are worth ", coupon, "each"))
      }
    }
    else {
      print(paste(
        "Bond is trading at par and the price equal to facevalue should be: ",
        Price
      ))
      if (coupons_present == TRUE) {
        print(paste("The Coupons are worth ", coupon, "each"))
      }
    }
    }
    return(Price)
  }

vecdat <- vector(mode = "numeric", length = 100)
for(i in 1:100){
  vecdat[i] <- CalcBondPrice(i,5,1,1,100)
}

CalcBondYield <- function(price, couponrate = 0, couponsperyear = 0, duration = 1, facevalue = 100, precision = 1){
  
  gridvalues <- seq(0,300, by = 1/precision)
  resultmat <- matrix(nrow = length(gridvalues), ncol = 3)
  
  for(i in gridvalues[1]:gridvalues[length(gridvalues)]){
  synthprice <- CalcBondPrice(yield = i , couponrate, couponsperyear, duration, facevalue, printinfo = FALSE)
  err <- abs(price - synthprice)
  
  resultmat[i,1] <- synthprice
  resultmat[i,2] <- err
  resultmat[i,3] <- i
  
  }
  result <- data.frame(format((resultmat[which.min(resultmat[,2]),]), scientific = F), row.names =
                         c("Closest Price", "Difference with Desired Price", "Yield"))
  colnames(result) <- "Bond Yield Calculation"
  return(result)
}



1 / (2*(2-1)*89.72486) * ( (2 * (2-1)/2 - 89.72486) + 
  sqrt((2*(89.72486 - ((2-1)/2)))^2 - 2*2*(2-1) * 89.72486 * (89.72486 - 100 - 2 * 5)))








