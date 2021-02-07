pow_noninf_bin <- function(numsims, nperarm, ctrlrate, trmtrate, delta){
  yctr <- rbinom(numsims, nperarm, ctrlrate)
  ytrt <- rbinom(numsims, nperarm, trmtrate)
  phatctrl <- yctr/nperarm
  phattrmt <- ytrt/(nperarm)
  lowerCL = phattrmt - phatctrl -
    qnorm(0.025)*sqrt((phatctrl*(1-phatctrl)/nperarm) +
                        (phattrmt*(1-phattrmt)/nperarm))
  return(mean(lowerCL< delta))
 }







