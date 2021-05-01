# test <- function(n,c1,c2,...){
#   cf <- colorRampPalette(c(c1,c2,...))
#   return(cf(n))
# }

damped_sine = function(a = 0.7,
                       b = 3.4,
                       c = 2,
                       d = 0,
                       x){((a / (b+x)) * sin(((2*pi) / c)*x))+d}

# par(mfrow = c(3,3),mai = rep(0.5,4))

# for(i in c(1:9)){
  
  out <- damped_sine(x = seq(0,100,0.25),
                     a = .2,
                     b = 50,
                     c = sample(c(10,10.1,10.2,10.5,10.75),1),
                     d = 0.1)
  
  out %>% 
    plot(type='l',
         ylim = c(0,1),
         col = sample(test(n = 5, 'blanchedalmond','indianred'),1),
         ylab = NA, xlab = NA, xaxt = 'n',yaxt = 'n',frame.plot = F,
         lwd = sample(c(0.5,1,2),1))
  
  for(i in seq(0.1,1,.01)){
      
      damped_sine(x = seq(0,100,0.25),
                  a = 0.5,
                  b = 50,
                  c = sample(c(10,10.1,10.2,10.5,10.75),1),
                  d = i) %>% 
        lines(col = sample(test(n = 5, 'indianred','blanchedalmond'),1),
              lwd = sample(c(0.5,1,2),1))
      
    }
  }
# }
