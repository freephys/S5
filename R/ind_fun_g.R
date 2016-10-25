ind_fun_g <-
function(ind2){ 
  n = nrow(X)
  p = ncol(X)
  g = tuning
  a0=0.01;b0=0.01
  tau = 1
  p.g=length(ind2)
  sb = lbeta(1+p.g,1+p-p.g)
  if(p.g >1){
    #X0 = X[,ind2];QR=qr(X0)
    #ress = crossprod(qr.resid(QR, y))
    fit = solve(crossprod(X[,ind2]))%*%crossprod(X[,ind2],y)
    ress = crossprod(y-X[,ind2]%*%fit)
    v = crossprod(y-mean(y))
    int = -0.5*p.g*log(1+g)-0.5*(n-1)*log(1+g*(ress/v))
    
  }else{
    if(p.g==1){
      fit = as.numeric(as.numeric(sum(X[,ind2]*y))/as.numeric(crossprod(X[,ind2])))
      ress = crossprod(y-X[,ind2]*fit)
      v = crossprod(y-mean(y))
      int = -0.5*p.g*log(1+g)-0.5*(n-1)*log(1+g*(ress/v))
      
    }else{if(p.g==0){
      int = -0.5*(n-1)*log(1+g)
    }
    }
  }
  return(int)
}
