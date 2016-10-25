\name{S5_parallel}
\alias{S5_parallel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel version of S5
}
\description{
The parallel version of the S5. Multiple S5 chains independently explore the model space to enhance the capacity of searching interesting region of the model space.
}
\usage{
S5_parallel(NC,X,y,ind_fun,model,tuning,tem,ITER=20,S=20,C0=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	 \item{NC}{
the number of cores (the number of parallel S5 chains) to be used.
}
   \item{X}{
the covariate matrix (a standardization is recommneded for nonlocal priors).
}
  \item{y}{
the response variable. 
}
  \item{ind_fun}{
  the log-marginal likelihood function of models, which is resulted from a pred-specified priors on the regression coefficients. The default is piMoM
}
  \item{model}{
the model prior; Uniform or Bernoulli_Uniform. The default is Bernoulli_Uniform
}
  \item{tuning}{
the tuning parameter for the objective function (tau for piMoM and peMoM priors; g for the g-prior).
}
  \item{tem}{
the temperature schedule. The default is the square of seq(0.4,1,length.out=20).
}
  \item{ITER}{
 the number of iterations in each temperature; default is 20.
}
  \item{S}{
the screening size of variables; default is 20.
}
  \item{C0}{
the number of repetition of  the S5 algorithm C0 times,default is 1. When the total number of variables is huge and real data sets are considered, using a large number of C0 is recommended, e.g., C0=10.  
}
}
 
\details{
Using the S5 (Shin et al., 2016+), you will get all the models searched by S5 algorithm, and their corresponding log (unnormalized) posterior probabilities, and also this function can receive searched model for g-prior,piMoM,and peMoM. 

After obtaining the object of the S5 function, by using the 'result' function, you can obtain the posterior probabilities of the searched models including the MAP model and  the marginal inclusion probabilities of each variable. 

By using the procedure of Nikooienejad et al. (2016), the 'hyper_par' function chooses the tuning parameter for nonlocal priors (piMoM or peMoM priors).
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{GAM}{ the binary vaiables of searched models by S5}
\item{OBJ}{ the corresponding log (unnormalized) posterior probability}
%% ...
}

\references{
Shin, M., Bhattacharya, A., Johnson V. E. (2016+) A Scalable Bayesian Variable Selection Using Nonlocal Prior Densities in Ultrahigh-dimensional Settings, under revision in Statistica Sinica. 

Hans, C., Dobra, A., and West, M. (2007). Shotgun stochastic search for large p regression. Journal of the American Statistical Association, 102, 507-516.


Nikooienejad,A., Wang, W., and Johnson V.E. (2016). Bayesian variable selection for binary outcomes in high dimensional genomic studies using non-local priors. Bioinformatics, 32(9), 1338-45.

}

\author{
Shin Minsuk and Ruoxuan Tian
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{result}}, \code{\link{S5}}  
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

library(MASS)
Boston = MASS::Boston
p = 3000
n = nrow(Boston)
y = as.vector(Boston$medv)
X = as.matrix(Boston[,-14])
X = cbind(X,matrix(rnorm(n*p),n,p)) # add noise variables
X = scale(X)
y = y-mean(y)

### parallel version of S5 (defalut)
fit_parallel = S5_parallel(NC=2,X,y)
 

#fit_parallel$GAM # the searched models by S5
#fit_parallel$OBJ # the corresponding log (unnormalized) posterior probability

res_parallel = result(fit_parallel)
#str(res_parallel)
print(res_parallel$hppm) # the MAP model 
print(res_parallel$hppm.prob) # the posterior probability of the hppm 
plot(res_parallel$marg.prob,ylim=c(0,1),ylab="marginal inclusion probability") # the marginal inclusion probability 

### parallel version of S5
NC = 2 # the number of cores for the prallel computing
C0 = 5 # the number of repetitions of S5 algorithms to explore the model space
tuning = hyper_par(type="pimom",X,y,thre = p^-0.5)  # tuning parameter selection for nonlocal priors
print(tuning) 

ind_fun = ind_fun_pimom # the log-marginal likelihood of models based on piMoM prior (nonlocal prior (piMoM 'ind_fun_pimom' and peMoM prior 'ind_fun_pemom') or Zellner's g-prior 'ind_fun_g')
model = Bernoulli_Uniform # the log-marginal likelihood of models based on piMoM prior ('Uniform' or 'Bernoulli_Uniform').
tem =  seq(0.4,1,length.out=20)^2 # the temperatures schedule
fit_parallel = S5_parallel(NC=2,X,y,ind_fun,model,tuning,tem,C0=C0)
 

#fit_parallel$GAM # the searched models by S5
#fit_parallel$OBJ # the corresponding log (unnormalized) posterior probability

res_parallel = result(fit_parallel)
#str(res_parallel)
print(res_parallel$hppm) # the MAP model 
print(res_parallel$hppm.prob) # the posterior probability of the hppm 
plot(res_parallel$marg.prob,ylim=c(0,1),ylab="marginal inclusion probability") # the marginal inclusion probability 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{Simplified Shotgun Stochastic Search with Screening}% use one of  RShowDoc("KEYWORDS")
\keyword{Shotgun Stochastic Search}% use one of  RShowDoc("KEYWORDS")
\keyword{Bayesian variable selection}% __ONLY ONE__ keyword per line
\keyword{g-prior}
\keyword{nonlocal prior}
\keyword{piMoM}
\keyword{peMoM}
