\name{hyper_par}
\alias{hyper_par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tuning parameter selection for nonlocal priors }
\description{
Hyper parameter tau selection for nonlocal priors using random sampling from the null distribution (Nikooienejad et al, 2016). 
}
\usage{
hyper_par(type, X, y, thre = p^-0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
the type of nonlocal priors; 'pimom' or 'pemom'.
}
  \item{X}{
the covariate matrix (a standardization is recommneded for nonlocal priors).
}
  \item{y}{
the response variable. 
}
  \item{thre}{
the threshold; for details, see below. The default is p^-0.5.
}
}
\details{
Nikooienejad et al. (2016) proposed a novel approach to choose the hyperparameter tau for nonlocal priors. They first derive the null distribution of the regression coefficient by randomly sampling the covariates, and shuffle the index of the samples in the covariates. Then, they calculate the MLE from the sampled covariates that are shuffled. This process is repeated large enough times to approximate the null distribution of the MLE under the situation where all true regression coefficients are zero. They compare the nonlocal density with different values of the parameter to the null distribution so that the overlap of these densities falls below the threshold; see Nikooienejad et al. (2016) for further details. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{tau}{  : the choosen hyper parameter tau}
%% ...
}

\references{
Shin, M., Bhattacharya, A., Johnson V. E. (2016+) A Scalable Bayesian Variable Selection Using Nonlocal Prior Densities in Ultrahigh-dimensional Settings, under revision in Statistica Sinica. 


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
\code{\link{ind_fun_pimom}}, \code{\link{ind_fun_pemom}}
}
\examples{
p=5000
n = 200

indx.beta = 1:5
xd0 = rep(0,p);xd0[indx.beta]=1
bt0 = rep(0,p); 
bt0[1:5]=c(1,1.25,1.5,1.75,2)*sample(c(1,-1),5,replace=TRUE)
xd=xd0
bt=bt0
X = matrix(rnorm(n*p),n,p)
y = crossprod(t(X),bt0) + rnorm(n)*sqrt(1.5) 
X = scale(X)
y = y-mean(y)
y = as.vector(y)

# piMoM  
C0 = 1 # the number of repetitions of S5 algorithms to explore the model space
tuning = 10 # tuning parameter
#tuning = hyper_par(type="pimom",X,y,thre = p^-0.5)
print(tuning)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tuning}
\keyword{pimom}
\keyword{pemom}
