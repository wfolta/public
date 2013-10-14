stan.code <-
"data
   {
   int cut ;
   int nu ;
   int N ;
   real dollars[N] ;
   real dca[N] ;
   int rate[N] ;
   int regime[N] ;
   }

transformed data
   {
   real dcaX[N] ;
   int ratetemp[N] ;
   
   for (n in 1:N)
      {
      dcaX[n] &lt;- dca[n] - cut ;

      if (dcaX[n] &lt; 0)
         { ratetemp[n] &lt;- 1 ; }
      else if (rate[n] == 1)
         { ratetemp[n] &lt;- 2 ; }
      else
         { ratetemp[n] &lt;- 3 ; }
      }
   }

parameters
   {
   real alpha[3] ;       // intercept, per ratetemp
   real alpha_alpha ;
   real alpha_sigma ;

   real beta[3] ;        // slope, per ratetemp
   real beta_alpha ;
   real beta_sigma ;

   real gamma[5] ;       // intercept, per regime
   real gamma_alpha ;
   real gamma_sigma ;

   real sigma[3] ;
   real sigma_alpha ;
   real sigma_sigma ;
   }

model
   {
   alpha ~ normal (alpha_alpha, alpha_sigma) ;
   alpha_alpha ~ normal (570, 150) ;
   alpha_sigma ~ cauchy (0, 70) ;

   beta ~ normal (beta_alpha, beta_sigma) ;
   beta_alpha ~ normal (0, 15) ;
   beta_sigma ~ cauchy (0, 10) ;

   gamma ~ normal (gamma_alpha, gamma_sigma) ;
   gamma_alpha ~ normal (0, 5) ;
   gamma_sigma ~ cauchy (0, 80) ;

   sigma ~ cauchy (sigma_alpha, sigma_sigma) ;
   sigma_alpha ~ cauchy (0, 40) ;
   sigma_sigma ~ cauchy (0, 5) ;

   for (n in 1:N)
      {
      dollars[n] ~ normal (gamma[regime[n]] + alpha[ratetemp[n]] + beta[ratetemp[n]] * dcaX[n], sigma[ratetemp[n]]) ;
      // dollars[n] ~ student_t (nu, gamma[regime[n]] + alpha[ratetemp[n]] + beta[ratetemp[n]] * dcaX[n], sigma[ratetemp[n]]) ;
      }
   }
"
