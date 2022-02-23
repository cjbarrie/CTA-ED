
rm(list=ls(all=TRUE))
library(tm)




# =====================
# WORDFISH VERSION 1.3
# =====================
#
# If you use WORDFISH, you agree to cite the following:
#
# Jonathan B. Slapin and Sven-Oliver Proksch (2008). "A Scaling 
# Model for Estimating Time-Series Party Positions from Texts", 
# American Journal of Political Science 52(3), pp.705-772.
#
# and 
# 
# Sven-Oliver Proksch and Jonathan B. Slapin (2009). WORDFISH: 
# Scaling Software for Estimating Political Positions from Texts. 
# Version 1.3. 15 January 2009. http://www.wordfish.org. 
#
# =====================
#
#  Copyright (C) 2007-2009 Sven-Oliver Proksch and Jonathan B. Slapin
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# =====================
#
# Description:
#
# Scaling model to estimate one-dimensional positions
# from texts. Poisson Model with word and actor fixed 
# effects
# alpha <- set of P document fixed effects (document 1 has alpha=0)
# omega <- set of P document positions 
# psi <- set of W word effects
# b   <- set of W word weights
#
# P <- documents
# W <- words 
#
#  y_ij ~ poisson(exp(alpha_i + psi_j + omega_i * beta_j)))
# 
# January 23, 2009
# General Function which should work for all texts
# Includes Bootstrap option for confidence intervals
#
# Questions/comments are welcome:
# Jonathan B. Slapin (slapinj@tcd.ie)
# Sven-Oliver Proksch (proksch@uni-mannheim.de)
#
# Please consult www.wordfish.org for a detailed manual and answers to FAQs.
#
# =====================


#####################################################################################
# 
# 1: input	term-document matrix
# 2: wordsincol	(if TRUE then the unique words are in the first column of the term
#					document matrix and will be removed. If FALSE, unique words are the 
#					rownames of the term-document matrix (default is FALSE))
# 3: fixtwo (if FALSE identifies the model by constraining omegas 
#			   to have mean 0 and std. dev. 1. If TRUE identifies the model
#			   by fixing two document positions at set values; these must be given
#				by fixdoc. Default is FALSE)
# 4: dir    (if fixtwo=FALSE, document whose position is constrained to be negative; 
# 			   if no value is provided, constrains first document to have a negative value. 
#				Necessary for global identification of mean 0 and SD 1 approach.)
# 5: fixdoc (if fixtwo=TRUE, vector of two documents and their set positions (four elements). 
#				This vector must be provided by the user, there are no default values. The first two 
#				vector elements indicate the documents in the matrix, the last two the fixed
#				values. E.g. If the first and second document in the matrix are to be constrained 
#				to have omegas -10 and +10, then this vector should look like this: 
#				fixdoc=c(1,2,-10,10). The two values cannot be identical.)
# 6: tol   (tolerance criteria for convergence, default is 1e-6)
# 7: sigma (variance parameter to constrain beta, default is 3)
# 8: boot  (if TRUE runs parametric bootstrap, default is FALSE)
# 9: nsim  (number of bootstrap simulations if bootstrap is turned on, default is 500)
# 10: output		name of output file if writeout=TRUE
# 11: writeout (if TRUE writes three output files to working directory)

#####################################################################################

wordfish<- function(input,wordsincol=FALSE,fixtwo=FALSE,dir=NULL,fixdoc=c(1,2,0,1),tol=1e-7,sigma=3,boots=FALSE,nsim=500,writeout=FALSE,output="wordfish_output") {    
    dta<-input 
    
    if(wordsincol==TRUE){
    	rownames(dta)<-dta[,1]
    	dta <- dta[,-c(1)]
    }
	 
	 dta <- t(dta)
    words<- colnames(dta)
    nparty<-nrow(dta)  
    nword<-ncol(dta)
	 
	 if(fixtwo==TRUE){
	 	  if (fixdoc[3]==fixdoc[4]){
	 	  	cat("Warning: fixed omega values in 'fixdoc' cannot be identical. \n")
	 		stop()

	 	  	
	 	  	}
	 	
	 		identprint<-paste("Omegas identified with",rownames(dta)[fixdoc[1]],"=",fixdoc[3],"and ",rownames(dta)[fixdoc[2]],"=",fixdoc[4])
	 	}
	 else{
	 	
	 	if(sum(c(length(dir)==2,is.numeric(dir)))!=2){
	 		cat("Warning: option 'dir' in wordfish() is empty. You must specify two documents for global identification (e.g. dir=c(1,2) ).\n")
	 		stop()
	 		}
	 	
	 		identprint<-paste("Omegas identified with mean 0, st.dev. 1")
	 	}
	 
	 
	 cat("======================================\n")
	 cat("WORDFISH (Version 1.3)\n")
	 cat("======================================\n")
    cat("Number of unique words: ",nword,"\n")
    cat("Number of documents: ",nparty,"\n")
    cat("Tolerance criterion: ",tol,"\n")
    cat("Identification: ",identprint,"\n")
    cat("======================================\n")

    # Generate starting values
    # ========================
	
	 if(fixtwo==FALSE){
	 	
	 	
	 	
	 	rockingstarts <- function(dta) {
	 	  cat("Performing mean 0 sd 1 starting value calc\n")	
        P <- nrow(dta)
        W <- ncol(dta)
        numword<-rep(1:W,each=P)
        numparty<-rep(1:P,W)
        dat <- matrix(1,nrow=W*P,ncol=3)
        dat[,1]<-as.vector(as.matrix(dta))
        dat[,2]<-as.vector(numword)
        dat[,3]<-as.vector(numparty)
        dat <- data.frame(dat)
        colnames(dat) <- c("y","word","party")
        dat$word <- factor(dat$word)
        dat$party <- factor(dat$party)

        # Starting values for psi   
        psi <- log(colMeans(dta))
        # Starting values for alpha
        alpha  <- log(rowMeans(dta)/rowMeans(dta)[1]) 

        # Starting values for beta and x        
        ystar <- log(dat$y+0.1) - alpha[dat$party] - psi[dat$word]
        res   <- svd(matrix(ystar,nrow(dta),ncol(dta),byrow=FALSE),nu=1)
        b <- as.vector(res$v[,1]*res$d[1])
        
        omega1 <- as.vector(res$u)-res$u[1,1]
        omega <- omega1/sd(omega1)
        b <- b*sd(omega1)

		

        # Create holding bins for some stuff for the convergence code
        min1<-c(rep(1,nrow(dta)-1))
        min2<-c(rep(1,ncol(dta)))
        iter<-0
        conv<-0
        diffparam<-0

        # Put everything together in a list
        list(alpha=as.vector(alpha),psi=as.vector(psi),b=b,omega=omega,
                   min1=min1,min2=min2,iter=iter,conv=conv,diffparam=diffparam)
    	  }
 	
	}
	else{
	
		
	 	rockingstarts <- function(dta,fixval) {
	 		cat("Performing fix two omega starting value calc\n")
        P <- nrow(dta)
        W <- ncol(dta)
        numword<-rep(1:W,each=P)
        numparty<-rep(1:P,W)
        dat <- matrix(1,nrow=W*P,ncol=3)
        dat[,1]<-as.vector(as.matrix(dta))
        dat[,2]<-as.vector(numword)
        dat[,3]<-as.vector(numparty)
        dat <- data.frame(dat)
        colnames(dat) <- c("y","word","party")
        dat$word <- factor(dat$word)
        dat$party <- factor(dat$party)

        # Starting values for psi   
        psi <- log(colMeans(dta))
        # Starting values for alpha
        alpha  <- log(rowMeans(dta)/rowMeans(dta)[1]) 

        # Starting values for beta and x        
        ystar <- log(dat$y+0.1) - alpha[dat$party] - psi[dat$word]
        res   <- svd(matrix(ystar,nrow(dta),ncol(dta),byrow=FALSE),nu=1)
        b <- as.vector(res$v[,1]*res$d[1])
        
        omega <- as.vector(res$u)
        
		        


        # Create holding bins for some stuff for the convergence code
        min1<-c(rep(1,nrow(dta)-1))
        min2<-c(rep(1,ncol(dta)))
        iter<-0
        conv<-0
        diffparam<-0

        # Put everything together in a list
        list(alpha=as.vector(alpha),psi=as.vector(psi),b=b,omega=omega,
                   min1=min1,min2=min2,iter=iter,conv=conv,diffparam=diffparam)
    	  }
	
	
	}	
    

    # Log-Likelihood Functions (Poisson model)
    # ========================================

    llik_psi_b <- function(p,y,omega,alpha,sigma) { # beta and psi will be estimated 
        b   <- p[1]   
        psi <- p[2]
       lambda<-exp(psi+alpha+b*omega)                  # Lambda parameter for Poisson distribution
       -(sum(-lambda+log(lambda)*y)-0.5*(b^2/sigma^2)) # Log-likelihood including normal prior on Beta
    }


    llik_alpha_1 <- function(p,y,b,psi) { # omega[1] is estimated
        omega <- p[1]
        lambda<-exp(psi+b*omega)        # Lambda parameter; alpha is excluded b/c it is set to be zero
        -sum(-lambda+log(lambda)*y)     # Log-likelihood
    }

    llik_alpha_omega <- function(p,y,b,psi) { # all other omegas and alphas are estimated
        omega <- p[1]
        alpha <- p[2]
        lambda<-exp(psi+alpha+b*omega)      # Lambda parameter
        -sum(-lambda+log(lambda)*y)     # Log-likelihood
    }


	  llik_justalpha <- function(p,y,b,psi,omega) { # alpha is estimated
        alpha <- p[1]
        lambda<-exp(psi+alpha+b*omega)      # Lambda parameter
        -sum(-lambda+log(lambda)*y)     # Log-likelihood
    }


  


	if(fixtwo==FALSE){
		
		
		   cat("Performing mean 0 sd 1 EM algorithm\n")
   			# Expectation-Maximization Algorithm FOR MEAN 0, SD 1 IDENTIFICATION
    		# ==================================================================

   			rockingpoisson <- function(dta,tol,sigma,params=NULL,dir=dir,printsum=TRUE) {

        	P <- nrow(dta)
        	W <- ncol(dta)
	
		 	if (is.null(params)) {
            params <- rockingstarts(dta) # Call up starting value calculation
        	}
		 
		 	iter<-2
        	maxllik<-cbind(-1e70,rep(0,1400))
        	ll.words<-matrix(-1e70,W,1400)
	      diffllik<-500

      	  	# Set the convergence criterion 
        	conv<-tol
        	params$conv<-conv   

        	while (diffllik>conv) { # Run algorithm if difference in LL > convergence criterion
            omegaprev<-params$omega
            bprev<-params$b   
            alphaprev<-params$alpha       
            psiprev<-params$psi 

            # ESTIMATE OMEGA AND ALPHA

			  if(printsum==TRUE){
            cat("Iteration",iter-1,"\n")
            cat("\tUpdating alpha and omega..\n")
            }
                   	
            	
            
            # Estimate first omega (alpha is set to 0)
            resa <- optim(p=c(params$omega[1]),
                        fn=llik_alpha_1,                        
                        y=as.numeric(dta[1,]),
                        b=params$b,
                        psi=params$psi,
                        method=c("BFGS")
                        )
                params$omega[1] <- resa$par[1]
                params$min1[1] <- -1.00*resa$value
                params$alpha[1] <- 0
                ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
               
                
          # Estimate all other omegas and alphas    
            for (i in 2:P) {
                
                 resa <- optim(par=c(params$omega[i],params$alpha[i]),
                        fn=llik_alpha_omega,
                        y=as.numeric(dta[i,]),
                        b=params$b,
                        psi=params$psi)
                    params$omega[i] <- resa$par[1]
                    params$alpha[i] <- resa$par[2]
                    params$min1[i] <- -1.00*resa$value
                    ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
                    
                }    
 				
            flush.console()


           	# Z-score transformation of estimates for omega (to identify model)
           	omegabar     <- mean(params$omega)
           	b1       <- params$b
           	params$b <- params$b * sd(params$omega)
           	params$omega <- (params$omega - omegabar)/ sd(params$omega)
           	params$psi <- params$psi + b1*omegabar  

				# Global identification
				if (params$omega[dir[1]]>params$omega[dir[2]]){params$omega<-params$omega*(-1)}
				
	

        	# ESTIMATE PSI AND BETA
			  if(printsum==TRUE){
        	cat("\tUpdating psi and beta..\n")}

              for (j in 1:W) {                        
                 resb <- optim(par=c(params$b[j],params$psi[j]),
                       fn=llik_psi_b,
                       y=dta[,j], 
                       omega=params$omega,
                       alpha=params$alpha,
                       sigma=sigma
                       )
                params$b[j] <- resb$par[1]
                params$psi[j] <- resb$par[2]
                params$min2[j] <- -1.00*resb$value
                ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
                }    

             flush.console()

        	# Calculate Log-Likelihood
        	maxllik[iter]<-sum(params$min2)
        	diffparam<-mean(abs(params$omega-omegaprev)) # difference btw current & previous estimate for omega

        	ll.words[,iter]<-params$min2
        	diff.ll.words<-(ll.words[,iter]-ll.words[,iter-1])
        	diffllik<-sum(diff.ll.words)/abs(maxllik[iter])
			
			
			if(printsum==TRUE){
				#print(sum(diff.ll.words))
				#print(abs(maxllik[iter]))
		 	 	cat("\tConvergence of LL: ",diffllik,"\n")
        	}
        	
        	params$diffllik[iter-1]<-diffllik
        	params$diffparam[iter-1]<-diffparam
        	params$diffparam.last<-diffparam
        	params$maxllik[iter-1]<-maxllik[iter]
        	params$iter<-iter-1
        	iter<-iter+1
        	}    
	     	params$diffllik[1]<-NA 
	     	return(params)
    	}

	 #  Run the algorithm
    est <- rockingpoisson(dta,tol,sigma,dir=dir) 
    }
    
    else{
    	  cat("Performing fix two omega EM algorithm\n")
    	 
    	# Expectation-Maximization Algorithm FOR TWO FIXED OMEGAS
    	# ==================================================================

	   rockingpoisson <- function(dta,tol,sigma,params=NULL,fixdoc=fixdoc,printsum=TRUE) {

      P <- nrow(dta)
      W <- ncol(dta)

	 	if (is.null(params)) {
            params <- rockingstarts(dta,fixval=fixdoc) # Call up starting value calculation
      	}

		iter<-2
      maxllik<-cbind(-1e70,rep(0,1000))
      ll.words<-matrix(-1e70,W,1000)

      diffllik<-500

      # Set the convergence criterion 
      conv<-tol
      params$conv<-conv   

      while (diffllik>conv) { # Run algorithm if difference in LL > convergence criterion
            omegaprev<-params$omega
            bprev<-params$b   
            alphaprev<-params$alpha       
            psiprev<-params$psi 

            # ESTIMATE OMEGA AND ALPHA

            if(printsum==TRUE){
            cat("Iteration",iter-1,"\n")
            cat("\tUpdating alpha and omega..\n")
            }


			 # Set omegas and first alpha 
            		
                params$omega[fixdoc[1]] <- fixdoc[3]
                params$omega[fixdoc[2]] <- fixdoc[4]
                params$alpha[1] <- 0 


			 if(1 %in% fixdoc[1:2]==TRUE){

            	   # if first doc is one of the fixed omegas, do nothing (alpha and omega are fixed)

					}

			 else{
					# Estimate first omega (alpha is set to 0)
            		resa <- optim(p=c(params$omega[1]),
                        fn=llik_alpha_1,                        
                        y=as.numeric(dta[1,]),
                        b=params$b,
                        psi=params$psi,
                        method=c("BFGS")
                        )
                params$omega[1] <- resa$par[1]
                params$min1[1] <- -1.00*resa$value
                params$alpha[1] <- 0
                ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
					}
			
			
			
			
          # Estimate all other omegas and alphas    
            for (i in 2:P) {
            	
            	
            	if(sum(fixdoc[1:2]==i)==1){
            		
					   # Estimate just alpha
			 				resa <- optim(par=params$alpha[i],
                        fn=llik_justalpha,
                        y=as.numeric(dta[i,]),
                        b=params$b,
                        psi=params$psi,
                        omega=params$omega[i],
                        method=c("BFGS"))
                    	params$alpha[P] <- resa$par[1]
                    
                    	ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
            		
            		 }
            	
            	else{
                   resa <- optim(par=c(params$omega[i],params$alpha[i]),
                        fn=llik_alpha_omega,
                        y=as.numeric(dta[i,]),
                        b=params$b,
                        psi=params$psi)
                    params$omega[i] <- resa$par[1]
                    params$alpha[i] <- resa$par[2]
                    params$min1[i] <- -1.00*resa$value
                    ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
                }    

				}
           
           
           flush.console()


         
        # ESTIMATE PSI AND BETA
			  if(printsum==TRUE){
        		cat("\tUpdating psi and beta..\n")
				}
				
              for (j in 1:W) {                        
                 resb <- optim(par=c(params$b[j],params$psi[j]),
                       fn=llik_psi_b,
                       y=dta[,j], 
                       omega=params$omega,
                       alpha=params$alpha,
                       sigma=sigma
                       )
                params$b[j] <- resb$par[1]
                params$psi[j] <- resb$par[2]
                params$min2[j] <- -1.00*resb$value
                ifelse(resa$convergence!=0,print("Warning: Optim Failed to Converge!"),NA)
                }    

             flush.console()

        # Calculate Log-Likelihood
        maxllik[iter]<-sum(params$min2)
        diffparam<-mean(abs(params$omega-omegaprev)) # difference between current and previous estimate for omega

        ll.words[,iter]<-params$min2
        diff.ll.words<-(ll.words[,iter]-ll.words[,iter-1])
        diffllik<-sum(diff.ll.words)/abs(maxllik[iter])
			
			#print(sum(diff.ll.words))
			#print(abs(maxllik[iter]))
		 	  if(printsum==TRUE){
		  cat("\tConvergence of LL: ",diffllik,"\n")}
		  
        params$diffllik[iter-1]<-diffllik
        params$diffparam[iter-1]<-diffparam
        params$diffparam.last<-diffparam
        params$maxllik[iter-1]<-maxllik[iter]
        params$iter<-iter-1
        iter<-iter+1
        }    
	     params$diffllik[1]<-NA 
	     return(params)
    	}
	
	 #  Run the algorithm
    est <- rockingpoisson(dta,tol,sigma,fixdoc=fixdoc) 
    }
    
    cat("======================================\n")
	cat("WORDFISH ML Estimation finished.\n")
	cat("======================================\n\n")

    # Write output
    output.documents <-cbind(est$omega,est$alpha)
    rownames(output.documents)<-rownames(dta)
    colnames(output.documents)<-c("omega","alpha")    
    output.words<-cbind(est$b,est$psi)
    rownames(output.words) <-words
    colnames(output.words) <-c("b","psi")

    # Write estimation output file
    # Include: Log-likelihood, iterations, number of words, number of documents

     output.estimation<-cbind(nword,nparty,est$iter,sum(est$min2),est$conv,est$diffparam.last)    
     colnames(output.estimation)<-c("Words","Documents","Iterations","Log-Likelihood","Convergence Criterion","Difference in X")

     if(writeout==TRUE){
        write.table(output.documents,file=paste(output,"documents.csv",sep="_"))
        write.table(output.words, file=paste(output,"words.csv",sep="_"))
        write.table(output.estimation, file=paste(output,"estimation.csv",sep="_"))
      }

     ###########################
     # Parametric Bootstrap Code
     ###########################

     bootstrap<-function(nsim,output.documents,output.words,nparty,nword) {

        cat("STARTING PARAMETRIC BOOTSTRAP\n")

        # input alpha and omega from estimation
        alpha.omega<-output.documents

        # input psis and betas from estimation
        psi.beta<-output.words

        # Create matrix of results.
        output.se.omega<-matrix(0,nparty,nsim)
        output.se.b<-matrix(0,nword,nsim)

        alpha<-alpha.omega[,2] 
        omega<-alpha.omega[,1]
        psi<-psi.beta[,2] 
        b<-psi.beta[,1]

        # create data matrix
        dtasim<-matrix(1,nrow=nparty,ncol=nword)
			cat("======================================\n")
			cat("Now running", nsim,"bootstrap trials.\n")
			cat("======================================\n")
			cat("Simulation ")
			
        for (k in 1:nsim){

            cat(k,"...")

             # Generate new data using lambda 
            for (i in 1:nparty) {
                dtasim[i,]<-rpois(nword,exp(psi+alpha[i]+b*omega[i]))  
                }

            alphastart<-alpha+rnorm(length(alpha.omega[,1]),mean=0,sd=(sd(alpha.omega[,2])/2))
            omegastart<-omega+rnorm(length(alpha.omega[,1]),mean=0,sd=(sd(alpha.omega[,1])/2))
            psistart<-psi+rnorm(length(psi.beta[,1]),mean=0,sd=(sd(psi.beta[,2])/2))
            bstart<-b+rnorm(length(psi.beta[,1]),mean=0,sd=(sd(psi.beta[,1])/2))
            params<-list(alpha=alphastart,omega=omegastart,psi=psistart,b=bstart)      


			if(fixtwo==FALSE){
	         	est <- rockingpoisson(dtasim,tol,sigma,params=params,dir=dir,printsum=FALSE)
				}
				
			else{
				est <- rockingpoisson(dtasim,tol,sigma,params=params,fixdoc=fixdoc,printsum=FALSE) 
				}


            # Store omegas 
            output.se.omega[,k]<-est$omega
            # Store Bs
            output.se.b[,k]<-est$b
            }


        conf.documents<-matrix(0,nparty,4)
        colnames(conf.documents)<-c("LB","UB","Omega: ML","Omega: Sim Mean")
        rownames(conf.documents)<-rownames(dta)
        for (i in 1:nparty) {
            conf.documents[i,1]<-quantile(output.se.omega[i,],0.025)
            conf.documents[i,2]<-quantile(output.se.omega[i,],0.975)
            conf.documents[i,3]<-omega[i]
            conf.documents[i,4]<-mean(output.se.omega[i,])
            }



        #CI for word weights
        conf.words<-matrix(0,nword,4)
        colnames(conf.words)<-c("LB","UB","B: ML","B: Sim Mean")
        rownames(conf.words)<-words
        
      
        for (i in 1:nword) {
            conf.words[i,1]<-quantile(output.se.b[i,],0.025)
            conf.words[i,2]<-quantile(output.se.b[i,],0.975)
            conf.words[i,3]<-b[i]
            conf.words[i,4]<-mean(output.se.b[i,])
            }

           return(list(conf.documents=conf.documents,conf.words=conf.words))
        }

        if(boots==TRUE){ 
             bootresult<-bootstrap(nsim,output.documents,output.words,nparty,nword) 
             ci.documents<-bootresult$conf.documents
             ci.words<-bootresult$conf.words

             if(writeout==TRUE){
                   write.table(ci.words,file=paste(output,"words_95_ci.csv",sep="_"))
                   write.table(ci.documents,file=paste(output,"documents_95_ci.csv",sep="_"))
             }

        }

        if(boots==F){
             ci.documents<-NULL
             ci.words<-NULL
        }

cat("Finished!\n")

return(list(documents=output.documents,words=output.words,diffllik=est$diffllik,diffomega=est$diffparam,maxllik=est$maxllik,estimation=output.estimation,ci.documents=ci.documents,ci.words=ci.words))


}




# ============================================================
# SAMPLE CODE FOR RUNNING WORDFISH WITH AN EXISTING
# WORD COUNT DATASET
# ============================================================

# LOAD DATA
wordcountdata<-read.table("YOURDATA.csv")


# RUN WORDFISH
example.A<-wordfish(wordcountdata,wordsincol=TRUE,dir=c(1,6)) 

# SHOW QUANTITIES OF INTEREST
example.A$documents
example.A$words




# ============================================================
# SAMPLE CODE FOR RUNNING WORDFISH WITH TEXT MINING PACKAGE TM
# Please refer to the documentation of the TM package 
# for more information
# ============================================================


# DEFINE DIRECTORY THAT CONTAIN UTF-8 TEXT DOCUMENTS HERE 
directory<-"YOUR DIRECTORY"

# LOAD TEXT DOC COLLECTION (Here: German)
textcorpus<-Corpus(DirSource(directory),readerControl = list(reader=readPlain,language = "de", load = FALSE))


# EXTRACT FILE NAMES
docnames<-list.files(directory)

	for (i in 1:length(textcorpus)){
		Author(textcorpus[[i]])<-docnames[i]
		}

# PRINT SUMMARY OF DOC COLLECTION
summary(textcorpus)


# GENERATE TERM-DOCUMENT MATRIX
text.corpus.format<-textcorpus
text.corpus.format<-tmMap(text.corpus.format,tmTolower) # MAKES EVERYTHING LOWERCASE
text.corpus.format<-tmMap(text.corpus.format,removeNumbers) # REMOVE NUMBERS
text.corpus.format<-tmMap(text.corpus.format,stripWhitespace) # REMOVE EXTRA WHITE SPACE
wordfreqmatrix <-TermDocMatrix(text.corpus.format)  
dim(wordfreqmatrix)
wordfreq<-as.matrix(wordfreqmatrix) # CONVERT WORD COUNT MATRIX FOR USE WITH WORDFISH
rownames(wordfreq)<-lapply(text.corpus.format,Author) # ASSIGN DOC TITLES TO MATRIX
wordfreq<-t(wordfreq) # TRANSPOSE TERM DOC MATRX


# RUN WORDFISH 
example.B<-wordfish(wordfreq,dir=c(1,6)) 

# Print out some quantities of interest
example.B$documents
example.B$words
