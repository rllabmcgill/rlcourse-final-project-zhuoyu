# Show the single estimator of the maximum expected value is biased

# assume we have three random variables

# each variable has its normal distribution

# get the 200 means of 100 samples for each variable

# get the maximum

# plot the density
X1 <- rep(NA, 200)
X2 <- rep(NA, 200)
X3 <- rep(NA, 200)
Q1 <- rep(NA, 200)
Q2 <- rep(NA, 200)


for(i in 1:200){
	X1[i] <- mean(rnorm(100, mean=0, sd=5))
	X2[i] <- mean(rnorm(100, mean=0.2, sd=5))
	X3[i] <- mean(rnorm(100, mean=0.4, sd=5))
	Q1[i] <- max(X1[i], X2[i], X3[i])
	a <- which(c(X1[i], X2[i], X3[i])==Q1[i])
	Q2[i] <- mean(rnorm(100, mean=c(0, 0.2, 0.4)[a], sd=5))
}

library(ggplot2)
df1 <- data.frame(Value = c(X1, X2, X3, Q1),Estimator=rep(c("u1","u2","u3","Q1"), each=200))
ggplot(df1, aes(Value, fill = Estimator, colour = Estimator)) +
  geom_density(alpha = 0.1) + geom_vline(xintercept = c(0.4, mean(Q1))) #+
#  geom_text(aes(x=c(0.4), label=c(paste("max =",0.4)), y=-0.05), angle=90, vjust = -1, size=11)+ 
#  geom_text(aes(x=c(mean(Q1)), label=c(paste("E(Q1) =", round(mean(Q1),2))), y=-0.05), angle=90, vjust = -1, size=5)+ 
#  geom_text(aes(x=c(mean(Q2)), label=c(paste("E(Q2) =", round(mean(Q2),2))), y=-0.05), angle=90, vjust = -1, size=5)

df2 <- data.frame(Value = c(X1, X2, X3, Q1, Q2),Estimator=rep(c("u1","u2","u3","Q1","Q2"), each=200))
ggplot(df2, aes(Value, fill = Estimator, colour = Estimator)) +
  geom_density(alpha = 0.1) + geom_vline(xintercept = c(0.4, mean(Q1), mean(Q2))) #+
#  geom_text(aes(x=c(0.4), label=c(paste("max =",0.4)), y=-0.05), angle=90, vjust = -1, size=11)+ 
#  geom_text(aes(x=c(mean(Q1)), label=c(paste("E(Q1) =", round(mean(Q1),2))), y=-0.05), angle=90, vjust = -1, size=5)+ 
#  geom_text(aes(x=c(mean(Q2)), label=c(paste("E(Q2) =", round(mean(Q2),2))), y=-0.05), angle=90, vjust = -1, size=5)


  
# convergence
X1 <- matrix(rep(NA, 200*200), nrow=200)
X2 <- matrix(rep(NA, 200*200), nrow=200)
X3 <- matrix(rep(NA, 200*200), nrow=200)
Q1 <- matrix(rep(NA, 200*200), nrow=200)
Q2 <- matrix(rep(NA, 200*200), nrow=200)
for(j in 1:200){
	for(i in 1:200){
		X1[i,j] <- mean(rnorm(i*5, mean=0, sd=2))
		X2[i,j] <- mean(rnorm(i*5, mean=0.2, sd=2))
		X3[i,j] <- mean(rnorm(i*5, mean=0.3, sd=2))
		Q1[i,j] <- max(X1[i,j], X2[i,j], X3[i,j])
		a <- which(c(X1[i,j], X2[i,j], X3[i,j])==Q1[i,j])
		Q2[i,j] <- mean(rnorm(i*5, mean=c(0, 0.2, 0.3)[a], sd=2))
	}
}
df2 <- data.frame(Value = c(rowMeans(X1), rowMeans(X2), rowMeans(X3), rowMeans(Q1), rowMeans(Q2)),Estimator=rep(c("u1","u2","u3","Q1","Q2"), each=200), SampleSize=rep(5*(1:200),5))
ggplot(df2, aes(x=SampleSize, y=Value, fill = Estimator, colour = Estimator)) + geom_line()# +  geom_line(aes(linetype=Estimator))
 

# Weighted Q learning

# Grid World in https://papers.nips.cc/paper/3964-double-q-learning
# a simplized version of the environment
dostep <- function(currX, currY, targetX=3, targetY=3, action)  { 
	# action <- c("N", "E", "S", "W")[action]
	reward <- 0 # default
	if((currX==1 & action==1)|	# "N" hit the upper wall 
		(currX==3 & action==3)|	# "S" hit the bottom wall
		(currY==1 & action==4)|	# "W" hit the left wall
		(currY==3 & action==2)){# "E" hit the right wall
			# reward=-1
			newX=currX
			newY=currY
	} else {	
		if (action==1){  # "N"
			newX=currX-1
			newY=currY			
		} else if (action==3){  # "S"
			newX=currX+1
			newY=currY			
		}else if (action==2){	# "E"
			newX=currX
			newY=currY+1			
		}else if(action==4){	# "W"
			newX=currX
			newY=currY-1			
		}
	} 

	if ((newX==targetX)&(newY==targetY)){ # land on garget (prize)
		reward=5
	} else {
		reward <- sample(c(-12, 10),1)
	}
	
	return(list(newX=newX, newY=newY, reward=reward))
}


# select action
greedy <- function(Qa, Qb=NULL, epsilon=0.05){	# Qa, Qb (length 4) = Q values of current position, epsilon-greedy 
	if(runif(1)<epsilon){
		A <- sample(1:4, 1) # sample(c("N", "E", "S", "W"), 1)
	} else {
		if(is.null(Qb)){
			Qab <- Qa	#Q-learning
		} else {
			Qab <- (Qa + Qb)/2	# double Q-learning
		}
		temp <- which(Qab %in% max(Qab))
		if(length(temp)>1) {
			temp <- sample(temp,1)
		}
		A <- temp
	}
	
	return(A)
}



# Q learning
# initial
QA <- array(0, dim=c(3,3,4))
N <- matrix(0, nrow=3, ncol=3) # for calculating epsilon
na <- array(0, dim=c(3,3,4)) # for calculating learning rate
gamma <- 0.95

experiments <- 500
steps <- 20000

Q_Start_Max <- matrix(NA, nrow=experiments, ncol=steps)
Q_reward_experiment <- matrix(NA, nrow=experiments, ncol=steps)

for(i in 1:experiments) {

	QA[] <- 0
	N[] <- 0
	na[] <- 0
	currX <- 1
	currY <- 1
	
	for (j in 1:steps){
		if(currX==3 & currY==3){
			currX <- 1
			currY <- 1
		}

		Q_Start_Max[i, j] <- max(QA[1, 1, ])
		N[currX, currY] <- N[currX, currY] + 1
		A <- greedy(Qa=QA[currX, currY, ],epsilon=1/sqrt(N[currX, currY]))
		na[currX, currY, A] <- na[currX, currY, A] + 1
		currStep <- dostep(currX, currY, action=A)
		nextX <- currStep$newX
		nextY <- currStep$newY
		reward <- currStep$reward
		Q_reward_experiment[i, j] <- reward		
		nextA <- greedy(QA[nextX, nextY, ], epsilon=0)
		# update Q(s,a)
		lr <- 1/na[currX, currY, A]
		#lr <- 1/(na[currX, currY, A]^0.8)
		QA[currX, currY, A] <- QA[currX, currY, A] + lr*(reward+gamma*QA[nextX, nextY, nextA]-QA[currX, currY, A])
		currX <- nextX
		currY <- nextY	
	}
}



# Double Q-learning
# initial
QA <- array(0, dim=c(3,3,4))
QB <- array(0, dim=c(3,3,4))
N <- matrix(0, nrow=3, ncol=3)
naA <- array(0, dim=c(3,3,4)) # for calculating learning rate
naB <- array(0, dim=c(3,3,4)) # for calculating learning rate
gamma <- 0.95

experiments <- 500
steps <- 20000

DQ_Start_Max <- matrix(NA, nrow=experiments, ncol=steps)
DQ_reward_experiment <- matrix(NA, nrow=experiments, ncol=steps)

for(i in 1:experiments) {

	QA[] <- 0
	QB[] <- 0
	N[] <- 0
	naA[] <- 0
	naB[] <- 0
	currX <- 1
	currY <- 1
	
	for (j in 1:steps){
		if(currX==3 & currY==3){
			currX <- 1
			currY <- 1
		}
		QAB <- (QA + QB)/2
		DQ_Start_Max[i, j] <- max(QAB[1, 1, ])
		N[currX, currY] <- N[currX, currY] + 1
		A <- greedy(Qa=QAB[currX, currY, ],epsilon=1/sqrt(N[currX, currY]))
		currStep <- dostep(currX, currY, action=A)
		nextX <- currStep$newX
		nextY <- currStep$newY
		reward <- currStep$reward
		DQ_reward_experiment[i, j] <- reward
		#lr <- 1/(N[currX, currY]^0.8)
		if(runif(1)<0.5){
			nextA <- greedy(QA[nextX, nextY, ], epsilon=0)
			naA[currX, currY, A] <- naA[currX, currY, A] + 1
			lr <- 1/naA[currX, currY, A]
			QA[currX, currY, A] <- QA[currX, currY, A] + lr*(reward+gamma*QB[nextX, nextY, nextA]-QA[currX, currY, A])
		} else {
			nextB <- greedy(QB[nextX, nextY, ], epsilon=0)
			naB[currX, currY, A] <- naB[currX, currY, A] + 1
			lr <- 1/naB[currX, currY, A]
			QB[currX, currY, A] <- QB[currX, currY, A] + lr*(reward+gamma*QA[nextX, nextY, nextB]-QB[currX, currY, A])
			
		}
		currX <- nextX
		currY <- nextY	
	}
}




# Weighted Double Q-learning
# initial
QA <- array(0, dim=c(3,3,4))
QB <- array(0, dim=c(3,3,4))
N <- matrix(0, nrow=3, ncol=3)
naA <- array(0, dim=c(3,3,4)) # for calculating learning rate
naB <- array(0, dim=c(3,3,4)) # for calculating learning rate
gamma <- 0.95

experiments <- 500
steps <- 20000

WDQ_Start_Max_0_75 <- matrix(NA, nrow=experiments, ncol=steps)
WDQ_reward_experiment_0_75 <- matrix(NA, nrow=experiments, ncol=steps)
weight <- 0.75
for(i in 1:experiments) {

	QA[] <- 0
	QB[] <- 0
	N[] <- 0
	naA[] <- 0
	naB[] <- 0
	currX <- 1
	currY <- 1
	
	for (j in 1:steps){
		if(currX==3 & currY==3){
			currX <- 1
			currY <- 1
		}
		QAB <- (QA + QB)/2
		WDQ_Start_Max_0_75[i, j] <- max(QAB[1, 1, ])
		N[currX, currY] <- N[currX, currY] + 1
		A <- greedy(Qa=QAB[currX, currY, ] ,epsilon=1/sqrt(N[currX, currY]))
		currStep <- dostep(currX, currY, action=A)
		nextX <- currStep$newX
		nextY <- currStep$newY
		reward <- currStep$reward
		WDQ_reward_experiment_0_75[i, j] <- reward
		#lr <- 1/(N[currX, currY]^0.8)
		if(runif(1)<0.5){
			nextA <- greedy(QA[nextX, nextY, ], epsilon=0)
			naA[currX, currY, A] <- naA[currX, currY, A] + 1
			lr <- 1/naA[currX, currY, A]
			QA[currX, currY, A] <- QA[currX, currY, A] + lr*(reward+gamma*(weight*QA[nextX, nextY, nextA]+(1-weight)*QB[nextX, nextY, nextA])-QA[currX, currY, A])
		} else {
			nextB <- greedy(QB[nextX, nextY, ], epsilon=0)
			naB[currX, currY, A] <- naB[currX, currY, A] + 1
			lr <- 1/naB[currX, currY, A]
			QB[currX, currY, A] <- QB[currX, currY, A] + lr*(reward+gamma*(weight*QB[nextX, nextY, nextB]+(1-weight)*QA[nextX, nextY, nextB])-QB[currX, currY, A])			
		}
		currX <- nextX
		currY <- nextY	
	}
}

#Q_Start_Max_mean <- colMeans(Q_Start_Max)
#Q_reward_experiment_mean <- colMeans(Q_reward_experiment)

#DQ_Start_Max_mean <- colMeans(DQ_Start_Max)
#DQ_reward_experiment_mean <- colMeans(DQ_reward_experiment)

WDQ_Start_Max_mean_0_75 <- colMeans(WDQ_Start_Max_0_75)
WDQ_reward_experiment_mean_0_75 <- colMeans(WDQ_reward_experiment_0_75)







# Q learning
# initial
QA <- array(0, dim=c(3,3,4))
N <- matrix(0, nrow=3, ncol=3)
na <- array(0, dim=c(3,3,4)) # for calculating learning rate
gamma <- 0.95

experiments <- 500
steps <- 20000

Q_Start_Max2 <- matrix(NA, nrow=experiments, ncol=steps)
Q_reward_experiment2 <- matrix(NA, nrow=experiments, ncol=steps)

for(i in 1:experiments) {

	QA[] <- 0
	N[] <- 0
	na[] <- 0
	currX <- 1
	currY <- 1
	
	for (j in 1:steps){
		if(currX==3 & currY==3){
			currX <- 1
			currY <- 1
		}

		Q_Start_Max2[i, j] <- max(QA[1, 1, ])
		N[currX, currY] <- N[currX, currY] + 1
		A <- greedy(Qa=QA[currX, currY, ],epsilon=1/sqrt(N[currX, currY]))
		na[currX, currY, A] <- na[currX, currY, A] + 1
		currStep <- dostep(currX, currY, action=A)
		nextX <- currStep$newX
		nextY <- currStep$newY
		reward <- currStep$reward
		Q_reward_experiment2[i, j] <- reward		
		nextA <- greedy(QA[nextX, nextY, ], epsilon=0)
		# update Q(s,a)
		# lr <- 1/N[currX, currY]
		lr <- 1/(na[currX, currY, A]^0.8)
		QA[currX, currY, A] <- QA[currX, currY, A] + lr*(reward+gamma*QA[nextX, nextY, nextA]-QA[currX, currY, A])
		currX <- nextX
		currY <- nextY	
	}
}



# Double Q-learning
# initial
QA <- array(0, dim=c(3,3,4))
QB <- array(0, dim=c(3,3,4))
N <- matrix(0, nrow=3, ncol=3)
naA <- array(0, dim=c(3,3,4)) # for calculating learning rate
naB <- array(0, dim=c(3,3,4)) # for calculating learning rate
gamma <- 0.95

experiments <- 500
steps <- 20000

DQ_Start_Max2 <- matrix(NA, nrow=experiments, ncol=steps)
DQ_reward_experiment2 <- matrix(NA, nrow=experiments, ncol=steps)

for(i in 1:experiments) {

	QA[] <- 0
	QB[] <- 0
	N[] <- 0
	naA[] <- 0
	naB[] <- 0
	currX <- 1
	currY <- 1
	
	for (j in 1:steps){
		if(currX==3 & currY==3){
			currX <- 1
			currY <- 1
		}
		QAB <- (QA + QB)/2
		DQ_Start_Max2[i, j] <- max(QAB[1, 1, ])
		N[currX, currY] <- N[currX, currY] + 1
		A <- greedy(Qa=QAB[currX, currY, ],epsilon=1/sqrt(N[currX, currY]))
		currStep <- dostep(currX, currY, action=A)
		nextX <- currStep$newX
		nextY <- currStep$newY
		reward <- currStep$reward
		DQ_reward_experiment2[i, j] <- reward
		if(runif(1)<0.5){
			nextA <- greedy(QA[nextX, nextY, ], epsilon=0)
			naA[currX, currY, A] <- naA[currX, currY, A] + 1
			lr <- 1/(naA[currX, currY, A]^0.8)
			QA[currX, currY, A] <- QA[currX, currY, A] + lr*(reward+gamma*QB[nextX, nextY, nextA]-QA[currX, currY, A])
		} else {
			nextB <- greedy(QB[nextX, nextY, ], epsilon=0)
			naB[currX, currY, A] <- naB[currX, currY, A] + 1
			lr <- 1/(naB[currX, currY, A]^0.8)			
			QB[currX, currY, A] <- QB[currX, currY, A] + lr*(reward+gamma*QA[nextX, nextY, nextB]-QB[currX, currY, A])			
		}
		currX <- nextX
		currY <- nextY	
	}
}




# Weighted Double Q-learning
# initial
QA <- array(0, dim=c(3,3,4))
QB <- array(0, dim=c(3,3,4))
N <- matrix(0, nrow=3, ncol=3)
naA <- array(0, dim=c(3,3,4)) # for calculating learning rate
naB <- array(0, dim=c(3,3,4)) # for calculating learning rate
gamma <- 0.95

experiments <- 500
steps <- 20000

WDQ_Start_Max2_0_75 <- matrix(NA, nrow=experiments, ncol=steps)
WDQ_reward_experiment2_0_75 <- matrix(NA, nrow=experiments, ncol=steps)
weight <- 0.75
for(i in 1:experiments) {

	QA[] <- 0
	QB[] <- 0
	N[] <- 0
	naA[] <- 0
	naB[] <- 0
	currX <- 1
	currY <- 1
	
	for (j in 1:steps){
		if(currX==3 & currY==3){
			currX <- 1
			currY <- 1
		}
		QAB <- (QA + QB)/2
		WDQ_Start_Max2_0_75[i, j] <- max(QAB[1, 1, ])
		N[currX, currY] <- N[currX, currY] + 1
		A <- greedy(Qa=QAB[currX, currY, ],epsilon=1/sqrt(N[currX, currY]))
		currStep <- dostep(currX, currY, action=A)
		nextX <- currStep$newX
		nextY <- currStep$newY
		reward <- currStep$reward
		WDQ_reward_experiment2_0_75[i, j] <- reward
		if(runif(1)<0.5){
			nextA <- greedy(QA[nextX, nextY, ], epsilon=0)
			naA[currX, currY, A] <- naA[currX, currY, A] + 1
			lr <- 1/(naA[currX, currY, A]^0.8)			
			QA[currX, currY, A] <- QA[currX, currY, A] + lr*(reward+gamma*(weight*QA[nextX, nextY, nextA]+(1-weight)*QB[nextX, nextY, nextA])-QA[currX, currY, A])
		} else {
			nextB <- greedy(QB[nextX, nextY, ], epsilon=0)
			naB[currX, currY, A] <- naB[currX, currY, A] + 1
			lr <- 1/(naB[currX, currY, A]^0.8)				
			QB[currX, currY, A] <- QB[currX, currY, A] + lr*(reward+gamma*(weight*QB[nextX, nextY, nextB]+(1-weight)*QA[nextX, nextY, nextB])-QB[currX, currY, A])			
		}
		currX <- nextX
		currY <- nextY	
	}
}


#Q_Start_Max_mean2 <- colMeans(Q_Start_Max2)
#Q_reward_experiment_mean2 <- colMeans(Q_reward_experiment2)

#DQ_Start_Max_mean2 <- colMeans(DQ_Start_Max2)
#DQ_reward_experiment_mean2 <- colMeans(DQ_reward_experiment2)

WDQ_Start_Max_mean2_0_75 <- colMeans(WDQ_Start_Max2_0_75)
WDQ_reward_experiment_mean2_0_75 <- colMeans(WDQ_reward_experiment2_0_75)


df1 <- data.frame(Q_Start = c(Q_Start_Max_mean, DQ_Start_Max_mean, WDQ_Start_Max_mean_0_25, WDQ_Start_Max_mean, WDQ_Start_Max_mean_0_75, Q_Start_Max_mean2, DQ_Start_Max_mean2, WDQ_Start_Max_mean2_0_25, WDQ_Start_Max_mean2, WDQ_Start_Max_mean2_0_75), steps=rep(1:steps,10), Method=rep(rep(c("QL", "DQL", "WDQL(0.25)", "WDQL(0.5)", "WDQL(0.75)"),each=steps),2), Learning_Rate = rep(c("Linear","polynomial"),each=steps*5))
library("ggplot2")
ggplot(data = df1, aes(x = steps, y = Q_Start, group = Method , colour = Method)) +     
	# geom_line() +
  geom_smooth() +  facet_grid( ~Learning_Rate ) +
  xlab("Time steps") +
  ylab("Start point Max Value")   +
  geom_hline(yintercept=0.36, linetype=3)
  # ggtitle("The returns of each time step when alpha=0.25")  
  

df2 <- data.frame(Reward = c(Q_reward_experiment_mean, DQ_reward_experiment_mean, WDQ_reward_experiment_mean_0_25, WDQ_reward_experiment_mean, WDQ_reward_experiment_mean_0_75, Q_reward_experiment_mean2, DQ_reward_experiment_mean2, WDQ_reward_experiment_mean2_0_25, WDQ_reward_experiment_mean2, WDQ_reward_experiment_mean2_0_75), steps=rep(1:steps,10), Method=rep(rep(c("QL", "DQL", "WDQL(0.25)", "WDQL(0.5)", "WDQL(0.75)"),each=steps),2), Learning_Rate = rep(c("Linear","polynomial"),each=steps*5))
library("ggplot2")
ggplot(data = df2, aes(x = steps, y = Reward, group = Method , colour = Method)) +       
  geom_smooth() +  facet_grid( ~Learning_Rate ) +
  xlab("Time steps") +
  ylab("Reward per step")   +
  geom_hline(yintercept=0.5, linetype=3)
  # ggtitle("The returns of each time step when alpha=0.25")    
  