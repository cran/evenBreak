
#' compareProbs
#' @importFrom combinat permn
#' @import graphics
#' @import stats
#' @import utils
#' @export
compareProbs<-
	function(verbose=FALSE) {
		# organizes invoking evenBreakDriver() for 4 deck sizes, and presenting the results in a table and in a graph
		# scatter plot of 2 sets of distribution probabilities

		df.13<-evenBreakDriver(13)
		df.10<-evenBreakDriver(10)
		df.7<-evenBreakDriver(7)
		df.4<-evenBreakDriver(4)
		
		# sum of probabilities should equal 1.00
		if(verbose) {
		  print("sum of probabilities should equal 1.00")
		  print(c("sum of probabilities for df.13: ",sum(df.13[,"probability"])),quote=FALSE)
		  print(c("sum of probabilities for df.10: ",sum(df.10[,"probability"])),quote=FALSE)
		  print(c("sum of probabilities for df.7: ",sum(df.7[,"probability"])),quote=FALSE)
		  print(c("sum of probabilities for df.4: ",sum(df.4[,"probability"])),quote=FALSE)
		}
		
		# use the full deck as the "standard"
		# use standard deviation across 4 hands as the measure of "unbalanced"
		for(r in 1:nrow(df.13))
			df.13[r,"sd"]<-round(sd(df.13[r,c("n1","n2","n3","n4")]),2)
			
		
		rownames(df.13)<-sprintf("%s.%s.%s.%s",df.13[,"n1"],df.13[,"n2"],df.13[,"n3"],df.13[,"n4"])
		rownames(df.10)<-sprintf("%s.%s.%s.%s",df.10[,"n1"],df.10[,"n2"],df.10[,"n3"],df.10[,"n4"])
		rownames(df.7)<-sprintf("%s.%s.%s.%s",df.7[,"n1"],df.7[,"n2"],df.7[,"n3"],df.7[,"n4"])
		rownames(df.4)<-sprintf("%s.%s.%s.%s",df.4[,"n1"],df.4[,"n2"],df.4[,"n3"],df.4[,"n4"])

		for(rn in rownames(df.10))
			df.13[rn,"prob10"]<-df.10[rn,"probability"]
			
		for(rn in rownames(df.7))
			df.13[rn,"prob7"]<-df.7[rn,"probability"]
			
		for(rn in rownames(df.4))
			df.13[rn,"prob4"]<-df.4[rn,"probability"]
			
		if(verbose)
		  print(df.13[order(df.13$sd),])
		numeric_columns<-sapply(df.13,mode)=='numeric'
		df.13[numeric_columns]<-round(df.13[numeric_columns],3)
		if(verbose)
		  print(as.matrix(df.13[,c("n1","n2","n3","n4","sd","probability","prob10","prob7","prob4")]),na.print="-")
		#write.table(as.matrix(df.13[,c("n1","n2","n3","n4","sd","probability","prob10","prob7","prob4")]),"/Users/barryzeeberg/Desktop/probs.xls",sep="\t",na="-")
		
		mx<-max(c(df.13[,"probability"],df.13[,"prob10"],df.13[,"prob7"],df.13[,"prob4"]),na.rm=TRUE)
		
		oldpar <- par(no.readonly = TRUE) # code line i
		on.exit(par(oldpar)) # code line i + 1		
		par(mar=c(5.1,5.0,4.1,2.1))
		# ylim=c(-.05,mx) is better than ylim=c(0,mx) to allow labelling of 5 4 2 2
		plot(df.13[,"sd"],df.13[,"probability"],main="Probability for Suit Distribution Across Four Player's Hands",xlab="Degree of Unbalanced Suit Distribution Across Four Player's Hands",cex.lab=1.5,cex.axis=1.5,ylab="Probability",ylim=c(-.05,mx),bg="black",pch=21,cex=2)
		points(df.13[,"sd"],df.13[,"prob10"],col="red",bg="red",pch=21,cex=2)
		points(df.13[,"sd"],df.13[,"prob7"],col="green",bg="green",pch=21,cex=2)
		points(df.13[,"sd"],df.13[,"prob4"],col="blue",bg="blue",pch=21,cex=2)
		text(.5,.05,"4 3 3 3",srt=90)
		text(.957,.15,"4 4 3 2",srt=90)
		text(1.26,.22,"5 3 3 2",srt=90)
		text(1.50,.15,"4 4 4 1",srt=90)
		text(1.50,-.02,"5 4 2 2",srt=90)
		legend(4,.4,legend=c("52 Card Deck","40 Card Deck","28 Card Deck","16 Card Deck"),col=c("black","red","green","blue"),pt.bg=c("black","red","green","blue"),pch=21)		
	}
#' evenBreakDrive
#' @export	
evenBreakDriver<-
	function(D4) {
		# loop through all possible distributions of a single suit across 4 hands
		
		df<-data.frame()
		
		n<-0
		#for(n1 in 0:13)
		for(n1 in 0:D4) # D4, not 13, since the total number of cards held by a player is D4 which is less than 13 for partial size decks !
			for(n2 in 0:(min(n1,(13 - n1))))
				for(n3 in 0:(min(n2,(13 - n1 - n2)))) {
					n4<-13 - n1 - n2 - n3
					if(n4<=n3) {
						n<-n+1
						df[n,"n1"]<-n1
						df[n,"n2"]<-n2
						df[n,"n3"]<-n3
						df[n,"n4"]<-13-n1-n2-n3
						df[n,"probability"]<-evenBreak(D4,n1,n2,n3,1)*length(unique(combinat::permn(c(n1,n2,n3,n4))))
						}
					}
					#print(df[order(-df$probability),])
					return(df)
	}
#' evenBreak
#' @export
evenBreak<-
	function(D4=13,n1,n2,n3,PERM) {
		# compute the probability of a given distribution of a single suit
		# across 4 hands
		# D4 is one quarter of the size of the deck, normally = 13
		# n1, n2, n3 are the number of cards in the suit in the hands of players 1, 2, and 3
		
		D<-4*D4
		
		F1<-choose(13,n1) * choose(D - 13,D4 - n1)
		F2<-choose(13 - n1,n2) * choose(D - D4 -13 + n1,D4 - n2)
		F3<-choose(13 - n1 - n2,n3) * choose(D - 2*D4 -13 +n1 + n2,D4 - n3)
		
		N<-F1*F2*F3
		
		D<-choose(D,D4) * choose(D - D4,D4) * choose(D - 2*D4,D4)
		
		return(PERM * N/D)
	}