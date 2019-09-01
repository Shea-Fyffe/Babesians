####Example 1:Calculate the probability of drawing a full house after four hands (i.e., four players) have been dealt 7 cards.*

##Calculating Total Number of Full House Conditions Possible with 7 cards, and Total Number of 7-Card Hands

#Condition 1: 2 Triple, 1 Random

TwoTriple<-choose(13,2)*choose(4,3)*choose(4,3)

C1<-TwoTriple*44

#Condition 2: 1 Triple, 2 Pair

OneTriple<-choose(13,1)*choose(4,3)
TwoPair<-choose(12,2)*choose(4,2)*choose(4,2)

C2<-OneTriple*TwoPair

#Condition 3: 1 Triple, 1 Pair, 1 Random

OnePair<-choose(12,1)*choose(4,2)
TwoRandom<-choose(11,2)*choose(4,1)*choose(4,1)

C3<-OneTriple*OnePair*TwoRandom

#Total Fullhouses Possible

FullHouses<-C1+C2+C3

#Calculating Total Possible Hands for 7 cards

Hands<-choose(52,7)

###Answer

P<-FullHouses/Hands

###Simplify Above Code?

FullHouseSeven<- /choose(52,7)

####Example 2: Probability that two people will have the same birthday in a classroom of 10, 40, 100, 250, and 500.

###Subtract probability that no birthdays match with i students from 1

##i=10
i10<-1-(364/365)^choose(10,2)
i10

##i=40
i40<-1-(364/365)^choose(40,2)
i40

##i=100

i100<-1-(364/365)^choose(100,2)
i100

##i=250
i250<-1-(364/365)^choose(250,2)
i250

##i=500
i500<-1-(364/365)^choose(500,2)
i500

###Simplifying the Above Code

BirthdayMatch<-function(i) {
  Match<-1-(364/365)^choose(i,2)
  return(Match)
}
BirthdayMatch(40)

#### Example 4: 
