/*
 * This Program Outputs the Type of Poker Hand you have with the input
 * of 5 cards.
 * 
 * 
 */

import scala.collection.mutable.HashMap


object Poker extends App{
  def getHistogram(cards: Array[Int]): HashMap[Int, Int] = {
    var hash = new HashMap[Int, Int]
    for (c <- cards) {
      var rank = (c + 1) % 13
      if (hash.keySet.contains(rank)) hash(rank)+=1 else hash(rank) = 0
    }
    hash
  }
  
  def isFlush(cards: Array[Int]): Boolean = {
    cards.map(_ / 13).toSet.size == 1
  }
  
  def isStraight(cards: Array[Int]) = {
    val ranks = cards.map(_ % 13)
    ranks.sorted
    scala.Math.abs(ranks(0) - ranks(4)) == 4
  }
  
  def card_to_int (in: String) = {
    val re = """(\d+|A|K|Q|J)(C|D|H|S)""".r
    val matches = re.findFirstMatchIn(in).get.subgroups
    var suit: Int = 0;
    var rank: Int = -1;
    if (matches.length == 2){
        var r = matches.apply(0) 
    	r match {
    	  case "A" => rank = 0
    	  case "K" => rank = 12
    	  case "Q" => rank = 11
    	  case "J" => rank = 10
    	  case  _  => rank = r.toInt - 1
    	}
        var s = matches.apply(1)
        s match {
          case "C" => suit = 0
          case "D" => suit = 1
          case "H" => suit = 2
          case "S" => suit = 3
        }
      
    }
    13*suit + rank;
    
  }
  
  def printHand(card_names: Array[String]) = {
    val cards: Array[Int] = card_names.map(card_to_int);
    val histo = getHistogram(cards)
    val straight = isStraight(cards)
    val flush = isFlush(cards)
    val numPairs = histo.values.filter(_ == 2).size
    val trips  = histo.values.filter(_ == 3).size > 0
    val quads  = histo.values.filter(_ == 4).size > 0
    if (straight && flush){
      println("Wow!, You have a straight flush!")
    } else if (quads){
      println("You have Four of a Kind")
    } else if (trips && numPairs == 1) {
      println("You have a Full House")
    } else if(flush) {
      println("You have a Flush")
    } else if (straight){
      println("You have a Straight")
    } else if (trips){
      println("You have Three of a Kind");
    } else if (numPairs == 2){
      println("You have Two Pair")
    } else if (numPairs == 1){
      println("You have a Pair")
    } else {
      println("You have a card high")
    }
  }
  
  override def main (args: Array[String]){
    val numHands = 5
    printf("Let's hear %d Hands, Format Example: 10D, KH, AD, 2D, 5H\n", numHands)
    for (i <- List.range(1, numHands)){
	  printf("Please Give %d Cards\n", numHands)
	  var input = readLine;
	  var cards = input.split(", ").map(_.toUpperCase())
	  for(c <- cards.map(card_to_int)) printf("%d\t", c)
	  print("\n")
	  printHand(cards)
    }
  }
}