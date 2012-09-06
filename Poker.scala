/*
 * Author: @nlim
 * This Program outputs the type of
 * Poker Hand you have with the given input
 */

import scala.collection.mutable.HashMap

object Poker extends App{

  def getHistogram(cards: Array[Int]): HashMap[Int, Int] = {
    var hash = new HashMap[Int, Int]
    for (c <- cards) {
      var rank = c % 13
      if (hash.keySet.contains(rank)) hash(rank)+=1 else hash(rank) = 1
    } 
    return hash
  }
  
  def isFlush(cards: Array[Int]): Boolean = {
    return cards.map(x => x/13).toSet.size == 1
  }
  
  def isStraight(cards: Array[Int]): Boolean = {
    val sorted_ranks = cards.map(x => x % 13).sorted
    return scala.math.abs(sorted_ranks(0) - sorted_ranks(4)) == 4
  }
  
  def card_to_int(input: String): Int = {
    val card_code = """(\d+|A|K|Q|J)(C|D|H|S)""".r
    val matches = card_code.findFirstMatchIn(input).get.subgroups
    if (matches.length != 2) throw new IllegalArgumentException
    val r = matches(0)
    val s = matches(1)
    val rank =  r match {
      case "A" => 0
      case "K" => 12
      case "Q" => 11
      case "J" => 10
      case  _  => r.toInt - 1
    }
    val suit = s match {
      case "C" => 0
      case "D" => 1
      case "H" => 2
      case "S" => 3
    }
    return 13*suit + rank;
  }
  
  def printHand(card_names: Array[String]) = {
    try {
      val cards    = card_names.map(card_to_int)
      val histo    = getHistogram(cards)
      val straight = isStraight(cards)
      val flush    = isFlush(cards)
      val numPairs = histo.values.filter(x => x == 2).size
      val trips    = histo.values.filter(x => x == 3).size > 0
      val quads    = histo.values.filter(x => x == 4).size > 0
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
    } catch { 
      case e: IllegalArgumentException => 
        println("You have given bad input.\nPlease follow the format example: 10D, KH, AD, 2D, 5H")
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
