/**
  MIT LICENSE

  Copyright (C) 2011 by Wiktor Wojtylak

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

package com.wojtylak.simplemarket {

  import scala.actors.Actor

  final class Consumer(
    id: String, utility_function: UtilityFunction, producers: Set[Producer])
  extends Actor with Log {

    private[this] val rand = new java.util.Random(System.currentTimeMillis())

    final case class Relation(friend: Consumer, var strength: Double) {
      setStrength(strength)
      def setStrength(s: Double) {
        // s >= 0.1
        require(s >= 0.0 && s <= 1.0)
        strength = s
      }
    }

    override def toString() = "Consumer " + id + ", " + utility_function + ")"

    def getId = id

    private[this] var active: Boolean = true
    private[this] def deactivate() = active = false

    private[this] var friends_receive: Set[Relation] = Set()
    private[this] var friends_observers: Set[Consumer] = Set()

    type producerId = String
    type howMuch = Double

    // Initial spending is random
    private[this] var spendings: Map[producerId, howMuch] = {
      val m = scala.collection.mutable.Map[producerId, howMuch]()
      producers.foreach{producer => m += (producer.getId -> (rand.nextInt(10) + 1.))}
      normalizeTo1(Map() ++ m)
    }

    private[this] var spendings_next: Map[producerId, howMuch] = spendings

    private[this] var basket_builder = new BasketBuilder(utility_function)
    // the most recent element is the head of the list
    private[this] var basket_history = List[Basket]()

    private def normalizeTo1(m: Map[producerId, howMuch]): Map[producerId, howMuch] = {
      val sum = m.foldLeft(0.)(_ + _._2)
      m.map(s => (s._1, s._2 / sum))
    }

    /*
     * Returns true if added, false if friend already stored or trying to add self
     */
    def addFriend(friend: Consumer, rel_strength: Double): Boolean = {
      if (friend == this || friends_receive.count(_.friend.hashCode == friend.hashCode) > 0) return false
      if (!friend.addFriendToObservers(this)) return false
      else {
        friends_receive += new Relation(friend, rel_strength)
        true
      }
    }

    /*
     * Invoked by other Consumer who wants to register and get spending info
     */
    private def addFriendToObservers(friend: Consumer): Boolean = {
      if (friend == this || friends_observers.contains(friend)) false
      else {
        friends_observers += friend
        true
      }
    }

    def act() {

      var waiting_for_producers: Int = 0

      def informFriends() {
        val hyped_spendings = normalizeTo1(spendings map {s => (s._1, s._2 + 0.1 * rand.nextDouble())})
        friends_observers foreach {
          _ ! MySpendingsAndUtility(this.getId, hyped_spendings, basket_history.head.utility)
        }
        trace(getId + " has informed friends")
      }

      def startNewPeriod() {
        spendings = spendings_next
        val budget = Controller.giveNewBudget(getId)
        if (budget == Controller.EMPTY_BUDGET) {
          deactivate()
          return
        }
        spendings foreach { s =>
          (for (producer <- producers.find(_.getId == s._1)) yield {
            waiting_for_producers += 1
            producer ! BuyFor(s._2 * budget)
          }) getOrElse { throw new Exception("Trying to send buy request to Producer which is unknown") }
        }
        trace(getId + " has sent buy requests")
      }

      var init_req_sent = false
      // consumer is active through specified number of "budget spending" periods
      loopWhile (active) {
        if (Controller.allAgentsReady) {
          if (!init_req_sent)  {
            startNewPeriod()
            init_req_sent = true
          }
          react {
            case good: BoughtGood =>
              if (!basket_builder.addGood(good)) throw new Exception("Trying to add a good more than one time")
              waiting_for_producers -= 1
              // if all producers answered (in a time period)
              if (waiting_for_producers == 0) {
                val bas = basket_builder.buildBasket
                basket_history = bas :: basket_history
                basket_builder = new BasketBuilder(utility_function)
                informFriends()
                startNewPeriod()
              }
            case sau: MySpendingsAndUtility =>
              friends_receive.find(_.friend.getId == sau.who_id) match {
                case Some(rel: Relation) =>
                  if ((basket_history.size > 0) && (sau.utility - basket_history.head.utility) / sau.utility > 0.1) {
                    spendings_next = spendings_next map { s =>
                      (s._1 -> ((sau.spendings(s._1) * rel.strength) + (s._2 * (1 - rel.strength))))
                    }
                  }             
                case None => throw new Exception("Got message from unknown Consumer")
              }

            case x =>
              warn(getId + " got unkown message")

          }
        }
      }
    }

    def collectInfo(collector: OutputCollector) {
      collector.addConsumerInfo(getId, basket_history, utility_function)
    }

    trace(getId + " created")
   
  }

}