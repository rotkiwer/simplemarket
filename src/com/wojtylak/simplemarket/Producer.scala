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
  
  final class Producer(
    id: String, private[this] var price: Double, private[this] var cash: Double)
  extends Actor with Log {
      
    override def toString() = "Producer(" + id + ", " + price + ", " + cash + ")"
    
    def getId = id

    private[this] var store = 0.

    private[this] var total_earnings = 0.

    // the most recent element is the head of the list
    private[this] var store_history = List[Double]()
    final case class ElasticityElem(amount_sold: Double, price: Double)
    // the most recent element is the head of the list
    private[this] var elasticity_history = List[ElasticityElem]()


    def act() {
      var transaction_nr = 0
      var sold = 0.
      val rand = new java.util.Random(System.currentTimeMillis())
      loopWhile (Controller.producersShouldAct) {
        react {
          case request: BuyFor =>
            val units: Double = request.howMuch / price
            cash += request.howMuch
            total_earnings += request.howMuch
            sold += request.howMuch //units
            sender ! BoughtGood(id, units, price)

            transaction_nr += 1
            // setting price policy between each 10 production periods
            if (transaction_nr == Controller.howManyConsumers) {
              elasticity_history = ElasticityElem(sold, price) :: elasticity_history
              sold = 0.
              transaction_nr = 0

              if (elasticity_history.length > 1) {

                var delta_sold = elasticity_history.head.amount_sold - elasticity_history.tail.head.amount_sold
                if (delta_sold > -0.00000001 && delta_sold < 0.00000001) delta_sold = 0.
                val delta_price = elasticity_history.head.price - elasticity_history.tail.head.price
                val ped = (delta_sold / elasticity_history.head.amount_sold) / (delta_price / elasticity_history.head.price)

                def priceDelta(n: Int) = rand.nextInt(n) * 0.01 * price

                if (ped.isNaN) price += priceDelta(2)
                else if (ped == 0.) price += priceDelta(3)
                else if (ped > 5) price += priceDelta(5)
                else if (ped > 0.1) price += priceDelta(3)
                else if (ped > -0.9 && ped < 0.1) price += priceDelta(3)
                else if (ped > -1.1 && ped < -0.9) price = price
                else if (ped < -5) price -= priceDelta(5)
                else if (ped < -1.1) price -= priceDelta(3)

                if (price <= 0.) price = 1.

              }
            }
            
          case x =>
            warn(getId + " got unkown message")
        }
      }
    }

    def collectInfo(collector: OutputCollector) {
      collector.addProducerInfo(getId, elasticity_history.map(e => (e.amount_sold, e.price)).toList, total_earnings)
    }

    trace(getId + " created")

  }

}