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

  // id of BoughtGood should be the same as id of Producer	
  final case class BoughtGood(id: String, amount: Double, price: Double)

  final class BasketBuilder(utility_function: UtilityFunction)
    extends Traversable[BoughtGood] {

    private[this] var goods: Set[BoughtGood] = Set()

    override def toString = "BasketBuilder goods:" + goods

    /*
     * Returns true if added, false if good is already stored
     */
    def addGood(g: BoughtGood): Boolean = {
      if (goods.count(_.id == g.id) > 0) false
      else {
        goods += g
        true;
      }
    }

    def buildBasket: Basket = new Basket(goods, utility_function.calculateUtility(this))

    def foreach[U](f: BoughtGood => U) = goods foreach f

    def howManyGoods: Int = goods.size

  }

  /*
   * Representation of basket in consumer's history.
   */
  final class Basket(
    private[this] val goods: Set[BoughtGood], val utility: Double)
    extends Traversable[BoughtGood] {

    /*
     * How much the basket cost?
     */
    val cost: Double = goods.foldLeft(0.)((sum, g) => sum + (g.amount * g.price))
	  
    override def toString() = "Basket(" + goods + ", " + utility + ")"

    def foreach[U](f: BoughtGood => U) = goods foreach f
    
    def howManyGoods: Int = goods.size

    def getGoodById(id: String): Option[BoughtGood] = goods.find(_.id == id)
    
  }

}
