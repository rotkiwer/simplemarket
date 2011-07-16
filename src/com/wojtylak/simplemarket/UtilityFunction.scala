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

  // params s in parts may be different than param s in function

  final case class UFPart(id: String, param_a: Double, param_s: Double) {
    require(param_s != 0. && param_s != 1.)

    def getUtil(amount: Double): Double = param_a * scala.math.pow(amount, (param_s - 1.) / param_s)

  }

  final class UtilityFunction(s: Double, parts: Set[UFPart]) {
    require(s != 1. && s != 0.)

    override def toString() = "UtilityFunction(" + s + ", " + parts + ")"

    private def adjustHoleUtilValue(u: Double): Double = scala.math.pow(u, s / (s - 1.))

    def calculateUtility(b: BasketBuilder): Double = {
      if (parts.size != b.howManyGoods) throw new Exception("Incompatible basket (incompatible sizes)")
      var u = 0.
      b foreach { good =>
        parts.find(_.id == good.id) match {
          case Some(p: UFPart) =>
            u += p.getUtil(good.amount)
          case None => throw new Exception("Incompatible basket")
        }
      }
      require(u >= 0.)
      adjustHoleUtilValue(u)
    }

    /*
     * Returns new(!) Basket that brings max utility,
     * prices and budget (cost) are given in param-Basket
     */
    def maximizeUtility(basket_env: Basket): Basket = {
      if (parts.size != basket_env.howManyGoods) throw new Exception("Incompatible basket (incompatible sizes)")
      val unit = 0.01 // of good (not money!)

      final case class CapsuleMaxPart(
        value: Double, value_next: Double, amount: Double, price: Double, part: UFPart)
      extends Ordered[CapsuleMaxPart] {

        override def compare(that: CapsuleMaxPart) = {
          val delta_this = (this.value_next - this.value) / this.price
          val delta_that = (that.value_next - that.value) / that.price
          if (delta_this - delta_that < 0.) -1
          else if (delta_this - delta_that == 0.) 0
          else 1
        }
      }

      def sortedListOfGoods: List[CapsuleMaxPart] = {
        // create initial list
        var list = List[CapsuleMaxPart]()
        basket_env foreach { good =>
          parts.find(_.id == good.id) match {
            case Some(p: UFPart) =>
              list = CapsuleMaxPart(value = 0., value_next = p.getUtil(unit),
                                    amount = 0., price = good.price, part = p) :: list
            case None => throw new Exception("Incompatible basket (element not found)")
          }
        }
        // sort initial list (decreasing)
        list = list.sorted.reverse
        list
      }

      var list_of_goods = sortedListOfGoods
      var list_of_performed_goods = List[CapsuleMaxPart]()

      // keeps list sorted
      // moves last-best to proper position (like insertion sort, but only for 1st elem)
      def insert(element: CapsuleMaxPart, list: List[CapsuleMaxPart]): List[CapsuleMaxPart] = {
        if (list.isEmpty || element >= list.head) element :: list
        else list.head :: insert(element, list.tail)
      }

      var budget = basket_env.cost
      var budget_after_buying = 0.

      while (!list_of_goods.isEmpty) {
        var best_good = list_of_goods.head
        list_of_goods = list_of_goods.tail

        budget_after_buying = budget - unit * best_good.price
        if (budget_after_buying >= 0.) {
          list_of_goods = insert(CapsuleMaxPart(value = best_good.value_next, value_next = best_good.part.getUtil(best_good.amount + 2 * unit),
                                                amount = best_good.amount + unit, price = best_good.price, part = best_good.part), list_of_goods)
          budget = budget_after_buying
        }
        else {
          list_of_performed_goods = best_good :: list_of_performed_goods
        }
      }

      // create maxi-Basket
      // may be faster when utility will be calculated here and not in calculateUtility method
      val basket_ret = new BasketBuilder(this)
      list_of_performed_goods foreach(good => basket_ret.addGood(BoughtGood(id = good.part.id, amount = good.amount, price = good.price)))
      basket_ret.buildBasket
    }

  }

}