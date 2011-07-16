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

  final case class ProducerInputBean(
    var id: String = "", var price: Double = 0., var cash: Double = 0.)
  final case class ConsumerInputBean(
    var id: String = "", var max_turn: Int = 0, var budget: Double = 0.,
    var utility_function: UtilityFunction = null, var relations: List[(String, Double)] = List())

  abstract class InputReader {
    def producers: List[ProducerInputBean]
    def consumers: List[ConsumerInputBean]
  }

  /*
   * Does not validate input file.
   * It is not prepered for usage by many threads simultaneously.
   */
  final class CSVInputReader(file_name: String) extends InputReader with Log {

    private[this] var _producers = List[ProducerInputBean]()
    private[this] var _consumers = List[ConsumerInputBean]()

    private[this] var parsed = false

    private[this] def parse {
      import scala.io._

      object State extends Enumeration {
        type State = Value
        val Prod, Cons, Rels, Unkw = Value
      }
      import State._

      var state = Unkw
      var line_nr = 0
      val lines_in_state = Map(Prod -> 2, Cons -> 3, Rels -> 1)

      var pb = new ProducerInputBean
      var cb = new ConsumerInputBean

      def changeState(s: State) = {
        state = s
        line_nr = 0
      }

      Source fromFile(file_name) getLines() foreach {
        case "Producers:" => changeState(Prod)
        case "Consumers:" => changeState(Cons)
        case "Relations:" => changeState(Rels)
        case l =>
          line_nr = line_nr % lines_in_state(state)
          val data = l.split(";")

          state match {
            
            case Prod =>
              if (line_nr == 0) pb = new ProducerInputBean(id = l)
              else if (line_nr == 1) {
                pb.price = data(0).toDouble
                pb.cash = data(1).toDouble
                _producers = pb :: _producers
              }

            case Cons =>
              if (line_nr == 0) cb = new ConsumerInputBean(id = l)
              else if (line_nr == 1) {
                cb.max_turn = data(0).toInt
                cb.budget = data(1).toDouble
              }
              else if (line_nr == 2) {
                var ufparts = Set[UFPart]()
                for (i <- 0 until (data.length / 3)) {
                  ufparts += UFPart(id = data(i * 3),
                                    param_a = data((i * 3) + 1).toDouble,
                                    param_s = data((i * 3) + 2).toDouble)
                }
                cb.utility_function = new UtilityFunction(data(data.length - 1).toDouble, ufparts)
                _consumers = cb :: _consumers
              }

            case Rels =>
              _consumers.find(_.id == data(0)) match {
                case Some(cbf: ConsumerInputBean) =>
                  cbf.relations = (data(1), data(2).toDouble) :: cbf.relations
                case None =>
                  throw new Exception("Trying to add relation to consumer who does not exist")
              }

          }

          line_nr += 1
      }

      parsed = true
      info("Input file \"" + file_name + "\" parsed")
    }
    
    override def producers: List[ProducerInputBean] = {
      if (!parsed) parse
      _producers.toList
    }

    override def consumers: List[ConsumerInputBean] = {
      if (!parsed) parse
      _consumers.toList
    }

  }
}