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

  // values = list of prices
  final case class ProducerOutputBean(id: String, values: List[(Double, Double)], earnings: Double)
  // values = list of: cost (budget), utility, utility
  final case class ConsumerOutputBean(id: String, utils: List[(Double, Double, Double)], boughts: scala.collection.mutable.Map[String, List[Double]])

  final class OutputCollector {

    private[this] var _producers = List[ProducerOutputBean]()
    private[this] var _consumers = List[ConsumerOutputBean]()

    def addConsumerInfo(id: String, basket_history: List[Basket], utility_function: UtilityFunction) {
      var utils = List[(Double, Double, Double)]()
      val spendings = scala.collection.mutable.Map[String, List[Double]]()
      basket_history foreach { basket =>
        utils = (basket.cost, basket.utility, utility_function.maximizeUtility(basket).utility) :: utils
        basket foreach { good =>
          spendings(good.id) = if (spendings.contains(good.id)) good.amount :: spendings(good.id) else List(good.amount)
        }
      }
      _consumers = ConsumerOutputBean(id, utils, spendings) :: _consumers
    }

    def addProducerInfo(id: String, values: List[(Double, Double)], earnings: Double) {
      _producers = ProducerOutputBean(id, values.reverse, earnings) :: _producers
    }

    def consumers = _consumers
    def producers = _producers

  }

  abstract class OutputWriter {
    def write(collector: OutputCollector)
  }

  final class CSVOutputWriter(file_name: String) extends OutputWriter with Log {

    override def write(collector: OutputCollector) {
      info("Generating output file \"" + file_name + "\"...")
      
      val new_line = System.getProperty("line.separator")
      val new_column = ";"
      val output_file = new java.io.FileWriter(file_name)

      def writeProducer(p: ProducerOutputBean) {
        output_file.write("price_" + p.id + ":" + new_column)
        p.values foreach { v => output_file.write(v._2 + new_column) }
        output_file.write(new_line)
        output_file.write("sold_" + p.id + ":" + new_column)
        p.values foreach { v => output_file.write(v._1 + new_column) }
        output_file.write(new_line)
        output_file.write("cash_" + p.id + ":" + new_column)
        output_file.write(p.earnings + new_line)
      }

      def writeConsumer(c: ConsumerOutputBean) {
        output_file.write("utility_" + c.id + ":" + new_column);
        c.utils foreach { v => output_file.write(v._2 + new_column) }
        output_file.write(new_line)
        output_file.write("max_utility_" + c.id + ":" + new_column)
        c.utils foreach ( v => output_file.write(v._3 + new_column) )
        output_file.write(new_line)
        output_file.write("rel_util_" + c.id + ":" + new_column)
        c.utils foreach { v => output_file.write(v._2 / v._3 + new_column) }
        output_file.write(new_line)
      }

      output_file.write("Producers:" + new_line)
      collector.producers foreach(writeProducer(_))
      output_file.write(new_line + "Consumers:" + new_line)
      collector.consumers foreach(writeConsumer(_))

      output_file.close()
      info("Output generated")

    }

  }

  final class MapReduceOutputWriter(file_name: String) extends OutputWriter with Log {

    override def write(collector: OutputCollector) {
      info("Generating output file \"" + file_name + "\"...")

      val new_line = System.getProperty("line.separator")
      val separator = ";"
      val output_file = new java.io.FileWriter(file_name)

      collector.consumers foreach { consumer_bean =>
        consumer_bean.boughts foreach { bought =>
          output_file.write(consumer_bean.id + separator + bought._1 + separator)
          bought._2 foreach { amount =>
            output_file.write(amount + separator)
          }
          output_file.write(new_line)
        }
      }    

      output_file.close()
      info("Output generated")

    }

  }

}

