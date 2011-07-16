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

  object Controller extends Log {

    final case class ProducerInfo(cash: Double)
    final case class ConsumerInfo(max_turn: Int, var actual_turn: Int, budget: Double)

    private[this] var producers = Map[String, (Producer, ProducerInfo)]()
    private[this] var consumers = Map[String, (Consumer, ConsumerInfo)]()

    private[this] var out: OutputWriter = null

    final val EMPTY_BUDGET = 0.
    final val DELAY_FOR_CONSUMER = 40

    implicit def producerBean2Info(bean: ProducerInputBean): ProducerInfo = {
      new ProducerInfo(cash = bean.cash)
    }

    implicit def consumerBean2Info(bean: ConsumerInputBean): ConsumerInfo = {
      new ConsumerInfo(max_turn = bean.max_turn, actual_turn = 1, budget = bean.budget)
    }

    private[this] var _all_agents_ready = false
    def allAgentsReady = _all_agents_ready

    private[this] var _active_consumers = Set[String]()
    def producersShouldAct = !_all_agents_ready || _active_consumers.size > 0

    def howManyConsumers = consumers.size

    def start(in: InputReader, out: OutputWriter) = {
      trace("Controller started")

      this.out = out

      in.producers foreach { bean =>
        val producer = new Producer(id = bean.id, price = bean.price, cash = bean.cash)
        producers += (bean.id -> (producer, bean))
        producer.start()
      }

      val producers_set = producers.map(p => p._2._1).toSet
      val consumers_list = in.consumers
      consumers_list foreach { bean =>
        val consumer = new Consumer(id = bean.id, utility_function = bean.utility_function, producers = producers_set)
        consumers += (bean.id -> (consumer, bean))
        _active_consumers += bean.id
      }
      consumers_list foreach { bean =>
        val consumer = consumers(bean.id)._1
        bean.relations foreach { rel => consumer.addFriend(consumers(rel._1)._1, rel._2) }
        consumer.start()
      }  

      Thread.sleep(consumers.size * DELAY_FOR_CONSUMER)
      info("Simulation started...")
      _all_agents_ready = true

    }

    private[this] def sendNewTurnInfo() {
      consumers foreach { c =>
        c._2._1 ! NewTurn
      }
    }

    private[this] var turn_counter = 0

    def giveNewBudget(c_id: String): Double = {

      val c_info = consumers(c_id)._2
      var ret = -1.
      if (c_info.actual_turn <= c_info.max_turn) {
        val b = consumers(c_id)._2.budget
        c_info.actual_turn += 1
        trace("Given " + b + " to consumer " + c_id)
        ret = b //+ (consumer_info.actual_turn * 10) // inflation
      }
      else {
        info("Consumer " + c_id + " has ended job after " + c_info.max_turn + " budget periods")
        synchronized {
          _active_consumers -= c_id
          if (_active_consumers.size <= 0) {
            end()
          }
          ret = EMPTY_BUDGET
        }
      }

      synchronized {
        turn_counter += 1
        if (turn_counter >= consumers.size) {
          turn_counter = 0
        }
      }
      ret
    }

    private[this] def end() {
      info("Simulation ended")
      info("Collecting information...")
      
      val col = new OutputCollector
      consumers foreach ( c => c._2._1.collectInfo(col) )
      producers foreach ( p => p._2._1.collectInfo(col) )
      out.write(col)

      System.exit(0)
    }

  }

}