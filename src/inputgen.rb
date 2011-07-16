#!/usr/bin/ruby

#  MIT LICENSE
#  
#  Copyright (C) 2011 by Wiktor Wojtylak
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in
#  all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#  THE SOFTWARE.
#

puts "simplemarket random input generator\n\n"

file_name = ARGV[0]
if file_name == nil then file_name = "sm_input.csv" end # default file name
$file = File.open(file_name, "w")

def output_print(s)
  $file.print s
end

puts " Producers:"
print "  Amount: "
prod_amount = Integer(STDIN.gets.chomp)
puts " Consumers:"
print "  Amount: "
cons_amount = Integer(STDIN.gets.chomp)
print "  How many turns: "
cons_turns = Integer(STDIN.gets.chomp)

srand

output_print "Producers:\n"
(1 .. prod_amount).each do |i|
  output_print "p#{i}\n" # id
  output_print "15;0" # initial price;starting cash
  output_print "\n"
end


output_print "Consumers:\n"
(1 .. cons_amount).each do |i|
  output_print "c#{i}\n" # id
  output_print "#{cons_turns};100\n" # turns;budget
  # CES function
  s = (rand(10) / 10.0) + 2.0
  (1.. prod_amount).each do |pi|
    output_print "p#{pi};#{rand(10) + 1.0};#{s};"
    #output_print "p1;#{rand(2) + 1};100;p2;#{rand(3) + 6};1.2;" # water and diamonds
  end
  output_print "#{s}\n"
  #output_print "1.5\n" # water and diamonds
end


output_print "Relations:\n"
(1 .. cons_amount).each do |i|
  (1 .. cons_amount).each do |j| 
    if (rand(10) == 0) then
      if (j != i) then output_print "c#{i};c#{j};0.1\n" end
    end
  end
end


puts "\nInput generated to file #{file_name}"
