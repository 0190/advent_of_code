# input = File.read('./day1_1.txt')
# floor = 0
input.each_char do |c|
  floor += 1 if c == '('
  floor -= 1 if c == ')'
end

puts floor
