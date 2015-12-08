def run
  input = File.read('./day1_1.txt')
  floor = 0
  input.split('').each_with_index do |c, i|
    floor += 1 if c == '('
    floor -= 1 if c == ')'
    if floor == -1
      return i + 1
    end
  end
end

puts run