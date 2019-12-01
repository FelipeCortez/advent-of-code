from itertools import cycle

found = set()
sum = 0
for number in cycle([int(x) for x in open("01.in")]):
    sum = sum + number
    if sum not in found:
        found.add(sum)
    else:
        print(sum)
        break
