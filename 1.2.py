from itertools import cycle

if __name__ == "__main__":
    with open('1.in') as f:
        found = set()
        sum = 0
        for number in cycle([int(x) for x in f]):
            sum = sum + number
            if sum not in found:
                found.add(sum)
            else:
                print(sum)
                break
