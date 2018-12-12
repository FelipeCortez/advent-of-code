import re
from collections import defaultdict

class Marble:
    def __init__(self, value, cw=None, ccw=None):
        self.value = value
        self.cw =  self if cw is None else cw
        self.ccw = self if ccw is None else ccw

    def insert_cw(self, value):
        new_marble = Marble(value, ccw=self, cw=self.cw)
        new_marble.cw.ccw = new_marble
        self.cw = new_marble
        return new_marble

    def remove_self(self):
        self.ccw.cw = self.cw
        self.cw.ccw = self.ccw
        return self

for line in open("09.in"):
    players, last = map(int, re.findall(r"\d+", line))
    print(players, last)
    scores = defaultdict(int)
    first = Marble(0)
    current_player = 0
    marble_count = 0
    current_marble = first

    for play in range(last):
        marble_count += 1
        current_player = (play % players) + 1

        if marble_count % 23 == 0:
            scores[current_player] += marble_count

            for _ in range(7):
                current_marble = current_marble.ccw

            removed = current_marble.remove_self()

            scores[current_player] += removed.value
            current_marble = current_marble.cw
        else:
            current_marble = current_marble.cw.insert_cw(marble_count)

    print(max(scores.items(), key=lambda kv: kv[1]))
