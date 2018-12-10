import re
from collections import defaultdict

players, last = map(int, re.findall(r"\d+", open("9.in").read()))
#players, last = 30, 5807
scores = defaultdict(int)
current_idx = 0
marbles = [0]
marble_count = 0

def restrict_inc(idx, total):
    while idx > total:
        idx -= total;
    while idx < 0:
        idx += total
    return idx

def restrict(idx, total):
    while idx >= total:
        idx -= total;
    while idx < 0:
        idx += total
    return idx

for play in range(last):
    marble_count += 1
    current_player = (play % players) + 1

    if marble_count % 23 == 0:
        scores[current_player] += marble_count

        idx_to_remove = restrict(current_idx - 7, len(marbles))
        scores[current_player] += marbles.pop(restrict(current_idx - 7, len(marbles)))
        current_idx = idx_to_remove
    else:
        current_idx = restrict_inc(current_idx + 2, len(marbles))
        marbles.insert(current_idx, marble_count)

    # uncomment for the first example (9, 25) :)
    # print(
    #     f"[{current_player}]",
    #     " ".join([f"({m})" if idx == current_idx else str(m)
    #               for idx, m in enumerate(marbles)])
    # )

print(max(scores.items(), key=lambda kv: kv[1]))
