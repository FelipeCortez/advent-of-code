from dataclasses import dataclass
from collections import defaultdict
import re

@dataclass
class Guard:
    minutes_asleep: defaultdict(int)
    total_asleep: int = 0

def most_slept_minute(guard):
    return max(guard.minutes_asleep.items(), key=lambda x: x[1])

guards = defaultdict(lambda: Guard(defaultdict(int)))

for line in map(str.rstrip, sorted(open("04.in").readlines())):
    minutes = int(line[15:17])

    if "begins" in line:
        guard_id = int(re.search(r"#(\d+)", line).group(1))
    elif "falls" in line:
        asleep_begin = minutes
    elif "wakes" in line:
        guards[guard_id].total_asleep += minutes - asleep_begin

        for i in range(asleep_begin, minutes):
            guards[guard_id].minutes_asleep[i] += 1

sleepiest_guard_id, sleepiest_guard = max(
        guards.items(),
        key=lambda x: x[1].total_asleep)

print(sleepiest_guard_id * most_slept_minute(sleepiest_guard)[0])

guard_with_minute_most_slept_on = max(
        guards.items(),
        key=lambda y: most_slept_minute(y[1])[1])

minute_most_slept_on_for_guard = max(
    guard_with_minute_most_slept_on[1].minutes_asleep.items(),
    key=lambda x: x[1])

print(guard_with_minute_most_slept_on[0] * minute_most_slept_on_for_guard[0])
