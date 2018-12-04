from datetime import datetime
from dataclasses import dataclass
from collections import defaultdict
import re

@dataclass
class Guard:
    minutes_asleep: defaultdict(int)
    total_asleep: int = 0

def iso_to_datetime(date: str):
    return datetime.strptime(date, "%Y-%m-%d %H:%M")

def diff_minutes(date1: str, date2: str):
    delta = iso_to_datetime(date1) - iso_to_datetime(date2)
    return round(delta.seconds / 60)

def most_slept_minute(guard):
    return max(guard.minutes_asleep.items(), key=lambda x: x[1])

if __name__ == "__main__":
    with open("4.in") as f:
        guards = defaultdict(lambda: Guard(defaultdict(int)))
        lines = map(str.rstrip, sorted(f.readlines()))

        for line in lines:
            timestamp = line[1:17]

            if re.search(r"begins", line):
                guard_id = int(line[line.find("#"):].split()[0][1:])
            elif re.search(r"falls", line):
                asleep_time = timestamp
            elif re.search(r"wakes", line):
                minutes_slept = diff_minutes(timestamp, asleep_time)
                asleep_minute = int(asleep_time[-2:])
                guards[guard_id].total_asleep += minutes_slept

                while minutes_slept != 0:
                    guards[guard_id].minutes_asleep[asleep_minute] += 1
                    asleep_minute = (asleep_minute + 1) % 60
                    minutes_slept -= 1

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
