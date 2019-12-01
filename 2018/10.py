import re
from math import sqrt
from copy import deepcopy
from operator import add, mul

stars = []

def print_stars(stars):
    positions = set([tuple(star["position"]) for star in stars])
    min_x = min([pos[0] for pos in positions])
    min_y = min([pos[1] for pos in positions])
    max_x = max([pos[0] for pos in positions])
    max_y = max([pos[1] for pos in positions])

    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print("#" if (x, y) in positions else ".", end="")
        print()

def area(stars):
    positions = [tuple(star["position"]) for star in stars]
    min_x = min([pos[0] for pos in positions])
    min_y = min([pos[1] for pos in positions])
    max_x = max([pos[0] for pos in positions])
    max_y = max([pos[1] for pos in positions])

    return (max_x - min_x) * (max_y - min_y)

def stars_at_time(stars, time):
    new_stars = deepcopy(stars)
    for star in new_stars:
        velocity_multiplied = list(map(mul, star["velocity"], [time, time]))
        star["position"] = list(map(add, star["position"], velocity_multiplied))

    return new_stars

for line in open("10.in"):
    pos_and_vels = list(map(int, re.findall(r"-?\d+", line)))
    stars.append({"position": pos_and_vels[:2], "velocity": pos_and_vels[2:]})

previous_area = None
previous_stars = None
time = 0
step = 10000

while True:
    time += step
    new_stars = stars_at_time(stars, time)
    if previous_area is not None and area(new_stars) > previous_area:
        if abs(step) == 1:
            print_stars(previous_stars)
            print(previous_time)
            break
        else:
            step = int(-(step // abs(step)) * sqrt(abs(step)))

    previous_stars = new_stars
    previous_area = area(previous_stars)
    previous_time = time
