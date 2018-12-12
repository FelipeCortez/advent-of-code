from collections import defaultdict

coords = []

for line in open("06.in"):
    coords.append(tuple(map(int, line.split(","))))

min_x = min([c[0] for c in coords])
min_y = min([c[1] for c in coords])

coords = [(c[0] - min_x, c[1] - min_y) for c in coords]

max_x = max([c[0] for c in coords])
max_y = max([c[1] for c in coords])

grid = [[None] * max_x for _ in range(max_y)]

def manhattan(c1: list, c2: list):
    return abs(c1[0] - c2[0]) + abs(c1[1] - c2[1])

for x in range(max_x):
    for y in range(max_y):
        for c in coords:
            distance = manhattan(c, [x, y])

            if grid[y][x] is None or distance < grid[y][x][1]:
                grid[y][x] = (c, distance)
            elif distance == grid[y][x][1]:
                grid[y][x] = (None, distance)

blacklist = set()
count = defaultdict(int)
for x in range(max_x):
    for y in range(max_y):
        count[grid[y][x][0]] += 1

        if x in (0, max_x - 1) or y in (0, max_y - 1):
            blacklist.add(grid[y][x][0])

print(list(filter(
    lambda x: x[0] not in blacklist,
    sorted(count.items(), key=lambda kv: kv[1], reverse=True)
))[0])

region_area = 0
for x in range(max_x):
    for y in range(max_y):
        if sum([manhattan(c, [x, y]) for c in coords]) < 10000:
            region_area += 1

print(region_area)
