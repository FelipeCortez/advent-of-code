from collections import defaultdict

lines = open("12.in").readlines()
pots = list(lines[0].split()[2])
rules = defaultdict(lambda: ".")

for instruction in lines[2:]:
    cin, cout = instruction.strip().split(" => ")
    rules[cin] = cout

first_plant = 0
generation = 0

while True:
    generation += 1
    new_pots = []
    new_first_plant = None

    for idx in range(-2, len(pots) + 2):
        neighborhood = []
        for neighbor_idx in range(idx - 2, idx + 3):
            if 0 <= neighbor_idx < len(pots):
                neighborhood.append(pots[neighbor_idx])
            else:
                neighborhood.append(".")

        result = rules[''.join(neighborhood)]

        if idx >= 0:
            new_pots.append(result)
        elif idx < 0:
            if result == "#" or new_first_plant is not None:
                new_pots.append(result)
                if new_first_plant is None:
                    new_first_plant = first_plant + idx

    if new_first_plant is not None:
        first_plant = new_first_plant

    while new_pots[0] == ".":
        new_pots = new_pots[1:]
        first_plant += 1

    while new_pots[-1] == ".":
        new_pots.pop()

    sum, count = 0, 0
    for idx, pot in enumerate(new_pots, start=first_plant):
        if pot == "#":
            count += 1
            sum += idx

    print(f"{generation} [{sum}] {''.join(new_pots)}")

    if "".join(pots) == "".join(new_pots):
        sum_when_5_billion = sum + ((50000000000 - generation) * count)
        print(f"50000000000 [{sum_when_5_billion}] {''.join(pots)}")
        break

    pots = new_pots


