from collections import defaultdict

no_overlap = set()
claimed = defaultdict(list)

for line in open("3.in"):
    claim_id = int(line.split()[0][1:])
    margin_left, margin_top = map(int, line.split()[2][:-1].split(","))
    size_left, size_top = map(int, line.split()[3].split("x"))
    overlaps = False

    for x in range(margin_left, margin_left + size_left):
        for y in range(margin_top, margin_top + size_top):
            pair = (x, y)
            if pair in claimed:
                overlaps = True
                for overlap_id in claimed[pair]:
                    if overlap_id in no_overlap:
                        no_overlap.remove(overlap_id)

            claimed[pair].append(claim_id)

    if not overlaps:
        no_overlap.add(claim_id)

print(no_overlap)
