if __name__ == "__main__":
    with open("3.in") as f:
        claimed = set()
        overlapped = set()

        for line in f:
            margin_left, margin_top = map(int, line.split()[2][:-1].split(","))
            size_left, size_top = map(int, line.split()[3].split("x"))

            for x in range(margin_left, margin_left + size_left):
                for y in range(margin_top, margin_top + size_top):
                    pair = (x, y)
                    if pair in claimed:
                        overlapped.add(pair)
                    else:
                        claimed.add(pair)

        print(len(overlapped))
