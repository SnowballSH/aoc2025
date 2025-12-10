from itertools import product


def parse(line):
    blocks = line.split(" ")
    config, *buttons, req = blocks
    config = config[1:-1]
    buttons = [[int(x) for x in b[1:-1].split(",")] for b in buttons]
    req = [int(x) for x in req.strip()[1:-1].split(",")]
    return buttons, config, req


def solve_part1(line):
    buttons, config, _ = parse(line)

    ans = len(buttons)
    for mask in product(range(2), repeat=len(buttons)):
        cnt = [int(c == "#") for c in config]
        onbits = 0
        for i, b in enumerate(mask):
            if b:
                for j in buttons[i]:
                    cnt[j] ^= 1
                onbits += 1
        if all(x == 0 for x in cnt):
            ans = min(ans, onbits)
    return ans


with open("day10.in") as f:
    lines = f.readlines()
    print("Part 1:", sum(map(solve_part1, lines)))
