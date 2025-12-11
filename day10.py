from itertools import product
from z3 import Optimize, Int, sat


def parse(line):
    blocks = line.split(" ")
    config, *buttons, req = blocks
    config = config[1:-1]
    buttons = [[int(x) for x in b[1:-1].split(",")] for b in buttons]
    req = [int(x) for x in req.strip()[1:-1].split(",")]
    return buttons, config, req


def solve(line, part):
    buttons, config, goal = parse(line)

    if part == 1:
        goal = [int(x == "#") for x in config]

    bs = [set(x) for x in buttons]
    s = Optimize()
    variables = [Int(f"b{i}") for i in range(len(buttons))]
    for x in variables:
        s.add(x >= 0)
        if part == 1:
            s.add(x <= 1)
    for i, g in enumerate(goal):
        acc = 0
        for j, b in enumerate(bs):
            if i in b:
                acc = acc + variables[j]
        if part == 1:
            s.add(acc % 2 == g)
        else:
            s.add(acc == g)
    s.minimize(sum(variables))
    assert s.check() == sat
    m = s.model()
    ans = sum(m[x].as_long() for x in variables)
    return ans


with open("day10.in") as f:
    lines = f.readlines()
    print("Part 1:", sum(solve(x, 1) for x in lines))
    print("Part 2:", sum(solve(x, 2) for x in lines))
