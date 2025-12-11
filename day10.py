from z3 import Optimize, Int, sat


def parse(line):
    blocks = line.split(" ")
    config, *buttons, joltage = blocks
    config = config[1:-1]
    buttons = [[int(x) for x in b[1:-1].split(",")] for b in buttons]
    joltage = [int(x) for x in joltage.strip()[1:-1].split(",")]
    return buttons, config, joltage


def solve(line, part):
    buttons, config, joltage = parse(line)
    goal = [x == "#" for x in config] if part == 1 else joltage
    bs = [set(x) for x in buttons]
    s = Optimize()
    variables = [Int(str(i)) for i in range(len(buttons))]
    s.add(x >= 0 for x in variables)
    for i, g in enumerate(goal):
        acc = sum(variables[j] for j, b in enumerate(bs) if i in b)
        s.add(acc % 2 == g if part == 1 else acc == g)
    s.minimize(sum(variables))
    assert s.check() == sat
    soln = s.model()
    return sum(soln[x].as_long() for x in variables)


with open("day10.in") as f:
    lines = f.readlines()
    print("Part 1:", sum(solve(x, 1) for x in lines))
    print("Part 2:", sum(solve(x, 2) for x in lines))
