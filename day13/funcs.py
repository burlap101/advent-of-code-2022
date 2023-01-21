def extract_packet_pairs(lines: list[str]) -> list[(str, str)]:
    result: list[(str, str)] = list()
    for i in range(0, len(lines), 3):
        result.append((lines[i], lines[i+1]))
    return result


# if same return -1 else if m1 > m2 0 else 1
def compare_ints(m1: int, m2: int) -> int:
    if m1 == m2:
        return -1
    elif m1 > m2:
        return 0
    return 1


def compare_members(m1: list[int | list], m2: list[int | list]) -> int:
    INT = type(1)
    LIST = type([1])

    ts = (type(m1), type(m2))
    if ts == (INT, INT):
        return compare_ints(m1, m2)
    elif ts == (LIST, LIST):
        return compare_lists(m1, m2)
    elif ts == (INT, LIST):
        return compare_lists([m1], m2)
    elif ts == (LIST, INT):
        return compare_lists(m1, [m2])


def compare_lists(l1: list[int | list], l2: list[int | list]) -> int:
    for m1, m2 in zip(l1, l2):
        res = compare_members(m1, m2)
        if res >= 0:
            return res
    if len(l1) == len(l2):
        return -1
    elif len(l1) > len(l2):
        return 0
    return 1


def part1(filename: str) -> int:
    with open(filename, 'r') as f:
        lines = f.readlines()
        tups = extract_packet_pairs(lines)
        result: list[bool] = []
        for t1, t2 in tups:
            res = compare_lists(eval(t1), eval(t2))
            if res == 0 or res == 1:
                result.append(res)
            else:
                raise ValueError("expected only 0 or 1; got", res)
        sum_of_indices = 0
        for i in range(len(result)):
            sum_of_indices += (i+1) * result[i]
        return sum_of_indices


