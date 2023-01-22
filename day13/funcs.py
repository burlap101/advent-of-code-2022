from functools import cmp_to_key


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


def cmp_translator(l1: str, l2: str) -> int:
    res = compare_lists(eval(l1), eval(l2))
    if res == -1:
        return 0
    elif res == 1:
        return -1
    return 1


def part2(filename: str) -> int:
    with open(filename, 'r') as f:
        lines = f.readlines()
        tups = extract_packet_pairs(lines)
        full_list: list[str] = []
        for t1, t2 in tups:
            full_list.append(t1)
            full_list.append(t2)
        sep1 = "[[2]]"
        sep2 = "[[6]]"
        full_list.append(sep1)
        full_list.append(sep2)
        srtd = sorted(full_list, key=cmp_to_key(cmp_translator))
        return (srtd.index(sep1)+1)*(srtd.index(sep2)+1)
