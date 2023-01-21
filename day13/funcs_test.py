import unittest
import funcs


class TestFuncs(unittest.TestCase):
    def test_extract_packet_pairs(self):
        lines = [
            "first",
            "second",
            "third",
            "fourth",
            "fifth",
            "sixth",
        ]
        res = funcs.extract_packet_pairs(lines)
        expected = [
            ("first", "second"),
            ("fourth", "fifth"),
        ]
        self.assertEqual(res, expected)

    def test_part1(self):
        self.assertEqual(funcs.part1("input/test.txt"), 13)



