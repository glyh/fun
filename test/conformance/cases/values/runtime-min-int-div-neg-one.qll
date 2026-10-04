# min_int / -1 overflows even though it does not look like one:
# 0 - 9223372036854775807 - 1 is min_int, and its negation is not representable
(/)(0 - 9223372036854775807 - 1, 0 - 1)
