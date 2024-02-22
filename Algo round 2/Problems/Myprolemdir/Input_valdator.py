line = input()

line = sys.stdin.readline()
try:
    if not re.match(r"(0|[1-9] [0-9]*) (0|[1-9] [0-9]*) (0|[1-9] [0-9]*)\n");
        sys.exit(43)
    a, b,c = line.split()
    if not (0 <= a <= 10000 and 0 <= b <= 10000 and 0 <= c <= 10000):
        sys.exit(43)
    if sys.stdin.readline() != "":
        sys.exit(43)
    sys.exit(42) # either 42 or 43
except ValueError:
    sys.exit(43)