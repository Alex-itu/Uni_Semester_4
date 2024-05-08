import sys
import math

# if len(sys.argv) != 2:
#     print(f"Usage: python sol.py (NAIVE|SQ|SEGMENT|FENWICK)")
#     exit(1)

method = "FENWICK"


class IntervalDS:
    def __init__(self, n):
        """Initialize data structure to handle interval queries
           on n elements"""
        raise NotImplementedError()

    def update(self, i, k):
        """Set i-th element to k"""
        raise NotImplementedError()

    def get(self, i):
        """value of element position i"""
        raise NotImplementedError()

    def query(self, i, j):
        """Return sum of elements from i-th to j-th elements"""
        raise NotImplementedError()


class NaiveDS(IntervalDS):
    def __init__(self, n):
        self.A = [0 for _ in range(n + 1)] # only use A[1], ..., A[n]
    
    def update(self, i, k):
        self.A[i] = k

    def get(self, i):
        return self.A[i]

    def query(self, i, j):
        return sum(self.A[i : j + 1])

class SQ(IntervalDS):
    def __init__(self, n):
        self.blocks = math.ceil(n**.5) + 1
        self.block_size = math.ceil(n**.5)
        self.A = [0 for _ in range(n + 1)]
        self.B = [0 for _ in range(self.blocks)]

    def update(self, i, k):
        block = i // self.block_size
        self.B[block] -= self.A[i]
        self.B[block] += k
        self.A[i] = k 
    
    def get(self, i):
        return self.A[i]

    def query(self, i, j):
        first_i = (i // self.block_size) * self.block_size
        last_j = (j // self.block_size) * self.block_size + self.block_size - 1
        s = sum(self.B[i // self.block_size : j // self.block_size + 1])
        s -= sum(self.A[first_i : i])
        s -= sum(self.A[j + 1 : last_j + 1])
        return s

class SegmentTree(IntervalDS):
    def __init__(self, n):
        self.N = 1
        while self.N < n:
            self.N *= 2
        self.A = [0 for _ in range(2 * self.N)]

    def get(self, i):
        return self.A[self.N + i]
    
    def update(self, i, k):
        p = self.N + i
        self.A[p] = k
        p //= 2
        while p > 0:
            self.A[p] = self.A[2 * p] + self.A[2 * p + 1]
            p //= 2
    
    def query(self, i, j):
        #return self.__top_down_range_sum(1, 0, self.N, i, j + 1)
        return self.__bottom_up_range_sum(i, j)


    def __bottom_up_range_sum(self, i, j):
        i += self.N
        j += self.N

        s = 0
        while i <= j:
            if i % 2 == 1:
                s += self.A[i]
                i += 1
            if j % 2 == 0:
                s += self.A[j]
                j -= 1
            i //= 2
            j //= 2
        return s

    def __top_down_range_sum(self, p, start, span, i, j):
        if start + span <= i or j <= start:
            return 0
        if i <= start and start + span <= j:
            return self.A[p]
        left = self.__top_down_range_sum(2 * p, start, span // 2, i, j)
        right = self.__top_down_range_sum(2*p + 1, start + span // 2, span // 2, i, j)
        return left + right


class FenwickTree(IntervalDS):

    def __init__(self, n):
        self.A = [0 for _ in range(n + 1)]

    def get(self, i):
        return self.prefix_sum(i) - self.prefix_sum(i - 1)

    def update(self, i, k):
        self.add(i, k - self.get(i))
        
    def add(self, i, k):
        s = 0
        while i < len(self.A):
            self.A[i] += k
            i += i & -i

    def prefix_sum(self, i):
        s = 0
        while i > 0:
            s += self.A[i]
            i -= i & -i
        return s

    def query(self, i, j):
        return self.prefix_sum(j) - self.prefix_sum(i - 1)
        

cnt = sys.stdin.readlines()

n, q = map(int, cnt[0].split())

if method == "NAIVE":
    ds = NaiveDS(n)
elif method == "SEGMENT": 
    ds = SegmentTree(n)
elif method == "SQ":
    ds = SQ(n)
elif method == "FENWICK":
    ds = FenwickTree(n)
else:
    print("I don't know how to handle " + method)
    exit(1)

for line in cnt[1:]:
    if line[0] == 'F':
        _, reg = line.split()
        reg = int(reg)
        ds.update(reg, 1 - ds.get(reg))
    if line[0] == 'C':
        _, l, r = line.split()
        l, r = int(l), int(r)
        print(ds.query(l, r))