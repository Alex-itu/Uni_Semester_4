import sys

sys.setrecursionlimit(10**6) # allow 1,000,000 recursive calls

results = {0: 0, 1: 1}

#@lru_cache(maxsize=None) save result instead of give a error
def fib_fast(n):
    if n in results: 
        return results[n]
    res = fib_fast(n - 1) + fib_fast(n - 2)
    results[n] = res
    return res

