n, m = map(int, input().split())

forbidden_pairs = set()
for _ in range(m):
    a,b = map(int, input().split())
    forbidden_pairs.add(tuple(sorted([a,b])))
subs = set()

def do_count (i): # all j<i might be in subs when called
    global subs
    if i == n+1:
        for a,b in forbidden_pairs:
            if a in subs and b in subs:
                return 0
        return 1
    ct = do_count(i+1)
    subs.add(i)
    ct += do_count(i+1)
    subs.remove(i)
    return ct

print(do_count(0))