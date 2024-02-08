n, m = map(int, input().split())

forbidden_pairs = set()
for _ in range(m):
    a,b = map(int, input().split())
    forbidden_pairs.add(tuple(sorted([a,b])))

count = 0
for i in range(2**n):
        #subset = f"{i:0{n}b}"
        forbidden = False
        for a,b in forbidden_pairs:
            #if subset[a-1]=='1' and subset[b-1] == '1':
            if (i & (1 << (a-1))) and (i & (1 << (b-1))):
                forbidden = True
                break
        if not forbidden:
            count += 1
print(count)