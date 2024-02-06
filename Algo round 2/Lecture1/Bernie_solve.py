n = int(input())

d = {}

for _ in range(n):
    country, year = input().split()
    year = int(year)
    if country not in d:
        d[country] = []
    d[country].append(year)
    
for country in d:
    d[country].sort()

q = int(input())
a = []
for _ in range(q):
    country, k = input().split()
    k = int(k)
    a.append(d[country][k-1])

for i in range(len(a)):
    print(a[i])