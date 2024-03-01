from collections import defaultdict
import sys

sys.setrecursionlimit(100001)
graph = defaultdict(list)

n = int(input())
for _ in range(n):
    vert,edgesstr = input().split(':')
    for x in edgesstr.split(' '):
        if x != '':
            graph[x].append(vert)
startpt = input().strip()

# print(graph)
# print(startpt)

output = []
seen = set()
def dfs(v):
    seen.add(v)
    for ngb in graph[v]:
        if ngb not in seen:
            dfs(ngb)
    output.append(v)

dfs(startpt)
for v in output[::-1]:
    print(v)