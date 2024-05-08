N = input().split()

Q = []

if (len(N) > 1):
    Q = N[1]

H = int(N[0]) + 1

root = 2**H
pos = 1

for path in Q:
    
    pos *= 2
    
    if path == 'R':
      pos += 1

print(root - pos)

# N = (3 + 1) = 4
# Q = RRL
# root = 2^N = 2^4 = 16

# path = R   pos = 1
# pos = (2*1) = 2
# pos = 2 + 1 = 3


# path = R   pos = 2
# pos = (2*3) = 6
# pos = 6 + 1 = 7

# path = L   pos = 4
# pos = (2*7) = 14

# sum = root - pos = 16 - 14

