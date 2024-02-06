n, t = input().split()
n = int(n)
t = int(t)

numbers = input().split()
a = [int(i) for i in numbers]

i = 0
time = 0
count = 0

while i < len(a):
    time += a[i]
    if time <= t:
        count += 1
    else:
        break
        
    i += 1


print(count)