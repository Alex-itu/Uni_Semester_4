press = int(input())

count_A = 1 # since it start with one A
count_B = 0 

# A -> B 
# B -> BA
# so one A = 0 1
# so one B = 1 1
# so one BA = 1 2
# o

for i in range(press):
    temp_A = count_A # since it seem to be that B is always itself plus the old A value
    count_A = count_B # since it seem to be that is always the old value for B
    count_B += temp_A # The new B


print(f"{count_A} {count_B}")

