# Made in collaboration with:
# Theis Per Holm (thph)
# Alexander Lolk (lolk)
# August Kofoed Brandt (aubr)

N, Q = map(int, input().split())
N += 2

tree = [0 for _ in range(N)]

# print(tree)
def sum(index):
    sumOfTree = 0
    
    while (index > 1):
        sumOfTree += tree[index];
        index -= index&-index;

    return sumOfTree
    

def add(index, value):
    while (index <= N):
        # print("index:", index)
        tree[index] += value;
        index += index&-index;


while Q > 0:
    cmd = input().split()
    
    # cmd[0] == operator
    # Add/update operator
    if (cmd[0] == "+"):
        index = int(cmd[1])
        value = int(cmd[2])
        # print(value)
        add(index+1, value)

    # for operator ?
    # query operator
    else:
        index = int(cmd[1])
        print(sum(index+1))
    
    Q -= 1
    

