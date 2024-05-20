import sys

sprinklerInfo = []
n = 0
l = 0

# there is also a way to do this where you take the one sprinkler that goes furthest to the left 
# and then just keep track of how far it goes to the right
# then you would sort on the left side (ls)

def findMin(sprinklers):
    hi = 0
    lo = l
    
    count = 0
    # for each sprinkler in the list
    for s in sprinklers:
        current = count
        
        # if the left side of the sprinkler is less than the low
        if s[1] < lo and s[0]:
            lo = s[1]
            count += 1
            
        # if the right side of the sprinkler is greater than the high
        if s[2] > hi:
            hi = s[2]
            if count != current + 1:
                count += 1  
            
        if lo <= 0 and hi >= l:
            return count
        
    return -1

for line in sys.stdin:
    if n > 0:
        x,r = map(int,line.split())
        if (r+r) > w and n > 0: # If not then that sprinkler diameter won't help cover anything
            r -= 1
            
            # List of tuples (range, left side, right side)
            # second element is range on left side
            ls = x-r
            
            # third element is range on right side
            rs = x+r
            
            
            # first element is the "length" of the range (used for sorting)
            rang = rs - (ls)
            
            # apppend the ring into the list with the range first so that it can be sorted by that
            # the left and right side are added so the range for each side is there
            if rang != 0:
                sprinklerInfo.append(((rang),(ls),(rs)))
            n -= 1
            
            if n == 0:
                print(findMin(sorted(sprinklerInfo, reverse=True)))
        
        elif n == 0:
            print(findMin(sprinklerInfo))
        
        else:
            n -= 1
                    
    else: 
        sprinklers = [] # for reseting when it starts again with a new case
        n,l,w = map(int,line.split())
        
        
