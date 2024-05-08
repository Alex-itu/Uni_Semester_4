sprinklerInfo = []
n = 0

def findMin(sprinklers):
   return "something"

for line in sys.stdin:
    if n > 0:
        x,r = map(int,line.split())
        if (r+r) > w & n > 0: # If not then that sprinkler diameter won't help cover anything
            # List of tuples
            # first is the "length" of the range 
            rang = rs - (ls)
            
            # second is range on right side
            rs = r-x
            
            # third is range on left side
            ls = x-r
            
            sprinklerInfo.append(((rang),(ls),(rs)))
            n -= 1
            
            if n == 0:
                print(findMin(sorted(sprinklerInfo)))
        
        elif n == 0:
            print(findMin(sprinklerInfo))
                    
    else: 
        sprinklers = [] # for reseting when it starts again with a new case
        n,l,w = map(int,line.split())
        
        
