n = int(input())

for i in range(n):
    m = int(input())
    
    set_1 = input().split()
    li_1 = [int(i) for i in set_1]
    
    set_2 = input().split()
    li_2 = [int(i) for i in set_2]
    
    li_1.sort(reverse=False)
    li_2.sort(reverse=True)
    
    count = 0
    
    for j in range(m):
        a = int(li_1[j])
        b = int(li_2[j])
        
        count += (a * b)
    print("Case #" + str(i+1) + ": " + str(count))

        
    


    