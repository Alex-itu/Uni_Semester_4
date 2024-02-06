n = int(input())

step = 0

li = []

for _ in range(n):
    num = int(input())
    li.append(num)



def sort (a, steps):
    if len(a) <= 1:
        return

    l_a = a[:len(a)//2]
    r_a = a[len(a)//2:]
    
    steps += sort(l_a, steps)
    steps += sort(r_a, steps)
    steps += merge(a, l_a, r_a, steps)
    return steps
    
    

def merge(a, l_a, r_a, steps):
    l_index = 0
    r_index = 0
    m_index = 0
    
    while l_index <= (len(l_a)-1) and r_index <= (len(r_a)-1):
        if l_a[l_index] < r_a[r_index]:
            a[m_index] = l_a[l_index]
            l_index += 1
            m_index += 1
        else:
            a[m_index] = r_a[r_index]
            r_index += 1
            m_index += 1
            steps += 1
        
    while l_index <= (len(l_a)-1):
        a[m_index] = l_a[l_index]
        l_index += 1
        m_index += 1
        
    while r_index <= (len(r_a)-1):
        a[m_index] = r_a[r_index]
        r_index += 1
        m_index += 1
    return steps


step = sort(li, 0)

print(li)
print(str(step))
        