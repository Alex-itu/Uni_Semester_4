# maybe linked list?
n = int(input())

num = input().split()
li = [int(i) for i in num]


def sort (a):
    if len(a) <= 1:
        return

    l_a = a[:len(a)//2]
    r_a = a[len(a)//2:]
    
    sort(l_a)
    sort(r_a)
    merge(a, l_a, r_a)
    
    

def merge(a, l_a, r_a):
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
        
    while l_index <= (len(l_a)-1):
        a[m_index] = l_a[l_index]
        l_index += 1
        m_index += 1
        
    while r_index <= (len(r_a)-1):
        a[m_index] = r_a[r_index]
        r_index += 1
        m_index += 1


sort(li)

#print(li)

text = ""

for i in range(n):
    if i == 0:
        text += "" + str(li[i])
    else:
        if li[i-1] == (li[i]-1):
            if i+1 < len(li) and li[i+1] == (li[i]+1):
                if i-2 >= 0 and li[i-1] == (li[i]-1) and li[i-2] == (li[i]-2):
                    text += ""
                else:
                    text += "-"
            elif i-2 >= 0 and li[i-1] == (li[i]-1) and li[i-2] == (li[i]-2):
                text += "" + str(li[i])
            else:
                text += " " + str(li[i])
        else:
            if i-2 >= 0 and li[i-1] == (li[i]-1) and li[i-2] == (li[i]-2):
                text += "" + str(li[i])
            else:
                text += " " + str(li[i])
            
print(text)