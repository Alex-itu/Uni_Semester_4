n, t = input().split(' ')

Customer_Queue = {}
posOfMoney = []

#print(f"People: {n} \nTime: {t}")

for current_Customer in range(n):
    customer_Money, customer_Time = input().split(' ')
    