t = int(input())

def solve(target, coins):
    Solutions = {0: 0}
    
    for coin in coins:
        #print(f"considering using coin {coin}: solutions {Solutions}")
        for price, coins_needed in list(Solutions.items()):
            new_price = price + coin
            new_amount = coins_needed + 1
            if new_price > target + max(coins): 
                continue
            if new_price not in Solutions:
                Solutions[new_price] = new_amount
            elif Solutions[new_price] > new_amount:
                Solutions[new_price] = new_amount
                
    #print(Solutions)
    for i in range(target, max(Solutions)+1):
        if i in Solutions:
            return f"{i} {Solutions[i]}"

for _ in range(t):
    price = int(input())
    n = int(input())
    coins = [int(input()) for _ in range(n)]
    
    #print(coins)
    
    print(solve(price, coins))