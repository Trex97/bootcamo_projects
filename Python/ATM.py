# Program to generate a random number between 0 and 9

# importing the random module
import random
number_ran = random.randint(0000,9999)
number_ran

class ATM:
    import random
    def __init__(self,name,balance):
        self.name = name
        self.balance = balance 

    def deposite(self,money):
        self.balance = self.balance+ money
        print(f"Account: {self.name}")
        print(f"You have New Balance: {self.balance}")
        print(f"Deposite Successfully")

    def withdraw(self,money):
        self.balance = self.balance - money 
        print(f"Account: {self.name}")
        print(f"You have New Balance: {self.balance}")
        print(f"Withdraw Successfully")

    def check_balance(self):
        print(f"Account: {self.name}")
        print(f"You have balace: {self.balance}")

    def reset_user(self,new_name):
        print(f"Username account: {self.name}")
        self.name = new_name
        print(f"Reset username to {self.name}")
        print(f"You have reset username success.")

    def otp(self):
        number_ran = random.randint(0000,9999)
        print(f"You OTP is {number_ran}")



SCB = ATM("AU",5000)
print(SCB.name)
print(SCB.balance)


SCB.deposite(5000)
SCB.withdraw(2000)
SCB.check_balance()
SCB.reset_user("Prayfa")
SCB.otp()
