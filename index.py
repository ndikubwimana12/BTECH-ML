import pandas as pd

data = "students.csv"

df = pd.read_csv(data)

print(df.head())
print(df.tail())