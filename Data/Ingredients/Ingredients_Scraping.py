from lxml import html
import requests
import pandas as pd
import numpy as np

# Load webpage
page = requests.get('https://www.paulaschoice.com/ingredient-dictionary')

# Format string
tree = html.fromstring(page.content)

# Pull ingredients
ingredients = tree.xpath('//*[@id="primary"]/div[2]/div/div[2]/div/table/tbody//td[2]/h2/a/text()')
ratings = tree.xpath('//*[@id="primary"]/div[2]/div/div[2]/div/table/tbody//td[1]/text()')

listy = []
for i in range(1715):
    list1 = tree.xpath('//*[@id="primary"]/div[2]/div/div[2]/div/table/tbody/tr[{}]/td[2]/p/text()'.format(i))
    listy.append(list1)
    
#categories = tree.xpath('')

# Write the ingredients to a dataframe
df = pd.DataFrame(index = ingredients)
df = pd.DataFrame({'Ingredient': ingredients, 'Rating': ratings, 'Description': listy})



# Clean dataframe
df['Description'] = df['Description'].astype(str)
df['Description'] = df['Description'].shift(-1)
df = df.replace('[]', '')
df = df.replace(np.NaN, '')

listy2 = []
for i in df['Description']:
    listy2.append(i.replace("['\\r\\n\\t\\t\\t\\t                \\t",'').
                  replace("\\r\\n\\t\\t\\t\\t                ']","").replace("',","").replace("']","").replace(
    '["\\r\\n\\t\\t\\t\\t                \\t',"").replace('\\r\\n\\t\\t\\t\\t                "',"").replace("[","").replace("]",""))

df['Description'] = listy2    
df = df[['Ingredient','Rating','Description']]

# Put in CSV, append each time it updates   
df.to_csv("ingredients.csv", mode = 'a', index = False)

