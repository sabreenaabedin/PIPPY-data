from lxml import html
import requests
import pandas as pd

# Load webpage
page = requests.get('https://www.paulaschoice.com/ingredient-dictionary')

# Format string
tree = html.fromstring(page.content)

# Pull ingredients
ingredients = tree.xpath('//*[@id="primary"]/div[2]/div/div[2]/div/table/tbody//td[2]/h2/a/text()')

# Write the ingredients to a dataframe
df = pd.DataFrame(index = ingredients)

# Put in CSV, append each time it updates
df.to_csv("ingredients_list.csv", mode = 'a', header = False)    
   