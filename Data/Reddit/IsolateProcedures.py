import praw
import django

from django.utils.encoding import smart_str

reddit = praw.Reddit(client_id='5BjtAtxEWG2KOw',
                     client_secret="46LxAZQpnwqNVZeNfzzLPC6LyLY", password='!Weather26',
                     user_agent='testOne', username='tss3dn')
print(reddit.user.me())


#replace 'acne' with the subreddit you want to scrape!
#for submission in reddit.subreddit(t'popping').hot(limit=10):
   #print(submission.title)

subredditName = "acne"
#wordOfInterest1 = "worse"
#wordOfInterest2 = "pain"
#wordOfInterest3 = "breakout"
#wordOfInterest4 = "dry"
#wordOfInterest5 = "itch"
##wordOfInterest6 = "red"
#wordOfInterest7 = "painful"
#wordOfInterest8 = "hate"
#wordOfInterest9 = "yikes"
#wordOfInterest10 = "lol"

output = ""

for submission in reddit.subreddit(subredditName).hot(limit=20):
   for top_level_comment in submission.comments:
        output = output + smart_str(top_level_comment.body)

print(output.encode("cp437", "ignore"),  file=open("outputTest.csv", "a"))
