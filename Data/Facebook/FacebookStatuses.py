import json
import datetime
import csv
import time
import re
try:
    from urllib.request import urlopen, Request
except ImportError:
    from urllib2 import urlopen, Request

app_id = "859372370885967"
app_secret = "835843de8ab5a1339c604da9311ce1a7"  # DO NOT SHARE WITH ANYONE!
group_ids = ["135818293109976","1582733551821102"] #Group 1: Acne and Pimple Solutions
# Group 2: Betty Organics

# input date formatted as YYYY-MM-DD
since_date = "2008-01-01"
until_date = "2018-03-26"

access_token = app_id + "|" + app_secret

for i in group_ids:
    def request_until_succeed(url):
        req = Request(url)
        success = False
        while success is False:
            try:
                response = urlopen(req)
                if response.getcode() == 200:
                    success = True
            except Exception as e:
                print(e)
                time.sleep(5)
    
                print("Error for URL {}: {}".format(url, datetime.datetime.now()))
                print("Retrying.")
    
        return response.read()
    
    # Needed to write tricky unicode correctly to csv
    
    
    def unicode_decode(text):
        try:
            return text.encode('utf-8').decode()
        except UnicodeDecodeError:
            return text.encode('utf-8')
    
    
    def getFacebookPageFeedUrl(base_url):
    
        # Construct the URL string; see http://stackoverflow.com/a/37239851 for
        # Reactions parameters
        fields = "&fields=message,link,created_time,type,name,id," + \
            "comments.limit(0).summary(true),shares,reactions" + \
            ".limit(0).summary(true),from"
        url = base_url + fields
    
        return url
    
    
    def getReactionsForStatuses(base_url):
    
        reaction_types = ['like', 'love', 'wow', 'haha', 'sad', 'angry']
        reactions_dict = {}   # dict of {status_id: tuple<6>}
    
        for reaction_type in reaction_types:
            fields = "&fields=reactions.type({}).limit(0).summary(total_count)".format(
                reaction_type.upper())
    
            url = base_url + fields
    
            data = json.loads(request_until_succeed(url))['data']
    
            data_processed = set()  # set() removes rare duplicates in statuses
            for comment in data:
                id = comment['id']
                count = comment['reactions']['summary']['total_count']
                data_processed.add((id, count))
    
            for id, count in data_processed:
                if id in reactions_dict:
                    reactions_dict[id] = reactions_dict[id] + (count,)
                else:
                    reactions_dict[id] = (count,)
    
        return reactions_dict
    
    
    def processFacebookPageFeedStatus(status):
    
        # The status is now a Python dictionary, so for top-level items,
        # we can simply call the key.
    
        # Additionally, some items may not always exist,
        # so must check for existence first
    
        status_id = status['id']
    
        comment_message = '' if 'message' not in status else \
            unicode_decode(status['message'])
   
    
        # Time needs special care since a) it's in UTC and
        # b) it's not easy to use in statistical programs.
    
        comment_published = datetime.datetime.strptime(
            status['created_time'], '%Y-%m-%dT%H:%M:%S+0000')
        comment_published = comment_published + \
            datetime.timedelta(hours=-5)  # EST
        comment_published = comment_published.strftime(
            '%Y-%m-%d %H:%M:%S')  # best time format for spreadsheet programs
        comment_author = unicode_decode(status['from']['name'])
    
        # Nested items require chaining dictionary keys.
    
        num_reactions = 0 if 'reactions' not in status else \
            status['reactions']['summary']['total_count']
        num_comments = 0 if 'comments' not in status else \
            status['comments']['summary']['total_count']
        num_shares = 0 if 'shares' not in status else status['shares']['count']
    
        return (status_id, comment_message, comment_author,
                comment_published, num_reactions, num_comments, num_shares)
    
    
    def scrapeFacebookPageFeedStatus(i, access_token, since_date, until_date):
        with open('facebook_statuses_{}.csv'.format(i), 'w', newline = "", encoding='utf-8') as file:
            w = csv.writer(file)
            w.writerow(["status_id", "comment_message", "comment_author",
                        "comment_published",
                        "num_reactions", "num_comments", "num_shares", "num_likes",
                        "num_loves", "num_wows", "num_hahas", "num_sads", "num_angrys" ,
                        "num_special"])
    
            has_next_page = True
            num_processed = 0   # keep a count on how many we've processed
            scrape_starttime = datetime.datetime.now()
    
            # /feed endpoint pagenates througn an `until` and `paging` parameters
            until = ''
            paging = ''
            base = "https://graph.facebook.com/v2.11"
            node = "/{}/feed".format(i)
            parameters = "/?limit={}&access_token={}".format(100, access_token)
            since = "&since={}".format(since_date) if since_date \
                is not '' else ''
            until = "&until={}".format(until_date) if until_date \
                is not '' else ''
    
            print("Scraping {} Facebook Group: {}\n".format(
                i, scrape_starttime))
    
            while has_next_page:
                until = '' if until is '' else "&until={}".format(until)
                paging = '' if until is '' else "&__paging_token={}".format(paging)
                base_url = base + node + parameters + since + until + paging
    
                url = getFacebookPageFeedUrl(base_url)
                statuses = json.loads(request_until_succeed(url))
                reactions = getReactionsForStatuses(base_url)
    
                for status in statuses['data']:
    
                    # Ensure it is a status with the expected metadata
                    if 'reactions' in status:
                        comment_data = processFacebookPageFeedStatus(status)
                        reactions_data = reactions[comment_data[0]]
    
                        # calculate thankful/pride through algebra
                        num_special = comment_data[4] - sum(reactions_data)
                        w.writerow(comment_data + reactions_data + (num_special,)) # THE PROBLEM IS status_data
    
                    # output progress occasionally to make sure code is not
                    # stalling
                    num_processed += 1
                    if num_processed % 100 == 0:
                        print("{} Statuses Processed: {}".format
                              (num_processed, datetime.datetime.now()))
    
                # if there is no next page, we're done.
                if 'paging' in statuses:
                    next_url = statuses['paging']['next']
                    until = re.search('until=([0-9]*?)(&|$)', next_url).group(1)
                    paging = re.search(
                        '__paging_token=(.*?)(&|$)', next_url).group(1)
    
                else:
                    has_next_page = False
    
            print("\nDone!\n{} Statuses Processed in {}".format(
                  num_processed, datetime.datetime.now() - scrape_starttime))
    
    
    if __name__ == '__main__':
        scrapeFacebookPageFeedStatus(i, access_token, since_date, until_date)