#Import Dependencies
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

#Twitter Developer Access Tokens
access_token = "846302774-pPB0TfVkUWOO4aEr8hKNlKupkog1g1VFPnho1Td3"
access_token_secret = "IUafuVm1YOk3S3hIflkUPxbI3Zb6pQubm2vseeBIRAx79"
consumer_key = "uAMKZaMyqwAan03Cd5ZJpL56A"
consumer_secret = "o9Q5OuYx6imBUcYvEIi8MVQ9WdSuCkCGIT9ieA0YNTJJZMBleL"

#Output Listener
class StdOutListener(StreamListener):
    def on_data(self, data):
        print(data)
        return True
    def on_error(self, status):
        print(status)

#Main Function
if __name__ == '__main__':
    #This handles Twitter authetification and the connection to Twitter 
    #Streaming API
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)
    #Filter Tweets to specified words
    stream.filter(track=['Republican','Democrat','Election','Voting','Donald Trump', 'Biden', 'Warren', 'Sanders', 'O\'Rourke','Presidential Nominations','President','Presidential Candidate'])
