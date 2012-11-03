require 'feed-normalizer'
require 'open-uri'

def get_eps
  feed = FeedNormalizer::FeedNormalizer.parse open('http://feeds.feedburner.com/postmodem')
  eps = []
  
  for entry in feed.entries
    eps.push({:title       => entry.title,
              :description => entry.description,
              :date        => entry.date_published,
              :audio       => {:url  => entry.enclosure.url,
                               :type => entry.enclosure.type},
             })
  end
  
  return eps
end

