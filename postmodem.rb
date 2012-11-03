require_relative 'feed'

set :haml, format: :html5
set :server, :thin
set :static_cache_control, [:public, :max_age => 300] if production?

$updated_at = Time.now
Sinatra::Application.reset!

def cache_results!
  $updated_at ||= Time.now
  last_modified $updated_at
  expires 500
end

before do
  @episodes = get_eps
  headers "Content-Type" => "text/html; charset=utf-8"
end

get '/' do
  cache_results!
  haml :index, locals: { eps: @episodes }
end

get '/episode/:n' do |n|
  cache_results!

  n = Integer(n) rescue 0
  not_found unless (1..@episodes.size).cover? n
  
  haml :episode, locals: { episode: @episodes[-n], idx: n }
end

get '/reset' do
  $updated_at = nil
  redirect '/'
end

not_found do
  "404 Not Found, Bro"
end
