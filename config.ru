require 'bundler'
Bundler.require

require './postmodem'

use Rack::Cache
use Rack::Reloader if development?

run Sinatra::Application