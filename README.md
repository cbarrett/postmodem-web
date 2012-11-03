# An website, I guess

## Dependencies

check the Gemfile yo

# How to set up a sandboxed development environment

Implicit assumptions: you have chmod'd /usr/local to be writable without sudo. YMMV otherwise. I recommend doing this because operating without sudo will prevent you from accidentally overwriting any part of the system Ruby (i.e. that in /usr/bin/ruby), and you don't want to overwrite the system Ruby.

1. Install rbenv. `brew install rbenv`
2. rbenv will output a line for you to put in your .zshrc. Do so. (For me it was `if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi`.)
3. Restart your shell so the shims can take effect. Marvel at the decreased speed of your login. Consider the nature of clownshoes.
4. Install ruby-build for automatic version downloads. `brew install ruby-build`
5. Install Ruby 1.9.2-p320. `rbenv install 1.9.2-p320`
5a. If you see a log message whining about Xcode 4.2 and llvm-gcc and ultimately come away with the impression that the developers of Ruby are mouth-breathing Neanderthals who should not walk the earth, much less program a computer, you may need to install 1.9.3-p194 instead.
6. Enable it as your current Ruby. `rbenv global 1.9.2-p320` (alternatively you can do it dir-by-dir but whatever)
7. Make sure that the output of `which gem` points to `$HOME/.rbenv/shims/gem`. Otherwise you're installing bundler globally, which is just a labyrinth of hurt. If it doesn't, try restarting your shell.
8. get bundler `gem install bundler`
9. download applicable gems `bundle update`

Bundler complained to me about that having a writable /usr/local/Cellar being possibly insecure. I told it to go eat a dick, I'm surprised I got it to work at all.

Fucking Ruby. Clownshoes.


# Running this shit

Follow all the instructions above, then run `rackup`.