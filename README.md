Structure
=========

This project contains my personal dotfiles collection. My primary machine is Mac OS X,
but most of this stuff should work equally well under Linux (with a few alias exceptions).

Changes
=======

I used to use git submodules to bring in my own form of Robby Russell's excellent
Oh My ZSH, and a bunch of Vim plugins. These days for ZSH, I just fork [ Robby's
repository](https://github.com/robbyrussell/oh-my-zsh) and overwrite .zshrc.
In the future, I will have a single bootstrap script here so you clone this repo,
run ```./bootstrap.sh``` and have everything "just work".

For vim, I use the excellent [SPF-13](http://vim.spf13.com/) distro and
symlink the ```.vimrc.local``` and ```.vimrc.bundles.local``` files to my
home directory. The main difference between my setup and just about every other
Vimmer on the planet, is that I use ```"\<space>"``` as my ```<leader>```.
It's a giant freaking key in the middle of the keyboard and both my thumbs are
well-trained to locate it. Why anyone would want to aim for a target as small as
the default (backslash) or everyone's favorite (comma)-even smaller on most keyboards-is beyond me.


[![endorse](http://api.coderwall.com/xmlblog/endorsecount.png)](http://coderwall.com/xmlblog)

License
=======

The MIT License

Copyright (c) 2010-2013 Christian Romney <xmlblog@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
