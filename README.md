Structure
=========

This project contains my personal dotfiles collection. My primary machine is Mac
OS X, but most of this stuff should work equally well under Linux with a few
tweaks.

I use [GNU stow](https://www.gnu.org/software/stow/) to manage the symlinks in
this repository, and [Homebrew](https://brew.sh/) as my package manager. The
`install.sh` script will bootstrap everything including the installation of stow
and other packages.

From then on, simply running `./homesync` should keep things running smoothly.

The install script also configures a pre-commit hook in this repository that
updates the brewfile and caskfile with any package differences it detects. This
forces me to commit those changes.

I run [iTerm2](https://www.iterm2.com/), [Fish Shell](https://fishshell.com/)
using [Fisher](https://github.com/jorgebucaran/fisher) to manage plugins. iTerm2
is configured to use the [Hack for Powerline](https://github.com/powerline/fonts/tree/master/Hack) font. .


License
=======

The MIT License

Copyright (c) 2010-2021 Christian Romney <christian.a.romney@gmail.com>

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
