# fish-git-util

Convenient git utility functions for the [fish shell](https://fishshell.com).

## Installation

With [Fisher](https://github.com/jorgebucaran/fisher)

```
fisher add fishpkg/fish-git-util
```

## Functions

| Function             | Build Status                                                                                                                               |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| git_branch_name      | Get the name of the current branch                                                                                                         |
| git_is_detached_head | Test if a repository is in a detached HEAD state                                                                                           |
| git_is_dirty         | Test if there are changes not staged for commit                                                                                            |
| git_is_empty         | Test if a repository is empty                                                                                                              |
| git_is_repo          | Test if the current directory is a repository                                                                                              |
| git_is_staged        | Test if there are changes staged for commit                                                                                                |
| git_is_stashed       | Test if there are changes recorded in the stash                                                                                            |
| git_is_tag           | Test if a repository's HEAD points to a tag.                                                                                               |
| git_is_touched       | Test if there are changes in a repository working tree                                                                                     |
| git_untracked_files  | Get the number of untracked files in a repository                                                                                          |
| git_ahead            | Get a character that indicates if the current repo is in sync, ahead, behind or has diverged from its upstream. Default: '', '+', '-', '±' |
| git_repository_root  | Get the root directory of the current repository                                                                                           |

## License

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.

In jurisdictions that recognize copyright laws, the author or authors of this software dedicate any and all copyright interest in the software to the public domain. We make this dedication for the benefit of the public at large and to the detriment of our heirs and successors. We intend this dedication to be an overt act of relinquishment in perpetuity of all present and future rights to this software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
