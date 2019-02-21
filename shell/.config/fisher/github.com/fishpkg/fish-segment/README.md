# fish-segment

Utilities to build a powerline prompt with [fish shell](https://fishshell.com).

## Installation

With [Fisher](https://github.com/jorgebucaran/fisher)

```
fisher add fishpkg/fish-segment
```

## Usage

Call segment / segment_right to create a powerline segment. Call segment_close after you have added all the prompt segments.

```fish
function fish_prompt
    segment white red FRONT
    segment black white BASE
    segment_close
end

function fish_right_prompt
    segment_right white red FRONT
    segment_right black white BASE
    segment_close
end
```

![Example](https://cloud.githubusercontent.com/assets/8317250/13501135/d0ccc7ec-e1a8-11e5-8bd1-e14b8b40242e.png)

## Options

- _foreground_: Set the text foreground color.

- _background_: Set the text background color.

- [*text*]: Set the text to display.

## Notes

- The left prompt is built from _right_ to left.
- The right prompt is built from _left_ to right.

## License

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.

In jurisdictions that recognize copyright laws, the author or authors of this software dedicate any and all copyright interest in the software to the public domain. We make this dedication for the benefit of the public at large and to the detriment of our heirs and successors. We intend this dedication to be an overt act of relinquishment in perpetuity of all present and future rights to this software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
