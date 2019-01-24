# raylib-scm

Chiken Scheme bindings for Raylib Game Library (https://www.raylib.com/, https://github.com/raysan5/raylib).

## DISCLAIMER

I'm just researching features of Raylib and Chicken Scheme. These bindings are incomplete, probably inefficient and not idiomatic.  Use this bindings at your own risk. I even haven't any intent to long-term development of this code.

But, anyway, some work writing glue code is done and may be useful to somebody else.

## LICENSE WARNING

Please note: although Raylib itself is licensed by BSD-like license, this library is licensed by GNU Affero Public License (https://www.gnu.org/licenses/agpl-3.0.en.html). You cannot use it in proprietary projects: in my very strong opinion, code once open-sourced shall be open-source forever.

## Installation

In the root directory of the project:
```
sudo chicken-install
```

## Usage

See `examples` directory.

## ToDo

 - [ ] Implement enough bindings to rewrite in Scheme all Raylib examples
 - [ ] Rewrite in Scheme all Raylib examples
 - [ ] Allocate C objects in memory managed by Chicken: it's probably much more efficient than `alloc()`
 - [ ] Make API and deployment files more idiomatic for Chicken Scheme.
 - [ ] Write something funny and/or useful using this bindings.
