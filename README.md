# raylib-scm

Chiken Scheme bindings for Raylib Game Library (https://www.raylib.com/, https://github.com/raysan5/raylib).

[![License](https://img.shields.io/github/license/yashrk/raylib-scm.svg?style=social)](LICENSE)

 * [DISCLAIMER](#disclaimer)
 * [LICENSE WARNING](#license-warning)
 * [Installation](#installation)
 * [Usage](#usage)
 * [ToDo](#todo)
 * [Current Raylib version](#current-raylib-version)
 * [Contacts](#contacts)
 * [Examples already implemented](#examples-already-implemented)
    * [core](#core)
       * [basic window](#basic-window)
       * [input keys](#input-keys)
       * [input mouse](#input-mouse)
       * [mouse wheel](#mouse-wheel)
       * [random values](#random-values)
       * [color select](#color-select)
       * [3d mode](#3d-mode)
       * [3d picking](#3d-picking)
       * [3d camera free](#3d-camera-free)
       * [3d camera first person](#3d-camera-first-person)
    * [textures](#textures)
       * [particles blending](#particles-blending)
    * [models](#models)
       * [box collisions](#box-collisions)
       * [obj loading](#obj-loading)
       * [heightmap](#heightmap)
       * [skybox](#skybox)
       * [yaw pitch roll](#yaw-pitch-roll)
    * [shaders](#shaders)
       * [postprocessing](#postprocessing)
       * [raymarching](#raymarching)
    * [audio](#audio)
       * [sound loading](#sound-loading)

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
 - [ ] Allocate C objects in memory managed by Chicken: it's probably much more efficient than `malloc()`
 - [ ] Make API and deployment files more idiomatic for Chicken Scheme.
 - [ ] Write something funny and/or useful using this bindings.
 
## Current Raylib version

Synchronized (well, kind of) with the commit [0ec46e89](https://github.com/raysan5/raylib/commit/0ec46e8976f48617fe39bc10a44ff7313d6b27ea).
 
## Contacts
 
 - https://yashrk.github.io/index-en.html
 - Raylib's discord: https://discord.gg/raylib, see #raylib-scm channel

## Examples already implemented

Click to screenshot to see source code of the respective example.

### core

#### basic window
[![basic window](https://github.com/yashrk/raylib-scm/blob/master/examples/core/basic_window/basic_window.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/basic_window)

#### input keys
[![input keys](https://github.com/yashrk/raylib-scm/blob/master/examples/core/input_keys/input_keys.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/input_keys/)

#### input mouse
[![input mouse](https://github.com/yashrk/raylib-scm/blob/master/examples/core/input_mouse/input_mouse.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/input_mouse/)

#### mouse wheel
[![mouse wheel](https://github.com/yashrk/raylib-scm/blob/master/examples/core/mouse_wheel/mouse_wheel.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/mouse_wheel/)

#### random values
[![random values](https://github.com/yashrk/raylib-scm/blob/master/examples/core/random_values/random_values.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/random_values/)

#### color select
[![color select](https://github.com/yashrk/raylib-scm/blob/master/examples/core/color_select/color_select.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/color_select/)

#### 3d mode
[![3d mode](https://github.com/yashrk/raylib-scm/blob/master/examples/core/3d_mode/3d_mode.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/3d_mode/)

#### 3d picking
[![3d picking](https://github.com/yashrk/raylib-scm/blob/master/examples/core/3d_picking/3d_picking.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/3d_picking/)

#### 3d camera free
[![3d camera free](https://github.com/yashrk/raylib-scm/blob/master/examples/core/3d_camera_free/3d_camera_free.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/3d_camera_free/)

#### 3d camera first person
[![3d camera first person](https://github.com/yashrk/raylib-scm/blob/master/examples/core/3d_camera_first_person/3d_camera_first_person.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/core/3d_camera_first_person/)

### textures

#### particles blending
[![particles blending](https://github.com/yashrk/raylib-scm/blob/master/examples/textures/particles_blending/particles_blending.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/textures/particles_blending/)

### models

#### box collisions
[![box collisions](https://github.com/yashrk/raylib-scm/blob/master/examples/models/box_collisions/box_collisions.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/models/box_collisions/)

#### obj loading
[![obj loading](https://github.com/yashrk/raylib-scm/blob/master/examples/models/obj_loading/obj_loading.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/models/obj_loading/)

#### heightmap
[![heightmap](https://github.com/yashrk/raylib-scm/blob/master/examples/models/heightmap/heightmap.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/models/heightmap/)

#### skybox
[![skybox](https://github.com/yashrk/raylib-scm/blob/master/examples/models/skybox/skybox.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/models/skybox/)

#### yaw pitch roll
[![yaw pitch roll](https://github.com/yashrk/raylib-scm/blob/master/examples/models/yaw_pitch_roll/yaw_pitch_roll.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/models/yaw_pitch_roll/)

### shaders

#### postprocessing
[![postprocessing](https://github.com/yashrk/raylib-scm/blob/master/examples/shaders/postprocessing/postprocessing.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/shaders/postprocessing)

#### raymarching
[![raymarching](https://github.com/yashrk/raylib-scm/blob/master/examples/shaders/raymarching/raymarching.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/shaders/raymarching)

### audio

#### sound loading
[![sound loading](https://github.com/yashrk/raylib-scm/blob/master/examples/audio/sound_loading/sound_loading.png)](https://github.com/yashrk/raylib-scm/tree/master/examples/audio/sound_loading)
