# MMacs

MMacs is my custom Emacs configuration.

# Purpose & Guiding Principles for MMacs

## MMacs as an Operating System

MMacs is meant to be my entire computing experience. It needs to be able to do everything in my daily workflow, including system administration, PKMS, software development, communications, web browsing, and all the other random stuff an computer needs to do.

I plan to have a Gentoo-based "MMOS", and MMacs will serve as the entire Desktop Environment via EXWM. I will be building MMacs to support this from the start.

## Organization

Development of this configuration will take a long time, and needs to be done right. I'm using version control this time, as well as splitting off certain parts of the config into separate files.

I like to keep package management and configuration as clean as possible. For this reason, I'm using `straight.el` and `use-package`. I'll try to configure everything through `use-package` where possible. The configuration files themselves are organized into major sections, with copious amounts of plain-english comments.

## Performance vs Features

I want MMacs to be as performant as possible. For starters I'll be using `--with-native-compilation` (a.k.a. GCC Emacs). I'll also be using the emacs client & server, as well as `async`.

I want to be picky, and use the packages that meet my needs with as little fluff or bloat as possible. I try to minimize usage of constantly-running or instantaneous things, like realtime syntax checking or completion suggestions, by adding acceptable delays and idling requirements, as well as simply using lighter packages.

## Accessibbility

I have several unique needs and restrictions which mean certain accesssibility considerations will need to go into MMacs.

I am already experiencing hand, shoulder, and neck pains. I'm a carpenter by trade and a computer nerd by hobby, so this will *never* get any better. To help with this, I'm crafting my own set of keybinds using `general.el` in a style similar to Spacemacs. Instead of a single leader, I'm diving heavily into separate leaders for things. There is still a global leader, but it's not going to be as central to my workflow.

I also have some undiagnosed condition, which I'm currently undergoing extensive screening for, which affects my attention, memory, and language processing ability. With every extra step that is introduced to something, I struggle exponentially more to keep track of all the details. I'll be working to make MMacs as visually-minimal and non-distracting as possible.

My eyes are also starting to hurt, and I've noticed some connections between my aforementioned condition, light, color, and how bad I feel. Certain combinations of color, brightness, and saturation make me physically sick to my stomach and can induce a mild headache. I'll be developing my own color scheme and Emacs theme from scratch.
