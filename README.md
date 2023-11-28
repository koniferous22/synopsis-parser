# `synopsis-parser`

Serializes linux man synopsis into json

Experimental PoC, tested only on git commands

## Example

Synopsis from `man git-clone` extracted through shell utils

```sh
# Input
man git-clone | awk '/^SYNOPSIS/,/^DESCRIPTION/ {print}' | head -n -1 | tail -n +2
```

```sh
# Ouptut
       git clone [--template=<template-directory>]
                 [-l] [-s] [--no-hardlinks] [-q] [-n] [--bare] [--mirror]
                 [-o <name>] [-b <name>] [-u <upload-pack>] [--reference <repository>]
                 [--dissociate] [--separate-git-dir <git-dir>]
                 [--depth <depth>] [--[no-]single-branch] [--no-tags]
                 [--recurse-submodules[=<pathspec>]] [--[no-]shallow-submodules]
                 [--[no-]remote-submodules] [--jobs <n>] [--sparse] [--[no-]reject-shallow]
                 [--filter=<filter> [--also-filter-submodules]] [--] <repository>
                 [<directory>]

```

Running Trough parser

```sh
make build
man git-clone | awk '/^SYNOPSIS/,/^DESCRIPTION/ {print}' | head -n -1 | tail -n +2 | ./bin/synopsis-parser | jq
```

Should produce [following output](./example.json)

## Example use-case

this repo - [Codeberg](https://codeberg.org/koniferous22/patched-git), [GitHub](https://github.com/koniferous22/patched-git)

## todo

Match fully [IEEE Utility conventions](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html)
