PathTagParse 0.1.0.0
====

A small script to tag files (mp3 for example) with id3v2 tags based on a
passed in regular expression and corresponding denoted fields.

Example Usage:

$ tree
.
└── Artist
    └── 2001\ -\ Album1
        ├── 01\ -\ song.mp3
        ├── 02\ -\ song.mp3
        ├── 03\ -\ song.mp3
        ├── 04\ -\ song.mp3
        ├── 05\ -\ song.mp3
        ├── 06\ -\ song.mp3
        ├── 07\ -\ song.mp3
        ├── 08\ -\ song.mp3
        ├── 09\ -\ song.mp3
        ├── 10\ -\ song.mp3
        ├── 11\ -\ song.mp3
        ├── 12\ -\ song.mp3
        ├── 13\ -\ song.mp3
        ├── 14\ -\ song.mp3
        ├── 15\ -\ song.mp3
        ├── 16\ -\ song.mp3
        ├── 17\ -\ song.mp3
        ├── 18\ -\ song.mp3
        ├── 19\ -\ song.mp3
        ├── 20\ -\ song.mp3
        ├── 21\ -\ song.mp3
        ├── 22\ -\ song.mp3
        ├── 23\ -\ song.mp3
        └── 24\ -\ song.mp3

$ PathTagParse -p"." -r"\\./(.*?)/(\\d+) - (.*)/(\\d+) - (.*)\\.mp3" --artist \
  --year --album --track --year
