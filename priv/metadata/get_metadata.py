#!/usr/bin/env python

from __future__ import with_statement 
import sys
from mutagen.mp3 import MP3
from mutagen.easyid3 import EasyID3
from mutagen.flac import FLAC
from mutagen.oggvorbis import OggVorbis
from mutagen.mp4 import MP4

extension = sys.argv[1].lower()
music_file = sys.argv[2]

if extension == '.mp3':
    tags = MP3(music_file, ID3=EasyID3)
elif extension == '.ogg':
    tags = OggVorbis(music_file)
elif extension == '.flac':
    tags = FLAC(music_file)
elif extension == '.m4a':
    tags = MP4(music_file)

with open(sys.argv[3], "w") as metadata: 
    metadata.write("TotalTime\n")
    metadata.write("%d\n" % tags.info.length)
    metadata.close()

