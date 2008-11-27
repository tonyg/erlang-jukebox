#!/usr/bin/env python

from __future__ import with_statement 

import os
import sys
import subprocess
import Image, ImageFile

from mutagen.apev2 import APEv2
from mutagen.mp3 import MP3
from mutagen.flac import FLAC
from mutagen.oggvorbis import OggVorbis
from mutagen.mp4 import MP4

thumb_size = (96, 96)

def get_tags(extension, music_file):
    if extension == '.mp3':
        tags = MP3(music_file)
    elif extension == '.ogg':
        tags = OggVorbis(music_file)
    elif extension == '.flac':
        tags = FLAC(music_file)
    elif extension == '.m4a':
        tags = MP4(music_file)
    return tags


def get_gain(extension, music_file):
    gain = None

    try:
        if extension == '.mp3':
            # mp3gain stores replay gain info in an APEv2 tag, not ID3.
            apev2 = APEv2(music_file)
            gain = apev2['REPLAYGAIN_TRACK_GAIN'].value

        elif extension == '.ogg':
            tags = OggVorbis(music_file)
            gain = tags['replaygain_track_gain'][0]

        elif extension == '.m4a':
            tags = MP4(music_file)
            gain = tags['----:com.apple.iTunes:replaygain_track_gain'][0] # Oh, how I wish I were kidding

        elif extension == '.flac':
            tags = FLAC(music_file)
            gain = tags['replaygain_track_gain'][0]

    except:
        pass # Lazily we assume that if anything went wrong it's because the tag was not there

    if gain:
        # We have e.g. -3.100000 dB. Remove the "dB" and convert to float
        gain = float(gain.split(" ")[0])
    return gain

# If the file does not have a gain tag, try to add one. This is expensive, so we try
# not to do it.
def evaluate_gain(extension, music_file):
    if extension == '.mp3':
        cmd = ["mp3gain", "-T", music_file] # -T means modify existing file
    elif extension == '.ogg':
        cmd = ["vorbisgain", "-f", music_file] # -f means ignore file if it has tags
    elif extension == '.m4a':
        cmd = ["aacgain", "-T", music_file] # -T means modify existing file
    elif extension == '.flac':
        cmd = ["metaflac", "--add-replay-gain", music_file]

    if cmd:
        try:
            subprocess.call(cmd, stdout="/dev/null")
        except:
            pass # It's probably not installed. Just continue.

def add_tag(tags, metadata, read_name, write_name):
    if read_name in tags.tags:
        tag = tags.tags[read_name]
        tag = tag[0]

        if isinstance(tag, tuple):
            # m4a files seem to return track number as a tuple: (tracknumber, totaltracks)
            (tag, _ignore) = tag

        metadata.write("%s\n" % write_name)
        metadata.write("%s\n" % tag)

try:
    extension = sys.argv[1].lower()
    music_file = sys.argv[2]
    
    tags = get_tags(extension, music_file)
    gain = get_gain(extension, music_file)
    
    if not gain:
        evaluate_gain(extension, music_file)
        gain = get_gain(extension, music_file)
    
    cacheName = sys.argv[3]
    cacheHash = cacheName.split("/")[1]
    
    with open(cacheName + ".metadata", "w") as metadata: 
        metadata.write("+ OK\n")
        metadata.write("totalTime\n")
        metadata.write("%d\n" % tags.info.length)
        if gain:
            metadata.write("replayGain\n")
            metadata.write("%f\n" % gain)
    
        if extension == '.m4a':
            # I don't know if this counts as a defect in Mutagen or iTunes
            add_tag(tags, metadata, "\xa9ART", "artistName")
            add_tag(tags, metadata, "\xa9alb", "albumTitle")
            add_tag(tags, metadata, "\xa9nam", "trackName")
            add_tag(tags, metadata, "trkn", "trackNumber")
    
        elif extension == '.mp3':
            add_tag(tags, metadata, "TPE1", "artistName")
            add_tag(tags, metadata, "TALB", "albumTitle")
            add_tag(tags, metadata, "TIT2", "trackName")
            add_tag(tags, metadata, "TRCK", "trackNumber")
            if 'APIC:Front Cover' in tags:
                apic = tags.tags['APIC:Front Cover']
                image_folder = os.path.join("priv", "server_root", "htdocs", "images")
                if not os.path.exists(image_folder):
                    os.mkdir(image_folder)
                image_file = os.path.join(image_folder, cacheHash)
                parser = ImageFile.Parser()
                parser.feed(apic.data)
                im = parser.close()
                im.thumbnail(thumb_size, Image.ANTIALIAS)
                im.save(image_file, "JPEG")
                metadata.write("albumArt\nYes\n")
    
        else:
            add_tag(tags, metadata, "artist", "artistName")
            add_tag(tags, metadata, "album", "albumTitle")
            add_tag(tags, metadata, "title", "trackName")
            add_tag(tags, metadata, "tracknumber", "trackNumber")
    
        metadata.write("cacheHash\n%s\n" % cacheHash)
        metadata.close()
    
except:
    with open(sys.argv[3], "w") as metadata:
        metadata.write("- Error\n")
        import traceback
        traceback.print_exc(file = metadata)
