#!/usr/bin/env python

from __future__ import with_statement 

import os
import sys
import subprocess

from mutagen.apev2 import APEv2
from mutagen.mp3 import MP3
from mutagen.flac import FLAC
from mutagen.oggvorbis import OggVorbis
from mutagen.mp4 import MP4

thumb_size = (96, 96)

class WAV:
    class Info:
        def __init__(self, music_file):
            # Assume that it's 44.1kHz stereo 16 bit. Anyone storing music as wav
            # deserves anything they get.
            self.length = os.path.getsize(music_file) / ( 2 * 2 * 44100) 

    def __init__(self, music_file):
        self.info = WAV.Info(music_file)
        self.tags = None

def get_tags(extension, music_file):
    if extension == '.mp3':
        tags = MP3(music_file)
    elif extension == '.ogg':
        tags = OggVorbis(music_file)
    elif extension == '.flac':
        tags = FLAC(music_file)
    elif extension == '.m4a':
        tags = MP4(music_file)
    elif extension == '.wav':
        tags = WAV(music_file)
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
    cmd = None

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
            subprocess.call(cmd, stdout=open("/dev/null"), stderr=open("/dev/null"))
        except OSError, e:
            pass # It's probably not installed. Just continue.

def add_tag(tags, metadata, read_name, write_name):
    if tags.tags and read_name in tags.tags:
        tag = tags.tags[read_name]
        tag = tag[0]

        if isinstance(tag, tuple):
            # m4a files seem to return track number as a tuple: (tracknumber, totaltracks)
            (tag, _ignore) = tag

        metadata.write("%s\n" % write_name)
        metadata.write("%s\n" % unicode(tag).encode("utf-8"))

def write_albumart(image_tag, metadata):
    if not image_tag:
        return

    image_file = os.path.join(cache_folder, cache_hash + ".orig")
    image_file_scaled = os.path.join(cache_folder, cache_hash + ".jpeg")

    with open(image_file, "w") as image: 
        image.write(image_tag.data)

    try:
        retcode = subprocess.call(["convert", "-resize", "96x96", image_file, image_file_scaled], stderr=open("/dev/null"))
        if retcode == 0:
            metadata.write("albumArt\nYes\n")
    except:
         pass #It's probably not installed. Do nothing.
        
    os.unlink(image_file)           

def write_metadata():
    extension = sys.argv[1].lower()
    music_file = sys.argv[2]
    
    tags = get_tags(extension, music_file)
    gain = get_gain(extension, music_file)
    
    if not gain:
        evaluate_gain(extension, music_file)
        gain = get_gain(extension, music_file)
        
    with open(cache_name + ".metadata", "w") as metadata: 
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

            if tags.tags:
                write_albumart(tags.tags.get('covr'), metadata)

        elif extension == '.mp3':
            add_tag(tags, metadata, "TPE1", "artistName")
            add_tag(tags, metadata, "TALB", "albumTitle")
            add_tag(tags, metadata, "TIT2", "trackName")
            add_tag(tags, metadata, "TRCK", "trackNumber")

            if tags.tags:
                images = tags.tags.getall('APIC')
                if len(images) > 0:
                    write_albumart(images[0], metadata)
    
        else:
            add_tag(tags, metadata, "artist", "artistName")
            add_tag(tags, metadata, "album", "albumTitle")
            add_tag(tags, metadata, "title", "trackName")
            add_tag(tags, metadata, "tracknumber", "trackNumber")
    
        metadata.write("cacheHash\n%s\n" % cache_hash)
        metadata.close()
        
cache_name = sys.argv[3]
(cache_folder, cache_hash) = cache_name.rsplit("/", 1)

try:
    write_metadata()
except:
    with open(cache_name + ".metadata", "w") as metadata:
        metadata.write("- Error\n")
        import traceback
        traceback.print_exc(file = metadata)
