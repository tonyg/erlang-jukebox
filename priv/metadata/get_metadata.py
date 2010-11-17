#!/usr/bin/env python

from __future__ import with_statement

import os
import sys
import subprocess
import types
from optparse import OptionParser

from mutagen.apev2 import APEv2
from mutagen.mp3 import MP3
from mutagen.flac import FLAC
from mutagen.oggvorbis import OggVorbis
from mutagen.mp4 import MP4, MP4Cover
from mutagen.mp3 import HeaderNotFoundError
from mutagen.asf import ASF

try:
    from musicbrainz2.webservice import *
    from urllib2 import urlopen, HTTPError
    musicbrainz = True
except ImportError:
    musicbrainz = False

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

class NullTags:
    class Info:
        def __init__(self):
            self.length = 0

    def __init__(self):
        self.info = NullTags.Info()
        self.tags = None

def get_tags(extension, music_file):
    try:
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
        elif extension == '.wma':
            tags = ASF(music_file)
        else: # don't have anything better to give the user
            tags = NullTags()
        return tags
    except HeaderNotFoundError, e:
        # Someone has uploaded a zero-length or badly corrupt file
        return NullTags()


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
        if opts.debug:
            print "'%s' '%s'"%(write_name,unicode(tag).encode("utf-8"))
        tags[write_name] = tag

def write_albumart(image_tag, metadata, tags):
    if not image_tag:
        if not musicbrainz: # don't have the module to do the smart tagging
            return
        if 'albumTitle' in tags:
            if 'artistName' in tags:
                rf = ReleaseFilter(title=tags['albumTitle'],artistName=tags['artistName'])
            else:
                rf = ReleaseFilter(title=tags['albumTitle'])
        elif 'artistName' in tags:
            rf = ReleaseFilter(artistName=tags['artistName'])
        else:
            return
        q = Query()
        r = q.getReleases(rf)
        if len(r) == 0:
            if opts.debug:
                print "no such release"
            return
        r = r[0].getRelease()
        if r.getAsin() != None:
            asin = r.getAsin()
            AMAZON_IMAGE_PATH = '/images/P/%s.%s.%sZZZZZZZ.jpg'
            url = "http://ec1.images-amazon.com"+ AMAZON_IMAGE_PATH % (asin, '01', 'L')
            try:
                image_tag = urlopen(url).read()
            except HTTPError,e:
                if opts.debug:
                    print "HTTP failure",e
                return
        else:
            if opts.debug:
                print "no asin"
            return

    image_file = os.path.join(cache_folder, cache_hash + ".orig")
    image_file_scaled = os.path.join(cache_folder, cache_hash + ".jpeg")

    with open(image_file, "w") as image:
        if type(image_tag) == types.ListType:
            image_tag = image_tag[0]
        if isinstance(image_tag, str):
            image.write(image_tag)
        else:
            image.write(image_tag.data)

    try:
        retcode = subprocess.call(["convert", "-resize", "96x96", image_file, image_file_scaled], stderr=open("/dev/null"))
        if retcode == 0:
            metadata.write("albumArt\nYes\n")
    except:
         pass #It's probably not installed. Do nothing.
        
    os.unlink(image_file)           

def write_metadata(extension, music_file):
    metadata = open(cache_name + ".metadata.tmp", "w")

    try:
        tags = get_tags(extension, music_file)
        gain = get_gain(extension, music_file)
        
        if not gain:
            evaluate_gain(extension, music_file)
            gain = get_gain(extension, music_file)

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
                write_albumart(tags.tags.get('covr'), metadata, tags)

        elif extension == '.mp3':
            add_tag(tags, metadata, "TPE1", "artistName")
            add_tag(tags, metadata, "TALB", "albumTitle")
            add_tag(tags, metadata, "TIT2", "trackName")
            add_tag(tags, metadata, "TRCK", "trackNumber")

            if tags.tags:
                write_albumart(tags.tags.getall('APIC'), metadata, tags)

        elif extension == ".wma":
            add_tag(tags, metadata, "Author", "artistName")
            add_tag(tags, metadata, "WM/AlbumArtist", "albumTitle")
            add_tag(tags, metadata, "Title","trackName")
            add_tag(tags, metadata, "WM/TrackNumber","trackNumber")
    
        else:
            add_tag(tags, metadata, "artist", "artistName")
            add_tag(tags, metadata, "album", "albumTitle")
            add_tag(tags, metadata, "title", "trackName")
            add_tag(tags, metadata, "tracknumber", "trackNumber")
    
        metadata.write("cacheHash\n%s\n" % cache_hash)

    except:
        if opts.debug:
            raise
        metadata.write("- Error\n")
        import traceback
        traceback.print_exc(file = metadata)

    finally:
        metadata.close()

    os.rename(cache_name + ".metadata.tmp", cache_name + ".metadata")

parser = OptionParser()
parser.add_option("-d","--debug",help="Write errors out to command line rather than the file for debugging purposes",default=False,dest="debug",action="store_true")
(opts,args) = parser.parse_args()

if len(args)!=3:
    parser.error("Usage: %s <extension of the file> <path to file> <cache file path>"%sys.argv[0])

cache_name = args[2]
(cache_folder, cache_hash) = cache_name.rsplit("/", 1)

write_metadata(args[0], args[1])
# vim: tabstop=4 shiftwidth=4 et
