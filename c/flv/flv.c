#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <netinet/in.h>

typedef unsigned char  u_char;
typedef unsigned int   u_int32;
typedef unsigned long  u_int64;

#pragma pack(1)
typedef struct flv_header {
    // Signature
    u_char f;
    u_char l;
    u_char v;
    u_char version;

    // TypeFlagsReserved: (typeFlags >> 3)
    // TypeFlagsAudio   : (typeFlags & 0x04) >> 2
    // TypeFlagsReserved: (typeFlags & 0x02) >> 1)
    // TypeFlagsVideo   : (typeFlags & 0x01)
    u_char typeFlags;

    // DataOffset
    u_int32 dataOffset;
} flv_header_t ;

typedef struct flv_tag_header {
    u_int32 pTagSize;

    // Reversed 2b      Filter 1b       tagType 5b      dataSize 3B
    u_int32 tagType_dataSize;

    // timeStamp 3      timeStamp_Ex 1
    // PTS  = timeStamp | timeStamp_Ex << 24
    u_int32 timeStamp_Ex;

    // streamID 3
    u_char streamID[3];
} flv_tag_header_t;
#pragma pack()

static const char* codecName[] = {
    "",
    "JPEG (currently unused)",
    "Sorenson H.263",
    "Screen video",
    "On2 VP6",
    "On2 VP6 with alpha channe",
    "Screen video version 2",
    "AVC",
};

static const char* frameType[] = {
    "",
    "keyframe (for AVC, a seekableframe)",
    "inter frame(for AVC, a non -seekable frame)",
    "disposable inter frame(H.263only)",
    "generated keyframe(reserved forserver use only)",
    "video info / command frame",
};

static const char* soundFormat[] = {
    "",
    "ADPCM",
    "MP3",
    "Linear PCM, little endian",
    "Nellymoser 16 - kHz mono",
    "Nellymoser 8 - kHz mono",
    "Nellymoser",
    "G.711 A - law logarithmic PCM",
    "G.711 mu - law logarithmic PCM",
    "reserved",
    "AAC",
    "Speex",
    "MP3 8 - Khz",
    "MP3 8 - Khz",
    "Device - specific sound",
};

static const char* soundRate[] = {
    "5.5-kHz",
    "11 - kHz",
    "22 - kHz",
    "44 - kHz",
};

static const char* soundSize[] = {
    "snd8Bit",
    "snd16Bit",
};

static const char* soundType[] = {
    "sndMono",
    "sndStereo",
};

static const char* audioObjectTypes[] = {
    "Null",
    "AAC Main",
    "AAC LC(Low Complexity)",
    "AAC SSR(Scalable Sample Rate)",
    "AAC LTP(Long Term Prediction)",
    "SBR (Spectral Band Replication)",
    "AAC Scalable",
    "TwinVQ",
    "CELP (Code Excited Linear Prediction)",
    "HXVC (Harmonic Vector eXcitation Coding)",
    "Reserved",
    "Reserved",
    "TTSI (Text-To-Speech Interface)",
    "Main Synthesis",
    "Wavetable Synthesis",
    "General MIDI",
    "Algorithmic Synthesis and Audio Effects",
    "ER (Error Resilient) AAC LC",
    "Reserved",
    "ER AAC LTP",
    "ER AAC Scalable",
    "ER TwinVQ",
    "ER BSAC (Bit-Sliced Arithmetic Coding)",
    "ER AAC LD (Low Delay)",
    "ER CELP",
    "ER HVXC",
    "ER HILN (Harmonic and Individual Lines plus Noise)",
    "ER Parametric",
    "SSC (SinuSoidal Coding)",
    "PS (Parametric Stereo)",
    "MPEG Surround",
    "(Escape value)",
    "Layer-1",
    "Layer-2",
    "Layer-3",
    "DST (Direct Stream Transfer)",
    "ALS (Audio Lossless)",
    "SLS (Scalable LosslesS)",
    "SLS non-core",
    "ER AAC ELD (Enhanced Low Delay)",
    "SMR (Symbolic Music Representation) Simple",
    "SMR Main",
    "USAC (Unified Speech and Audio Coding) (no SBR)",
    "SAOC (Spatial Audio Object Coding)",
    "LD MPEG Surround",
    "USAC",
};

const char* samplingFrequencies[] = {
    "96000 Hz",
    "88200 Hz",
    "64000 Hz",
    "48000 Hz",
    "44100 Hz",
    "32000 Hz",
    "24000 Hz",
    "22050 Hz",
    "22050 Hz",
    "12000 Hz",
    "11025 Hz",
    "8000 Hz",
    "7350 Hz",
    "Reserved",
    "Reserved",
    "frequency is written explictly",
};

const char* channelConfigurations[] = {
    "Defined in AOT Specifc Config",
    "1 channel: front-center",
    "2 channels: front-left, front-right",
    "3 channels: front-center, front-left, front-right",
    "4 channels: front-center, front-left, front-right, back-center",
    "5 channels: front-center, front-left, front-right, back-left, back-right",
    "6 channels: front-center, front-left, front-right, back-left, back-right, LFE-channel",
    "8 channels: front-center, front-left, front-right, side-left, side-right, back-left, back-right, LFE-channel",
    "Reserved",
    "Reserved",
    "Reserved",
    "Reserved",
    "Reserved",
    "Reserved",
    "Reserved",
    "Reserved",
};

const char* channel_configurations_name(u_int32 id) {
    return channelConfigurations[id];
}

const char* sampling_frequencies_name(u_int32 id) {
    return samplingFrequencies[id];
}

const char* audio_object_types_name(u_int32 id) {
    return audioObjectTypes[id];
}

const char* codec_name(u_int32 id) {
    return codecName[id];
}

const char* frame_type_name(u_int32 id) {
    return frameType[id];
}

const char* sound_format_name(u_int32 id) {
    return soundFormat[id];
}

const char* sound_rate_name(u_int32 id) {
    return soundRate[id];
}

const char* sound_size_name(u_int32 id) {
    return soundSize[id];
}

const char* sound_type_name(u_int32 id) {
    return soundType[id];
}

void usage(char *pro) {
    printf("Usage: %s [file.flv]\n", pro);
}

void mstrrev(u_char *val, u_int32 len) {

    u_int32 i, j;
    u_char temp;

    for(i = 0, j = len - 1; i <= j; i++, j--) {
        temp = val[i];
        val[i] = val[j];
        val[j] = temp;
    }
}

double str2double(u_char *buf, u_int32 index) {

    u_char    svalue[8];
    u_int32   ssize = 8;
    double    value;

    strncpy((char *)svalue, (char *)(buf + index), ssize);

    mstrrev(svalue, ssize);

    value = *(double *)svalue;

    return value;
}

u_int32 str2uint32(u_char *buf, u_int32 index) {

    u_int32 value = (buf[index] << 24 | buf[index + 1] << 16 | buf[index + 2] << 8 | buf[index + 3]);

    return value;
}

u_int32 str2uint16(u_char *buf, u_int32 index) {

    u_int32 value = (buf[index] << 8 | buf[index + 1]);

    return value;
}

void print_flv_header(flv_header_t f_header) {

    printf("\n* * * * * * * * * * * FLV HEADER * * * * * * * * * * * * \n\n");

    printf("Signature              : %c%c%c\n", f_header.f, f_header.l, f_header.v);
    printf("Version                : %d\n", f_header.version);
    printf("TypeFlagsReserved      : %d\n", f_header.typeFlags >> 3);
    printf("TypeFlagsAudio         : %d\n", (f_header.typeFlags & 0x04) >> 2);
    printf("TypeFlagsReserved      : %d\n", (f_header.typeFlags & 0x02) >> 1);
    printf("TypeFlagsVideo         : %d\n", (f_header.typeFlags & 0x01));
    printf("DataOffset(HeaderSize) : %d(%02X)\n", ntohl(f_header.dataOffset), ntohl(f_header.dataOffset));
}

static u_int32 audio_data_index = 0;
static u_int32 video_data_index = 0;
static u_int32 scriptobj_data_index = 0;
static u_int32 reserved_data_index = 0;

void print_flv_tag_header(flv_tag_header_t f_tag_header) {

    u_int32 reversed = ntohl(f_tag_header.tagType_dataSize) >> 30;
    u_int32 filter =  (ntohl(f_tag_header.tagType_dataSize) >> 29) & 0x01;
    u_int32 tagType = (ntohl(f_tag_header.tagType_dataSize) >> 24) & 0x1F;

    if (tagType == 8) {
        printf("\n (%d) AUDIODATA", audio_data_index++);
    } else if (tagType == 9) {
        printf("\n (%d) VIDEODATA", video_data_index++);
    } else if (tagType == 18) {
        printf("\n (%d) SCRIPTDATAOBJECT", scriptobj_data_index++);
    } else {
        printf("\n (%d) Reserved", reserved_data_index++);
    }

    printf(" * * * * * * * * * * * FLV TAG HEADER * * * * * * * * * * \n\n");

    printf("PreviousTagSize   : %d\n", ntohl(f_tag_header.pTagSize));
    printf("Reversed          : %d\n", reversed);
    printf("Filter            : %d\n", filter);
    printf("TagType           : %d\n", tagType);
    printf("DataSize          : %d\n", ntohl(f_tag_header.tagType_dataSize) & 0x00FFFFFF);
    printf("TimeStamp         : %d\n", ntohl(f_tag_header.timeStamp_Ex) >> 8);
    printf("TimeStampExtended : %d\n", ntohl(f_tag_header.timeStamp_Ex) & 0x000000FF);
    printf("StreamID          : %d\n", (f_tag_header.streamID[0] << 16) | (f_tag_header.streamID[1] << 8) | f_tag_header.streamID[2]);
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage(argv[0]);
        return -1;
    }

    int flv_fd, ok;

    flv_header_t my_flv_header;

    // open flv file
    flv_fd = open(argv[1], O_RDONLY);
    if (flv_fd == -1) {
        fprintf(stderr, "open file \"%s\" failed, err: %s\n", argv[1], strerror(errno));
        return -1;
    }

    // printf("\nflv header sizeof:%ld\n", sizeof(flv_header_t));

    ok = read(flv_fd, &my_flv_header, sizeof(flv_header_t));
    if (ok <= 0) {
        fprintf(stderr, "read file failed, err: %s", strerror(errno));
        return -1;
    }
    /*
     * 9B
     *
     * 0000 0000  0000 0000  0000 0000  0000 0000   0000 0                0                 0                     0                 0000 0000 0000 0000 0000 0000 0000 0000
     * [F]        [L]        [V]        [Version]   [TypeFlagsReserved0]  [TypeFlagsAudio]  [TypeFlagsReserved1]  [TypeFlagsVideo]  [DataOffset(HeaderSize)]
     */

    print_flv_header(my_flv_header);

    // lseek(flv_fd, 9, SEEK_SET);
    flv_tag_header_t my_flv_tag;

    u_int32 preTagSize = 0;

    u_int32 op = 0;
    u_int32 amf_index = 0;
    u_int32 i, j;
    u_int32 tagType;
    u_int32 dataSize = sizeof(flv_tag_header_t);

    u_char *buf;

    while (1) {

        // printf("\nflv header tag sizeof:%ld\n", sizeof(flv_tag_header_t));

        ok = read(flv_fd, &my_flv_tag, dataSize);
        if (ok < 0) {
            fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
            return -1;
        }
        if (ok == 0) {
            break;
        }

        /*
         * 15B
         *
         * 0000 0000 0000 0000 0000 0000 0000 0000  00          0         0 0000     0000 0000 0000 0000 0000 0000  0000 0000 0000 0000 0000 0000  0000 0000            0000 0000 0000 0000 0000 0000
         * [PreviousTagSize]                        [Reversed]  [Filter]  [TagType]  [DataSize]                     [TimeStamp]                    [TimestampExtended]  [StreamId]
         */

        u_int32 pTagSize = ntohl(my_flv_tag.pTagSize);
        if (preTagSize != pTagSize) {
            fprintf(stderr, "pre tag size error, currTagSize:%d, preTagSize:%d\n", pTagSize, preTagSize);
            return -1;
        }

        print_flv_tag_header(my_flv_tag);

        // tagType  = ntohl(my_flv_tag.tagType_dataSize) >> 24;
        tagType = (ntohl(my_flv_tag.tagType_dataSize) >> 24) & 0x1F;

        dataSize = ntohl(my_flv_tag.tagType_dataSize) & 0x00FFFFFF;

        preTagSize = dataSize + 11;

        buf = (u_char *)malloc((sizeof(u_char) * dataSize) + 1);

        ok = read(flv_fd, buf, dataSize);
        if (ok < 0) {
            fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
            return -1;
        }
        if (ok == 0) {
            break;
        }

        if (tagType == 8) {
            // printf("\n AUDIODATA");
            op = 0;

            /*
             * 0000             00            0             0
             * [sound format]   [sound rate]  [sound size]  [sound type]
             */
            u_int32 sound_format = buf[op] >> 4;
            u_int32 sound_rate   = buf[op] & 0x0C >> 2;
            u_int32 sound_size   = buf[op] & 0x02 >> 1;
            u_int32 sound_type   = buf[op] & 0x01;
            op++;

            printf("\tSoundFormat : %d (%s)\n", sound_format, sound_format_name(sound_format));
            printf("\tSoundRate   : %d (%s)\n", sound_rate, sound_rate_name(sound_rate));
            printf("\tSoundSize   : %d (%s)\n", sound_size, sound_size_name(sound_size));
            printf("\tSoundType   : %d (%s)\n", sound_type, sound_type_name(sound_type));

            if (sound_format == 10) {

                printf("\t\tAACAUDIODATA\n");

                u_int32 aac_pkt_type = buf[op++];
                printf("\t\t\tAACPacketType : %d\n", aac_pkt_type);

                if (aac_pkt_type == 0) {

                    printf("\t\t\t\tAudioSpecificConfig(AAC sequence header)\n");

                    /*
                     * 0000 0              000 0                      00                       0 0000 0000 0000
                     * [audioObjectType]   [samplingFrequencyIndex]   [channelConfiguration]
                     */
                    u_int32 audioObjectType = buf[op] >> 3;
                    printf("\t\t\t\t\taudioObjectType        : %d (%s)\n", audioObjectType, audio_object_types_name(audioObjectType));

                    u_int32 samplingFrequencyIndex = ((buf[op] & 0x07) << 1) | ((buf[op + 1] & 0x80) >> 7);
                    op++;
                    printf("\t\t\t\t\tsamplingFrequencyIndex : %d (%s)\n", samplingFrequencyIndex, sampling_frequencies_name(samplingFrequencyIndex));

                    u_int32 channelConfiguration = ((buf[op] & 0x78) >> 3);
                    printf("\t\t\t\t\tchannelConfiguration   : %d (%s)\n", channelConfiguration, channel_configurations_name(channelConfiguration));

                    u_int32 frameLengthFlag = ((buf[op] >> 2) & 0x01);
                    printf("\t\t\t\t\tframeLengthFlag        : %d\n", frameLengthFlag);

                    u_int32 dependsOnCoreCoder = ((buf[op] >> 1) & 0x01);
                    printf("\t\t\t\t\tdependsOnCoreCoder     : %d\n", dependsOnCoreCoder);

                    u_int32 extensionFlag = buf[op] & 0x01;
                    printf("\t\t\t\t\textensionFlag          : %d\n", extensionFlag);
                    op++;

                } else if (aac_pkt_type == 1) {
                    printf("\t\t\t\tRaw AAC frame data\n");
                }
            }

        } else if (tagType == 9) {
            // printf("\n VIDEODATA");
            op = 0;
            u_int32 frame_type = buf[op] >> 4;
            u_int32 codec_id   = buf[op] & 0x0F;
            op++;

            printf("\tFrameType : %d (%s)\n", frame_type, frame_type_name(frame_type));
            printf("\tCodecId   : %d (%s)\n", codec_id, codec_name(codec_id));

            if (codec_id == 7) {
                u_int32 audio_pkt_type = buf[op++];
                u_int32 composition_time = buf[op] << 16 | buf[op + 1] << 8 | buf[op + 2]; op += 3;

                printf("\t\tCompositionTime : %d\n", composition_time);
                printf("\t\tAVCPacketType   : %d ", audio_pkt_type);

                if (audio_pkt_type == 0) {
                    printf("(AVC sequence header)\n");
                    if (composition_time != 0 ) {
                        fprintf(stderr, "CompositionTime must be 0\n");
                        return -1;
                    }
                    /*
                     * aligned(8) class AVCDecoderConfigurationRecord {
                     *     unsigned int(8) configurationVersion = 1;
                     *     unsigned int(8) AVCProfileIndication;
                     *     unsigned int(8) profile_compatibility;
                     *     unsigned int(8) AVCLevelIndication;
                     *
                     *     bit(6) reserved = '111111'b;
                     *     unsigned int(2) lengthSizeMinusOne;
                     *
                     *     bit(3) reserved = '111'b;
                     *     unsigned int(5) numOfSequenceParameterSets;
                     *
                     *     for (i=0; i< numOfSequenceParameterSets; i++) {
                     *         unsigned int(16) sequenceParameterSetLength ;
                     *         bit(8*sequenceParameterSetLength) sequenceParameterSetNALUnit;
                     *     }
                     *     unsigned int(8) numOfPictureParameterSets;
                     *     for (i=0; i< numOfPictureParameterSets; i++) {
                     *         unsigned int(16) pictureParameterSetLength;
                     *         bit(8*pictureParameterSetLength) pictureParameterSetNALUnit;
                     *     }
                     * }
                     */
                    printf("\t\t\tAVCDecoderConfigurationRecord\n");
                    printf("\t\t\t\tconfigurationVersion : %d\n", (u_int32)(buf[op++]));
                    printf("\t\t\t\tAVCProfileIndication : %d\n", (u_int32)(buf[op++]));
                    printf("\t\t\t\tprofile_compatibility : %d\n", (u_int32)(buf[op++]));
                    printf("\t\t\t\tAVCLevelIndication : %d\n", (u_int32)(buf[op++]));
                    printf("\t\t\t\tlengthSizeMinusOne : %d\n", (u_int32)((buf[op++]) & 0x03));

                    u_int32 nsps = (u_int32)((buf[op++]) & 0x1F);
                    printf("\t\t\t\tnumOfSequenceParameterSets : %d\n", nsps);

                    for(i = 0; i < nsps; i++) {
                        u_int32 spsl = str2uint16(buf, op); op += 2;
                        printf("\t\t\t\t\t %d sequenceParameterSetLength : %d\n", i, spsl);
                        printf("\t\t\t\t\t %d sequenceParameterSetNALUnit(SPS) : ", i);
                        for(j = 0; j < spsl; j++, op++) {
                            printf("%X ", buf[op]);
                        }
                        printf("\n");
                    }

                    u_int32 npps = buf[op++];
                    printf("\t\t\t\tnumOfPictureParameterSets : %d\n", npps);
                    for(i = 0; i < npps; i++) {
                        u_int32 ppsl = str2uint16(buf, op); op += 2;
                        printf("\t\t\t\t\t %d pictureParameterSetLength : %d\n", i, ppsl);
                        printf("\t\t\t\t\t %d pictureParameterSetNALUnit(PPS) : ", i);
                        for(j = 0; j < ppsl; j++, op++) {
                            printf("%X ", buf[op]);
                        }
                        printf("\n");
                    }

                } else if (audio_pkt_type == 1) {
                    printf("(AVC NALU)\n");
                } else if (audio_pkt_type == 2) {
                    printf("(AVC end of sequence (lower level NALU sequence ender is not required or supported))\n");
                }
            }
        } else if (tagType == 18) {
            // printf("\n SCRIPTDATAOBJECT");

            for (op = 0, amf_index = 0; op < dataSize; amf_index++) {
                u_int32 amf_type = buf[op++];
                printf("\tAMF:%d type   : %d\n", amf_index, amf_type);
                if (amf_type == 0) {

                } else if (amf_type == 2) {

                    u_int32 string_size = str2uint16(buf, op); op += 2;
                    printf("\tAMF:%d string : ", amf_index);
                    for(i = 0; i < string_size; i++, op++) {
                        printf("%c", buf[op]);
                    }
                    printf("\n");

                } else if (amf_type == 8) {

                    u_int32 metadata_count = str2uint32(buf, op); op += 4;

                    printf("\tAMF:%d metadata count : %d\n", amf_index, metadata_count);

                    for(i = 0; i < metadata_count; i++) {

                        u_int32 string_size = str2uint16(buf, op); op += 2;
                        printf("\t\t%3d ", i + 1);
                        for(j = 0; j < string_size; j++, op++) {
                            printf("%c", buf[op]);
                        }
                        for(; j < 25; j++) { printf(" "); }
                        printf(" : ");

                        u_int32 string_type = buf[op++];

                        if (string_type == 0) {                                          // double

                            double value = str2double(buf, op); op += 8;
                            printf("%lf\n", value);

                        } else if (string_type == 1) {                                   // boolean

                            u_int32 flag = buf[op++];
                            printf("%d\n", flag);

                        } else if (string_type == 2) {                                   // string

                            u_int32 ssize = str2uint16(buf, op); op += 2;

                            for(j = 0; j < ssize; j++, op++) {
                                printf("%c", buf[op]);
                            }
                            printf("\n");

                        } else if (string_type == 3) {

                            u_int32 filearraylength = 0;
                            u_int32 timearraylength = 0;

                            double *fileposs = NULL;
                            double *times = NULL;

                            u_int32 ssize = str2uint16(buf, op); op += 2;
                            u_char *fs = (u_char *)(malloc(sizeof(u_char) * ssize + 1));

                            for(j = 0; j < ssize; j++, op++) {
                                fs[j] = buf[op];
                            }
                            fs[j] = '\0';

                            u_int32 sstype = buf[op++];

                            if (sstype == 10) {
                                filearraylength = str2uint32(buf, op); op += 4;
                                fileposs = (double *)(malloc(sizeof(double) * filearraylength + 1));

                                for(j = 0; j < filearraylength; j++) {
                                    u_int32 atype = buf[op++];
                                    if (atype == 0) {

                                        fileposs[j] = str2double(buf, op); op += 8;
                                        // double value = str2double(buf, op); op += 8;
                                        // printf("%lf\t", value);
                                    }
                                    // op += 8;
                                }
                            }

                            ssize = str2uint16(buf, op); op += 2;
                            u_char *ts = (u_char *)(malloc(sizeof(u_char) * ssize + 1));

                            for(j = 0; j < ssize; j++, op++) {
                                ts[j] = buf[op];
                            }
                            ts[j] = '\0';

                            sstype = buf[op++];
                            if (sstype == 10) {
                                timearraylength = str2uint32(buf, op); op += 4;

                                times = (double *)(malloc(sizeof(double) * timearraylength + 1));

                                for(j = 0; j < timearraylength; j++) {
                                    u_int32 atype = buf[op++];
                                    if (atype == 0) {

                                        times[j] = str2double(buf, op); op += 8;
                                        // double value = str2double(buf, op); op += 8;
                                        // printf("%lf\t", value);
                                    }
                                    // op += 8;
                                }
                            }

                            if (filearraylength != timearraylength) {
                                fprintf(stderr, "keyframes filepositions not equal times, fs:%d ts:%d\n", filearraylength, timearraylength);
                                return -1;
                            }

                            for(j = 0; j < filearraylength; j++) {
                                printf("\n\t\t\t %s: %lf -> %s: %lf", fs, fileposs[j], ts, times[j]);
                            }
                            printf("\n");

                            free(fs);
                            free(fileposs);
                            free(ts);
                            free(times);

                            /*
                            printf("op = %d\n", op);
                            return -1;
                            */
                        }
                    }
                }
            }
        } else {
            // printf("\n Reserved");
        }

        free(buf); buf=NULL;

        dataSize = sizeof(flv_tag_header_t);
    }


    printf("\n\nvideo_data_index: %d\n", video_data_index);
    printf("audio_data_index: %d\n", audio_data_index);
    printf("scriptobj_data_index: %d\n", scriptobj_data_index);
    printf("reserved_data_index: %d\n", reserved_data_index);

    ok = close(flv_fd);
    if (ok == -1) {
        fprintf(stderr, "close file failed, err: %s\n", strerror(errno));
        return -1;
    }
    return 0;
}
