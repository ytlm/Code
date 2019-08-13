#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>

#define MAX_BUFFER_LEN 8192

#define USER_DEFINE_TYPE "uuid"

typedef struct {
    uint32_t box_header_size;
    uint8_t  box_header_type[4];
} mp4_box_header_t;

void usage(char *pro) {
    printf("Usage: %s [file.mp4]\n", pro);
}

uint64_t readbuf_to_uint64(uint8_t *buf) {
    uint64_t value = 0;

    value = (((uint64_t)buf[0]) << 56) | (((uint64_t)buf[1]) << 48) | (((uint64_t)buf[2]) << 40) | (((uint64_t)buf[3]) << 32) | (buf[4] << 24) | (buf[5] << 16) | (buf[6] << 8) | (buf[7]);

    return value;
}

uint32_t readbuf_to_uint32(uint8_t *buf) {
    uint32_t value = 0;

    value = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | (buf[3]);

    return value;
}

uint32_t readbuf_to_uint24(uint8_t *buf) {
    uint32_t value = 0;

    value = (buf[0] << 16) | (buf[1] << 8) | (buf[2]);

    return value;
}

uint32_t readbuf_to_uint16(uint8_t *buf) {
    uint32_t value = 0;

    value = (buf[0] << 8) | (buf[1]);

    return value;
}

void readbuf_to_string(uint8_t *buf, uint8_t *dst, uint32_t size) {

    memcpy(dst, buf, size);

    dst[size] = '\0';
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage(argv[0]);
        return -1;
    }
    uint32_t mp4fd = -1;
    uint32_t ok = -1;
    uint32_t i = -1;
    uint32_t j = -1;
    uint32_t box_index = 0;
    uint8_t buf[MAX_BUFFER_LEN];
    uint32_t op;

    mp4fd = open(argv[1], O_RDONLY);
    if (mp4fd < 0) {
        fprintf(stderr, "open file [%s] error:%s\n", argv[1], strerror(errno));
        return -1;
    }

    while(1) {

        op = 0;
        ok = read(mp4fd, buf, 8);
        if (ok < 0) {
            fprintf(stderr, "read file error:%s\n", strerror(errno));
            return -1;
        }
        if (ok == 0) {
            printf("\n**************** DONE ************************\n");
            break;
        }
        printf("mp4 box -> %d\n", ++box_index);

        uint32_t box_header_size = readbuf_to_uint32(buf + op);
        op += 4;
        printf("\t  box header size : %u\n", box_header_size);

        uint8_t box_header_type[5];
        readbuf_to_string(buf + op, box_header_type, 4);
        op += 4;

        printf("\t  box header type : %s\n", box_header_type);

        op = 0;
        if (box_header_size == 1) {
            ok = read(mp4fd, buf, 8);
            if (ok < 0) {
                fprintf(stderr, "read file error:%s\n", strerror(errno));
                return -1;
            }
            if (ok == 0) {
                break;
            }
            box_header_size = readbuf_to_uint64(buf + op);
        }

        box_header_size -= 8;
        if (box_header_size <= 0 ) {
            continue;
        }

        op = 0;
        if (strncmp((char *)box_header_type, "ftyp", 4) == 0) {
            // printf("ftype\n");
            ok = read(mp4fd, buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file error:%s\n", strerror(errno));
                return -1;
            }
            if (ok == 0) {
                break;
            }

            uint8_t major_brand[5];
            readbuf_to_string(buf + op, major_brand, 4);
            op += 4;
            printf("\t      Major Brand : %s\n", major_brand);

            uint32_t minor_version = readbuf_to_uint32(buf + op);
            op += 4;
            printf("\t    Minor Version : %u\n", minor_version);

            printf("\tCompatible Brands : ( ");
            for(;op < box_header_size;) {
                for(i = 0; i < 4; i++, op++) {
                    printf("%c", buf[op]);
                }
                printf(" ");
            }
            printf(")\n");

            continue;

        } else if (strncmp((char *)box_header_type, "free", 4) == 0) {
            // printf("free\n");
        } else if (strncmp((char *)box_header_type, "mdat", 4) == 0) {
            // printf("mdat\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "moov", 4) == 0) {
            // printf("moov\n");
            /*
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            */
            // printf("\t");
            continue;
        } else if (strncmp((char *)box_header_type, "mvhd", 4) == 0) {
            // printf("mvhd\n");
            uint8_t *mvhd_buf = NULL;
            mvhd_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (mvhd_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }
            op = 0;

            ok = read(mp4fd, mvhd_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = mvhd_buf[op++];
            printf("\t\t              version : %d (0x%01X)\n", version, version);

            uint32_t flags = readbuf_to_uint24(mvhd_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t creation_time = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t        creation_time : %d (0x%08X)\n", creation_time, creation_time);

            uint32_t modification_time = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t    modification_time : %d (0x%08X)\n", modification_time, modification_time);

            uint32_t time_scale = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t           time_scale : %d (0x%08X)\n", time_scale, time_scale);

            uint32_t duration = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t             duration : %d (0x%08X)\n", duration, duration);

            uint32_t rate_int = readbuf_to_uint16(mvhd_buf + op);
            op += 2;
            uint32_t rate_float = readbuf_to_uint16(mvhd_buf + op);
            op += 2;

            printf("\t\t       preferred_rate : %d.%d (0x%08X)\n", rate_int, rate_float, ((rate_int << 16) | rate_float));

            uint32_t volume_int = mvhd_buf[op++];
            uint32_t volume_float = mvhd_buf[op++];

            printf("\t\t     preferred_volume : %d.%d (0x%04X)\n", volume_int, volume_float, ((volume_int << 8) | volume_float));

            printf("\t\t             Reverved : (");
            for(i = 0; i < 10; i++, op++) {
                printf(" %02X", buf[op]);
            }
            printf(" )\n");

            printf("\t\t     Matrix structure : \n");
            for(i = 0; i < 3; i++) {
                printf("\t\t                       ");
                for(j = 0; j < 3; j++) {
                    uint32_t matrix = readbuf_to_uint32(mvhd_buf + op);
                    op += 4;
                    printf(" %d (0x%08X)", matrix, matrix);
                }
                printf("\n");
            }
            printf("\n");

            uint32_t preview_time = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t         preview_time : %d (0x%08X)\n", preview_time, preview_time);

            uint32_t preview_duration = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t     preview_duration : %d (0x%08X)\n", preview_duration, preview_duration);

            uint32_t poster_time = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t          poster_time : %d (0x%08X)\n", poster_time, poster_time);

            uint32_t selection_time = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t       selection_time : %d (0x%08X)\n", selection_time, selection_time);

            uint32_t selection_duration = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t   selection_duration : %d (0x%08X)\n", selection_duration, selection_duration);
            uint32_t current_time = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t         current_time : %d (0x%08X)\n", current_time, current_time);

            uint32_t next_trick_id = readbuf_to_uint32(mvhd_buf + op);
            op += 4;
            printf("\t\t        next_trick_id : %d (0x%08X)\n", next_trick_id, next_trick_id);

            free(mvhd_buf);

            continue;

        } else if (strncmp((char *)box_header_type, "trak", 4) == 0) {
            // printf("trak\n");
            /*
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            */
            continue;
        } else if (strncmp((char *)box_header_type, "tkhd", 4) == 0) {
            // printf("udta\n");
            uint8_t *tkhd_buf = NULL;
            tkhd_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (tkhd_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }
            op = 0;

            ok = read(mp4fd, tkhd_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = tkhd_buf[op++];
            printf("\t\t              version : %d \n", version);

            uint32_t flags = readbuf_to_uint24(tkhd_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t creation_time = readbuf_to_uint32(tkhd_buf + op);
            op += 4;
            printf("\t\t        creation_time : %d (0x%08X)\n", creation_time, creation_time);

            uint32_t modification_time = readbuf_to_uint32(tkhd_buf + op);
            op += 4;
            printf("\t\t    modification_time : %d (0x%08X)\n", modification_time, modification_time);

            uint32_t track_id = readbuf_to_uint32(tkhd_buf + op);
            op += 4;
            printf("\t\t             track_id : %d (0x%08X)\n", track_id, track_id);

            // reverved
            op += 4;

            uint32_t duration = readbuf_to_uint32(tkhd_buf + op);
            op += 4;
            printf("\t\t             duration : %d (0x%08X)\n", duration, duration);

            // reverved
            op += 8;

            uint32_t layer = readbuf_to_uint16(tkhd_buf + op);
            op += 2;
            printf("\t\t                layer : %d (0x%04X)\n", layer, layer);

            uint32_t alternate_group = readbuf_to_uint16(tkhd_buf + op);
            op += 2;
            printf("\t\t      alternate_group : %d (0x%04X)\n", alternate_group, alternate_group);

            uint32_t volume_int = tkhd_buf[op++];
            uint32_t volume_float = tkhd_buf[op++];

            printf("\t\t               volume : %d.%d (0x%04X)\n", volume_int, volume_float, ((volume_int << 8) | volume_float));

            // reverved
            op += 2;

            printf("\t\t     Matrix structure : \n");
            for(i = 0; i < 3; i++) {
                printf("\t\t                       ");
                for(j = 0; j < 3; j++) {
                    uint32_t matrix = readbuf_to_uint32(tkhd_buf + op);
                    op += 4;
                    printf(" %d (0x%08X)", matrix, matrix);
                }
                printf("\n");
            }
            printf("\n");

            uint32_t track_width_int = readbuf_to_uint16(tkhd_buf + op);
            op += 2;
            uint32_t track_width_float = readbuf_to_uint16(tkhd_buf + op);
            op += 2;
            printf("\t\t          track_width : %d.%d (0x%08X)\n", track_width_int, track_width_float, ((track_width_int << 16) | track_width_float));

            uint32_t track_height_int = readbuf_to_uint16(tkhd_buf + op);
            op += 2;
            uint32_t track_height_float = readbuf_to_uint16(tkhd_buf + op);
            op += 2;
            printf("\t\t         track_height : %d.%d (0x%08X)\n", track_height_int, track_height_float, ((track_height_int << 16) | track_height_float));

            free(tkhd_buf);

            continue;

        } else if (strncmp((char *)box_header_type, "edts", 4) == 0) {
            // printf("edts\n");
            continue;
        } else if (strncmp((char *)box_header_type, "elst", 4) == 0) {
            uint8_t *elst_buf = NULL;
            elst_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (elst_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }
            op = 0;

            ok = read(mp4fd, elst_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = elst_buf[op++];
            printf("\t\t              version : %d\n", version);

            uint32_t flags = readbuf_to_uint24(elst_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t number_or_entries = readbuf_to_uint32(elst_buf + op);
            op += 4;
            printf("\t\t    number_or_entries : %d (0x%08X)\n", number_or_entries, number_or_entries);
            printf("\n");
            for(i = 0; i < number_or_entries; i++) {

                uint32_t track_duration = readbuf_to_uint32(elst_buf + op);
                op += 4;
                printf("\t\t\t  track_duration : %d (0x%08X)\n", track_duration, track_duration);

                uint32_t media_time = readbuf_to_uint32(elst_buf + op);
                op += 4;
                printf("\t\t\t      media_time : %d (0x%08X)\n", media_time, media_time);

                uint32_t media_rate = readbuf_to_uint32(elst_buf + op);
                op += 4;
                printf("\t\t\t      media_rate : %d (0x%08X)\n", media_rate, media_rate);

                printf("\n");
            }
            printf("\n");

            // printf("op : %d\n", op);

            free(elst_buf);

            continue;

        } else if (strncmp((char *)box_header_type, "mdia", 4) == 0) {
            // printf("mdia\n");
            continue;
        } else if (strncmp((char *)box_header_type, "mdhd", 4) == 0) {
            // printf("mdhd\n");
            uint8_t *mdhd_buf = NULL;
            mdhd_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (mdhd_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }
            op = 0;

            ok = read(mp4fd, mdhd_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = mdhd_buf[op++];
            printf("\t\t              version : %d \n", version);

            uint32_t flags = readbuf_to_uint24(mdhd_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t creation_time = readbuf_to_uint32(mdhd_buf + op);
            op += 4;
            printf("\t\t        creation_time : %d (0x%08X)\n", creation_time, creation_time);

            uint32_t modification_time = readbuf_to_uint32(mdhd_buf + op);
            op += 4;
            printf("\t\t    modification_time : %d (0x%08X)\n", modification_time, modification_time);

            uint32_t time_scale = readbuf_to_uint32(mdhd_buf + op);
            op += 4;
            printf("\t\t           time_scale : %d (0x%08X)\n", time_scale, time_scale);

            uint32_t duration = readbuf_to_uint32(mdhd_buf + op);
            op += 4;
            printf("\t\t             duration : %d (0x%08X)\n", duration, duration);

            uint32_t language = readbuf_to_uint16(mdhd_buf + op);
            op += 2;
            printf("\t\t             language : %d (0x%08X)\n", language, language);

            uint32_t quality = readbuf_to_uint16(mdhd_buf + op);
            op += 2;
            printf("\t\t              quality : %d (0x%08X)\n", quality, quality);

            // pre-defined
            // op += 2;

            printf("op : %d size : %d\n", op, box_header_size);

            free(mdhd_buf);

            continue;

        } else if (strncmp((char *)box_header_type, "hdlr", 4) == 0) {
            // printf("hdlr\n");
            uint8_t *hdlr_buf = NULL;
            hdlr_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (hdlr_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }
            op = 0;

            ok = read(mp4fd, hdlr_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = hdlr_buf[op++];
            printf("\t\t              version : %d \n", version);

            uint32_t flags = readbuf_to_uint24(hdlr_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint8_t component_type[5];
            readbuf_to_string(hdlr_buf + op, component_type, 4);
            op += 4;
            printf("\t\t       component_type : %s\n", component_type);

            uint8_t component_subtype[5];
            readbuf_to_string(hdlr_buf + op, component_subtype, 4);
            op += 4;
            printf("\t\t    component_subtype : %s\n", component_subtype);

            // component manufacturer       default 0
            op += 4;

            // component flags              default 0
            op += 4;

            // component flags mask         default 0
            op += 4;

            printf("\t\t       component name : ");
            for(; op < box_header_size; op++) {
                printf("%c", hdlr_buf[op]);
            }
            printf("\n");

            free(hdlr_buf);

            continue;

        } else if (strncmp((char *)box_header_type, "minf", 4) == 0) {
            // printf("minf\n");
            /*
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            */
            continue;
        } else if (strncmp((char *)box_header_type, "vmhd", 4) == 0) {
            // printf("vmhd\n");
            uint8_t *vmhd_buf = NULL;
            vmhd_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (vmhd_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }

            ok = read(mp4fd, vmhd_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = vmhd_buf[op++];
            printf("\t\t              version : %d\n", version);

            uint32_t flags = readbuf_to_uint24(vmhd_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t graphics_mode = readbuf_to_uint16(vmhd_buf + op);
            op += 2;
            printf("\t\t        graphics_mode : %d (0x%04X)\n", graphics_mode, graphics_mode);

            printf("\t\t              opcolor : (");
            for(i = 0; i < 3; i++) {
                uint32_t opcolor = readbuf_to_uint16(vmhd_buf + op);
                op += 2;
                printf(" %d (0x%04X)", opcolor, opcolor);
            }
            printf(" )\n");

            free(vmhd_buf);

            continue;
        } else if (strncmp((char *)box_header_type, "dinf", 4) == 0) {
            // printf("dinf\n");
            /*
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            */
            continue;
        } else if (strncmp((char *)box_header_type, "dref", 4) == 0) {
            // printf("dref\n");
            uint8_t *dref_buf = NULL;
            dref_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (dref_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }

            ok = read(mp4fd, dref_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = dref_buf[op++];
            printf("\t\t              version : %d\n", version);

            uint32_t flags = readbuf_to_uint24(dref_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t data_references_num = readbuf_to_uint32(dref_buf + op);
            op += 4;
            printf("\t\t  data_references_num : %d (0x%08X)\n", data_references_num, data_references_num);

            printf("\t\t       data reference : \n");
            for(i = 0; i < data_references_num; i++) {

                uint32_t data_ref_size = readbuf_to_uint32(dref_buf + op);
                op += 4;
                printf("\t\t\t      data ref size : %d\n", data_ref_size);

                uint8_t data_ref_type[5];
                readbuf_to_string(dref_buf + op, data_ref_type, 4);
                op += 4;
                printf("\t\t\t      data ref type : %s\n", data_ref_type);

                uint8_t data_ref_version = dref_buf[op++];
                printf("\t\t\t   data ref version : %d\n", data_ref_version);

                uint32_t data_ref_flags = readbuf_to_uint24(dref_buf + op);
                op += 3;
                printf("\t\t\t     data ref flags : %d (0x%06X)\n", data_ref_flags, data_ref_flags);

               data_ref_size -= 12;

               if (data_ref_size > 0) {
                    printf("\t\t\t           data ref : ");
                   for(j = 0; j < data_ref_size; j++, op++) {
                       printf("%c",dref_buf[op]);
                   }
               }

                printf("\n");
            }

            continue;
        } else if (strncmp((char *)box_header_type, "stbl", 4) == 0) {
            // printf("stbl\n");
            continue;
        } else if (strncmp((char *)box_header_type, "stsd", 4) == 0) {
            // printf("stsd\n");
            uint8_t *stsd_buf = NULL;
            stsd_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (stsd_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }

            ok = read(mp4fd, stsd_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = stsd_buf[op++];
            printf("\t\t              version : %d\n", version);

            uint32_t flags = readbuf_to_uint24(stsd_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t sample_des_num = readbuf_to_uint32(stsd_buf + op);
            op += 4;
            printf("\t\t       sample des num : %d (0x%08X)\n", sample_des_num, sample_des_num);

            printf("\t\t   sample description : \n");
            for(i = 0; i < sample_des_num; i++) {
                uint32_t sample_des_size = readbuf_to_uint32(stsd_buf + op);
                op += 4;
                printf("\t\t\t      sample des size : %d\n", sample_des_size);

                uint8_t sample_des_type[5];
                readbuf_to_string(stsd_buf + op, sample_des_type, 4);
                op += 4;
                printf("\t\t\t      sample des type : %s\n", sample_des_type);

                // reserved
                op += 6;

                uint32_t sample_des_index = readbuf_to_uint16(stsd_buf + op);
                op += 2;
                printf("\t\t\t     sample des index : %d\n", sample_des_index);

                printf("\n");
            }

            printf("op : %d, size : %d\n", op, box_header_size);

            // continue;
        } else if (strncmp((char *)box_header_type, "stts", 4) == 0) {
            // printf("stts\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "stss", 4) == 0) {
            // printf("stss\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "ctts", 4) == 0) {
            // printf("ctts\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "stsc", 4) == 0) {
            // printf("stsc\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "stsz", 4) == 0) {
            // printf("stsz\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "stco", 4) == 0) {
            // printf("stco\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "smhd", 4) == 0) {
            // printf("smhd\n");
            uint8_t *smhd_buf = NULL;
            smhd_buf = (uint8_t *)malloc(sizeof(uint8_t) * (box_header_size + 1));
            if (smhd_buf == NULL) {
                fprintf(stderr, "malloc failed, err: %s\n", strerror(errno));
                return -1;
            }

            ok = read(mp4fd, smhd_buf, box_header_size);
            if (ok < 0) {
                fprintf(stderr, "read file failed, err: %s\n", strerror(errno));
                return -1;
            }

            uint8_t version = smhd_buf[op++];
            printf("\t\t              version : %d\n", version);

            uint32_t flags = readbuf_to_uint24(smhd_buf + op);
            op += 3;
            printf("\t\t                flags : %d (0x%06X)\n", flags, flags);

            uint32_t balance_int = smhd_buf[op++];
            uint32_t balance_float = smhd_buf[op++];
            printf("\t\t              balance : %d.%d (0x%04X)\n", balance_int, balance_float, ((balance_int << 8) | balance_float));

            // reserved
            op += 2;

            continue;
        } else if (strncmp((char *)box_header_type, "sgpd", 4) == 0) {
            // printf("sgpd\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "sbgp", 4) == 0) {
            // printf("sbgp\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        } else if (strncmp((char *)box_header_type, "udta", 4) == 0) {
            // printf("udta\n");
            /*
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            */
            continue;
        } else if (strncmp((char *)box_header_type, "meta", 4) == 0) {
            // printf("meta\n");
            off_t sk = lseek(mp4fd, box_header_size, SEEK_CUR);
            if (sk < 0) {
                fprintf(stderr, "lseek file failed: %s\n", strerror(errno));
                return -1;
            }
            continue;
        }

        break;

    }
    close(mp4fd);

    return 0;
}
