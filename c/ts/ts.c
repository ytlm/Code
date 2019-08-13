#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <netinet/in.h>


#define PAT_PID     0x0000
#define SDT_PID     0x0011

#define TS_PACKET_SIZE 188

#define SYNC_BYTE 0x47
#define PEX_START_CODE 0x000001

#define MAX_STREAM_PER_PROGRAM 2

#define NB_PID_MAX 8192
#define TYPE_MAX_LEN 8192

typedef struct {

    uint8_t    sync_byte;
    uint8_t    transport_error_indicator:1;
    uint8_t    payload_unit_start_indicator:1;
    uint8_t    transport_priority:1;
    uint32_t   pid:13;
    uint8_t    transport_scrambling_control:2;
    uint8_t    adaptation_field_control:2;
    uint8_t    continuity_counter:4;
} ts_packet_header_t;

typedef uint32_t (* pid_filter)(uint8_t *buf, uint32_t op, uint32_t payload_length, void *data) ;

typedef struct {
    uint8_t         stream_str[TYPE_MAX_LEN];
    uint8_t         pes_flag;
    uint32_t        stream_type;
    uint32_t        stream_id;
    pid_filter      filter;

} ts_stream_t;


typedef struct {

    ts_stream_t *pids[NB_PID_MAX];

} ts_filter_t;


void usage(char *pro) {
    printf("Usage: %s [file.ts]\n", pro);
}

static char *tsc_s[] = {
    "Not scrambled.",
    "Reserved for future use.",
    "Scrambled with even key.",
    "Scrambled with odd key",
};

static char *afc_s[] = {
    "Reserved for future use.",
    "No adaptation field, Payload only.",
    "Adaptation field only, No payload.",
    "Adaptation field followed by Payload.",
};

char* TSC(uint8_t tscn) {
    return tsc_s[tscn];
}

char* AFC(uint8_t afcn) {
    return afc_s[afcn];
}

uint32_t pes_call(uint8_t *buf, uint32_t op, uint32_t payload_length, void *data) {

    // ts_filter_t *ts = (ts_filter_t *)data;

    uint32_t index = 0;
    uint32_t i = 0;

    uint32_t start_code = buf[op + index] << 16 | buf[op + index + 1] << 8 | buf[op + index + 2];
    if ((start_code & PEX_START_CODE) != start_code) {
        printf("not pes start code\n");
        return index;
    }
    index += 3;

    uint32_t stream_id = buf[op + index];
    index += 1;

    uint32_t pes_packet_length = buf[op + index] << 8 | buf[op + index + 1];
    index += 2;

    printf("\t\t\t                  Start Code : %4d (0x%04X)\n", start_code, start_code);
    printf("\t\t\t                   Stream ID : %4d (0x%04X)\n", stream_id, stream_id);
    printf("\t\t\t           Pes Packet Length : %4d (0x%04X)\n", pes_packet_length, pes_packet_length);

    uint8_t res1                     = (buf[op + index] & 0xC0) >> 6;
    uint8_t pes_scrambling_control   = (buf[op + index] & 0x30) >> 4;
    uint8_t pes_priority             = (buf[op + index] & 0x08) >> 3;
    uint8_t data_alignment_indicator = (buf[op + index] & 0x04) >> 2;
    uint8_t copyright                = (buf[op + index] & 0x02) >> 1;
    uint8_t original_or_copy         = (buf[op + index] & 0x01);
    index += 1;

    printf("\t\t\t                    Reserved : %d (0x%01X)\n", res1, res1);
    printf("\t\t\t      Pes Scrambling Control : %d (0x%01X)\n", pes_scrambling_control, pes_scrambling_control);
    printf("\t\t\t                Pes Priority : %d (0x%01X)\n", pes_priority, pes_priority);
    printf("\t\t\t    Data Alignment Indicator : %d (0x%01X)\n", data_alignment_indicator, data_alignment_indicator);
    printf("\t\t\t                   CopyRight : %d (0x%01X)\n", copyright, copyright);
    printf("\t\t\t            Original or Copy : %d (0x%01X)\n", original_or_copy, original_or_copy);

    uint8_t pts_dts_flag              = (buf[op + index] & 0xC0) >> 6;
    uint8_t escr_flag                 = (buf[op + index] & 0x20) >> 5;
    uint8_t es_rate_flag              = (buf[op + index] & 0x10) >> 4;
    uint8_t dsm_trick_mode_flag       = (buf[op + index] & 0x08) >> 3;
    uint8_t additional_copy_info_flag = (buf[op + index] & 0x04) >> 2;
    uint8_t pes_crc_flag              = (buf[op + index] & 0x02) >> 1;
    uint8_t pes_extension_flag        = (buf[op + index] & 0x01);
    index += 1;

    printf("\t\t\t               PTS DTS Flags : %d (0x%01X)\n", pts_dts_flag, pts_dts_flag);
    printf("\t\t\t                   ESCR Flag : %d (0x%01X)\n", escr_flag, escr_flag);
    printf("\t\t\t                ES Rage Flag : %d (0x%01X)\n", es_rate_flag, es_rate_flag);
    printf("\t\t\t         DSM Trick Mode Flag : %d (0x%01X)\n", dsm_trick_mode_flag, dsm_trick_mode_flag);
    printf("\t\t\t   Additional Copy Info Flag : %d (0x%01X)\n", additional_copy_info_flag, additional_copy_info_flag);
    printf("\t\t\t                PES CRC Flag : %d (0x%01X)\n", pes_crc_flag, pes_crc_flag);
    printf("\t\t\t          PES Extension Flag : %d (0x%01X)\n", pes_extension_flag, pes_extension_flag);

    uint32_t pes_header_data_length = buf[op + index];
    index += 1;

    printf("\t\t\t      PES Header Data Length : %d (0x%01X)\n", pes_header_data_length, pes_header_data_length);

    // 90Hz
    if (pts_dts_flag & 0x2) {
        uint64_t pts = ((((uint64_t)(buf[op + index] & 0x0E)) >> 1) << 30) | (buf[op + index + 1] << 22) | (((buf[op + index + 2] & 0xFE) >> 1) << 15) | (buf[op + index + 3] << 7) | ((buf[op + index + 4] & 0xFE) >> 1);
        // uint64_t pts = (((uint64_t)(buf[op + index] & 0x0E)) << 29) | (buf[op + index + 1] << 22) | ((buf[op + index + 2] & 0xFE) << 14) | (buf[op + index + 3] << 7) | ((buf[op + index + 4] & 0xFE) >> 1);
        index += 5;
        printf("\t\t\t                         PTS : %lu %.4f\n", pts, pts / 90000.0);
    }
    if (pts_dts_flag & 0x1) {
        uint64_t dts = ((((uint64_t)(buf[op + index] & 0x0E)) >> 1) << 30) | (buf[op + index + 1] << 22) | (((buf[op + index + 2] & 0xFE) >> 1) << 15) | (buf[op + index + 3] << 7) | ((buf[op + index + 4] & 0xFE) >> 1);
        // uint64_t dts = (((uint64_t)(buf[op + index] & 0x0E)) << 29) | (buf[op + index + 1] << 22) | ((buf[op + index + 2] & 0xFE) << 14) | (buf[op + index + 3] << 7) | ((buf[op + index + 4] & 0xFE) >> 1);
        index += 5;
        printf("\t\t\t                         DTS : %lu %.4f\n", dts, dts / 90000.0);
    }

    if (escr_flag) {
        uint64_t escr = ((((uint64_t)(buf[op + index] & 0x30)) >> 4) << 30) | \
                        ((((uint64_t)(buf[op + index] & 0x08)) >> 3) << 29) | \
                        (((uint64_t)(buf[op + index] & 0x03)) << 28) |        \
                        (((uint64_t)buf[op + index + 1]) << 20) |             \
                        (((buf[op + index + 2] & 0xF0) >> 4) << 16) |         \
                        (((buf[op + index + 2] & 0x08) >> 3) << 15) |         \
                        ((buf[op + index + 2] & 0x03) << 13) |                \
                        (buf[op + index + 3] << 5) |                          \
                        (((buf[op + index + 4] & 0xF0) >> 4 ) << 1) |         \
                        ((buf[op + index + 4] & 0x08) >> 3);

        uint32_t escr_ext = ((buf[op + index + 4] & 0x03) << 7) | ((buf[op + index + 5] & 0xFE) >> 1);
        index += 6;
        printf("\t\t\t                        ESCR : %lu\n", escr);
        printf("\t\t\t                    ESCR Ext : %u\n", escr_ext);
    }

    // 50 bytes / second
    if (es_rate_flag) {
        uint32_t es_rate = ((buf[op + index] & 0x7F) << 15) | (buf[op + index + 1] << 7) | ((buf[op + index + 2] & 0xFE) >> 1);
        index += 3;
        printf("\t\t\t                     ES Rate : %u\n", es_rate);
    }

    // DMS trick mode - not used by DVD
    if (dsm_trick_mode_flag) {
        uint8_t trick_mode_control = (buf[op + index] & 0xE0) >> 5;

        if (trick_mode_control == 0 || trick_mode_control == 3) {

            uint8_t field_id             = (buf[op + index] & 0x18) >> 3;
            uint8_t intra_slice_refresh  = (buf[op + index] & 0x04) >> 2;
            uint8_t frequency_truncation = (buf[op + index] & 0x03);

            printf("\t\t\t                    Field ID : %u\n", field_id);
            printf("\t\t\t         Intra Slice Refresh : %u\n", intra_slice_refresh);
            printf("\t\t\t        Frequency Truncation : %u\n", frequency_truncation);

        } else if (trick_mode_control == 1 || trick_mode_control == 4) {

            uint8_t rep_cntrl = buf[op + index] & 0x1F;
            printf("\t\t\t                   Rep Cntrl : %u\n", rep_cntrl);

        } else if (trick_mode_control == 2) {

            uint8_t field_id             = (buf[op + index] & 0x18) >> 3;
            uint8_t res2                 = buf[op + index] & 0x07;

            printf("\t\t\t                    Field ID : %u\n", field_id);
            printf("\t\t\t                    Reserved : %u\n", res2);

        } else {

            uint8_t res3                 = buf[op + index] & 0x1F;

            printf("\t\t\t                    Reserved : %u\n", res3);
        }
        index += 1;
    }

    if (additional_copy_info_flag) {
        uint32_t additional_copy_info = buf[op + index] & 0x7F;
        index += 1;
        printf("\t\t\t        Additional Copy Info : %u\n", additional_copy_info);
    }

    if (pes_crc_flag) {
        uint32_t pre_pes_crc = (buf[op + index] << 8) | (buf[op + index + 1]);
        index += 2;
        printf("\t\t\t     Previous PES packet CRC : %u\n", pre_pes_crc);
    }

    if (pes_extension_flag) {
        uint8_t pes_private_data_flag = (buf[op + index] & 0x80) >> 7;
        uint8_t packheader_field_flag = (buf[op + index] & 0x40) >> 6;
        uint8_t program_packet_sequence_counter_flag = (buf[op + index] & 0x20) >> 5;
        uint8_t p_std_buffer_flag     = (buf[op + index] & 0x10) >> 4;
        uint8_t res2 = (buf[op + index] & 0x0E) >> 1;
        uint8_t pes_extension_flag2 = (buf[op + index] & 0x01);

        printf("\t\t\t       Pes Private Data Flag : %d\n", pes_private_data_flag);
        if (pes_private_data_flag) {
            printf("\n\t\t\t            PES Private Data : (");
            for(i = 0; i < 16; i++, index++) {
                printf(" %X", buf[op + index]);
            }
            printf(" )\n");
        }

        printf("\t\t\t      Pack Header Field Flag : %d\n", packheader_field_flag);
        if (packheader_field_flag) {
            uint32_t pack_field_length = buf[op + index];
            index += 1;
            printf("\t\t\t   Pack Header Field Length : %d\n", pack_field_length);

            printf("\t\t\t          Pack Header Field : (");
            for(i = 0; i < pack_field_length; i++, index++) {
                printf(" %X", buf[op + index]);
            }
            printf(" )\n");
        }

        uint8_t original_stuffing_length = 0;

        printf("\t\t\tProgram Packet Sequence Counter Flag : %d\n", packheader_field_flag);
        if (program_packet_sequence_counter_flag) {
            uint32_t packet_sequence_counter = buf[op + index] & 0x7F;
            index += 1;

            printf("\t\t\t    Packet Sequence Counter : %d\n", packet_sequence_counter);

            uint8_t mpeg1_mpeg2_identifier = (buf[op + index] & 0x40) >> 6;
            original_stuffing_length = buf[op+ index] & 0x3F;
            index += 1;

            printf("\t\t\t     Mpeg1 Mpeg2 Identifier : %d\n", mpeg1_mpeg2_identifier);
            printf("\t\t\t   Original Stuffing Length : %d\n", original_stuffing_length);
        }

        printf("\t\t\t           P-STD Buffer Flag : %d\n", packheader_field_flag);
        if (p_std_buffer_flag) {
            uint8_t p_std_buffer_scale = (buf[op + index] & 0x20) >> 5;
            uint32_t p_std_buffer_size = ((buf[op + index] & 0x1F) << 8) | buf[op + index + 1];
            index += 2;

            printf("\t\t\t          P-STD Buffer Scale : %d\n", p_std_buffer_scale);
            printf("\t\t\t           P-STD Buffer Size : %d\n", p_std_buffer_size);
        }

        printf("\t\t\t                    Reserved : %d\n", res2);
        printf("\t\t\t         PES Extension Flag2 : %d\n", packheader_field_flag);
        if (pes_extension_flag2) {
            uint32_t pes_extension_field_length = buf[op + index] & 0x7F;
            index += 1;
            printf("\t\t\t  PES Extension Field Length : %d\n", pes_extension_field_length);

            printf("\t\t\t                    Reserved : (");
            for(i = 0; i < pes_extension_field_length; i++, index++) {
                printf(" %X", buf[op + index]);
            }
            printf(" )\n");
        }

        printf("\t\t\t              Stuffing bytes : (");
        for(i = 0; i < original_stuffing_length; i++, index++) {
            printf(" %X", buf[op + index]);
        }
        printf(" )\n");
    }

    return index;
}

void init_pes(ts_filter_t *ts, uint32_t stream_id, uint32_t stream_type){

    ts->pids[stream_id] = (ts_stream_t *)malloc(sizeof(ts_stream_t));
    memset(ts->pids[stream_id], 0, sizeof(ts_stream_t));

    ts->pids[stream_id]->stream_id = stream_id;
    ts->pids[stream_id]->stream_type = stream_type;
    strcpy((char *)ts->pids[stream_id]->stream_str, "PES.");
    ts->pids[stream_id]->filter = pes_call;
    ts->pids[stream_id]->pes_flag = 1;

}

uint32_t pmt_call(uint8_t *buf, uint32_t op, uint32_t payload_length, void *data) {

    payload_length = payload_length - 5 - 4;

    ts_filter_t *ts = (ts_filter_t *)data;

    uint32_t index = 0;

    uint32_t res1    = (buf[op + index] & 0xE0) >> 5;
    uint32_t pcr_pid = (buf[op + index] & 0x1F) << 8 | buf[op + index + 1];
    index += 2;

    uint32_t res2    = (buf[op + index] & 0xF0) >> 4;
    uint32_t program_info_length = (buf[op + index] & 0x0F ) << 8| buf[op + index + 1];
    index += 2;

    printf("\t\t\t                    Reserved : %4d (0x%01X)\n", res1, res1);
    printf("\t\t\t                     PCR PID : %4d (0x%04X)\n", pcr_pid, pcr_pid);
    printf("\t\t\t                    Reserved : %4d (0x%01X)\n", res2, res2);
    printf("\t\t\t         Program Info Length : %4d (0x%04X)\n", program_info_length, program_info_length);

    if (program_info_length > 0 ) {
        // TODO Program Info Length
        index += program_info_length;
    }


    uint32_t stream_type, stream_id, stream_info_length;
    uint32_t res3, res4;

    for (; index < payload_length; ) {
        stream_type        = buf[op + index];                                     index += 1;
        res3               = (buf[op + index] & 0xE0) >> 5;
        stream_id          = (buf[op + index] & 0x1F) << 8 | buf[op + index + 1]; index += 2;
        res4               = (buf[op + index] & 0xF0) >> 4;
        stream_info_length = (buf[op + index] & 0x0F) << 8 | buf[op + index + 1]; index += 2;

        printf("\n");
        printf("\t\t\t                 Stream Type : %4d (0x%02X)\n", stream_type, stream_type);
        printf("\t\t\t                    Reserved : %4d (0x%01X)\n", res3, res3);
        printf("\t\t\t                  Stream PID : %4d (0x%04X)\n", stream_id, stream_id);
        printf("\t\t\t                    Reserved : %4d (0x%01X)\n", res4, res4);
        printf("\t\t\t              ES Info Length : %4d (0x%04X)\n", stream_info_length, stream_info_length);

        init_pes(ts, stream_id, stream_type);

        if (stream_info_length > 0) {
            // TODO ES Info Length
            index += stream_info_length;
        }
    }

    return index;
}

void init_pmt(ts_filter_t *ts, uint32_t pmt_id) {

    ts->pids[pmt_id] = (ts_stream_t *)malloc(sizeof(ts_stream_t));
    memset(ts->pids[pmt_id], 0, sizeof(ts_stream_t));

    ts->pids[pmt_id]->stream_id = pmt_id;
    ts->pids[pmt_id]->stream_type = pmt_id;
    strcpy((char *)ts->pids[pmt_id]->stream_str, "PMT, Program Map Table.");
    ts->pids[pmt_id]->filter = pmt_call;

}

uint32_t pat_call(uint8_t *buf, uint32_t op, uint32_t payload_length, void *data) {

    uint32_t index = 0;
    uint32_t i = 0;

    ts_filter_t *ts = (ts_filter_t *)data;

    payload_length = payload_length - 5 - 4;

    for ( ; index < payload_length; ) {
        uint32_t program_number = buf[op + index] << 8 | buf[op + index + 1];
        index += 2;

        uint32_t res1 = (buf[op + index] & 0xE0) >> 5;

        for(i = 0; i < program_number; i++) {
            uint32_t pmt_id = (buf[op + index] & 0x1F) << 8 |  buf[op + index + 1];
            index += 2;

            printf("\t\t\t              Program Number : %d (0x%04X)\n", program_number, program_number);
            printf("\t\t\t                    Reserved : %d (0x%01X)\n", res1, res1);
            printf("\t\t\t             Program Map PID : %d (0x%04X)\n", pmt_id, pmt_id);

            init_pmt(ts, pmt_id);
        }
    }

    return index;
}

uint32_t sdt_call(uint8_t *buf, uint32_t op, uint32_t payload_length, void *data) {

    uint32_t j = -1;
    uint32_t k = -1;
    uint32_t index = 0;

    // 5
    //          [Transport Stream ID] |
    //          [Reserved] | [Version Number] | [Current/Next Indicator] |
    //          [Section Number] | [Last Section Number]
    // 4
    //          [CRC]
    payload_length = payload_length - 5 - 4;

    uint32_t original_network_id = buf[op + index] << 8 | buf[op + index + 1];

    printf("\t\t\t         Original Network ID : %d (0x%04X)\n", original_network_id, original_network_id);
    if (original_network_id < 0) {
        return index;
    }
    index += 2;

    uint32_t res4 = buf[op + index];
    printf("\t\t\t                    Reserved : %d (0x%02X)\n", res4, res4);
    if (res4 < 0) {
        return index;
    }
    index += 1;

    for( ; index < payload_length;) {

        uint32_t service_id = buf[op + index] << 8 | buf[op + index + 1];
        printf("\t\t\t                  Service ID : %d (0x%04X)\n", service_id, service_id);
        if (service_id < 0) {
            break;
        }
        index += 2;

        uint32_t res3 = (buf[op + index] & 0xFC) >> 2;

        uint8_t eitsf = buf[op + index] & 0x02;
        uint8_t eitpff = buf[op + index] & 0x01;
        index += 1;
        printf("\t\t\t                    Reserved : %d (0x%02X)\n", res3, res3);
        printf("\t\t\t           EIT Schedule Flag : %d\n", eitsf);
        printf("\t\t\t  EIT Present Following Flag : %d\n", eitpff);

        uint32_t running_status  = (buf[op + index] & 0xE0) >> 5;
        uint8_t  free_ca_mode    = buf[op + index] & 0x10;
        uint32_t des_loop_length = ((buf[op + index] & 0x0F) << 8) | buf[op + index + 1];
        index += 2;

        printf("\t\t\t              Running Status : %d (0x%01X)\n", running_status, running_status);
        printf("\t\t\t                Free CA Mode : %d\n", free_ca_mode);
        printf("\t\t\t     Descriptors Loop Length : %d (0x%04X)\n", des_loop_length, des_loop_length);


        for(j = 0; j < des_loop_length;) {
            uint32_t des_tag = buf[op + index]; index += 1; j += 1;
            printf("\n\t\t\t             Description Tag : %d (0x%02X)\n", des_tag, des_tag);
            if (des_tag < 0) {
                break;
            }

            uint32_t des_length = buf[op + index]; index += 1; j += 1;
            printf("\t\t\t          Description Length : %d\n", des_length);

            uint32_t service_type;
            uint32_t plength;

            switch (des_tag) {
                case 0x48:
                    service_type = buf[op + index]; index += 1; j += 1;
                    printf("\t\t\t                Service Type : %d\n", service_type);

                    if (service_type < 0) {
                        break;
                    }

                    plength = buf[op + index]; index += 1; j += 1;

                    printf("\t\t\t       Pervice Provider Name : ");
                    for(k = 0; k < plength; k++, index++, j++) {
                        printf("%c", buf[op + index]);
                    }
                    printf("\n");

                    plength = buf[op + index]; index++; j += 1;
                    printf("\t\t\t                Service Name : ");
                    for(k = 0; k < plength; k++, index++, j++) {
                        printf("%c", buf[op + index]);
                    }
                    printf("\n");

                    break;
                default:
                    break;
            }
        }
    }

    return index;
}

void init_ts(ts_filter_t *ts) {
    // PAT
    ts->pids[PAT_PID] = (ts_stream_t *)malloc(sizeof(ts_stream_t));
    memset(ts->pids[PAT_PID], 0, sizeof(ts_stream_t));

    ts->pids[PAT_PID]->stream_id = PAT_PID;
    ts->pids[PAT_PID]->stream_type = PAT_PID;
    strcpy((char *)ts->pids[PAT_PID]->stream_str, "PAT, Program Association Table.");
    ts->pids[PAT_PID]->filter = pat_call;

    // SDT
    ts->pids[SDT_PID] = (ts_stream_t *)malloc(sizeof(ts_stream_t));
    memset(ts->pids[SDT_PID], 0, sizeof(ts_stream_t));

    ts->pids[SDT_PID]->stream_id = SDT_PID;
    ts->pids[SDT_PID]->stream_type = SDT_PID;
    strcpy((char *)ts->pids[SDT_PID]->stream_str, "SDT, Service Description Table.");
    ts->pids[SDT_PID]->filter = sdt_call;
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        usage(argv[0]);
        return -1;
    }

    uint32_t tsfd = -1;
    uint32_t ok = -1;
    uint32_t i = -1;
    uint32_t op = -1;
    uint32_t tspktindex = 0;

    uint8_t buf[TS_PACKET_SIZE + 1];

    tsfd = open(argv[1], O_RDONLY);
    if (tsfd < 0) {
        fprintf(stderr, "open file [%s] error:%s\n", argv[1], strerror(errno));
        return -1;
    }

    ts_filter_t *ts;
    ts = (ts_filter_t *)malloc(sizeof(ts_filter_t));
    if (ts == NULL) {
        fprintf(stderr, "malloc failed error:%s\n", strerror(errno));
        return -1;
    }
    memset(ts, 0, sizeof(ts_filter_t));

    init_ts(ts);

    while (1) {

        op = 0;

        ok = read(tsfd, buf, TS_PACKET_SIZE);
        if (ok < 0) {
            fprintf(stderr, "read file error:%s\n", strerror(errno));
            return -1;
        }
        if (ok == 0) {
            break;
        }

        printf("ts packet index -> %d", ++tspktindex);

        uint8_t sync_byte = buf[op++];

        uint8_t transport_error_indicator = buf[op] >> 7;

        uint8_t payload_unit_start_indicator = buf[op] >> 6 & 0x01;

        uint8_t transport_priority = buf[op] >> 5 & 0x01;

        uint32_t pid = ((buf[op] & 0x1F) << 8) | buf[op+1];

        op += 2;

        uint8_t transport_scrambling_control = buf[op] >> 6;

        uint8_t adaptation_field_control = buf[op] >> 4 & 0x03;

        uint8_t continuity_counter = buf[op++] & 0x0F;

        if ((sync_byte & SYNC_BYTE) != sync_byte) {
            fprintf(stderr, "ts packet sync byte error:%d\n", sync_byte);
            return -1;
        }

        if (ts->pids[pid]) {
            printf(" (%s)", ts->pids[pid]->stream_str);
        }
        /*
        if (pid == 0) {
            printf(" (PAT, Program Association Table.)");
        } else if (pid == 1) {
            printf(" (CAT, Conditional Access Table.)");
        } else if (pid == 17) {
            printf(" (SDT, Service Description Table.)");
        } else if (pid == pmt_id) {
            printf(" (PMT, Program Map Table.)");
        }
        */
        printf("\n\n");

        printf("\t                Sync byte : 0x%01X (%d|%c)\n", sync_byte, sync_byte, sync_byte);
        printf("\t          Transport error : %d\n", transport_error_indicator);
        printf("\t       Payload unit start : %d\n", payload_unit_start_indicator);
        printf("\t       Transport priority : %d\n", transport_priority);
        printf("\t  Packet Identifier (PID) : 0x%04X (%d)\n", pid, pid);
        printf("\t     Transport scrambling : %d (%s)\n", transport_scrambling_control,
                                                            TSC(transport_scrambling_control));
        printf("\t Adaptation field control : %d (%s)\n", adaptation_field_control,
                                                            AFC(adaptation_field_control));
        printf("\t       Continuity counter : %d\n", continuity_counter);

        if (adaptation_field_control == 2 || adaptation_field_control == 3) {
            // adaptation field
            printf("\n\t\t *** ADAPTATION FIELD ***\n");
            uint32_t afl = buf[op++];

            uint8_t  di    = buf[op] & 0x80;
            uint8_t  rai   = buf[op] & 0x40;
            uint8_t  espi  = buf[op] & 0x20;
            uint8_t  pcrf  = buf[op] & 0x10;
            uint8_t  opcrf = buf[op] & 0x08;
            uint8_t  spf   = buf[op] & 0x04;
            uint8_t  tpdf  = buf[op] & 0x02;
            uint8_t  afef  = buf[op] & 0x01;

            op++;
            printf("\t\t              Adaptation field length : %d\n", afl);
            printf("\t\t              Discontinuity indicator : %d\n", di);
            printf("\t\t              Random access indicator : %d\n", rai);
            printf("\t\t Elementary stream priority indicator : %d\n", espi);
            printf("\t\t      Adaptation field extension flag : %d\n", afef);

            printf("\t\t                             PCR flag : %d", pcrf);
            if (pcrf) {
                printf(" (");
                for(i = 0; i < 6; i++, op++) {
                    printf(" %02X", buf[op]);
                }
                printf(" )");
            }
            printf("\n");

            printf("\t\t                            OPCR flag : %d", opcrf);
            if (opcrf) {
                printf(" (");
                for(i = 0; i < 6; i++, op++) {
                    printf(" %02X", buf[op]);
                }
                printf(" )");
            }
            printf("\n");

            printf("\t\t                  Splicing point flag : %d", spf);
            if (spf) {
                int32_t sc = buf[op++];
                printf(" ( %d )", sc);
            }
            printf("\n");

            printf("\t\t          Transport private data flag : %d", tpdf);
            if (tpdf) {
            }
            printf("\n");

            printf("\t\t      Adaptation field extension flag : %d\n", afef);
            if (afef) {
                uint32_t ael  = buf[op++];
                uint8_t  ltwf = buf[op] & 0x80;
                uint8_t  prf  = buf[op] & 0x40;
                uint8_t  sspf = buf[op] & 0x20;
                uint32_t aefr = buf[op++] & 0x1F;

                printf("\t\t      Adaptation extension length : %d\n", ael);
                printf("\t\t     Legal time window (LTW) flag : %d\n", afef);
                if (ltwf) {
                    uint32_t ltwvf = buf[op] & 0x80;
                    uint32_t ltwoffset = (buf[op]& 0x7F) << 8 | buf[op + 1];
                    op++;
                    printf("\t\t      LTW valid flag : %d\n", ltwvf);
                    printf("\t\t      LTW offset : %d\n", ltwoffset);
                }

                printf("\t\t     Piecewise rate flag : %d\n", prf);
                if (prf) {
                    uint32_t prfr = buf[op] & 0xC0;
                    uint32_t pr   = ((buf[op] & 0x3F) << 16) | (buf[op + 1] << 8) | buf[op + 2];
                    op += 3;

                    printf("\t\t          Reserved : %d\n", prfr);
                    printf("\t\t    Piecewise rate : %d\n", pr);
                }

                printf("\t\t     Seamless splice fla g:%d\n", prf);
                if (sspf) {
                    uint32_t splice_type = buf[op] & 0xF0;
                    uint64_t dts_next_access_unit = (((uint64_t)(buf[op] & 0x0E)) << 32) | (buf[op + 1] << 24) |  ((buf[op + 2] & 0xFE) << 16) | (buf[op + 3] << 8) | (buf[op + 4] & 0xFE);

                    op += 5;

                    printf("\t\t           Splice type : %d\n", splice_type);
                    printf("\t\t  DTS next access unit : %lu\n", dts_next_access_unit);
                }

                printf("\t\t     Reserved: %d\n", aefr);
            }
        }
        if (adaptation_field_control == 1 || adaptation_field_control == 3)  {
            // payload
            printf("\n\t\t *** PAYLOAD FIELD ***\n");

            if (ts->pids[pid]) {
                uint32_t payload_length = 0;

                if (ts->pids[pid]->pes_flag == 0) {
                    uint32_t pointer = buf[op++];
                    printf("\t\t\t                     Pointer : %d\n", pointer);

                    // printf("op: %d\n", op);

                    uint32_t table_id       = buf[op++];
                    uint8_t  si             = (buf[op] & 0x80) >> 7;
                    uint32_t res1           = (buf[op] & 0x70) >> 4;

                    payload_length = ((buf[op] & 0x0F) << 8) | buf[op + 1];
                    op += 2;

                    printf("\t\t\t                    Table ID : %d (0x%02X)\n", table_id, table_id);
                    printf("\t\t\t            Syntax indicator : %d\n", si);
                    printf("\t\t\t                    Reserved : %d\n", res1);
                    printf("\t\t\t                      Length : %d\n", payload_length);

                    uint32_t stream_id = buf[op] << 8 | buf[op + 1];
                    op += 2;
                    printf("\t\t\t         Transport Stream ID : %d (0x%04X)\n", stream_id, stream_id);

                    uint32_t res2           = (buf[op] & 0xC0) >> 6;
                    uint32_t version_number = (buf[op] & 0x3E) >> 1;
                    uint32_t c_n_i          = buf[op] & 0x01;
                    op += 1;
                    printf("\t\t\t                    Reserved : %d\n", res2);
                    printf("\t\t\t              Version Number : %d\n", version_number);
                    printf("\t\t\t      Current/Next Indicator : %d\n", c_n_i);

                    uint32_t section_number      = buf[op++];
                    uint32_t last_section_number = buf[op++];

                    printf("\t\t\t              Section Number : %d\n", section_number);
                    printf("\t\t\t         Last Section Number : %d\n", last_section_number);
                    printf("\n");
                }

                op = op + ts->pids[pid]->filter(buf, op, payload_length, (void *)ts);
            }

            /*
            if (pid == 0) {
                op = op + pat_call(buf, op, payload_length, NULL);
            } else if (pid == 17) {
                op = op + sdt_call(buf, op, payload_length, NULL);
            } else if (pid == pmt_id) {
                op = op + pmt_call(buf, op, payload_length, NULL);
            }
            */

            printf("\n\t\t\t                         CRC : ( ");
            for(i = 0; i < 4; i++, op++) {
                printf("%02X ", buf[op]);
            }
            printf(")\n");

        }

        printf("\n");

        /*
        if (tspktindex == 5) {
            break;
        }
        */
    }

    for(i = 0; i < NB_PID_MAX; i++) {
        if (ts->pids[i]) {
            free(ts->pids[i]);
        }
    }

    free(ts);

    close(tsfd);

    return 0;
}
