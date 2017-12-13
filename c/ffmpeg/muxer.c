#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "libavformat/avformat.h"
#include "libavcodec/avcodec.h"
#include "libavutil/avutil.h"
#include "libavutil/rational.h"
#include "libavdevice/avdevice.h"
#include "libavutil/mathematics.h"
#include "libswscale/swscale.h"

#if 0
void printPkt(AVPacket pkt)
{
    printf("AVBufferRef : %p\n"
           "pts : %d\n"
           "dts : %d\n"
           "data : %p\n"
           "size : %d\n"
           "stream_index : %d\n"
           "flags : %d\n"
           "AVPacketSideData : %p\n"
           "side_data_elems : %d\n"
           "duration : %d\n"
           "pos : %d\n",
            &(pkt.buf), pkt.pts, pkt.dts, &(pkt.data), pkt.size, pkt.stream_index,
            pkt.flags, &(pkt.side_data), pkt.side_data_elems, pkt.duration, pkt.pos);
}
#endif

int main(int argc, char* argv[])
{
    if (argc < 2) {
        printf("help: ./muxer [input file name]\n");
        return 1;
    }
    char* input;
    input = argv[1];
    printf("input file name : %s\n", input);
	// const char* input = "/data/webroot/outdata.h264";
    // const char* input = "/data/webroot/outdata.aac";
    // const char* input = "/data/webroot/jljt.mpg";
    const char* output = "/tmp/test.ts";

    int buffer_size = 32768;

	unsigned char* buffer_in = NULL; unsigned char* buffer_out = NULL;

	AVFormatContext* ic = NULL;
	AVFormatContext* oc = NULL;

	AVStream* out_stream = NULL;
	AVStream* in_stream = NULL;

    AVIOContext *avio_in = NULL;
    AVIOContext *avio_out = NULL;

    AVInputFormat *ifmt = NULL;
    AVOutputFormat *ofmt = NULL;

	AVPacket pkt;

	int i = 0, nRet = -1;

	av_register_all();
	// avcodec_register_all();

    /* init input contex */
    /*
	ic = avformat_alloc_context();
    if (ic == NULL) {
        printf("avformat_alloc_context() failed\n");
        return 1;
    }

    buffer_in = (unsigned char*)av_malloc(buffer_size);
    if (buffer_in == NULL) {
        printf("av_malloc() failed\n");
        return 1;
    }

    avio_in = avio_alloc_context(buffer_in, buffer_size, 0, NULL, read_buffer, NULL, NULL);
    if (avio_in == NULL) {
        printf("avio_alloc_context() failed\n");
        return 1;
    }
    ic->pb = avio_in;

    ifmt = av_find_input_format("mpeg");
    if (ifmt == NULL) {
        printf("av_find_input_format() failed\n");
        return 1;
    }
    */

	// nRet = avformat_open_input(&ic, "nothing", ifmt, NULL);
	nRet = avformat_open_input(&ic, input, NULL, NULL);
	if (nRet != 0)
	{
		printf("avformat_open_input() failed\n");
		return 0;
	}
    /* finish input contex */

	if (avformat_find_stream_info(ic, NULL) < 0)
	{
		printf("Call av_find_stream_info function failed!\n");
		return 0;
	}

    /* init output contex */
    ofmt = av_guess_format("mpegts", NULL, NULL);
    if ( !ofmt ) {
        printf("av_guess_format() failed");
        return 1;
    }

    // avformat_alloc_context
	avformat_alloc_output_context2(&oc, ofmt, NULL, output);
	if ( !oc ) {
		printf("Create output context error\n");
		return 0;
	}

    /*
    buffer_out = (unsigned char*)av_malloc(buffer_size);
    if (buffer_out == NULL) {
        printf("av_malloc() failed\n");
        return 1;
    }

    // avio_out = avio_alloc_context(buffer_out, buffer_size, 0, NULL, NULL, write_buffer, NULL);
    avio_out = avio_alloc_context(buffer_out, buffer_size, 0, NULL, NULL, NULL, NULL);
    if (avio_out == NULL) {
        printf("avio_alloc_context() failed\n");
        return 1;
    }
    oc->pb = avio_out;
    */

    if (avio_open(&(oc->pb), output, AVIO_FLAG_WRITE) < 0) {
        printf("avio_open2() failed\n");
        return 1;
    }

    /* finish output contex */

	for(i = 0;i < ic->nb_streams; ++i){
		in_stream = ic->streams[i];
		out_stream = avformat_new_stream(oc,in_stream->codec->codec);
		if(!out_stream){
			printf("Failed allocating output Stream\n");
			return 0;
		}
		nRet = avcodec_copy_context(out_stream->codec,in_stream->codec);
		if(nRet < 0){
			printf("avcodec_copy_context()  failed\n");
			return 0;
		}
		out_stream->codec->codec_tag = 0;
		if(oc->oformat->flags & AVFMT_GLOBALHEADER)
				out_stream->codec->flags |= AVFMT_GLOBALHEADER;
	}

	nRet = avformat_write_header(oc, NULL);
	if(nRet < 0){
		printf("write header failed\n");
		return 0;
	}
	int frame_index = 0;
    int ss = (int)time(NULL); // date()

    av_dump_format(ic, 0, NULL, 0);

    while(1) {
		nRet = av_read_frame(ic, &pkt);
		if(nRet < 0){
            printf("av_read_frame() failed\n");
            break;
            // continue;
		}
        in_stream = ic->streams[pkt.stream_index];
        out_stream = ic->streams[pkt.stream_index];

        printf("instream: frame rate : %d/%d, audio sample rate : %d, informat: name : %s, "
                "pkt: pts : %d, dts : %d\n",
                in_stream->avg_frame_rate.num, in_stream->avg_frame_rate.den, in_stream->codecpar->sample_rate,
                ic->iformat->name, pkt.pts, pkt.dts);

        if (strncmp(ic->iformat->name, "mpeg", 4) != 0) {
            if (pkt.pts == 0 || pkt.pts == AV_NOPTS_VALUE) {
                pkt.pts = ss;

                if (in_stream->codecpar->codec_type == AVMEDIA_TYPE_VIDEO) {
                    pkt.dts = ss;
                    ss = ss + (90000 / (in_stream->avg_frame_rate.num / in_stream->avg_frame_rate.den));
                } else if (in_stream->codecpar->codec_type == AVMEDIA_TYPE_AUDIO) {
                    pkt.dts = 0;
                    ss = ss + 90000 / in_stream->codecpar->sample_rate;
                }
                // printf("pts : %d, dts : %d\n", pkt.pts, pkt.dts);
            }
        }

        pkt.pts = av_rescale_q_rnd(pkt.pts, in_stream->codec->time_base, out_stream->codec->time_base, (enum AVRounding)(AV_ROUND_NEAR_INF|AV_ROUND_PASS_MINMAX));

        pkt.dts = av_rescale_q_rnd(pkt.dts, in_stream->codec->time_base, out_stream->codec->time_base, (enum AVRounding)(AV_ROUND_NEAR_INF|AV_ROUND_PASS_MINMAX));

        nRet = av_write_frame(oc, &pkt);
        if(nRet < 0) {
            printf("av_write_frame(0 failed\n");
            break;
        }

        av_packet_unref(&pkt);

        pkt.pos = -1;

        frame_index++;
	}

	av_write_trailer(oc);

    avio_close(oc->pb);

	avformat_close_input(&ic);

	avformat_free_context(oc);

	return 0;
}
