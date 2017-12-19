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

    // int buffer_size = 32768;

	// unsigned char* buffer_in = NULL;
    // unsigned char* buffer_out = NULL;

	AVFormatContext* ic = NULL;
	AVFormatContext* oc = NULL;

    // AVIOContext *avio_in = NULL;
    // AVIOContext *avio_out = NULL;

    // AVInputFormat *ifmt = NULL;
    AVOutputFormat *ofmt = NULL;

	AVPacket pkt;

	int i = 0, nRet = -1;
    // int *stream_mapping = NULL;

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

	// nRet = avformat_open_input(&ic, NULL, NULL, NULL);
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

    /*
    stream_mapping = av_mallocz_array(ic->nb_streams, sizeof(*stream_mapping));
    if (!stream_mapping) {
        printf("av_mallocz_array() failed!\n");
        return 1;
    }
    */

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

    /* finish output contex */

    // int stream_index = 0;

	for(i = 0;i < ic->nb_streams; ++i){

        AVStream *out_stream;

        AVStream *in_stream = ic->streams[i];

        AVCodecParameters *in_codecpar = in_stream->codecpar;

        if (in_codecpar->codec_type != AVMEDIA_TYPE_VIDEO &&
            in_codecpar->codec_type != AVMEDIA_TYPE_AUDIO &&
            in_codecpar->codec_type != AVMEDIA_TYPE_SUBTITLE) {
            // stream_mapping[i] = -1;
            continue;
        }

        // stream_mapping[i] = stream_index++;

        out_stream = avformat_new_stream(oc, NULL);
		if(!out_stream){
			printf("Failed allocating output Stream\n");
			return 1;
		}

        nRet = avcodec_parameters_copy(out_stream->codecpar, in_codecpar);
		if(nRet < 0){
			printf("avcodec_copy_context()  failed\n");
			return 0;
		}

		out_stream->codecpar->codec_tag = 0;
	}

    if (!(ofmt->flags & AVFMT_NOFILE)) {
        if (avio_open(&(oc->pb), output, AVIO_FLAG_WRITE) < 0) {
            printf("avio_open2() failed\n");
            return 1;
        }
    }

	nRet = avformat_write_header(oc, NULL);
	if(nRet < 0){
		printf("write header failed\n");
		return 0;
	}
    int ss = (int)time(NULL); // date()

    // av_dump_format(ic, 0, NULL, 0);

    while(1) {
        AVStream *in_stream, *out_stream;

		nRet = av_read_frame(ic, &pkt);
		if(nRet < 0){
            printf("av_read_frame() failed\n");
            break;
            // continue;
		}

        in_stream = ic->streams[pkt.stream_index];
        if (pkt.stream_index >= ic->nb_streams) {
            //stream_mapping[pkt.stream_index] < 0) {
            av_packet_unref(&pkt);
            continue;
        }
        // pkt.stream_index = stream_mapping[pkt.stream_index];

        out_stream = ic->streams[pkt.stream_index];

        printf("aaaa instream: frame rate : %d/%d, audio sample rate : %d, informat: name : %s, "
                "pkt: pts : %" PRId64 ", dts : %" PRId64 "\n",
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
                printf("0000 pts : %" PRId64 ", dts : %" PRId64 "\n", pkt.pts, pkt.dts);
            }
        }

        pkt.pts = av_rescale_q_rnd(pkt.pts, in_stream->time_base, out_stream->time_base, AV_ROUND_NEAR_INF|AV_ROUND_PASS_MINMAX);

        pkt.dts = av_rescale_q_rnd(pkt.dts, in_stream->time_base, out_stream->time_base, AV_ROUND_NEAR_INF|AV_ROUND_PASS_MINMAX);

        pkt.duration = av_rescale_q(pkt.duration, in_stream->time_base, out_stream->time_base);

        printf("bbbb instream: frame rate : %d/%d, audio sample rate : %d, informat: name : %s, "
                "pkt: pts : %" PRId64 ", dts : %" PRId64 "\n",
                in_stream->avg_frame_rate.num, in_stream->avg_frame_rate.den, in_stream->codecpar->sample_rate,
                ic->iformat->name, pkt.pts, pkt.dts);

        nRet = av_interleaved_write_frame(oc, &pkt);
        if(nRet < 0) {
            printf("av_write_frame() failed\n");
            break;
        }

        av_packet_unref(&pkt);
	}

	av_write_trailer(oc);

    if (oc && (oc->oformat->flags & AVFMT_NOFILE)) {
        avio_close(oc->pb);
    }

	avformat_close_input(&ic);

	avformat_free_context(oc);

    // av_freep(&stream_mapping);

	return 0;
}
