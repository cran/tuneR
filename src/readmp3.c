#include <stdio.h>
#include <stdlib.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "config.h"
#include "decoder.h"

/* Short names for MAD structs: */
typedef struct mad_decoder mad_decoder_t;
typedef struct mad_stream mad_stream_t;
typedef struct mad_header mad_header_t;
typedef struct mad_pcm mad_pcm_t;

#include "sexp_macros.h"

typedef struct {
  unsigned char *input;
  R_len_t input_size;
  int sample_rate;
  R_len_t output_size;
  R_len_t output_pos;
  R_len_t output_channels;
  int *left_output;
  int *right_output;
} state_t;

static enum mad_flow mad_input_cb(void *blob, mad_stream_t *stream) {
  state_t *data = (state_t *)blob;
  enum mad_flow res = MAD_FLOW_STOP;
  
  if (0 != data->input_size) {
    mad_stream_buffer(stream, data->input, data->input_size);
    data->input_size = 0;
    res = MAD_FLOW_CONTINUE;
  }
  return res;
}

static enum mad_flow mad_header_cb(void *blob, const mad_header_t *header) {
    state_t *data = (state_t *)blob;

    data->sample_rate = header->samplerate;
    data->output_channels = MAD_NCHANNELS(header);
    data->output_size += mad_timer_count(header->duration, header->samplerate);
    return MAD_FLOW_IGNORE;
}

static R_INLINE int scale(mad_fixed_t sample) {
  /* round */
  sample += (1L << (MAD_F_FRACBITS - 16));

  /* clip */
  if (sample >= MAD_F_ONE)
      sample = MAD_F_ONE - 1;
  else if (sample < -MAD_F_ONE)
      sample = -MAD_F_ONE;
  
  /* quantize */
  return sample >> (MAD_F_FRACBITS + 1 - 16);
}

static enum mad_flow mad_output_cb(void *blob,
                   const mad_header_t *header,
                   mad_pcm_t *pcm) {
  int i;
  const int nchannels = pcm->channels;
  const R_len_t nsamples = pcm->length;
  state_t *data = (state_t *)blob;

  for (i = 0; i < nsamples; ++i) {
      /* Use 'tricky' switch with fallthrough! */
      switch(nchannels) {
      case 2:
      data->right_output[data->output_pos + i] = scale(pcm->samples[1][i]);
      case 1:
      data->left_output[data->output_pos + i] = scale(pcm->samples[0][i]);
      }
  }
  data->output_pos += nsamples;
  return MAD_FLOW_CONTINUE;
}

SEXP do_read_mp3(SEXP s_blob) {
  state_t state;
  mad_decoder_t decoder;
  int result;
  SEXP s_res;

  /* Unpack argument */
  UNPACK_RAW_VECTOR(s_blob, blob, n_blob);

  /* Scan stream to determine the number of samples */
  state.input = blob;
  state.input_size = n_blob;
  state.output_size = 0;
  mad_decoder_init(&decoder, &state, 
           mad_input_cb, mad_header_cb, NULL,
           NULL, NULL, NULL);
  result = mad_decoder_run(&decoder, MAD_DECODER_MODE_SYNC);
  mad_decoder_finish(&decoder);
  if (0 != result)
      error("MAD decoder error. Your MP3 is likely corrupt.");

  /* Allocate result matrix based on calculated number of samples */
  PROTECT(s_res = NEW_OBJECT(MAKE_CLASS("Wave")));
  *REAL(GET_SLOT(s_res, install("samp.rate"))) = state.sample_rate;
  *REAL(GET_SLOT(s_res, install("bit"))) = 16;
  *LOGICAL(GET_SLOT(s_res, install("stereo"))) = state.output_channels == 2;
  SET_SLOT(s_res, install("left"), allocVector(INTSXP, state.output_size));
  state.left_output = INTEGER(GET_SLOT(s_res, install("left")));
  if (state.output_channels == 2) {
      SET_SLOT(s_res, install("right"), allocVector(INTSXP, state.output_size));
      state.right_output = INTEGER(GET_SLOT(s_res, install("right")));
  }

  /* Run decoder again. This time decode frames and store PCM data in
   * state 
   */
  state.input = blob;
  state.input_size = n_blob;
  mad_decoder_init(&decoder, &state,
           mad_input_cb, NULL, NULL,
           mad_output_cb, NULL, NULL);

  result = mad_decoder_run(&decoder, MAD_DECODER_MODE_SYNC);
  mad_decoder_finish(&decoder);
  /* Unprotect memory */
  UNPROTECT(1); /* s_res */

  if (0 != result)
      error("MAD decoder error. Your MP3 is likely corrupt.");
  
  return s_res;
}
