#ifndef EEP0018_H
#define EEP0018_H

/* commands */

#define EEP0018_JSON_PARSE              1
#define EEP0018_JSON_PARSE_EI           2

/* simple types */
/* each simple type is followed by the actual data in a string buffer */

#define EEP0018_ATOM     10
#define EEP0018_NUMBER   11
#define EEP0018_STRING   12
#define EEP0018_KEY      13

/* complex types */

#define EEP0018_MAP      14
#define EEP0018_ARRAY    15
#define EEP0018_END      16

#define EEP0018_EI       17


#include "erl_interface.h"
#include "ei.h"    

extern void json_to_binary(ei_x_buff *buf, const unsigned char* s, int len);

#endif
