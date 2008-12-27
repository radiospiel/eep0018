#ifndef EEP0018_H
#define EEP0018_H

/* commands */

#define EEP0018_JSON_PARSE      1
#define EEP0018_JSON_PARSE_DONE 2

/* simple types */
/* each simple type is followed by the actual data in a string buffer */

#define EEP0018_ATOM     3
#define EEP0018_NUMBER   4
#define EEP0018_STRING   5

/* complex types */

#define EEP0018_MAP      6
#define EEP0018_ARRAY    7
#define EEP0018_END      8

#endif
