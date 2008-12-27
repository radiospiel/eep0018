#ifndef EEP0018_H
#define EEP0018_H

/* commands */

#define EEP0018_JSON_PARSE              1

/* simple types */
/* each simple type is followed by the actual data in a string buffer */

#define EEP0018_ATOM     10
#define EEP0018_NUMBER   11
#define EEP0018_STRING   12

/* complex types */

#define EEP0018_MAP      13
#define EEP0018_ARRAY    14
#define EEP0018_END      15

#endif
