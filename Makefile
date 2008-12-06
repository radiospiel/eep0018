#
# The erlang source dir
OTPROOT=/opt/local/lib/erlang


#
# The yajl dir
YAJLROOT=yajl

#
# The erlang interface dir
EIROOT=$(OTPROOT)/lib/erl_interface-3.5.7
ERTSROOT=$(OTPROOT)/lib/erts-5.6.3


#
# -----------------------------------------------------------------------------
ERLANG_INCLUDES=-I$(OTPROOT)/usr/include/
# ERLANG_LIBS=-L$(OTPROOT)/usr/lib/ -lerts

EI_INCLUDES=-I $(EIROOT)/include -I $(EIROOT)/src/misc
EI_LIBS=-L$(EIROOT)/lib -lerl_interface -lei

# ./usr/include/
# -----------------------------------------------------------------------------

GCCFLAGS=-fPIC -shared -bundle -flat_namespace -undefined suppress -fno-common -Wall

CFLAGS=$(GCCFLAGS) -I include $(ERLANG_INCLUDES) $(EI_INCLUDES)
LFLAGS=$(GCCFLAGS) $(ERLANG_LIBS) $(EI_LIBS)

ERLCFLAGS=

# -- objects -------------------------------------------------------------------

VPATH=src:$(YAJLROOT)/src

YAJL_OBJECTS=yajl.o yajl_encode.o yajl_lex.o yajl_buf.o yajl_gen.o yajl_parser.o
EEP_OBJECTS=json_to_term.o eep0018.o
# EEP_OBJECTS=eep0018.o

PATHS=bin include include/yajl

OUTDIR=bin

DRIVER=$(OUTDIR)/eep0018_drv.so
BEAM=$(OUTDIR)/eep0018.beam

# -- rules --------------------------------------------------------------------

all: $(PATHS) driver beam

clean: 
	rm -rf $(PATHS) *.o $(DRIVER) $(BEAM)

bin:
	mkdir bin

include: 
	mkdir include

include/yajl: 
	ln -sf ../yajl/src/api include/yajl

beam: $(BEAM)

$(OUTDIR)/eep0018.beam: eep0018.erl
	$(OTPROOT)/bin/erlc -o $(OUTDIR) $^ $(ERLCFLAGS)

driver: $(DRIVER)

$(OUTDIR)/eep0018_drv.so: $(YAJL_OBJECTS) $(EEP_OBJECTS)
	gcc -o $@ $^ $(LFLAGS)
