#
FFLAGS= $(OPT) $(FARGS)
#
OBJS= stope.o
#
LIBS= ../Dstopelib/stopelib.a\
      ../Ddammlib/dammlib.a\
      ../Dxglib/xglib.a\
      ../Ddammlib/dammlib.a\
      ../Dxglib/xglib.a\
      ../Dmilibb/milibb.a\
      ../Dorphlib/orphlib.a
#
stope: $(OBJS) $(LIBS)
#
	$(F77) $(FFLAGS) -o stope $(OBJS) $(LIBS) $(FLIBS) -lX11 -lpthread
.f.o:
	$(F77) $(FFLAGS) -c $<
install: stope
	install -d $(INSTALLDIR)
	install -p stope $(INSTALLDIR)
	rm -f *.exe  stope
	install -p stope.hep $(INSTALLDIR)
	install -p stope.dat $(INSTALLDIR)
