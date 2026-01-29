# Makefile for Barev Pascal Library

FPC = fpc
FPCFLAGS = -O2 -gl -Xs -XX
UNITS = barevtypes.pas barevxml.pas barevnet.pas barev.pas

all: install-units
		$(FPC) $(FPCFLAGS) testbarev.pas
#testbarev: $(UNITS) testbarev.pas
#	$(FPC) $(FPCFLAGS) testbarev.pas

clean:
	rm -f *.o *.ppu testbarev
	rm -f lib/*.o lib/*.ppu

install-units:
#	mkdir -p lib
#	$(FPC) $(FPCFLAGS) -FUlib barevtypes.pas
#	$(FPC) $(FPCFLAGS) -FUlib barevconfig.pas
#	$(FPC) $(FPCFLAGS) -FUlib barevavatar.pas
#	$(FPC) $(FPCFLAGS) -FUlib barevchatstates.pas
#	$(FPC) $(FPCFLAGS) -FUlib barevft.pas
#	$(FPC) $(FPCFLAGS) -FUlib -Fulib barevxml.pas
#	$(FPC) $(FPCFLAGS) -FUlib -Fulib barevnet.pas
#	$(FPC) $(FPCFLAGS) -FUlib -Fulib barev.pas

	$(FPC) $(FPCFLAGS)  -fPIC barevtypes.pas
	$(FPC) $(FPCFLAGS)  -fPIC barevconfig.pas
	$(FPC) $(FPCFLAGS)  -fPIC barevavatar.pas
	$(FPC) $(FPCFLAGS)  -fPIC barevchatstates.pas
	$(FPC) $(FPCFLAGS)  -fPIC barevft.pas
	$(FPC) $(FPCFLAGS)  -fPIC barevxml.pas
	$(FPC) $(FPCFLAGS)  -fPIC barevnet.pas
	$(FPC) $(FPCFLAGS)  -fPIC barev.pas

.PHONY: all clean install-units
