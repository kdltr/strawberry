run: main.scm dsp.so midi.so portaudio.so
	csi -s main.scm graphics.scm full.mid

dsp.so: dsp.scm
	csc -s -O3 dsp.scm

midi.so: midi.scm
	csc -sJ -O3 midi.scm
	csc -s -d0 midi.import.scm

portaudio.so: portaudio.scm
	csc -sJ -O3 portaudio.scm -lportaudio
	csc -s -d0 portaudio.import.scm
