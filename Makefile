main: main.scm dsp.scm midi.scm portaudio.scm
	csc -O5 main.scm -lportaudio
