(
"~/Desktop/toplap15/synthdefs.scd".loadPaths;
// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
//s.options.outDevice = "Soundflower (64ch)";
//s.options.inDevice = "Soundflower (64ch)";
s.options.numBuffers = 1024 * 16; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 8; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.latency = 0.05;
// boot the server and start SuperDirt
e=();
e.notes = [];
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	~dirt.loadSoundFiles("~/Desktop/toplap15/samples/*");
	// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
	s.sync; // wait for samples to be read
	~dirt.start(57120, [0,2,4,6,8,10,12,14]);   // start listening on port 57120, create four busses each sending audio to channel 0
	{
		var diversions=();
		var dirt=~dirt;
		SynthDef(\rec,{
			|buffer,duration|
			var line = Line.ar(dur:duration, doneAction:2);
			RecordBuf.ar(SoundIn.ar([0,1]), buffer, doneAction: Done.freeSelf, loop: 0);
		}).add;
		~diversions=diversions;
		~diversions[\clearnotes] = {
			e.notes=[];
			~tidalSocket = NetAddr("127.0.0.1", 6010);
			~tidalSocket.sendMsg('/ctrl', "notes", format("%", e.notes-60));
			true;
		};
		~diversions[\veda] = {
			~vedaSocket = NetAddr("127.0.0.1", 4567);
			~vedaSocket.sendMsg('/cycle', ~cycle);
			true;
		};
		~diversions[\rec] = {
			var seconds = 1/(~cps*~speed);
			var frames = ~dirt.server.sampleRate*seconds;
			var bufferName = format("rec%",~n.asInteger).asSymbol;
			[\cycle,~cycle].postln;
			if(mod(~cycle,1/~speed)==0,{
				var buffer = Buffer.alloc(~dirt.server,frames,~numChannels);
				~dirt.soundLibrary.addBuffer(bufferName, buffer);
				// --- //

				/*
				Synth(\rec,[buffer:buffer,duration:seconds]);
				*/

				Ndef(\rec,{
					var line = Line.ar(dur:seconds, doneAction:2);
					RecordBuf.ar(/*SoundIn.ar([0,1])*/ In.ar(0,2), buffer, doneAction: Done.freeSelf, loop: 0);
				});

				// --- //
			});
			true;
		};
		~dirt.orbits[0].defaultParentEvent[\diversion] = { |e| diversions[~s].value; };
	}.value;
	// let's set up our orbits (these first steps are just so we can redefine them on the fly if they're already there)
	~modBus.free;
	~carBus.free;
	~dirt.orbits[0].outBus = 0;
	~dirt.orbits[1].outBus = 0;
	// --- //
	~modBus = Bus.audio(s, numChannels:2); // assuming stereo, expand if needed
	~carBus = Bus.audio(s, numChannels:2);
	~recBus = Bus.audio(s, numChannels:2);
	~dirt.orbits[0].outBus = ~carBus; // play into that bus.
	~dirt.orbits[1].outBus = ~modBus;
	(
		// effects processing on the busses
		Ndef(\x, {
			var modBus = In.ar(~modBus, 2);
			var carBus = In.ar(~carBus, 2);
			var out = modBus+carBus;
			//carBus = carBus+((-15).dbamp*Compander.ar(FreeVerb.ar(carBus, 0.9), carBus, -30.dbamp, 1, 0.01, 0.01, 0.07));
			carBus = Compander.ar(carBus,carBus,-20.dbamp,1,0.5,0.01,0.35)*(6).dbamp;
			out = Compander.ar(carBus, modBus, -30.dbamp, 1, 0.01, 0.01, 0.07)+modBus;
			out = out+Compander.ar(FreeVerb2.ar(out[0],out[1],-20.dbamp,0.95,0.95), out, -30.dbamp, 1, 0.001, 0.001, 0.07);
			out = Compander.ar(out, out, -12.dbamp, 1, 1/2)*4;
			out = Limiter.ar(out,-3.dbamp);
			Out.ar(0, out);
		});
	);
	(
		// here's the bit that sends MIDI data from all controllers to TidalCycles via OSC
		MIDIClient.init;
		MIDIIn.connectAll;
		~tidalSocket = NetAddr("127.0.0.1", 6010);
		e.notes=[];
		MIDIFunc.cc({|val, num, chan, src| ~tidalSocket.sendMsg('/ctrl', num, val/127.0);}, nil, nil, nil);
		MIDIFunc.noteOn({|veloc, num, chan, src| e.notes=e.notes.add(num); e.notes=e.notes.sort({|a,b| a>b}).reverse; ~tidalSocket.sendMsg('/ctrl', "notes", format("%", e.notes-60)); e.notes.postln;});
		MIDIFunc.noteOff({|veloc, num, chan, src| e.notes=e.notes.takeThese({|x| x==num}); ~tidalSocket.sendMsg('/ctrl', "notes", format("%", e.notes-60)); e.notes.postln;});
	);
};
);
