:set -XOverloadedStrings
:module Sound.Tidal.Context

:load "/Users/ericfai/Desktop/toplap15/lib.hs"

tidal <- startTidal (superdirtTarget {oLatency = 0.05}) defaultConfig

let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal False
    asap = streamOnce tidal True
    clearnotes = asap $ s "clearnotes"
    setcps = asap . cps
    bpm t = setcps (t/240)
    xfade = transition tidal (Sound.Tidal.Transition.xfadeIn 4)
    xfadeIn t = transition tidal (Sound.Tidal.Transition.xfadeIn t)
    histpan t = transition tidal (Sound.Tidal.Transition.histpan t)
    wait t = transition tidal (Sound.Tidal.Transition.wait t)
    waitT f t = transition tidal (Sound.Tidal.Transition.waitT f t)
    jump = transition tidal (Sound.Tidal.Transition.jump)
    jumpIn t = transition tidal (Sound.Tidal.Transition.jumpIn t)
    jumpIn' t = transition tidal (Sound.Tidal.Transition.jumpIn' t)
    jumpMod t = transition tidal (Sound.Tidal.Transition.jumpMod t)
    mortal lifespan release = transition tidal (Sound.Tidal.Transition.mortal lifespan release)
    interpolate = transition tidal (Sound.Tidal.Transition.interpolate)
    interpolateIn t = transition tidal (Sound.Tidal.Transition.interpolateIn t)
    clutch = transition tidal (Sound.Tidal.Transition.clutch)
    clutchIn t = transition tidal (Sound.Tidal.Transition.clutchIn t)
    anticipate = transition tidal (Sound.Tidal.Transition.anticipate)
    anticipateIn t = transition tidal (Sound.Tidal.Transition.anticipateIn t)
    d1 = p 1
    d2 = p 2
    d3 = p 3
    d4 = p 4
    d5 = p 5
    d6 = p 6
    d7 = p 7
    d8 = p 8
    d9 = p 9
    d10 = p 10
    d11 = p 11
    d12 = p 12
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
    r0 c = asap $ loopAt c $ s "rec" + n 0
    r1 c = asap $ loopAt c $ s "rec" + n 1
    r2 c = asap $ loopAt c $ s "rec" + n 2
    r3 c = asap $ loopAt c $ s "rec" + n 3
    r4 c = asap $ loopAt c $ s "rec" + n 4
    r5 c = asap $ loopAt c $ s "rec" + n 5
    r6 c = asap $ loopAt c $ s "rec" + n 6
    r7 c = asap $ loopAt c $ s "rec" + n 7
    r8 c = asap $ loopAt c $ s "rec" + n 8
    r9 c = asap $ loopAt c $ s "rec" + n 9
    r10 c = asap $ loopAt c $ s "rec" + n 10
    r11 c = asap $ loopAt c $ s "rec" + n 11
    r12 c = asap $ loopAt c $ s "rec" + n 12
    r13 c = asap $ loopAt c $ s "rec" + n 13
    r14 c = asap $ loopAt c $ s "rec" + n 14
    r15 c = asap $ loopAt c $ s "rec" + n 15
    r16 c = asap $ loopAt c $ s "rec" + n 16

:set prompt "Go Plastic! ~>"
