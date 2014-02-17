import music

pattern1 = '1   1   1   1   '
pattern2 = '  1   1   1   1 '

def bass(signal_rate):
    freq = music.segment_signal(signal_rate, [(0, 200),
                                              (0.25, 50)])
    osc = music.Oscillator(signal_rate, freq, music.triangle_wave_func)
    volume = music.segment_signal(signal_rate, [(0, 1.0),
                                                (0.25, 0.0)])
    return music.multiply_signals(osc, volume)

def drum(signal_rate):
    freq = music.segment_signal(signal_rate, [(0, 400),
                                              (0.1, 300)])
    osc = music.Oscillator(signal_rate, freq, music.triangle_wave_func)
    volume = music.segment_signal(signal_rate, [(0, 1.0),
                                                (0.1, 0.0)])
    return music.multiply_signals(osc, volume)

if __name__ == '__main__':
    notes = []
    for i in xrange(12):
        notes.append((i/2.0, i/2.0 + 0.5))
    music.s32outputfile('pysound.s32', music.gain(2**30, music.Instrument(44100, notes, lambda s, e: drum(44100))))
