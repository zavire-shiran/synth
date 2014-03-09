import music
import math

class Pattern(object):
    def __init__(self, sample_rate, tempo, patterns, instruments):
        self.sample_rate = sample_rate
        self.tempo = float(tempo) # in beats per second
        self.patterns = patterns
        self.instruments = instruments
        self.time = 0.0
        self.last_note_played = -1.0
        self.playing_notes = []

        pattern_length = len(patterns[0])
        for pattern in patterns[1:]:
            if len(pattern) != pattern_length:
                print 'patterns not the same length!'

    def __iter__(self):
        return self

    def next(self):
        self.time += self.tempo / self.sample_rate

        if math.floor(self.time) > self.last_note_played:
            self.last_note_played = int(math.floor(self.time))
            for pattern, instrument in zip(self.patterns, self.instruments):
                if self.last_note_played < len(pattern) and pattern[self.last_note_played] != ' ':
                    self.playing_notes.append(instrument(self.sample_rate))

        i = 0
        sample = 0.0
        while i < len(self.playing_notes):
            try:
                sample += self.playing_notes[i].next()
                i = i + 1
            except StopIteration:
                del self.playing_notes[i]

        if self.last_note_played > len(self.patterns[0]) and \
           len(self.playing_notes) == 0:
            raise StopIteration()

        return sample

def bass(signal_rate):
    freq = music.segment_signal(signal_rate, [(0, 200),
                                              (0.25, 50)])
    osc = music.Oscillator(signal_rate, freq, music.triangle_wave_func)
    volume = music.segment_signal(signal_rate, [(0, 1.0),
                                                (0.25, 0.0)])
    return music.multiply_signals(osc, volume)

def drum(signal_rate):
    freq = music.segment_signal(signal_rate, [(0, 400), (0.1, 300)])
    osc = music.Oscillator(signal_rate, freq, music.square_wave_func)
    volume = music.segment_signal(signal_rate, [(0, 0.5),
                                                (0.1, 0.0)])
    return music.multiply_signals(osc, volume)

def test_module():
    pattern1 = '1   1   1   1   '
    pattern2 = '  1   1   1   1 '
    pattern = Pattern(44100, 8.0, [pattern1, pattern2], [bass, drum])
    music.s32outputfile('pysound.s32', music.gain(2**29, pattern))

if __name__ == '__main__':
    test_module()
