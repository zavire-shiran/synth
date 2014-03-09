import struct
import math

def sine_wave_func(phase):
    return math.sin(phase * 2 * math.pi)

def square_wave_func(phase):
    if phase < 0.5:
        return 1.0
    else:
        return -1.0

def triangle_wave_func(phase):
    if phase < 0.25:
        return phase * 4
    elif phase < 0.75:
        return 1 - ((phase - 0.25) * 4)
    else:
        return (phase - 1) * 4

def null_signal():
    while True:
        yield 0.0

def constant_signal(value):
    while True:
        yield value

def segment_signal(sample_rate, points):
    time = 0.0

    if len(points) == 0:
        yield 0
        return

    current = 0

    while len(points) > 0:
        until_next_point = points[0][0] - time
        if until_next_point > 0:
            current += (points[0][1] - current) / (until_next_point * sample_rate)
        else:
            current = points[0][1]
        time += 1.0 / sample_rate
        if time >= points[0][0]:
            del points[0]
        yield current

class Oscillator(object):
    def __init__(self, sample_rate, frequency, func):
        self.sample_rate = sample_rate
        self.frequency = frequency
        self.func = func
        self.phase = 0.0

    def __iter__(self):
        return self

    def next(self):
        frequency = self.frequency.next()
        sample = self.func(self.phase)
        self.phase += frequency / self.sample_rate
        if self.phase > 1.0:
            self.phase -= 1.0
        return sample

class Instrument(object):
    def __init__(self, sample_rate, tempo, notes, func):
        self.sample_rate = sample_rate
        self.tempo = float(tempo)
        self.notes = sorted(notes)
        self.func = func
        self.time = 0.0
        self.playing_notes = []

    def __iter__(self):
        return self

    def next(self):
        self.time += self.tempo / self.sample_rate

        self.playing_notes.sort()
        while len(self.playing_notes) and self.playing_notes[0][0] <= self.time:
            del self.playing_notes[0]

        while len(self.notes) > 0 and self.notes[0][0] <= self.time:
            self.playing_notes.append((self.notes[0][1], self.func(*self.notes[0])))
            del self.notes[0]

        if len(self.notes) + len(self.playing_notes) == 0:
            raise StopIteration()

        sample = 0.0

        for end, signal in self.playing_notes:
            try:
                sample += signal.next()
            except StopIteration:
                pass #we don't care if these signals run dry. They'll be culled eventually.

        return sample

def append_signals(*signals):
    for signal in signals:
        for sample in signal:
            yield sample

def delay_signal(sample_rate, seconds, signal):
    return append_signals(truncate_signal(sample_rate, seconds, null_signal()), signal)

def add_signals(*signals):
    while True:
        sample = 0.0
        running_signals = 0
        for s in signals:
            try:
                sample += s.next()
                running_signals += 1
            except StopIteration:
                pass
        if running_signals == 0:
            return
        yield sample

def multiply_signals(signal1, signal2):
    for s1, s2 in zip(signal1, signal2):
        yield s1 * s2

def s32outputfile(filename, signal_iter):
    f = file(filename, 'wb')
    buf = ''
    sample_num = 0
    for sample in signal_iter:
        sample_num += 1
        buf += struct.pack('i', sample)
        if len(buf) >= 1024:
            f.write(buf)
            buf = ''
        if sample_num % 44100 == 0:
            print '%i seconds' % (sample_num / 44100)
    f.write(buf)

def truncate_signal(sample_rate, seconds, signal):
    for i, sample in enumerate(signal):
        if i < (sample_rate * seconds):
            yield sample
        else:
            return

def gain(g, signal):
    for sample in signal:
        yield g * sample

def pitch2frequency(pitch):
    return 440 * (2 ** ((pitch - 69.0) / 12))

def basic_envelope(sample_rate, length):
    return segment_signal(sample_rate, [(0, 0.0),
                                        (0.001, 1.0),
                                        (length - 0.001, 1.0),
                                        (length, 0.0)])

def triangle_oscillator_instr_func(start, end, f):
    osc = Oscillator(44100, constant_signal(pitch2frequency(note2pitch(f))), triangle_wave_func)
    env = basic_envelope(44100, end - start)
    return multiply_signals(osc, env)

def clip_signal(max_amplitude, signal):
    for sample in signal:
        if math.fabs(sample) > max_amplitude:
            if sample > 0:
                yield max_amplitude
            else:
                yield -max_amplitude
        else:
            yield sample

notes = {'C': 0,
         'C#': 1,
         'Db': 1,
         'D': 2,
         'D#': 3,
         'Eb': 3,
         'E': 4,
         'F': 5,
         'F#': 6,
         'Gb': 6,
         'G': 7,
         'G#': 8,
         'Ab': 8,
         'A': 9,
         'A#': 10,
         'Bb': 10,
         'B': 11}

pitches = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']

def note2pitch(note):
    if len(note) == 2:
        octave = int(note[1])
        return octave*12 + notes[note[0]]
    if len(note) >= 3:
        octave = int(note[2:])
        return octave*12 + notes[note[0:2]]
    raise Exception('Invalid note specification: ' + note)

def pitch2note(pitch):
    octave = pitch/12
    n = pitches[pitch % 12]
    return n + str(octave)
    
music_notes = [[0, 0.5, 'F#5'],
               [0.5, 1, 'G#5'],
               [1, 1.5, 'A#5'],
               [1.5, 2, 'B5'],
               [2, 2.5, 'C#6'],
               [2.5, 3, 'D#6'],
               [3, 3.5, 'F6'],
               [3.5, 4, 'F#6'],
               [4, 5, 'F#5'],
               [4, 5, 'A#5'],
               [4, 5, 'C#6'],
               [4, 5, 'F#6'],
               [5, 6, 'F#5'],
               [5, 6, 'B5'],
               [5, 6, 'D#6'],
               [5, 6, 'F#6'],
               [6, 7, 'G#5'],
               [6, 7, 'C#6'],
               [6, 7, 'F6'],
               [7, 8, 'A#5'],
               [7, 8, 'C#6'],
               [7, 8, 'F#6']]

if __name__ == '__main__':
    instr = Instrument(44100, 1, music_notes, triangle_oscillator_instr_func)
    s32outputfile('pysound.s32', gain(2**27, instr))
